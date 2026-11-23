# The generated plotting helpers are evaluated here, not only matched as text.
generated_plot_helpers <- function(scheme = "clinical") {
  root <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(root, "code"))
  s <- reflow_scheme(scheme)
  env <- new.env(parent = globalenv())
  env$requireNamespace <- function(package, ...) FALSE
  env$library <- function(...) invisible(NULL)
  suppressMessages(capture.output(
    sys.source(generate_setup_script(root, s), envir = env)
  ))
  sources <- c(generate_utils_script(root, s),
               tempfile("plot-helpers-", root, fileext = ".R"))
  writeLines(generate_plot_helpers_content(s), sources[[2]])
  for (path in sources) {
    for (expr in parse(path, keep.source = FALSE)) {
      if (is.call(expr) && identical(expr[[1L]], as.name("source"))) next
      eval(expr, envir = env)
    }
  }
  env
}

local_generated_plot_state <- function(envir = parent.frame()) {
  old_theme <- ggplot2::theme_get()
  old_chunks <- knitr::opts_chunk$get()
  withr::defer(ggplot2::theme_set(old_theme), envir = envir)
  withr::defer(knitr::opts_chunk$set(old_chunks), envir = envir)
}

skip_without_generated_plot_packages <- function() {
  for (package in c("here", "knitr", "tibble")) skip_if_not_installed(package)
}

test_that("generated forest plot draws horizontal intervals without deprecated geoms", {
  skip_without_generated_plot_packages()
  local_generated_plot_state()
  env <- generated_plot_helpers()
  expect_no_warning(
    p <- env$wf_forest_plot(c(1.4, 0.8), c(1.1, 0.5), c(1.9, 1.2), c("a", "b"))
  )
  expect_no_warning(built <- ggplot2::ggplot_build(p))
  bars <- built$data[[2]]
  expect_equal(bars$xmin, c(1.1, 0.5))
  expect_equal(bars$xmax, c(1.9, 1.2))
  # Caps span the category axis: 0.2 around positions 2 ("a") and 1 ("b").
  expect_equal(bars$ymin, c(1.9, 0.9))
  expect_equal(bars$ymax, c(2.1, 1.1))
})

test_that("generated ROC plot identifies several curves in a legend", {
  skip_without_generated_plot_packages()
  local_generated_plot_state()
  env <- generated_plot_helpers()
  s <- seq(0, 1, 0.25)
  first <- data.frame(specificity = 1 - s, sensitivity = sqrt(s))
  second <- data.frame(specificity = 1 - s, sensitivity = s)
  p <- env$wf_roc_plot(Logistic = first, second)
  built <- ggplot2::ggplot_build(p)
  scale <- built$plot$scales$get_scales("colour")
  expect_identical(scale$get_limits(), c("Logistic", "Curve 2"))
  expect_identical(unique(built$data[[2]]$colour), env$wf_palette[[1]])
  expect_identical(unique(built$data[[3]]$colour), env$wf_palette[[2]])
  expect_equal(built$data[[2]]$y, sqrt(s))
  expect_false(identical(scale$guide, "none"))
  # One curve needs no legend; unusable inputs are reported, not dropped silently.
  single <- env$wf_roc_plot(first)
  expect_identical(single$scales$get_scales("colour")$guide, "none")
  expect_warning(env$wf_roc_plot(first, data.frame(x = 1)), "Ignoring 1 input")
})

test_that("generated distributions draw bars and curve on one density scale", {
  skip_without_generated_plot_packages()
  skip_if_not_installed("patchwork")
  local_generated_plot_state()
  env <- generated_plot_helpers()
  d <- data.frame(x = c(1, 2, 2, 3, 3, 3, 4, 4, 5, 9))
  both <- ggplot2::ggplot_build(env$wf_distributions(d, type = "both")[[1]])
  bars <- both$data[[1]]
  curve <- both$data[[2]]
  expect_equal(sum(bars$y * (bars$xmax - bars$xmin)), 1)
  # Counts would be up to 3 while the density curve stays far below 1.
  expect_lt(max(bars$y) / max(curve$y), 10)
  counts <- ggplot2::ggplot_build(env$wf_distributions(d, type = "histogram")[[1]])
  expect_equal(sum(counts$data[[1]]$y), nrow(d))
})

test_that("generated box and correlation plots keep legends informative", {
  skip_without_generated_plot_packages()
  local_generated_plot_state()
  env <- generated_plot_helpers()
  d <- data.frame(g = rep(c("a", "b"), each = 5), h = rep(c("u", "v"), 5),
                  y = c(1:5, 3:7))
  # ggplot2 3.5 moved the plot guides from a plain list into a Guides object.
  fill_guide <- function(p) {
    guides <- if (is.list(p$guides$guides)) p$guides$guides else p$guides
    guides[["fill"]]
  }
  expect_identical(fill_guide(env$wf_boxjitter(d, "g", "y")), "none")
  expect_null(fill_guide(env$wf_boxjitter(d, "g", "y", fill = "h")))
  heat <- env$wf_cor_heatmap(data.frame(a = 1:5, b = c(2, 1, 4, 3, 5), c = 5:1))
  expect_identical(format(heat$theme$legend.key.width), "1.5cm")
})

test_that("generated volcano plot labels points without a text glyph in the legend", {
  skip_without_generated_plot_packages()
  skip_if_not_installed("dplyr")
  skip_if_not_installed("ggrepel")
  local_generated_plot_state()
  env <- generated_plot_helpers()
  d <- data.frame(gene = c("g1", "g2", "g3"), log2fc = c(2, 0.1, -3),
                  p_value = c(0.001, 0.5, 0.01))
  p <- env$wf_volcano_plot(d, label_col = "gene")
  expect_false(p$layers[[length(p$layers)]]$show.legend)
  expect_null(p$scales$get_scales("colour")$name)
  expect_identical(as.character(p$layers[[length(p$layers)]]$data$gene), c("g1", "g3"))
})
