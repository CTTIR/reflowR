tile_data <- function() {
  data.frame(key = c("second", "first", "third"), x = c(-10, 20, 20),
    y = c(100, -30, 100), category = c("b", "a", "b"),
    status = c("zero", "available", "unavailable"))
}
tile_args <- function() {
  list(type = "numeric_tiles", mapping = c(x = "x", y = "y", fill = "category"),
    row_key = "key", levels = list(fill = c("b", "a", "unused")),
    colour_values = c(b = "#112233", a = "#445566", unused = "#778899"),
    tile = list(width = 4, height = 10, legend = "observed"),
    labels = list(title = "Literal title", subtitle = "No new analysis", x = "X",
      y = "Y", fill = "Category", caption = "Blank space has no inferred value"))
}

test_that("numeric tiles retain literal geometry, gaps, order and categorical fill", {
  d <- tile_data()
  args <- tile_args()
  before <- serialize(list(d, args), NULL)
  r <- do.call(reflow_figure_recipe, args)
  out <- reflow_figure_plot(d, r)
  b <- ggplot2::ggplot_build(out$plot)
  z <- b$data[[1]]
  expect_identical(z$x, c(-10, 20, 20))
  expect_identical(z$y, c(100, -30, 100))
  expect_identical(z$xmin, c(-12, 18, 18))
  expect_identical(z$xmax, c(-8, 22, 22))
  expect_identical(z$ymin, c(95, -35, 95))
  expect_identical(z$ymax, c(105, -25, 105))
  expect_identical(z$fill, c("#112233", "#445566", "#112233"))
  expect_true(all(is.na(z$colour)))
  expect_identical(out$plot$coordinates$ratio, 1)
  expect_identical(as.character(b$plot$scales$get_scales("fill")$get_breaks()), c("b", "a"))
  expect_identical(out$rows, 3L)
  expect_identical(out$plot$labels$caption, args$labels$caption)
  expect_identical(serialize(list(d, args), NULL), before)
  # Independent direct construction uses the literal expected tile geometry.
  original <- ggplot2::ggplot(data.frame(x = c(-10, 20, 20), y = c(100, -30, 100),
    category = factor(c("b", "a", "b"), levels = c("b", "a"))),
    ggplot2::aes(x, y, fill = category)) +
    ggplot2::geom_tile(width = 4, height = 10, colour = NA) +
    ggplot2::scale_fill_manual(values = c(b = "#112233", a = "#445566"),
      limits = c("b", "a"), drop = FALSE) + ggplot2::coord_equal() +
    ggplot2::theme_bw(base_size = 8, base_family = "") +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "bottom",
      axis.text.x = ggplot2::element_text(angle = 0))
  ob <- ggplot2::ggplot_build(original)
  expect_identical(b$data, ob$data)
  expect_identical(b$layout$panel_params[[1]]$x.range,
    ob$layout$panel_params[[1]]$x.range)
  expect_identical(b$layout$panel_params[[1]]$y.range,
    ob$layout$panel_params[[1]]$y.range)
})

test_that("unused tile levels are explicit and singleton tiles remain whole", {
  d <- tile_data()[1, , drop = FALSE]
  args <- tile_args()
  args$tile$legend <- "declared"
  r <- do.call(reflow_figure_recipe, args)
  out <- reflow_figure_plot(d, r)
  b <- ggplot2::ggplot_build(out$plot)
  expect_identical(nrow(b$data[[1]]), 1L)
  expect_identical(b$data[[1]]$xmin, -12)
  expect_identical(b$data[[1]]$ymax, 105)
  expect_identical(as.character(b$plot$scales$get_scales("fill")$get_breaks()),
    c("b", "a", "unused"))
  args$tile$legend <- "observed"
  reduced <- reflow_figure_plot(d, do.call(reflow_figure_recipe, args))
  rb <- ggplot2::ggplot_build(reduced$plot)
  expect_identical(as.character(rb$plot$scales$get_scales("fill")$get_breaks()), "b")
  expect_identical(reduced$payload_sha256, out$payload_sha256)
  expect_false(identical(reduced$recipe_sha256, out$recipe_sha256))
  expect_error(reflow_figure_plot(d[FALSE, ], r), "nonempty", fixed = TRUE)
})

test_that("tile validation refuses malformed data without modifying it", {
  r <- do.call(reflow_figure_recipe, tile_args())
  cases <- list(
    function(d) {
      d$x[1] <- Inf
      d
    },
    function(d) {
      d$y[1] <- NA_real_
      d
    },
    function(d) {
      d$x <- as.character(d$x)
      d
    },
    function(d) {
      d$key[2] <- d$key[1]
      d
    },
    function(d) {
      d$key[1] <- ""
      d
    },
    function(d) {
      d$category[1] <- "unknown"
      d
    },
    function(d) {
      d$category <- factor(d$category)
      d
    },
    function(d) {
      d$x <- NULL
      d
    },
    function(d) {
      d$x[1] <- .Machine$double.xmax
      d
    })
  for (mutate in cases) {
    d <- mutate(tile_data())
    before <- serialize(d, NULL)
    expect_error(reflow_figure_plot(d, r))
    expect_identical(serialize(d, NULL), before)
  }
  d <- tile_data()
  d$ignored <- list(1, NULL, "arbitrary")
  expect_identical(reflow_figure_plot(d, r)$payload_sha256,
    reflow_figure_plot(tile_data(), r)$payload_sha256)
  expect_identical(reflow_figure_plot(d[3:1, ], r)$payload_sha256,
    reflow_figure_plot(d, r)$payload_sha256)
  expect_identical(ggplot2::ggplot_build(reflow_figure_plot(d[3:1, ], r)$plot)$data[[1]]$x,
    c(20, 20, -10))
})

test_that("tile recipes fail closed on invalid declarations and tampering", {
  cases <- list(
    function(a) {
      a$tile$width <- 0
      a
    },
    function(a) {
      a$tile$height <- Inf
      a
    },
    function(a) {
      a$tile$legend <- "automatic"
      a
    },
    function(a) {
      a$tile$extra <- TRUE
      a
    },
    function(a) {
      a$colour_values <- c(b = "red")
      a
    },
    function(a) {
      a$colour_values[1] <- "not-a-colour"
      a
    },
    function(a) {
      a$levels$fill <- c("a", "a")
      a
    },
    function(a) {
      a$status <- "empty"
      a
    },
    function(a) {
      a$facets <- list(rows = "category")
      a
    },
    function(a) {
      a$reference <- 0
      a
    },
    function(a) {
      a$row_key <- "x"
      a
    })
  for (mutate in cases) {
    a <- mutate(tile_args())
    before <- serialize(a, NULL)
    expect_error(do.call(reflow_figure_recipe, a))
    expect_identical(serialize(a, NULL), before)
  }
  r <- do.call(reflow_figure_recipe, tile_args())
  r$extra <- TRUE
  expect_error(reflow_figure_plot(tile_data(), r), "schema", fixed = TRUE)
  r <- do.call(reflow_figure_recipe, tile_args())
  r$tile$width <- -1
  expect_error(reflow_figure_plot(tile_data(), r), "dimensions", fixed = TRUE)
  expect_error(reflow_figure_plot(tile_data(),
    do.call(reflow_figure_recipe, tile_args()), annotation_data = data.frame()),
    "Separate annotation", fixed = TRUE)
})


test_that("numeric tile recipes roundtrip with literal labels and exact payload", {
  args <- tile_args()
  args$labels$fill <- "Full categorical label"
  r <- do.call(reflow_figure_recipe, args)
  before <- serialize(list(args, r), NULL, version = 2)
  restored <- unserialize(serialize(r, NULL, version = 2))
  expect_identical(restored, r)
  d <- tile_data()
  out <- reflow_figure_plot(d, restored)
  original <- reflow_figure_plot(d, r)
  expect_identical(out$plot$labels$subtitle, "No new analysis")
  expect_identical(out$plot$labels$fill, "Full categorical label")
  expect_identical(out$recipe_sha256, original$recipe_sha256)
  expect_identical(out$payload_sha256, original$payload_sha256)
  expect_identical(ggplot2::ggplot_build(out$plot)$data,
    ggplot2::ggplot_build(original$plot)$data)
  expect_identical(serialize(list(args, r), NULL, version = 2), before)
})

test_that("tile legend size changes only its theme element and recipe hash", {
  d <- tile_data()
  args <- tile_args()
  args$style <- list(base_size = 10)
  plain <- do.call(reflow_figure_recipe, args)
  args$tile$legend_text_size_pt <- 7
  sized <- do.call(reflow_figure_recipe, args)
  before <- serialize(list(d, args, plain, sized), NULL)
  a <- reflow_figure_plot(d, plain)
  b <- reflow_figure_plot(d, sized)
  expect_identical(b$plot$theme$legend.text$size, 7)
  expect_identical(b$plot$theme$text$size, 10)
  expect_identical(b$payload_sha256, a$payload_sha256)
  expect_false(identical(b$recipe_sha256, a$recipe_sha256))
  ab <- ggplot2::ggplot_build(a$plot)
  bb <- ggplot2::ggplot_build(b$plot)
  expect_identical(bb$data, ab$data)
  expect_identical(bb$layout$layout, ab$layout$layout)
  expect_identical(bb$layout$panel_params[[1]]$x.range,
    ab$layout$panel_params[[1]]$x.range)
  expect_identical(bb$layout$panel_params[[1]]$y.range,
    ab$layout$panel_params[[1]]$y.range)
  restored <- unserialize(serialize(sized, NULL, version = 2))
  expect_identical(restored, sized)
  expect_identical(reflow_figure_plot(d, restored)$recipe_sha256, b$recipe_sha256)
  args$style$text_size <- 12
  override <- reflow_figure_plot(d, do.call(reflow_figure_recipe, args))
  expect_identical(override$plot$theme$text$size, 12)
  expect_identical(override$plot$theme$legend.text$size, 7)
  expect_identical(override$payload_sha256, a$payload_sha256)
  expect_identical(ggplot2::ggplot_build(override$plot)$data, ab$data)
  expect_identical(serialize(list(d, plain, sized), NULL),
    serialize(unserialize(before)[c(1, 3, 4)], NULL))
})

test_that("tile legend default bytes match the accepted predecessor implementation", {
  path <- testthat::test_path("fixtures", "numeric-tiles-v3.R")
  expect_identical(digest::digest(file = path, algo = "sha256"),
    "a85736d79ad6769389cfd445cd31fa167c88a9e20edccf2147683a41691b7b51")
  old <- new.env(parent = asNamespace("reflowR"))
  sys.source(path, envir = old)
  args <- tile_args()
  actual <- do.call(reflow_figure_recipe, args)
  explicit <- args
  explicit$tile["legend_text_size_pt"] <- list(NULL)
  expect_identical(do.call(reflow_figure_recipe, explicit), actual)
  complete <- unclass(actual)
  complete[c("schema", "type")] <- NULL
  prior <- do.call(old$.rf_tiles_recipe, complete)
  expect_identical(serialize(actual, NULL, version = 2), serialize(prior, NULL, version = 2))
  d <- tile_data()
  current <- reflow_figure_plot(d, actual)
  expected <- old$.rf_tiles_plot(d, prior)
  expect_identical(current$payload_sha256, expected$payload_sha256)
  expect_identical(current$recipe_sha256, expected$recipe_sha256)
  expect_identical(ggplot2::ggplot_build(current$plot)$data,
    ggplot2::ggplot_build(expected$plot)$data)
  expect_identical(current$plot$theme, expected$plot$theme)
})

test_that("tile text options and declarations refuse malformed input unchanged", {
  invalid <- list(0, -1, Inf, NA_real_, "7", c(7, 8), numeric(),
    structure(7, names = "size"), matrix(7), TRUE, list(7))
  for (value in invalid) {
    args <- tile_args()
    args$tile["legend_text_size_pt"] <- list(value)
    before <- serialize(args, NULL)
    expect_error(do.call(reflow_figure_recipe, args), "Invalid tile legend text size",
      fixed = TRUE)
    expect_identical(serialize(args, NULL), before)
  }
  for (value in list(NA_character_, c("one", "two"), 1, expression(x), list("x"))) {
    args <- tile_args()
    args$labels <- list(title = value)
    before <- serialize(args, NULL)
    expect_error(do.call(reflow_figure_recipe, args), "Labels must be plain text",
      fixed = TRUE)
    expect_identical(serialize(args, NULL), before)
  }
  args <- tile_args()
  args$labels <- list(subtitle = "", fill = NULL)
  before <- serialize(args, NULL)
  result <- reflow_figure_plot(tile_data(), do.call(reflow_figure_recipe, args))
  expect_identical(result$plot$labels$subtitle, "")
  expect_null(result$plot$labels$fill)
  expect_identical(serialize(args, NULL), before)
  args$labels <- list(unknown = "label")
  expect_error(do.call(reflow_figure_recipe, args), "Invalid tile labels", fixed = TRUE)
})


test_that("tile axis inheritance is opt-in with exact default recipe compatibility", {
  args <- tile_args()
  data <- tile_data()
  before <- serialize(list(args, data), NULL, version = 2)
  base <- do.call(reflow_figure_recipe, args)
  omitted <- reflow_figure_plot(data, base)
  args$tile["axis_text_x_inherit_blank"] <- list(NULL)
  null <- do.call(reflow_figure_recipe, args)
  expect_identical(serialize(null, NULL, version = 2), serialize(base, NULL, version = 2))
  explicit_null <- reflow_figure_plot(data, null)
  expect_identical(explicit_null$recipe_sha256, omitted$recipe_sha256)
  expect_identical(explicit_null$plot$theme, omitted$plot$theme)
  args$tile$axis_text_x_inherit_blank <- FALSE
  false_recipe <- do.call(reflow_figure_recipe, args)
  explicit_false <- reflow_figure_plot(data, false_recipe)
  expect_identical(explicit_false$plot$theme, omitted$plot$theme)
  expect_identical(explicit_false$payload_sha256, omitted$payload_sha256)
  expect_false(identical(explicit_false$recipe_sha256, omitted$recipe_sha256))
  args$tile$axis_text_x_inherit_blank <- NULL
  expect_identical(serialize(list(args, data), NULL, version = 2), before)
})

test_that("tile opt-in matches unmodified base-theme axis and leaves geometry exact", {
  args <- tile_args()
  args$style <- list(base_size = 10)
  args$tile$legend_text_size_pt <- 7
  data <- tile_data()
  before <- serialize(list(args, data), NULL, version = 2)
  old <- reflow_figure_plot(data, do.call(reflow_figure_recipe, args))
  opt <- args
  opt$tile$axis_text_x_inherit_blank <- TRUE
  recipe <- do.call(reflow_figure_recipe, opt)
  out <- reflow_figure_plot(data, recipe)
  expect_identical(recipe$tile$axis_text_x_inherit_blank, TRUE)
  expect_identical(unserialize(serialize(recipe, NULL, version = 2)), recipe)
  expect_identical(out$payload_sha256, old$payload_sha256)
  expect_false(identical(out$recipe_sha256, old$recipe_sha256))
  original_theme <- ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 7))
  roles <- c("text", "plot.title", "plot.subtitle", "plot.caption", "axis.text.x",
    "axis.text.y", "axis.title.x", "axis.title.y", "legend.text", "legend.title",
    "panel.grid.major", "panel.grid.minor")
  for (role in roles) {
    expect_identical(ggplot2::calc_element(role, out$plot$theme),
      ggplot2::calc_element(role, original_theme))
  }
  expect_identical(ggplot2::calc_element("axis.text.x", out$plot$theme)$inherit.blank, TRUE)
  expect_identical(ggplot2::calc_element("axis.text.x", old$plot$theme)$inherit.blank, FALSE)
  built <- ggplot2::ggplot_build(out$plot)
  prior <- ggplot2::ggplot_build(old$plot)
  expect_identical(built$data, prior$data)
  expect_identical(built$layout$layout, prior$layout$layout)
  expect_identical(built$layout$panel_params[[1]]$x.range,
    prior$layout$panel_params[[1]]$x.range)
  expect_identical(built$layout$panel_params[[1]]$y.range,
    prior$layout$panel_params[[1]]$y.range)
  expect_identical(as.list(out$plot$labels), as.list(old$plot$labels))
  expect_identical(serialize(list(args, data), NULL, version = 2), before)
  opt$style$x_angle <- 30
  rotated <- reflow_figure_plot(data, do.call(reflow_figure_recipe, opt))
  axis <- ggplot2::calc_element("axis.text.x", rotated$plot$theme)
  expect_identical(axis$angle, 30)
  expect_identical(axis$inherit.blank, TRUE)
  expect_identical(ggplot2::ggplot_build(rotated$plot)$data, built$data)
})

test_that("tile axis inheritance rejects malformed values without mutation", {
  invalid <- list(NA, c(TRUE, FALSE), logical(), 1, "TRUE", list(TRUE),
    structure(TRUE, names = "x"), matrix(TRUE), structure(TRUE, class = "flag"))
  for (value in invalid) {
    args <- tile_args()
    args$tile["axis_text_x_inherit_blank"] <- list(value)
    before <- serialize(args, NULL, version = 2)
    expect_error(do.call(reflow_figure_recipe, args), "Invalid tile axis text inheritance",
      fixed = TRUE)
    expect_identical(serialize(args, NULL, version = 2), before)
  }
})
