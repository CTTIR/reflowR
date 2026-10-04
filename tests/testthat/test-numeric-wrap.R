wrap_fixture <- function() {
  data <- data.frame(key = c("p4", "p1", "p3", "p2"),
    x = c(0.8, 0.1, 0.6, 0.3), y = c(20, 0, 40, 1),
    group = c("b", "a", "b", "a"), panel = c("high", "low", "high", "low"))
  corners <- data.frame(key = c("summary-high", "summary-low"),
    panel = c("high", "low"), text = c("rho=NA; n=2\nCONSTANT", "rho=0.50; n=2\nPASS"))
  args <- list(type = "numeric_points", mapping = c(x = "x", y = "y", colour = "group"),
    row_key = "key", levels = list(colour = c("a", "b"), facet_rows = c("low", "high")),
    facets = list(rows = "panel"), numeric_axes = list(
      x = list(limits = c(0, 1), breaks = NULL, labels = NULL),
      y = list(limits = c(0, 50), breaks = NULL, labels = NULL)),
    colour_values = c(a = "#112233", b = "#445566"), style = list(point_size = 1.1),
    scatter = list(ncol = 4, point_alpha = 0.55, corner = list(
      row_key = "key", column = "text", hjust = -0.04, vjust = 1.1,
      size_pt = 7, lineheight = 0.9)))
  list(data = data, corners = corners, args = args)
}

test_that("wrapped scatter preserves literal coordinates and independent corners", {
  f <- wrap_fixture()
  before <- serialize(f, NULL)
  recipe <- do.call(reflow_figure_recipe, f$args)
  result <- reflow_figure_plot(f$data, recipe, annotation_data = f$corners)
  built <- ggplot2::ggplot_build(result$plot)
  expect_identical(built$data[[1]]$x, c(0.8, 0.1, 0.6, 0.3))
  expect_identical(built$data[[1]]$y, c(20, 0, 40, 1))
  expect_identical(as.integer(built$data[[1]]$PANEL), c(2L, 1L, 2L, 1L))
  expect_identical(built$data[[1]]$colour, c("#445566", "#112233", "#445566", "#112233"))
  expect_identical(built$data[[1]]$alpha, rep(0.55, 4))
  expect_identical(built$data[[1]]$size, rep(1.1, 4))
  expect_identical(built$data[[2]]$label,
    c("rho=NA; n=2\nCONSTANT", "rho=0.50; n=2\nPASS"))
  expect_identical(as.integer(built$data[[2]]$PANEL), c(2L, 1L))
  expect_identical(built$data[[2]]$x, rep(-Inf, 2))
  expect_identical(built$data[[2]]$y, rep(Inf, 2))
  expect_identical(built$data[[2]]$hjust, rep(-0.04, 2))
  expect_identical(built$data[[2]]$vjust, rep(1.1, 2))
  expect_identical(built$data[[2]]$lineheight, rep(0.9, 2))
  expect_identical(built$layout$panel_scales_y[[1]]$range$range, c(0, 1))
  expect_identical(built$layout$panel_scales_y[[2]]$range$range, c(20, 40))
  expect_identical(result$plot$coordinates$limits$x, c(0, 1))
  expect_null(result$plot$coordinates$limits$y)
  expect_identical(as.character(built$layout$layout$facet_rows), c("low", "high"))
  expect_identical(result$annotation_records, 2L)
  expect_identical(result$visible_annotations, NA_integer_)
  expect_identical(serialize(f, NULL), before)
})

test_that("corners require exact keyed facet coverage and remain separately hashed", {
  f <- wrap_fixture()
  recipe <- do.call(reflow_figure_recipe, f$args)
  plot <- function(z) reflow_figure_plot(f$data, recipe, annotation_data = z)
  original <- plot(f$corners)
  changed <- f$corners
  changed$text[[1]] <- "rho=NA; n=2\nOTHER"
  altered <- plot(changed)
  expect_identical(original$payload_sha256, altered$payload_sha256)
  expect_false(identical(original$annotation_payload_sha256, altered$annotation_payload_sha256))
  expect_identical(original$recipe_sha256, altered$recipe_sha256)
  expect_identical(original$annotation_payload_sha256,
    plot(f$corners[2:1, ])$annotation_payload_sha256)
  missing <- f$corners[1, ]
  expect_error(plot(missing), "Exactly one corner per facet required", fixed = TRUE)
  duplicate <- f$corners
  duplicate$panel[[2]] <- "high"
  expect_error(plot(duplicate), "Corner key or facet coverage differs", fixed = TRUE)
  duplicate <- f$corners
  duplicate$key[[2]] <- duplicate$key[[1]]
  expect_error(plot(duplicate), "Corner key or facet coverage differs", fixed = TRUE)
  extra <- f$corners
  extra$panel[[2]] <- "unknown"
  expect_error(plot(extra), "Corner key or facet coverage differs", fixed = TRUE)
  invalid <- f$corners
  invalid$text[[1]] <- NA_character_
  expect_error(plot(invalid), "Corner columns must be plain nonmissing text", fixed = TRUE)
  expect_identical(f$corners$text, c("rho=NA; n=2\nCONSTANT", "rho=0.50; n=2\nPASS"))
})

test_that("extension refuses invalid policies and preserves strict input domains", {
  f <- wrap_fixture()
  for (alpha in list(NA_real_, Inf, -0.1, 1.1, structure(0.5, names = "x"))) {
    bad <- f$args
    bad$scatter$point_alpha <- alpha
    expect_error(do.call(reflow_figure_recipe, bad), "Invalid point alpha", fixed = TRUE)
  }
  bad <- f$args
  bad$scatter$corner$size_pt <- 6.9
  expect_error(do.call(reflow_figure_recipe, bad), "Invalid corner geometry or size", fixed = TRUE)
  bad <- f$args
  bad$numeric_axes$y$labels <- "ignored"
  expect_error(do.call(reflow_figure_recipe, bad),
    "Automatic breaks need NULL labels", fixed = TRUE)
  recipe <- do.call(reflow_figure_recipe, f$args)
  bad_data <- f$data
  bad_data$y[[1]] <- 51
  expect_error(reflow_figure_plot(bad_data, recipe, f$corners),
    "Coordinates outside supplied limits", fixed = TRUE)
  bad_data$y[[1]] <- Inf
  expect_error(reflow_figure_plot(bad_data, recipe, f$corners),
    "Numeric coordinates must be plain finite vectors", fixed = TRUE)
  altered <- recipe
  altered$scatter$unexpected <- 1
  expect_error(reflow_figure_plot(f$data, altered, f$corners),
    "Invalid scatter fields", fixed = TRUE)
})

test_that("explicit omission keeps prior recipe serialization and layer semantics", {
  f <- wrap_fixture()
  f$args$scatter <- NULL
  for (axis in c("x", "y")) {
    f$args$numeric_axes[[axis]]$breaks <- f$args$numeric_axes[[axis]]$limits
    f$args$numeric_axes[[axis]]$labels <- as.character(f$args$numeric_axes[[axis]]$limits)
  }
  omitted <- f$args
  omitted$scatter <- NULL
  r <- do.call(reflow_figure_recipe, omitted)
  f$args["scatter"] <- list(NULL)
  expect_identical(serialize(do.call(reflow_figure_recipe, f$args), NULL, version = 2),
    serialize(r, NULL, version = 2))
  old <- .rf_numeric_recipe(f$args$mapping, f$args$row_key, f$args$levels,
    f$args$facets, list(), NULL, NULL, f$args$style, "available", character(),
    list(), f$args$numeric_axes, f$args$colour_values, NULL)
  expect_identical(serialize(r, NULL, version = 2), serialize(old, NULL, version = 2))
  a <- reflow_figure_plot(f$data, r)
  b <- .rf_numeric_plot(f$data, old)
  expect_identical(a$payload_sha256, b$payload_sha256)
  expect_identical(a$recipe_sha256, b$recipe_sha256)
  expect_identical(ggplot2::ggplot_build(a$plot)$data, ggplot2::ggplot_build(b$plot)$data)
  expect_error(reflow_figure_plot(f$data, r, f$corners),
    "Separate annotation data require wrapped numeric points", fixed = TRUE)
})

test_that("wrap rejects multiple facet columns before data indexing", {
  f <- wrap_fixture()
  f$args$facets$rows <- c("panel", "group")
  expect_error(do.call(reflow_figure_recipe, f$args),
    "Wrapped points require one row facet", fixed = TRUE)
})

test_that("wrapped recipes roundtrip and reject schema or field tampering", {
  f <- wrap_fixture()
  recipe <- do.call(reflow_figure_recipe, f$args)
  roundtrip <- unserialize(serialize(recipe, NULL, version = 2))
  expect_identical(roundtrip, recipe)
  original <- reflow_figure_plot(f$data, recipe, f$corners)
  restored <- reflow_figure_plot(f$data, roundtrip, f$corners)
  expect_identical(restored$recipe_sha256, original$recipe_sha256)
  expect_identical(restored$payload_sha256, original$payload_sha256)
  expect_identical(restored$annotation_payload_sha256, original$annotation_payload_sha256)
  expect_identical(ggplot2::ggplot_build(restored$plot)$data,
    ggplot2::ggplot_build(original$plot)$data)
  changed <- recipe
  changed$schema <- "reflow_numeric_points_wrap_unknown"
  expect_error(reflow_figure_plot(f$data, changed, f$corners),
    "Separate annotation data require wrapped numeric points", fixed = TRUE)
  changed <- recipe
  changed$extra <- "unexpected"
  expect_error(reflow_figure_plot(f$data, changed, f$corners),
    "Invalid wrapped recipe schema", fixed = TRUE)
  changed <- recipe
  attr(changed, "extra") <- TRUE
  expect_error(reflow_figure_plot(f$data, changed, f$corners),
    "Wrapped recipe failed revalidation", fixed = TRUE)
})

test_that("corner keys remain literal and attributed columns are refused", {
  f <- wrap_fixture()
  f$corners$key <- c("a|b\nc", "a\nb|c")
  recipe <- do.call(reflow_figure_recipe, f$args)
  snapshot <- serialize(f, NULL)
  result <- reflow_figure_plot(f$data, recipe, f$corners)
  expect_identical(result$annotation_records, 2L)
  expect_identical(serialize(f, NULL), snapshot)
  for (column in c("key", "panel", "text")) {
    invalid <- f$corners
    attr(invalid[[column]], "annotation") <- "unexpected"
    expect_error(reflow_figure_plot(f$data, recipe, invalid),
      "Corner columns must be plain nonmissing text", fixed = TRUE)
  }
  invalid <- f$corners
  invalid$key[[1]] <- " "
  expect_error(reflow_figure_plot(f$data, recipe, invalid),
    "Corner columns must be plain nonmissing text", fixed = TRUE)
  invalid <- f$corners
  names(invalid)[[3]] <- "key"
  expect_error(reflow_figure_plot(f$data, recipe, invalid), "Invalid corner data", fixed = TRUE)
  invalid <- f$corners
  invalid$panel <- factor(invalid$panel)
  expect_error(reflow_figure_plot(f$data, recipe, invalid),
    "Corner columns must be plain nonmissing text", fixed = TRUE)
})

test_that("wrapped explicit scales and display text preserve keyed geometry", {
  f <- wrap_fixture()
  original <- reflow_figure_plot(f$data,
    do.call(reflow_figure_recipe, f$args), f$corners)
  args <- f$args
  args$numeric_axes$x$breaks <- c(0, 0.5, 1)
  args$numeric_axes$x$labels <- c("Start", "Middle", "End")
  args$numeric_axes$y$breaks <- c(0, 1, 20, 40)
  args$numeric_axes$y$labels <- c("Zero", "One", "Twenty", "Forty")
  args$display_labels <- list(facet_rows = c(low = "Lower\nstratum", high = "Upper stratum"))
  args$style$text_size <- 10
  before <- serialize(list(f, args), NULL)
  recipe <- do.call(reflow_figure_recipe, args)
  result <- reflow_figure_plot(f$data, recipe, f$corners)
  aa <- ggplot2::ggplot_build(original$plot)
  bb <- ggplot2::ggplot_build(result$plot)
  expect_identical(bb$data, aa$data)
  expect_identical(bb$layout$layout, aa$layout$layout)
  expect_identical(result$payload_sha256, original$payload_sha256)
  expect_identical(result$annotation_payload_sha256, original$annotation_payload_sha256)
  expect_false(identical(result$recipe_sha256, original$recipe_sha256))
  expect_identical(bb$layout$panel_scales_x[[1]]$get_breaks(c(0, 1)), c(0, 0.5, 1))
  expect_identical(bb$layout$panel_scales_x[[1]]$get_labels(c(0, 0.5, 1)),
    c("Start", "Middle", "End"))
  for (scale in bb$layout$panel_scales_y) {
    expect_identical(scale$get_breaks(c(0, 50)), c(0, 1, 20, 40))
    expect_identical(scale$get_labels(c(0, 1, 20, 40)),
      c("Zero", "One", "Twenty", "Forty"))
  }
  expect_identical(bb$layout$panel_scales_y[[1]]$range$range, c(0, 1))
  expect_identical(bb$layout$panel_scales_y[[2]]$range$range, c(20, 40))
  labels <- result$plot$facet$params$labeller(
    data.frame(facet_rows = c("low", "high")))
  expect_identical(unname(unlist(labels)), c("Lower\nstratum", "Upper stratum"))
  expect_identical(ggplot2::calc_element("text", result$plot$theme)$size, 10)
  expect_false(identical(ggplot2::calc_element("text", original$plot$theme)$size, 10))
  expect_identical(result$visible_annotations, NA_integer_)
  expect_identical(serialize(list(f, args), NULL), before)
})
