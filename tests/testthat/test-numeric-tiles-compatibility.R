test_that("existing recipes and plots match the published pre-tile baseline", {
  baseline <- testthat::test_path("fixtures", "figure-api-feda7a9.R")
  expect_identical(digest::digest(file = baseline, algo = "sha256"),
    "35c7d0325b12d30ac1f0b2e6ca96def5de93cd5b710aadd4a01e14a148320c97")
  old <- new.env(parent = asNamespace("reflowR"))
  sys.source(baseline, envir = old)
  d <- data.frame(key = c("b", "a"), x = c(0, 1), y = c(1, 0),
    group = c("g", "g"), facet = c("second", "first"))
  axis <- list(limits = c(-1, 2), breaks = c(-1, 0, 1, 2),
    labels = c("-1", "0", "1", "2"))
  numeric <- list(type = "numeric_points", mapping = c(x = "x", y = "y", colour = "group"),
    row_key = "key", levels = list(colour = "g"), colour_values = c(g = "#123456"),
    numeric_axes = list(x = axis, y = axis))
  wrapped <- numeric
  wrapped$facets <- list(rows = "facet")
  wrapped$levels$facet_rows <- c("first", "second")
  wrapped$scatter <- list(ncol = 2, point_alpha = 0.5,
    corner = list(row_key = "key", column = "label", hjust = 0, vjust = 1,
      size_pt = 7, lineheight = 0.9))
  corners <- data.frame(key = c("c2", "c1"), facet = c("second", "first"),
    label = c("Literal second", "Literal first"))
  hd <- data.frame(key = c("b", "a"), x = c("right", "left"),
    y = c("top", "bottom"), value = c(0, 2), group = c("g", "g"))
  heat <- list(type = "tile_heatmap", mapping = c(x = "x", y = "y", value = "value"),
    row_key = "key", levels = list(x = c("left", "right"), y = c("bottom", "top")),
    midpoint = 0)
  effect <- list(type = "effect_points", mapping = c(x = "value", y = "y", colour = "group"),
    row_key = "key", levels = list(y = c("bottom", "top"), colour = "g"), reference = 0)
  fixtures <- list(list(args = heat, data = hd, annotation = NULL),
    list(args = effect, data = hd, annotation = NULL),
    list(args = numeric, data = d, annotation = NULL),
    list(args = wrapped, data = d, annotation = corners))
  for (fixture in fixtures) {
    before <- serialize(fixture, NULL)
    expected_recipe <- do.call(old$reflow_figure_recipe, fixture$args)
    actual_recipe <- do.call(reflow_figure_recipe, fixture$args)
    expect_identical(serialize(actual_recipe, NULL, version = 2),
      serialize(expected_recipe, NULL, version = 2))
    expected <- old$reflow_figure_plot(fixture$data, expected_recipe, fixture$annotation)
    actual <- reflow_figure_plot(fixture$data, actual_recipe, fixture$annotation)
    expect_identical(actual[setdiff(names(actual), "plot")],
      expected[setdiff(names(expected), "plot")])
    eb <- ggplot2::ggplot_build(expected$plot)
    ab <- ggplot2::ggplot_build(actual$plot)
    expect_identical(ab$data, eb$data)
    expect_identical(ab$layout$layout, eb$layout$layout)
    expect_identical(lapply(ab$layout$panel_params, function(x) list(x$x.range, x$y.range)),
      lapply(eb$layout$panel_params, function(x) list(x$x.range, x$y.range)))
    expect_identical(serialize(fixture, NULL), before)
  }
})
