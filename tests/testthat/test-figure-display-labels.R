testthat::test_that("display maps retain literal categories and finalized geometry", {
  d <- data.frame(id = letters[1:4], x = c("a|b", "a", "a|b", "a"),
    y = c("one", "two", "one", "two"), value = 1:4,
    row = c("r1", "r1", "r2", "r2"), col = c("c1", "c2", "c1", "c2"))
  args <- list(type = "tile_heatmap", mapping = c(x = "x", y = "y", value = "value"),
    row_key = "id", levels = list(x = c("a", "a|b"), y = c("two", "one"),
      facet_rows = c("r2", "r1"), facet_columns = c("c2", "c1")),
    facets = list(rows = "row", columns = "col"), midpoint = 2.5)
  plain <- do.call(reflow_figure_recipe, args)
  testthat::expect_identical(plain, do.call(reflow_figure_recipe,
    c(args, list(display_labels = list()))))
  maps <- list(x = c("a|b" = "same", a = "same"),
    y = c(one = "line\nbreak", two = "literal | delimiter"),
    facet_rows = c(r1 = "row\none", r2 = "row two"),
    facet_columns = c(c1 = "column one", c2 = "column\ntwo"))
  recipe <- do.call(reflow_figure_recipe, c(args, list(display_labels = maps)))
  before <- serialize(list(d, recipe), NULL)
  a <- reflow_figure_plot(d, plain)
  b <- reflow_figure_plot(d, recipe)
  testthat::expect_identical(a$payload_sha256, b$payload_sha256)
  testthat::expect_false(identical(a$recipe_sha256, b$recipe_sha256))
  aa <- ggplot2::ggplot_build(a$plot)
  bb <- ggplot2::ggplot_build(b$plot)
  testthat::expect_identical(aa$data, bb$data)
  testthat::expect_identical(aa$layout$layout, bb$layout$layout)
  testthat::expect_identical(levels(b$plot$data$x), levels(a$plot$data$x))
  for (axis in c("panel_scales_x", "panel_scales_y")) {
    testthat::expect_identical(lapply(aa$layout[[axis]], function(s) s$get_limits()),
      lapply(bb$layout[[axis]], function(s) s$get_limits()))
  }
  expect_literal <- function(actual, expected) {
    testthat::expect_identical(as.character(actual), expected)
    pos <- attr(actual, "pos", exact = TRUE)
    if (!is.null(pos)) testthat::expect_identical(pos, seq_along(expected))
  }
  for (scale in bb$layout$panel_scales_x) {
    expect_literal(scale$get_breaks(), c("a", "a|b"))
    expect_literal(scale$get_labels(), c("same", "same"))
  }
  for (scale in bb$layout$panel_scales_y) {
    expect_literal(scale$get_breaks(), c("two", "one"))
    expect_literal(scale$get_labels(), c("literal | delimiter", "line\nbreak"))
  }
  labels <- data.frame(facet_rows = c("r2", "r1"), facet_columns = c("c2", "c1"))
  actual <- b$plot$facet$params$labeller(labels)
  testthat::expect_identical(unname(unlist(actual[[1]])), c("row two", "row\none"))
  testthat::expect_identical(unname(unlist(actual[[2]])), c("column\ntwo", "column one"))
  testthat::expect_identical(before, serialize(list(d, recipe), NULL))
  reordered <- lapply(maps, rev)
  testthat::expect_identical(recipe, do.call(reflow_figure_recipe,
    c(args, list(display_labels = reordered))))
  for (bad in list(c(a = "one"), c(a = "one", extra = "two"),
    c(a = "one", a = "two"), c("one", "two"),
    c(a = NA_character_, "a|b" = "two"), c(a = "", "a|b" = "two"),
    structure(c(a = "one", "a|b" = "two"), class = "custom"))) {
    testthat::expect_error(do.call(reflow_figure_recipe,
      c(args, list(display_labels = list(x = bad)))), "exact levels")
  }
  testthat::expect_error(do.call(reflow_figure_recipe,
    c(args, list(display_labels = list(other = c(a = "one"))))), "roles")
})

testthat::test_that("effect label maps leave numeric scales and reference unchanged", {
  d <- data.frame(id = c("i1", "i2"), value = c(-2, 3),
    label = c("long one", "long two"), group = c("g1", "g2"))
  args <- list(type = "effect_points", mapping = c(x = "value", y = "label", colour = "group"),
    row_key = "id", levels = list(y = c("long two", "long one"), colour = c("g2", "g1")),
    reference = 0)
  a <- reflow_figure_plot(d, do.call(reflow_figure_recipe, args))
  maps <- list(y = c("long one" = "long\none", "long two" = "long\ntwo"))
  b <- reflow_figure_plot(d, do.call(reflow_figure_recipe,
    c(args, list(display_labels = maps))))
  testthat::expect_identical(ggplot2::ggplot_build(a$plot)$data,
    ggplot2::ggplot_build(b$plot)$data)
  testthat::expect_identical(a$payload_sha256, b$payload_sha256)
  for (role in c("x", "colour", "facet_rows")) {
    testthat::expect_error(do.call(reflow_figure_recipe,
      c(args, list(display_labels = setNames(list(c(g1 = "other")), role)))), "roles")
  }
})
