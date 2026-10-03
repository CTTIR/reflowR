testthat::test_that("text size changes text while retaining finalized layers", {
  d <- data.frame(id = c("one", "two"), x = c("a", "b"), y = c("u", "v"),
    value = c(-1, 2), group = c("g1", "g2"))
  for (type in c("tile_heatmap", "effect_points")) {
    heat <- type == "tile_heatmap"
    args <- list(type = type, mapping = if (heat) c(x = "x", y = "y", value = "value") else
      c(x = "value", y = "y", colour = "x"), row_key = "id",
      levels = if (heat) list(x = c("b", "a"), y = c("v", "u")) else
        list(y = c("v", "u"), colour = c("b", "a")))
    args$facets <- list(rows = "group")
    args$levels$facet_rows <- c("g2", "g1")
    args[[if (heat) "midpoint" else "reference"]] <- 0
    recipe <- do.call(reflow_figure_recipe, args)
    testthat::expect_false("text_size" %in% names(recipe$style))
    before <- serialize(list(d, recipe), NULL)
    a <- reflow_figure_plot(d, recipe)
    args$style <- list(text_size = 10)
    changed <- do.call(reflow_figure_recipe, args)
    b <- reflow_figure_plot(d, changed)
    aa <- ggplot2::ggplot_build(a$plot)
    bb <- ggplot2::ggplot_build(b$plot)
    testthat::expect_identical(aa$data, bb$data)
    testthat::expect_identical(aa$layout$layout, bb$layout$layout)
    for (axis in c("panel_scales_x", "panel_scales_y")) {
      testthat::expect_identical(lapply(aa$layout[[axis]], function(s) s$get_limits()),
        lapply(bb$layout[[axis]], function(s) s$get_limits()))
    }
    testthat::expect_identical(a$payload_sha256, b$payload_sha256)
    testthat::expect_false(identical(a$recipe_sha256, b$recipe_sha256))
    testthat::expect_identical(ggplot2::calc_element("text", b$plot$theme)$size, 10)
    testthat::expect_false(identical(ggplot2::calc_element("text", a$plot$theme)$size,
      ggplot2::calc_element("text", b$plot$theme)$size))
    if (heat) testthat::expect_identical(
      ggplot2::calc_element("axis.text.x", b$plot$theme)$size, 7)
    testthat::expect_identical(before, serialize(list(d, recipe), NULL))
    for (bad in list(NULL, NA_real_, NaN, Inf, 0, -1, "10", c(8, 10),
      structure(10, class = "custom"), matrix(10))) {
      args$style <- list(text_size = bad)
      testthat::expect_error(do.call(reflow_figure_recipe, args), "Invalid text size")
    }
  }
})
