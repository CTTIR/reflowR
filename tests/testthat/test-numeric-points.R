numeric_fixture <- function() {
  data.frame(id = c("third", "first", "second"), x = c(2.18, 1, 1.82),
    y = c(1, 0, -1), group = c("B", "A", "B"), page = c("late", "early", "late"),
    label = c("gamma", "alpha", "beta"), status = c("PASS", "TRUE_ZERO", "PASS"))
}
numeric_recipe <- function(annotation = TRUE) {
  reflow_figure_recipe("numeric_points", c(x = "x", y = "y", colour = "group"),
    "id", list(colour = c("A", "B"), facet_columns = c("early", "late")),
    facets = list(columns = "page"), reference = 0,
    numeric_axes = list(x = list(limits = c(0.55, 3.45), breaks = 1:3,
      labels = c("one", "two", "three")),
      y = list(limits = c(-1, 1), breaks = c(-1, 0, 1), labels = c("-1", "0", "1"))),
    colour_values = c(A = "#112233", B = "#445566"),
    annotation = if (annotation) list(column = "label", nudge_x = 0, nudge_y = 0.035,
      size_pt = 7, check_overlap = TRUE, show_legend = FALSE) else NULL,
    style = list(point_size = 2.5, base_size = 8, text_size = 10))
}
test_that("numeric points retain literal supplied geometry, order and annotations", {
  d <- numeric_fixture()
  r <- numeric_recipe()
  before <- serialize(list(d, r), NULL)
  result <- reflow_figure_plot(d, r)
  built <- ggplot2::ggplot_build(result$plot)
  expect_identical(built$data[[2]]$x, c(2.18, 1, 1.82))
  expect_identical(built$data[[2]]$y, c(1, 0, -1))
  expect_identical(built$data[[2]]$colour, c("#445566", "#112233", "#445566"))
  expect_identical(as.integer(built$data[[2]]$PANEL), c(2L, 1L, 2L))
  expect_true(all(built$data[[1]]$yintercept == 0))
  expect_identical(built$data[[3]]$label, c("gamma", "alpha", "beta"))
  expect_equal(built$data[[3]]$y, c(1.035, 0.035, -0.965), tolerance = 1e-15)
  expect_true(result$plot$layers[[3]]$geom_params$check_overlap)
  expect_identical(result$annotation_records, 3L)
  expect_identical(result$visible_annotations, NA_integer_)
  expect_identical(serialize(list(d, r), NULL), before)
  expect_identical(d$status, c("PASS", "TRUE_ZERO", "PASS"))
  expect_identical(result$plot$coordinates$limits$x, c(0.55, 3.45))
  expect_identical(result$plot$coordinates$limits$y, c(-1, 1))
})
test_that("numeric refusal preserves input and recipe", {
  d <- numeric_fixture()
  r <- numeric_recipe()
  cases <- list(
    function(x) {
      x$x[[1]] <- Inf
      x },
    function(x) {
      x$y[[1]] <- NA_real_
      x },
    function(x) {
      x$x <- structure(x$x, extra = 1)
      x },
    function(x) {
      x$id[[1]] <- x$id[[2]]
      x },
    function(x) {
      x$label[[1]] <- ""
      x },
    function(x) {
      x$group[[1]] <- "unknown"
      x },
    function(x) {
      x$x[[1]] <- 4
      x },
    function(x) x[FALSE, ])
  for (change in cases) {
    bad <- change(d)
    before <- serialize(list(bad, r), NULL)
    expect_error(reflow_figure_plot(bad, r))
    expect_identical(serialize(list(bad, r), NULL), before)
  }
  mutations <- list(
    function(x) {
      x$schema <- "unknown"
      x },
    function(x) {
      x$extra <- 1
      x },
    function(x) {
      x$annotation$size_pt <- 6
      x },
    function(x) {
      x$annotation$check_overlap <- NA
      x },
    function(x) {
      x$numeric_axes$y$limits <- c(1, -1)
      x },
    function(x) {
      x$numeric_axes$x$breaks <- c(3, 1, 2)
      x },
    function(x) {
      x$colour_values <- c(A = "red")
      x },
    function(x) {
      x$reference <- 2
      x })
  for (change in mutations) {
    bad <- change(r)
    before <- serialize(list(d, bad), NULL)
    expect_error(reflow_figure_plot(d, bad))
    expect_identical(serialize(list(d, bad), NULL), before)
  }
})
test_that("coincident points remain distinct and annotations are optional", {
  d <- numeric_fixture()
  d$x <- c(1, 1, 1)
  d$y <- c(0, 0, 0)
  r <- numeric_recipe(FALSE)
  result <- reflow_figure_plot(d, r)
  expect_identical(nrow(ggplot2::ggplot_build(result$plot)$data[[2]]), 3L)
  expect_identical(result$annotation_records, 0L)
  reversed <- reflow_figure_plot(d[3:1, ], r)
  expect_identical(reversed$payload_sha256, result$payload_sha256)
  expect_identical(result$rows, 3L)
})
test_that("legacy recipe defaults remain unchanged and reject numeric-only options", {
  r <- reflow_figure_recipe("effect_points", c(x = "x", y = "key", colour = "group"),
    "id", list(y = "row", colour = "A"), reference = 0)
  expect_identical(r$schema, "reflow_finalized_figure_1")
  expect_false(any(c("numeric_axes", "colour_values", "annotation") %in% names(r)))
  expect_error(reflow_figure_recipe("effect_points",
    c(x = "x", y = "key", colour = "group"), "id",
    list(y = "row", colour = "A"), reference = 0, annotation = list()),
    "Numeric options require numeric_points", fixed = TRUE)
})


test_that("numeric points without facets preserve literal geometry", {
  d <- numeric_fixture()
  r <- numeric_recipe()
  r$facets <- list()
  r$levels <- r$levels["colour"]
  before <- serialize(list(d, r), NULL)
  result <- reflow_figure_plot(d, r)
  built <- ggplot2::ggplot_build(result$plot)
  expect_identical(built$data[[2]]$x, c(2.18, 1, 1.82))
  expect_identical(built$data[[2]]$y, c(1, 0, -1))
  expect_identical(built$data[[2]]$colour, c("#445566", "#112233", "#445566"))
  expect_identical(as.integer(built$data[[2]]$PANEL), c(1L, 1L, 1L))
  expect_identical(nrow(built$layout$layout), 1L)
  expect_identical(built$data[[3]]$label, c("gamma", "alpha", "beta"))
  expect_equal(built$data[[3]]$y, c(1.035, 0.035, -0.965), tolerance = 1e-15)
  expect_identical(result$annotation_records, 3L)
  expect_identical(serialize(list(d, r), NULL), before)
})

test_that("row and two-dimensional facets preserve literal order and shared domains", {
  d <- data.frame(id = c("d", "a", "c", "b"), x = c(3, 1, 2, 1.5),
    y = c(1, 0, -1, 0.5), group = c("B", "A", "B", "A"),
    row = c("bottom", "top", "bottom", "top"),
    page = c("late", "early", "early", "late"),
    label = c("delta", "alpha", "gamma", "beta"),
    status = c("PASS", "TRUE_ZERO", "PASS", "PASS"))
  for (two_dimensions in c(FALSE, TRUE)) {
    r <- numeric_recipe()
    r$facets <- list(rows = "row")
    r$levels <- list(colour = c("A", "B"), facet_rows = c("top", "bottom"))
    r$display_labels <- list(facet_rows = c(top = "Top full label", bottom = "Bottom label"))
    if (two_dimensions) {
      r$facets$columns <- "page"
      r$levels$facet_columns <- c("early", "late")
      r$display_labels$facet_columns <- c(early = "Early full label", late = "Late label")
    }
    r$annotation$nudge_x <- 0.1
    r$annotation$nudge_y <- -0.05
    before <- serialize(list(d, r), NULL)
    result <- reflow_figure_plot(d, r)
    b <- ggplot2::ggplot_build(result$plot)
    expect_identical(b$data[[2]]$x, c(3, 1, 2, 1.5))
    expect_identical(b$data[[2]]$y, c(1, 0, -1, 0.5))
    expect_identical(b$data[[2]]$colour, c("#445566", "#112233", "#445566", "#112233"))
    expected_panels <- if (two_dimensions) c(4L, 1L, 3L, 2L) else c(2L, 1L, 2L, 1L)
    expect_identical(as.integer(b$data[[2]]$PANEL), expected_panels)
    expect_identical(b$data[[3]]$label, c("delta", "alpha", "gamma", "beta"))
    expect_equal(b$data[[3]]$x, c(3.1, 1.1, 2.1, 1.6), tolerance = 1e-15)
    expect_equal(b$data[[3]]$y, c(0.95, -0.05, -1.05, 0.45), tolerance = 1e-15)
    expect_identical(nrow(b$data[[1]]), if (two_dimensions) 4L else 2L)
    expect_true(all(b$data[[1]]$yintercept == 0))
    expect_identical(unique(b$layout$layout$SCALE_X), 1L)
    expect_identical(unique(b$layout$layout$SCALE_Y), 1L)
    for (panel in b$layout$panel_params) {
      expect_equal(panel$x.range, c(0.55, 3.45), tolerance = 0)
      expect_equal(panel$y.range, c(-1, 1), tolerance = 0)
    }
    labels <- result$plot$facet$params$labeller(data.frame(facet_rows = c("top", "bottom")))
    expect_identical(unname(unlist(labels)), c("Top full label", "Bottom label"))
    expect_identical(serialize(list(d, r), NULL), before)
    expect_identical(d$status, c("PASS", "TRUE_ZERO", "PASS", "PASS"))
  }
})

test_that("strict numeric refusals identify the violated contract without mutation", {
  d <- numeric_fixture()
  r <- numeric_recipe()
  refuse_data <- function(bad, message) {
    before <- serialize(list(bad, r), NULL)
    expect_error(reflow_figure_plot(bad, r), message, fixed = TRUE)
    expect_identical(serialize(list(bad, r), NULL), before)
  }
  bad <- d
  bad$id <- NULL
  refuse_data(bad, "Missing selected column")
  bad <- d
  bad$id[[1]] <- NA_character_
  refuse_data(bad, "Invalid text column")
  bad <- d
  bad$id[[1]] <- bad$id[[2]]
  refuse_data(bad, "Duplicate complete row key")
  bad <- d
  bad$page[[1]] <- "unrecognized"
  refuse_data(bad, "Declared levels do not exactly cover observed categories")
  for (value in c(Inf, -Inf, NaN, NA_real_)) {
    bad <- d
    bad$y[[1]] <- value
    refuse_data(bad, "Numeric coordinates must be plain finite vectors")
  }
  refuse_recipe <- function(bad, message) {
    before <- serialize(list(d, bad), NULL)
    expect_error(reflow_figure_plot(d, bad), message, fixed = TRUE)
    expect_identical(serialize(list(d, bad), NULL), before)
  }
  for (colours in list(c(A = "red"), c(A = "red", C = "blue"),
    c(A = "red", B = "blue", C = "green"))) {
    bad <- r
    bad$colour_values <- colours
    refuse_recipe(bad, "Invalid named colour values")
  }
  for (value in c(Inf, NaN, NA_real_)) {
    bad <- r
    bad$reference <- value
    refuse_recipe(bad, "Invalid horizontal reference")
    for (axis in c("nudge_x", "nudge_y")) {
      bad <- r
      bad$annotation[[axis]] <- value
      refuse_recipe(bad, "Invalid annotation geometry or size")
    }
  }
  bad <- r
  bad$numeric_axes$x$limits <- c(1, 1)
  refuse_recipe(bad, "Invalid numeric limits")
  bad <- r
  bad$numeric_axes$y$limits <- c(-Inf, Inf)
  refuse_recipe(bad, "Invalid numeric limits")
  bad <- r
  bad$numeric_axes$x$breaks <- c(1, 1, 2)
  refuse_recipe(bad, "Invalid numeric breaks")
  bad <- r
  bad$extra <- TRUE
  refuse_recipe(bad, "Invalid numeric recipe schema")
  bad <- r
  bad$display_labels$facet_columns <- c(early = "Missing late")
  refuse_recipe(bad, "Display labels must cover exact levels")
})

test_that("legacy recipes and built plots match the authenticated published source", {
  baseline <- testthat::test_path("fixtures", "finalized-figures-65fda310.R")
  expect_identical(digest::digest(file = baseline, algo = "sha256"),
    "f0624f98cd479738ac0cb36ec6bf6b533f856f73aba5caadf1b6c1150ef5f32b")
  # Isolated original definitions: no candidate namespace mutation or patched baseline.
  old <- new.env(parent = baseenv())
  sys.source(baseline, envir = old)
  fixtures <- list(
    list(data = data.frame(id = c("b", "a"), x = c("right", "left"),
      y = c("lower", "upper"), value = c(0, -1), facet = c("two", "one")),
      args = list(type = "tile_heatmap", mapping = c(x = "x", y = "y", value = "value"),
        row_key = "id", levels = list(x = c("left", "right"), y = c("upper", "lower"),
          facet_columns = c("one", "two")), facets = list(columns = "facet"),
        midpoint = 0, display_labels = list(x = c(left = "Full left", right = "Full right")),
        style = list(text_size = 10))),
    list(data = data.frame(id = c("b", "a"), x = c(0.5, -0.5),
      y = c("lower", "upper"), group = c("B", "A")),
      args = list(type = "effect_points", mapping = c(x = "x", y = "y", colour = "group"),
        row_key = "id", levels = list(y = c("upper", "lower"), colour = c("A", "B")),
        reference = 0)))
  for (fixture in fixtures) {
    before <- serialize(fixture, NULL)
    expected_recipe <- do.call(old$reflow_figure_recipe, fixture$args)
    actual_recipe <- do.call(reflow_figure_recipe, fixture$args)
    expect_identical(serialize(actual_recipe, NULL), serialize(expected_recipe, NULL))
    expected <- old$reflow_figure_plot(fixture$data, expected_recipe)
    actual <- reflow_figure_plot(fixture$data, actual_recipe)
    expect_identical(actual[names(actual) != "plot"], expected[names(expected) != "plot"])
    expect_identical(actual$plot$data, expected$plot$data)
    expected_build <- ggplot2::ggplot_build(expected$plot)
    actual_build <- ggplot2::ggplot_build(actual$plot)
    expect_identical(actual_build$data, expected_build$data)
    expect_identical(actual_build$layout$layout, expected_build$layout$layout)
    expect_identical(actual$plot$labels, expected$plot$labels)
    expect_identical(serialize(fixture, NULL), before)
  }
})


test_that("two-dimensional facet column labels use full literal display mapping", {
  d <- numeric_fixture()
  d$row <- c("bottom", "top", "bottom")
  r <- numeric_recipe()
  r$facets <- list(rows = "row", columns = "page")
  r$levels <- list(colour = c("A", "B"), facet_rows = c("top", "bottom"),
    facet_columns = c("early", "late"))
  r$display_labels <- list(facet_rows = c(top = "Top full label", bottom = "Bottom label"),
    facet_columns = c(early = "Early full column label", late = "Late full column label"))
  before <- serialize(list(d, r), NULL)
  result <- reflow_figure_plot(d, r)
  built <- ggplot2::ggplot_build(result$plot)
  expect_identical(as.character(built$layout$layout$facet_columns),
    c("early", "late", "early", "late"))
  column_input <- data.frame(facet_columns = c("early", "late"))
  attr(column_input, "type") <- "cols"
  attr(column_input, "facet") <- "grid"
  actual <- result$plot$facet$params$labeller(column_input)
  expect_identical(unname(unlist(actual)),
    c("Early full column label", "Late full column label"))
  expect_identical(serialize(list(d, r), NULL), before)
})


test_that("numeric public labels preserve literal text and finalized payload", {
  data <- numeric_fixture()
  base <- numeric_recipe()
  labels <- list(title = "Literal title", subtitle = "Literal subtitle",
    caption = "Literal caption", x = "", y = NULL, colour = "Group key")
  args <- unclass(base)
  args$schema <- NULL
  args$labels <- labels
  before <- serialize(list(data, base, args, labels), NULL)
  recipe <- do.call(reflow_figure_recipe, args)
  expect_identical(recipe$labels, labels)
  result <- reflow_figure_plot(data, recipe)
  plain <- reflow_figure_plot(data, base)
  expect_identical(result$plot$labels$title, "Literal title")
  expect_identical(result$plot$labels$subtitle, "Literal subtitle")
  expect_identical(result$plot$labels$caption, "Literal caption")
  expect_identical(result$plot$labels$x, "")
  expect_null(result$plot$labels$y)
  expect_identical(result$plot$labels$colour, "Group key")
  expect_identical(result$payload_sha256, plain$payload_sha256)
  expect_false(identical(result$recipe_sha256, plain$recipe_sha256))
  expect_identical(ggplot2::ggplot_build(result$plot)$data,
    ggplot2::ggplot_build(plain$plot)$data)
  repeated <- reflow_figure_plot(data, do.call(reflow_figure_recipe, args))
  expect_identical(repeated$recipe_sha256, result$recipe_sha256)
  expect_identical(repeated$payload_sha256, result$payload_sha256)
  expect_identical(serialize(list(data, base, args, labels), NULL), before)
})

test_that("numeric public labels reject malformed values without mutation", {
  base <- numeric_recipe()
  args <- unclass(base)
  args$schema <- NULL
  bad <- list(NA_character_, character(), c("first", "second"), 1,
    factor("title"), matrix("title"), expression(title), list("title"))
  for (value in bad) {
    candidate <- args
    candidate$labels <- list(title = value)
    before <- serialize(candidate, NULL)
    expect_error(do.call(reflow_figure_recipe, candidate),
      "Labels must be plain text scalars or NULL", fixed = TRUE)
    expect_identical(serialize(candidate, NULL), before)
  }
  for (labels in list(list(unknown = "title"), list("unnamed"),
    stats::setNames(list("one", "two"), c("title", "title")))) {
    candidate <- args
    candidate$labels <- labels
    before <- serialize(candidate, NULL)
    expect_error(do.call(reflow_figure_recipe, candidate), "Invalid labels", fixed = TRUE)
    expect_identical(serialize(candidate, NULL), before)
  }
})
