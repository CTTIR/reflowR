testthat::test_that("finalized recipes retain values and explicit category geometry", {
  heat <- data.frame(
    id = letters[1:4], endpoint = c("b", "a", "a", "b"),
    patient = c("p2", "p1", "p2", "p1"), group = c("B", "A", "B", "A"), value = c(1, 2, 3, 4)
  )
  recipe <- reflow_figure_recipe("tile_heatmap", c(
    x = "endpoint", y = "patient",
    value = "value"
  ), "id", list(x = c("a", "b"), y = c("p1", "p2"), facet_rows = c(
    "A",
    "B"
  )), facets = list(rows = "group"), midpoint = 2.5, labels = list(
    title = "Example",
    fill = "Value"
  ))
  before <- serialize(list(heat, recipe), NULL)
  out <- reflow_figure_plot(heat, recipe)
  expected <- ggplot2::ggplot(transform(heat, endpoint = factor(endpoint, levels = c(
    "a",
    "b"
  )), patient = factor(patient, levels = c("p1", "p2")), group = factor(group,
    levels = c("A", "B")
  )), ggplot2::aes(endpoint, patient, fill = value)) +
    ggplot2::geom_tile(colour = "white", linewidth = .15) +
    ggplot2::facet_grid(group ~ ., scales = "free_y", space = "free_y") +
    ggplot2::scale_fill_gradient2(
      low = "#2166AC", mid = "white", high = "#B2182B",
      midpoint = 2.5
    )
  actual <- ggplot2::ggplot_build(out$plot)
  ref <- ggplot2::ggplot_build(expected)
  for (k in c(
    "x", "y", "xmin", "xmax", "ymin", "ymax", "fill",
    "PANEL"
  )) {
    testthat::expect_equal(actual$data[[1]][[k]], ref$data[[1]][[k]], tolerance = 0)
  }
  testthat::expect_identical(before, serialize(list(heat, recipe), NULL))
  testthat::expect_equal(out$rows, 4L)
  testthat::expect_identical(out$payload_sha256, reflow_figure_plot(
    heat[4:1, ],
    recipe
  )$payload_sha256)
  styled <- recipe
  styled$labels$title <- "Other"
  styled$style$low <- "black"
  styled$provenance <- c(note = "Unverified")
  testthat::expect_identical(out$payload_sha256, reflow_figure_plot(heat, styled)$payload_sha256)
  testthat::expect_false(identical(out$recipe_sha256, reflow_figure_plot(
    heat,
    styled
  )$recipe_sha256))
  shifted <- recipe
  shifted$midpoint <- 3
  testthat::expect_false(identical(out$payload_sha256, reflow_figure_plot(
    heat,
    shifted
  )$payload_sha256))
  empty <- heat[FALSE, ]
  emptyrecipe <- recipe
  emptyrecipe$status <- "empty"
  testthat::expect_null(reflow_figure_plot(empty, emptyrecipe)$plot)
  testthat::expect_error(reflow_figure_plot(empty, recipe), "Status")
  testthat::expect_error(reflow_figure_plot(heat, emptyrecipe), "Status")
  bad <- heat
  bad$value[1] <- NA_real_
  testthat::expect_error(reflow_figure_plot(bad, recipe), "finite")
  bad$value[1] <- Inf
  testthat::expect_error(reflow_figure_plot(bad, recipe), "finite")
  bad$value <- as.character(heat$value)
  testthat::expect_error(reflow_figure_plot(bad, recipe), "numeric")
  bad$value <- I(matrix(heat$value, 4))
  testthat::expect_error(reflow_figure_plot(bad, recipe), "numeric")
  testthat::expect_error(reflow_figure_plot(rbind(heat, heat[1, ]), recipe), "Duplicate")
  bad <- heat
  bad$id[1] <- NA
  testthat::expect_error(reflow_figure_plot(bad, recipe), "keys")
  bad <- recipe
  bad$levels$x <- c("a", "b", "extra")
  testthat::expect_error(reflow_figure_plot(heat, bad), "cover")
  bad <- recipe
  bad$levels$x <- "a"
  testthat::expect_error(reflow_figure_plot(heat, bad), "cover")
  bad <- recipe
  bad$style$unknown <- 1
  testthat::expect_error(reflow_figure_plot(heat, bad), "style")
  bad <- recipe
  bad$labels$title <- quote(system("false"))
  testthat::expect_error(reflow_figure_plot(heat, bad), "plain text")
  bad <- recipe
  bad$extra <- TRUE
  testthat::expect_error(reflow_figure_plot(heat, bad), "fields")
  # Two unique keys at one display location are retained, not aggregated.
  collision <- heat
  collision$endpoint[1] <- "a"
  testthat::expect_equal(nrow(ggplot2::ggplot_build(reflow_figure_plot(
    collision,
    recipe
  )$plot)$data[[1]]), 4L)
  # Selected column names cannot capture internal drawing columns.
  renamed <- heat
  names(renamed) <- c("key", "x", "y", "facet_rows", "fill")
  rr <- reflow_figure_recipe("tile_heatmap", c(x = "x", y = "y", value = "fill"), "key",
    recipe$levels,
    facets = list(rows = "facet_rows"), midpoint = 2.5
  )
  testthat::expect_equal(ggplot2::ggplot_build(reflow_figure_plot(
    renamed,
    rr
  )$plot)$data[[1]]$fill, ref$data[[1]]$fill)
})
testthat::test_that("effect recipes preserve collisions, facets and supplied ties", {
  d <- data.frame(id = letters[1:4], effect = c(1, 3, 2, 2), label = c(
    "same", "same",
    "alpha", "beta"
  ), contrast = c("B", "A", "A", "B"), family = c("f", "f", "g", "g"))
  recipe <- reflow_figure_recipe("effect_points", c(
    x = "effect", y = "label",
    colour = "contrast"
  ), "id", list(
    y = c("alpha", "beta", "same"), colour = c("A", "B"),
    facet_rows = c("f", "g"), facet_columns = c("A", "B")
  ), facets = list(
    rows = "family",
    columns = "contrast"
  ), reference = 0)
  out <- reflow_figure_plot(d, recipe)
  expected <- ggplot2::ggplot(transform(d, label = factor(label, levels = c(
    "alpha",
    "beta", "same"
  ))), ggplot2::aes(effect, label, colour = contrast)) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey60") +
    ggplot2::geom_point(size = 2) +
    ggplot2::facet_grid(family ~ contrast, scales = "free_y", space = "free_y")
  a <- ggplot2::ggplot_build(out$plot)
  b <- ggplot2::ggplot_build(expected)
  for (i in 1:2) {
    for (k in intersect(
      c("x", "y", "colour", "PANEL", "xintercept"),
      names(b$data[[i]])
    )) {
      testthat::expect_equal(a$data[[i]][[k]], b$data[[i]][[k]],
        tolerance = 0
      )
    }
  }
  testthat::expect_equal(nrow(a$data[[2]]), 4L)
  testthat::expect_identical(out$payload_sha256, reflow_figure_plot(
    d[c(4, 2, 1, 3), ],
    recipe
  )$payload_sha256)
  testthat::expect_error(reflow_figure_recipe("effect_points", recipe$mapping, "id",
    recipe$levels,
    facets = recipe$facets, reference = NA_real_
  ), "reference")
  bad <- recipe
  bad$mapping["x"] <- "id"
  testthat::expect_error(reflow_figure_plot(d, bad), "key")
})
testthat::test_that("malformed recipes and ambiguous keys fail without IO", {
  d <- data.frame(
    k1 = c("a\034b", "a"), k2 = c("c", "b\034c"), x = c("x", "x"),
    y = c("y", "y"), v = c(1, 2)
  )
  make <- function(...) {
    reflow_figure_recipe("tile_heatmap", c(x = "x", y = "y", value = "v"), c("k1", "k2"),
      list(x = "x", y = "y"),
      midpoint = 1.5, ...
    )
  }
  r <- make()
  a <- reflow_figure_plot(d, r)
  testthat::expect_equal(nrow(ggplot2::ggplot_build(a$plot)$data[[1]]), 2L)
  testthat::expect_identical(a$payload_sha256, reflow_figure_plot(d[2:1, ], r)$payload_sha256)
  changed <- d
  changed$v[1] <- 1 + .Machine$double.eps
  testthat::expect_false(identical(a$payload_sha256, reflow_figure_plot(
    changed,
    r
  )$payload_sha256))
  bad <- d
  bad$v <- structure(d$v, unit = "arbitrary")
  testthat::expect_error(reflow_figure_plot(bad, r), "plain")
  bad <- d
  bad$y <- factor(bad$y)
  testthat::expect_error(reflow_figure_plot(bad, r), "categories")
  bad <- d
  bad$k1 <- I(matrix(bad$k1, 2))
  testthat::expect_error(reflow_figure_plot(bad, r), "keys")
  bad <- d
  bad$k1[1] <- " "
  testthat::expect_error(reflow_figure_plot(bad, r), "keys")
  bad <- r
  bad$midpoint <- Inf
  testthat::expect_error(reflow_figure_plot(d, bad), "midpoint")
  bad <- r
  bad$levels$x <- c("x", "x")
  testthat::expect_error(reflow_figure_plot(d, bad), "levels")
  testthat::expect_error(make(style = list(base_size = 0)), "size")
  testthat::expect_error(make(style = list(low = "not-a-colour")), "colour")
  testthat::expect_error(make(provenance = setNames("x", "")), "provenance")
  testthat::expect_error(make(labels = list(title = c("a", "b"))), "scalars")
  testthat::expect_error(make(facets = list(rows = "missing")), "Levels")
  testthat::expect_error(make(status = "approved"), "status")
  empty <- reflow_figure_recipe("tile_heatmap", c(x = "x", y = "y", value = "v"), c(
    "k1",
    "k2"
  ), list(x = character(), y = character()), midpoint = NULL, status = "empty")
  testthat::expect_null(reflow_figure_plot(d[FALSE, ], empty)$plot)
  testthat::expect_error(reflow_figure_recipe("unknown", c(x = "x", y = "y", value = "v"),
    "k1", list(x = "x", y = "y"),
    midpoint = 1
  ), "type")
})

testthat::test_that("reserved key names cannot capture order arguments", {
  for (key in c("method", "na.last", "decreasing", "...", ".N", ".SD", "with space")) {
    d <- data.frame(id = c("b", "a"), x = c("x", "x"), y = c("y", "y"), v = c(1, 2))
    names(d)[1] <- key
    r <- reflow_figure_recipe("tile_heatmap", c(x = "x", y = "y", value = "v"), key,
      list(x = "x", y = "y"),
      midpoint = 1.5
    )
    a <- reflow_figure_plot(d, r)
    testthat::expect_equal(a$rows, 2L)
    testthat::expect_identical(a$payload_sha256, reflow_figure_plot(d[2:1, ], r)$payload_sha256)
    d$v[1] <- 999
    testthat::expect_false(identical(a$payload_sha256, reflow_figure_plot(d, r)$payload_sha256))
  }
})
testthat::test_that("recipe scalar declarations are plain and complex stays rejected", {
  make <- function(type = "tile_heatmap", status = "available", midpoint = 1) {
    reflow_figure_recipe(type, c(x = "x", y = "y", value = "v"), "id", list(
      x = "x",
      y = "y"
    ), midpoint = midpoint, status = status)
  }
  testthat::expect_error(make(type = matrix("tile_heatmap", 1)), "type")
  testthat::expect_error(make(type = structure("tile_heatmap", class = "custom")), "type")
  testthat::expect_error(make(status = matrix("available", 1)), "status")
  testthat::expect_error(make(status = structure("available", class = "custom")), "status")
  testthat::expect_error(make(midpoint = 1 + 1i), "midpoint")
  d <- data.frame(id = "a", x = "x", y = "y", v = 1 + 1i)
  testthat::expect_error(reflow_figure_plot(d, make()), "numeric")
})

testthat::test_that("explicit NULL labels remain distinct from empty text", {
  withr::local_pdf(NULL)
  d <- data.frame(id = "a", x = "x", y = "y", value = 1)
  make <- function(label) {
    reflow_figure_recipe("tile_heatmap", c(x = "x", y = "y", value = "value"), "id",
      list(x = "x", y = "y"),
      midpoint = 1, labels = list(x = label)
    )
  }
  removed <- reflow_figure_plot(d, make(NULL))
  empty <- reflow_figure_plot(d, make(""))
  testthat::expect_null(removed$plot$labels$x)
  testthat::expect_identical(empty$plot$labels$x, "")
  testthat::expect_identical(removed$payload_sha256, empty$payload_sha256)
  testthat::expect_false(identical(removed$recipe_sha256, empty$recipe_sha256))
  r <- ggplot2::ggplotGrob(removed$plot)
  e <- ggplot2::ggplotGrob(empty$plot)
  at <- which(r$layout$name == "xlab-b")
  ae <- which(e$layout$name == "xlab-b")
  testthat::expect_s3_class(r$grobs[[at]], "zeroGrob")
  testthat::expect_s3_class(e$grobs[[ae]], "titleGrob")
  testthat::expect_identical(as.character(r$heights[r$layout$t[at]]), "0cm")
  testthat::expect_false(identical(
    as.character(r$heights[r$layout$t[at]]),
    as.character(e$heights[e$layout$t[ae]])
  ))
})


testthat::test_that("right facet strip angle is explicit presentation only", {
  withr::local_pdf(NULL)
  d <- data.frame(id = c("a", "b"), x = c("x", "x"),
    y = c("y", "y"), v = c(1, 2), facet = c("A", "B"))
  make <- function(style = list()) {
    reflow_figure_recipe("tile_heatmap", c(x = "x", y = "y", value = "v"), "id",
      list(x = "x", y = "y", facet_rows = c("A", "B")),
      facets = list(rows = "facet"), midpoint = 1.5, style = style)
  }
  baseline <- reflow_figure_plot(d, make())
  horizontal <- reflow_figure_plot(d, make(list(strip_y_angle = 0)))
  testthat::expect_identical(make()$style$strip_y_angle, -90)
  testthat::expect_identical(baseline$payload_sha256, horizontal$payload_sha256)
  testthat::expect_false(identical(baseline$recipe_sha256, horizontal$recipe_sha256))
  testthat::expect_identical(ggplot2::ggplot_build(baseline$plot)$data,
    ggplot2::ggplot_build(horizontal$plot)$data)
  rotations <- function(grob) {
    out <- if (inherits(grob, "text")) grob$rot else numeric()
    for (child in c(grob$grobs, as.list(grob$children))) {
      out <- c(out, rotations(child))
    }
    out
  }
  strip_rotations <- function(plot) {
    g <- ggplot2::ggplotGrob(plot)
    unlist(lapply(g$grobs[grepl("^strip-r", g$layout$name)], rotations))
  }
  testthat::expect_true(all(strip_rotations(baseline$plot) == -90))
  testthat::expect_true(all(strip_rotations(horizontal$plot) == 0))
  testthat::expect_length(strip_rotations(horizontal$plot), 2L)
  for (angle in list(NA_real_, Inf, NaN, "0", numeric(), c(0, 90), matrix(0))) {
    testthat::expect_error(make(list(strip_y_angle = angle)), "strip angle")
  }
})
