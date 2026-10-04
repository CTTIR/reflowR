# Optional numeric layout version; omitted options retain the original recipe.
.rf_wrap_axes <- function(axes) {
  a <- .rf_figure_assert
  a(.rf_numeric_names(axes, c("x", "y")), "Both numeric axes are required")
  result <- axes[c("x", "y")]
  for (axis in names(result)) {
    z <- result[[axis]]
    a(.rf_numeric_names(z, c("limits", "breaks", "labels")), "Invalid numeric axis")
    a(.rf_numeric_vector(z$limits) && length(z$limits) == 2L &&
      z$limits[[1]] < z$limits[[2]], "Invalid numeric limits")
    if (is.null(z$breaks) || is.null(z$labels)) {
      a(is.null(z$breaks) && is.null(z$labels), "Automatic breaks need NULL labels")
      result[[axis]] <- list(limits = z$limits, breaks = z$limits,
        labels = as.character(z$limits))
    }
  }
  result
}

.rf_wrap_recipe <- function(mapping, row_key, levels, facets, labels, midpoint,
    reference, style, status, provenance, display_labels, numeric_axes,
    colour_values, annotation, scatter) {
  a <- .rf_figure_assert
  a(.rf_numeric_names(scatter, c("ncol", "point_alpha", "corner")),
    "Invalid scatter fields")
  a(.rf_figure_scalar(scatter$ncol) && scatter$ncol >= 1 &&
    scatter$ncol <= 100 && scatter$ncol == floor(scatter$ncol),
    "Invalid wrap column count")
  a(.rf_figure_scalar(scatter$point_alpha) && scatter$point_alpha >= 0 &&
    scatter$point_alpha <= 1, "Invalid point alpha")
  a(identical(names(facets), "rows") &&
    .rf_figure_text(facets$rows) && length(facets$rows) == 1L,
    "Wrapped points require one row facet")
  a(is.null(reference) && is.null(annotation),
    "Wrapped points require separate corner annotations and no reference")
  z <- scatter$corner
  a(.rf_numeric_names(z, c("row_key", "column", "hjust", "vjust", "size_pt",
    "lineheight")), "Invalid corner fields")
  a(.rf_figure_text(z$row_key) && length(z$row_key) > 0L &&
    !anyDuplicated(z$row_key) && .rf_figure_text(z$column) &&
    length(z$column) == 1L && !z$column %in% c(z$row_key, unlist(facets)),
    "Invalid corner columns")
  a(.rf_figure_scalar(z$hjust) && .rf_figure_scalar(z$vjust) &&
    .rf_figure_scalar(z$size_pt) && z$size_pt >= 7 &&
    .rf_figure_scalar(z$lineheight) && z$lineheight > 0,
    "Invalid corner geometry or size")
  base <- .rf_numeric_recipe(mapping, row_key, levels, facets, labels, midpoint,
    reference, style, status, provenance, display_labels,
    .rf_wrap_axes(numeric_axes), colour_values, annotation)
  base$schema <- "reflow_numeric_points_wrap_1"
  base$numeric_axes <- lapply(numeric_axes[c("x", "y")], function(x) {
    x[c("limits", "breaks", "labels")]
  })
  base$scatter <- list(ncol = scatter$ncol, point_alpha = scatter$point_alpha,
    corner = z[c("row_key", "column", "hjust", "vjust", "size_pt", "lineheight")])
  base
}

.rf_wrap_plot <- function(data, recipe, annotation_data) {
  a <- .rf_figure_assert
  expected <- c("schema", "type", "mapping", "row_key", "levels", "facets",
    "labels", "midpoint", "reference", "style", "status", "provenance",
    "display_labels", "numeric_axes", "colour_values", "annotation", "scatter")
  a(identical(class(recipe), "reflow_figure_recipe") &&
    identical(recipe$schema, "reflow_numeric_points_wrap_1") &&
    identical(names(recipe), expected), "Invalid wrapped recipe schema")
  rebuilt <- do.call(reflow_figure_recipe, unclass(recipe)[setdiff(expected, "schema")])
  a(identical(recipe, rebuilt), "Wrapped recipe failed revalidation")
  proxy <- recipe
  proxy$scatter <- NULL
  proxy$schema <- "reflow_numeric_points_1"
  proxy$numeric_axes <- .rf_wrap_axes(recipe$numeric_axes)
  validated <- .rf_numeric_plot(data, proxy)
  z <- recipe$scatter$corner
  facet <- recipe$facets$rows
  selected <- unique(c(z$row_key, facet, z$column))
  a(is.data.frame(annotation_data) && !anyDuplicated(names(annotation_data)) &&
    all(selected %in% names(annotation_data)), "Invalid corner data")
  d <- as.data.frame(annotation_data)[selected]
  a(nrow(d) == length(recipe$levels$facet_rows), "Exactly one corner per facet required")
  for (key in selected) {
    a(.rf_figure_text(d[[key]]) && is.null(attributes(d[[key]])),
      "Corner columns must be plain nonmissing text")
  }
  a(!anyDuplicated(d[z$row_key]) && !anyDuplicated(d[[facet]]) &&
    setequal(d[[facet]], recipe$levels$facet_rows), "Corner key or facet coverage differs")
  draw <- data.frame(x = data[[recipe$mapping[["x"]]]],
    y = data[[recipe$mapping[["y"]]]],
    colour = factor(data[[recipe$mapping[["colour"]]]], levels = recipe$levels$colour),
    facet_rows = factor(data[[facet]], levels = recipe$levels$facet_rows))
  corners <- data.frame(label = d[[z$column]],
    facet_rows = factor(d[[facet]], levels = recipe$levels$facet_rows))
  .data <- rlang::.data
  plot <- ggplot2::ggplot(draw, ggplot2::aes(x = .data$x, y = .data$y,
    colour = .data$colour)) +
    ggplot2::geom_point(size = recipe$style$point_size,
      alpha = recipe$scatter$point_alpha) +
    ggplot2::geom_text(data = corners,
      ggplot2::aes(x = -Inf, y = Inf, label = .data$label),
      inherit.aes = FALSE, hjust = z$hjust, vjust = z$vjust,
      size = z$size_pt * 25.4 / 72.27, lineheight = z$lineheight,
      family = recipe$style$family, show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = recipe$colour_values,
      limits = recipe$levels$colour, drop = FALSE)
  for (axis in c("x", "y")) {
    spec <- recipe$numeric_axes[[axis]]
    if (!is.null(spec$breaks)) {
      scale <- if (axis == "x") ggplot2::scale_x_continuous else ggplot2::scale_y_continuous
      plot <- plot + scale(breaks = spec$breaks, labels = spec$labels)
    }
  }
  args <- list(facets = ggplot2::vars(.data$facet_rows),
    ncol = recipe$scatter$ncol, scales = "free_y", drop = FALSE)
  if (length(recipe$display_labels)) {
    args$labeller <- do.call(ggplot2::labeller, recipe$display_labels)
  }
  plot <- plot + do.call(ggplot2::facet_wrap, args) +
    ggplot2::coord_cartesian(xlim = recipe$numeric_axes$x$limits) +
    do.call(ggplot2::labs, recipe$labels) +
    ggplot2::theme_bw(base_size = recipe$style$base_size,
      base_family = recipe$style$family) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = recipe$style$x_angle),
      strip.text = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(), legend.position = "bottom")
  if ("text_size" %in% names(recipe$style)) {
    plot <- plot + ggplot2::theme(text = ggplot2::element_text(size = recipe$style$text_size))
  }
  canonical <- d[do.call(order, c(unname(d[z$row_key]), list(method = "radix"))),
    , drop = FALSE]
  rownames(canonical) <- NULL
  hash <- function(x) {
    digest::digest(serialize(x, NULL, version = 2), algo = "sha256", serialize = FALSE)
  }
  list(plot = plot, schema = "reflow_numeric_wrap_plot_1", status = "available",
    rows = validated$rows, selected_columns = validated$selected_columns,
    payload_sha256 = validated$payload_sha256,
    annotation_payload_sha256 = hash(list(data = canonical, row_key = z$row_key,
      facet = facet, column = z$column)), recipe_sha256 = hash(unclass(recipe)),
    annotation_records = nrow(d), visible_annotations = NA_integer_)
}
