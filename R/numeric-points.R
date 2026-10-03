.rf_numeric_names <- function(x, required, optional = character()) {
  is.list(x) && !is.object(x) && !is.null(names(x)) && !anyNA(names(x)) &&
    !anyDuplicated(names(x)) && all(required %in% names(x)) &&
    all(names(x) %in% c(required, optional))
}
.rf_numeric_vector <- function(x) {
  is.numeric(x) && is.null(attributes(x)) && length(x) > 0L && all(is.finite(x))
}
.rf_numeric_recipe <- function(mapping, row_key, levels, facets, labels, midpoint,
    reference, style, status, provenance, display_labels, numeric_axes,
    colour_values, annotation) {
  a <- .rf_figure_assert
  a(is.null(midpoint) && identical(status, "available"),
    "Numeric points require available status and no midpoint")
  need <- c("x", "y", "colour")
  a(.rf_figure_text(mapping) && !is.null(names(mapping)) &&
    !anyDuplicated(names(mapping)) && setequal(names(mapping), need) &&
    !anyDuplicated(unname(mapping)), "Invalid numeric mapping")
  mapping <- mapping[need]
  a(.rf_figure_text(row_key) && length(row_key) > 0L && !anyDuplicated(row_key),
    "Invalid row key")
  a(is.list(facets) && !is.object(facets) && (length(facets) == 0L ||
    .rf_numeric_names(facets, character(), c("rows", "columns"))), "Invalid facets")
  a(all(vapply(facets, function(x) .rf_figure_text(x) && length(x) == 1L,
    logical(1))) && !anyDuplicated(unlist(facets)), "Invalid facet columns")
  roles <- c("colour", if ("rows" %in% names(facets)) "facet_rows",
    if ("columns" %in% names(facets)) "facet_columns")
  a(.rf_numeric_names(levels, roles) && all(vapply(levels, function(x) {
    .rf_figure_text(x) && length(x) > 0L && !anyDuplicated(x)
  }, logical(1))), "Invalid numeric categorical levels")
  levels <- levels[roles]
  a(.rf_figure_text(colour_values) && !is.null(names(colour_values)) &&
    !anyDuplicated(names(colour_values)) &&
    setequal(names(colour_values), levels$colour), "Invalid named colour values")
  tryCatch(grDevices::col2rgb(colour_values), error = function(e) stop("Invalid colour"))
  colour_values <- colour_values[levels$colour]
  a(.rf_numeric_names(numeric_axes, c("x", "y")), "Both numeric axes are required")
  for (axis in c("x", "y")) {
    z <- numeric_axes[[axis]]
    a(.rf_numeric_names(z, c("limits", "breaks", "labels")), "Invalid numeric axis")
    a(.rf_numeric_vector(z$limits) && length(z$limits) == 2L &&
      z$limits[[1]] < z$limits[[2]], "Invalid numeric limits")
    a(.rf_numeric_vector(z$breaks) && !anyDuplicated(z$breaks) &&
      all(diff(z$breaks) > 0) && all(z$breaks >= z$limits[[1]]) &&
      all(z$breaks <= z$limits[[2]]), "Invalid numeric breaks")
    a(.rf_figure_text(z$labels, TRUE) && length(z$labels) == length(z$breaks),
      "Invalid numeric break labels")
    numeric_axes[[axis]] <- z[c("limits", "breaks", "labels")]
  }
  numeric_axes <- numeric_axes[c("x", "y")]
  a(is.null(reference) || (.rf_figure_scalar(reference) &&
    reference >= numeric_axes$y$limits[[1]] && reference <= numeric_axes$y$limits[[2]]),
    "Invalid horizontal reference")
  if (!is.null(annotation)) {
    keys <- c("column", "nudge_x", "nudge_y", "size_pt", "check_overlap", "show_legend")
    a(.rf_numeric_names(annotation, keys), "Invalid annotation fields")
    a(.rf_figure_text(annotation$column) && length(annotation$column) == 1L,
      "Invalid annotation column")
    a(.rf_figure_scalar(annotation$nudge_x) && .rf_figure_scalar(annotation$nudge_y) &&
      .rf_figure_scalar(annotation$size_pt) && annotation$size_pt >= 7,
      "Invalid annotation geometry or size")
    for (k in c("check_overlap", "show_legend")) {
      a(is.logical(annotation[[k]]) && length(annotation[[k]]) == 1L &&
        is.null(attributes(annotation[[k]])) && !is.na(annotation[[k]]),
        "Invalid annotation policy")
    }
    annotation <- annotation[keys]
  }
  a(is.list(labels) && !is.object(labels) && (length(labels) == 0L ||
    .rf_numeric_names(labels, character(),
      c("title", "subtitle", "x", "y", "fill", "colour", "caption"))), "Invalid labels")
  a(all(vapply(labels, function(x) {
    is.null(x) || (.rf_figure_text(x, TRUE) && length(x) == 1L)
  }, logical(1))), "Labels must be plain text scalars or NULL")
  # Reuse existing plain style/provenance validation without changing its defaults.
  proxy_levels <- c(list(y = "numeric"), levels)
  proxy <- reflow_figure_recipe("effect_points", mapping, row_key, proxy_levels,
    facets, labels[setdiff(names(labels), "subtitle")], reference = 0,
    style = style, provenance = provenance,
    display_labels = display_labels)
  a(all(names(display_labels) %in% c("facet_rows", "facet_columns")),
    "Numeric axes use explicit break labels")
  structure(list(schema = "reflow_numeric_points_1", type = "numeric_points",
    mapping = mapping, row_key = row_key, levels = levels, facets = facets,
    labels = labels, midpoint = NULL, reference = reference, style = proxy$style,
    status = status, provenance = provenance, display_labels = display_labels,
    numeric_axes = numeric_axes, colour_values = colour_values, annotation = annotation),
    class = "reflow_figure_recipe")
}
.rf_numeric_plot <- function(data, recipe) {
  a <- .rf_figure_assert
  expected <- c("schema", "type", "mapping", "row_key", "levels", "facets", "labels",
    "midpoint", "reference", "style", "status", "provenance", "display_labels",
    "numeric_axes", "colour_values", "annotation")
  a(identical(class(recipe), "reflow_figure_recipe") &&
    identical(recipe$schema, "reflow_numeric_points_1") &&
    identical(names(recipe), expected), "Invalid numeric recipe schema")
  rebuilt <- do.call(reflow_figure_recipe, unclass(recipe)[setdiff(expected, "schema")])
  a(identical(recipe, rebuilt), "Numeric recipe failed revalidation")
  a(is.data.frame(data) && !anyDuplicated(names(data)) && nrow(data) > 0L,
    "Numeric points require a nonempty data frame")
  selected <- unique(c(recipe$row_key, unname(recipe$mapping), unlist(recipe$facets),
    recipe$annotation$column))
  a(all(selected %in% names(data)), "Missing selected column")
  d <- as.data.frame(data)[selected]
  numeric_columns <- unname(recipe$mapping[c("x", "y")])
  a(!any(numeric_columns %in% c(recipe$row_key, unlist(recipe$facets),
    recipe$annotation$column)), "Numeric columns cannot be keys, facets or annotations")
  for (k in numeric_columns) {
    a(.rf_numeric_vector(d[[k]]), "Numeric coordinates must be plain finite vectors")
  }
  for (k in setdiff(selected, numeric_columns)) {
    a(.rf_figure_text(d[[k]]) && length(d[[k]]) == nrow(d), "Invalid text column")
  }
  a(!anyDuplicated(d[recipe$row_key]), "Duplicate complete row key")
  role_columns <- list(colour = recipe$mapping[["colour"]])
  if (length(recipe$facets)) {
    role_columns <- c(role_columns,
      stats::setNames(recipe$facets, paste0("facet_", names(recipe$facets))))
  }
  for (role in names(role_columns)) {
    a(setequal(d[[role_columns[[role]]]], recipe$levels[[role]]),
      "Declared levels do not exactly cover observed categories")
  }
  for (axis in c("x", "y")) {
    v <- d[[recipe$mapping[[axis]]]]
    limits <- recipe$numeric_axes[[axis]]$limits
    a(all(v >= limits[[1]] & v <= limits[[2]]), "Coordinates outside supplied limits")
  }
  canonical <- d[do.call(order, c(unname(d[recipe$row_key]), list(method = "radix"))),
    , drop = FALSE]
  rownames(canonical) <- NULL
  hash <- function(x) {
    digest::digest(serialize(x, NULL, version = 2), algo = "sha256", serialize = FALSE)
  }
  a(requireNamespace("ggplot2", quietly = TRUE), "ggplot2 is required")
  .data <- rlang::.data
  draw <- data.frame(x = d[[recipe$mapping[["x"]]]], y = d[[recipe$mapping[["y"]]]],
    colour = factor(d[[recipe$mapping[["colour"]]]], levels = recipe$levels$colour))
  if (!is.null(recipe$annotation)) draw$label <- d[[recipe$annotation$column]]
  for (role in names(recipe$facets)) {
    key <- paste0("facet_", role)
    draw[[key]] <- factor(d[[recipe$facets[[role]]]], levels = recipe$levels[[key]])
  }
  plot <- ggplot2::ggplot(draw, ggplot2::aes(x = .data$x, y = .data$y,
    colour = .data$colour))
  if (!is.null(recipe$reference)) {
    plot <- plot + ggplot2::geom_hline(yintercept = recipe$reference,
      colour = "grey55", linewidth = 0.4)
  }
  plot <- plot + ggplot2::geom_point(size = recipe$style$point_size)
  if (!is.null(recipe$annotation)) {
    z <- recipe$annotation
    plot <- plot + ggplot2::geom_text(ggplot2::aes(label = .data$label),
      nudge_x = z$nudge_x, nudge_y = z$nudge_y, size = z$size_pt * 25.4 / 72.27,
      check_overlap = z$check_overlap, show.legend = z$show_legend,
      family = recipe$style$family)
  }
  plot <- plot + ggplot2::scale_colour_manual(values = recipe$colour_values,
    limits = recipe$levels$colour, drop = FALSE)
  for (axis in c("x", "y")) {
    z <- recipe$numeric_axes[[axis]]
    scale <- if (axis == "x") ggplot2::scale_x_continuous else ggplot2::scale_y_continuous
    plot <- plot + scale(breaks = z$breaks, labels = z$labels, expand = c(0, 0))
  }
  # Coordinate limits retain annotation records beyond the visible panel for explicit auditing.
  plot <- plot + ggplot2::coord_cartesian(xlim = recipe$numeric_axes$x$limits,
    ylim = recipe$numeric_axes$y$limits, expand = FALSE, clip = "on")
  if (length(recipe$facets)) {
    rows <- if ("rows" %in% names(recipe$facets)) ggplot2::vars(.data$facet_rows) else NULL
    cols <- if ("columns" %in% names(recipe$facets)) ggplot2::vars(.data$facet_columns) else NULL
    args <- list(rows = rows, cols = cols, scales = "fixed", space = "fixed", drop = FALSE)
    if (length(recipe$display_labels)) {
      args$labeller <- do.call(ggplot2::labeller, recipe$display_labels)
    }
    plot <- plot + do.call(ggplot2::facet_grid, args)
  }
  plot <- plot + do.call(ggplot2::labs, recipe$labels) +
    ggplot2::theme_bw(base_size = recipe$style$base_size, base_family = recipe$style$family) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = recipe$style$x_angle),
      strip.text.y.right = ggplot2::element_text(angle = recipe$style$strip_y_angle))
  if ("text_size" %in% names(recipe$style)) {
    plot <- plot + ggplot2::theme(text = ggplot2::element_text(size = recipe$style$text_size))
  }
  list(plot = plot, schema = "reflow_numeric_plot_1", status = "available", rows = nrow(d),
    selected_columns = selected, payload_sha256 = hash(list(data = canonical,
      mapping = recipe$mapping, row_key = recipe$row_key, reference = recipe$reference)),
    recipe_sha256 = hash(unclass(recipe)), annotation_records =
      if (is.null(recipe$annotation)) 0L else nrow(d), visible_annotations = NA_integer_)
}
