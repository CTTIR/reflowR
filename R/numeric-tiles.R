# Numeric categorical tiles preserve supplied centres, gaps and record order.
.rf_tiles_recipe <- function(mapping, row_key, levels, facets, labels, midpoint,
    reference, style, status, provenance, display_labels, numeric_axes,
    colour_values, annotation, scatter, tile) {
  a <- .rf_figure_assert
  a(identical(facets, list()) && identical(display_labels, list()) &&
    is.null(midpoint) && is.null(reference) && is.null(numeric_axes) &&
    is.null(annotation) && is.null(scatter), "Unsupported numeric tile options")
  a(identical(status, "available"), "Numeric tiles require available status")
  need <- c("x", "y", "fill")
  a(.rf_figure_text(mapping) && !is.null(names(mapping)) &&
    !anyDuplicated(names(mapping)) && setequal(names(mapping), need) &&
    !anyDuplicated(unname(mapping)), "Invalid numeric tile mapping")
  mapping <- mapping[need]
  a(.rf_figure_text(row_key) && length(row_key) > 0L && !anyDuplicated(row_key) &&
    !any(row_key %in% mapping[c("x", "y")]), "Invalid numeric tile row key")
  a(.rf_numeric_names(levels, "fill") && .rf_figure_text(levels$fill) &&
    is.null(attributes(levels$fill)) && length(levels$fill) > 0L &&
    !anyDuplicated(levels$fill), "Invalid fill levels")
  a(.rf_figure_text(colour_values) && !is.null(names(colour_values)) &&
    !anyDuplicated(names(colour_values)) &&
    setequal(names(colour_values), levels$fill), "Invalid tile palette")
  tryCatch(grDevices::col2rgb(colour_values), error = function(e) stop("Invalid colour"))
  colour_values <- colour_values[levels$fill]
  a(.rf_numeric_names(tile, c("width", "height", "legend"),
    c("legend_text_size_pt", "axis_text_x_inherit_blank")),
    "Invalid tile fields")
  a(.rf_figure_scalar(tile$width) && tile$width > 0 &&
    .rf_figure_scalar(tile$height) && tile$height > 0, "Invalid tile dimensions")
  a(identical(tile$legend, "observed") || identical(tile$legend, "declared"),
    "Invalid tile legend policy")
  legend_size <- tile[["legend_text_size_pt"]]
  a(is.null(legend_size) || (.rf_figure_scalar(legend_size) && legend_size > 0),
    "Invalid tile legend text size")
  inherit_blank <- tile[["axis_text_x_inherit_blank"]]
  a(is.null(inherit_blank) || (is.logical(inherit_blank) &&
    is.null(attributes(inherit_blank)) && length(inherit_blank) == 1L &&
    !is.na(inherit_blank)), "Invalid tile axis text inheritance")
  tile <- tile[c("width", "height", "legend",
    if (!is.null(legend_size)) "legend_text_size_pt",
    if (!is.null(inherit_blank)) "axis_text_x_inherit_blank")]
  a(is.list(labels) && !is.object(labels) && (length(labels) == 0L ||
    .rf_numeric_names(labels, character(), c("title", "subtitle", "x", "y", "fill",
      "caption"))), "Invalid tile labels")
  a(all(vapply(labels, function(x) {
    is.null(x) || (.rf_figure_text(x, TRUE) && length(x) == 1L)
  }, logical(1))), "Labels must be plain text scalars or NULL")
  # Reuse validated style and provenance defaults; the proxy is never plotted.
  proxy <- reflow_figure_recipe("effect_points", c(x = "x", y = "y", colour = "fill"),
    "key", list(y = "row", colour = levels$fill),
    labels = labels[setdiff(names(labels), "subtitle")], reference = 0,
    style = style, provenance = provenance)
  structure(list(schema = "reflow_numeric_tiles_1", type = "numeric_tiles",
    mapping = mapping, row_key = row_key, levels = levels["fill"],
    facets = list(), labels = labels, midpoint = NULL, reference = NULL,
    style = proxy$style, status = status, provenance = provenance,
    display_labels = list(), numeric_axes = NULL, colour_values = colour_values,
    annotation = NULL, scatter = NULL, tile = tile),
    class = "reflow_figure_recipe")
}

.rf_tiles_plot <- function(data, recipe) {
  a <- .rf_figure_assert
  expected <- c("schema", "type", "mapping", "row_key", "levels", "facets", "labels",
    "midpoint", "reference", "style", "status", "provenance", "display_labels",
    "numeric_axes", "colour_values", "annotation", "scatter", "tile")
  a(identical(class(recipe), "reflow_figure_recipe") &&
    identical(recipe$schema, "reflow_numeric_tiles_1") &&
    identical(names(recipe), expected), "Invalid numeric tile schema")
  rebuilt <- do.call(reflow_figure_recipe, unclass(recipe)[setdiff(expected, "schema")])
  a(identical(recipe, rebuilt), "Numeric tile recipe failed revalidation")
  a(is.data.frame(data) && !anyDuplicated(names(data)) && nrow(data) > 0L,
    "Numeric tiles require a nonempty data frame")
  selected <- unique(c(recipe$row_key, unname(recipe$mapping)))
  a(all(selected %in% names(data)), "Missing selected tile column")
  d <- as.data.frame(data)[selected]
  for (axis in c("x", "y")) {
    a(.rf_numeric_vector(d[[recipe$mapping[[axis]]]]), "Invalid numeric tile coordinates")
  }
  for (key in setdiff(selected, unname(recipe$mapping[c("x", "y")]))) {
    a(.rf_figure_text(d[[key]]) && is.null(attributes(d[[key]])),
      "Tile keys and categories must be plain nonmissing text")
  }
  a(!anyDuplicated(d[recipe$row_key]), "Duplicate tile row key")
  fill <- d[[recipe$mapping[["fill"]]]]
  a(all(fill %in% recipe$levels$fill), "Unknown tile fill category")
  x <- d[[recipe$mapping[["x"]]]]
  y <- d[[recipe$mapping[["y"]]]]
  w <- recipe$tile$width
  h <- recipe$tile$height
  a(all(is.finite(c(x - w / 2, x + w / 2, y - h / 2, y + h / 2))) &&
    all(x - w / 2 < x + w / 2) && all(y - h / 2 < y + h / 2),
    "Tile bounds must be finite and distinct")
  shown <- recipe$levels$fill
  if (identical(recipe$tile$legend, "observed")) shown <- shown[shown %in% fill]
  draw <- data.frame(x = x, y = y, fill = factor(fill, levels = shown))
  .data <- rlang::.data
  plot <- ggplot2::ggplot(draw, ggplot2::aes(x = .data$x, y = .data$y, fill = .data$fill)) +
    ggplot2::geom_tile(width = w, height = h, colour = NA) +
    ggplot2::scale_fill_manual(values = recipe$colour_values, limits = shown, drop = FALSE) +
    ggplot2::coord_equal() + do.call(ggplot2::labs, recipe$labels) +
    ggplot2::theme_bw(base_size = recipe$style$base_size,
      base_family = recipe$style$family) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "bottom",
      axis.text.x = ggplot2::element_text(angle = recipe$style$x_angle,
        inherit.blank = isTRUE(recipe$tile[["axis_text_x_inherit_blank"]])))
  if ("text_size" %in% names(recipe$style)) {
    plot <- plot + ggplot2::theme(text = ggplot2::element_text(size = recipe$style$text_size))
  }
  if (!is.null(recipe$tile[["legend_text_size_pt"]])) {
    plot <- plot + ggplot2::theme(legend.text =
      ggplot2::element_text(size = recipe$tile[["legend_text_size_pt"]]))
  }
  canonical <- d[do.call(order, c(unname(d[recipe$row_key]), list(method = "radix"))),
    , drop = FALSE]
  rownames(canonical) <- NULL
  hash <- function(x) {
    digest::digest(serialize(x, NULL, version = 2), algo = "sha256", serialize = FALSE)
  }
  list(plot = plot, schema = "reflow_numeric_tiles_plot_1", status = "available",
    rows = nrow(d), selected_columns = selected,
    payload_sha256 = hash(list(data = canonical, row_key = recipe$row_key,
      mapping = recipe$mapping)), recipe_sha256 = hash(unclass(recipe)))
}
