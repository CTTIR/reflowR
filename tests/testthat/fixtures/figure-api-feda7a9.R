.rf_figure_assert <- function(ok, message) {
  if (!isTRUE(ok)) stop(message, call. = FALSE)
}
.rf_figure_text <- function(x, empty = FALSE) {
  is.character(x) && is.null(dim(x)) && !is.object(x) && !anyNA(x) &&
    (empty || all(nzchar(trimws(x))))
}
.rf_figure_scalar <- function(x) {
  is.numeric(x) && length(x) == 1L && is.null(attributes(x)) && is.finite(x)
}

#' Declare a Finalized-Value Figure Recipe
#'
#' Supports tile heatmaps, effect points and supplied numeric points. Values,
#' categorical orders and scales are supplied by the caller. No observations are
#' filtered, aggregated, fitted, jittered or interpreted as independent units.
#' @param type `tile_heatmap`, `effect_points` or `numeric_points`.
#' @param mapping Named character vector of data column names. Heatmaps require
#'   `x`, `y`, `value`; effects and numeric points require `x`, `y`, `colour`.
#' @param row_key Nonempty character vector of complete unique-key columns.
#' @param levels Named list of explicit unique character levels: x/y for heatmaps,
#'   y/colour for effects, colour for numeric points, plus facet_rows/facet_columns
#'   for requested facets. Nonempty data require exact coverage of observed levels.
#' @param facets Named list with optional rows/columns column names. Heatmap and
#'   effect facets use free y scales and space. Numeric-point facets use fixed
#'   scales and space in both directions. Facets do not aggregate values.
#' @param labels Named list of optional title/x/y/fill/colour/caption text;
#'   numeric points also accept subtitle. Explicit NULL removes a label;
#'   empty text remains a distinct label.
#' @param midpoint Required finite scalar for available heatmaps; empty heatmaps
#'   may supply NULL. Must be NULL for effects and numeric points.
#' @param reference Finite vertical reference for effects; NULL for heatmaps.
#'   Numeric points accept NULL or a finite horizontal reference within y limits.
#' @param style Named list of presentation overrides: base_size, family, low, mid,
#'   high, point_size, tile_linewidth, x_angle, strip_y_angle (default -90).
#'   Optional positive finite text_size overrides only root text, preserving geom
#'   defaults and explicit child text sizes. Omission preserves legacy recipes.
#'   No expressions allowed.
#' @param status Explicit available or empty for heatmaps/effects, agreeing with
#'   supplied rows. Numeric points require available and nonempty data.
#' @param provenance Named character vector of caller declarations, not verified
#'   evidence. It is excluded from the scientific payload hash.
#' @param display_labels Optional complete named plaintext maps for categorical
#'   x/y axes or facet_rows/facet_columns. Numeric points permit only facet maps;
#'   numeric axis labels come from numeric_axes. Map keys are original levels.
#'   Newlines and repeated display text are allowed without merging categories.
#'   Omitted roles use identity labels; colour labels are not mapped here.
#' @param numeric_axes Numeric points only: named x/y lists, each containing limits
#'   (two strictly increasing finite values), breaks (unique increasing finite
#'   values within limits), and an equally long character labels vector. Both
#'   axes are required. Coordinates outside the declared limits are refused.
#'   See scatter for its explicitly selected automatic-break mode.
#' @param colour_values Numeric points only: complete named colour vector whose
#'   unique names exactly match levels$colour. Colour order follows those levels.
#' @param annotation Numeric points only: NULL or a named list with column,
#'   nudge_x, nudge_y, size_pt, check_overlap and show_legend. Nudges must be finite;
#'   size_pt must be finite and at least 7; both policies must be logical scalars.
#'   Text size uses ggplot2's 72.27 points per inch conversion. Annotation records
#'   are retained; clipping and overlap suppression can hide text. Actual visible
#'   label coverage and exported glyph sizes require device and visual checks.
#' @param scatter Numeric points only: NULL keeps the existing fixed-scale recipe.
#'   An optional plain list with ncol (integer-valued 1..100), point_alpha (0..1)
#'   and corner selects wrapped free-y panels. Exactly one facets$rows column is
#'   required; facet order is declared by levels$facet_rows. Corner is a plain
#'   list with row_key, column, finite hjust/vjust, size_pt (at least 7), and
#'   positive lineheight. Independent annotation_data must contain exactly one
#'   uniquely keyed, nonmissing plaintext row per facet. Corners use fixed
#'   negative/positive infinite display anchors, without admitting infinite
#'   point coordinates. Reference and row-linked annotation must both be NULL.
#'   Both numeric_axes limits remain strict finite input envelopes. Only this
#'   mode permits paired NULL breaks/labels for automatic ticks. The x limits
#'   also set the visible coordinate range; y trains separately per panel with
#'   ordinary scale expansion and no common y viewing limit. No values change.
#' @details Numeric-only options must remain NULL for existing types.
#'   Their default omission preserves existing recipe structure and behavior.
#'   Numeric coordinates are supplied directly: no aggregation, filtering, jitter,
#'   automatic offsets or implicit reordering is performed. Plotted rows retain
#'   input order; categorical display order follows the declared levels.
#' @return A versioned recipe; no data are processed or files written.
#' @export
reflow_figure_recipe <- function(type, mapping, row_key, levels,
    facets = list(), labels = list(), midpoint = NULL, reference = NULL,
    style = list(), status = "available", provenance = character(),
    display_labels = list(), numeric_axes = NULL, colour_values = NULL,
    annotation = NULL, scatter = NULL) {
  if (identical(type, "numeric_points") && !is.null(scatter)) {
    return(.rf_wrap_recipe(mapping, row_key, levels, facets, labels, midpoint,
      reference, style, status, provenance, display_labels, numeric_axes,
      colour_values, annotation, scatter))
  }
  .rf_figure_assert(is.null(scatter), "Scatter options require numeric_points")
  if (identical(type, "numeric_points")) {
    return(.rf_numeric_recipe(mapping, row_key, levels, facets, labels, midpoint,
      reference, style, status, provenance, display_labels, numeric_axes,
      colour_values, annotation))
  }
  .rf_figure_assert(is.null(numeric_axes) && is.null(colour_values) &&
    is.null(annotation), "Numeric options require numeric_points")
  a <- .rf_figure_assert
  a(.rf_figure_text(type) && length(type) == 1L &&
      type %in% c("tile_heatmap", "effect_points"), "Unsupported figure type")
  need <- if (type == "tile_heatmap") c("x", "y", "value") else c("x", "y", "colour")
  a(.rf_figure_text(mapping) && !is.null(names(mapping)) &&
      !anyDuplicated(names(mapping)) && setequal(names(mapping), need) &&
      !anyDuplicated(unname(mapping)), "Invalid mapping")
  mapping <- mapping[need]
  a(.rf_figure_text(row_key) && length(row_key) > 0L && !anyDuplicated(row_key),
    "Invalid row key")
  named_list <- function(x, allowed) {
    is.list(x) && !is.object(x) && (length(x) == 0L ||
      (!is.null(names(x)) && !anyNA(names(x)) && !anyDuplicated(names(x)) &&
        all(names(x) %in% allowed)))
  }
  a(named_list(facets, c("rows", "columns")), "Invalid facets")
  a(all(vapply(facets, function(x) .rf_figure_text(x) && length(x) == 1L,
    logical(1))), "Invalid facet columns")
  a(!anyDuplicated(unlist(facets, use.names = FALSE)), "Repeated facet column")
  required_levels <- c(if (type == "tile_heatmap") c("x", "y") else c("y", "colour"),
    if ("rows" %in% names(facets)) "facet_rows",
    if ("columns" %in% names(facets)) "facet_columns")
  a(named_list(levels, required_levels) && setequal(names(levels), required_levels),
    "Levels must be explicitly supplied for all categorical roles")
  a(all(vapply(levels, function(x) .rf_figure_text(x) && !anyDuplicated(x),
    logical(1))), "Invalid categorical levels")
  a(named_list(labels, c("title", "x", "y", "fill", "colour", "caption")),
    "Invalid labels")
  a(all(vapply(labels, function(x) {
    is.null(x) || (.rf_figure_text(x, TRUE) && length(x) == 1L)
  }, logical(1))), "Labels must be plain text scalars or NULL")
  defaults <- list(base_size = if (type == "tile_heatmap") 9 else 8, family = "",
    low = "#2166AC", mid = "white", high = "#B2182B", point_size = 2,
    tile_linewidth = 0.15, x_angle = if (type == "tile_heatmap") 55 else 0,
    strip_y_angle = -90)
  a(named_list(style, c(names(defaults), "text_size")), "Invalid style fields")
  if ("text_size" %in% names(style)) {
    a(.rf_figure_scalar(style$text_size) && style$text_size > 0,
      "Invalid text size")
  }
  for (k in names(style)) defaults[[k]] <- style[[k]]
  for (k in c("base_size", "point_size", "tile_linewidth")) {
    a(.rf_figure_scalar(defaults[[k]]) && defaults[[k]] > 0, "Invalid style size")
  }
  a(.rf_figure_scalar(defaults$x_angle), "Invalid axis angle")
  a(.rf_figure_scalar(defaults$strip_y_angle), "Invalid strip angle")
  a(.rf_figure_text(defaults$family, TRUE) && length(defaults$family) == 1L,
    "Invalid font family")
  for (k in c("low", "mid", "high")) {
    a(.rf_figure_text(defaults[[k]]) && length(defaults[[k]]) == 1L,
      "Invalid colour")
    tryCatch(grDevices::col2rgb(defaults[[k]]), error = function(e) stop("Invalid colour"))
  }
  if (type == "tile_heatmap") {
    a((.rf_figure_scalar(midpoint) || (identical(status, "empty") && is.null(midpoint))) &&
        is.null(reference), "Heatmap needs midpoint only")
  } else {
    a(is.null(midpoint) && .rf_figure_scalar(reference), "Effects need reference only")
  }
  a(.rf_figure_text(status) && length(status) == 1L &&
      status %in% c("available", "empty"), "Invalid explicit status")
  a(.rf_figure_text(provenance, TRUE) && (length(provenance) == 0L ||
      (!is.null(names(provenance)) && .rf_figure_text(names(provenance)) &&
        !anyDuplicated(names(provenance)))), "Invalid provenance declarations")
  display_roles <- intersect(c("x", "y", "facet_rows", "facet_columns"), required_levels)
  a(named_list(display_labels, display_roles), "Invalid display label roles")
  for (role in names(display_labels)) {
    map <- display_labels[[role]]
    a(.rf_figure_text(map) && !is.null(names(map)) &&
        .rf_figure_text(names(map)) && !anyDuplicated(names(map)) &&
        setequal(names(map), levels[[role]]), "Display labels must cover exact levels")
    display_labels[[role]] <- map[levels[[role]]]
  }
  display_labels <- display_labels[intersect(display_roles, names(display_labels))]
  recipe <- structure(list(schema = "reflow_finalized_figure_1", type = type,
    mapping = mapping, row_key = row_key, levels = levels[required_levels],
    facets = facets, labels = labels, midpoint = midpoint, reference = reference,
    style = defaults, status = status, provenance = provenance),
    class = "reflow_figure_recipe")
  if (length(display_labels)) recipe$display_labels <- display_labels
  recipe
}

#' Build a Plot from a Finalized-Value Figure Recipe
#' @param data Data frame with plain finite numeric plotted values and nonblank
#'   character keys/categories. Complete row keys must be unique. Extra columns
#'   are ignored without modifying caller input. Repeated display positions remain
#'   distinct keyed rows. Numeric points require nonempty data and preserve input
#'   row order, with complete colour/facet levels and fixed numeric scales.
#' @param recipe A recipe from [reflow_figure_recipe()].
#' @param annotation_data NULL for existing recipes. Wrapped numeric points
#'   require a separate data frame with exactly one corner annotation per
#'   declared facet. Selected keys, facet and label columns must be plain,
#'   nonmissing character vectors without attributes; extra columns are ignored.
#'   Complete keys and facet values must each be unique. Input row order, literal
#'   newlines and supplied status/NA text are preserved; no statistics are computed.
#' @return Versioned list with plot (ggplot, or NULL for explicitly empty legacy
#'   input), status, rows, selected_columns, payload_sha256 and recipe_sha256.
#'   Numeric points additionally return annotation_records (zero when absent)
#'   and visible_annotations = NA_integer_: retained records do not establish
#'   device-visible labels. Clipping and check_overlap can suppress visibility.
#'   The payload hash binds key-sorted selected values and mapping, row keys,
#'   and supplied midpoint/reference as applicable. Styling, level orders, numeric
#'   axis declarations, colour maps and unverified provenance bind the recipe hash.
#'   Extra unselected status columns are not authenticated by these hashes.
#'   Wrapped numeric points additionally return annotation_payload_sha256 for
#'   key-sorted selected annotation values and declared key/facet/label columns.
#'   Neither canonical payload hash records input row order; drawing preserves
#'   that order and order equivalence requires a separate diagnostic assertion.
#'   Hashes use R version-2 serialization, not a cross-language protocol.
#' @details Numeric points draw the supplied coordinates without aggregation,
#'   filtering, fitting, jitter or automatic offsets. Text nudges affect annotations
#'   only. Device-specific visible-label coverage, clipping and minimum exported
#'   font sizes require separate render and visual qualification. No files are written.
#' @export
reflow_figure_plot <- function(data, recipe, annotation_data = NULL) {
  if (is.list(recipe) && identical(recipe$schema, "reflow_numeric_points_wrap_1")) {
    return(.rf_wrap_plot(data, recipe, annotation_data))
  }
  .rf_figure_assert(is.null(annotation_data),
    "Separate annotation data require wrapped numeric points")
  if (is.list(recipe) && identical(recipe$type, "numeric_points")) {
    return(.rf_numeric_plot(data, recipe))
  }
  a <- .rf_figure_assert
  a(inherits(recipe, "reflow_figure_recipe") &&
      identical(recipe$schema, "reflow_finalized_figure_1"), "Invalid recipe schema")
  expected <- c("schema", "type", "mapping", "row_key", "levels", "facets", "labels",
    "midpoint", "reference", "style", "status", "provenance")
  if ("display_labels" %in% names(recipe)) expected <- c(expected, "display_labels")
  a(identical(names(recipe), expected), "Unexpected recipe fields")
  rebuilt <- do.call(reflow_figure_recipe, unclass(recipe)[setdiff(expected, "schema")])
  a(identical(recipe, rebuilt), "Recipe failed revalidation")
  a(is.data.frame(data) && !anyDuplicated(names(data)), "Invalid data frame")
  selected <- unique(c(recipe$row_key, unname(recipe$mapping),
    unlist(recipe$facets, use.names = FALSE)))
  a(all(selected %in% names(data)), "Missing selected column")
  d <- as.data.frame(data)[selected]
  numeric_role <- if (recipe$type == "tile_heatmap") "value" else "x"
  numeric_column <- recipe$mapping[[numeric_role]]
  a(!numeric_column %in% c(recipe$row_key, unlist(recipe$facets)),
    "Numeric plotting column cannot be a key or facet")
  v <- d[[numeric_column]]
  a(is.numeric(v) && is.null(attributes(v)) && all(is.finite(v)),
    "Plotted values must be plain finite numeric vectors")
  for (k in setdiff(selected, numeric_column)) {
    a(.rf_figure_text(d[[k]]) && length(d[[k]]) == nrow(d), "Invalid keys or categories")
  }
  a(!anyDuplicated(d[recipe$row_key]), "Duplicate complete row key")
  a(identical(recipe$status == "empty", nrow(d) == 0L), "Status disagrees with rows")
  role_columns <- as.list(recipe$mapping[setdiff(names(recipe$mapping), numeric_role)])
  if ("rows" %in% names(recipe$facets)) role_columns$facet_rows <- recipe$facets$rows
  if ("columns" %in% names(recipe$facets)) role_columns$facet_columns <- recipe$facets$columns
  for (role in names(role_columns)) {
    if (nrow(d)) a(setequal(d[[role_columns[[role]]]], recipe$levels[[role]]),
      "Declared levels do not exactly cover observed categories")
  }
  # Canonicalize by the complete character key without delimiter concatenation.
  ord <- do.call(order, c(unname(d[recipe$row_key]), list(method = "radix")))
  canonical <- d[ord, , drop = FALSE]
  rownames(canonical) <- NULL
  hash <- function(x) {
    digest::digest(serialize(x, NULL, version = 2), algo = "sha256", serialize = FALSE)
  }
  payload <- hash(list(data = canonical, mapping = recipe$mapping,
    row_key = recipe$row_key, midpoint = recipe$midpoint, reference = recipe$reference))
  audit <- list(schema = "reflow_finalized_plot_1", status = recipe$status,
    rows = nrow(d), selected_columns = selected, payload_sha256 = payload,
    recipe_sha256 = hash(unclass(recipe)))
  if (!nrow(d)) return(c(list(plot = NULL), audit))
  a(requireNamespace("ggplot2", quietly = TRUE), "ggplot2 is required")
  .data <- rlang::.data
  # Internal roles avoid column-name capture and retain caller data unchanged.
  draw <- data.frame(x = d[[recipe$mapping[["x"]]]],
    y = factor(d[[recipe$mapping[["y"]]]], levels = recipe$levels$y))
  if (recipe$type == "tile_heatmap") {
    draw$x <- factor(draw$x, levels = recipe$levels$x)
    draw$value <- d[[recipe$mapping[["value"]]]]
    plot <- ggplot2::ggplot(draw, ggplot2::aes(x = .data$x, y = .data$y, fill = .data$value)) +
      ggplot2::geom_tile(colour = "white", linewidth = recipe$style$tile_linewidth) +
      ggplot2::scale_fill_gradient2(low = recipe$style$low, mid = recipe$style$mid,
        high = recipe$style$high, midpoint = recipe$midpoint)
  } else {
    draw$colour <- factor(d[[recipe$mapping[["colour"]]]], levels = recipe$levels$colour)
    plot <- ggplot2::ggplot(draw, ggplot2::aes(x = .data$x, y = .data$y,
      colour = .data$colour)) +
      ggplot2::geom_vline(xintercept = recipe$reference, colour = "grey60") +
      ggplot2::geom_point(size = recipe$style$point_size)
  }
  for (role in c("rows", "columns")) {
    if (role %in% names(recipe$facets)) {
      key <- paste0("facet_", role)
      plot$data[[key]] <- factor(d[[recipe$facets[[role]]]], levels = recipe$levels[[key]])
    }
  }
  if (length(recipe$facets)) {
    rows <- if ("rows" %in% names(recipe$facets)) ggplot2::vars(.data$facet_rows) else NULL
    cols <- if ("columns" %in% names(recipe$facets)) ggplot2::vars(.data$facet_columns) else NULL
    facet_maps <- recipe$display_labels[intersect(
      c("facet_rows", "facet_columns"), names(recipe$display_labels)
    )]
    facet_args <- list(rows = rows, cols = cols, scales = "free_y", space = "free_y")
    if (length(facet_maps)) facet_args$labeller <- do.call(ggplot2::labeller, facet_maps)
    plot <- plot + do.call(ggplot2::facet_grid, facet_args)
  }
  if ("x" %in% names(recipe$display_labels)) {
    plot <- plot + ggplot2::scale_x_discrete(labels = recipe$display_labels$x)
  }
  if ("y" %in% names(recipe$display_labels)) {
    plot <- plot + ggplot2::scale_y_discrete(labels = recipe$display_labels$y)
  }
  plot <- plot + do.call(ggplot2::labs, recipe$labels) +
    ggplot2::theme_bw(base_size = recipe$style$base_size, base_family = recipe$style$family) +
    if (recipe$type == "tile_heatmap") {
      ggplot2::theme(panel.grid = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(angle = recipe$style$x_angle, hjust = 1, size = 7))
    } else {
      ggplot2::theme(legend.position = "none",
        axis.text.x = ggplot2::element_text(angle = recipe$style$x_angle))
    }
  plot <- plot + ggplot2::theme(
    strip.text.y.right = ggplot2::element_text(angle = recipe$style$strip_y_angle))
  if ("text_size" %in% names(recipe$style)) {
    plot <- plot + ggplot2::theme(text = ggplot2::element_text(size = recipe$style$text_size))
  }
  c(list(plot = plot), audit)
}

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
