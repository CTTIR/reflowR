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
