% Generated documentation candidate; source in R/finalized-figures.R
\name{reflow_figure_recipe}
\alias{reflow_figure_recipe}
\title{Declare a Finalized-Value Figure Recipe}
\usage{
reflow_figure_recipe(
  type, mapping, row_key, levels,
  facets = list(), labels = list(), midpoint = NULL, reference = NULL,
  style = list(), status = "available", provenance = character(),
  display_labels = list(), numeric_axes = NULL, colour_values = NULL,
  annotation = NULL, scatter = NULL,
  tile = NULL
)
}
\arguments{
\item{type}{\code{tile_heatmap}, \code{effect_points}, \code{numeric_points} or \code{numeric_tiles}.}

\item{mapping}{Named character vector of data column names. Heatmaps require
\code{x}, \code{y}, \code{value}; effects and numeric points require \code{x}, \code{y}, \code{colour}.
Numeric tiles require \code{x}, \code{y}, \code{fill}.}

\item{row_key}{Nonempty character vector of complete unique-key columns.}

\item{levels}{Named list of explicit unique character levels: x/y for heatmaps,
y/colour for effects, colour for numeric points, plus facet_rows/facet_columns
for requested facets. Nonempty data require exact coverage of observed levels,
except numeric tiles: levels$fill may contain unused palette categories.}

\item{facets}{Named list with optional rows/columns column names. Heatmap and
effect facets use free y scales and space. Numeric-point facets use fixed
scales and space in both directions. Facets do not aggregate values.}

\item{labels}{Named list of optional title/x/y/fill/colour/caption text;
numeric points also accept subtitle. Explicit NULL removes a label;
empty text remains a distinct label.}

\item{midpoint}{Required finite scalar for available heatmaps; empty heatmaps
may supply NULL. Must be NULL for effects and numeric points.}

\item{reference}{Finite vertical reference for effects; NULL for heatmaps.
Numeric points accept NULL or a finite horizontal reference within y limits.}

\item{style}{Named list of presentation overrides: base_size, family, low, mid,
high, point_size, tile_linewidth, x_angle, strip_y_angle (default -90).
Optional positive finite text_size overrides only root text, preserving geom
defaults and explicit child text sizes. Omission preserves legacy recipes.
No expressions allowed.}

\item{status}{Explicit available or empty for heatmaps/effects, agreeing with
supplied rows. Numeric points require available and nonempty data.}

\item{provenance}{Named character vector of caller declarations, not verified
evidence. It is excluded from the scientific payload hash.}

\item{display_labels}{Optional complete named plaintext maps for categorical
x/y axes or facet_rows/facet_columns. Numeric points permit only facet maps;
numeric axis labels come from numeric_axes. Map keys are original levels.
Newlines and repeated display text are allowed without merging categories.
Omitted roles use identity labels; colour labels are not mapped here.}

\item{numeric_axes}{Numeric points only: named x/y lists, each containing limits
(two strictly increasing finite values), breaks (unique increasing finite
values within limits), and an equally long character labels vector. Both
axes are required. Coordinates outside the declared limits are refused.}

\item{colour_values}{Numeric points or tiles: complete named colour vector whose
unique names exactly match \code{levels$colour} (points) or \code{levels$fill} (tiles).
Colour order follows those levels.}

\item{annotation}{Numeric points only: NULL or a named list with column,
nudge_x, nudge_y, size_pt, check_overlap and show_legend. Nudges must be finite;
size_pt must be finite and at least 7; both policies must be logical scalars.
Text size uses ggplot2's 72.27 points per inch conversion. Annotation records
are retained; clipping and overlap suppression can hide text. Actual visible
label coverage and exported glyph sizes require device and visual checks.}
\item{scatter}{Numeric points only: NULL preserves the existing fixed-scale recipe.
A plain list of ncol (integer-valued 1..100), point_alpha (0..1), and corner
selects free-y facet_wrap with one facets$rows column and declared facet order.
Corner contains row_key, column, finite hjust/vjust, size_pt at least7, and positive
lineheight. Separate annotation_data supplies exactly one uniquely keyed
plaintext row per facet. Display anchors are negative/positive infinity; point
coordinates remain finite. Reference and row-linked annotation must be NULL.
Both numeric_axes limits remain strict finite input envelopes. Paired NULL
breaks/labels request automatic ticks in this mode only. The x viewing range
uses its limits; y trains per panel with ordinary expansion, not a common y
viewing limit. No values or statistics are changed.}
\item{tile}{Numeric tiles only: a list with finite positive width and height,
and legend equal to "observed" or "declared". Optional legend_text_size_pt is
NULL or a positive finite scalar overriding only legend text. Omitted or NULL
values preserve the default tile recipe. Optional axis_text_x_inherit_blank
is NULL or a plain logical scalar. TRUE preserves blank-element inheritance
for numeric x-axis text; omitted, NULL or FALSE retain the previous plotting
default. Omitted and NULL values preserve recipe bytes. Other types are unaffected. Requires mapping x/y/fill,
levels$fill and a complete named colour_values palette. Coordinates stay numeric,
borders are absent and aspect ratio is one. Axes train on tile extents with
ordinary expansion. Observed legends include present categories in declared
order; declared legends include unused levels without adding tiles. Singleton
input is supported, empty input refused. Facets, numeric_axes, annotation,
scatter, reference, midpoint and display_labels are unsupported. Status is
available. Labels accept title/subtitle/x/y/fill/caption. No categories are inferred.}
}
\value{
A versioned recipe; no data are processed or files written.
}
\description{
Supports tile heatmaps, effect points and supplied numeric points. Values,
categorical orders and scales are supplied by the caller. No observations are
filtered, aggregated, fitted, jittered or interpreted as independent units.
}
\details{
Numeric-only options must remain NULL for existing types.
Their default omission preserves existing recipe structure and behavior.
Numeric coordinates are supplied directly: no aggregation, filtering, jitter,
automatic offsets or implicit reordering is performed. Plotted rows retain
input order; categorical display order follows the declared levels.
}
