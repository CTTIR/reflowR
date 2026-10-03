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
  annotation = NULL
)
}
\arguments{
\item{type}{\code{tile_heatmap}, \code{effect_points} or \code{numeric_points}.}

\item{mapping}{Named character vector of data column names. Heatmaps require
\code{x}, \code{y}, \code{value}; effects and numeric points require \code{x}, \code{y}, \code{colour}.}

\item{row_key}{Nonempty character vector of complete unique-key columns.}

\item{levels}{Named list of explicit unique character levels: x/y for heatmaps,
y/colour for effects, colour for numeric points, plus facet_rows/facet_columns
for requested facets. Nonempty data require exact coverage of observed levels.}

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

\item{colour_values}{Numeric points only: complete named colour vector whose
unique names exactly match \code{levels$colour}. Colour order follows those levels.}

\item{annotation}{Numeric points only: NULL or a named list with column,
nudge_x, nudge_y, size_pt, check_overlap and show_legend. Nudges must be finite;
size_pt must be finite and at least 7; both policies must be logical scalars.
Text size uses ggplot2's 72.27 points per inch conversion. Annotation records
are retained; clipping and overlap suppression can hide text. Actual visible
label coverage and exported glyph sizes require device and visual checks.}
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
The three numeric-only arguments must remain NULL for existing types.
Their default omission preserves existing recipe structure and behavior.
Numeric coordinates are supplied directly: no aggregation, filtering, jitter,
automatic offsets or implicit reordering is performed. Plotted rows retain
input order; categorical display order follows the declared levels.
}
