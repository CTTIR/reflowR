# Write the reflowR site scheme icons, one CTTIR v5 family hex per topic key.
#
#   Rscript tools/branding/family-icons.R            # replaces inst/icons
#   Rscript tools/branding/family-icons.R /path/dir  # review copy
#
# Each icon is the pointy-top v5 frame on a light surface: a face mixed from
# the family accent and the page, a solid accent rim, a transparent exterior
# and a small line glyph in the family edge ink. Colours come from
# cttir_style(); the SVG text is written directly so the files stay small.
run_family_icons <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  script <- sub("^--file=", "", args[grepl("^--file=", args)])
  if (length(script) != 1L) stop("Run this file with Rscript.")
  branding <- dirname(normalizePath(script, winslash = "/", mustWork = TRUE))
  repository <- normalizePath(file.path(branding, "..", ".."),
                              winslash = "/", mustWork = TRUE)
  params <- commandArgs(trailingOnly = TRUE)
  if (length(params) > 1L) stop("Supply at most one output directory.")
  output <- if (length(params)) params else file.path(repository, "inst", "icons")
  dir.create(output, recursive = TRUE, showWarnings = FALSE)

  engine <- new.env(parent = baseenv())
  sys.source(file.path(branding, "R", "tokens.R"), engine)
  engine$cttir_tokens <- engine$cttir_load_tokens(file.path(branding, "design_tokens.csv"))
  for (file in c("accessibility.R", "palette.R")) {
    sys.source(file.path(branding, "R", file), engine)
  }
  families <- names(engine$cttir_accents)

  num <- function(x) formatC(x, format = "f", digits = 2, drop0trailing = TRUE)
  points <- function(x, y) paste(num(x), num(y), sep = ",", collapse = " ")
  line <- function(x, y) sprintf('<polyline points="%s"/>', points(x, y))
  dot <- function(x, y, r = 2.6) sprintf('<circle cx="%s" cy="%s" r="%s" fill="currentColor" stroke="none"/>',
                                          num(x), num(y), num(r))

  # Frame: 56 x 64 units, centre (28, 32), circumradius 30.5, rim 3.
  cx <- 28
  cy <- 32
  angles <- (90 + 0:5 * 60) * pi / 180
  frame <- points(cx + 30.5 * cos(angles), cy - 30.5 * sin(angles))

  # Glyphs stay inside a radius of about 15 units around the centre.
  glyphs <- list(
    # A two-peak emission spectrum above its baseline.
    spectral = {
      x <- seq(16, 40, length.out = 37)
      y <- 40 - 15 * exp(-((x - 24) / 2.6)^2) - 9 * exp(-((x - 33) / 2.4)^2)
      c(line(c(15, 41), c(42.5, 42.5)), line(x, y))
    },
    # Regularly spaced tissue positions.
    spatial = unlist(lapply(c(-8, 0, 8), function(dy)
      vapply(c(-8, 0, 8), function(dx) dot(cx + dx, cy + dy, 2.8), ""))),
    # Two helical strands with paired bases.
    omics = {
      t <- seq(0, 2 * pi, length.out = 33)
      y <- 18 + 28 * t / (2 * pi)
      c(line(cx + 7.5 * sin(t), y), line(cx - 7.5 * sin(t), y),
        line(c(cx - 7.5, cx + 7.5), c(25, 25)), line(c(cx - 7.5, cx + 7.5), c(39, 39)))
    },
    # A clinical cross.
    clinical = c(sprintf('<polyline points="%s" stroke-width="6.5"/>', points(c(cx, cx), c(21, 43))),
                 sprintf('<polyline points="%s" stroke-width="6.5"/>', points(c(17, 39), c(cy, cy)))),
    # Observations and a fitted line.
    methods = c(line(c(16, 40), c(42, 22)),
                dot(19, 34), dot(25, 42), dot(30, 24.5), dot(37, 32)),
    # A closed loop with direction: a system advancing in steps.
    simulation = {
      t <- seq(70, 380, length.out = 32) * pi / 180
      x <- cx + 10 * cos(t)
      y <- cy - 10 * sin(t)
      tip <- c(x[32], y[32])
      c(line(x, y), sprintf('<polygon points="%s" fill="currentColor" stroke="none"/>',
        points(tip[1] + c(-4.2, 4.2, 1.6), tip[2] + c(-3.2, -1.4, 5.2))))
    },
    # A lens over the record.
    evidence = c(sprintf('<circle cx="%s" cy="%s" r="8.5"/>', num(25.5), num(29.5)),
                 line(c(31.6, 39), c(35.6, 43))),
    # Stacked layers of shared infrastructure.
    platform = c(sprintf('<polygon points="%s"/>', points(c(17, cx, 39, cx), c(26, 20.5, 26, 31.5))),
                 line(c(17, cx, 39), c(32.5, 38, 32.5)), line(c(17, cx, 39), c(39, 44.5, 39))),
    # One centre connected to three partners.
    brand = {
      a <- (90 + c(0, 120, 240)) * pi / 180
      x <- cx + 11 * cos(a)
      y <- cy - 11 * sin(a)
      c(vapply(1:3, function(i) line(c(cx, x[i]), c(cy, y[i])), ""),
        dot(cx, cy, 3.6), vapply(1:3, function(i) dot(x[i], y[i], 3), ""))
    }
  )
  if (!setequal(names(glyphs), families)) stop("Every v5 family needs exactly one glyph.")

  for (family in families) {
    s <- engine$cttir_style(family, "light")
    svg <- c(
      sprintf(paste0('<svg xmlns="http://www.w3.org/2000/svg" width="56" height="64" ',
                     'viewBox="0 0 56 64" role="img" aria-label="%s">'),
              gsub("&", "&amp;", engine$cttir_family_labels[[family]], fixed = TRUE)),
      sprintf('<polygon points="%s" fill="%s" stroke="%s" stroke-width="3" stroke-linejoin="round"/>',
              frame, s$face, s$rim),
      sprintf(paste0('<g color="%s" fill="none" stroke="currentColor" stroke-width="3" ',
                     'stroke-linecap="round" stroke-linejoin="round">'), s$edge),
      paste0("  ", glyphs[[family]]),
      "</g>",
      "</svg>"
    )
    writeLines(svg, file.path(output, paste0("icon_", family, ".svg")), useBytes = TRUE)
  }
  message("Wrote ", length(families), " family icons to ", output)
}

run_family_icons()
