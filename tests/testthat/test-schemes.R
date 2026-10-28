v5_families <- c("spectral", "spatial", "omics", "clinical", "methods",
                 "simulation", "evidence", "platform", "brand")

shipped_tokens <- function() {
  utils::read.csv(system.file("cttir", "design_tokens.csv", package = "reflowR"),
                  stringsAsFactors = FALSE)
}

# Load the CTTIR branding engine from tools/branding when the source tree is
# available (it is excluded from built packages).
branding_engine <- function() {
  root <- testthat::test_path("..", "..", "tools", "branding")
  files <- file.path(root, c("design_tokens.csv", "R/tokens.R", "R/accessibility.R", "R/palette.R"))
  testthat::skip_if_not(all(file.exists(files)), "tools/branding is not available")
  env <- new.env(parent = baseenv())
  sys.source(file.path(root, "R", "tokens.R"), env)
  env$cttir_tokens <- env$cttir_load_tokens(file.path(root, "design_tokens.csv"))
  sys.source(file.path(root, "R", "accessibility.R"), env)
  sys.source(file.path(root, "R", "palette.R"), env)
  env
}

forget_alias_messages <- function(envir = parent.frame()) {
  cache <- reflowR:::.cttir_cache
  old <- cache$aliases_noted
  cache$aliases_noted <- NULL
  withr::defer(assign("aliases_noted", old, envir = cache), envir = envir)
}

test_that("the schemes are exactly the v5 topic families of the shipped tokens", {
  tokens <- shipped_tokens()
  topics <- tokens$token[tokens$group == "topic"]
  expect_identical(names(reflowR:::.cttir_schemes()), topics)
  expect_setequal(topics, v5_families)
  for (s in v5_families) {
    scheme <- reflow_scheme(s)
    expect_identical(scheme$name, s)
    expect_identical(scheme$family, s)
    expect_identical(scheme$accent, tokens$value[tokens$group == "topic" & tokens$token == s])
    expect_identical(scheme$primary, scheme$accent)
    expect_identical(scheme$navbar_icon, paste0("icon_", s, ".svg"))
  }
})

test_that("every scheme keeps the earlier fields and adds the v5 fields", {
  legacy <- c("name", "label", "navbar_icon", "primary", "primary_light", "accent",
              "secondary", "yellow", "orange", "red", "green", "blue", "light_grey",
              "grey", "dark", "white", "gradient_from", "gradient_to", "palette")
  surfaces <- c("page", "text", "heading", "link", "code_surface", "border", "muted")
  v5 <- c("family", "material", "face_light", "face_dark",
          paste0(surfaces, "_light"), paste0(surfaces, "_dark"),
          "syntax_light", "syntax_dark", "font_body", "font_code", "font_brand",
          "brand_weight")
  for (s in v5_families) {
    scheme <- reflow_scheme(s)
    expect_true(all(c(legacy, v5) %in% names(scheme)))
    colours <- unlist(scheme[setdiff(c(legacy, v5), c("name", "label", "navbar_icon",
      "family", "font_body", "font_code", "font_brand", "brand_weight"))])
    expect_true(all(grepl("^#[0-9A-F]{6}$", colours)))
  }
})

test_that("surfaces, neutrals and typography come from the token table", {
  tokens <- shipped_tokens()
  token <- function(group, name) tokens$value[tokens$group == group & tokens$token == name]
  s <- reflow_scheme("spatial")
  for (mode in c("light", "dark")) {
    for (field in c("page", "text", "heading", "link", "code_surface", "border")) {
      expect_identical(s[[paste0(field, "_", mode)]], token(mode, field))
    }
  }
  expect_identical(s$white, token("light", "page"))
  expect_identical(s$dark, token("light", "heading"))
  expect_identical(s$light_grey, token("light", "code_surface"))
  expect_identical(s$font_body, "Inter")
  expect_identical(s$font_code, "JetBrains Mono")
  expect_identical(s$font_brand, "JetBrains Mono")
  expect_identical(s$brand_weight, 600L)
  expect_identical(c(s$yellow, s$orange, s$red, s$green, s$blue),
                   c(token("topic", "evidence"), token("topic", "simulation"),
                     token("topic", "clinical"), token("topic", "spatial"),
                     token("topic", "spectral")))
})

test_that("labels and derived tints follow the v5 branding rules", {
  engine <- branding_engine()
  for (s in v5_families) {
    scheme <- reflow_scheme(s)
    light <- engine$cttir_style(s, "light")
    dark <- engine$cttir_style(s, "dark")
    expect_identical(scheme$label, engine$cttir_family_labels[[s]])
    expect_identical(scheme$face_light, light$face)
    expect_identical(scheme$face_dark, dark$face)
    expect_identical(scheme$accent, light$rim)
    expect_identical(scheme$material, light$material)
    expect_identical(scheme$primary_light, light$light)
    expect_identical(scheme$secondary, light$deep)
  }
})

test_that("the palette is the Okabe-Ito topic set, accent first, without duplicates", {
  topics <- unique(vapply(v5_families, function(s) reflow_scheme(s)$accent, ""))
  for (s in v5_families) {
    scheme <- reflow_scheme(s)
    expect_length(scheme$palette, 8L)
    expect_identical(scheme$palette[1], scheme$accent)
    expect_false(anyDuplicated(scheme$palette) > 0)
    expect_setequal(scheme$palette, topics)
    # Yellow is the weakest hue on light pages and only leads for its own family.
    if (s != "evidence") expect_identical(scheme$palette[8], "#F0E442")
  }
})

test_that("text colours meet WCAG AA contrast on their surfaces", {
  contrast <- reflowR:::.cttir_contrast
  for (s in v5_families) {
    scheme <- reflow_scheme(s)
    for (mode in c("light", "dark")) {
      field <- function(name) scheme[[paste0(name, "_", mode)]]
      for (surface in c(field("page"), field("code_surface"))) {
        for (ink in c(field("text"), field("heading"), field("link"), field("muted"))) {
          expect_gte(contrast(ink, surface), 4.5)
        }
        for (ink in field("syntax")) expect_gte(contrast(ink, surface), 4.5)
      }
      expect_gte(contrast(field("heading"), scheme[[paste0("face_", mode)]]), 4.5)
    }
  }
})

test_that("earlier scheme names are aliases of their v5 family", {
  forget_alias_messages()
  aliases <- c(basic = "brand", code = "methods", special = "evidence", other = "platform")
  for (old in names(aliases)) {
    expect_message(scheme <- reflow_scheme(old), aliases[[old]])
    expect_identical(scheme, reflow_scheme(aliases[[old]]))
  }
  expect_identical(reflow_scheme("clinical")$accent, "#D55E00")
})

test_that("an alias message is informative and shown once per session", {
  forget_alias_messages()
  expect_message(reflow_scheme("code"), "methods")
  expect_no_message(reflow_scheme("code"))
  expect_no_warning(reflow_scheme("special"))
})

test_that("reflow_scheme errors on unknown scheme", {
  expect_error(reflow_scheme("neon"))
})

test_that("reflow_scheme supports partial matching via match.arg", {
  expect_identical(reflow_scheme("clin")$name, "clinical")
  expect_identical(reflow_scheme("simu")$name, "simulation")
})

test_that("reflow_schemes lists the nine families and mentions the aliases", {
  out <- capture.output(res <- reflow_schemes(), type = "message")
  expect_identical(res, v5_families)
  expect_invisible(reflow_schemes())
  for (s in v5_families) {
    expect_true(any(grepl(s, out, fixed = TRUE)))
    expect_true(any(grepl(reflow_scheme(s)$label, out, fixed = TRUE)))
  }
  expect_true(any(grepl("basic -> brand", out, fixed = TRUE)))
  expect_true(any(grepl("other -> platform", out, fixed = TRUE)))
})

test_that("scheme definitions are stable (snapshot of all schemes)", {
  all_schemes <- lapply(v5_families, reflow_scheme)
  names(all_schemes) <- v5_families
  expect_snapshot_value(all_schemes, style = "json2")
})
