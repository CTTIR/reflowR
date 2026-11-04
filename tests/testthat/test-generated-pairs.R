generated_comparison <- function() {
  env <- new.env(parent = globalenv())
  code <- generate_formulary_content(reflow_scheme("clinical"))
  for (expr in parse(text = code)) {
    if (is.call(expr) && identical(expr[[1L]], as.name("source"))) next
    eval(expr, envir = env)
  }
  env$wf_compare_groups
}

pair_fixture <- function(difference) {
  n <- length(difference)
  second <- seq_len(n) + 10
  data.frame(
    id = rep(paste0("person-", seq_len(n)), 2),
    group = factor(rep(c("before", "after"), each = n),
                   levels = c("before", "after")),
    value = c(second + difference, second)
  )
}

test_that("generated paired comparisons follow IDs and independent vector tests", {
  skip_if_not_installed("effectsize")
  compare <- generated_comparison()
  for (differences in list(c(-1.2, -0.7, -0.25, 0.15, 0.55, 1.1),
                          c(0.1, 0.2, 0.4, 0.8, 1.6, 3.2, 20, 100))) {
    d <- pair_fixture(differences)
    first <- d$value[d$group == "before"]
    second <- d$value[d$group == "after"]
    normal <- stats::shapiro.test(first - second)$p.value >= 0.05
    oracle <- if (normal) stats::t.test(first, second, paired = TRUE) else
      stats::wilcox.test(first, second, paired = TRUE)
    # Reverse only one group to make row-order pairing demonstrably wrong.
    n <- length(first)
    d <- d[c(seq_len(n), rev(n + seq_len(n))), ]
    before <- serialize(d, NULL)
    actual <- compare(d, "value", "group", paired = TRUE, pair_id = "id")
    expect_identical(names(actual), c("test_name", "test", "effect_size"))
    expect_identical(actual$test_name,
                     if (normal) "Paired t-test" else "Wilcoxon signed-rank")
    expect_equal(actual$test$statistic, oracle$statistic, tolerance = 1e-12)
    expect_equal(actual$test$p.value, oracle$p.value, tolerance = 1e-12)
    expect_identical(actual$test$method, oracle$method)
    delta <- first - second
    if (normal) {
      expected_effect <- mean(delta) / stats::sd(delta)
      expect_equal(actual$effect_size$Cohens_d, expected_effect,
                   tolerance = 1e-12)
    } else {
      ranks <- rank(abs(delta[delta != 0]))
      expected_effect <- sum(sign(delta[delta != 0]) * ranks) / sum(ranks)
      expect_equal(actual$effect_size$r_rank_biserial, expected_effect,
                   tolerance = 1e-12)
    }
    expect_identical(serialize(d, NULL), before)
    reversed <- compare(d[nrow(d):1, ], "value", "group", TRUE, "id")
    expect_equal(reversed$test$statistic, oracle$statistic, tolerance = 1e-12)
    expect_equal(reversed$test$p.value, oracle$p.value, tolerance = 1e-12)
  }
})

test_that("generated paired comparisons reject ambiguous or undefined pairs", {
  compare <- generated_comparison()
  d <- pair_fixture(c(-1.2, -0.7, -0.25, 0.15, 0.55, 1.1))
  expect_error(compare(d, "value", "group", TRUE), "pair_id")
  expect_error(compare(d, "value", "group", TRUE, "absent"), "pair_id")
  invalid <- d
  invalid$id[2] <- invalid$id[1]
  expect_error(compare(invalid, "value", "group", TRUE, "id"), "one observation")
  invalid <- d
  invalid$id[1] <- "unknown"
  expect_error(compare(invalid, "value", "group", TRUE, "id"), "same complete")
  for (missing in c(NA_character_, "")) {
    invalid <- d
    invalid$id[1] <- missing
    expect_error(compare(invalid, "value", "group", TRUE, "id"), "Pair IDs")
  }
  invalid <- d
  invalid$group <- as.character(invalid$group)
  invalid$group[1] <- "third"
  expect_error(compare(invalid, "value", "group", TRUE, "id"), "exactly two")
  invalid$group[1] <- NA_character_
  expect_error(compare(invalid, "value", "group", TRUE, "id"), "nonmissing group")
  for (bad_value in c(NA_real_, Inf)) {
    invalid <- d
    invalid$value[1] <- bad_value
    expect_error(compare(invalid, "value", "group", TRUE, "id"), "finite real")
  }
  constant <- pair_fixture(rep(2, 6))
  expect_error(compare(constant, "value", "group", TRUE, "id"), "nonconstant")
  overflow <- pair_fixture(c(1, 2))
  overflow$value <- c(.Machine$double.xmax, 1, -.Machine$double.xmax, 0)
  expect_error(compare(overflow, "value", "group", TRUE, "id"), "finite, nonconstant")
})

test_that("pair IDs preserve literal identity and unpaired calls remain valid", {
  skip_if_not_installed("effectsize")
  compare <- generated_comparison()
  d <- pair_fixture(c(-1.2, -0.7, -0.25, 0.15, 0.55, 1.1))
  d$id <- rep(c("001", "1", "002", "2", "003", "3"), 2)
  d <- d[c(1:6, 12:7), ]
  observed <- compare(d, "value", "group", TRUE, "id")
  direct <- stats::t.test(d$value[1:6], rev(d$value[7:12]), paired = TRUE)
  expect_equal(observed$test$statistic, direct$statistic, tolerance = 1e-12)
  d <- data.frame(value = rep(c(-1.2, -0.7, -0.25, 0.15, 0.55, 1.1), 3) +
                    rep(c(0, 1, 3), each = 6),
                  group = rep(c("A", "B", "C"), each = 6))
  a <- compare(d, "value", "group")
  b <- compare(d, "value", "group", FALSE, "does-not-affect-unpaired")
  expect_identical(a$test_name, b$test_name)
  expect_identical(stats::coef(a$test), stats::coef(b$test))
  expect_identical(stats::residuals(a$test), stats::residuals(b$test))
  expect_identical(a$effect_size, b$effect_size)
  expect_identical(names(a), c("test_name", "test", "effect_size"))
})
