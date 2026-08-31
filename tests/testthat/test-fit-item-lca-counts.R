test_that("fit_item_lca_counts has a stable public contract", {
  expect_named(
    formals(fit_item_lca_counts),
    c("transition_counts", "...", "start", "control")
  )
  expect_false(exists("lca_cor", envir = asNamespace("guess"), inherits = FALSE))

  counts <- matrix(
    c(40L, 30L, 10L, 20L),
    nrow = 1L,
    dimnames = list("algebra", c("x00", "x01", "x10", "x11"))
  )
  expect_error(fit_item_lca_counts(counts, NULL), "must be empty")
  expect_error(fit_item_lca_counts(counts, control = 1), "must be a list")
})

test_that("binary counts recover an interior analytic solution", {
  counts <- matrix(
    c(40L, 30L, 10L, 20L),
    nrow = 1L,
    dimnames = list("algebra", c("x00", "x01", "x10", "x11"))
  )

  fit <- fit_item_lca_counts(counts)

  expect_s3_class(fit, "guess_fit")
  expect_equal(
    fit$params[, "algebra"],
    c(gg = 0.625, gk = 0.25, kk = 0.125, gamma = 0.2),
    tolerance = 1e-5
  )
  expect_equal(fit$learning, c(algebra = 0.25), tolerance = 1e-5)
  expect_equal(fit$n_items, 1L)
  expect_equal(fit$n_obs, 100)
  expect_equal(
    fit$diagnostics$objective,
    guess_lik(fit$params[, "algebra"], data = counts["algebra", ]),
    tolerance = 1e-8
  )
  expect_equal(fit$diagnostics$convergence, 0L)
  expect_true(fit$diagnostics$evaluations > 0L)
  expect_true(fit$diagnostics$iterations > 0L)
})

test_that("canonical count cells are matched by name", {
  counts <- matrix(
    c(40L, 30L, 10L, 20L),
    nrow = 1L,
    dimnames = list("algebra", c("x00", "x01", "x10", "x11"))
  )
  permuted <- counts[, c("x11", "x10", "x01", "x00"), drop = FALSE]

  canonical_fit <- fit_item_lca_counts(counts)
  permuted_fit <- fit_item_lca_counts(permuted)

  expect_equal(permuted_fit$params, canonical_fit$params, tolerance = 1e-8)
  expect_equal(permuted_fit$learning, canonical_fit$learning, tolerance = 1e-8)
})

test_that("transition counts reject malformed or uninformative data", {
  valid <- matrix(
    c(10, 5, 2, 8),
    nrow = 1L,
    dimnames = list("item", c("x00", "x01", "x10", "x11"))
  )

  expect_error(fit_item_lca_counts(data.frame(valid)), "matrix")
  expect_error(
    fit_item_lca_counts(unname(valid)),
    "canonical transition-cell names"
  )

  wrong_cells <- valid
  colnames(wrong_cells)[1] <- "wrong"
  expect_error(fit_item_lca_counts(wrong_cells), "canonical transition-cell names")

  unnamed_item <- valid
  rownames(unnamed_item) <- NULL
  expect_error(fit_item_lca_counts(unnamed_item), "unique, non-empty item names")

  duplicate_items <- rbind(valid, valid)
  rownames(duplicate_items) <- c("item", "item")
  expect_error(fit_item_lca_counts(duplicate_items), "unique, non-empty item names")

  negative <- valid
  negative[1, 1] <- -1
  expect_error(fit_item_lca_counts(negative), "negative counts")

  fractional <- valid
  fractional[1, 1] <- 1.5
  expect_error(fit_item_lca_counts(fractional), "whole-number counts")

  missing <- valid
  missing[1, 1] <- NA
  expect_error(fit_item_lca_counts(missing), "finite, non-missing")

  zero <- valid
  zero[] <- 0
  expect_error(fit_item_lca_counts(zero), "at least one transition")
})

test_that("DK counts return the documented parameters and learning estimand", {
  sim <- simulate_lca_dk(
    n = 5000,
    n_items = 2,
    gg = 0.25,
    gk = 0.15,
    gd = 0.10,
    kk = 0.20,
    dg = 0.10,
    dk = 0.10,
    dd = 0.10,
    gamma = c(0.20, 0.35),
    seed = 712
  )
  counts <- count_item_transitions(sim$pre, sim$post)

  fit <- fit_item_lca_counts(counts)

  expect_equal(
    rownames(fit$params),
    c("gg", "gk", "gd", "kk", "dg", "dk", "dd", "gamma")
  )
  expect_equal(fit$learning, fit$params["gk", ] + fit$params["dk", ])
  expect_lte(
    max(abs(fit$params["gamma", ] - c(item1 = 0.20, item2 = 0.35))),
    0.04
  )
  expect_true(all(fit$diagnostics$convergence == 0L))
})

test_that("supplied starts are named, feasible, and solution-invariant", {
  counts <- matrix(
    c(40L, 30L, 10L, 20L),
    nrow = 1L,
    dimnames = list("algebra", c("x00", "x01", "x10", "x11"))
  )
  start <- c(gg = 0.4, gk = 0.3, kk = 0.3, gamma = 0.4)

  default_fit <- fit_item_lca_counts(counts)
  supplied_fit <- fit_item_lca_counts(counts, start = start)
  expect_equal(supplied_fit$params, default_fit$params, tolerance = 1e-5)

  expect_error(
    fit_item_lca_counts(counts, start = unname(start)),
    "finite named numeric vector"
  )
  expect_error(
    fit_item_lca_counts(counts, start = c(gg = 0.4, gk = 0.3, kk = 0.2, gamma = 0.4)),
    "sum to 1"
  )
  invalid_bound <- start
  invalid_bound[["gamma"]] <- 1.1
  expect_error(
    fit_item_lca_counts(counts, start = invalid_bound),
    "between 0 and 1"
  )
})

test_that("automatic starts are feasible at unidentified and boundary ratios", {
  parameter_names <- c("gg", "gk", "kk", "gamma")
  unidentified <- make_lca_start(
    c(x00 = 0, x01 = 10, x10 = 0, x11 = 0),
    parameter_names
  )
  boundary <- make_lca_start(
    c(x00 = 0, x01 = 0, x10 = 10, x11 = 0),
    parameter_names
  )

  expect_equal(sum(unidentified[c("gg", "gk", "kk")]), 1)
  expect_equal(unidentified[["gamma"]], 0.5)
  expect_gt(boundary[["gamma"]], 0)
  expect_lt(boundary[["gamma"]], 1)
})

test_that("aggregate counts are fitted separately from items", {
  pre_test <- data.frame(
    algebra = c(0, 0, 1, 1),
    reading = c(1, 1, 0, 0)
  )
  post_test <- data.frame(
    algebra = c(0, 1, 1, 1),
    reading = c(1, 0, 1, 0)
  )
  counts <- count_item_transitions(
    pre_test,
    post_test,
    include_aggregate = TRUE
  )

  fit <- fit_item_lca_counts(counts)

  expect_equal(colnames(fit$params), c("algebra", "reading"))
  expect_equal(fit$n_items, 2L)
  expect_equal(fit$n_obs, sum(counts[c("algebra", "reading"), ]))
  expect_named(fit$aggregate$params, rownames(fit$params))
  expect_length(fit$aggregate$learning, 1L)
  expect_equal(fit$aggregate$diagnostics$convergence, 0L)

  aggregate_only <- counts["aggregate", , drop = FALSE]
  expect_error(
    fit_item_lca_counts(aggregate_only),
    "at least one non-aggregate item"
  )
})

test_that("optimizer failures identify the affected item", {
  counts <- matrix(
    c(40L, 30L, 10L, 20L),
    nrow = 1L,
    dimnames = list("algebra", c("x00", "x01", "x10", "x11"))
  )

  expect_error(
    fit_item_lca_counts(counts, control = list(outer.iter = 1L)),
    "did not converge for item `algebra`"
  )
})
