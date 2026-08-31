test_that("simulate_lca produces valid output structure", {
  sim <- simulate_lca(n = 100, n_items = 2, seed = 123)

  expect_true(is.list(sim))
  expect_true(all(c("pre", "post") %in% names(sim)))
  expect_true(is.data.frame(sim$pre))
  expect_true(is.data.frame(sim$post))
  expect_equal(nrow(sim$pre), 100)
  expect_equal(nrow(sim$post), 100)
  expect_equal(ncol(sim$pre), 2)
  expect_equal(ncol(sim$post), 2)
  expect_equal(names(sim$pre), c("item1", "item2"))
  expect_equal(names(sim$post), c("item1", "item2"))
})

test_that("simulate_lca produces valid response values", {
  sim <- simulate_lca(n = 500, n_items = 3, seed = 456)

  for (col in names(sim$pre)) {
    expect_true(all(sim$pre[[col]] %in% c(0, 1)))
    expect_true(all(sim$post[[col]] %in% c(0, 1)))
  }
})

test_that("simulate_lca normalizes parameters", {
  expect_warning(
    sim <- simulate_lca(n = 100, gg = 0.5, gk = 0.5, kk = 0.5, seed = 123),
    "normalized"
  )
  expect_true(is.list(sim))
})

test_that("simulate_lca seed produces reproducible results", {
  sim1 <- simulate_lca(n = 100, seed = 999)
  sim2 <- simulate_lca(n = 100, seed = 999)

  expect_identical(sim1$pre, sim2$pre)
  expect_identical(sim1$post, sim2$post)
})

test_that("simulate_lca_dk produces valid DK output", {
  sim <- simulate_lca_dk(n = 100, n_items = 2, seed = 123)

  expect_true(is.list(sim))
  expect_true(all(c("pre", "post") %in% names(sim)))
  expect_equal(nrow(sim$pre), 100)
  expect_equal(ncol(sim$pre), 2)

  for (col in names(sim$pre)) {
    expect_true(all(sim$pre[[col]] %in% c("0", "1", "d")))
    expect_true(all(sim$post[[col]] %in% c("0", "1", "d")))
  }
})

test_that("simulate_lca_dk includes DK responses", {
  sim <- simulate_lca_dk(n = 1000, gg = 0.1, gd = 0.2, dd = 0.2, seed = 789)

  has_dk_pre <- any(sim$pre$item1 == "d")
  has_dk_post <- any(sim$post$item1 == "d")

  expect_true(has_dk_pre || has_dk_post)
})

test_that("simulated data can be fitted by fit_item_lca", {
  sim <- simulate_lca(n = 200, gg = 0.4, gk = 0.3, kk = 0.3, gamma = 0.25, seed = 111)
  fit <- fit_item_lca(sim$pre, sim$post)

  expect_true(inherits(fit, "guess_fit"))
  expect_true(all(fit$params >= 0 & fit$params <= 1))
})

test_that("simulated DK data can be fitted by fit_item_lca", {
  sim <- simulate_lca_dk(n = 300, seed = 222)
  fit <- fit_item_lca(sim$pre, sim$post)

  expect_true(inherits(fit, "guess_fit"))
  expect_equal(nrow(fit$params), 8)
})

test_that("validate_recovery returns expected structure", {
  results <- validate_recovery(
    c(gg = 0.35, gk = 0.30, kk = 0.35, gamma = 0.25),
    n = 100, n_sims = 5, seed = 333
  )

  expect_true(is.data.frame(results))
  expect_equal(nrow(results), 4)
  expect_true(all(c(
    "parameter", "true_value", "mean_estimate", "bias",
    "rmse", "estimate_sd", "mcse"
  ) %in% names(results)))
  expect_false("se" %in% names(results))
  expect_false("coverage_95" %in% names(results))
  expect_equal(results$parameter, c("gg", "gk", "kk", "gamma"))
})

test_that("validate_recovery uses every simulated item", {
  result_one <- validate_recovery(
    c(gg = 0.35, gk = 0.30, kk = 0.35, gamma = 0.25),
    n = 150, n_items = 1, n_sims = 8, seed = 812
  )
  result_many <- validate_recovery(
    c(gg = 0.35, gk = 0.30, kk = 0.35, gamma = 0.25),
    n = 150, n_items = 4, n_sims = 8, seed = 812
  )

  expect_false(isTRUE(all.equal(
    result_one$mean_estimate,
    result_many$mean_estimate
  )))
})

test_that("validate_recovery works with DK model", {
  results <- validate_recovery(
    c(
      gg = 0.25, gk = 0.15, gd = 0.10, kk = 0.20,
      dg = 0.10, dk = 0.10, dd = 0.10, gamma = 0.25
    ),
    n = 100, n_sims = 3, seed = 444
  )

  expect_true(is.data.frame(results))
  expect_equal(nrow(results), 8)
})

test_that("validate_recovery bias is reasonable with adequate sample", {
  skip_on_cran()

  results <- validate_recovery(
    c(gg = 0.35, gk = 0.30, kk = 0.35, gamma = 0.25),
    n = 500, n_sims = 50, seed = 555
  )

  row <- results$parameter == "gk"
  expect_bias_within_mc_error(
    results$bias[row], results$estimate_sd[row],
    reps = 50, label = "validate_recovery gk"
  )
  expect_equal(results$mcse[row], results$estimate_sd[row] / sqrt(50))
})

test_that("validate_recovery requires a named feasible generating model", {
  valid <- c(gg = 0.35, gk = 0.30, kk = 0.35, gamma = 0.25)

  expect_error(
    validate_recovery(unname(valid), n = 100, n_sims = 2),
    "named"
  )
  invalid_names <- valid
  names(invalid_names)[1] <- "wrong"
  expect_error(
    validate_recovery(invalid_names, n = 100, n_sims = 2),
    "named vector"
  )
  invalid_weights <- valid
  invalid_weights["gg"] <- 0.40
  expect_error(
    validate_recovery(invalid_weights, n = 100, n_sims = 2),
    "sum to 1"
  )
  invalid_gamma <- valid
  invalid_gamma["gamma"] <- 1.1
  expect_error(
    validate_recovery(invalid_gamma, n = 100, n_sims = 2),
    "gamma"
  )
  boundary_weight <- valid
  boundary_weight["gg"] <- 0
  boundary_weight["kk"] <- 0.70
  expect_error(
    validate_recovery(boundary_weight, n = 100, n_sims = 2),
    "strictly between"
  )
  boundary_gamma <- valid
  boundary_gamma["gamma"] <- 0
  expect_error(
    validate_recovery(boundary_gamma, n = 100, n_sims = 2),
    "strictly between"
  )
})

test_that("validate_recovery preserves caller RNG state", {
  parameters <- c(gg = 0.35, gk = 0.30, kk = 0.35, gamma = 0.25)
  set.seed(917)
  before <- get(".Random.seed", envir = .GlobalEnv)
  validate_recovery(parameters, n = 100, n_sims = 2, seed = 918)
  after <- get(".Random.seed", envir = .GlobalEnv)

  expect_identical(before, after)
})

test_that("validate_recovery rejects degenerate generating models", {
  degenerate <- c(gg = 0, gk = 0, kk = 1, gamma = 0)

  expect_error(
    validate_recovery(degenerate, n = 10, n_items = 1, n_sims = 1, seed = 1),
    "strictly between"
  )
})

# =============================================================================
# Item-specific gamma tests
# =============================================================================

test_that("simulate_lca accepts vector gamma", {
  sim <- simulate_lca(n = 100, n_items = 3, gamma = c(0.2, 0.25, 0.3), seed = 123)

  expect_true(is.list(sim))
  expect_equal(ncol(sim$pre), 3)
  expect_equal(ncol(sim$post), 3)
})

test_that("simulate_lca rejects wrong-length gamma vector", {
  expect_error(
    simulate_lca(n = 100, n_items = 3, gamma = c(0.2, 0.3)),
    "gamma must be scalar or have length n_items"
  )
})

test_that("simulate_lca accepts difficulty parameter", {
  sim <- simulate_lca(n = 100, n_items = 3, difficulty = c(1, 0, -1), seed = 456)

  expect_true(is.list(sim))
  expect_equal(ncol(sim$pre), 3)
})

test_that("simulate_lca difficulty produces expected gamma range", {
  base_rate <- 0.25
  difficulty <- c(5, 0, -5)
  expected_gamma <- base_rate + (1 - base_rate) * plogis(-difficulty)

  expect_true(expected_gamma[1] < 0.30)
  expect_true(abs(expected_gamma[2] - 0.625) < 0.01)
  expect_true(expected_gamma[3] > 0.95)
})

test_that("simulate_lca rejects wrong-length difficulty", {
  expect_error(
    simulate_lca(n = 100, n_items = 3, difficulty = c(1, 0)),
    "difficulty must have length n_items"
  )
})

test_that("simulate_lca_dk accepts vector gamma", {
  sim <- simulate_lca_dk(n = 100, n_items = 3, gamma = c(0.2, 0.25, 0.3), seed = 123)

  expect_true(is.list(sim))
  expect_equal(ncol(sim$pre), 3)
})

test_that("simulate_lca_dk rejects wrong-length gamma vector", {
  expect_error(
    simulate_lca_dk(n = 100, n_items = 3, gamma = c(0.2, 0.3)),
    "gamma must be scalar or have length n_items"
  )
})

test_that("simulate_lca_dk accepts difficulty parameter", {
  sim <- simulate_lca_dk(n = 100, n_items = 3, difficulty = c(1, 0, -1), seed = 789)

  expect_true(is.list(sim))
  expect_equal(ncol(sim$pre), 3)
})

test_that("simulate_lca_dk rejects wrong-length difficulty", {
  expect_error(
    simulate_lca_dk(n = 100, n_items = 3, difficulty = c(1, 0)),
    "difficulty must have length n_items"
  )
})

test_that("simulated data with vector gamma can be fitted", {
  sim <- simulate_lca(n = 300, n_items = 3, gamma = c(0.2, 0.25, 0.3), seed = 111)
  fit <- fit_item_lca(sim$pre, sim$post)

  expect_true(inherits(fit, "guess_fit"))
  expect_true(all(fit$params >= 0 & fit$params <= 1))
})

test_that("simulated data with difficulty can be fitted", {
  sim <- simulate_lca(n = 300, n_items = 3, difficulty = c(1, 0, -1), seed = 222)
  fit <- fit_item_lca(sim$pre, sim$post)

  expect_true(inherits(fit, "guess_fit"))
})
