test_that("lca_difficulty returns correct structure", {
  sim <- simulate_lca(n = 200, n_items = 2, seed = 123)
  transmatrix <- multi_transmat(sim$pre, sim$post)
  fit <- lca_difficulty(transmatrix)

  expect_true(inherits(fit, "guess_difficulty_fit"))
  expect_true(inherits(fit, "guess_fit"))
  expect_true("difficulty" %in% rownames(fit$params))
  expect_true(!is.null(fit$gamma))
  expect_true(!is.null(fit$base_rate))
  expect_equal(length(fit$gamma), 2)
})

test_that("lca_difficulty row replaces gamma", {
  sim <- simulate_lca(n = 200, n_items = 3, seed = 456)
  transmatrix <- multi_transmat(sim$pre, sim$post)
  fit <- lca_difficulty(transmatrix)

  expect_true("difficulty" %in% rownames(fit$params))
  expect_false("gamma" %in% rownames(fit$params))
})

test_that("lca_difficulty derives gamma correctly", {
  sim <- simulate_lca(n = 200, n_items = 2, seed = 789)
  transmatrix <- multi_transmat(sim$pre, sim$post)
  fit <- lca_difficulty(transmatrix)

  base_rate <- fit$base_rate
  difficulty <- fit$params["difficulty", ]
  expected_gamma <- base_rate + (1 - base_rate) * plogis(-difficulty)

  expect_equal(as.numeric(fit$gamma), as.numeric(expected_gamma), tolerance = 1e-6)
})

test_that("lca_difficulty works with DK data", {
  sim <- simulate_lca_dk(n = 300, n_items = 2, seed = 111)
  transmatrix <- multi_transmat(sim$pre, sim$post)
  fit <- lca_difficulty(transmatrix)

  expect_true(inherits(fit, "guess_difficulty_fit"))
  expect_equal(nrow(fit$params), 8)
  expect_true("difficulty" %in% rownames(fit$params))
})

test_that("lca_difficulty accepts custom base_rate", {
  sim <- simulate_lca(n = 200, n_items = 2, seed = 222)
  transmatrix <- multi_transmat(sim$pre, sim$post)
  fit <- lca_difficulty(transmatrix, base_rate = 0.2)

  expect_equal(fit$base_rate, 0.2)
})

# =============================================================================
# Parameter recovery tests
# =============================================================================

test_that("lca_difficulty recovers direction of difficulty ordering", {
  skip_on_cran()

  true_difficulty <- c(2, 0, -2)
  sim <- simulate_lca(
    n = 1000, n_items = 3,
    gg = 0.35, gk = 0.30, kk = 0.35,
    difficulty = true_difficulty,
    seed = 333
  )

  transmatrix <- multi_transmat(sim$pre, sim$post)
  fit <- lca_difficulty(transmatrix)

  est_difficulty <- fit$params["difficulty", ]
  expect_true(est_difficulty[1] > est_difficulty[2])
  expect_true(est_difficulty[2] > est_difficulty[3])
})

test_that("lca_difficulty recovers difficulty with reasonable accuracy", {
  skip_on_cran()

  true_difficulty <- c(1, 0, -1)
  sim <- simulate_lca(
    n = 2000, n_items = 3,
    gg = 0.35, gk = 0.30, kk = 0.35,
    difficulty = true_difficulty,
    seed = 444
  )

  transmatrix <- multi_transmat(sim$pre, sim$post)
  fit <- lca_difficulty(transmatrix)

  est_difficulty <- fit$params["difficulty", ]
  correlation <- cor(true_difficulty, as.numeric(est_difficulty))
  expect_gt(correlation, 0.8)
})

test_that("lca_difficulty learning estimates match lca_cor approximately", {
  sim <- simulate_lca(n = 500, n_items = 2, gg = 0.35, gk = 0.30, kk = 0.35, seed = 555)
  transmatrix <- multi_transmat(sim$pre, sim$post)

  fit_difficulty <- lca_difficulty(transmatrix)
  fit_cor <- lca_cor(transmatrix)

  expect_equal(fit_difficulty$learning, fit_cor$learning, tolerance = 0.05)
})

# =============================================================================
# Helper function tests
# =============================================================================

test_that("difficulty_to_gamma produces valid gamma", {
  difficulty <- c(-3, 0, 3)
  gamma <- difficulty_to_gamma(difficulty, base_rate = 0.25)

  expect_true(all(gamma >= 0.25))
  expect_true(all(gamma <= 1))
  expect_true(gamma[1] > gamma[2])
  expect_true(gamma[2] > gamma[3])
})

test_that("gamma_to_difficulty is inverse of difficulty_to_gamma", {
  original_d <- c(-2, -1, 0, 1, 2)
  gamma <- difficulty_to_gamma(original_d, base_rate = 0.25)
  recovered_d <- gamma_to_difficulty(gamma, base_rate = 0.25)

  expect_equal(original_d, recovered_d, tolerance = 1e-4)
})

test_that("difficulty_to_gamma handles extreme values", {
  gamma_hard <- difficulty_to_gamma(10, base_rate = 0.25)
  gamma_easy <- difficulty_to_gamma(-10, base_rate = 0.25)

  expect_true(abs(gamma_hard - 0.25) < 0.01)
  expect_true(abs(gamma_easy - 1) < 0.01)
})

# =============================================================================
# Edge cases
# =============================================================================

test_that("lca_difficulty handles single item", {
  sim <- simulate_lca(n = 200, n_items = 1, seed = 666)
  transmatrix <- multi_transmat(sim$pre, sim$post)
  fit <- lca_difficulty(transmatrix)

  expect_equal(fit$n_items, 1)
  expect_equal(length(fit$gamma), 1)
})

test_that("lca_difficulty validates base_rate bounds", {
  sim <- simulate_lca(n = 100, n_items = 1, seed = 777)
  transmatrix <- multi_transmat(sim$pre, sim$post)

  expect_error(lca_difficulty(transmatrix, base_rate = -0.1))
  expect_error(lca_difficulty(transmatrix, base_rate = 1.1))
})
