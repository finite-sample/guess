context("Parameter Recovery Validation")

test_that("no-DK model recovers true parameters", {
  skip_if(
    Sys.getenv("GUESS_FULL_TESTS") != "true",
    "Extended tests require GUESS_FULL_TESTS=true"
  )

  set.seed(12345)
  true_gg <- 0.35
  true_gk <- 0.30
  true_kk <- 0.35
  true_gamma <- 0.25

  n_sims <- 50
  n_obs <- 500

  estimates <- matrix(NA, n_sims, 4)
  colnames(estimates) <- c("gg", "gk", "kk", "gamma")

  for (sim in seq_len(n_sims)) {
    data <- simulate_lca(n_obs, gg = true_gg, gk = true_gk,
                         kk = true_kk, gamma = true_gamma)
    fit <- lca_fit(data$pre, data$post)
    estimates[sim, ] <- fit$params[, 1]
  }

  bias_gg <- mean(estimates[, "gg"]) - true_gg
  bias_gk <- mean(estimates[, "gk"]) - true_gk
  bias_kk <- mean(estimates[, "kk"]) - true_kk
  bias_gamma <- mean(estimates[, "gamma"]) - true_gamma

  expect_lt(abs(bias_gg), 0.05)
  expect_lt(abs(bias_gk), 0.05)
  expect_lt(abs(bias_kk), 0.05)
  expect_lt(abs(bias_gamma), 0.10)
})

test_that("DK model recovers learning parameter", {
  skip_if(
    Sys.getenv("GUESS_FULL_TESTS") != "true",
    "Extended tests require GUESS_FULL_TESTS=true"
  )

  set.seed(67890)
  true_gk <- 0.15

  n_sims <- 30
  n_obs <- 500

  estimates <- numeric(n_sims)

  for (sim in seq_len(n_sims)) {
    data <- simulate_lca_dk(n_obs, gk = true_gk)
    fit <- lca_fit(data$pre, data$post)
    estimates[sim] <- fit$params["gk", 1]
  }

  bias <- mean(estimates, na.rm = TRUE) - true_gk
  expect_lt(abs(bias), 0.10)
})

test_that("learning estimate improves with sample size", {
  set.seed(11111)
  true_gk <- 0.25

  sample_sizes <- c(100, 500)
  rmse_values <- numeric(length(sample_sizes))

  for (j in seq_along(sample_sizes)) {
    n <- sample_sizes[j]
    n_sims <- 20
    estimates <- numeric(n_sims)

    for (sim in seq_len(n_sims)) {
      data <- simulate_lca(n, gg = 0.4, gk = true_gk, kk = 0.35, gamma = 0.25)
      fit <- lca_fit(data$pre, data$post)
      estimates[sim] <- fit$params["gk", 1]
    }

    rmse_values[j] <- sqrt(mean((estimates - true_gk)^2, na.rm = TRUE))
  }

  expect_lt(rmse_values[2], rmse_values[1])
})

test_that("parameter recovery works across different gamma values", {
  set.seed(22222)
  gamma_values <- c(0.15, 0.35)

  for (true_gamma in gamma_values) {
    data <- simulate_lca(n = 300, gg = 0.4, gk = 0.3, kk = 0.3,
                         gamma = true_gamma)
    fit <- lca_fit(data$pre, data$post)

    est_gamma <- fit$params["gamma", 1]
    expect_lt(abs(est_gamma - true_gamma), 0.20)
  }
})

test_that("multi-item estimation produces consistent results", {
  set.seed(33333)
  n <- 300
  n_items <- 3

  true_gk <- 0.30
  data <- simulate_lca(n, n_items = n_items, gg = 0.35, gk = true_gk,
                       kk = 0.35, gamma = 0.25)
  fit <- lca_fit(data$pre, data$post)

  learning_estimates <- fit$params["gk", ]
  expect_equal(length(learning_estimates), n_items)

  range_est <- max(learning_estimates) - min(learning_estimates)
  expect_lt(range_est, 0.30)
})

test_that("extreme parameter values are recoverable", {
  set.seed(44444)

  extreme_cases <- list(
    c(gg = 0.70, gk = 0.15, kk = 0.15),
    c(gg = 0.15, gk = 0.70, kk = 0.15),
    c(gg = 0.15, gk = 0.15, kk = 0.70)
  )

  for (params in extreme_cases) {
    data <- simulate_lca(n = 500, gg = params["gg"], gk = params["gk"],
                         kk = params["kk"], gamma = 0.25)
    fit <- lca_fit(data$pre, data$post)

    estimates <- fit$params[c("gg", "gk", "kk"), 1]
    correlation <- cor(estimates, unname(params))

    expect_gt(correlation, 0.5)
  }
})
