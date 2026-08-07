test_that("no-DK model recovers true parameters", {
  skip_on_cran()

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
    data <- simulate_lca(n_obs,
      gg = true_gg, gk = true_gk,
      kk = true_kk, gamma = true_gamma
    )
    fit <- item_lca_fit(data$pre, data$post)
    estimates[sim, ] <- fit$params[, 1]
  }

  # Against the Monte Carlo standard error rather than fixed 0.05/0.10, so that
  # raising n_sims tightens the test instead of leaving it where it was.
  expect_unbiased(estimates[, "gg"], true_gg, "gg")
  expect_unbiased(estimates[, "gk"], true_gk, "gk")
  expect_unbiased(estimates[, "kk"], true_kk, "kk")
  expect_unbiased(estimates[, "gamma"], true_gamma, "gamma")
})

test_that("DK model recovers learning parameter", {
  skip_on_cran()

  set.seed(67890)
  true_gk <- 0.15

  n_sims <- 30
  n_obs <- 500

  estimates <- numeric(n_sims)

  for (sim in seq_len(n_sims)) {
    data <- simulate_lca_dk(n_obs, gk = true_gk)
    fit <- item_lca_fit(data$pre, data$post)
    estimates[sim] <- fit$params["gk", 1]
  }

  expect_unbiased(estimates, true_gk, "DK model gk")
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
      fit <- item_lca_fit(data$pre, data$post)
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
    data <- simulate_lca(
      n = 300, gg = 0.4, gk = 0.3, kk = 0.3,
      gamma = true_gamma
    )
    fit <- item_lca_fit(data$pre, data$post)

    est_gamma <- fit$params["gamma", 1]
    expect_lt(abs(est_gamma - true_gamma), 0.20)
  }
})

test_that("multi-item estimation produces consistent results", {
  set.seed(33333)
  n <- 300
  n_items <- 3

  true_gk <- 0.30
  data <- simulate_lca(n,
    n_items = n_items, gg = 0.35, gk = true_gk,
    kk = 0.35, gamma = 0.25
  )
  fit <- item_lca_fit(data$pre, data$post)

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
    data <- simulate_lca(
      n = 500, gg = params["gg"], gk = params["gk"],
      kk = params["kk"], gamma = 0.25
    )
    fit <- item_lca_fit(data$pre, data$post)

    estimates <- fit$params[c("gg", "gk", "kk"), 1]
    correlation <- cor(estimates, unname(params))

    expect_gt(correlation, 0.5)
  }
})
