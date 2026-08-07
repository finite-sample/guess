test_that("lca_cor recovers true parameters with low bias", {
  skip_on_cran()

  set.seed(54321)

  n_sims <- 100
  n_obs <- 500

  true_gg <- 0.40
  true_gk <- 0.25
  true_kk <- 0.35
  true_gamma <- 0.25

  estimates <- matrix(NA, nrow = n_sims, ncol = 4)
  colnames(estimates) <- c("gg", "gk", "kk", "gamma")

  for (sim in seq_len(n_sims)) {
    data <- simulate_prepost_data(
      n_obs,
      c(true_gg, true_gk, true_kk),
      true_gamma
    )
    trans <- multi_transmat(data$pre, data$post)
    result <- lca_cor(trans)
    estimates[sim, ] <- result$params[, 1]
  }

  # Compared against the Monte Carlo standard error of the mean rather than a
  # fixed 0.05. The measured biases are 0.001 to 0.005, so the old threshold was
  # ten to fifty times looser than the study could resolve -- it would have
  # passed a bias an order of magnitude larger than anything real here, and
  # raising n_sims made it no stricter.
  #
  # Measured t statistics at 100 replicates: gg -0.26, gk -1.16, kk +1.46,
  # gamma -1.12. All well inside the 3-sigma gate.
  expect_unbiased(estimates[, "gg"], true_gg, "gg")
  expect_unbiased(estimates[, "gk"], true_gk, "gk")
  expect_unbiased(estimates[, "kk"], true_kk, "kk")
  expect_unbiased(estimates[, "gamma"], true_gamma, "gamma")
})

test_that("lca_cor parameter RMSE is within theoretical bounds", {
  skip_on_cran()

  set.seed(98765)

  n_sims <- 100
  n_obs <- 1000

  true_gg <- 0.35
  true_gk <- 0.30
  true_kk <- 0.35
  true_gamma <- 0.25

  estimates <- matrix(NA, nrow = n_sims, ncol = 4)
  colnames(estimates) <- c("gg", "gk", "kk", "gamma")

  for (sim in seq_len(n_sims)) {
    data <- simulate_prepost_data(
      n_obs,
      c(true_gg, true_gk, true_kk),
      true_gamma
    )
    trans <- multi_transmat(data$pre, data$post)
    result <- lca_cor(trans)
    estimates[sim, ] <- result$params[, 1]
  }

  rmse_gg <- sqrt(mean((estimates[, "gg"] - true_gg)^2))
  rmse_gk <- sqrt(mean((estimates[, "gk"] - true_gk)^2))
  rmse_kk <- sqrt(mean((estimates[, "kk"] - true_kk)^2))
  rmse_gamma <- sqrt(mean((estimates[, "gamma"] - true_gamma)^2))

  expected_rmse_order <- 5 / sqrt(n_obs)

  expect_lt(rmse_gg, expected_rmse_order)
  expect_lt(rmse_gk, expected_rmse_order)
  expect_lt(rmse_kk, expected_rmse_order)
  expect_lt(rmse_gamma, expected_rmse_order)
})

test_that("learning estimate recovers true learning fraction", {
  skip_on_cran()

  set.seed(11111)

  n_sims <- 50
  n_obs <- 800

  true_learning <- 0.20

  learning_estimates <- numeric(n_sims)

  for (sim in seq_len(n_sims)) {
    data <- simulate_with_learning(n_obs, learning_frac = true_learning, gamma = 0.25)
    trans <- multi_transmat(data$pre, data$post)
    result <- lca_cor(trans)
    learning_estimates[sim] <- result$learning[1]
  }

  expect_unbiased(learning_estimates, true_learning, "learning fraction")
})

test_that("parameter recovery improves with sample size", {
  set.seed(22222)

  true_gg <- 0.40
  true_gk <- 0.25
  true_kk <- 0.35
  true_gamma <- 0.25

  sample_sizes <- c(100, 500)
  rmse_values <- numeric(length(sample_sizes))

  for (j in seq_along(sample_sizes)) {
    n_obs <- sample_sizes[j]
    n_sims <- 30

    gk_estimates <- numeric(n_sims)

    for (sim in seq_len(n_sims)) {
      data <- simulate_prepost_data(
        n_obs,
        c(true_gg, true_gk, true_kk),
        true_gamma
      )
      trans <- multi_transmat(data$pre, data$post)
      result <- lca_cor(trans)
      gk_estimates[sim] <- result$params["gk", 1]
    }

    rmse_values[j] <- sqrt(mean((gk_estimates - true_gk)^2))
  }

  expect_lt(rmse_values[2], rmse_values[1])
})
