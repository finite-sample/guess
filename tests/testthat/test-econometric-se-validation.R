test_that("bootstrap SEs are consistent with Monte Carlo SEs", {
  skip_if(
    Sys.getenv("GUESS_FULL_TESTS") != "true",
    "Extended tests require GUESS_FULL_TESTS=true"
  )

  set.seed(99999)

  n_obs <- 500
  n_bootstrap <- 100
  n_mc_sims <- 50

  true_gg <- 0.40
  true_gk <- 0.25
  true_kk <- 0.35
  true_gamma <- 0.25

  mc_estimates <- matrix(NA, n_mc_sims, 4)
  colnames(mc_estimates) <- c("gg", "gk", "kk", "gamma")

  for (sim in seq_len(n_mc_sims)) {
    data <- simulate_prepost_data(
      n_obs,
      c(true_gg, true_gk, true_kk),
      true_gamma
    )
    trans <- multi_transmat(data$pre, data$post)
    result <- lca_cor(trans)
    mc_estimates[sim, ] <- result$params[, 1]
  }
  mc_se <- apply(mc_estimates, 2, sd)

  data <- simulate_prepost_data(
    n_obs,
    c(true_gg, true_gk, true_kk),
    true_gamma
  )
  bootstrap_result <- lca_se(data$pre, data$post, n_resamples = n_bootstrap, seed = 123)
  bootstrap_se <- bootstrap_result$se_params[, 1]

  for (i in seq_len(4)) {
    if (mc_se[i] > 0.01) {
      ratio <- bootstrap_se[i] / mc_se[i]
      expect_true(
        ratio > 0.3 && ratio < 3.0,
        info = paste("Parameter", i, "SE ratio:", round(ratio, 2))
      )
    }
  }
})

test_that("95% confidence intervals achieve reasonable coverage", {
  skip_if(
    Sys.getenv("GUESS_FULL_TESTS") != "true",
    "Extended tests require GUESS_FULL_TESTS=true"
  )

  set.seed(77777)

  n_sims <- 100
  n_obs <- 500
  n_bootstrap <- 50

  true_gk <- 0.25
  coverage_count <- 0

  for (sim in seq_len(n_sims)) {
    data <- simulate_prepost_data(n_obs, c(0.4, true_gk, 0.35), 0.25)

    trans <- multi_transmat(data$pre, data$post)
    result <- lca_cor(trans)
    point_est <- result$params["gk", 1]

    se_result <- lca_se(data$pre, data$post, n_resamples = n_bootstrap, seed = sim)
    se <- se_result$se_params["gk", 1]

    ci_lower <- point_est - 1.96 * se
    ci_upper <- point_est + 1.96 * se

    if (true_gk >= ci_lower && true_gk <= ci_upper) {
      coverage_count <- coverage_count + 1
    }
  }

  coverage_rate <- coverage_count / n_sims

  expect_true(
    coverage_rate >= 0.80 && coverage_rate <= 0.99,
    info = paste("Coverage rate:", coverage_rate)
  )
})

test_that("bootstrap SE decreases with sample size", {
  set.seed(33333)

  sample_sizes <- c(100, 500)
  se_values <- numeric(length(sample_sizes))

  for (j in seq_along(sample_sizes)) {
    n_obs <- sample_sizes[j]

    data <- simulate_prepost_data(n_obs, c(0.4, 0.25, 0.35), 0.25)
    se_result <- lca_se(data$pre, data$post, n_resamples = 30, seed = 42)
    se_values[j] <- se_result$se_params["gk", 1]
  }

  expect_lt(se_values[2], se_values[1])
})

test_that("learning SE is reasonable", {
  set.seed(44444)

  n_obs <- 500
  n_bootstrap <- 50

  data <- simulate_with_learning(n_obs, learning_frac = 0.20, gamma = 0.25)

  se_result <- lca_se(data$pre, data$post, n_resamples = n_bootstrap, seed = 123)
  learning_se <- se_result$se_effects[1, 1]

  expect_true(learning_se > 0)
  expect_true(learning_se < 0.5)
})
