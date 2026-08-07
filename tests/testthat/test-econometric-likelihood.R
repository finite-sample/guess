test_that("cell probabilities sum to 1 for any valid parameters", {
  set.seed(42)
  n_tests <- 100

  for (i in seq_len(n_tests)) {
    lambdas <- runif(3)
    lambdas <- lambdas / sum(lambdas)
    gamma <- runif(1, 0.1, 0.9)

    g <- gamma
    gg <- lambdas[1]
    gk <- lambdas[2]
    kk <- lambdas[3]

    vec <- numeric(4)
    vec[1] <- (1 - g) * (1 - g) * gg
    vec[2] <- (1 - g) * g * gg + (1 - g) * gk
    vec[3] <- (1 - g) * g * gg
    vec[4] <- g * g * gg + g * gk + kk

    expect_equal(sum(vec), 1, tolerance = 1e-10)
  }
})

test_that("cell probabilities are non-negative for all valid parameters", {
  set.seed(43)
  n_tests <- 100

  for (i in seq_len(n_tests)) {
    lambdas <- runif(3)
    lambdas <- lambdas / sum(lambdas)
    gamma <- runif(1, 0.01, 0.99)

    g <- gamma
    gg <- lambdas[1]
    gk <- lambdas[2]
    kk <- lambdas[3]

    vec <- numeric(4)
    vec[1] <- (1 - g) * (1 - g) * gg
    vec[2] <- (1 - g) * g * gg + (1 - g) * gk
    vec[3] <- (1 - g) * g * gg
    vec[4] <- g * g * gg + g * gk + kk

    expect_true(all(vec >= 0))
  }
})

test_that("expected values match likelihood function formulas", {
  gamma_values <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  lambda_sets <- list(
    c(0.4, 0.3, 0.3),
    c(0.6, 0.2, 0.2),
    c(0.33, 0.33, 0.34),
    c(0.1, 0.1, 0.8)
  )

  for (gamma_i in gamma_values) {
    for (lambdas in lambda_sets) {
      gg <- lambdas[1]
      gk <- lambdas[2]
      kk <- lambdas[3]

      lik_vec <- numeric(4)
      lik_vec[1] <- (1 - gamma_i) * (1 - gamma_i) * gg
      lik_vec[2] <- (1 - gamma_i) * gamma_i * gg + (1 - gamma_i) * gk
      lik_vec[3] <- (1 - gamma_i) * gamma_i * gg
      lik_vec[4] <- gamma_i * gamma_i * gg + gamma_i * gk + kk

      expected <- calculate_expected_values(gamma_i, lambdas, 1, "nodk")

      expect_equal(expected, lik_vec, tolerance = 1e-12)
    }
  }
})

test_that("negative log-likelihood is minimized near true parameters", {
  skip_on_cran()

  set.seed(123)
  n <- 1000

  true_lambdas <- c(0.4, 0.3, 0.3)
  true_gamma <- 0.25

  counts <- generate_transition_counts(n, true_lambdas, true_gamma)
  data_vec <- as.numeric(counts)

  true_nll <- guess_lik(c(true_lambdas, true_gamma), data = data_vec)

  perturbations <- list(
    c(0.5, 0.25, 0.25, 0.25),
    c(0.35, 0.35, 0.30, 0.30),
    c(0.4, 0.3, 0.3, 0.40),
    c(0.2, 0.4, 0.4, 0.15)
  )

  for (pert in perturbations) {
    pert_nll <- guess_lik(pert, data = data_vec)
    expect_true(
      pert_nll >= true_nll - 0.5,
      info = paste("Perturbed params:", paste(pert, collapse = ", "))
    )
  }
})

test_that("DK model cell probabilities are non-negative and sum to 1", {
  # This test used to restate the cell equations inline and assert only that
  # they were non-negative. They were -- and they also summed to as much as
  # 2.29, because they were not the model's equations. It now calls the
  # function the likelihood actually uses, and checks the property that
  # distinguishes a distribution from an arbitrary set of non-negative numbers.
  set.seed(44)

  for (i in seq_len(200)) {
    lambdas <- runif(7)
    lambdas <- lambdas / sum(lambdas)
    gamma <- runif(1, 0.1, 0.9)

    vec <- dk_cell_probs(
      lambdas[1], lambdas[2], lambdas[3], lambdas[4],
      lambdas[5], lambdas[6], lambdas[7], gamma
    )

    expect_length(vec, 9)
    expect_true(all(vec >= 0))
    expect_equal(sum(vec), 1)
  }
})

test_that("no-DK cell probabilities are non-negative and sum to 1", {
  set.seed(45)

  for (i in seq_len(200)) {
    lambdas <- runif(3)
    lambdas <- lambdas / sum(lambdas)
    gamma <- runif(1, 0.1, 0.9)

    vec <- nodk_cell_probs(lambdas[1], lambdas[2], lambdas[3], gamma)

    expect_length(vec, 4)
    expect_true(all(vec >= 0))
    expect_equal(sum(vec), 1)
  }
})
