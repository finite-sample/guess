test_that("no-DK model converges to unique solution from different starting values", {
  set.seed(444)
  n <- 1000

  data <- simulate_prepost_data(n, c(0.35, 0.30, 0.35), 0.25)
  trans <- multi_transmat(data$pre, data$post)

  result1 <- lca_cor(trans, nodk_priors = c(0.3, 0.3, 0.3, 0.25))
  result2 <- lca_cor(trans, nodk_priors = c(0.5, 0.2, 0.2, 0.30))
  result3 <- lca_cor(trans, nodk_priors = c(0.2, 0.4, 0.3, 0.20))

  expect_equal(result1$params[, 1], result2$params[, 1], tolerance = 0.05)
  expect_equal(result1$params[, 1], result3$params[, 1], tolerance = 0.05)
})

test_that("model estimates are within valid parameter space", {
  set.seed(555)

  for (i in 1:10) {
    n <- 500
    data <- simulate_prepost_data(
      n,
      c(runif(1, 0.2, 0.5), runif(1, 0.1, 0.4), runif(1, 0.2, 0.5)),
      runif(1, 0.15, 0.35)
    )
    data$pre$item1 <- pmax(0, pmin(1, data$pre$item1))
    data$post$item1 <- pmax(0, pmin(1, data$post$item1))

    trans <- multi_transmat(data$pre, data$post)
    result <- lca_cor(trans)

    params <- result$params[, 1]

    expect_true(all(params >= 0 & params <= 1))
    expect_equal(sum(params[c("gg", "gk", "kk")]), 1, tolerance = 1e-6)
  }
})

test_that("lambda parameters sum to 1", {
  set.seed(666)

  for (i in 1:20) {
    n <- 300
    data <- simulate_prepost_data(n, c(0.4, 0.3, 0.3), 0.25)
    trans <- multi_transmat(data$pre, data$post)
    result <- lca_cor(trans)

    lambda_sum <- sum(result$params[c("gg", "gk", "kk"), 1])

    expect_equal(lambda_sum, 1.0, tolerance = 1e-6)
  }
})

test_that("DK model GOF test has reasonable Type I error rate", {
  skip_if(
    Sys.getenv("GUESS_FULL_TESTS") != "true",
    "Extended tests require GUESS_FULL_TESTS=true"
  )

  set.seed(55555)

  n_sims <- 100
  n <- 500
  alpha <- 0.05

  rejection_count <- 0

  for (sim in seq_len(n_sims)) {
    data <- simulate_dk_prepost_data(n)

    trans <- multi_transmat(data$pre, data$post, force9 = TRUE)

    tryCatch(
      {
        result <- lca_cor(trans)

        fit_result <- fit_model(
          data$pre, data$post,
          result$params["gamma", ],
          result$params[c("gg", "gk", "gd", "kg", "kk", "kd", "dd"), ],
          force9 = TRUE
        )

        if (any(fit_result["p-value", ] < alpha, na.rm = TRUE)) {
          rejection_count <- rejection_count + 1
        }
      },
      error = function(e) NULL
    )
  }

  rejection_rate <- rejection_count / n_sims

  expect_true(
    rejection_rate >= 0.01 && rejection_rate <= 0.20,
    info = paste("Type I error rate:", rejection_rate)
  )
})

test_that("model recovers extreme parameter values", {
  set.seed(777)

  extreme_cases <- list(
    c(0.8, 0.1, 0.1),
    c(0.1, 0.1, 0.8),
    c(0.1, 0.8, 0.1)
  )

  for (true_lambdas in extreme_cases) {
    n <- 1000
    data <- simulate_prepost_data(n, true_lambdas, 0.25)
    trans <- multi_transmat(data$pre, data$post)
    result <- lca_cor(trans)

    estimates <- result$params[c("gg", "gk", "kk"), 1]

    correlation <- cor(estimates, true_lambdas)

    expect_true(
      correlation > 0.5,
      info = paste(
        "True:", paste(round(true_lambdas, 2), collapse = ","),
        "Est:", paste(round(estimates, 2), collapse = ",")
      )
    )
  }
})

test_that("gamma estimate is reasonable across different true values", {
  set.seed(888)

  gamma_values <- c(0.15, 0.25, 0.40)

  for (true_gamma in gamma_values) {
    n <- 800
    data <- simulate_prepost_data(n, c(0.4, 0.3, 0.3), true_gamma)
    trans <- multi_transmat(data$pre, data$post)
    result <- lca_cor(trans)

    estimated_gamma <- result$params["gamma", 1]

    expect_true(
      abs(estimated_gamma - true_gamma) < 0.15,
      info = paste("True gamma:", true_gamma, "Estimated:", round(estimated_gamma, 3))
    )
  }
})

test_that("multi-item estimation produces consistent results", {
  set.seed(999)

  n <- 500
  n_items <- 3

  true_lambdas <- c(0.35, 0.30, 0.35)
  true_gamma <- 0.25

  pre_list <- vector("list", n_items)
  post_list <- vector("list", n_items)

  for (item in seq_len(n_items)) {
    data <- simulate_prepost_data(n, true_lambdas, true_gamma)
    pre_list[[item]] <- data$pre$item1
    post_list[[item]] <- data$post$item1
  }

  pre_df <- as.data.frame(pre_list)
  post_df <- as.data.frame(post_list)
  names(pre_df) <- paste0("item", 1:n_items)
  names(post_df) <- paste0("item", 1:n_items)

  trans <- multi_transmat(pre_df, post_df)
  result <- lca_cor(trans)

  gammas <- result$params["gamma", ]

  expect_equal(length(gammas), n_items)
  expect_true(all(gammas > 0 & gammas < 1))

  gamma_range <- max(gammas) - min(gammas)
  expect_lt(gamma_range, 0.3)
})
