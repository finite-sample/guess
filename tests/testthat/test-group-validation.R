test_that("group_adj returns expected structure", {
  sim <- simulate_lca(n = 100, n_items = 3, seed = 123)
  gamma <- rep(0.25, 3)

  result <- group_adj(sim$pre, sim$post, gamma)

  expect_true(is.list(result))
  expect_true(all(c("indiv", "learn") %in% names(result)))
  expect_equal(length(result$learn), 3)
})

test_that("group_adj learning estimates are bounded", {
  sim <- simulate_lca(n = 200, n_items = 2, seed = 456)
  gamma <- c(0.25, 0.30)

  result <- group_adj(sim$pre, sim$post, gamma)

  expect_true(all(result$learn >= -1 & result$learn <= 1))
})

test_that("group_adj with true gamma improves estimates", {
  skip_on_cran()

  set.seed(789)
  true_gamma <- 0.25
  true_learning <- 0.30

  n_sims <- 30
  adj_errors <- numeric(n_sims)
  naive_errors <- numeric(n_sims)

  for (i in seq_len(n_sims)) {
    sim <- simulate_lca(
      n = 300, gg = 0.35, gk = true_learning,
      kk = 0.35, gamma = true_gamma
    )

    result <- group_adj(sim$pre, sim$post, gamma = true_gamma)
    adj_errors[i] <- abs(result$learn[1] - true_learning)

    naive <- mean(sim$post$item1) - mean(sim$pre$item1)
    naive_errors[i] <- abs(naive - true_learning)
  }

  # The old gate was `mean_adj_error < mean_naive_error * 1.5`, which passes when
  # the adjustment is 50% *worse* than doing nothing -- it could not fail in the
  # direction the test exists to check.
  #
  # The comparison is paired: both estimators see the same simulated dataset on
  # each replicate, so differencing within replicate removes the draw and leaves
  # only the estimator. The mean difference is judged against its own Monte Carlo
  # standard error, so "improves" means an improvement the study can actually
  # resolve, not one that could be noise.
  improvement <- naive_errors - adj_errors
  mc_se <- stats::sd(improvement) / sqrt(n_sims)

  expect_gt(
    mean(improvement) / mc_se, GATE_SIGMAS,
    label = sprintf(
      "adjusted error %.4f vs naive %.4f; improvement %+.4f is %.2f MC SEs",
      mean(adj_errors), mean(naive_errors), mean(improvement),
      mean(improvement) / mc_se
    )
  )
})

test_that("group_adj handles varying gamma across items", {
  sim <- simulate_lca(n = 150, n_items = 3, seed = 111)
  gamma <- c(0.20, 0.25, 0.30)

  result <- group_adj(sim$pre, sim$post, gamma)

  expect_equal(length(result$learn), 3)
  expect_true(all(is.finite(result$learn)))
})

test_that("lca_adj returns expected structure", {
  pre_test <- data.frame(
    item1 = c(1, 0, 0, 1, "d", "d", 0, 1, 0),
    stringsAsFactors = FALSE
  )
  post_test <- data.frame(
    item1 = c(1, 0, 1, "d", 1, 0, 1, 1, "d"),
    stringsAsFactors = FALSE
  )

  result <- suppressWarnings(lca_adj(pre_test, post_test))

  expect_true(is.list(result))
  expect_true(all(c("pre", "pst") %in% names(result)))
})

test_that("lca_adj produces reasonable individual adjustments", {
  sim <- simulate_lca_dk(n = 100, seed = 222)

  result <- lca_adj(sim$pre, sim$post)

  expect_equal(nrow(result$pre), 100)
  expect_equal(nrow(result$pst), 100)
})

test_that("lca_adj handles both DK and non-DK data", {
  sim_dk <- simulate_lca_dk(n = 50, seed = 333)
  result_dk <- lca_adj(sim_dk$pre, sim_dk$post)
  expect_true(is.list(result_dk))

  sim_nodk <- simulate_lca(n = 50, seed = 444)
  sim_nodk$pre$item1 <- as.character(sim_nodk$pre$item1)
  sim_nodk$post$item1 <- as.character(sim_nodk$post$item1)

  result_nodk <- suppressWarnings(lca_adj(sim_nodk$pre, sim_nodk$post))
  expect_true(is.list(result_nodk))
})

test_that("group_adj produces consistent results across runs with same data", {
  sim <- simulate_lca(n = 100, seed = 555)
  gamma <- 0.25

  result1 <- group_adj(sim$pre, sim$post, gamma)
  result2 <- group_adj(sim$pre, sim$post, gamma)

  expect_equal(result1$learn, result2$learn)
})

test_that("group_adj handles single item", {
  sim <- simulate_lca(n = 100, n_items = 1, seed = 666)
  gamma <- 0.25

  result <- group_adj(sim$pre, sim$post, gamma)

  expect_equal(length(result$learn), 1)
  expect_true(is.finite(result$learn))
})

test_that("lca_adj does not crash with missing values", {
  pre_test <- data.frame(item1 = c(1, 0, NA, 1, 0))
  post_test <- data.frame(item1 = c(1, NA, 1, 1, 0))

  expect_no_error(result <- lca_adj(pre_test, post_test))
  expect_true(is.list(result))
})
