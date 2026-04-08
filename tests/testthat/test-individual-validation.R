context("Individual-Level Function Validation")

test_that("perplexity_individuals returns correct length", {
  sim <- simulate_lca(n = 100, seed = 123)
  fit <- lca_fit(sim$pre, sim$post)

  perp <- perplexity_individuals(fit, sim$pre, sim$post, per_individual = TRUE)
  expect_equal(length(perp), 100)
})

test_that("perplexity_individuals values are positive", {
  sim <- simulate_lca(n = 100, seed = 456)
  fit <- lca_fit(sim$pre, sim$post)

  perp <- perplexity_individuals(fit, sim$pre, sim$post, per_individual = TRUE)
  valid <- is.finite(perp)
  expect_true(all(perp[valid] > 0))
})

test_that("aggregate perplexity is scalar", {
  sim <- simulate_lca(n = 100, seed = 789)
  fit <- lca_fit(sim$pre, sim$post)

  perp_agg <- perplexity_individuals(fit, sim$pre, sim$post, per_individual = FALSE)
  expect_equal(length(perp_agg), 1)
  expect_true(is.finite(perp_agg))
})

test_that("individual_log_likelihood sums correctly", {
  sim <- simulate_lca(n = 50, n_items = 2, seed = 111)
  fit <- lca_fit(sim$pre, sim$post)

  ind_ll <- individual_log_likelihood(fit, sim$pre, sim$post)
  total_ind_ll <- sum(ind_ll)

  trans <- multi_transmat(sim$pre, sim$post)
  total_agg_ll <- log_likelihood(fit$params[, 1], trans[1, ]) +
                  log_likelihood(fit$params[, 2], trans[2, ])

  expect_equal(total_ind_ll, total_agg_ll, tolerance = 0.01)
})

test_that("cv_individuals returns expected structure", {
  sim <- simulate_lca(n = 50, seed = 222)
  cv_result <- cv_individuals(sim$pre, sim$post, k = 3, seed = 333)

  expect_true(inherits(cv_result, "guess_cv"))
  expect_true("fold_results" %in% names(cv_result))
  expect_true("perplexity" %in% names(cv_result))
  expect_equal(cv_result$k, 3)
  expect_equal(cv_result$cv_type, "individuals")
})

test_that("cv_individuals perplexity is reasonable", {
  sim <- simulate_lca(n = 100, seed = 444)
  cv_result <- cv_individuals(sim$pre, sim$post, k = 5, seed = 555)

  expect_true(cv_result$perplexity > 0)
  expect_true(cv_result$perplexity < 100)
})

test_that("cv_individuals is reproducible with seed", {
  sim <- simulate_lca(n = 50, seed = 666)

  cv1 <- cv_individuals(sim$pre, sim$post, k = 3, seed = 777)
  cv2 <- cv_individuals(sim$pre, sim$post, k = 3, seed = 777)

  expect_equal(cv1$perplexity, cv2$perplexity)
  expect_equal(cv1$fold_results, cv2$fold_results)
})

test_that("perplexity_individuals works with DK model", {
  sim <- simulate_lca_dk(n = 80, seed = 888)
  fit <- lca_fit(sim$pre, sim$post)

  perp <- perplexity_individuals(fit, sim$pre, sim$post, per_individual = TRUE)
  expect_equal(length(perp), 80)

  perp_agg <- perplexity_individuals(fit, sim$pre, sim$post, per_individual = FALSE)
  expect_true(is.finite(perp_agg))
})

test_that("cv_individuals handles edge case of minimal data", {
  sim <- simulate_lca(n = 10, seed = 999)

  cv_result <- cv_individuals(sim$pre, sim$post, k = 2, seed = 111)
  expect_true(inherits(cv_result, "guess_cv"))
})

test_that("cv_items works with simulated data", {
  sim <- simulate_lca(n = 200, n_items = 5, seed = 222)
  trans <- multi_transmat(sim$pre, sim$post)

  cv_result <- cv_items(trans, k = 3, seed = 333)

  expect_true(inherits(cv_result, "guess_cv"))
  expect_equal(cv_result$cv_type, "items")
  expect_true(cv_result$perplexity > 0)
})

test_that("perplexity_items is consistent with cv_items", {
  sim <- simulate_lca(n = 200, n_items = 5, seed = 444)
  trans <- multi_transmat(sim$pre, sim$post)
  fit <- lca_cor(trans)

  perp_full <- perplexity_items(fit, trans)
  cv_result <- cv_items(trans, k = 5, seed = 555)

  expect_true(cv_result$perplexity >= perp_full * 0.8)
})
