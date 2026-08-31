test_that("cell_probs returns valid probabilities for nodk model", {
  params <- c(0.4, 0.3, 0.3, 0.25)
  probs <- cell_probs(params)

  expect_length(probs, 4)
  expect_true(all(probs >= 0))
  expect_true(all(probs <= 1))
  expect_equal(sum(probs), 1, tolerance = 1e-10)
})

test_that("cell_probs returns valid values for dk model", {
  params <- c(0.25, 0.15, 0.10, 0.10, 0.15, 0.10, 0.15, 0.25)
  probs <- cell_probs(params)

  expect_length(probs, 9)
  expect_true(all(probs >= 0))
})

test_that("cell_probs errors on invalid parameter length", {
  expect_error(cell_probs(c(0.5, 0.5)), "must have length 4")
  expect_error(cell_probs(c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1)), "must have length 4")
})

test_that("log_likelihood matches negative of guess_lik", {
  params <- c(0.4, 0.3, 0.3, 0.25)
  data <- c(10, 5, 3, 12)

  ll <- log_likelihood(params, data)
  neg_ll <- guess_lik(params, data = data)

  expect_equal(ll, -neg_ll, tolerance = 1e-10)
})

test_that("log_likelihood returns -Inf for zero probabilities", {
  params <- c(0, 0, 1, 0.5)
  data <- c(10, 5, 3, 12)

  ll <- log_likelihood(params, data)
  expect_equal(ll, -Inf)
})

test_that("log_likelihood errors on mismatched dimensions", {
  params <- c(0.4, 0.3, 0.3, 0.25)
  data <- c(10, 5, 3, 12, 2, 1, 3, 2, 1)

  expect_error(log_likelihood(params, data), "incompatible")
})

test_that("response_to_cell maps correctly for nodk", {
  expect_equal(response_to_cell(0, 0, FALSE), 1)
  expect_equal(response_to_cell(0, 1, FALSE), 2)
  expect_equal(response_to_cell(1, 0, FALSE), 3)
  expect_equal(response_to_cell(1, 1, FALSE), 4)
  expect_true(is.na(response_to_cell(
    NA, 0, FALSE,
    na_as = "missing"
  )))
  expect_error(response_to_cell(NA, 0, FALSE), "require a DK model")
  expect_error(response_to_cell("d", 1, FALSE), "require a DK model")
})

test_that("response_to_cell maps correctly for dk", {
  expect_equal(response_to_cell(0, 0, TRUE), 1)
  expect_equal(response_to_cell(0, 1, TRUE), 2)
  expect_equal(response_to_cell(0, "d", TRUE), 3)
  expect_equal(response_to_cell(1, 0, TRUE), 4)
  expect_equal(response_to_cell(1, 1, TRUE), 5)
  expect_equal(response_to_cell(1, "d", TRUE), 6)
  expect_equal(response_to_cell("d", 0, TRUE), 7)
  expect_equal(response_to_cell("d", 1, TRUE), 8)
  expect_equal(response_to_cell("d", "d", TRUE), 9)
})

# Individual-level tests

test_that("fit_item_lca agrees with explicit transition counts", {
  pre_test <- data.frame(
    item1 = c(1, 0, 0, 1, 0, 1, 0, 1),
    item2 = c(1, 0, 1, 1, 0, 0, 1, 0)
  )
  pst_test <- data.frame(
    item1 = c(1, 1, 0, 1, 1, 1, 0, 1),
    item2 = c(1, 1, 1, 1, 0, 1, 1, 1)
  )

  fit1 <- fit_item_lca(pre_test, pst_test)
  transmat <- count_item_transitions(pre_test, pst_test)
  fit2 <- fit_item_lca_counts(transmat)

  expect_equal(fit1$params, fit2$params)
  expect_equal(fit1$learning, fit2$learning)
})

test_that("score_individual_lca returns a positive aggregate perplexity", {
  pre_test <- data.frame(
    item1 = c(1, 0, 0, 1, 0, 1, 0, 1, 0, 0),
    item2 = c(1, 0, 1, 1, 0, 0, 1, 0, 1, 0)
  )
  pst_test <- data.frame(
    item1 = c(1, 1, 0, 1, 1, 1, 0, 1, 0, 1),
    item2 = c(1, 1, 1, 1, 0, 1, 1, 1, 1, 0)
  )

  fit <- fit_item_lca(pre_test, pst_test)
  score <- score_individual_lca(fit, pre_test, pst_test)

  expect_true(is.numeric(score$perplexity))
  expect_true(score$perplexity > 0)
  expect_true(is.finite(score$perplexity))
})

test_that("score_individual_lca returns one score per individual", {
  pre_test <- data.frame(
    item1 = c(1, 0, 0, 1, 0),
    item2 = c(1, 0, 1, 1, 0)
  )
  pst_test <- data.frame(
    item1 = c(1, 1, 0, 1, 1),
    item2 = c(1, 1, 1, 1, 0)
  )

  fit <- fit_item_lca(pre_test, pst_test)
  score <- score_individual_lca(fit, pre_test, pst_test)

  expect_equal(nrow(score$individual_scores), 5)
  expect_true(all(score$individual_scores$perplexity > 0))
})

test_that("cv_individual_lca runs and returns expected structure", {
  set.seed(42)
  pre_test <- data.frame(
    item1 = rbinom(20, 1, 0.4),
    item2 = rbinom(20, 1, 0.4),
    item3 = rbinom(20, 1, 0.4)
  )
  pst_test <- data.frame(
    item1 = pmin(1, pre_test$item1 + rbinom(20, 1, 0.3)),
    item2 = pmin(1, pre_test$item2 + rbinom(20, 1, 0.3)),
    item3 = pmin(1, pre_test$item3 + rbinom(20, 1, 0.3))
  )

  cv_result <- cv_individual_lca(pre_test, pst_test, k = 5, seed = 123)

  expect_true(is.list(cv_result))
  expect_true("fold_results" %in% names(cv_result))
  expect_true("mean_ll" %in% names(cv_result))
  expect_true("perplexity" %in% names(cv_result))
  expect_true("fold_id" %in% names(cv_result))

  expect_equal(nrow(cv_result$fold_results), 5)
  expect_true(cv_result$perplexity > 0)
})

test_that("cv_individual_lca errors when k > n_individuals", {
  pre_test <- data.frame(item1 = c(1, 0))
  pst_test <- data.frame(item1 = c(1, 1))

  expect_error(cv_individual_lca(pre_test, pst_test, k = 5), "must be >= k")
})

test_that("cv_individual_lca is reproducible with seed", {
  set.seed(42)
  pre_test <- data.frame(
    item1 = rbinom(15, 1, 0.4),
    item2 = rbinom(15, 1, 0.4)
  )
  pst_test <- data.frame(
    item1 = pmin(1, pre_test$item1 + rbinom(15, 1, 0.3)),
    item2 = pmin(1, pre_test$item2 + rbinom(15, 1, 0.3))
  )

  cv1 <- cv_individual_lca(pre_test, pst_test, k = 5, seed = 999)
  cv2 <- cv_individual_lca(pre_test, pst_test, k = 5, seed = 999)

  expect_equal(cv1$mean_ll, cv2$mean_ll)
  expect_equal(cv1$perplexity, cv2$perplexity)
})
