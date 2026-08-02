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

test_that("perplexity_items returns positive value for fitted model", {
  transmat <- matrix(
    c(
      10, 5, 2, 8,
      12, 3, 4, 6
    ),
    nrow = 2, byrow = TRUE
  )
  colnames(transmat) <- c("x00", "x01", "x10", "x11")
  rownames(transmat) <- c("item1", "item2")

  result <- lca_cor(transmat)
  ppl <- perplexity_items(result, transmat)

  expect_true(is.numeric(ppl))
  expect_true(ppl > 0)
  expect_true(is.finite(ppl))
})

test_that("perplexity_items works with item-specific calculation", {
  transmat <- matrix(
    c(
      10, 5, 2, 8,
      12, 3, 4, 6
    ),
    nrow = 2, byrow = TRUE
  )
  colnames(transmat) <- c("x00", "x01", "x10", "x11")
  rownames(transmat) <- c("item1", "item2")

  result <- lca_cor(transmat)

  ppl1 <- perplexity_items(result, transmat, item = 1)
  ppl2 <- perplexity_items(result, transmat, item = 2)

  expect_true(is.numeric(ppl1))
  expect_true(is.numeric(ppl2))
  expect_true(ppl1 > 0)
  expect_true(ppl2 > 0)
})

test_that("perplexity_items accepts raw parameter vector", {
  params <- c(0.4, 0.3, 0.3, 0.25)
  transmat <- matrix(c(10, 5, 2, 8), nrow = 1)
  colnames(transmat) <- c("x00", "x01", "x10", "x11")

  ppl <- perplexity_items(params, transmat)

  expect_true(is.numeric(ppl))
  expect_true(ppl > 0)
})

test_that("cv_items runs and returns expected structure", {
  set.seed(42)
  transmat <- matrix(
    c(
      15, 8, 4, 13,
      12, 6, 3, 9,
      18, 5, 2, 15,
      10, 7, 5, 8,
      14, 4, 3, 19
    ),
    nrow = 5, byrow = TRUE
  )
  colnames(transmat) <- c("x00", "x01", "x10", "x11")
  rownames(transmat) <- paste0("item", 1:5)

  cv_result <- cv_items(transmat, k = 5, seed = 123)

  expect_true(is.list(cv_result))
  expect_true("fold_results" %in% names(cv_result))
  expect_true("mean_ll" %in% names(cv_result))
  expect_true("total_ll" %in% names(cv_result))
  expect_true("perplexity" %in% names(cv_result))
  expect_true("se" %in% names(cv_result))

  expect_equal(nrow(cv_result$fold_results), 5)
  expect_true(cv_result$perplexity > 0)
})

test_that("cv_items errors when k > n_items", {
  transmat <- matrix(c(10, 5, 2, 8), nrow = 1)
  colnames(transmat) <- c("x00", "x01", "x10", "x11")

  expect_error(cv_items(transmat, k = 5), "must be >= k")
})

test_that("cv_items works with dk model", {
  set.seed(42)
  transmat <- matrix(
    c(
      5, 3, 1, 2, 6, 1, 2, 1, 2,
      4, 2, 2, 3, 5, 0, 1, 2, 1,
      6, 1, 1, 1, 7, 2, 1, 1, 3,
      3, 4, 0, 2, 4, 1, 2, 2, 2,
      5, 2, 1, 3, 6, 1, 1, 1, 2
    ),
    nrow = 5, byrow = TRUE
  )
  colnames(transmat) <- c(
    "x00", "x01", "x0d", "x10", "x11", "x1d",
    "xd0", "xd1", "xdd"
  )
  rownames(transmat) <- paste0("item", 1:5)

  cv_result <- cv_items(transmat, k = 5, seed = 456)

  expect_true(is.list(cv_result))
  expect_true(cv_result$perplexity > 0)
})

test_that("lower perplexity for true model vs misspecified", {
  set.seed(123)
  true_lambdas <- c(0.4, 0.3, 0.3)
  true_gamma <- 0.25

  probs <- numeric(4)
  probs[1] <- (1 - true_gamma)^2 * true_lambdas[1]
  probs[2] <- (1 - true_gamma) * true_gamma * true_lambdas[1] +
    (1 - true_gamma) * true_lambdas[2]
  probs[3] <- (1 - true_gamma) * true_gamma * true_lambdas[1]
  probs[4] <- true_gamma^2 * true_lambdas[1] +
    true_gamma * true_lambdas[2] + true_lambdas[3]

  n <- 100
  transmat <- matrix(nrow = 5, ncol = 4)
  for (i in 1:5) {
    transmat[i, ] <- as.vector(rmultinom(1, n, probs))
  }
  colnames(transmat) <- c("x00", "x01", "x10", "x11")
  rownames(transmat) <- paste0("item", 1:5)

  true_params <- c(true_lambdas, true_gamma)
  misspec_params <- c(0.1, 0.1, 0.8, 0.5)

  ppl_true <- perplexity_items(true_params, transmat)
  ppl_misspec <- perplexity_items(misspec_params, transmat)

  expect_true(ppl_true < ppl_misspec)
})

test_that("cv_items is reproducible with seed", {
  transmat <- matrix(
    c(
      15, 8, 4, 13,
      12, 6, 3, 9,
      18, 5, 2, 15,
      10, 7, 5, 8,
      14, 4, 3, 19
    ),
    nrow = 5, byrow = TRUE
  )
  colnames(transmat) <- c("x00", "x01", "x10", "x11")
  rownames(transmat) <- paste0("item", 1:5)

  cv1 <- cv_items(transmat, k = 5, seed = 999)
  cv2 <- cv_items(transmat, k = 5, seed = 999)

  expect_equal(cv1$mean_ll, cv2$mean_ll)
  expect_equal(cv1$perplexity, cv2$perplexity)
})

# Individual-level tests

test_that("item_lca_fit is equivalent to multi_transmat + lca_cor", {
  pre_test <- data.frame(
    item1 = c(1, 0, 0, 1, 0, 1, 0, 1),
    item2 = c(1, 0, 1, 1, 0, 0, 1, 0)
  )
  pst_test <- data.frame(
    item1 = c(1, 1, 0, 1, 1, 1, 0, 1),
    item2 = c(1, 1, 1, 1, 0, 1, 1, 1)
  )

  fit1 <- item_lca_fit(pre_test, pst_test)
  transmat <- multi_transmat(pre_test, pst_test)
  fit2 <- lca_cor(transmat)

  expect_equal(fit1$params, fit2$params)
  expect_equal(fit1$learning, fit2$learning)
})

test_that("perplexity_individuals returns positive value", {
  pre_test <- data.frame(
    item1 = c(1, 0, 0, 1, 0, 1, 0, 1, 0, 0),
    item2 = c(1, 0, 1, 1, 0, 0, 1, 0, 1, 0)
  )
  pst_test <- data.frame(
    item1 = c(1, 1, 0, 1, 1, 1, 0, 1, 0, 1),
    item2 = c(1, 1, 1, 1, 0, 1, 1, 1, 1, 0)
  )

  fit <- item_lca_fit(pre_test, pst_test)
  ppl <- perplexity_individuals(fit, pre_test, pst_test)

  expect_true(is.numeric(ppl))
  expect_true(ppl > 0)
  expect_true(is.finite(ppl))
})

test_that("perplexity_individuals per_individual returns vector", {
  pre_test <- data.frame(
    item1 = c(1, 0, 0, 1, 0),
    item2 = c(1, 0, 1, 1, 0)
  )
  pst_test <- data.frame(
    item1 = c(1, 1, 0, 1, 1),
    item2 = c(1, 1, 1, 1, 0)
  )

  fit <- item_lca_fit(pre_test, pst_test)
  ppl_vec <- perplexity_individuals(fit, pre_test, pst_test, per_individual = TRUE)

  expect_length(ppl_vec, 5)
  expect_true(all(ppl_vec > 0))
})

test_that("cv_individuals runs and returns expected structure", {
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

  cv_result <- cv_individuals(pre_test, pst_test, k = 5, seed = 123)

  expect_true(is.list(cv_result))
  expect_true("fold_results" %in% names(cv_result))
  expect_true("mean_ll" %in% names(cv_result))
  expect_true("perplexity" %in% names(cv_result))
  expect_true("se" %in% names(cv_result))

  expect_equal(nrow(cv_result$fold_results), 5)
  expect_true(cv_result$perplexity > 0)
})

test_that("cv_individuals errors when k > n_individuals", {
  pre_test <- data.frame(item1 = c(1, 0))
  pst_test <- data.frame(item1 = c(1, 1))

  expect_error(cv_individuals(pre_test, pst_test, k = 5), "must be >= k")
})

test_that("cv_individuals is reproducible with seed", {
  set.seed(42)
  pre_test <- data.frame(
    item1 = rbinom(15, 1, 0.4),
    item2 = rbinom(15, 1, 0.4)
  )
  pst_test <- data.frame(
    item1 = pmin(1, pre_test$item1 + rbinom(15, 1, 0.3)),
    item2 = pmin(1, pre_test$item2 + rbinom(15, 1, 0.3))
  )

  cv1 <- cv_individuals(pre_test, pst_test, k = 5, seed = 999)
  cv2 <- cv_individuals(pre_test, pst_test, k = 5, seed = 999)

  expect_equal(cv1$mean_ll, cv2$mean_ll)
  expect_equal(cv1$perplexity, cv2$perplexity)
})
