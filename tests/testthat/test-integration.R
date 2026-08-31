test_that("complete workflow without DK works", {
  # Create test data - realistic learning scenario
  set.seed(123) # For reproducible results
  n_obs <- 20
  n_items <- 3

  # Generate pre-test data (some knowledge)
  pre_test <- data.frame(
    item1 = sample(c(0, 1), n_obs, replace = TRUE, prob = c(0.7, 0.3)),
    item2 = sample(c(0, 1), n_obs, replace = TRUE, prob = c(0.6, 0.4)),
    item3 = sample(c(0, 1), n_obs, replace = TRUE, prob = c(0.8, 0.2))
  )

  # Generate post-test data (improved knowledge)
  pst_test <- data.frame(
    item1 = pmax(pre_test$item1, sample(c(0, 1), n_obs, replace = TRUE, prob = c(0.4, 0.6))),
    item2 = pmax(pre_test$item2, sample(c(0, 1), n_obs, replace = TRUE, prob = c(0.3, 0.7))),
    item3 = pmax(pre_test$item3, sample(c(0, 1), n_obs, replace = TRUE, prob = c(0.5, 0.5)))
  )

  # Step 1: Create transition matrix
  trans_matrix <- count_item_transitions(pre_test, pst_test)

  expect_true(is.matrix(trans_matrix))
  expect_equal(nrow(trans_matrix), n_items)
  expect_equal(ncol(trans_matrix), 4) # No DK, so 4 columns
  expect_true(all(trans_matrix >= 0))
  expect_equal(sum(trans_matrix), n_obs * n_items)

  # Step 2: Fit LCA model
  lca_results <- fit_item_lca_counts(trans_matrix)

  expect_true(is.list(lca_results))
  expect_true("params" %in% names(lca_results))
  expect_true("learning" %in% names(lca_results))
  expect_equal(ncol(lca_results$params), n_items)
  expect_equal(nrow(lca_results$params), 4) # gg, gk, kk, gamma

  # Parameters should be reasonable
  expect_true(all(lca_results$params >= 0, na.rm = TRUE))
  expect_true(all(lca_results$params <= 1, na.rm = TRUE))

  # Step 3: Calculate goodness of fit
  fit_stats <- assess_item_lca_fit(lca_results, pre_test, pst_test)

  expect_s3_class(fit_stats, "guess_gof")
  expect_equal(dim(fit_stats$statistics), c(n_items, 4L))
  # The no-DK model is saturated, so there is no goodness of fit test to run.
  expect_true(all(is.na(fit_stats$statistics$statistic)))
  expect_true(all(is.na(fit_stats$statistics$p_value)))

  # Step 4: Standard correction for comparison
  lucky <- rep(0.25, n_items) # Assume 4-option multiple choice
  std_results <- stnd_cor(pre_test, pst_test, lucky)

  expect_true(is.list(std_results))
  expect_true(all(c("pre", "pst", "learn") %in% names(std_results)))
  expect_equal(length(std_results$learn), n_items)
})

test_that("complete workflow with DK works", {
  # Create test data with don't know responses
  n_obs <- 15

  # Generate mixed responses including DK
  pre_test <- data.frame(
    item1 = sample(c("0", "1", "d"), n_obs, replace = TRUE, prob = c(0.4, 0.3, 0.3)),
    item2 = sample(c("0", "1", "d"), n_obs, replace = TRUE, prob = c(0.5, 0.2, 0.3))
  )

  pst_test <- data.frame(
    item1 = sample(c("0", "1", "d"), n_obs, replace = TRUE, prob = c(0.3, 0.4, 0.3)),
    item2 = sample(c("0", "1", "d"), n_obs, replace = TRUE, prob = c(0.2, 0.5, 0.3))
  )

  # Full workflow
  trans_matrix <- count_item_transitions(pre_test, pst_test)

  expect_true(is.matrix(trans_matrix))
  expect_equal(nrow(trans_matrix), 2)
  expect_equal(ncol(trans_matrix), 9) # DK present, so 9 columns

  # LCA with DK data
  lca_results <- fit_item_lca_counts(trans_matrix)

  expect_equal(nrow(lca_results$params), 8) # DK model has 8 parameters
  expect_equal(ncol(lca_results$params), 2)

  # All parameters should be valid
  expect_true(all(lca_results$params >= 0, na.rm = TRUE))
  expect_true(all(lca_results$params <= 1, na.rm = TRUE))
})

test_that("workflow handles edge cases gracefully", {
  # Case 1: No learning (identical pre and post)
  pre_identical <- data.frame(
    item1 = c(1, 0, 1, 0, 1),
    item2 = c(0, 1, 0, 1, 0)
  )
  pst_identical <- pre_identical # Identical

  trans_matrix <- count_item_transitions(pre_identical, pst_identical)

  # Should have only diagonal transitions (x00 and x11)
  expect_equal(as.numeric(trans_matrix[, "x01"]), rep(0, 2))
  expect_equal(as.numeric(trans_matrix[, "x10"]), rep(0, 2))

  # LCA should still work
  lca_results <- fit_item_lca_counts(trans_matrix)
  expect_true(all(lca_results$learning <= 0.1, na.rm = TRUE)) # Little to no learning

  # Case 2: Perfect learning (all wrong -> all right)
  pre_wrong <- data.frame(item1 = rep(0, 10))
  pst_right <- data.frame(item1 = rep(1, 10))

  trans_matrix_perfect <- count_item_transitions(pre_wrong, pst_right)

  # Should have only x01 transitions
  expect_equal(trans_matrix_perfect[, "x00"], 0)
  expect_equal(trans_matrix_perfect[, "x10"], 0)
  expect_equal(trans_matrix_perfect[, "x11"], 0)
  expect_equal(trans_matrix_perfect[, "x01"], 10)

  # Case 3: Mixed NA values
  pre_na <- data.frame(
    item1 = c(1, NA, 0, 1, NA),
    item2 = c(NA, 1, NA, 0, 1)
  )
  pst_na <- data.frame(
    item1 = c(0, 1, NA, 1, 1),
    item2 = c(1, NA, 1, NA, 0)
  )

  # By default NAs are observed don't know responses.
  trans_matrix_na <- count_item_transitions(pre_na, pst_na)
  expect_true(is.matrix(trans_matrix_na))
  expect_equal(ncol(trans_matrix_na), 9)
  expect_equal(sum(trans_matrix_na), 10) # 5 observations * 2 items
})

test_that("workflows are consistent between approaches", {
  # Test that different methods give consistent results for same data
  set.seed(456)

  pre_test <- data.frame(
    item1 = sample(c(0, 1), 20, replace = TRUE),
    item2 = sample(c(0, 1), 20, replace = TRUE)
  )
  pst_test <- data.frame(
    item1 = sample(c(0, 1), 20, replace = TRUE),
    item2 = sample(c(0, 1), 20, replace = TRUE)
  )

  # Method 1: Step by step
  trans1 <- count_item_transitions(pre_test, pst_test)
  lca1 <- fit_item_lca_counts(trans1)

  # Method 2: Direct approach with same data
  trans2 <- count_item_transitions(pre_test, pst_test)
  lca2 <- fit_item_lca_counts(trans2)

  # Results should be identical
  expect_equal(trans1, trans2)
  expect_equal(lca1$params, lca2$params)
  expect_equal(lca1$learning, lca2$learning)

  # Standard correction should be deterministic
  lucky <- c(0.25, 0.25)
  std1 <- stnd_cor(pre_test, pst_test, lucky)
  std2 <- stnd_cor(pre_test, pst_test, lucky)

  expect_equal(std1, std2)
})

test_that("subgroup analysis works in complete workflow", {
  # Create larger dataset
  n <- 30
  pre_test <- data.frame(
    item1 = sample(c(0, 1), n, replace = TRUE),
    item2 = sample(c(0, 1), n, replace = TRUE)
  )
  pst_test <- data.frame(
    item1 = sample(c(0, 1), n, replace = TRUE),
    item2 = sample(c(0, 1), n, replace = TRUE)
  )

  # Define subgroup (first half)
  subgroup <- rep(c(TRUE, FALSE), each = n / 2)

  # Full sample analysis
  trans_full <- count_item_transitions(pre_test, pst_test)
  lca_full <- fit_item_lca_counts(trans_full)

  # Subgroup analysis
  trans_sub <- count_item_transitions(
    pre_test,
    pst_test,
    subgroup = subgroup
  )
  lca_sub <- fit_item_lca_counts(trans_sub)

  # Subgroup should have half the observations
  expect_equal(sum(trans_sub), sum(trans_full) / 2)

  # Both should produce valid results
  expect_true(all(lca_full$params >= 0, na.rm = TRUE))
  expect_true(all(lca_sub$params >= 0, na.rm = TRUE))
  expect_true(all(lca_full$params <= 1, na.rm = TRUE))
  expect_true(all(lca_sub$params <= 1, na.rm = TRUE))

  # Standard correction with subgroup
  std_full <- stnd_cor(pre_test, pst_test, c(0.25, 0.25))
  std_sub <- stnd_cor(pre_test[subgroup, ], pst_test[subgroup, ], c(0.25, 0.25))

  expect_equal(length(std_full$learn), 2)
  expect_equal(length(std_sub$learn), 2)
})
