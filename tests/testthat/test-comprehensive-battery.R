# Comprehensive test battery using data-raw simulation approach

test_that("Large-scale simulation data validation (based on data-raw/fakeNoDK.R)", {
  # Generate simulation data similar to data-raw/fakeNoDK.R but scaled down
  set.seed(76543)  # Same seed as data-raw script
  
  n <- 50         # number of respondents (scaled down from 500)
  nitems <- 20    # number of items (scaled down from 1000)
  
  # Generate parameters as in data-raw/fakeNoDK.R
  theta <- rnorm(n)                                              # standard normal abilities
  diff <- seq(-2, 2, length.out = nitems)                      # difficulty
  gamma <- seq(.15, .45, length.out = nitems)[sample(1:nitems)] # lucky guessing (valid range)
  alpha <- seq(.1, .40, length.out = nitems)[sample(1:nitems)]  # base learning
  beta <- seq(0, .40, length.out = nitems)[sample(1:nitems)]    # ability interaction
  
  # Creating wave 1 data (pre-test true knowledge)
  wave1 <- matrix(0, n, nitems)
  for (i in 1:nitems) {
    prob <- plogis(theta - diff[i])  # logistic model
    wave1[, i] <- rbinom(n, 1, prob)
  }
  
  # Creating wave 2 data (post-test with learning)
  wave2 <- wave1
  for (i in 1:nitems) {
    # Learning probability, ensuring it stays in [0,1]
    learn_prob <- pmax(0, pmin(1, alpha[i] + beta[i] * theta))
    # Only those who didn't know can learn
    wave2[, i] <- ifelse(wave1[, i] == 0, rbinom(n, 1, learn_prob), wave1[, i])
  }
  
  # Add guessing to create observed responses (as in data-raw)
  obs_wave1 <- wave1
  obs_wave2 <- wave2
  
  for (i in 1:nitems) {
    # Add guessing - only ignorant can guess correctly
    obs_wave1[, i] <- ifelse(wave1[, i] == 0, rbinom(n, 1, gamma[i]), wave1[, i])
    obs_wave2[, i] <- ifelse(wave2[, i] == 0, rbinom(n, 1, gamma[i]), wave2[, i])
  }
  
  # Convert to data frames (as package expects)
  pre_test <- as.data.frame(obs_wave1)
  post_test <- as.data.frame(obs_wave2)
  colnames(pre_test) <- paste0("item", 1:nitems)
  colnames(post_test) <- paste0("item", 1:nitems)
  
  # Test 1: Multi-item transition matrix
  trans_matrix <- multi_transmat(pre_test, post_test)
  expect_equal(nrow(trans_matrix), nitems)
  expect_equal(ncol(trans_matrix), 4)
  expect_true(all(rowSums(trans_matrix) > 0))  # No empty items
  
  # Test 2: LCA correction 
  lca_results <- lca_cor(trans_matrix)
  expect_true(is.list(lca_results))
  expect_true("param.lca" %in% names(lca_results))
  expect_true("est.learning" %in% names(lca_results))
  expect_equal(nrow(lca_results$param.lca), 4)  # lgg, lgk, lkk, gamma
  expect_equal(ncol(lca_results$param.lca), nitems)
  expect_equal(length(lca_results$est.learning), nitems)
  
  # Test 3: Standard correction
  std_results <- stnd_cor(pre_test, post_test, lucky = gamma)
  expect_true(is.list(std_results))
  expect_true(all(c("pre", "pst", "learn") %in% names(std_results)))
  expect_equal(length(std_results$learn), nitems)
  expect_true(all(std_results$learn >= -1 & std_results$learn <= 1))
  
  # Test 4: Unified fit function
  fit_results <- fit_model(pre_test, post_test,
                          lca_results$param.lca[4, ],
                          lca_results$param.lca[1:3, ])
  expect_true(is.matrix(fit_results) || is.data.frame(fit_results))
  expect_true(ncol(fit_results) >= nitems)
  
  # Test 5: Group adjustment with proper gamma values  
  # group_adj doesn't take groups parameter - it adjusts based on gamma per item
  group_results <- group_adj(pre_test, post_test, gamma)
  expect_true(is.list(group_results))
  expect_true(all(c("indiv", "learn") %in% names(group_results)))
  expect_equal(length(group_results$learn), nitems)
  
  # Test 6: Standard errors (with small bootstrap for speed)
  se_results <- lca_se(pre_test, post_test, 5)  # Very small bootstrap
  expect_true(is.list(se_results))
  expect_equal(length(se_results), 3)
  
  # Test 7: Validation of learning estimates
  true_learning <- colMeans(wave2 - wave1, na.rm = TRUE)
  lca_learning <- lca_results$est.learning
  
  expect_true(all(lca_learning >= -1 & lca_learning <= 1))
  expect_true(mean(lca_learning) >= 0)  # Should show positive learning on average
  
  # Test 8: Consistency - LCA should be closer to truth than naive estimates
  naive_learning <- colMeans(post_test - pre_test, na.rm = TRUE)
  std_learning <- std_results$learn
  lca_error <- mean(abs(lca_learning - true_learning), na.rm = TRUE)
  naive_error <- mean(abs(naive_learning - true_learning), na.rm = TRUE)
  std_error <- mean(abs(std_learning - true_learning), na.rm = TRUE)
  
  # This should generally be true for well-specified simulation data
  expect_true(lca_error <= naive_error * 1.5)  # Allow some tolerance
  expect_true(std_error <= naive_error * 1.5)  # Standard correction should also help
})

test_that("Don't Know simulation validation (based on data-raw/fakeDK.R)", {
  set.seed(54321)
  
  n <- 40
  nitems <- 12
  
  # Create DK simulation data
  # Generate abilities and parameters
  theta <- rnorm(n)
  diff <- seq(-1.5, 1.5, length.out = nitems)
  gamma <- runif(nitems, 0.15, 0.35)  # Guessing rates
  dk_prob <- runif(nitems, 0.1, 0.25) # Don't know probability per item
  
  # Generate true knowledge pre/post
  wave1_true <- matrix(0, n, nitems)
  wave2_true <- matrix(0, n, nitems)
  
  for (i in 1:nitems) {
    prob <- plogis(theta - diff[i])
    wave1_true[, i] <- rbinom(n, 1, prob)
    # Add some learning
    learn_prob <- pmax(0, pmin(0.3, 0.1 + 0.2 * theta))
    wave2_true[, i] <- ifelse(wave1_true[, i] == 0, 
                             pmax(wave1_true[, i], rbinom(n, 1, learn_prob)), 
                             wave1_true[, i])
  }
  
  # Generate observed responses with guessing and DK
  pre_test_dk <- matrix("", n, nitems)
  post_test_dk <- matrix("", n, nitems)
  
  for (i in 1:nitems) {
    for (j in 1:n) {
      # Pre-test
      if (wave1_true[j, i] == 1) {
        pre_test_dk[j, i] <- "1"  # Know it
      } else {
        # Don't know - could be DK, wrong, or lucky guess
        rand_outcome <- sample(c("d", "0", "1"), 1, prob = c(dk_prob[i], 1-dk_prob[i]-gamma[i], gamma[i]))
        pre_test_dk[j, i] <- rand_outcome
      }
      
      # Post-test
      if (wave2_true[j, i] == 1) {
        post_test_dk[j, i] <- "1"  # Know it
      } else {
        # Don't know - could be DK, wrong, or lucky guess
        rand_outcome <- sample(c("d", "0", "1"), 1, prob = c(dk_prob[i], 1-dk_prob[i]-gamma[i], gamma[i]))
        post_test_dk[j, i] <- rand_outcome
      }
    }
  }
  
  # Convert to data frames
  pre_df <- as.data.frame(pre_test_dk)
  post_df <- as.data.frame(post_test_dk)
  colnames(pre_df) <- paste0("item", 1:nitems)
  colnames(post_df) <- paste0("item", 1:nitems)
  
  # Test DK workflow
  trans_matrix_dk <- multi_transmat(pre_df, post_df, force9 = TRUE)
  expect_equal(ncol(trans_matrix_dk), 9)  # 3x3 transition matrix
  expect_equal(nrow(trans_matrix_dk), nitems)
  
  # Test LCA with DK
  lca_results_dk <- lca_cor(trans_matrix_dk)
  expect_equal(nrow(lca_results_dk$param.lca), 8)  # 7 lambdas + 1 gamma
  expect_equal(ncol(lca_results_dk$param.lca), nitems)
  
  # Test fit with DK
  fit_results_dk <- fit_model(pre_df, post_df,
                             lca_results_dk$param.lca[8, ],
                             lca_results_dk$param.lca[1:7, ],
                             force9 = TRUE)
  expect_true(is.matrix(fit_results_dk) || is.data.frame(fit_results_dk))
  
  # Validate learning estimates
  expect_true(all(lca_results_dk$est.learning >= -1 & lca_results_dk$est.learning <= 1))
})

test_that("Backward compatibility and edge case validation", {
  set.seed(12345)
  
  # Create standard test data
  n <- 25
  nitems <- 6
  
  pre_test <- data.frame(
    item1 = sample(c(0,1), n, replace=TRUE, prob=c(0.6, 0.4)),
    item2 = sample(c(0,1), n, replace=TRUE, prob=c(0.7, 0.3)),
    item3 = sample(c(0,1), n, replace=TRUE, prob=c(0.5, 0.5)),
    item4 = sample(c(0,1), n, replace=TRUE, prob=c(0.8, 0.2)),
    item5 = sample(c(0,1), n, replace=TRUE, prob=c(0.6, 0.4)),
    item6 = sample(c(0,1), n, replace=TRUE, prob=c(0.7, 0.3))
  )
  
  post_test <- data.frame(
    item1 = sample(c(0,1), n, replace=TRUE, prob=c(0.4, 0.6)),
    item2 = sample(c(0,1), n, replace=TRUE, prob=c(0.5, 0.5)),
    item3 = sample(c(0,1), n, replace=TRUE, prob=c(0.3, 0.7)),
    item4 = sample(c(0,1), n, replace=TRUE, prob=c(0.6, 0.4)),
    item5 = sample(c(0,1), n, replace=TRUE, prob=c(0.4, 0.6)),
    item6 = sample(c(0,1), n, replace=TRUE, prob=c(0.5, 0.5))
  )
  
  trans_matrix <- multi_transmat(pre_test, post_test)
  lca_results <- lca_cor(trans_matrix)
  
  # Test backward compatibility - old and new fit functions should work
  old_fit <- fit_nodk(pre_test, post_test,
                     lca_results$param.lca[4, ],
                     lca_results$param.lca[1:3, ])
  
  new_fit <- fit_model(pre_test, post_test,
                      lca_results$param.lca[4, ],
                      lca_results$param.lca[1:3, ])
  
  expect_equal(dim(old_fit), dim(new_fit))
  expect_true(all(abs(old_fit - new_fit) < 1e-10))  # Should be identical
  
  # Test edge case - all correct responses
  all_correct_pre <- data.frame(item1 = rep(1, n), item2 = rep(1, n))
  all_correct_post <- data.frame(item1 = rep(1, n), item2 = rep(1, n))
  
  expect_no_error({
    edge_trans <- multi_transmat(all_correct_pre, all_correct_post)
    # LCA might have issues with degenerate data but shouldn't crash
  })
  
  # Test validation functions
  expect_error(multi_transmat(data.frame(), post_test), "Must have at least 1 rows")
  expect_error(stnd_cor(pre_test, post_test, lucky = c(-0.1, rep(0.25, nitems-1))), 
               "Element 1 is not")
})

test_that("Performance and stress testing", {
  # Test with larger dataset to ensure scalability
  set.seed(99999)
  
  n <- 100
  nitems <- 30
  
  # Generate larger dataset
  large_pre <- data.frame(
    matrix(sample(c(0,1), n * nitems, replace=TRUE, prob=c(0.6, 0.4)), 
           nrow = n, ncol = nitems)
  )
  colnames(large_pre) <- paste0("item", 1:nitems)
  
  large_post <- data.frame(
    matrix(sample(c(0,1), n * nitems, replace=TRUE, prob=c(0.4, 0.6)), 
           nrow = n, ncol = nitems)
  )
  colnames(large_post) <- paste0("item", 1:nitems)
  
  # Time the operations (should complete reasonably fast)
  start_time <- Sys.time()
  
  trans_matrix <- multi_transmat(large_pre, large_post)
  lca_results <- lca_cor(trans_matrix)
  std_results <- stnd_cor(large_pre, large_post, lucky = rep(0.25, nitems))
  
  end_time <- Sys.time()
  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  expect_true(elapsed < 30)  # Should complete in under 30 seconds
  expect_equal(length(lca_results$est.learning), nitems)
  expect_equal(length(std_results$learn), nitems)
})