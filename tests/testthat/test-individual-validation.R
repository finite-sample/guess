test_that("score_individual_lca matches the aggregated held-out score", {
  sim <- simulate_lca(n = 120, n_items = 2, seed = 123)
  fit <- fit_item_lca(sim$pre, sim$post)

  individual_score <- score_individual_lca(fit, sim$pre, sim$post)
  item_score <- score_item_lca(
    fit,
    count_item_transitions(sim$pre, sim$post)
  )

  expect_s3_class(individual_score, "guess_individual_score")
  expect_equal(nrow(individual_score$individual_scores), 120)
  expect_equal(
    individual_score$total_log_likelihood,
    item_score$total_log_likelihood,
    tolerance = 1e-10
  )
  expect_equal(individual_score$perplexity, item_score$perplexity, tolerance = 1e-10)
})

test_that("score_individual_lca reports individual and aggregate perplexity", {
  sim <- simulate_lca(n = 100, seed = 456)
  fit <- fit_item_lca(sim$pre, sim$post)
  score <- score_individual_lca(fit, sim$pre, sim$post)

  expect_true(all(score$individual_scores$perplexity > 0))
  expect_true(is.finite(score$perplexity))
})

test_that("score_individual_lca preserves item-name semantics", {
  sim <- simulate_lca(n = 80, n_items = 2, seed = 789)
  fit <- fit_item_lca(sim$pre, sim$post)

  original <- score_individual_lca(fit, sim$pre, sim$post)
  reordered <- score_individual_lca(fit, sim$pre, sim$post[, 2:1])
  expect_equal(original$total_log_likelihood, reordered$total_log_likelihood)

  names(sim$post)[1] <- "wrong"
  expect_error(score_individual_lca(fit, sim$pre, sim$post), "same item names")
})

test_that("score_individual_lca observes the structural-missingness contract", {
  sim <- simulate_lca(n = 40, n_items = 2, seed = 246)
  sim$pre[1, 1] <- NA
  fit <- fit_item_lca(sim$pre, sim$post, na_as = "missing")

  score <- score_individual_lca(
    fit, sim$pre, sim$post, na_as = "missing"
  )
  expect_equal(score$individual_scores$n_observations[1], 1L)
  expect_error(score_individual_lca(fit, sim$pre, sim$post), "require a DK model")
})

test_that("cv_individual_lca records every held-out fold reproducibly", {
  sim <- simulate_lca(n = 60, n_items = 2, seed = 333)
  set.seed(444)
  before <- get(".Random.seed", envir = .GlobalEnv)
  cv1 <- cv_individual_lca(sim$pre, sim$post, k = 3, seed = 555)
  after <- get(".Random.seed", envir = .GlobalEnv)
  cv2 <- cv_individual_lca(sim$pre, sim$post, k = 3, seed = 555)

  expect_s3_class(cv1, "guess_cv")
  expect_equal(nrow(cv1$fold_results), 3)
  expect_equal(sort(unique(cv1$fold_id)), 1:3)
  expect_equal(cv1$fold_id, cv2$fold_id)
  expect_equal(cv1$fold_results, cv2$fold_results)
  expect_equal(before, after)
  expect_true(all(is.finite(cv1$fold_results$test_ll)))
  expect_true(is.na(cv1$se))
})

test_that("cv_individual_lca rejects invalid folds and unsupported inputs", {
  sim <- simulate_lca(n = 10, seed = 999)

  expect_error(cv_individual_lca(sim$pre, sim$post, k = 11), "must be >= k")
  expect_error(cv_individual_lca(sim$pre, sim$post, control = 1), "control must be a list")
  expect_error(cv_individual_lca(sim$pre, sim$post, unexpected = TRUE), "must be empty")
})

test_that("cv_individual_lca never omits an optimizer-failed fold", {
  sim <- simulate_lca(n = 30, n_items = 2, seed = 135)

  expect_error(
    cv_individual_lca(
      sim$pre, sim$post, k = 3, seed = 246, control = list(outer.iter = 1L)
    ),
    "Fold 1 failed: Optimization did not converge"
  )
})

test_that("deprecated individual scoring entry points are absent", {
  expect_false("perplexity_individuals" %in% getNamespaceExports("guess"))
  expect_false("cv_individuals" %in% getNamespaceExports("guess"))
})
