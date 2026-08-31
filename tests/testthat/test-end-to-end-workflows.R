test_that("public item and person workflows recover no-DK simulations", {
  truth <- c(gg = 0.40, gk = 0.30, kk = 0.30)
  gamma <- c(item1 = 0.15, item2 = 0.25, item3 = 0.35, item4 = 0.45)
  sim <- simulate_lca(
    n = 3000, n_items = 4,
    gg = truth[["gg"]], gk = truth[["gk"]], kk = truth[["kk"]],
    gamma = gamma, seed = 951, return_classes = TRUE
  )

  item_fit <- fit_item_lca(sim$pre, sim$post)
  person_fit <- fit_person_lca(sim$pre, sim$post)
  posterior <- posterior_class_probs(person_fit)

  expect_s3_class(item_fit, "guess_fit")
  expect_equal(rowMeans(item_fit$params[names(truth), ]), truth, tolerance = 0.05)
  expect_equal(item_fit$params["gamma", ], gamma, tolerance = 0.05)
  expect_s3_class(person_fit, "guess_person_fit")
  expect_true(person_fit$converged)
  expect_equal(person_fit$class_priors, truth, tolerance = 0.04)
  expect_equal(person_fit$gamma, gamma, tolerance = 0.04)
  expect_equal(
    unname(rowSums(posterior)),
    rep(1, nrow(sim$pre)),
    tolerance = 1e-10
  )
  expect_equal(posterior_learned(person_fit), posterior$P_gk)
  expect_gt(cor(posterior$P_gk, as.numeric(sim$learned)), 0.8)
})

test_that("public DK workflow preserves explicit and NA-coded DK responses", {
  sim <- simulate_lca_dk(
    n = 2200, n_items = 3,
    gg = 0.25, gk = 0.15, gd = 0.10, kk = 0.15,
    dg = 0.10, dk = 0.10, dd = 0.15,
    gamma = 0.25, seed = 952
  )
  pre_na <- sim$pre
  post_na <- sim$post
  pre_na[pre_na == "d"] <- NA
  post_na[post_na == "d"] <- NA

  explicit_transitions <- count_item_transitions(sim$pre, sim$post)
  na_transitions <- count_item_transitions(pre_na, post_na)
  explicit_fit <- fit_item_lca(sim$pre, sim$post)
  na_fit <- fit_item_lca(pre_na, post_na)

  expect_equal(na_transitions, explicit_transitions)
  expect_equal(na_fit$params, explicit_fit$params, tolerance = 1e-8)
  expect_equal(na_fit$learning, explicit_fit$learning, tolerance = 1e-8)
  expect_equal(
    rowSums(na_transitions),
    stats::setNames(rep(2200, 3), names(sim$pre))
  )

  fit_stats <- assess_item_lca_fit(na_fit, pre_na, post_na)
  expect_equal(dim(fit_stats$statistics), c(3L, 4L))
  expect_true(all(is.finite(as.matrix(fit_stats$statistics))))
})

test_that("public structural-missingness workflow omits only incomplete pairs", {
  sim <- simulate_lca(n = 1600, n_items = 3, seed = 953)
  pre <- sim$pre
  post <- sim$post
  pre[1:200, 1] <- NA
  post[101:300, 2] <- NA
  complete <- !is.na(as.matrix(pre)) & !is.na(as.matrix(post))

  transitions <- count_item_transitions(pre, post, na_as = "missing")
  item_fit <- fit_item_lca(pre, post, na_as = "missing")
  person_fit <- fit_person_lca(pre, post)

  expect_equal(rowSums(transitions), colSums(complete))
  expect_equal(item_fit$n_obs, sum(complete))
  expect_equal(person_fit$n_obs, sum(complete))
  expect_error(
    fit_item_lca(
      pre, post,
      na_as = "missing", missing_action = "error"
    ),
    "Structural missing responses"
  )
})

test_that("public score workflows satisfy their definitions", {
  sim <- simulate_lca(n = 1800, n_items = 3, seed = 954)
  pre_score <- estimate_logit_score(sim$pre)
  post_score <- estimate_logit_score(sim$post)
  learning <- cross_sectional_learning(sim$pre, sim$post)
  bounded <- cross_sectional_learning_score(sim$pre, sim$post)
  expect_equal(learning, post_score - pre_score)
  expect_equal(bounded, plogis(learning))
})
