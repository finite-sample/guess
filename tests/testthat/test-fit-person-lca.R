test_that("fit_person_lca has a stable public contract", {
  expect_named(
    formals(fit_person_lca),
    c(
      "pre_test", "post_test", "...", "missing_action", "start",
      "max_iterations", "tolerance"
    )
  )
  expect_false(exists("person_item_lca_fit", envir = asNamespace("guess"), inherits = FALSE))

  pre_test <- data.frame(item = c(0, 1))
  post_test <- data.frame(item = c(0, 1))
  expect_error(fit_person_lca(pre_test, post_test, NULL), "must be empty")
  expect_error(
    fit_person_lca(
      data.frame(item = c("d", "1")),
      data.frame(item = c("1", "1"))
    ),
    "binary responses only"
  )
})

test_that("fit_person_lca pairs items by name and preserves respondent IDs", {
  sim <- simulate_lca(
    n = 1800, n_items = 4,
    gg = 0.40, gk = 0.30, kk = 0.30,
    gamma = c(0.10, 0.20, 0.35, 0.45), seed = 811
  )
  respondent_ids <- paste0("person_", seq_len(nrow(sim$pre)))
  rownames(sim$pre) <- respondent_ids
  rownames(sim$post) <- respondent_ids

  original <- fit_person_lca(sim$pre, sim$post)
  reordered <- fit_person_lca(sim$pre, sim$post[c(4L, 2L, 1L, 3L)])

  expect_equal(original$class_priors, reordered$class_priors, tolerance = 1e-10)
  expect_equal(original$gamma, reordered$gamma[names(original$gamma)], tolerance = 1e-10)
  expect_equal(rownames(original$posterior), respondent_ids)
})

test_that("fit_person_lca validates starts and numerical controls", {
  sim <- simulate_lca(n = 1200, n_items = 3, seed = 812)
  valid_start <- list(
    class_priors = c(gg = 0.40, gk = 0.30, kk = 0.30),
    gamma = c(item1 = 0.20, item2 = 0.25, item3 = 0.30)
  )
  expect_s3_class(
    fit_person_lca(sim$pre, sim$post, start = valid_start),
    "guess_person_fit"
  )

  expect_error(
    fit_person_lca(
      sim$pre,
      sim$post,
      start = list(class_priors = valid_start$class_priors)
    ),
    "class_priors.*gamma"
  )
  expect_error(
    fit_person_lca(
      sim$pre,
      sim$post,
      start = list(
        class_priors = c(gg = 0.6, gk = 0.3, kk = 0.3),
        gamma = valid_start$gamma
      )
    ),
    "sum to 1"
  )
  expect_error(
    fit_person_lca(
      sim$pre,
      sim$post,
      start = list(
        class_priors = valid_start$class_priors,
        gamma = c(item1 = -0.1, item2 = 0.25, item3 = 0.30)
      )
    ),
    "finite named probability"
  )
  expect_error(fit_person_lca(sim$pre, sim$post, tolerance = 0), "strictly positive")
  expect_error(fit_person_lca(sim$pre, sim$post, tolerance = Inf), "tolerance")
  expect_error(
    fit_person_lca(sim$pre, sim$post, max_iterations = 1L),
    "did not converge"
  )
})

test_that("fit_person_lca preserves valid boundary estimates", {
  pre_test <- data.frame(item = rep(0, 100))
  post_test <- data.frame(item = rep(0, 100))
  start <- list(
    class_priors = c(gg = 1, gk = 0, kk = 0),
    gamma = c(item = 0)
  )

  fit <- fit_person_lca(pre_test, post_test, start = start)

  expect_identical(unname(fit$class_priors), c(1, 0, 0))
  expect_identical(unname(fit$gamma), 0)
  expect_identical(fit$log_likelihood, 0)
})
