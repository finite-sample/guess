test_that("count_item_transitions has a stable public contract", {
  expect_named(
    formals(count_item_transitions),
    c(
      "pre_test", "post_test", "...", "subgroup", "include_aggregate",
      "na_as", "missing_action"
    )
  )

  pre_test <- data.frame(item = c(0, 1))
  post_test <- data.frame(item = c(1, 1))
  expect_visible(count_item_transitions(pre_test, post_test))
  expect_type(count_item_transitions(pre_test, post_test), "integer")
  expect_error(
    count_item_transitions(pre_test, post_test, NULL),
    "must be empty"
  )
  expect_false(
    exists("multi_transmat", envir = asNamespace("guess"), inherits = FALSE)
  )
})

test_that("item columns are paired by unique names", {
  pre_test <- data.frame(
    algebra = c(0, 0, 1, 1),
    reading = c(1, 1, 0, 0)
  )
  post_test <- data.frame(
    reading = c(1, 0, 1, 0),
    algebra = c(0, 1, 1, 1)
  )

  result <- count_item_transitions(pre_test, post_test)

  expect_equal(rownames(result), c("algebra", "reading"))
  expect_equal(
    result["algebra", ],
    count_transitions(pre_test$algebra, post_test$algebra)
  )
  expect_equal(
    result["reading", ],
    count_transitions(pre_test$reading, post_test$reading)
  )
})

test_that("invalid item frames and names are rejected", {
  valid <- data.frame(item = 1:3)
  expect_error(count_item_transitions(NULL, valid), "data.frame")
  expect_error(count_item_transitions(valid, NULL), "data.frame")
  expect_error(count_item_transitions(as.matrix(valid), valid), "data.frame")
  expect_error(
    count_item_transitions(valid, data.frame(item = 1:2)),
    "3 rows"
  )
  expect_error(
    count_item_transitions(valid, data.frame(other = 1:3)),
    "same item names"
  )

  duplicate_names <- data.frame(a = 1:3, b = 1:3)
  names(duplicate_names) <- c("item", "item")
  expect_error(
    count_item_transitions(duplicate_names, duplicate_names),
    "unique, non-empty"
  )

  empty_name <- valid
  names(empty_name) <- ""
  expect_error(
    count_item_transitions(empty_name, empty_name),
    "unique, non-empty"
  )
})

test_that("response schema is consistent across items", {
  pre_test <- data.frame(
    binary = c(0, 0, 1, 1),
    dk = c("0", "d", "1", "d")
  )
  post_test <- data.frame(
    binary = c(0, 1, 0, 1),
    dk = c("1", "1", "1", "d")
  )

  automatic <- count_item_transitions(pre_test, post_test)
  expect_equal(dim(automatic), c(2L, 9L))
  expect_equal(rowSums(automatic), c(binary = 4, dk = 4))
  expect_equal(
    unname(automatic["binary", c("x0d", "x1d", "xd0", "xd1", "xdd")]),
    rep(0L, 5L)
  )

  expect_error(
    count_item_transitions(pre_test, post_test, response_schema = "binary"),
    "empty"
  )
})

test_that("subgroup and structural missingness control usable pairs", {
  pre_test <- data.frame(item = c(1, NA, 0, 1))
  post_test <- data.frame(item = c(1, 1, NA, 0))

  result <- count_item_transitions(
    pre_test,
    post_test,
    subgroup = c(TRUE, TRUE, FALSE, TRUE),
    na_as = "missing"
  )

  expect_equal(sum(result), 2L)
  expect_equal(result["item", c("x10", "x11")], c(x10 = 1L, x11 = 1L))
  expect_error(
    count_item_transitions(
      pre_test,
      post_test,
      subgroup = c(TRUE, NA, FALSE, TRUE)
    ),
    "missing"
  )
})

test_that("aggregate counts equal exact column sums", {
  pre_test <- data.frame(
    algebra = c(1, 0, 0, 1),
    reading = c(0, 1, 0, 1)
  )
  post_test <- data.frame(
    algebra = c(1, 1, 0, 0),
    reading = c(0, 1, 1, 1)
  )

  items <- count_item_transitions(pre_test, post_test)
  pooled <- count_item_transitions(
    pre_test,
    post_test,
    include_aggregate = TRUE
  )

  expect_equal(rownames(pooled), c("algebra", "reading", "aggregate"))
  expect_equal(pooled[c("algebra", "reading"), ], items)
  expect_equal(
    unname(pooled["aggregate", ]),
    as.integer(colSums(items))
  )
  expect_error(
    count_item_transitions(
      data.frame(aggregate = 0:1),
      data.frame(aggregate = 1:0),
      include_aggregate = TRUE
    ),
    "cannot be named"
  )
  expect_error(
    count_item_transitions(pre_test, post_test, include_aggregate = 1),
    "logical flag"
  )
})
