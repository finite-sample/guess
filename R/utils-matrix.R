# Matrix Utilities
# Internal matrix manipulation utilities
# @keywords internal

#' Tabulate transitions between pre-test and post-test responses
#' @param pre_responses character vector of pre-test responses
#' @param post_responses character vector of post-test responses
#' @param include_dk whether to include the five don't-know transition cells
#' @return named vector of transition counts
#' @keywords internal
tabulate_transition_pairs <- function(
  pre_responses,
  post_responses,
  include_dk = FALSE
) {
  pairs <- paste0(pre_responses, post_responses)
  cell_names <- c("x00", "x01", "x0d", "x10", "x11", "x1d", "xd0", "xd1", "xdd")
  pair_names <- substring(cell_names, 2L)
  counts <- vapply(pair_names, function(x) sum(pairs == x), integer(1L))
  names(counts) <- cell_names

  if (include_dk) {
    counts
  } else {
    counts[c("x00", "x01", "x10", "x11")]
  }
}

#' Format transition matrix result with appropriate row and column names
#' @param transition_list list of transition vectors
#' @param item_names item names in output row order
#' @param include_aggregate whether to add an aggregate row
#' @return formatted matrix
format_transition_matrix <- function(
  transition_list,
  item_names,
  include_aggregate = FALSE
) {
  n_items <- length(item_names)
  transition_lengths <- lengths(transition_list)
  if (!all(transition_lengths %in% c(4L, 9L))) {
    stop("Transition vectors must contain either 4 or 9 cells.")
  }

  if (any(transition_lengths == 9L)) {
    dk_names <- c(
      "x00", "x01", "x0d", "x10", "x11", "x1d", "xd0", "xd1", "xdd"
    )
    transition_list <- lapply(transition_list, function(x) {
      if (length(x) == 9L) {
        return(x[dk_names])
      }

      promoted <- numeric(9L)
      names(promoted) <- dk_names
      promoted[names(x)] <- x
      promoted
    })
  }

  # Create matrix
  result_matrix <- matrix(
    unlist(transition_list),
    nrow = n_items,
    byrow = TRUE,
    dimnames = list(
      item_names,
      names(transition_list[[1]])
    )
  )

  if (include_aggregate) {
    aggregate_counts <- as.integer(colSums(result_matrix))
    result_matrix <- rbind(result_matrix, aggregate = aggregate_counts)
  }

  storage.mode(result_matrix) <- "integer"
  result_matrix
}

#' Calculate expected values for goodness of fit test
#' @param gamma_i item-specific gamma value
#' @param params estimated parameters for the item
#' @param total_obs total observations for the item
#' @param model_type "nodk" or "dk" model
#' @return vector of expected values
calculate_expected_values <- function(gamma_i, params, total_obs, model_type = "nodk") {
  if (model_type == "nodk") {
    probs <- nodk_cell_probs(params[1], params[2], params[3], gamma_i)
  } else {
    probs <- dk_cell_probs(
      params[1], params[2], params[3], params[4],
      params[5], params[6], params[7], gamma_i
    )
  }

  unname(probs * total_obs)
}
