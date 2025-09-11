# Matrix Utilities
# Internal matrix manipulation utilities
# @keywords internal

#' Count transitions between pre and post test responses
#' @param pre_responses character vector of pre-test responses
#' @param pst_responses character vector of post-test responses
#' @return named vector of transition counts
#' @keywords internal
count_transitions <- function(pre_responses, pst_responses) {
  # Create paired responses
  pre_pst <- paste0(pre_responses, pst_responses)
  
  # Count all possible transitions
  x00 <- sum(pre_pst == "00")
  x01 <- sum(pre_pst == "01") 
  x0d <- sum(pre_pst == "0d")
  x10 <- sum(pre_pst == "10")
  x11 <- sum(pre_pst == "11")
  x1d <- sum(pre_pst == "1d")
  xd0 <- sum(pre_pst == "d0")
  xd1 <- sum(pre_pst == "d1")
  xdd <- sum(pre_pst == "dd")
  
  # Return appropriate vector based on whether DK responses exist
  has_dk <- (x0d + x1d + xd0 + xd1 + xdd) > 0
  
  if (has_dk) {
    result <- c(x00, x01, x0d, x10, x11, x1d, xd0, xd1, xdd)
    names(result) <- c("x00", "x01", "x0d", "x10", "x11", "x1d", "xd0", "xd1", "xdd")
  } else {
    result <- c(x00, x01, x10, x11)
    names(result) <- c("x00", "x01", "x10", "x11")
  }
  
  result
}

#' Format transition matrix result with appropriate row and column names
#' @param transition_list list of transition vectors
#' @param n_items number of items
#' @param add_aggregate whether to add aggregate row
#' @return formatted matrix
format_transition_matrix <- function(transition_list, n_items, add_aggregate = FALSE) {
  # Determine matrix dimensions
  ncols <- length(transition_list[[1]])
  
  # Create matrix
  result_matrix <- matrix(
    unlist(transition_list),
    nrow = n_items,
    byrow = TRUE,
    dimnames = list(
      paste0("item", 1:n_items),
      names(transition_list[[1]])
    )
  )
  
  # Add aggregate row if requested
  if (add_aggregate) {
    result_matrix <- rbind(result_matrix, colSums(result_matrix, na.rm = TRUE))
    rownames(result_matrix)[nrow(result_matrix)] <- "agg"
  }
  
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
    expected <- numeric(4)
    expected[1] <- (1 - gamma_i) * (1 - gamma_i) * params[1] * total_obs
    expected[2] <- ((1 - gamma_i) * gamma_i * params[1] + (1 - gamma_i) * params[2]) * total_obs
    expected[3] <- (1 - gamma_i) * params[3] * params[1] * total_obs
    expected[4] <- (gamma_i * gamma_i * params[1] + gamma_i * params[2] + params[3]) * total_obs
  } else {
    expected <- numeric(9)
    expected[1] <- (1 - gamma_i) * (1 - gamma_i) * params[1] * total_obs
    expected[2] <- ((1 - gamma_i) * gamma_i * params[1] + (1 - gamma_i) * params[2]) * total_obs
    expected[3] <- (1 - gamma_i) * params[3] * total_obs
    expected[4] <- (1 - gamma_i) * gamma_i * params[1] * total_obs
    expected[5] <- (gamma_i * gamma_i * params[1] + gamma_i * params[2] + params[4]) * total_obs
    expected[6] <- gamma_i * params[3] * total_obs
    expected[7] <- (1 - gamma_i) * params[5] * total_obs
    expected[8] <- (gamma_i * params[5] + params[6]) * total_obs
    expected[9] <- params[7] * total_obs
  }
  
  expected
}
