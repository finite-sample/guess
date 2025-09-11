# Package Constants
# Constants used throughout the package
# @keywords internal

# Valid response values for transition matrices
VALID_RESPONSE_VALUES <- c(NA_character_, "1", "0", "d")

# Response value meanings
RESPONSE_CORRECT <- "1"
RESPONSE_INCORRECT <- "0" 
RESPONSE_DK <- "d"
RESPONSE_MISSING <- NA_character_

# Transition matrix column names for different scenarios
TRANSMAT_COLS_NODK <- c("x00", "x01", "x10", "x11")
TRANSMAT_COLS_DK <- c("x00", "x01", "x0d", "x10", "x11", "x1d", "xd0", "xd1", "xdd")

# Parameter names for LCA models
LCA_PARAM_NAMES_NODK <- c("lgg", "lgk", "lkk", "gamma")
LCA_PARAM_NAMES_DK <- c("lgg", "lgk", "lgc", "lkk", "lcg", "lck", "lcc", "gamma")

# Default prior values
DEFAULT_NODK_PRIORS <- c(0.3, 0.1, 0.1, 0.25)
DEFAULT_DK_PRIORS <- c(0.3, 0.1, 0.2, 0.05, 0.1, 0.1, 0.05, 0.25)

# Default don't know knowledge estimate
DEFAULT_DK_KNOWLEDGE <- 0.03

# Optimization bounds
OPT_LOWER_BOUND <- 0
OPT_UPPER_BOUND <- 1

# Matrix dimensions
TRANSMAT_NCOL_NODK <- 4
TRANSMAT_NCOL_DK <- 9
LCA_NPARAMS_NODK <- 4
LCA_NPARAMS_DK <- 8
