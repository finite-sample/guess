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
LCA_PARAM_NAMES_NODK <- c("gg", "gk", "kk", "gamma")
LCA_PARAM_NAMES_DK <- c("gg", "gk", "gd", "kg", "kk", "kd", "dd", "gamma")

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

# Cell indices for transition matrix (no DK model)
# Pre-test response (0/1) x Post-test response (0/1)
CELL_00 <- 1L
CELL_01 <- 2L
CELL_10 <- 3L
CELL_11 <- 4L

# Cell indices for transition matrix (DK model)
# Pre-test response (0/1/d) x Post-test response (0/1/d)
CELL_00_DK <- 1L
CELL_01_DK <- 2L
CELL_0D <- 3L
CELL_10_DK <- 4L
CELL_11_DK <- 5L
CELL_1D <- 6L
CELL_D0 <- 7L
CELL_D1 <- 8L
CELL_DD <- 9L
