#' Simulate GBM price paths under the risk-neutral measure
#'
#' @param S0 numeric(1), initial price, > 0
#' @param r numeric(1), continuously compounded riskless rate
#' @param sigma numeric(1), volatility, > 0
#' @param maturity numeric(1), horizon in years, > 0
#' @param M integer(1), number of time steps, >= 1
#' @param n_paths integer(1), number of paths; must be even if antithetic
#' @param antithetic logical(1), use antithetic variates
#'
#' @return numeric matrix, n_paths x (M + 1). Rows are paths, columns are
#'   time points 0 to M. Column 1 equals S0.
#' @export
simulate_gbm <- function(S0, r, sigma, maturity, M, n_paths,
                         antithetic = TRUE) {

  # --- 1. validate inputs -----------------------------------------------
  stopifnot(
    is.numeric(S0),         length(S0) == 1L,         S0 > 0,
    is.numeric(r),          length(r) == 1L,
    is.numeric(sigma),      length(sigma) == 1L,      sigma > 0,
    is.numeric(maturity),   length(maturity) == 1L,   maturity > 0,
    is.numeric(M),          length(M) == 1L,          M >= 1,
    is.numeric(n_paths),    length(n_paths) == 1L,    n_paths >= 1,
    is.logical(antithetic), length(antithetic) == 1L
  )

  if (antithetic && n_paths %% 2 != 0) {
    stop("n_paths must be even when antithetic = TRUE, got ", n_paths)
  }

  # --- 2. draw the shocks -----------------------------------------------
  if (antithetic) {
    half <- n_paths / 2
    Z <- matrix(stats::rnorm(half * M), nrow = half, ncol = M)
    Z <- rbind(Z, -Z)
  } else {
    Z <- matrix(stats::rnorm(n_paths * M), nrow = n_paths, ncol = M)
  }

  # --- 3. accumulate the log-path relative to S0 ------------------------
  dt         <- maturity / M
  drift_step <- (r - 0.5 * sigma^2) * dt
  diff_step  <- sigma * sqrt(dt)

  log_rel <- matrix(0, nrow = n_paths, ncol = M + 1)
  for (j in seq_len(M)) {
    log_rel[, j + 1] <- log_rel[, j] + drift_step + diff_step * Z[, j]
  }

  # --- 4. back to price space -------------------------------------------
  S0 * exp(log_rel)
}
