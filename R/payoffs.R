#' Terminal payoff of a European call
#'
#' @param paths numeric matrix, `n_paths x (M + 1)`, as returned by
#'   [simulate_gbm()]. Only the final column is used.
#' @param K numeric(1), strike price, > 0
#'
#' @return numeric vector of length `nrow(paths)`
#' @export
payoff_call <- function(paths, K) {
  stopifnot(
    is.matrix(paths), is.numeric(paths), ncol(paths) >= 2L,
    is.numeric(K), length(K) == 1L, K > 0
  )
  pmax(paths[, ncol(paths)] - K, 0)
}

#' Terminal payoff of the variance certificate
#'
#' The range of each path divided by its average level,
#' \eqn{(\max S_t - \min S_t) / \mathrm{avg}(S_t)}, with the initial value at
#' \eqn{t = 0} included in all three statistics. The payoff is a dimensionless
#' ratio and is therefore invariant to the scale of the underlying.
#'
#' @param paths numeric matrix, `n_paths x (M + 1)`, as returned by
#'   [simulate_gbm()]
#'
#' @return numeric vector of length `nrow(paths)`
#' @export
payoff_variance_certificate <- function(paths) {
  # Only the structure is validated here: a full positivity scan would cost
  # O(n_paths * M) on every call, and GBM paths are positive by construction.
  stopifnot(is.matrix(paths), is.numeric(paths), ncol(paths) >= 2L)

  path_max <- apply(paths, 1L, max)
  path_min <- apply(paths, 1L, min)
  (path_max - path_min) / rowMeans(paths)
}

#' Present value of a stream of fixed annual coupons
#'
#' Coupons are paid at the end of each year and discounted continuously.
#'
#' @param coupon numeric(1), amount paid at the end of every year
#' @param r numeric(1), continuously compounded riskless rate
#' @param maturity numeric(1), whole number of years, >= 1
#'
#' @return numeric(1), the present value of the coupon leg
#' @export
pv_coupons <- function(coupon, r, maturity) {
  stopifnot(
    is.numeric(coupon),   length(coupon) == 1L,
    is.numeric(r),        length(r) == 1L,
    is.numeric(maturity), length(maturity) == 1L,
    maturity >= 1, maturity == round(maturity)
  )
  times <- seq_len(maturity)
  sum(coupon * exp(-r * times))
}
