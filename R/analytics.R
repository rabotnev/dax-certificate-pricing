#' Black-Scholes d1 and d2
#'
#' Internal helper shared by the call and put formulas.
#'
#' @inheritParams bs_call
#' @return named numeric(2) with elements `d1` and `d2`
#' @noRd
bs_d1d2 <- function(S0, K, r, sigma, maturity) {
  stopifnot(
    is.numeric(S0),       length(S0) == 1L,       S0 > 0,
    is.numeric(K),        length(K) == 1L,        K > 0,
    is.numeric(r),        length(r) == 1L,
    is.numeric(sigma),    length(sigma) == 1L,    sigma > 0,
    is.numeric(maturity), length(maturity) == 1L, maturity > 0
  )
  vol <- sigma * sqrt(maturity)
  d1  <- (log(S0 / K) + (r + 0.5 * sigma^2) * maturity) / vol
  c(d1 = d1, d2 = d1 - vol)
}

#' Black-Scholes price of a European call
#'
#' @param S0 numeric(1), spot price, > 0
#' @param K numeric(1), strike price, > 0
#' @param r numeric(1), continuously compounded riskless rate
#' @param sigma numeric(1), volatility, > 0
#' @param maturity numeric(1), time to expiry in years, > 0
#'
#' @return numeric(1), the arbitrage-free call price
#' @export
bs_call <- function(S0, K, r, sigma, maturity) {
  d <- bs_d1d2(S0, K, r, sigma, maturity)
  S0 * stats::pnorm(d[["d1"]]) -
    K * exp(-r * maturity) * stats::pnorm(d[["d2"]])
}

#' Black-Scholes price of a European put
#'
#' @inheritParams bs_call
#' @return numeric(1), the arbitrage-free put price
#' @export
bs_put <- function(S0, K, r, sigma, maturity) {
  d <- bs_d1d2(S0, K, r, sigma, maturity)
  K * exp(-r * maturity) * stats::pnorm(-d[["d2"]]) -
    S0 * stats::pnorm(-d[["d1"]])
}
