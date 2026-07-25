#' Monte Carlo price of a claim from its simulated payoffs
#'
#' @param payoffs numeric vector of terminal payoffs, one per simulated path
#' @param r numeric(1), continuously compounded riskless rate
#' @param maturity numeric(1), horizon in years, > 0
#' @param conf_level numeric(1), confidence level of the interval, in (0, 1)
#'
#' @return A list with elements
#'   \describe{
#'     \item{price}{the discounted mean payoff}
#'     \item{se}{standard error of `price`}
#'     \item{ci}{numeric(2), lower and upper confidence bounds}
#'     \item{n}{number of payoffs used}
#'   }
#' @export
price_mc <- function(payoffs, r, maturity, conf_level = 0.95) {
  stopifnot(
    is.numeric(payoffs), length(payoffs) >= 2L, all(is.finite(payoffs)),
    is.numeric(r),          length(r) == 1L,
    is.numeric(maturity),   length(maturity) == 1L, maturity > 0,
    is.numeric(conf_level), length(conf_level) == 1L,
    conf_level > 0, conf_level < 1
  )

  discount <- exp(-r * maturity)
  n        <- length(payoffs)

  price <- discount * mean(payoffs)
  se    <- discount * stats::sd(payoffs) / sqrt(n)
  z     <- stats::qnorm(1 - (1 - conf_level) / 2)

  list(
    price = price,
    se    = se,
    ci    = price + c(-1, 1) * z * se,
    n     = n
  )
}
