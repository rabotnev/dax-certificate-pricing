#' Monte Carlo price of a claim from its simulated payoffs
#'
#' @param payoffs numeric vector of terminal payoffs, one per simulated path.
#'   When `antithetic = TRUE` the first half must be the original draws and the
#'   second half their antithetic partners in matching order — the layout
#'   produced by [simulate_gbm()].
#' @param r numeric(1), continuously compounded riskless rate
#' @param maturity numeric(1), horizon in years, > 0
#' @param antithetic logical(1), were the payoffs generated with antithetic
#'   variates? See Details.
#' @param conf_level numeric(1), confidence level of the interval, in (0, 1)
#'
#' @details
#' Antithetic sampling produces `n/2` *pairs*, not `n` independent draws, so the
#' pooled standard deviation over all `n` payoffs is not a valid basis for the
#' standard error. With `antithetic = TRUE` the estimator is treated as the mean
#' of the `n/2` pair averages, which is what it actually is. The point estimate
#' is identical either way; only its reported precision changes.
#'
#' Whether pairing helps or hurts depends on the payoff. For a payoff monotone
#' in the underlying shock, such as a call, the two members of a pair are
#' negatively correlated and the true standard error is *below* the pooled one.
#' For a payoff close to invariant under a sign flip of the shocks — the range
#' \eqn{\max S_t - \min S_t} of a Brownian path is exactly invariant in the
#' driftless case — the members are strongly *positively* correlated, the true
#' standard error is *above* the pooled one, and antithetic sampling costs
#' precision instead of adding it.
#'
#' `ess` is the number of independent draws that would give the same estimator
#' variance. Values below `n` mean simulation effort is being wasted.
#'
#' @return A list with elements
#'   \describe{
#'     \item{price}{the discounted mean payoff}
#'     \item{se}{standard error of `price`}
#'     \item{ci}{numeric(2), lower and upper confidence bounds}
#'     \item{n}{number of payoffs supplied}
#'     \item{ess}{effective sample size}
#'   }
#' @export
price_mc <- function(payoffs, r, maturity, antithetic = FALSE,
                     conf_level = 0.95) {
  stopifnot(
    is.numeric(payoffs), length(payoffs) >= 2L, all(is.finite(payoffs)),
    is.numeric(r),          length(r) == 1L,
    is.numeric(maturity),   length(maturity) == 1L, maturity > 0,
    is.logical(antithetic), length(antithetic) == 1L,
    is.numeric(conf_level), length(conf_level) == 1L,
    conf_level > 0, conf_level < 1
  )

  discount <- exp(-r * maturity)
  n        <- length(payoffs)
  price    <- discount * mean(payoffs)

  if (antithetic) {
    if (n %% 2 != 0) {
      stop("length(payoffs) must be even when antithetic = TRUE, got ", n)
    }
    half       <- n / 2
    pair_means <- (payoffs[seq_len(half)] + payoffs[half + seq_len(half)]) / 2
    se         <- discount * stats::sd(pair_means) / sqrt(half)
    ess        <- half * stats::var(payoffs) / stats::var(pair_means)
  } else {
    se  <- discount * stats::sd(payoffs) / sqrt(n)
    ess <- n
  }

  z <- stats::qnorm(1 - (1 - conf_level) / 2)

  list(
    price = price,
    se    = se,
    ci    = price + c(-1, 1) * z * se,
    n     = n,
    ess   = ess
  )
}
