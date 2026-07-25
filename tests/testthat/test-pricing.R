test_that("the price is the discounted mean payoff", {
  res <- price_mc(c(0, 10, 20, 30), r = 0.05, maturity = 2)
  expect_equal(res$price, exp(-0.10) * 15)
  expect_equal(res$n, 4L)
})

test_that("a zero rate leaves the payoff undiscounted", {
  expect_equal(price_mc(c(1, 3), r = 0, maturity = 5)$price, 2)
})

test_that("the confidence interval is centred on the price", {
  set.seed(9)
  res <- price_mc(stats::rexp(1000), r = 0.02, maturity = 1)
  expect_equal(mean(res$ci), res$price)
  expect_lt(res$ci[1], res$price)
  expect_gt(res$ci[2], res$price)
})

test_that("a higher confidence level widens the interval", {
  set.seed(9)
  x <- stats::rexp(1000)
  expect_lt(diff(price_mc(x, 0.02, 1, conf_level = 0.90)$ci),
            diff(price_mc(x, 0.02, 1, conf_level = 0.99)$ci))
})

test_that("the standard error shrinks like one over root n", {
  set.seed(11)
  x <- stats::rexp(4e4)
  expect_equal(price_mc(x[1:1e4], 0.02, 1)$se / price_mc(x, 0.02, 1)$se,
               2, tolerance = 0.05)
})

test_that("pricing a call reproduces the Black-Scholes value", {
  set.seed(2)
  S   <- simulate_gbm(S0 = 100, r = 0.05, sigma = 0.2,
                      maturity = 1, M = 50, n_paths = 2e5)
  res <- price_mc(payoff_call(S, K = 100), r = 0.05, maturity = 1)
  expect_equal(res$price, bs_call(100, 100, 0.05, 0.2, 1), tolerance = 0.01)
})

test_that("the point estimate does not depend on the antithetic flag", {
  set.seed(3)
  x <- stats::rexp(1000)
  expect_equal(price_mc(x, 0.02, 1, antithetic = FALSE)$price,
               price_mc(x, 0.02, 1, antithetic = TRUE)$price)
})

test_that("an odd number of payoffs is rejected when antithetic", {
  expect_error(price_mc(stats::rexp(101), 0.02, 1, antithetic = TRUE),
               "must be even")
})

test_that("pairing lowers the standard error for a monotone payoff", {
  set.seed(4)
  S <- simulate_gbm(S0 = 25250, r = 0.02, sigma = 0.18,
                    maturity = 4, M = 60, n_paths = 1e4)
  p      <- payoff_call(S, K = 27000)
  pooled <- price_mc(p, 0.02, 4, antithetic = FALSE)
  paired <- price_mc(p, 0.02, 4, antithetic = TRUE)
  expect_lt(paired$se, pooled$se)
  expect_gt(paired$ess, pooled$n)
})

test_that("pairing raises the standard error for the range payoff", {
  # (max - min) of a Brownian path is invariant under Z -> -Z, so the pair
  # members are strongly positively correlated. The pooled SE understates the
  # true error and the effective sample size falls below n.
  set.seed(4)
  S <- simulate_gbm(S0 = 25250, r = 0.02, sigma = 0.18,
                    maturity = 4, M = 60, n_paths = 1e4)
  p      <- payoff_variance_certificate(S)
  pooled <- price_mc(p, 0.02, 4, antithetic = FALSE)
  paired <- price_mc(p, 0.02, 4, antithetic = TRUE)
  expect_gt(paired$se, pooled$se)
  expect_lt(paired$ess, pooled$n)
})

test_that("effective sample size equals n when the pairs are independent", {
  set.seed(6)
  res <- price_mc(stats::rexp(2e5), 0.02, 1, antithetic = TRUE)
  expect_equal(res$ess, res$n, tolerance = 0.05)
})
