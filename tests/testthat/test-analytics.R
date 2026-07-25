test_that("call and put match the standard textbook values", {
  # S0 = 100, K = 100, r = 5%, sigma = 20%, T = 1
  expect_equal(bs_call(100, 100, 0.05, 0.2, 1), 10.4505835722, tolerance = 1e-8)
  expect_equal(bs_put(100, 100, 0.05, 0.2, 1),   5.5735260223, tolerance = 1e-8)
})

test_that("put-call parity holds", {
  S0 <- 25250; K <- 27000; r <- 0.02; sigma <- 0.18; maturity <- 4
  expect_equal(
    bs_call(S0, K, r, sigma, maturity) - bs_put(S0, K, r, sigma, maturity),
    S0 - K * exp(-r * maturity)
  )
})

test_that("a deep in-the-money call is worth its discounted intrinsic value", {
  expect_equal(bs_call(100, 1, 0.05, 0.2, 1),
               100 - 1 * exp(-0.05), tolerance = 1e-8)
})

test_that("a deep out-of-the-money call is worthless", {
  expect_equal(bs_call(100, 1e6, 0.05, 0.2, 1), 0)
})

test_that("the call price is increasing in volatility", {
  expect_lt(bs_call(100, 100, 0.05, 0.1, 1),
            bs_call(100, 100, 0.05, 0.3, 1))
})

test_that("invalid inputs are rejected", {
  expect_error(bs_call(-1,  100, 0.05,  0.2, 1))
  expect_error(bs_call(100, 100, 0.05, -0.2, 1))
  expect_error(bs_call(100, 100, 0.05,  0.2, 0))
})

test_that("Monte Carlo agrees with the closed form", {
  set.seed(123)
  S  <- simulate_gbm(S0 = 100, r = 0.05, sigma = 0.2,
                     maturity = 1, M = 50, n_paths = 2e5)
  mc <- exp(-0.05) * mean(pmax(S[, ncol(S)] - 100, 0))
  expect_equal(mc, bs_call(100, 100, 0.05, 0.2, 1), tolerance = 0.01)
})
