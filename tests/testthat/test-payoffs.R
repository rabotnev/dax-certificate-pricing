test_that("the call payoff uses only the terminal column", {
  paths <- rbind(
    c(100, 150,  90),   # ends at  90 -> 0
    c(100,  80, 120),   # ends at 120 -> 20
    c(100, 100, 100)    # ends at 100 -> 0
  )
  expect_equal(payoff_call(paths, K = 100), c(0, 20, 0))
})

test_that("the call payoff is never negative", {
  paths <- rbind(c(100, 1), c(100, 1e6))
  expect_true(all(payoff_call(paths, K = 100) >= 0))
})

test_that("the certificate payoff matches a hand computation", {
  # path 100, 120, 80, 100 -> max 120, min 80, avg 100 -> (120 - 80) / 100
  expect_equal(payoff_variance_certificate(rbind(c(100, 120, 80, 100))), 0.4)
})

test_that("a flat path has zero range and therefore zero payoff", {
  expect_equal(payoff_variance_certificate(rbind(rep(100, 5))), 0)
})

test_that("the certificate payoff is invariant to the scale of the index", {
  paths <- rbind(c(100, 130, 90, 110))
  expect_equal(payoff_variance_certificate(paths),
               payoff_variance_certificate(paths * 7))
})

test_that("payoffs return one value per path", {
  set.seed(5)
  S <- simulate_gbm(S0 = 100, r = 0.02, sigma = 0.2,
                    maturity = 1, M = 10, n_paths = 20)
  expect_length(payoff_call(S, K = 100), 20)
  expect_length(payoff_variance_certificate(S), 20)
})

test_that("the coupon leg discounts every annual payment", {
  expect_equal(pv_coupons(10, 0.05, 3),
               10 * (exp(-0.05) + exp(-0.10) + exp(-0.15)))
})

test_that("a zero rate leaves the coupon leg undiscounted", {
  expect_equal(pv_coupons(7.5, 0, 4), 30)
})

test_that("a fractional maturity is rejected", {
  expect_error(pv_coupons(7.5, 0.02, 3.5))
})
