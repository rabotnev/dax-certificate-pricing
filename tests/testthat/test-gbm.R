test_that("returns the right shape, starts at S0 and stays positive", {
  S <- simulate_gbm(S0 = 100, r = 0.02, sigma = 0.2,
                    maturity = 1, M = 12, n_paths = 100)
  expect_equal(dim(S), c(100L, 13L))
  expect_true(all(S[, 1] == 100))
  expect_true(all(S > 0))
})

test_that("terminal mean matches the forward price under Q", {
  set.seed(1)
  S <- simulate_gbm(S0 = 100, r = 0.02, sigma = 0.2,
                    maturity = 1, M = 12, n_paths = 2e5)
  expect_equal(mean(S[, ncol(S)]), 100 * exp(0.02), tolerance = 0.01)
})

test_that("antithetic sampling rejects an odd number of paths", {
  expect_error(
    simulate_gbm(100, 0.02, 0.2, 1, 12, n_paths = 101, antithetic = TRUE),
    "must be even"
  )
})

test_that("antithetic partners use exactly mirrored shocks", {
  # r - sigma^2/2 == 0 here, so the drift term vanishes and the log-increments
  # of a path and its antithetic partner must cancel exactly
  set.seed(42)
  S <- simulate_gbm(S0 = 100, r = 0.02, sigma = 0.2,
                    maturity = 1, M = 4, n_paths = 10, antithetic = TRUE)
  d <- t(diff(t(log(S))))                       # n_paths x M log-increments
  expect_equal(d[1:5, ] + d[6:10, ], matrix(0, 5, 4), tolerance = 1e-12)
})
