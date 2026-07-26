# DAX Index Certificate Pricing

<!-- badges: start -->
[![R-CMD-check](https://github.com/rabotnev/dax-certificate-pricing/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rabotnev/dax-certificate-pricing/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Monte Carlo valuation of two DAX-linked instruments under the risk-neutral
measure — a European call and a variance certificate paying
$(\max - \min)/\text{avg}$ of the index path.

The pricing itself is textbook. What this repository actually does is audit its
own numerical machinery: whether the variance reduction works, whether the
confidence intervals mean what they claim, and which source of error is worth
attention. All three answers turned out to be different from what the first
version of the analysis assumed.

## Key findings

**1. The reported standard error was wrong, in both directions.**

Antithetic sampling produces `n/2` correlated pairs, not `n` independent draws,
so `sd(payoffs) / sqrt(n)` is not a valid standard error. Computed the pooled
way, the reported precision was **20% too wide** for the call and **28% too
narrow** for the certificate. Validated against the empirical spread of the
price over 200 independent reruns, which the paired estimator matches to within
5% and the pooled one misses by roughly a quarter in both cases.

**2. Antithetic sampling backfires on the certificate.**

The technique relies on the payoff being monotone in the underlying shock. The
range of a Brownian path is not — it is exactly invariant under
$Z \rightarrow -Z$ in log space, so the antithetic partner is nearly a
duplicate.

| Payoff | corr(X, X′) | Effective sample size |
|---|---|---|
| European call | −0.308 | 14,452 of 10,000 |
| Variance certificate | **+0.943** | **5,147 of 10,000** |

Half the simulation budget buys nothing for the certificate. It is priced on
crude draws here; the call keeps antithetic sampling.

**3. Monte Carlo noise is the smallest error in the model.**

| Source | Size | Relative to MC noise |
|---|---|---|
| Monte Carlo noise (n = 10,000) | 0.0016 | 1× |
| Discretisation bias (M = 60) | 0.0484 | **30×** |
| Volatility wrong by 3 points | 0.0848 | **53×** |

Adding paths is the least useful available improvement. The 60-step grid
understates the continuously monitored range by **9.1%**, and volatility
misspecification dominates everything inside the model.

## Results

At `S0 = 25,250`, `r = 2%`, `sigma = 18%`, maturity 4 years, `M = 60`,
10,000 paths.

| Instrument | Price | SE | Sampling | Benchmark |
|---|---|---|---|---|
| European call, K = 27,000 | 3735.28 | 56.49 | antithetic, ESS 14,372 | Black–Scholes **3748.86** |
| Variance certificate, coupon 7.5 | 29.0280 | 0.0016 | crude | — |

The analytic Black–Scholes value lies inside the simulated 95% interval, which
validates path construction, discounting and the standard error end to end.

The certificate splits into a deterministic coupon leg of **28.5440** and a
terminal payoff of **0.4840**. The terminal leg is a lower bound: see below.

### Convergence to continuous monitoring

The discrete maximum and minimum can only understate the continuous range, so
the bias is one-sided and does not shrink with more paths. It decays like
$1/\sqrt{M}$, which makes it extrapolable:

| M | Terminal payoff |
|---|---|
| 60 | 0.4829 |
| 1000 | 0.5194 |
| ∞ (extrapolated) | **0.5313** |

Linear fit against $1/\sqrt{M}$, $R^2 = 0.998$. At `M = 60` the payoff is
understated by 0.0484, about 30 times the Monte Carlo standard error at that
setting — a confidence interval that is precise and centred on the wrong value.

## Repository

| Path | Contents |
|---|---|
| `R/gbm.R` | GBM path simulator, exact stepping, optional antithetic variates |
| `R/payoffs.R` | call and certificate payoffs, coupon leg |
| `R/analytics.R` | Black–Scholes call and put, closed form |
| `R/pricing.R` | Monte Carlo pricer with paired standard errors and effective sample size |
| `tests/` | 42 expectations across 4 files |
| `analysis/01-pricing.Rmd` | the valuation |
| `analysis/02-variance-reduction.Rmd` | does antithetic sampling help? |
| `analysis/03-discretisation.Rmd` | what the 60-step grid costs |
| `analysis/04-sensitivity.Rmd` | volatility sensitivity and the error budget |

Each report answers a question raised by the previous one.

## Reproducing

```r
renv::restore()          # exact package versions from renv.lock
devtools::test()         # 42 expectations
rmarkdown::render("analysis/01-pricing.Rmd")
```

`R CMD check` and the full test suite run on every push across macOS, Windows
and three versions of R on Ubuntu.

## Limitations

- **The certificate value is a lower bound.** The 60-step grid understates the
  continuous range by roughly 9%. Section above gives an extrapolated value;
  the headline number does not use it.
- **Volatility is assumed, not calibrated.** It is the single largest source of
  error, larger than the grid bias and roughly fifty times Monte Carlo noise.
  A market-based estimate — realised volatility from DAX returns, or an
  option-implied figure — is the obvious next step.
- **Constant volatility, no jumps.** Both prices are model prices under
  geometric Brownian motion and are not calibrated to traded DAX option prices.
- **No credit risk.** Index certificates are unsecured bank debt; issuer default
  is not modelled.

## License

MIT
