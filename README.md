# BizDecisionMath

An R package for **Business Decision Analysis** — covering financial mathematics, annuities, investment analysis, options pricing, and portfolio selection.

Built for students of the *Data Analysis for Business Decision* course.

---

## Installation

Install directly from GitHub:

```r
# Install devtools if you don't have it
install.packages("devtools")

# Install BizDecisionMath from GitHub
devtools::install_github("francescods04/BizDecisionMath")
```

Or load locally for development:

```r
setwd("/path/to/BizDecisionMath")
devtools::load_all()
```

---

## Features

### 1. Annuities & Perpetuities
- `pv_ordinary_annuity()` — Present value of ordinary annuity
- `fv_ordinary_annuity()` — Future value of ordinary annuity
- `pv_due_annuity()` — Present value of due annuity
- `fv_due_annuity()` — Future value of due annuity
- `pv_perpetuity()` — Present value of perpetuity
- `pv_growing_annuity()` — Present value of growing annuity
- `pv_growing_perpetuity()` — Gordon growth model / growing perpetuity

### 2. Investment Analysis
- `npv()` — Net Present Value
- `irr()` — Internal Rate of Return
- `profitability_index()` — Profitability Index
- `payback_period()` — Payback Period

### 3. Interest Rate Conversions
- `annual_to_periodic_rate()` — Convert annual rate to periodic
- `periodic_to_annual_rate()` — Convert periodic rate to annual
- `nominal_to_effective_rate()` — Nominal to effective rate
- `forward_rate()` — Calculate forward rate from spot rates

### 4. Options Pricing (Binomial Model)
- `binomial_option_price()` — Price European call/put options
- `replicating_portfolio()` — Calculate replicating portfolio (α, β)
- `risk_neutral_probability()` — Calculate risk-neutral probability π
- `put_call_parity()` — Verify or calculate using put-call parity

### 5. Portfolio Selection
- `portfolio_return()` — Expected portfolio return
- `portfolio_variance()` — Portfolio variance
- `minimum_variance_portfolio()` — Find minimum variance weights
- `efficient_frontier()` — Generate efficient frontier points

### 6. Proofs Recall
Quickly recall key proofs and derivations from the course:

```r
# See all available proofs
show_proof()

# Recall a specific proof
show_proof("put call")
show_proof("npv irr")
show_proof("risk neutral")
```

### 7. Formula Reference
Quick access to 26 key formulas:

```r
# See all available formulas
show_formula()

# Recall a specific formula
show_formula("compound interest")
show_formula("gordon")
show_formula("binomial option")
```

**Categories:** Interest & Time Value, Annuities, Perpetuities, Term Structure, Investment Analysis, Options.

**Available proofs:**
1. Relationship between annual rate and rate compounded k times a year
2. Spot rate, forward rates and their relationship
3. Present value of an ordinary annuity with instalments growing at rate g
4. Equivalence between NPV and IRR rules
5. The Put-Call Parity
6. Replicating portfolio of a call option in a binomial market
7. Risk Neutral Probability

---

## Quick Examples

### NPV Calculation
```r
# Investment: -1000 today, +300 for 5 years, rate 10%
cash_flows <- c(-1000, 300, 300, 300, 300, 300)
npv(cash_flows, rate = 0.10)
```

### Growing Annuity (Gordon Model)
```r
# First payment 100, growth 2%, discount rate 8%, 10 periods
pv_growing_annuity(R = 100, g = 0.02, i = 0.08, n = 10)
```

### Binomial Option Pricing
```r
# Stock at 100, strike 105, u=1.1, d=0.9, r=3%, call option
binomial_option_price(S0 = 100, K = 105, u = 1.1, d = 0.9, r = 0.03, type = "call")
```

### Put-Call Parity
```r
# Given call price, find put price
put_call_parity(S0 = 100, K = 105, r = 0.03, T = 1, call_price = 8)
```

---

## Verbose Mode

Most functions support `verbose = TRUE` for step-by-step exam-style solutions:

```r
npv(c(-1000, 300, 300, 300), rate = 0.10, verbose = TRUE)
```

This prints the full calculation with formulas and intermediate steps.

---

## Dependencies

- `httr` (for API calls)
- `jsonlite` (for JSON parsing)

---

## License

MIT

---

## Contributing

Pull requests welcome! Please ensure all functions include roxygen documentation and verbose output support.
