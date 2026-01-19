# Exam Validation Script
# Tests BizDecisionMath package against specific exercises from Past Papers 2015-2018

# Load Package (Source files directly for testing without install)
r_files <- list.files("R", full.names = TRUE)
sapply(r_files, source)

tolerance <- 0.01 # Tolerance for float comparison

check_result <- function(name, calc, expected) {
  diff <- abs(calc - expected)
  if (diff < tolerance) {
    cat(sprintf("[PASS] %s: Calc %.4f ~ Expected %.4f\n", name, calc, expected))
  } else {
    cat(sprintf("[FAIL] %s: Calc %.4f != Expected %.4f (Diff: %.4f)\n", name, calc, expected, diff))
  }
}

cat("\n=== JAN 15 2015 EXAM (Group A) ===\n")

# Exercise 1: Portfolio Selection
# Given: sigma_p = sqrt(50*mu^2 - 12*mu + 0.75)
# r = 0.06 (from solution logic: -2c + br ... r = 0.06)
# mu_M = 0.13
# (a) Beta for mu_P = 0.15
# Solution beta = 1.2857
beta_calc <- capm_beta(mu = 0.15, r_m = 0.13, r_f = 0.06)
check_result("Ex 1a (Beta)", beta_calc, 1.2857)

# (b) Efficient portfolio with sigma = 0.12. Compute mu.
# Need sigma_M first.
sigma_M <- efficient_frontier_risk(mu = 0.13, A = 50, B = -12, C = 0.75)
# CML: mu = r + (mu_M - r)/sigma_M * sigma
# (mu - r)/sigma_M is Sharpe Ratio
sharpe <- (0.13 - 0.06) / sigma_M
mu_portfolio <- 0.06 + sharpe * 0.12
check_result("Ex 1b (Expected Return)", mu_portfolio, 0.1049)


# Exercise 2: Binomial Model
# S0=25, K=24, T=26 months, rho=4% (continuous), u=1.583, d=0.689
# Two periods (n=2)
# Solution: 7.033
t_years <- 26/12
price_call <- binomial_option_price(S = 25, K = 24, r = 0.04, T = t_years, n = 2, u = 1.583, d = 0.689, type = "call")
check_result("Ex 2a (Binomial Call)", price_call, 7.033)


# Exercise 3: Annuities
# Part 1: 10 dividends, F=0.5, increasing j=1.5%. Start n=1 (posticipated).
# Part 2: Perpetuity constant G10, deferred.
# r = 3%.
# (a) Current Value P. Solution: 19.0090
F_val <- 0.5
j <- 0.015
r <- 0.03

# Part 1: Geom Annuity (Ordinary)
# First payment R = F*(1+j)
pv1 <- geom_annuity_pv(R = F_val*(1+j), g = j, i = r, n = 10, type = "ordinary")

# Part 2: Perpetuity deferred
# Payments start at n=11 with G10 = F*(1+j)^10
# So it's a perpetuity deferred by 10 years (first payment at 11)
# Ordinary perpetuity starting at 10 (first payment at 11) -> Value at 10. Discount to 0.
G10 <- F_val * (1+j)^10
# My perpetuity_pv with deferral=10 means first payment at 11 (if ordinary). 
# Wait, deferral logic:
# PV ordinary = R/i. (First payment at 1).
# Deferral = 10 -> PV * v^10. First payment at 11. Correct.
pv2 <- perpetuity_pv(R = G10, i = r, deferral = 10)

total_P <- pv1 + pv2
check_result("Ex 3a (Stock Value)", total_P, 19.0090)


cat("\n=== FEB 02 2015 EXAM (Group A) ===\n")

# Exercise 3: Loan Amortization
# S=50000, 10 constant semiannual instalments, deferred 6 months (so first at 1 semester? No, "starting after 6 months" usually means first payment at t=1 semester for ordinary annuity).
# Actually "starting after 6 months" usually means the amortization PROCESS starts, and first payment is at end of period (ordinary) or beginning (due).
# Standard loan is ordinary. So first payment at 6 months.
# Wait, if "starting after 6 months" means deferred?
# Usually "Ordinary annuity starting now" has first payment at 1.
# "Starting after 6 months" (1 semester) -> First payment at 1 semester? That IS an ordinary annuity.
# Let's check calculation.
# r_annual = 0.0609.
i2 <- calc_equivalent_rate(0.0609, 2, "annual_to_periodic") # 0.03
# Solution U = 5861.53
# U = S / a_10_0.03
U_calc <- 50000 / annuity_pv(R = 1, i = i2, n = 10, type = "ordinary")
check_result("Ex 3a (Loan Instalment)", U_calc, 5861.53)


cat("\n=== SEP 03 2015 EXAM ===\n")
# Exercise 3: Capital Constitution (Accumulation)
# Target 12000 in 4 years (16 quarters). Constant quarterly payments.
# Start 3 months from now (quarter 1). (Ordinary annuity).
# r = 5%.
# Solution: F approx 683.34
i4 <- calc_equivalent_rate(0.05, 4, "annual_to_periodic") # 1.227%
# 12000 = F * s_16_i4 (Final Value)
F_calc <- 12000 / annuity_fv(R = 1, i = i4, n = 16, type = "ordinary")
check_result("Ex 3a (Accumulation Payment)", F_calc, 683.34)

# Part (b): At end of 2nd year (8 payments paid), target 15000. 8 remaining.
# Capital accumulated at year 2:
# M2 = F * s_8_i4
M2 <- F_calc * annuity_fv(R = 1, i = i4, n = 8)
# This capital grows for 2 more years (8 quarters).
# M2_at_4 = M2 * (1+i4)^8
M2_proj <- M2 * (1+i4)^8
# Remaining needed: 15000 - M2_proj
needed <- 15000 - M2_proj
# New payment F' for remaining 8 quarters:
# needed = F' * s_8_i4
F_new <- needed / annuity_fv(R = 1, i = i4, n = 8)
check_result("Ex 3b (New Payment)", F_new, 1042.54)

