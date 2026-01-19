# Mock Exam Solution Script
# Using BizDecisionMath package 

# Load package files
sapply(list.files("R", full.names = TRUE), source)

cat("--- EXERCISE 1: ANNUITIES ---\n")
# Parameters:
# Ordinary Perpetuity, Semi-annual payments.
# R1..R10 = 500
# R11..inf = 200
# i = 4.04% (Annual)

r_annual <- 0.0404

# (a) Semi-annual rate i2
i2 <- calc_equivalent_rate(r_annual, 2, "annual_to_periodic")
cat(sprintf("(a) Semi-annual rate i2: %.4f\n", i2))

# (b) PV of Perpetuity
# Strategy: Split into two parts
# Part 1: Annuity of 500 for 10 periods (ordinary, semi-annual)
pv_part1 <- annuity_pv(R = 500, i = i2, n = 10, type = "ordinary")

# Part 2: Perpetuity of 200 starting from period 11 (ordinary)
# This is a perpetuity deferred by 10 periods.
# PV at t=10 is 200/i2. Discount 10 periods.
pv_part2 <- perpetuity_pv(R = 200, i = i2, deferral = 10)

pv_total <- pv_part1 + pv_part2
cat(sprintf("(b) PV Total: %.4f\n", pv_total))

# (c) Replace with ordinary annuity, semi-annual, duration 10 years (20 semesters)
# Find R' such that PV_annuity = PV_total
# PV_total = R' * a_20_i2
# R' = PV_total / a_20_i2
# Note: "duration of 10 years" with semi-annual payments means n=20.
annuity_factor <- annuity_pv(R = 1, i = i2, n = 20, type = "ordinary")
R_new <- pv_total / annuity_factor
cat(sprintf("(c) New Instalment R: %.4f\n", R_new))


cat("\n--- EXERCISE 2: CAPITAL PROJECT ---\n")
# Cashflows: t=0: -100, t=1: 200, t=... (missing data in prompt, assumed placeholder)
# Assuming general question about NPV properties.
# Function 'calculate_npv' exists.
# No numerical calculation possible without data, but functions cover it.
cat("(a) NPV Criterion: Use calculate_npv()\n")
cat("(b) NPV Properties: Package supports plotting NPV profile if needed (custom script).\n")
# We could demonstrate a plot
# rates <- seq(0, 0.2, by=0.01)
# npvs <- sapply(rates, function(r) calculate_npv(c(-100, 50, 60), i=r))


cat("\n--- EXERCISE 3: OPTION VALUATION ---\n")
# Parameters:
# Put Option, Non-dividend
# S0 = 30
# K = 36
# T = 12 months = 1 year
# r = 5% (instantaneous risk-free -> continuous)
# u = 1.3132
# d = 0.8005
# Two periods of 6 months (n=2)

S0 <- 30
K <- 36
r_cont <- 0.05
T_year <- 1
u <- 1.3132
d <- 0.8005
n <- 2

# (a) Stock Tree & Risk-Neutral Prob
# Use detailed=TRUE to get tree and p
res <- binomial_option_price(S = S0, K = K, r = r_cont, T = T_year, type = "put", n = n, u = u, d = d, detailed = TRUE)

cat(sprintf("(a) Risk-Neutral Probability p: %.4f\n", res$probability))
cat("(a) Stock Price Tree:\n")
print(res$stock_tree)

# (b) Put Option Price Tree
cat("(b) Put Option Tree:\n")
print(res$option_tree)
cat(sprintf("Put Price at t=0: %.4f\n", res$price))

# (c) Call Price using Put-Call Parity
# c - p = S - K * exp(-rT)
# c = p + S - K * exp(-rT)
call_price <- put_call_parity(p = res$price, S = S0, K = K, r = r_cont, T = T_year, c = NA)
cat(sprintf("(c) Call Price (Parity): %.4f\n", call_price))
