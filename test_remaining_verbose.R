# Test Remaining Verbose Functions
library(BizDecisionMath)

cat("=================================================================\n")
cat("TEST 1: Portfolio Return (Two Assets)\n")
cat("=================================================================\n\n")

mu_p <- portfolio_return_two_assets(
  w = 0.5295,
  mu1 = 0.08,
  mu2 = 0.12,
  verbose = TRUE
)

cat("\nExpected: 0.09882 (from Exam Aug 2018)\n")
cat("Result:", mu_p, "\n\n")

cat("=================================================================\n")
cat("TEST 2: Simple Discount\n")
cat("=================================================================\n\n")

pv_simple <- calc_simple_discount(
  M = 1000,
  i = 0.05,
  t = 2,
  verbose = TRUE
)

cat("\nExpected: 1000/(1+0.05*2) = 909.09\n")
cat("Result:", pv_simple, "\n\n")

cat("=================================================================\n")
cat("TEST 3: Compound Discount\n")
cat("=================================================================\n\n")

pv_compound <- calc_compound_discount(
  M = 1000,
  i = 0.05,
  t = 2,
  verbose = TRUE
)

cat("\nExpected: 1000/1.05^2 = 907.03\n")
cat("Result:", pv_compound, "\n\n")

cat("=================================================================\n")
cat("TEST 4: Payback Period\n")
cat("=================================================================\n\n")

payback <- payback_period(
  initial_investment = 10000,
  cash_flows = c(3000, 4000, 5000, 2000),
  verbose = TRUE
)

cat("\nExpected: 2.6 years (3000+4000+60% of 5000 = 10000)\n")
cat("Result:", payback, "years\n")
