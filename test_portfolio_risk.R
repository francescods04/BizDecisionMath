# Test Portfolio Risk and Market Portfolio Functions
library(BizDecisionMath)

cat("=================================================================\n")
cat("TEST 1: Portfolio Risk (from Exam August 30th 2018)\n")
cat("=================================================================\n\n")

# Market portfolio risk calculation
sigma_m <- efficient_frontier_risk(
  mu = 0.09882,
  A = 29.25,
  B = -5.4,
  C = 0.2592,
  verbose = TRUE
)

cat("\nExpected from exam: 0.10588\n")
cat("Our result:", sigma_m, "\n\n")

cat("=================================================================\n")
cat("TEST 2: Market Portfolio Return (from Exam August 30th 2018)\n")
cat("=================================================================\n\n")

mu_m <- solve_market_portfolio_return(
  r = 0.04,
  A = 29.25,
  B = -5.4,
  C = 0.2592,
  verbose = TRUE
)

cat("\nExpected from exam: 0.09882\n")
cat("Our result:", mu_m, "\n\n")

cat("=================================================================\n")
cat("TEST 3: Risk Calculation for Different Return (Exam Sept 2015)\n")
cat("=================================================================\n\n")

sigma_m2 <- efficient_frontier_risk(
  mu = 0.128,
  A = 50,
  B = -12,
  C = 0.75,
  verbose = TRUE
)

cat("\nExpected from exam: 0.182\n")
cat("Our result:", sigma_m2, "\n")
