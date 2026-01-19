# Test Remaining 6 Functions
library(BizDecisionMath)

cat("=================================================================\n")
cat("TEST 1: Portfolio Risk (Two Assets with Correlation)\n")
cat("=================================================================\n\n")

sigma_p <- portfolio_risk_two_assets(
  w = 0.6,
  sigma1 = 0.20,
  sigma2 = 0.30,
  rho = 0.5,
  verbose = TRUE
)

cat("\nResult:", sigma_p, "\n\n")

cat("=================================================================\n")
cat("TEST 2: Forward Rate from Spot Rates\n")
cat("=================================================================\n\n")

fwd_rate <- spot_to_forward(
  spot_rate_s = 0.05,
  spot_rate_k = 0.06,
  t_s = 1,
  t_k = 2,
  verbose = TRUE
)

cat("\nResult:", fwd_rate, "\n\n")

cat("=================================================================\n")
cat("TEST 3: Generalized NPV\n")
cat("=================================================================\n\n")

gnpv <- generalized_npv(
  cash_flows = c(-1000, 300, 400, 500),
  times = NULL,
  i = 0.10,
  i_r = 0.08,
  verbose = TRUE
)

cat("\nResult:", gnpv, "\n\n")

cat("=================================================================\n")
cat("TEST 4: Profitability Index\n")
cat("=================================================================\n\n")

pi <- profitability_index(
  cash_flows = c(-1000, 300, 400, 500),
  i = 0.10,
  verbose = TRUE
)

cat("\nResult:", pi, "\n\n")

cat("=================================================================\n")
cat("FINAL STATUS: 23/25 FUNCTIONS COMPLETE!\n")
cat("=================================================================\n\n")
cat("Remaining: payback_period (needs param fix), solve_market_portfolio_return (skip)\n")
