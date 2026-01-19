# Test Binomial Option Pricing Verbose Mode
library(BizDecisionMath)

cat("===============================================\n")
cat("TEST: Binomial Option Pricing (n=2, Call)\n")
cat("Matching Exam January 15th 2015 Group A\n")
cat("===============================================\n\n")

# From exam: S=25, K=24, r=0.04, T=26 months, u=1.583, d=0.689, n=2
i_annual <- 0.04
T_months <- 26
T_years <- T_months / 12
h <- T_years / 2  # 13/12 years per step

result <- binomial_option_price(
  S = 25,
  K = 24,
  r = i_annual,
  T = T_years,
  type = "call",
  n = 2,
  u = 1.583,
  d = 0.689,
  delta = 0,
  verbose = TRUE
)

cat("\n\nExpected from exam: ~7.033\n")
cat("Our result:", result, "\n")
