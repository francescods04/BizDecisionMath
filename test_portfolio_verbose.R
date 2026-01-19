# Test Portfolio Functions Verbose Output
library(BizDecisionMath)

cat("=================================================================\n")
cat("TEST 1: CAPM Beta (from Exam August 30th 2018)\n")
cat("=================================================================\n\n")

beta_p <- capm_beta(
  mu = 0.08722,
  r_m = 0.09882,
  r_f = 0.04,
  verbose = TRUE
)

cat("\nExpected from exam: 0.80279\n")
cat("Our result:", beta_p, "\n\n")

cat("=================================================================\n")
cat("TEST 2: CAPM Expected Return (from Exam August 31st 2017)\n")
cat("=================================================================\n\n")

mu_p <- capm_expected_return(
  beta = 1.2,
  r_m = 0.0944,
  r_f = 0.048,
  verbose = TRUE
)

cat("\nExpected from exam: 0.1037\n")
cat("Our result:", mu_p, "\n\n")

cat("=================================================================\n")
cat("TEST 3: Multiple Beta Calculations (Exam August 31st 2017)\n")
cat("=================================================================\n\n")

cat("Asset 1 Beta:\n")
beta1 <- capm_beta(mu = 0.08, r_m = 0.0944, r_f = 0.048, verbose = TRUE)
cat("\nExpected: 0.6897\n")
cat("Result:", beta1, "\n\n")

cat("Asset 2 Beta:\n")
beta2 <- capm_beta(mu = 0.12, r_m = 0.0944, r_f = 0.048, verbose = TRUE)
cat("\nExpected: 1.5517\n")
cat("Result:", beta2, "\n")
