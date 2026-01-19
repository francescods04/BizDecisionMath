# Test Verbose Mode for BizDecisionMath

library(BizDecisionMath)

cat("=" , rep("=", 70), "\n", sep="")
cat("TEST 1: Annuity PV (verbose=TRUE)\n")
cat("=", rep("=", 70), "\n\n", sep="")

result1 <- annuity_pv(R = 1000, i = 0.05, n = 10, verbose = TRUE)
cat("\nReturned value:", result1, "\n\n")

cat("=", rep("=", 70), "\n", sep="")
cat("TEST 2: NPV Calculation (verbose=TRUE)\n")
cat("=", rep("=", 70), "\n\n", sep="")

cf <- c(-10000, 3000, 4000, 5000, 2000)
result2 <- calculate_npv(cash_flows = cf, i = 0.08, verbose = TRUE)
cat("\nReturned value:", result2, "\n\n")

cat("=", rep("=", 70), "\n", sep="")
cat("TEST 3: IRR Calculation (verbose=TRUE)\n")
cat("=", rep("=", 70), "\n\n", sep="")

cf_irr <- c(-1000, 3500, 4500, 2500)
result3 <- calculate_irr(cash_flows = cf_irr, verbose = TRUE)
cat("\nReturned value:", result3, "\n\n")

cat("=", rep("=", 70), "\n", sep="")
cat("TEST 4: Compound Accumulation (verbose=TRUE)\n")
cat("=", rep("=", 70), "\n\n", sep="")

result4 <- calc_compound_accumulation(C = 5000, i = 0.06, t = 10, verbose = TRUE)
cat("\nReturned value:", result4, "\n\n")

cat("=", rep("=", 70), "\n", sep="")
cat("TEST 5: Gordon Model (verbose=TRUE)\n")
cat("=", rep("=", 70), "\n\n", sep="")

result5 <- gordon_model(d = 5, i = 0.10, g = 0.03, verbose = TRUE)
cat("\nReturned value:", result5, "\n\n")

cat("\n\n=== ALL TESTS COMPLETED ===\n")
