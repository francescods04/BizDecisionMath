# Test Annuity & Perpetuity Verbose Output
library(BizDecisionMath)

cat("=================================================================\n")
cat("TEST 1: Geometric Annuity (from Exam Jan 2015 Group A)\n")
cat("=================================================================\n")
cat("Problem: 10 payments starting at 0.50, growing at 1.5%, i=3%\n\n")

pv_geom <- geom_annuity_pv(
  R = 0.5,
  g = 0.015,
  i = 0.03,
  n = 10,
  verbose = TRUE
)

cat("\nExpected from exam: ≈ 4.6165\n")
cat("Our result:", pv_geom, "\n\n")

cat("=================================================================\n")
cat("TEST 2: Deferred Perpetuity (from Exam Jan 2015 Group A)\n")
cat("=================================================================\n")
cat("Problem: Payment of 0.5×(1.015)^10 starting in 10 years, i=3%\n\n")

# First calculate the payment at year 10
payment_at_10 <- 0.5 * (1.015)^10
cat("Payment at year 10:", payment_at_10, "\n\n")

pv_perp <- perpetuity_pv(
  R = payment_at_10,
  i = 0.03,
  g = 0,
  deferral = 10,
  verbose = TRUE
)

cat("\nExpected from exam: ≈ 14.3925\n")
cat("Our result:", pv_perp, "\n\n")

cat("=================================================================\n")
cat("TEST 3: Total Stock Value (Sum of both components)\n")
cat("=================================================================\n")
total <- pv_geom + pv_perp
cat("Geometric annuity PV:", pv_geom, "\n")
cat("Deferred perpetuity PV:", pv_perp, "\n")
cat("Total stock value P:", total, "\n")
cat("Expected from exam: 19.0090\n")
