# Test Script for BizDecisionMath

# Source all R files
r_files <- list.files("R", full.names = TRUE)
sapply(r_files, source)

cat("--- Testing Set 0 (Basics) ---\n")
# Example from Slide Set 0
pv_annuity <- annuity_pv(R = 150, i = 0.075, n = 15, type = "ordinary")
cat(sprintf("PV Annuity (Expected ~1324.07): %.2f\n", pv_annuity))

cat("\n--- Testing Set 1 (Advanced Annuities) ---\n")
# Example from Slide Set 1: Geom Annuity
# R=1500, g=0.02, i=0.035, n=12
pv_geom <- geom_annuity_pv(R = 1500, g = 0.02, i = 0.035, n = 12)
cat(sprintf("PV Geom Annuity (Expected ~16069.88): %.2f\n", pv_geom))

cat("\n--- Testing Set 2 (Investment Analysis) ---\n")
# Example from Slide Set 2: Gordon Model
# d=0.75, i=0.025, g=0.015
stock_val <- gordon_model(d = 0.75, i = 0.025, g = 0.015)
cat(sprintf("Gordon Value (Expected 75): %.2f\n", stock_val))

cat("\n--- Testing Set 3 (Advanced Investment) ---\n")
# Example: Put-Call Parity
# c=4.3124, S=100, K=104, T=2, r=0.02
p_val <- put_call_parity(c = 4.3124, S = 100, K = 104, r = 0.02, T = 2)
cat(sprintf("Put Price (Expected ~4.2740): %.4f\n", p_val))

cat("\n--- Testing Set 4 (Options Pricing) ---\n")
# Example from Slide Set 4: Binomial Call
# S=100, K=106, r_periodic=0.03, T=1, n=2, u=1.1, d=0.92
# Slide says r=0.03 is "coherent with Delta t" (0.5), so it's a periodic rate.
# My function expects annual continuous rate.
# exp(r_annual * dt) = 1 + r_periodic
# r_annual * 0.5 = ln(1.03)
# r_annual = 2 * ln(1.03)

r_cont <- 2 * log(1.03)
call_price <- binomial_option_price(S = 100, K = 106, r = r_cont, T = 1, n = 2, u = 1.1, d = 0.92, type = "call")

cat(sprintf("Binomial Call Price (Expected ~5.2803): %.4f\n", call_price))
