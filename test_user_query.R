# Test Script for Dividend Exercise (User Query)
source("R/05_options_pricing.R")

cat("--- Testing Exercise from User Query (Step 257) ---\n")
# Parameters extracted from user snippet
S0 <- 20
K <- 20 # Assumed from context of "Mock Exam Jan 2023" question 3, but irrelevant for stock tree/prob
r <- 0.045
T_years <- 16/12 # "two periods of 8 months each" -> T=16 months
n <- 2
u <- 1.3165
d <- 0.8066
delta <- 0 # "non-dividend-paying"

# Run Binomial
res <- binomial_option_price(S=S0, K=K, r=r, T=T_years, type="call", n=n, u=u, d=d, delta=delta, detailed=TRUE)

cat(sprintf("Calculated p: %.4f\n", res$probability))
cat(sprintf("Expected p (from snippet): 0.439\n"))

cat("\nCalculated Stock Tree:\n")
print(res$stock_tree)
cat("\nExpected Stock Tree (from snippet):\n")
cat("S1_u = 26.33, S1_d = 16.132\n")
cat("S2_uu = 34.66, S2_ud = 21.23, S2_dd = 13.01\n")
