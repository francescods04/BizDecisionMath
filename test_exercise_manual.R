# Test Script for Manual Exercise Check
source("R/05_options_pricing.R")
source("R/02_advanced_annuities.R") # Should not be needed but just in case of dependencies

# Exercise Parameters
S0 <- 30
K <- 36
T_months <- 12
r_annual_cont <- 0.05
u <- 1.3132
d <- 0.8005
n_steps <- 2

# Calculate with detailed=TRUE
result <- binomial_option_price(
  S = S0, 
  K = K, 
  r = r_annual_cont, 
  T = T_months/12, 
  type = "put", 
  n = n_steps, 
  u = u, 
  d = d, 
  detailed = TRUE
)

cat("Risk Neutral Probability (p):", result$probability, "\n")
cat("\nStock Price Tree:\n")
print(result$stock_tree)
cat("\nOption Value:", result$price, "\n")
