
library(BizDecisionMath)

cf <- c(-1000, 3500, 4500, 2500)
times <- c(0, 1, 2, 3)

cat("Testing calculate_irr with high return cash flows...\n")
tryCatch({
  irr <- calculate_irr(cash_flows = cf, times = times)
  print(paste("IRR:", irr))
}, warning = function(w) {
  print(paste("Warning captured:", w$message))
}, error = function(e) {
  print(paste("Error captured:", e$message))
})
