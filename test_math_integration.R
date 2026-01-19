
# Test script to verify Math Topic Integration

library(BizDecisionMath)

# Test Cases
questions <- c(
  "What is the linear regression model?",
  "How do I calculate the confidence interval for beta?",
  "Explain hypothesis testing for regression coefficients",
  "How to predict the mean response?",
  "What is simple interest?"
)

for (q in questions) {
  cat("\n--------------------------------------------------\n")
  cat("Question:", q, "\n")
  result <- classify_question(q)
  cat("Classified Type:", result$type, "\n")
  cat("Selected Slides:", paste(result$slides, collapse = ", "), "\n")
}

cat("\n--------------------------------------------------\n")
cat("Testing get_all_slides('slides'):\n")
all_slides <- get_all_slides("/Users/francescodelsesto/Downloads/Data", "slides")
print(head(all_slides, 10))
