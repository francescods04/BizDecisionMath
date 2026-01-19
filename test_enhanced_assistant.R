# Test: Enhanced Gemini Assistant
# Tests the enhanced ask_gemini_tutor() function
# NOTE: Requires valid GEMINI_API_KEY in environment

library(BizDecisionMath)

cat("=== Testing Enhanced Gemini Assistant ===\n\n")

# Check API key
if (Sys.getenv("GEMINI_API_KEY") == "") {
  cat("❌ ERROR: GEMINI_API_KEY not set.\n")
  cat("   Please set it with: Sys.setenv(GEMINI_API_KEY = 'your_key_here')\n")
  quit(status = 1)
}

cat("✓ API key found\n\n")

# Test Case 1: Binomial Option (from Aug 2018 Exam)
cat("=== Test Case 1: Binomial Option Pricing ===\n")
cat("Question from Aug 2018 Exam Q2...\n\n")

question1 <- "European put option on stock with S₀=28, K=32, T=30 months, ρ=4.5% (risk-free force of interest), u=1.604, d=0.656. Use 2-period binomial model (each period 15 months). Find no-arbitrage price."

response1 <- ask_gemini_tutor(
  question1,
  context_type = "auto",
  show_r_code = TRUE,
  detail_level = "exam"
)

cat("RESPONSE:\n")
cat(response1)
cat("\n\n")

# Validation checks
cat("Validation checks:\n")
checks_passed <- 0
total_checks <- 0

total_checks <- total_checks + 1
if (grepl("π", response1) || grepl("pi", response1, ignore.case = TRUE)) {
  cat("✓ Includes risk-neutral probability\n")
  checks_passed <- checks_passed + 1
} else {
  cat("✗ Missing risk-neutral probability\n")
}

total_checks <- total_checks + 1
if (grepl("Substituting:|substituting:", response1, ignore.case = TRUE)) {
  cat("✓ Shows explicit substitution\n")
  checks_passed <- checks_passed + 1
} else {
  cat("✗ Missing explicit substitution\n")
}

total_checks <- total_checks + 1
if (grepl("■|Final:|Result:", response1)) {
  cat("✓ Has final answer marker\n")
  checks_passed <- checks_passed + 1
} else {
  cat("✗ Missing final answer marker\n")
}

total_checks <- total_checks + 1
if (grepl("binomial_option_price", response1, ignore.case = TRUE)) {
  cat("✓ Mentions R function\n")
  checks_passed <- checks_passed + 1
} else {
  cat("✗ Doesn't mention R function\n")
}

total_checks <- total_checks + 1
if (grepl("7\\.|7\\.0|7\\.02", response1)) {
  cat("✓ Answer is approximately 7.025 (expected)\n")
  checks_passed <- checks_passed + 1
} else {
  cat("⚠ Answer may not match expected value (~7.025)\n")
}

cat(sprintf("\nTest 1 Score: %d / %d checks passed\n", checks_passed, total_checks))

# Manual review prompt
cat("\n")
cat("========================================\n")
cat("MANUAL REVIEW REQUIRED:\n")
cat("Compare the response above to:\n")
cat("file:///Users/francescodelsesto/Downloads/Data/Exam_August_30th2018_Solution.md (lines 28-62)\n")
cat("========================================\n\n")

cat("Tests complete. Review the output quality.\n")
