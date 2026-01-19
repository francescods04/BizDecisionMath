# Test: Problem Classifier
# Tests the classify_question() function with various question types

library(BizDecisionMath)

cat("=== Testing Problem Classifier ===\n\n")

# Test 1: Options question
cat("Test 1: Binomial option question... ")
result <- classify_question("Compute binomial option price with S=100, K=105, u=1.5, d=0.8")
stopifnot(result$type == "options")
stopifnot("Slide_Set_4.md" %in% result$slides)
stopifnot("binomial_option_price" %in% result$r_functions)
cat("✓ PASSED\n")

# Test 2: Portfolio question  
cat("Test 2: Portfolio/CAPM question... ")
result <- classify_question("Find beta coefficient for portfolio with μ=0.15 given frontier and CAPM")
stopifnot(result$type == "portfolio")
stopifnot("capm_beta" %in% result$r_functions)
cat("✓ PASSED\n")

# Test 3: Annuity question
cat("Test 3: Annuity question... ")
result <- classify_question("Calculate PV of ordinary annuity with R=100, i=0.05, n=10")
stopifnot(result$type == "annuities")
stopifnot("annuity_pv" %in% result$r_functions)
cat("✓ PASSED\n")

# Test 4: Perpetuity question
cat("Test 4: Perpetuity question... ")
result <- classify_question("Calculate PV of perpetuity with R=100, i=0.05")
stopifnot(result$type == "perpetuity")
stopifnot("perpetuity_pv" %in% result$r_functions)
cat("✓ PASSED\n")

# Test 5: Geometric annuity
cat("Test 5: Geometric annuity question... ")
result <- classify_question("Find PV of growing annuity with g=0.02, R=150")
stopifnot(result$type == "geometric_annuity")
stopifnot("geom_annuity_pv" %in% result$r_functions)
cat("✓ PASSED\n")

# Test 6: NPV/IRR question
cat("Test 6: Investment valuation question... ")
result <- classify_question("Calculate NPV and IRR for investment with cash flows")
stopifnot(result$type == "valuation")
stopifnot("calculate_npv" %in% result$r_functions || "calculate_irr" %in% result$r_functions)
cat("✓ PASSED\n")

# Test 7: Put-call parity
cat("Test 7: Put-call parity question... ")
result <- classify_question("Use put-call parity to find call option price given put price")
stopifnot(result$type == "options")
stopifnot("put_call_parity" %in% result$r_functions)
cat("✓ PASSED\n")

# Test 8: Basic interest
cat("Test 8: Basic compound interest question... ")
result <- classify_question("Find accumulated value using compound interest formula")
stopifnot(result$type == "basics")
stopifnot("calc_compound_accumulation" %in% result$r_functions)
cat("✓ PASSED\n")

# Test 9: General/fallback
cat("Test 9: General question (fallback)... ")
result <- classify_question("What is the difference between simple and compound interest?")
stopifnot(result$type %in% c("general", "basics"))
cat("✓ PASSED\n")

cat("\n=== All Classifier Tests Passed! ===\n")
