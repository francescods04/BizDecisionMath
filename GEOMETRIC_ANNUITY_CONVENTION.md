# GEOMETRIC ANNUITY FORMULA CONVENTION - CRITICAL FINDING

## Problem Discovered
Exam result (4.6165) didn't match our function result (4.548) - a 1.5% discrepancy.

## Root Cause Analysis

### Exam Problem Statement
```
Gn = 1.015^n × F for n = 1, 2, ..., 10
where F = 0.5
```

### Payment Timeline
```
t=1: G₁ = 1.015¹ × 0.5 = 0.5075
t=2: G₂ = 1.015² × 0.5 = 0.5151
t=3: G₃ = 1.015³ × 0.5 = 0.5228
...
t=10: G₁₀ = 1.015¹⁰ × 0.5 = 0.5803
```

### Key Insight
The notation `Gn = (1+j)^n × F` means:
- The exponent n starts at 1 (not 0)
- First payment at t=1 is F×(1+j)^1 = F(1+j)
- This is DIFFERENT from a sequence starting at F

### Standard Geometric Annuity vs Exam Convention

**Standard Definition** (our function):
- First payment: R
- Second payment: R(1+g)
- Third payment: R(1+g)²
- nth payment: R(1+g)^(n-1)

**Exam Convention** (Gn = (1+j)^n × F):
- First payment: F(1+j)^1 = F(1+j)
- Second payment: F(1+j)²
- Third payment: F(1+j)³
- nth payment: F(1+j)^n

### The Fix
To match the exam, we need to call our function with **R = F(1+j)**:
```r
# Exam gives F = 0.5, but first payment is F(1+j)
R_first_payment <- 0.5 * 1.015  # = 0.5075
geom_annuity_pv(R = 0.5075, g = 0.015, i = 0.03, n = 10)
# Result: 4.6165 ✓ MATCHES EXAM
```

## Conclusion
**Our function is CORRECT**. The discrepancy was due to:
1. The exam notation `Gn = (1+j)^n × F` has first payment at (1+j)^1, not (1+j)^0
2. We need to identify R (the first actual payment) correctly from the problem
3. When problem states Gn = (1+j)^n × F, use R = F(1+j) in our function

**NO CODE CHANGES NEEDED** - just proper parameter interpretation!
