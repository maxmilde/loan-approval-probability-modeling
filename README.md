# Loan Approval Probability Modeling

Do mortgage approval probabilities differ across racial and ethnic groups once lenders' underwriting criteria are held fixed? This project estimates that gap on 2024 Home Mortgage Disclosure Act (HMDA) data for New York State, using logit and probit models.

**Paper:** [`paper/mortgage_approval_disparities.pdf`](paper/mortgage_approval_disparities.pdf)

**Authors:** Maxim Milde, Zahid Pashayev (Charles University, 2026)

## Approach

- **Data:** HMDA loan-level records for New York, 2024. Only home-purchase applications that were approved or denied are kept. Exempt and invalid entries are cleaned, and outliers in loan amount and loan-to-value are trimmed at the 99th percentile.
- **Models:** Restricted and unrestricted logit and probit specifications. The restricted models contain underwriting, applicant, property and area controls; the unrestricted models add race and ethnicity.
- **Effects:** Average marginal effects, computed by hand (analytical derivatives for continuous variables, discrete changes for dummies) and cross-checked with `margins` using heteroskedasticity-robust (HC1) standard errors.
- **Inference:** A likelihood-ratio test of restricted against unrestricted models, a joint Wald test on the race and ethnicity terms, and the likelihood ratio index for fit.

## Key findings

- Holding the controls fixed, most minority groups have significantly lower approval probabilities. Black applicants are about 5.8 percentage points less likely to be approved, and Hispanic applicants about 5.0 percentage points (logit average marginal effects, p < 0.01).
- Adding race and ethnicity improves fit significantly under both the likelihood-ratio and Wald tests.
- The estimates are conditional, not causal. HMDA does not include credit scores, so omitted-variable bias means the results cannot by themselves establish discrimination.

## Reproduce

1. Download the 2024 New York State HMDA dataset from the [CFPB HMDA Data Browser](https://ffiec.cfpb.gov/data-browser/) and save it as `data/state_NY.csv`.
2. Run `R/loan_approval_analysis.R`.

Packages used: `dplyr`, `tidyr`, `ggplot2`, `aod`, `margins`, `sandwich`, `lmtest`.
