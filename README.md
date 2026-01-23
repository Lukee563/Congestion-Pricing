# NYC Congestion Pricing and Its Effects on Motor Vehicle Collisions

## Overview
This paper explores the causal effect of New York City’s Lower Manhattan congestion pricing policy on traffic collisions in the district of Manhattan. The study utilizes a **Difference-in-Differences (DiD)** empirical approach to evaluate how the policy, implemented on January 5th, 2025, affected total weekly collision counts.

### Key Findings
* **Significant Reduction:** We find a statistically significant decrease of approximately **20 total collisions** per week in the treatment area.
* **Impact Scale:** This represents a **16.66% reduction** from the average weekly collision count over the 10 weeks following implementation.
* **Economic/Safety Implications:** The study focuses on "reportable" collisions—those involving at least $1,000 in vehicle damage or personal injury.

---

## Methodology
The analysis compares two geographically similar areas that exhibited parallel collision trends prior to the policy:
* **Treatment Group:** Lower Manhattan (the "Congestion Relief Zone" below 60th Street / 40.7650° N).
* **Control Group:** Upper Manhattan (above 40.7650° N).

### Empirical Strategy
The study employs a linear regression model with **robust heteroscedastic standard errors** to account for non-constant variance over time:

$$crash.count = \beta_0 + \delta_0(post_i) + \beta_1(treat_i) + \delta_1(post_i \times treat_i)$$

* **Validation:** The "Parallel Trends" assumption was verified through visual trend plotting and a Treatment-Control Balance Table, confirming no statistically significant differences in pre-treatment characteristics such as time of day or injury severity.

---

## Data
The dataset is derived from the **City of New York’s Motor Vehicle Collisions-Crashes** database, maintained by the NYPD.
* **Period:** January 2023 – March 2025.
* **Scope:** 115 weeks of data, providing 230 total observations (treatment vs. control pairings).
* **Variables:** Includes collision coordinates (latitude/longitude), injury/death counts, and contributing factors.

---

## Authors
* **Luke Catalano** - *M.S. Quantitative Economics & Finance, UC Santa Cruz*
* **Eleanor Stoever**

*Project formatted in LaTeX - November 2025*
