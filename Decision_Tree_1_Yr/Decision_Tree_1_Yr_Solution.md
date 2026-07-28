# Healthy Unhealthy Decision Tree (1-Year Time Horizon) With Solution
Rachel Yan

## Exercise 1: Decision Tree - 1 Year Time Horizon

Consider a latent disease whereby there are three possible health
states: “Healthy”, “Unhealthy” and “Death.” A new treatment has been
discovered that has been found to reduce the probability of disease
carriers becoming unhealthy (though people must continue treatment even
after they become unhealthy). You have been asked to conduct an
appraisal of the costs and benefits of the treatment.

A randomised study found that 13 out of 1456 patients using the new
treatment became unhealthy each year, compared to 26 out of 1464 in the
control group (which is conventional management). The risk of mortality
was the same for each arm (8 deaths out of 1825 healthy patients per
year and 9 deaths out of 1095 unhealthy patients). Once patients become
unhealthy, they cannot become healthy again but all patients start off
as healthy carriers.

The cost of the new treatment is $1000 per year and the cost of treating
unhealthy patients is $6000 (SE $50) per year. There is no cost
associated with healthy patients or death.

The health utility associated with healthy patients is 0.9 (SE 0.02).
Unhealthy patients have a health utility of 0.4 (SE 0.04).

1.1. Use the information above to create a decision tree to represent
the 1st year of the situation described above.

1.2. Using the decision tree created, and the cost and probability data,
populate the model to calculate the following:

-   What is the expected cost of treatment in each strategy - the new
    treatment (‘T1’) versus conventional management (‘T0’)?
-   What is the expected QALYs of patients in each strategy?
-   What is the 1 year cost effectiveness of the new treatment?

## Modeling

First we’ll load some helpful packages.

``` r
# Note the package must first be installed with:
# install.packages("tidyverse")
# install.packages("knitr")

library(tidyverse)
```

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.2.0     ✔ readr     2.1.5
    ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ✔ ggplot2   4.0.1     ✔ tibble    3.3.0
    ✔ lubridate 1.9.4     ✔ tidyr     1.3.2
    ✔ purrr     1.2.0     
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(knitr)
```

## Defining Parameters

Now we can define parameters based on the model description.

**\*\*Question: Fill in the parameters below using the information in
the model description.**

``` r
# Probabilities
p_HU_T0 <- 26 / 1464  # probability of becoming Unhealthy when Healthy under conventional management (T0)
p_HU_T1 <- 13 / 1456  # probability of becoming Unhealthy when Healthy under treatment (T1) 
p_HD    <- 8  / 1825  # probability of Dying when Healthy

# Costs
C_H    <- 0     # annual cost of being Healthy (excluding treatment cost)
C_U    <- 6000  # annual cost of being Unhealthy (excluding treatment cost)
C_D    <- 0     # annual cost of being Dead
C_T0   <- 0     # annual cost of conventional management (T0)
C_T1   <- 1000  # annual cost of treatment (T1)

# Utilities
U_H <- 0.9  # annual utility of being Healthy
U_U <- 0.4  # annual utility of being Unhealthy
U_D <- 0    # annual utility of being Dead
```

## Building the 1 Year Decision Tree: Conventional Management (T0)

Conventional management (T0) has no initial cost so C_T0 is 0. Every
patient enters the model in a Healthy state. From this root node
patients may remain Healthy, become Unhealthy, or Die in the first-year.

``` r
# pathway probabilities 
p_H1_T0 = 1 - p_HU_T0 - p_HD      # probability the patient stays Healthy
p_U1_T0 = p_HU_T0                 # probability the patient becomes Unhealthy
p_D1_T0 = p_HD                    # probability the patient Dies

p_T0_1_table <- tribble(
  ~Outcome, ~Probability,
  "Healthy", round(p_H1_T0, 3),
  "Unhealthy", round(p_U1_T0, 3),
  "Death", round(p_D1_T0, 3)
)

kable(p_T0_1_table, caption = "1-Year Pathway Probabilities: T0")
```

<table>
<caption>1-Year Pathway Probabilities: T0</caption>
<thead>
<tr class="header">
<th style="text-align: left;">Outcome</th>
<th style="text-align: right;">Probability</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Healthy</td>
<td style="text-align: right;">0.978</td>
</tr>
<tr class="even">
<td style="text-align: left;">Unhealthy</td>
<td style="text-align: right;">0.018</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Death</td>
<td style="text-align: right;">0.004</td>
</tr>
</tbody>
</table>

1-Year Pathway Probabilities: T0

``` r
# Check: probabilities should sum to 1
sum(p_H1_T0, p_U1_T0, p_D1_T0)
```

    [1] 1

``` r
# pathway costs
cost_H1_T0 = C_H + C_T0    # cost of the patient if they stay Healthy
cost_U1_T0 = C_U + C_T0    # cost of the patient if they become Unhealthy
cost_D1_T0 = C_D + C_T0    # cost of the patient if they Die

cost_T0_1_table <- tribble(
  ~Outcome, ~Cost,
  "Healthy", scales::dollar(cost_H1_T0, accuracy = 0.01),
  "Unhealthy", scales::dollar(cost_U1_T0, accuracy = 0.01),
  "Death", scales::dollar(cost_D1_T0, accuracy = 0.01)
)

kable(cost_T0_1_table, caption = "1-Year Pathway Costs: T0")
```

<table>
<caption>1-Year Pathway Costs: T0</caption>
<thead>
<tr class="header">
<th style="text-align: left;">Outcome</th>
<th style="text-align: left;">Cost</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Healthy</td>
<td style="text-align: left;">$0.00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Unhealthy</td>
<td style="text-align: left;">$6,000.00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Death</td>
<td style="text-align: left;">$0.00</td>
</tr>
</tbody>
</table>

1-Year Pathway Costs: T0

``` r
# pathway utility
utility_H1_T0 = U_H    # utility of the patient if they stay Healthy
utility_U1_T0 = U_U    # utility of the patient if they become Unhealthy
utility_D1_T0 = U_D    # utility of the patient if they Die


utility_T0_1_table <- tribble(
  ~Outcome, ~Utility,
  "Healthy", round(utility_H1_T0, 3),
  "Unhealthy", round(utility_U1_T0, 3),
  "Death", round(utility_D1_T0, 3)
)

kable(utility_T0_1_table, caption = "1-Year Pathway Utilities: T0")
```

<table>
<caption>1-Year Pathway Utilities: T0</caption>
<thead>
<tr class="header">
<th style="text-align: left;">Outcome</th>
<th style="text-align: right;">Utility</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Healthy</td>
<td style="text-align: right;">0.9</td>
</tr>
<tr class="even">
<td style="text-align: left;">Unhealthy</td>
<td style="text-align: right;">0.4</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Death</td>
<td style="text-align: right;">0.0</td>
</tr>
</tbody>
</table>

1-Year Pathway Utilities: T0

## Building the 1 Year Decision Tree: Treatment (T1)

The treatment (T1) has an annual cost of 1000 so C_T1 is 1000. Every
patient receiving the treatment must pay the annual cost regardless of
the outcome.

``` r
# pathway probabilities 
p_H1_T1 = 1 - p_HU_T1 - p_HD      # probability the patient stays Healthy
p_U1_T1 = p_HU_T1                 # probability the patient becomes Unhealthy
p_D1_T1 = p_HD                    # probability the patient Dies

p_T1_1_table <- tribble(
  ~Outcome, ~Probability,
  "Healthy", round(p_H1_T1, 3),
  "Unhealthy", round(p_U1_T1, 3),
  "Death", round(p_D1_T1, 3)
)

kable(p_T1_1_table, caption = "1-Year Pathway Probabilities: T1")
```

<table>
<caption>1-Year Pathway Probabilities: T1</caption>
<thead>
<tr class="header">
<th style="text-align: left;">Outcome</th>
<th style="text-align: right;">Probability</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Healthy</td>
<td style="text-align: right;">0.987</td>
</tr>
<tr class="even">
<td style="text-align: left;">Unhealthy</td>
<td style="text-align: right;">0.009</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Death</td>
<td style="text-align: right;">0.004</td>
</tr>
</tbody>
</table>

1-Year Pathway Probabilities: T1

``` r
# Check: probabilities should sum to 1
sum(p_H1_T1, p_U1_T1, p_D1_T1)
```

    [1] 1

``` r
# pathway costs
cost_H1_T1 = C_H + C_T1    # cost of the patient if they stay Healthy
cost_U1_T1 = C_U + C_T1    # cost of the patient if they become Unhealthy
cost_D1_T1 = C_D + C_T1    # cost of the patient if they Die

cost_T1_1_table <- tribble(
  ~Outcome, ~Cost,
  "Healthy", scales::dollar(cost_H1_T1, accuracy = 0.01),
  "Unhealthy", scales::dollar(cost_U1_T1, accuracy = 0.01),
  "Death", scales::dollar(cost_D1_T1, accuracy = 0.01)
)

kable(cost_T1_1_table, caption = "1-Year Pathway Costs: T1")
```

<table>
<caption>1-Year Pathway Costs: T1</caption>
<thead>
<tr class="header">
<th style="text-align: left;">Outcome</th>
<th style="text-align: left;">Cost</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Healthy</td>
<td style="text-align: left;">$1,000.00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Unhealthy</td>
<td style="text-align: left;">$7,000.00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Death</td>
<td style="text-align: left;">$1,000.00</td>
</tr>
</tbody>
</table>

1-Year Pathway Costs: T1

``` r
# pathway utility
utility_H1_T1 = U_H    # utility of the patient if they stay Healthy
utility_U1_T1 = U_U    # utility of the patient if they become Unhealthy
utility_D1_T1 = U_D    # utility of the patient if they Die

utility_T1_1_table <- tribble(
  ~Outcome, ~Utility,
  "Healthy", round(utility_H1_T1, 3),
  "Unhealthy", round(utility_U1_T1, 3),
  "Death", round(utility_D1_T1, 3)
)

kable(utility_T1_1_table, caption = "1-Year Pathway Utility: T1")
```

<table>
<caption>1-Year Pathway Utility: T1</caption>
<thead>
<tr class="header">
<th style="text-align: left;">Outcome</th>
<th style="text-align: right;">Utility</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Healthy</td>
<td style="text-align: right;">0.9</td>
</tr>
<tr class="even">
<td style="text-align: left;">Unhealthy</td>
<td style="text-align: right;">0.4</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Death</td>
<td style="text-align: right;">0.0</td>
</tr>
</tbody>
</table>

1-Year Pathway Utility: T1

## Expected Cost and QALYs Per Strategy

**\*\*Question: What is the expected cost of treatment in each
strategy?**

``` r
# expected cost (p*cost) under conventional management (T0)
exp_cost_T0_1 <- c(
  Healthy = p_H1_T0 * cost_H1_T0,
  Unhealthy = p_U1_T0 * cost_U1_T0,
  Death = p_D1_T0 * cost_D1_T0
)

exp_cost_T0_1_table <- tribble(
  ~Outcome,    ~Expected_Cost,
  "Healthy",    scales::dollar(exp_cost_T0_1["Healthy"], accuracy = 1),
  "Unhealthy",  scales::dollar(exp_cost_T0_1["Unhealthy"], accuracy = 1),
  "Death",      scales::dollar(exp_cost_T0_1["Death"], accuracy = 1),
)

kable(exp_cost_T0_1_table, caption = "1-Year Expected Costs: T0")
```

<table>
<caption>1-Year Expected Costs: T0</caption>
<thead>
<tr class="header">
<th style="text-align: left;">Outcome</th>
<th style="text-align: left;">Expected_Cost</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Healthy</td>
<td style="text-align: left;">$0</td>
</tr>
<tr class="even">
<td style="text-align: left;">Unhealthy</td>
<td style="text-align: left;">$107</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Death</td>
<td style="text-align: left;">$0</td>
</tr>
</tbody>
</table>

1-Year Expected Costs: T0

``` r
# expected cost (p*cost) under treatment (T1)
exp_cost_T1_1 <- c(
  Healthy = p_H1_T1 * cost_H1_T1,
  Unhealthy = p_U1_T1 * cost_U1_T1,
  Death = p_D1_T1 * cost_D1_T1
)
  
exp_cost_T1_1_table <- tribble(
  ~Outcome,    ~Expected_Cost,
  "Healthy",    scales::dollar(exp_cost_T1_1["Healthy"], accuracy = 1),
  "Unhealthy",  scales::dollar(exp_cost_T1_1["Unhealthy"], accuracy = 1),
  "Death",      scales::dollar(exp_cost_T1_1["Death"], accuracy = 1),
)

kable(exp_cost_T1_1_table, caption = "1-Year Expected Costs: T1")
```

<table>
<caption>1-Year Expected Costs: T1</caption>
<thead>
<tr class="header">
<th style="text-align: left;">Outcome</th>
<th style="text-align: left;">Expected_Cost</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Healthy</td>
<td style="text-align: left;">$987</td>
</tr>
<tr class="even">
<td style="text-align: left;">Unhealthy</td>
<td style="text-align: left;">$62</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Death</td>
<td style="text-align: left;">$4</td>
</tr>
</tbody>
</table>

1-Year Expected Costs: T1

``` r
exp_cost_1yr <- tribble(
  ~Strategy, ~Expected_Cost,
  "T0 (Conventional)", scales::dollar(sum(exp_cost_T0_1), accuracy = 0.01),
  "T1 (Treatment)", scales::dollar(sum(exp_cost_T1_1), accuracy = 0.01)
)

kable(exp_cost_1yr, caption = "1-Year Expected Costs")
```

<table>
<caption>1-Year Expected Costs</caption>
<thead>
<tr class="header">
<th style="text-align: left;">Strategy</th>
<th style="text-align: left;">Expected_Cost</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">T0 (Conventional)</td>
<td style="text-align: left;">$106.56</td>
</tr>
<tr class="even">
<td style="text-align: left;">T1 (Treatment)</td>
<td style="text-align: left;">$1,053.57</td>
</tr>
</tbody>
</table>

1-Year Expected Costs

**\*\*Question: What is the expected QALYs of patients in each
strategy?**

``` r
# expected utility (p*utility) under conventional management (T0)
exp_qaly_T0_1 <- c(
  Healthy = p_H1_T0 * utility_H1_T0,
  Unhealthy = p_U1_T0 * utility_U1_T0,
  Death = p_D1_T0 * utility_D1_T0
)

exp_qaly_T0_1_table <- tribble(
  ~Outcome,    ~Expected_QALY,
  "Healthy",    round(exp_qaly_T0_1["Healthy"], 3),
  "Unhealthy",  round(exp_qaly_T0_1["Unhealthy"], 3),
  "Death",      round(exp_qaly_T0_1["Death"], 3),
)

kable(exp_qaly_T0_1_table, caption = "1-Year Expected QALY: T0")
```

<table>
<caption>1-Year Expected QALY: T0</caption>
<thead>
<tr class="header">
<th style="text-align: left;">Outcome</th>
<th style="text-align: right;">Expected_QALY</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Healthy</td>
<td style="text-align: right;">0.880</td>
</tr>
<tr class="even">
<td style="text-align: left;">Unhealthy</td>
<td style="text-align: right;">0.007</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Death</td>
<td style="text-align: right;">0.000</td>
</tr>
</tbody>
</table>

1-Year Expected QALY: T0

``` r
# expected QALY (p*utility) under treatment (T1)
exp_qaly_T1_1 <- c(
  Healthy = p_H1_T1 * utility_H1_T1,
  Unhealthy = p_U1_T1 * utility_U1_T1,
  Death = p_D1_T1 * utility_D1_T1
)

exp_qaly_T1_1_table <- tribble(
  ~Outcome,    ~Expected_QALY,
  "Healthy",    round(exp_qaly_T1_1["Healthy"], 3),
  "Unhealthy",  round(exp_qaly_T1_1["Unhealthy"], 3),
  "Death",      round(exp_qaly_T1_1["Death"], 3),
)

kable(exp_qaly_T1_1_table, caption = "1-Year Expected QALY: T1")
```

<table>
<caption>1-Year Expected QALY: T1</caption>
<thead>
<tr class="header">
<th style="text-align: left;">Outcome</th>
<th style="text-align: right;">Expected_QALY</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Healthy</td>
<td style="text-align: right;">0.888</td>
</tr>
<tr class="even">
<td style="text-align: left;">Unhealthy</td>
<td style="text-align: right;">0.004</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Death</td>
<td style="text-align: right;">0.000</td>
</tr>
</tbody>
</table>

1-Year Expected QALY: T1

``` r
exp_qaly_1yr <- tribble(
  ~Strategy, ~Expected_QALY,
  "T0 (Conventional)", round(sum(exp_qaly_T0_1), 3),
  "T1 (Treatment)", round(sum(exp_qaly_T1_1), 3)
)

kable(exp_qaly_1yr, caption = "1-Year Expected QALY")
```

<table>
<caption>1-Year Expected QALY</caption>
<thead>
<tr class="header">
<th style="text-align: left;">Strategy</th>
<th style="text-align: right;">Expected_QALY</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">T0 (Conventional)</td>
<td style="text-align: right;">0.887</td>
</tr>
<tr class="even">
<td style="text-align: left;">T1 (Treatment)</td>
<td style="text-align: right;">0.892</td>
</tr>
</tbody>
</table>

1-Year Expected QALY

## 1 Year ICER

**\*\*Question: What is the 1-year-cost-effectiveness of the new
treatment?**

``` r
incremental_cost_1 <- sum(exp_cost_T1_1) - sum(exp_cost_T0_1)
incremental_qaly_1 <- sum(exp_qaly_T1_1) - sum(exp_qaly_T0_1)
icer_1 <- incremental_cost_1/incremental_qaly_1

icer_1yr <- tribble(
  ~Incremental_Cost, ~Incremental_QALY, ~ICER,
  scales::dollar(incremental_cost_1, accuracy = 1), round(incremental_qaly_1, 3), scales::dollar(icer_1, accuracy = 1)
)

kable(icer_1yr, caption = "1-Year ICER")
```

<table>
<caption>1-Year ICER</caption>
<thead>
<tr class="header">
<th style="text-align: left;">Incremental_Cost</th>
<th style="text-align: right;">Incremental_QALY</th>
<th style="text-align: left;">ICER</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">$947</td>
<td style="text-align: right;">0.004</td>
<td style="text-align: left;">$214,475</td>
</tr>
</tbody>
</table>

1-Year ICER
