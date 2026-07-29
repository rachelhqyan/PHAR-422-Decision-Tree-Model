---
title: "Healthy_Unhealthy_Decision_Tree_(2_Year_Time_Horizon)"
output: md
author: Rachel Yan
---

## Exercise 2: Decision Tree - 2 Year Time Horizon

Consider a latent disease whereby there are three possible health states: “Healthy”, “Unhealthy” and “Death.” A new treatment has been discovered that has been found to reduce the probability of disease carriers becoming unhealthy (though people must continue treatment even after they become unhealthy). You have been asked to conduct an appraisal of the costs and benefits of the treatment.

A randomised study found that 13 out of 1456 patients using the new treatment became unhealthy each year, compared to 26 out of 1464 in the control group (which is conventional management). The risk of mortality was the same for each arm (8 deaths out of 1825 healthy patients per year and 9 deaths out of 1095 unhealthy patients). Once patients become unhealthy, they cannot become healthy again but all patients start off as healthy carriers.

The cost of the new treatment is $1000 per year and the cost of treating unhealthy patients is $6000 (SE $50) per year. There is no cost associated with healthy patients or death. 

The health utility associated with healthy patients is 0.9 (SE 0.02). Unhealthy patients have a health utility of 0.4 (SE 0.04). 

2.1. Using the same information from exercise 1, extend your decision tree structure to represent a 2 year time horizon

2.2. Using the decision tree structure created, and the cost and probability data, populate the model to calculate the following: 

 - What is the expected cost of treatment in each strategy - the new treatment ('T1') versus conventional management ('T0')?
 - What is the expected QALYs of patients in each strategy?
 - What is the 2 year cost effectiveness of the new treatment?
  
## Modeling

First we'll load some helpful packages.

```{r}
# Note the package must first be installed with:
# install.packages("tidyverse")
# install.packages("knitr")

library(tidyverse)
library(knitr)
```

## Defining Parameters

Now we can define parameters based on the model description.

**\*\*Question: Fill in the parameters below using the information in the model description.**

```{r}
# Probabilities
p_HU_T0 <- 26 / 1464  # probability of becoming Unhealthy when Healthy under conventional management (T0)
p_HU_T1 <- 13 / 1456  # probability of becoming Unhealthy when Healthy under treatment (T1)
p_HD    <- 8  / 1825  # probability of Dying when Healthy
p_UD    <- 9  / 1095  # probability of Dying when Unhealthy

## Costs
C_H    <- 0     # annual cost of being Healthy (excluding treatment cost)
C_U    <- 6000  # annual cost of being Unhealthy (excluding treatment cost)
C_D    <- 0     # annual cost of being Dead
C_T0   <- 0     # annual cost of conventional management (T0)
C_T1   <- 1000  # annual cost of treatment (T1)

## Utilities
U_H <- 0.9  # annual utility of being Healthy
U_U <- 0.4  # annual utility of being Unhealthy
U_D <- 0    # annual utility of being Dead
```

## Building the 2 Year Decision Tree: Conventional Management (T0)

Conventional management (T0) has no initial cost so C_T0 is $0. Every patient enters the model in a Healthy state. From this root node patients may remain Healthy, become Unhealthy, or Die in the first-year time horizon. In the second year, each year 1 state becomes a decision node with different transition structures.

```{r}
# Year 1 pathway probabilities
p_H1_T0 = 1 - p_HU_T0 - p_HD     # probability the patient stays Healthy in year 1
p_U1_T0 = p_HU_T0                # probability the patient becomes Unhealthy in year 1
p_D1_T0 = p_HD                    # probability the patient Dies in year 1

p_T0_1_table <- tribble(
  ~Outcome, ~Probability,
  "Healthy", round(p_H1_T0, 3),
  "Unhealthy", round(p_U1_T0, 3),
  "Death", round(p_D1_T0, 3)
)

kable(p_T0_1_table, caption = "1-Year Pathway Probabilities: T0")
```

```{r}
# Year 2 pathway probabilities 

# Healthy after Year 1
p_HH2_T0 = p_H1_T0  # probability the patient stays Healthy in year 2
p_HU2_T0 = p_U1_T0  # probability the patient becomes Unhealthy in year 2
p_HD2_T0 = p_D1_T0  # probability the patient Dies in year 2

# Unhealthy after Year 1
p_UU2_T0 = 1-p_UD  # probability the patient stays Unhealthy in year 2
p_UD2_T0 = p_UD    # probability the patient Dies in year 2

# Death after Year 1
p_DD2_T0 = p_D1_T0  # probability the patient stays Dead in Year 2
```

```{r}
# Total pathway probabilities (combine all terminal nodes)

# Healthy after Year 1
p_HH_T0_2 = p_HH2_T0 * p_H1_T0  # total probability the patient stays Healthy
p_HU_T0_2 = p_HU2_T0 * p_H1_T0  # total probability the patient becomes Unhealthy
p_HD_T0_2 = p_HD2_T0 * p_H1_T0  # total probability the patietn dies
  
# Unhealthy after Year 1
p_UU_T0_2 = p_UU2_T0 * p_U1_T0  # total probability the patient stays Unhealthy
p_UD_T0_2 = p_UD2_T0 * p_U1_T0  # total probability the patient Dies
  
# Death after Year 1
p_DD_T0_2 = p_DD2_T0  # probability the patient stays Dead

p_T0_2_table <- tribble(
  ~Outcome, ~Probability,
  "Healthy_Healthy", round(p_HH_T0_2, 3),
  "Healthy_Unhealthy", round( p_HU_T0_2, 3),
  "Healthy_Death", round(p_HD_T0_2, 3),
  "Unhealthy_Unhealthy", round(p_UU_T0_2, 3),
  "Unhealthy_Death", round(p_UD_T0_2, 3),
  "Death_Death", round(p_DD_T0_2, 3)
)

kable(p_T0_2_table, caption = "2-Year Pathway Probabilities: T0")
```

```{r}
# Check: probabilities should sum to 1
sum(p_HH_T0_2, p_HU_T0_2, p_HD_T0_2, p_UU_T0_2, p_UD_T0_2, p_DD_T0_2)
```

```{r}
# 2 year pathway costs

# Healthy after Year 1
cost_HH_T0_2 = C_H + C_H  # cost of the patient if they stay Healthy
cost_HU_T0_2 = C_H + C_U  # cost of the patient if they become Unhealthy
cost_HD_T0_2 = C_H + C_D  # cost of the patient if they Die

# Unhealthy after Year 1
cost_UU_T0_2 = C_U + C_U  # cost of the patient if they stay Unhealthy
cost_UD_T0_2 = C_U + C_D  # cost of the patient if they Die

# Death after Year 1
cost_DD_T0_2 = C_D + C_D  # cost of the patient if they stay Dead

cost_T0_2_table <- tribble(
  ~Outcome, ~Cost,
  "Healthy_Healthy", scales::dollar(cost_HH_T0_2, accuracy = 0.01),
  "Healthy_Unhealthy", scales::dollar(cost_HU_T0_2, accuracy = 0.01),
  "Healthy_Death", scales::dollar(cost_HD_T0_2, accuracy = 0.01),
  "Unhealthy_Unhealthy", scales::dollar(cost_UU_T0_2, accuracy = 0.01),
  "Unhealthy_Death", scales::dollar(cost_UD_T0_2, accuracy = 0.01),
  "Death_Death", scales::dollar(cost_DD_T0_2, accuracy = 0.01)
)

kable(cost_T0_2_table, caption = "2-Year Pathway Costs: T0")
```

```{r}
# 2 year pathway utility

# Healthy after Year 1
utility_HH_T0_2 = U_H + U_H  # utility of the patient if they stay Healthy
utility_HU_T0_2 = U_H + U_U  # utility of the patient if they become Unhealthy
utility_HD_T0_2 = U_H + U_D  # utility of the patient if they Die

# Unhealthy after Year 1
utility_UU_T0_2 = U_U + U_U  # utility of the patient if they stay Unhealthy
utility_UD_T0_2 = U_U + U_D  # utility of the patient if they Die

# Death after Year 1
utility_DD_T0_2 = U_D + U_D  # utility of the patient if they stay Dead

utility_T0_2_table <- tribble(
  ~Outcome, ~Utility,
  "Healthy_Healthy", round(utility_HH_T0_2, 3),
  "Healthy_Unhealthy", round(utility_HU_T0_2, 3),
  "Healthy_Death", round(utility_HD_T0_2, 3),
  "Unhealthy_Unhealthy", round(utility_UU_T0_2, 3),
  "Unhealthy_Death", round(utility_UD_T0_2, 3),
  "Death_Death", round(utility_DD_T0_2, 3)
)

kable(utility_T0_2_table, caption = "2-Year Pathway Utilities: T0")
```

## Building the 2 Year Decision Tree: Treatment (T1)

The treatment (T1) has an annual cost of $1000 so C_T1 is 1000. Every patient receiving the treatment must pay the annual cost regardless of the outcome.

```{r}
# Year 1 pathway probabilities
p_H1_T1 <- 1 - p_HU_T1 - p_HD  # probability the patient stays Healthy in year 1
p_U1_T1 <- p_HU_T1             # probability the patient becomes Unhealthy in year 1
p_D1_T1 <- p_HD                # probability the patient Dies in year 1

p_T1_1_table <- tribble(
  ~Outcome, ~Probability,
  "Healthy", round(p_H1_T1, 3),
  "Unhealthy", round(p_U1_T1, 3),
  "Death", round(p_D1_T1, 3)
)

kable(p_T1_1_table, caption = "1-Year Pathway Probabilities: T1")
```

```{r}
# Year 2 pathway probabilities 

# Healthy after Year 1
p_HH2_T1 <- p_H1_T1  # probability the patient stays Healthy in year 2
p_HU2_T1 <- p_U1_T1  # probability the patient becomes Unhealthy in year 2
p_HD2_T1 <- p_D1_T1  # probability the patient Dies in year 2

# Unhealthy after Year 1
p_UU2_T1 <- 1-p_UD  # probability the patient stays Unhealthy in year 2
p_UD2_T1 <- p_UD    # probability the patient Dies in year 2

# Dead after Year 1
p_DD2_T1 <- p_D1_T1  # probability the patient stays Dead in year 2
```

```{r}
# Total pathway probabilities (combine all terminal nodes)

# Healthy after Year 1
p_HH_T1_2 = p_HH2_T1 * p_H1_T1  # total probability the patient stays Healthy
p_HU_T1_2 = p_HU2_T1 * p_H1_T1  # total probability the patient become Unhealthy
p_HD_T1_2 = p_HD2_T1 * p_H1_T1  # total probability the patient Dies

# Unhealthy after Year 1
p_UU_T1_2 = p_UU2_T1 * p_U1_T1  # total probability the patient stays Unhealthy
p_UD_T1_2 = p_UD2_T1 * p_U1_T1  # total probability the patient Dies

# Death after Year 1
p_DD_T1_2 = p_DD2_T1  # total probability the patient stays Dead

p_T1_2_table <- tribble(
  ~Outcome, ~Probability,
  "Healthy_Healthy", round(p_HH_T1_2, 3),
  "Healthy_Unhealthy", round(p_HU_T1_2, 3),
  "Healthy_Death", round(p_HD_T1_2, 3),
  "Unhealthy_Unhealthy", round(p_UU_T1_2, 3),
  "Unhealthy_Death", round(p_UD_T1_2, 3),
  "Death_Death", round(p_DD_T1_2, 3)
)

kable(p_T1_2_table, caption = "2-Year Pathway Probabilities: T1")
```

```{r}
# Check: probabilities should sum to 1
sum(p_HH_T1_2, p_HU_T1_2, p_HD_T1_2, p_UU_T1_2, p_UD_T1_2, p_DD_T1_2)
```

```{r}
# 2 year pathway costs

# Healthy after Year 1
cost_HH_T1_2 = (C_H + C_H) + 2*C_T1  # cost of the patient if they stay Healthy
cost_HU_T1_2 = (C_H + C_U) + 2*C_T1  # cost of the patient if they become Unhealthy
cost_HD_T1_2 = (C_H + C_D) + 2*C_T1  # cost of the patient if they Die

# Unhealthy after Year 1
cost_UU_T1_2 = (C_U + C_U) + 2*C_T1  # cost of the patient if they stay Unhealthy
cost_UD_T1_2 = (C_U + C_D) + 2*C_T1  # cost of the patient if they Die

# Death after Year 1
cost_DD_T1_2 = (C_D + C_D) + C_T1  # cost of the patient if they stay Dead; only add cost for one year because they died in year 1

cost_T1_2_table <- tribble(
  ~Outcome, ~Cost,
  "Healthy_Healthy", scales::dollar(cost_HH_T1_2, accuracy = 0.01),
  "Healthy_Unhealthy", scales::dollar(cost_HU_T1_2, accuracy = 0.01),
  "Healthy_Death", scales::dollar(cost_HD_T1_2, accuracy = 0.01),
  "Unhealthy_Unhealthy", scales::dollar(cost_UU_T1_2, accuracy = 0.01),
  "Unhealthy_Death", scales::dollar(cost_UD_T1_2, accuracy = 0.01),
  "Death_Death", scales::dollar(cost_DD_T1_2, accuracy = 0.01)
)

kable(cost_T1_2_table, caption = "2-Year Pathway Costs: T1")
```

```{r}
# 2 year pathway utility

# Healthy after Year 1
utility_HH_T1_2 = U_H + U_H  # utility of the patient if they stay Healthy
utility_HU_T1_2 = U_H + U_U  # utility of the patient if they become Unhealthy
utility_HD_T1_2 = U_H + U_D  # utility of the patient if they Die
  
# Unhealthy after Year 1
utility_UU_T1_2 = U_U + U_U  # utility of the patient if they stay Unhealthy
utility_UD_T1_2 = U_U + U_D  # utility of the patient if they Die
  
# Death after Year 1
utility_DD_T1_2 = U_D + U_D  # utility of the patient if they stay Dead

utility_T1_2_table <- tribble(
  ~Outcome, ~Utility,
  "Healthy_Healthy", round(utility_HH_T1_2, 3),
  "Healthy_Unhealthy", round(utility_HU_T1_2, 3),
  "Healthy_Death", round(utility_HD_T1_2, 3),
  "Unhealthy_Unhealthy", round(utility_UU_T1_2, 3),
  "Unhealthy_Death", round(utility_UD_T1_2, 3),
  "Death_Death", round(utility_DD_T1_2, 3)
)

kable(utility_T1_2_table, caption = "2-Year Pathway Utilities: T1")
```

### Expected Cost and QALYs Per Strategy

**\*\*Question: What is the expected cost of treatment in each strategy?**

```{r}
# 2 year expected cost (p*cost) under conventional management (T0)
exp_cost_T0_2 <- c(
  HH = p_HH_T0_2 * cost_HH_T0_2,
  HU = p_HU_T0_2 * cost_HU_T0_2,
  HD = p_HD_T0_2 * cost_HD_T0_2,
  UU = p_UU_T0_2 * cost_UU_T0_2,
  UD = p_UD_T0_2 * cost_UD_T0_2,
  DD = p_DD_T0_2 * cost_DD_T0_2
)

exp_cost_T0_2_table <- tribble(
  ~Outcome, ~Expected_Cost,
  "Healthy_Healthy", scales::dollar(exp_cost_T0_2["HH"], accuracy = 1),
  "Healthy_Unhealthy", scales::dollar(exp_cost_T0_2["HU"], accuracy = 1),
  "Healthy_Death", scales::dollar(exp_cost_T0_2["HD"], accuracy = 1),
  "Unhealthy_Unhealthy", scales::dollar(exp_cost_T0_2["UU"], accuracy = 1),
  "Unhealthy_Death", scales::dollar(exp_cost_T0_2["UD"], accuracy = 1),
  "Death_Death", scales::dollar(exp_cost_T0_2["DD"], accuracy = 1)
)

kable(exp_cost_T0_2_table, caption = "2-Year Expected Costs: T0")
```

```{r}
# 2 year expected cost (p*cost) under treatment (T1)
exp_cost_T1_2 <- c(
  HH = p_HH_T1_2 * cost_HH_T1_2,
  HU = p_HU_T1_2 * cost_HU_T1_2,
  HD = p_HD_T1_2 * cost_HD_T1_2,
  UU = p_UU_T1_2 * cost_UU_T1_2,
  UD = p_UD_T1_2 * cost_UD_T1_2,
  DD = p_DD_T1_2 * cost_DD_T1_2
)

exp_cost_T1_2_table <- tribble(
  ~Outcome, ~Expected_Cost,
  "Healthy_Healthy", scales::dollar(exp_cost_T1_2["HH"], accuracy = 1),
  "Healthy_Unhealthy", scales::dollar(exp_cost_T1_2["HU"], accuracy = 1),
  "Healthy_Death", scales::dollar(exp_cost_T1_2["HD"], accuracy = 1),
  "Unhealthy_Unhealthy", scales::dollar(exp_cost_T1_2["UU"], accuracy = 1),
  "Unhealthy_Death", scales::dollar(exp_cost_T1_2["UD"], accuracy = 1),
  "Death_Death", scales::dollar(exp_cost_T1_2["DD"], accuracy = 1)
)

kable(exp_cost_T1_2_table, caption = "2-Year Expected Costs: T1")

```

```{r}
exp_cost_2yr <- tribble(
  ~Strategy, ~Expected_Cost,
  "T0 (Conventional)", scales::dollar(sum(exp_cost_T0_2), accuracy = 0.01),
  "T1 (Treatment)", scales::dollar(sum(exp_cost_T1_2), accuracy = 0.01)
)

kable(exp_cost_2yr, caption = "2-Year: Expected Costs")
```

**\*\*Question: What is the expected QALYs of patients in each strategy?**

```{r}
# 2 year expected QALY (p*utility) under conventional management (T0)
exp_qaly_T0_2  <- c(
  HH = p_HH_T0_2 * utility_HH_T0_2,
  HU = p_HU_T0_2 * utility_HU_T0_2,
  HD = p_HD_T0_2 * utility_HD_T0_2,
  UU = p_UU_T0_2 * utility_UU_T0_2,
  UD = p_UD_T0_2 * utility_UD_T0_2,
  DD = p_DD_T0_2 * utility_DD_T0_2
)

exp_qaly_T0_2_table <- tribble(
  ~Outcome, ~Expected_QALY,
  "Healthy_Healthy", round(exp_qaly_T0_2["HH"], 3),
  "Healthy_Unhealthy", round(exp_qaly_T0_2["HU"], 3),
  "Healthy_Death", round(exp_qaly_T0_2["HD"], 3),
  "Unhealthy_Unhealthy", round(exp_qaly_T0_2["UU"], 3),
  "Unhealthy_Death", round(exp_qaly_T0_2["UD"], 3),
  "Death_Death", round(exp_qaly_T0_2["DD"], 3)
)

kable(exp_qaly_T0_2_table, caption = "2-Year Expected QALY: T0")
```

```{r}
# 2 year expected QALY (p*utility) under treatment (T1)
exp_qaly_T1_2 <- c(
  HH = p_HH_T1_2 * utility_HH_T1_2,
  HU = p_HU_T1_2 * utility_HU_T1_2,
  HD = p_HD_T1_2 * utility_HD_T1_2,
  UU = p_UU_T1_2 * utility_UU_T1_2,
  UD = p_UD_T1_2 * utility_UD_T1_2,
  DD = p_DD_T1_2 * utility_DD_T1_2
)

exp_qaly_T1_2_table <- tribble(
  ~Outcome, ~Expected_QALY,
  "Healthy_Healthy", round(exp_qaly_T1_2["HH"], 3),
  "Healthy_Unhealthy", round(exp_qaly_T1_2["HU"], 3),
  "Healthy_Death", round(exp_qaly_T1_2["HD"], 3),
  "Unhealthy_Unhealthy", round(exp_qaly_T1_2["UU"], 3),
  "Unhealthy_Death", round(exp_qaly_T1_2["UD"], 3),
  "Death_Death", round(exp_qaly_T1_2["DD"], 3)
)

kable(exp_qaly_T1_2_table, caption = "2-Year Expected QALY: T1")
```

```{r}
exp_qaly_2yr <- tribble(
  ~Strategy, ~Expected_QALY,
  "T0 (Conventional)", round(sum(exp_qaly_T0_2), 3),
  "T1 (Treatment)", round(sum(exp_qaly_T1_2), 3)
)

kable(exp_qaly_2yr, caption = "2-Year: Expected QALY")
```

### 2 Year ICER

**\*\*Question: What is the 1-year-cost-effectiveness of the new treatment?**

```{r}
incremental_cost_2 <- sum(exp_cost_T1_2) - sum(exp_cost_T0_2)
incremental_qaly_2 <- sum(exp_qaly_T1_2) - sum(exp_qaly_T0_2)
icer_2 <- incremental_cost_2/incremental_qaly_2

icer_2yr <- tribble(
  ~Incremental_Cost, ~Incremental_QALY, ~ICER,
  scales::dollar(incremental_cost_2, accuracy = 1), round(incremental_qaly_2, 3), scales::dollar(icer_2, accuracy = 1)
)

kable(icer_2yr, caption = "2-Year: ICER")
```
