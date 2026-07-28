# Healthy Unhealthy Decision Tree (1-Year Time Horizon) Working

## Exercise 1: Decision Tree - 1 Year Time Horizon
  
Consider a latent disease whereby there are three possible health states: “Healthy”, “Unhealthy” and “Death.” A new treatment has been discovered that has been found to reduce the probability of disease carriers becoming unhealthy (though people must continue treatment even after they become unhealthy). You have been asked to conduct an appraisal of the costs and benefits of the treatment.

A randomised study found that 13 out of 1456 patients using the new treatment became unhealthy each year, compared to 26 out of 1464 in the control group (which is conventional management). The risk of mortality was the same for each arm (8 deaths out of 1825 healthy patients per year and 9 deaths out of 1095 unhealthy patients). Once patients become unhealthy, they cannot become healthy again but all patients start off as healthy carriers.

The cost of the new treatment is $1000 per year and the cost of treating unhealthy patients is $6000 (SE $50) per year. There is no cost associated with healthy patients or death. 

The health utility associated with healthy patients is 0.9 (SE 0.02). Unhealthy patients have a health utility of 0.4 (SE 0.04). 

1.1. Use the information above to create a decision tree to represent the 1st year of the situation described above.

1.2. Using the decision tree created, and the cost and probability data, populate the model to calculate the following: 
  
 - What is the expected cost of treatment in each strategy - the new treatment ('T1') versus conventional management ('T0')?
 - What is the expected QALYs of patients in each strategy?
 - What is the 1 year cost effectiveness of the new treatment?

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
p_HU_T0 <-   # probability of becoming Unhealthy when Healthy under conventional management (T0)
p_HU_T1 <-   # probability of becoming Unhealthy when Healthy under treatment (T1) 
p_HD    <-   # probability of Dying when Healthy

# Costs
C_H    <-     # annual cost of being Healthy (excluding treatment cost)
C_U    <-     # annual cost of being Unhealthy (excluding treatment cost)
C_D    <-     # annual cost of being Dead
C_T0   <-     # annual cost of conventional management (T0)
C_T1   <-     # annual cost of treatment (T1)

# Utilities
U_H <-     # annual utility of being Healthy
U_U <-     # annual utility of being Unhealthy
U_D <-     # annual utility of being Dead
```

## Building the 1 Year Decision Tree: Conventional Management (T0)

### Conventional management (T0) has no initial cost so C_T0 is 0. Every patient enters the model in a Healthy state. From this root node patients may remain Healthy, become Unhealthy, or Die in the first-year.

```{r}
# pathway probabilities 
p_H1_T0 =            # probability the patient stays Healthy
p_U1_T0 =            # probability the patient becomes Unhealthy
p_D1_T0 =            # probability the patient Dies

p_T0_1_table <- tribble(
  ~Outcome, ~Probability,
  "Healthy", ,
  "Unhealthy", ,
  "Death", 
)

kable(p_T0_1_table, caption = "1-Year Pathway Probabilities: T0")
```

```{r}
# Check: probabilities should sum to 1
sum( , , )
```


```{r}
# pathway costs
cost_H1_T0 =     # cost of the patient if they stay Healthy
cost_U1_T0 =     # cost of the patient if they become Unhealthy
cost_D1_T0 =     # cost of the patient if they Die

cost_T0_1_table <- tribble(
  ~Outcome, ~Cost,
  "Healthy", ,
  "Unhealthy", ,
  "Death", 
)

kable(cost_T0_1_table, caption = "1-Year Pathway Costs: T0")
```

```{r}
# pathway utility
utility_H1_T0 =     # utility of the patient if they stay Healthy
utility_U1_T0 =     # utility of the patient if they become Unhealthy
utility_D1_T0 =     # utility of the patient if they become unhealthy


utility_T0_1_table <- tribble(
  ~Outcome, ~Utility,
  "Healthy", ,
  "Unhealthy", ,
  "Death", 
)

kable(utility_T0_1_table, caption = "1-Year Pathway Utilities: T0")
```

## Building the 1 Year Decision Tree: Treatment (T1)

### The treatment (T1) has an annual cost of 1000 so C_T1 is 1000. Every patient receiving the treatment must pay the annual cost regardless of the outcome.

```{r}
# pathway probabilities 
p_H1_T1 =            # probability the patient stays Healthy
p_U1_T1 =            # probability the patient becomes Unhealthy
p_D1_T1 =            # probability the patient Dies

p_T1_1_table <- tribble(
  ~Outcome, ~Probability,
  "Healthy", ,
  "Unhealthy", ,
  "Death", 
)

kable(p_T1_1_table, caption = "1-Year Pathway Probabilities: T1")
```

```{r}
# Check: probabilities should sum to 1
sum( , , )
```

```{r}
# pathway costs
cost_H1_T1 =     # cost of the patient if they stay Healthy
cost_U1_T1 =     # cost of the patient if they become Unhealthy
cost_D1_T1 =     # cost of the patient if they Die

cost_T1_1_table <- tribble(
  ~Outcome, ~Cost,
  "Healthy", ,
  "Unhealthy", ,
  "Death", 
)

kable(cost_T1_1_table, caption = "1-Year Pathway Costs: T1")
```

```{r}
# pathway utility
utility_H1_T1 =     # utility of the patient if they stay Healthy
utility_U1_T1 =     # utility of the patient if they become Unhealthy
utility_D1_T1 =     # utility of the patient if they become unhealthy

utility_T1_1_table <- tribble(
  ~Outcome, ~Utility,
  "Healthy", ,
  "Unhealthy", ,
  "Death", 
)

kable(utility_T1_1_table, caption = "1-Year Pathway Utility: T1")
```

## Expected Cost and QALYs Per Strategy

**\*\*Question: What is the expected cost of treatment in each strategy?**

```{r}
# expected cost (p*cost) under conventional management (T0)
exp_cost_T0_1 <- c(
  Healthy = ,
  Unhealthy = ,
  Death = 
)

exp_cost_T0_1_table <- tribble(
  ~Outcome,    ~Expected_Cost,
  "Healthy",    exp_cost_T0_1[""],
  "Unhealthy",  exp_cost_T0_1[""],
  "Death",      exp_cost_T0_1[""],
)

kable(exp_cost_T0_1_table, caption = "1-Year Expected Costs: T0")
```

```{r}
# expected cost (p*cost) under treatment (T1)
exp_cost_T1_1 <- c(
  Healthy = ,
  Unhealthy = ,
  Death = 
)
  
exp_cost_T1_1_table <- tribble(
  ~Outcome,    ~Expected_Cost,
  "Healthy",    exp_cost_T1_1[""],
  "Unhealthy",  exp_cost_T1_1[""],
  "Death",      exp_cost_T1_1[""],
)

kable(exp_cost_T1_1_table, caption = "1-Year Expected Costs: T1")
```
```{r}
exp_cost_1yr <- tribble(
  ~Strategy, ~Expected_Cost,
  "T0 (Conventional)", sum(),
  "T1 (Treatment)", sum()
)

kable(exp_cost_1yr, caption = "1-Year Expected Costs")
```

**\*\*Question: What is the expected QALYs of patients in each strategy?**

```{r}
# expected utility (p*utility) under conventional management (T0)
exp_qaly_T0_1 <- c(
  Healthy = ,
  Unhealthy = ,
  Death = 
)

exp_qaly_T0_1_table <- tribble(
  ~Outcome,    ~Expected_QALY,
  "Healthy",    exp_qaly_T0_1[""],
  "Unhealthy",  exp_qaly_T0_1[""],
  "Death",      exp_qaly_T0_1[""],
)

kable(exp_qaly_T0_1_table, caption = "1-Year Expected QALY: T0")
```

```{r}
# expected QALY (p*utility) under treatment (T1)
exp_qaly_T1_1 <- c(
  Healthy = ,
  Unhealthy = ,
  Death = 
)

exp_qaly_T1_1_table <- tribble(
  ~Outcome,    ~Expected_QALY,
  "Healthy",    exp_qaly_T1_1[""],
  "Unhealthy",  exp_qaly_T1_1[""],
  "Death",      exp_qaly_T1_1[""],
)

kable(exp_qaly_T1_1_table, caption = "1-Year Expected QALY: T1")
```

```{r}
exp_qaly_1yr <- tribble(
  ~Strategy, ~Expected_QALY,
  "T0 (Conventional)", sum(),
  "T1 (Treatment)", sum()
)

kable(exp_qaly_1yr, caption = "1-Year Expected QALY")
```

## 1 Year ICER

**\*\*Question: What is the 1-year-cost-effectiveness of the new treatment?**

```{r}
incremental_cost_1 <- sum() - sum()
incremental_qaly_1 <- sum() - sum()
icer_1 <- 

icer_1yr <- tribble(
  ~Incremental_Cost, ~Incremental_QALY, ~ICER,
  , , 
)

kable(icer_1yr, caption = "1-Year ICER")
```
