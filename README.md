# RiskManagement

In the directory `CreditScoringApp` there is a credit scoring model based on `logistic regression`. A simplified version of this model that uses only the parameters `age` and `duration of checking account` can be tried out via the following options: 

1. Shiny app (via Shiny UI)
2. Plumber app (via Swagger UI)

In the directory `RiskCalculations` there are `R` scripts to calculate the following:

2. `Value at risk` and `expected shortfall` (calculations under various assumptions)
3. `Volatility clustering` (via GARCH model)
3. `Trend in annual stock returns`