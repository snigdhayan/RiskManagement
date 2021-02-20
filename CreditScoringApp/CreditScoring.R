
# Credit Scoring via Logistic Regression
library(scorecard)

# Load and preprocess data
data("germancredit")

# Filter variables based on specified conditions, such as information value, missing rate, identical value rate
filtered_data = var_filter(germancredit, y="creditability")

# filtered_data = data.frame("duration.in.month" = filtered_data$duration.in.month, 
#                            "age.in.years" = filtered_data$age.in.years,
#                            "creditability" = filtered_data$creditability)

# filtered_data$creditability <- as.factor(filtered_data$creditability)

# Split data into train and test dataset
train_test_data = split_df(filtered_data, y="creditability", ratios = c(0.6, 0.4), seed = 30)
label_list = lapply(train_test_data, function(x) x$creditability)

# Weight of Evidence (woe) binning
bins = woebin(filtered_data, y="creditability")

# Plot the bins for all variables
# woebin_plot(bins) 

# Adjust binning interactively
# break_adjustments = woebin_adj(filtered_data, "creditability", bins) 

## Adjust binning manually
break_adjustments = list(age.in.years=c(26, 35, 40),
                         other.debtors.or.guarantors=c("none", "co-applicant%,%guarantor"))
bin_adjustments = woebin(filtered_data, y="creditability", breaks_list=break_adjustments)

# Convert train and test datasets into woe values
train_test_woe_data = lapply(train_test_data, function(x) woebin_ply(x, bin_adjustments))

# Apply generalized linear models (glm)
m1 = glm(creditability ~ ., family = binomial(), data = train_test_woe_data$train)
# vif(m1, merge_coef = TRUE) # summary(m1)

# Select a formula-based model by AIC (or by LASSO for large dataset)
m_step = step(m1, direction="both", trace = FALSE)
m2 = eval(m_step$call)
# vif(m2, merge_coef = TRUE) # summary(m2)

# setwd('~/MyProgramsWindows/Repositories/RiskManagement/CreditScoringApp/')
# saveRDS(m2, file = './CreditScoringShinyApp/myCreditScoringModel.rds')


# Predicted probability
pred_list = lapply(train_test_woe_data, function(x) predict(m2, x, type='response'))

# Adjusting for oversampling (support.sas.com/kb/22/601.html)
# card_prob_adj = scorecard2(bin_adjustments, dt=train_test_data$train, y='creditability', 
#                x=sub('_woe$','',names(coef(m2))[-1]), badprob_pop=0.03, return_prob=TRUE)

# Performance metrics - e.g., Kolmogorov-Smirnov statistic (KS), Area Under the Curve (AUC), etc.
perf = perf_eva(pred = pred_list, label = label_list)
# perf_adj = perf_eva(pred = card_prob_adj$prob, label = label_list$train)

# Scorecard
card = scorecard(bin_adjustments, m2)

# Credit Score
score_list = lapply(train_test_data, function(x) scorecard_ply(x, card))

# Population Stability Index (PSI)
perf_psi(score = score_list, label = label_list)