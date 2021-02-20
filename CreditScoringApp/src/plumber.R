library(plumber)
library(jsonlite)

# SET WORKING DIRECTORY SO THAT THE myCreditScoringModel.rds FILE CAN BE FOUND IN THE NEXT LINE

model <- readRDS('./myCreditScoringModel.rds')

#* @apiTitle Breast Cancer Diagnosis

#* @apiDescription Breast cancer diagnosis based on three relevant parameters. The prediction returns both predicted class and its class probability.

#* @param duration_in_months ...enter the value as a number
#* @param age_in_years ...enter the value as a number
#* @get /predict
function(duration_in_months,age_in_years) {
  df <- data.frame("duration_in_month_woe" = as.numeric(duration_in_months), 
                   "age_in_years_woe" = as.numeric(age_in_years))
  
  prediction <- data.frame("Prediction" = predict(model, newdata = df, type = "response"), 
                           "Probability" = max(as.numeric(unlist(predict(model, newdata = df, type = "response")))))
  
  response <- toJSON(prediction, dataframe = "columns")
  response
}

