#Load all required libraries
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(GGally)
library(tseries)
library(forecast)
library(xgboost)

#Import dataset
data <- read_csv("Load the data file")

#Filter data focusing on California
CA_data <- data |> filter(state == "CA")

#Check data structure
str(CA_data)

#Select the relevant columns
CA_data <- CA_data |> select(
  collection_week, total_beds_7_day_avg, total_icu_beds_7_day_avg, 
  inpatient_beds_used_7_day_avg,icu_beds_used_7_day_avg,total_adult_patients_hospitalized_confirmed_covid_7_day_avg,
  total_patients_hospitalized_confirmed_influenza_7_day_avg)
summary(CA_data)    #Summarize dataset

#Renaming the columns
CA_data <- CA_data |> rename(
  date = collection_week,
  total_beds = total_beds_7_day_avg,
  total_icu_beds = total_icu_beds_7_day_avg,
  inpatient_beds_used = inpatient_beds_used_7_day_avg,
  icu_beds_used = icu_beds_used_7_day_avg,
  covid_patients = total_adult_patients_hospitalized_confirmed_covid_7_day_avg,
  influenza_patients = total_patients_hospitalized_confirmed_influenza_7_day_avg
)

#Replace invalid placeholder values (-999999.0) with NA
CA_data <- CA_data |> mutate(across(where(is.numeric), ~ifelse(. == -999999.0, NA, .)))
colSums(is.na(CA_data))      #Check for number of missing values

#Calculate the proportion of missing values for each column
missing_proportions <- colSums(is.na(CA_data)) / nrow(CA_data)
missing_proportions

#Fill the missing values with forward and backward filling
CA_data <- CA_data |> arrange(date) |> fill(everything(), .direction = "downup")
colSums(is.na(CA_data))     #Check for missing values again
summary(CA_data)

#Convert the 'date' column to date format
CA_data$date <- as.Date(CA_data$date)

#Aggregate data by date
CA_data <- CA_data |> group_by(date) |> summarise(across(everything(), ~ mean(., na.rm = TRUE)))

#Pivot data to long format for plotting bed usage trends
CA_data_long <- CA_data |> select(date, total_beds, total_icu_beds, inpatient_beds_used, icu_beds_used) |>
  pivot_longer(cols = c(total_beds, total_icu_beds, inpatient_beds_used, icu_beds_used),
               names_to = "bed_type", values_to = "bed_usage")

#Plot the trends in bed usage over time
ggplot(CA_data_long, aes(x=date, y=bed_usage, color=bed_type)) +
  geom_line() + labs(title = "Trend of Bed Usage Over Time",
                     x = "Date",
                     y= "Bed usage",
                     color = "Bed Type") +
  theme_minimal() + scale_x_date(labels = scales::date_format("%Y-%m-%d"), breaks = "6 months")

#Reshape the data to long format for COVID and Influenza hospital trends
CA_data_long_covid_flu <- CA_data |> select(date, covid_patients, influenza_patients) |>
  pivot_longer(cols = c(covid_patients, influenza_patients),
               names_to = "condition", values_to = "hospitalized_patients")

#Plot trends in COVID and Influenza hospitalizations over time
ggplot(CA_data_long_covid_flu, aes(x=date, y=hospitalized_patients, color=condition)) +
         geom_line() + labs(title = "Trend of COVID and Influenza Hospitalization Over time",
                            x = "Date",
                            y = "Number of Hopsitalized patients",
                            color = "Condition") +
         theme_minimal() + scale_x_date(labels = scales::date_format("%Y-%m-%d"), breaks = "6 months")

#Correlation among variables
cor_data <- CA_data |> select(total_beds, total_icu_beds, inpatient_beds_used, icu_beds_used, covid_patients, influenza_patients)
ggpairs(cor_data, upper = list(continuous = "points"),
                  lower = list(continuous = "smooth"),
                  diag = list(continuous = "densityDiag"),
        title = "Scatter Plot Matrix of Bed Usage and Hospitalizations")

#Trend and seasonal form decomposed time series
total_beds_ts <- ts(CA_data$total_beds, frequency = 52, start = c(2020,1))
stl_decomp <- stl(total_beds_ts, s.window = "periodic")
plot(stl_decomp)
title("Decomposition of Total Bed Usage Time Series")

inpatient_beds_ts <- ts(CA_data$inpatient_beds_used, frequency = 52, start = c(2020,1))
stl_decomp <- stl(inpatient_beds_ts, s.window = "periodic")
plot(stl_decomp)
title("Decomposition of Inpatient Bed Usage Time Series")

icu_beds_ts <- ts(CA_data$icu_beds_used, frequency = 52, start = c(2020,1))
stl_decomp <- stl(icu_beds_ts, s.window = "periodic")
plot(stl_decomp)
title("Decomposition of ICU Bed Usage Time Series")

#Check for stationary using Augmented Dickey-Fuller test
adf_total_beds <- adf.test(CA_data$total_beds, alternative = "stationary")
adf_inpatient_beds <- adf.test(CA_data$inpatient_beds_used, alternative = "stationary")
adf_icu_beds <- adf.test(CA_data$icu_beds_used, alternative = "stationary")
adf_covid_patients <- adf.test(CA_data$covid_patients, alternative = "stationary")
adf_influenza_patients <- adf.test(CA_data$influenza_patients, alternative = "stationary")

#ACF and PACF plot to determine 'p' and 'q' values for Total Beds
acf(CA_data$total_beds, main = "ACF for Total Beds")
pacf(CA_data$total_beds, main = "PACF for Total Beds")
diff_total_beds <- diff(CA_data$total_beds, lag = 1)
acf(diff_total_beds, main = "ACF for Differenced Total Beds")
pacf(diff_total_beds, main = "PACF for Differenced Total Beds")

#ACF and PACF plot to determine 'p' and 'q' values for Inpatient Beds Used
acf(CA_data$inpatient_beds_used, main = "ACF for Inpatient Beds Used")
pacf(CA_data$inpatient_beds_used, main = "PACF for Inpatient Beds Used")
diff_inpatient_beds <- diff(CA_data$inpatient_beds_used, lag = 1)
acf(diff_inpatient_beds , main = "ACF for Differenced Inpatient Beds")
pacf(diff_inpatient_beds , main = "PACF for Differenced Inpatient Beds")

#ACF and PACF plot to determine 'p' and 'q' values for ICU Beds Used
acf(CA_data$icu_beds_used, main = "ACF for ICU Beds Used")
pacf(CA_data$icu_beds_used, main = "PACF for ICU Beds Used")
diff_icu_beds <- diff(CA_data$icu_beds_used, lag = 1)
acf(diff_icu_beds , main = "ACF for Differenced ICU Beds")
pacf(diff_icu_beds , main = "PACF for Differenced ICU Beds")

#ARIMA model to forecast hospital resources
arima_total_beds <- arima(CA_data$total_beds, order = c(2, 1, 4))
arima_inpatient_beds <- arima(CA_data$inpatient_beds_used, order = c(4, 1, 0))
arima_icu_beds <- arima(CA_data$icu_beds_used, order = c(1, 1, 0))

#Forecast for 24 weeks into the future
forecast_total_beds <- forecast(arima_total_beds, h = 24)
forecast_icu_beds <- forecast(arima_icu_beds, h = 24)
forecast_inpatient_beds <- forecast(arima_inpatient_beds, h = 24)

#Create forecast dates
forecast_date <- seq.Date(from = max(CA_data$date), by = "week", length.out = 24)

#Combine the forecast into a single data frame
forecast_df <- data.frame(
  date = forecast_date,
  total_beds = as.numeric(forecast_total_beds$mean),
  icu_beds_used = as.numeric(forecast_icu_beds$mean),
  inpatient_beds_used = as.numeric(forecast_inpatient_beds$mean)
)

forecast_df <- forecast_df |> mutate(Type = "Forecasted")

#Prepare the historical data
historical_data <- CA_data |> select(date, total_beds, inpatient_beds_used, icu_beds_used) |>
  mutate(Type = "Historical")

#Plot historical and forecasted data
combined_data <- bind_rows(historical_data, forecast_df)
combined_data_long <- combined_data |> 
  pivot_longer(cols = c(total_beds, inpatient_beds_used, icu_beds_used),
               names_to = "Resource", values_to = "Usage")
ggplot(combined_data_long, aes(x=date, y=Usage, color=Resource, linetype = Type)) +
  geom_line(linewidth = 1) +
  labs(title = "Historical and Forecasted Hospital Resource Usage",
       x = "Date",
       y = "Resource Usage (Count)",
       color = "Resource",
       linetype = "Type") +
  theme_minimal() + scale_x_date(labels = scales::date_format("%Y-%m-%d"), breaks = "6 months")

#Accuracy metrics from the ARIMA model
summary(arima_total_beds)
summary(arima_inpatient_beds)
summary(arima_icu_beds)

#Residual plots
residuals_total_beds <- residuals(arima_total_beds)
residuals_inpatient_beds <- residuals(arima_inpatient_beds)
residuals_icu_beds <- residuals(arima_icu_beds)
par(mfrow = c(3,1))
plot(residuals_total_beds, main = "Residuals for Total Beds", type = "o")
plot(residuals_inpatient_beds, main = "Residuals for Inpatient Beds", type = "o")
plot(residuals_icu_beds, main = "Residuals for ICU Beds", type = "o")

#Ljung-Box test for autocorrelation in residuals
Box.test(residuals_total_beds, lag = 20, type = "Ljung-Box")
Box.test(residuals_inpatient_beds, lag = 20, type = "Ljung-Box")
Box.test(residuals_icu_beds, lag = 20, type = "Ljung-Box")

#Train and evaluate XB Boost model to analyze how COVID and Influenza influence in bed usage
dtrain <- xgb.DMatrix(data = as.matrix(CA_data[, c("covid_patients", "influenza_patients")]),
                      label = CA_data$inpatient_beds_used)
param <- list(objective = "reg:squarederror", max_depth = 6, eta = 0.3)
xgb_model <- xgb.train(param, dtrain, nrounds = 100)

#Make predictions on the training data
predictions <- predict(xgb_model, dtrain)

#Calculate the accuracy metrics for XGBoost model
accuracy_metrics <- function(actual, predicted) {
  mae <- mean(abs(actual - predicted), na.rm = TRUE)  # Mean Absolute Error
  mse <- mean((actual - predicted)^2, na.rm = TRUE)  # Mean Squared Error
  rmse <- sqrt(mse)  # Root Mean Squared Error
  mape <- mean(abs((actual - predicted) / actual), na.rm = TRUE) * 100  # Mean Absolute Percentage Error
  return(data.frame(MAE = mae, MSE = mse, RMSE = rmse, MAPE = mape))
}
xgboost_metrics <- accuracy_metrics(CA_data$inpatient_beds_used, predictions)
print(xgboost_metrics)

#Plot Actual vs Predicted values
plot(CA_data$inpatient_beds_used, predictions, 
      xlab = "Actual Values", 
      ylab = "Predicted Values", 
      main = "Impact of COVID-19 and Influenza on Inpatient Bed Usage: Actual vs Predicted", 
      col = "blue", pch = 16)
abline(a = 0, b = 1, col = "red", lwd = 2, lty = 2)
