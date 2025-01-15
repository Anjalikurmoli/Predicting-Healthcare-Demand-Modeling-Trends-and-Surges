# Detailed Results Analysis

## ARIMA Model
- **Model Summary**: We used the ARIMA model to forecast the total number of hospital beds, inpatient beds, and ICU beds over time. The residuals of the model were checked for autocorrelation using the Ljung-Box test, with p-values above 0.05, indicating no significant autocorrelation.
- **Model Performance**: The ARIMA model produced forecasts with an acceptable level of accuracy and minimal residual correlation, which indicates that the model was effective in capturing the underlying data patterns.

### Ljung-Box Test Results
The Ljung-Box test results showed no significant autocorrelation in the residuals of the models. Below are the results for each model:

| Model            | Ljung-Box p-value | X-squared |
|------------------|-------------------|-----------|
| **Total Beds**   | 0.104             | 28.234    |
| **Inpatient Beds**| 0.1316            | 27.131    |
| **ICU Beds**     | 0.0843            | 29.181    |

Since the p-values are greater than 0.05, we conclude that the residuals are independent, confirming the validity of the ARIMA models.

## XGBoost Model
- **Model Summary**: The XGBoost model, which uses a gradient-boosting algorithm, was trained on the same dataset. This model showed better prediction accuracy than the ARIMA model, particularly for ICU beds.
- **Feature Importance**: Key features influencing bed utilization predictions included COVID-19 case trends, historical bed occupancy rates, and seasonal demand fluctuations.

### Model Comparison
| Model            | RMSE | Accuracy (%) |
|------------------|------|--------------|
| **ARIMA**        | 0.95 | 97.3         |
| **XGBoost**      | 0.85 | 98.1         |

As we can see, XGBoost outperformed ARIMA in terms of both RMSE and accuracy, which makes it a more suitable model for this dataset.

## Conclusion
The ARIMA and XGBoost models provide valuable insights into hospital resource forecasting. Both models show effective predictive power with low autocorrelation in residuals. However, the XGBoost model is preferred for its superior performance in accuracy.

For further implementation or replication of the analysis, refer to the [R Script](r_script.R) for the full implementation of the models.
