# Predicting-Healthcare-Demand-Modeling-Trends-and-Surges
Predicting resource allocation is crucial for various sectors, particularly healthcare. Effective distribution and utilization of resources become especially vital during national and international epidemic outbreaks, as evidenced by the COVID-19 pandemic. Analyzing historical healthcare data enables better preparedness and management of similar future outbreaks by informing how hospital resources—such as ICU beds, ventilators, and medical personnel—should be allocated during emergencies. This research utilizes multi-sourced datasets to examine healthcare dynamics in the state of California, employing predictive models to generate insights that support healthcare providers in making informed resource allocation decisions. By leveraging advanced analytics, this study aims to enhance the responsiveness and efficiency of healthcare systems, ensuring optimal inpatient and ICU bed utilization during periods of fluctuating demand.


Keywords: Healthcare, Resource Utilization, Time series forecasting, Predictive Analysis, COVID-19, Influenza 

### Key Findings
- **ARIMA Model**: Showed promising forecasting results for inpatient and ICU bed usage with minimal autocorrelation in residuals (Ljung-Box p-value > 0.05).
- **XGBoost Model**: Outperformed the ARIMA model in terms of prediction accuracy, offering robust forecasts with low error margins.
- **Ljung-Box Test**: Autocorrelation in residuals was not significant for any model, validating the models' assumptions.

### Performance Metrics

| Model             | Ljung-Box p-value | X-squared  | RMSE (Root Mean Squared Error) | Accuracy (%) |
|------------------ |-------------------|------------|--------------------------------|--------------|
| **Total Beds**    | 0.104             | 28.234     | 0.95                           | 97.3         |
| **Inpatient Beds**| 0.1316            | 27.131     | 1.05                           | 94.7         |
| **ICU Beds**      | 0.0843            | 29.181     | 0.85                           | 98.1         |

The results indicate that our models are effective in predicting hospital bed demands with high accuracy and minimal autocorrelation, which suggests that the models are well-calibrated for forecasting future healthcare resource needs.

For a more detailed analysis, see the [Results Analysis File](results.md).
