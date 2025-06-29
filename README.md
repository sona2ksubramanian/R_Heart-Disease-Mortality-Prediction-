# R_Heart-Disease-Mortality-Prediction-

Project Purpose: 
Heart disease remains the leading cause of death globally, accounting for over 32% of all annual deaths. Despite advancements in medical treatments, mortality rates remain high, particularly among vulnerable populations. This project was developed to address that gap by building a comprehensive, data-driven model that examines how lifestyle, demographic, geographic, and socioeconomic factors influence heart disease mortality at the county level across the United States.

Analytical Process in R:
- Collected and merged multiple publicly available datasets containing county-level health, demographic, and socioeconomic indicators to form a comprehensive analytic dataset.
- Performed data cleaning and preprocessing, including removal of missing values, normalization, and feature engineering of categorical variables.
- Conducted exploratory data analysis (EDA) using scatterplots, boxplots, and correlation heatmaps to identify patterns, trends, and potential outliers.
- Evaluated multicollinearity among variables using variance inflation factors (VIF), and removed highly correlated predictors to improve model stability.
- Split the cleaned dataset into training, validation, and testing to support model development, tuning, and unbiased performance evaluation.
- Built and evaluated six predictive models — Linear Regression, Ridge, Lasso, Elastic Net, Decision Tree, Random Forest, and Support Vector Machine (SVM) — using cross-validation and hyperparameter tuning.
- Compared models using performance metrics such as R², Root Mean Squared Error (RMSE), and Mean Absolute Error (MAE), training and test data. 
- The model with the highest performance was selected after comparative evaluation, and its predictive accuracy was assessed on the validation dataset.




