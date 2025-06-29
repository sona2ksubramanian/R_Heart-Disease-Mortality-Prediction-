# imported libraries
library(dplyr)
library(ggplot2)
library(car)
library(dplyr)
library(tidyverse)
library(glmnet)
library(tidymodels)
library(randomForest)
library(xgboost)
library(parallel)
library(doParallel)
library(pROC)
library(e1071)
library(parsnip)
library(yardstick)
library(caret)


#first file with mortality rates 
mydf <- read.csv("Heart_disease_CDC_data.csv")
county_df <- subset(mydf, GeographicLevel == "County")
state_df <- subset(mydf, GeographicLevel == "State")

county_sub_df <- county_df[, c("LocationAbbr", "LocationDesc", "GeographicLevel", 
                               "Data_Value", "Stratification1", "Stratification2", 
                               "Y_lat", "X_lon")]
colnames(county_sub_df) <- c("LocationAbbr", "LocationDesc", "GeographicLevel", "hd_mort", "gender", "race", "Y_lat", "X_lon")
county_df_overall <- subset(county_sub_df, race == "Overall" & gender == "Overall")


#merging second file with food dessert factors 
fd_df <- read.csv("food_access_research_atlas.csv")
fd_subset <- fd_df[, c("State", "County", "Urban", "LowIncomeTracts", "PovertyRate"
                       , "LILATracts_1And10", "HUNVFlag", "TractWhite", 
                       "TractBlack", "TractAsian", "TractHispanic")]

#renaming states to merge since some counties have same names
state_names <- c(
  AL = "Alabama", AK = "Alaska", AZ = "Arizona", AR = "Arkansas",
  CA = "California", CO = "Colorado", CT = "Connecticut", 
  DE = "Delaware", FL = "Florida", GA = "Georgia", 
  HI = "Hawaii", ID = "Idaho", IL = "Illinois", 
  IN = "Indiana", IA = "Iowa", KS = "Kansas", 
  KY = "Kentucky", LA = "Louisiana", ME = "Maine", 
  MD = "Maryland", MA = "Massachusetts", MI = "Michigan", 
  MN = "Minnesota", MS = "Mississippi", MO = "Missouri", 
  MT = "Montana", NE = "Nebraska", NV = "Nevada", 
  NH = "New Hampshire", NJ = "New Jersey", NM = "New Mexico", 
  NY = "New York", NC = "North Carolina", ND = "North Dakota", 
  OH = "Ohio", OK = "Oklahoma", OR = "Oregon", 
  PA = "Pennsylvania", RI = "Rhode Island", SC = "South Carolina", 
  SD = "South Dakota", TN = "Tennessee", TX = "Texas", 
  UT = "Utah", VT = "Vermont", VA = "Virginia", 
  WA = "Washington", WV = "West Virginia", WI = "Wisconsin", 
  WY = "Wyoming"
)

#making county names same 
county_df_overall$StateFull <- state_names[county_df_overall$LocationAbbr]
county_df_overall

county_df_overall$LocationDesc <- trimws(gsub(" County$", "", 
                                              county_df_overall$LocationDesc))
county_df_overall$LocationDesc <- trimws(gsub(" Parish$", "", 
                                              county_df_overall$LocationDesc))
county_df_overall$LocationDesc <- trimws(gsub(" City$", "", 
                                              county_df_overall$LocationDesc))
county_df_overall$LocationDesc <- trimws(gsub(" Municipio$", "", 
                                              county_df_overall$LocationDesc))
fd_subset$County <- gsub(" City$", "", fd_subset$County) 
fd_subset$County <- gsub(" County$", "", fd_subset$County) 
fd_subset$County <- gsub(" Municipality$", "", fd_subset$County)

fd_subset2 <- fd_subset %>%
  group_by(State, County) %>%
  summarise(
    LowIncomeTracts = ifelse(mean(LowIncomeTracts, na.rm = TRUE) > 0.5, 1, 0),
    PovertyRate = mean(PovertyRate, na.rm = TRUE), 
    LILATracts_1And10 = ifelse(mean(LILATracts_1And10, na.rm = TRUE) > 0.5, 1, 0),
    HUNVFlag = ifelse(mean(HUNVFlag, na.rm = TRUE) > 0.5, 1, 0),
    TractWhite = mean(TractWhite, na.rm = TRUE),
    TractBlack = mean(TractBlack, na.rm = TRUE),
    TractAsian = mean(TractAsian, na.rm = TRUE),
    TractHispanic = mean(TractHispanic, na.rm = TRUE),
    .groups = "drop"
  )


#merging the two files
merged_df <- merge(county_df_overall, fd_subset2, by.x = c("LocationDesc", "StateFull"), 
                   by.y = c("County", "State"), all.x = TRUE)

#reading in third file 
county_health_df <- read.csv("County_health_data.csv")
county_health_df

#merging the third file
county_health_df
final_merged_df <- merge(merged_df, county_health_df, by.x = c("LocationDesc", "StateFull"), 
                         by.y = c("County", "State"), all.x = TRUE)

# removing any values not in the US such as puerto Rico
final_merged_df <- final_merged_df %>%
  filter(!(LocationAbbr %in% c("PR", "AS")))

#checking for any NA rows 

final_merged_df <- final_merged_df %>%
  filter(!is.na(Percent_uninsured))

final_df <- na.omit(final_merged_df)
final_df

region_map <- c(
  AL = "South", AK = "West", AZ = "West", AR = "South",
  CA = "West", CO = "West", CT = "Northeast", 
  DE = "Northeast", FL = "South", GA = "South", 
  HI = "West", ID = "West", IL = "Midwest", 
  IN = "Midwest", IA = "Midwest", KS = "Midwest", 
  KY = "South", LA = "South", ME = "Northeast", 
  MD = "Northeast", MA = "Northeast", MI = "Midwest", 
  MN = "Midwest", MS = "South", MO = "Midwest", 
  MT = "West", NE = "Midwest", NV = "West", 
  NH = "Northeast", NJ = "Northeast", NM = "West", 
  NY = "Northeast", NC = "South", ND = "Midwest", 
  OH = "Midwest", OK = "South", OR = "West", 
  PA = "Northeast", RI = "Northeast", SC = "South", 
  SD = "Midwest", TN = "South", TX = "South", 
  UT = "West", VT = "Northeast", VA = "South", 
  WA = "West", WV = "South", WI = "Midwest", 
  WY = "West"
)

# Add a Region column based on the state
final_df$Region <- region_map[final_df$LocationAbbr]
final_df$Region <- factor(final_df$Region)
final_df_encoded <- final_df %>%
  mutate(Region = factor(Region))  # ensure Region is a factor

# Use one-hot encoding
final_df_encoded <- dummyVars("~ Region", data = final_df_encoded) %>%
  predict(newdata = final_df_encoded) %>%
  as.data.frame()
final_df_encoded

final_df <- cbind(final_df, final_df_encoded)

#EDA 

# heart disease mortality appears to be evenly distributed and slightly skewed to the right
ggplot(final_df, aes(x = hd_mort)) + 
  geom_histogram(bins = 30)+
  labs(title = "Histogram of Heart Disease Mortality", 
                                y = "Heart Disease Mortality") +
  theme_minimal()

ggplot(final_df, aes(y = hd_mort)) + 
  geom_boxplot() +
  labs(title = "Boxplot of Heart Disease Mortality", 
       y = "Heart Disease Mortality") +
  theme_minimal()

Q1 <- quantile(final_df$hd_mort, 0.25, na.rm = TRUE)
Q3 <- quantile(final_df$hd_mort, 0.75, na.rm = TRUE)
IQR_value <- IQR(final_df$hd_mort, na.rm = TRUE)

# Define the lower and upper bounds for non-outlier data
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Filter the data to remove outliers
final_df <- final_df %>%
  filter(hd_mort >= lower_bound & hd_mort <= upper_bound)

#plotting for continous variables
ggplot(final_df, aes(x = Percent_smoke, y = hd_mort)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatterplot of Heart Disease Mortality vs. Smoking Percentage",
       x = "Percentage of Smokers (%)",
       y = "Heart Disease Mortality") +
  theme_minimal()   

ggplot(final_df, aes(x = PovertyRate, y = hd_mort)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatterplot of Heart Disease Mortality vs. Poverty Rate",
       x = "Poverty Rate (%)",
       y = "Heart Disease Mortality") +
  theme_minimal()   

ggplot(final_df, aes(x = TractWhite, y = hd_mort)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatterplot of Heart Disease Mortality vs. White Population",
       x = "Tract White",
       y = "Heart Disease Mortality") +
  theme_minimal()   

ggplot(final_df, aes(x = TractBlack, y = hd_mort)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatterplot of Heart Disease Mortality vs. Tract Black",
       x = "Tract Black",
       y = "Heart Disease Mortality") +
  theme_minimal()   

ggplot(final_df, aes(x = TractAsian, y = hd_mort)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatterplot of Heart Disease Mortality vs. Tract Asian",
       x = "Tract Asian",
       y = "Heart Disease Mortality") +
  theme_minimal()   

ggplot(final_df, aes(x = TractHispanic, y = hd_mort)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatterplot of Heart Disease Mortality vs. Tract Hispanic ",
       x = "Hispanic Tract",
       y = "Heart Disease Mortality") +
  theme_minimal()   

ggplot(final_df, aes(x = Percent_obese, y = hd_mort)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatterplot of Heart Disease Mortality vs. Obese Percentage",
       x = "Percentage Obese (%)",
       y = "Heart Disease Mortality") +
  theme_minimal() 


ggplot(final_df, aes(x = Percent_inactive, y = hd_mort)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatterplot of Heart Disease Mortality vs. Inactive Percentage",
       x = "Percentage of Inactive People",
       y = "Heart Disease Mortality") +
  theme_minimal() 


ggplot(final_df, aes(x = Percent_access_activity, y = hd_mort)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatterplot of Heart Disease Mortality vs. Physical Activity Access Percentage",
       x = "Percentage of Physical Activity Access",
       y = "Heart Disease Mortality") +
  theme_minimal() 


ggplot(final_df, aes(x = Percent_uninsured, y = hd_mort)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatterplot of Heart Disease Mortality vs. Percentage Uninsured",
       x = "Percentage of Individuals Uninsured",
       y = "Heart Disease Mortality") +
  theme_minimal() 


ggplot(final_df, aes(x = PC_physician_rate, y = hd_mort)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatterplot of Heart Disease Mortality vs. Physician Rate",
       x = "Physician Rate",
       y = "Heart Disease Mortality") +
  theme_minimal() 


ggplot(final_df, aes(x = Percent_unemployed, y = hd_mort)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatterplot of Heart Disease Mortality vs.Percentage Unemployed",
       x = "Percentage of Population Unemployed",
       y = "Heart Disease Mortality") +
  theme_minimal() 


ggplot(final_df, aes(x = Percent_limited_access_to_food, y = hd_mort)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatterplot of Heart Disease Mortality vs.Percentage of Population with limited
       access to food",
       x = "Percentage of Population with limited access to food",
       y = "Heart Disease Mortality") +
  theme_minimal() 


# plotting for binary variables 
ggplot(final_df, aes(x = factor(HUNVFlag), y = hd_mort)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Heart Disease Mortality Rates by HUNV Flag",
       x = "HUNV Flag (0 = Vechicle Access, 1 = No Vechicle)",
       y = "Heart Disease Mortality Rate") +
  theme_minimal()

ggplot(final_df, aes(x = factor(LILATracts_1And10), y = hd_mort)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Heart Disease Mortality Rates by Access to Food 1-10 Miles",
       x = "Access to Food Flag (0 = No Access, 1 = Access)",
       y = "Heart Disease Mortality Rate") +
  theme_minimal()

ggplot(final_df, aes(x = factor(LowIncomeTracts), y = hd_mort)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Heart Disease Mortality Rates by Access to Low Income",
       x = "Low Income Flag (0 = Not Low Income, 1 = Low Income)",
       y = "Heart Disease Mortality Rate") +
  theme_minimal()

#geographical plots 
county_map <- map_data("county")
county_map
final_df$County <- tolower(final_df$LocationDesc)
us_data_map <- merge(county_map, final_df, by.x = "subregion", by.y = "County", all.x = TRUE)
us_data_map <- us_data_map[order(us_data_map$order), ]


ggplot(us_data_map, aes(x = long, y = lat, group = group, fill = Percent_smoke)) +
  geom_polygon(color = "black", size = 0.05) + # borders for counties
  scale_fill_gradient(name = "Percent Smoking", low = "lightgray", high = "#4E2A84") + # color scale
  labs(title = "Percent Smoking by County in the U.S.",
       subtitle = "Color shows the percentage of smokers in each county") +
  theme_void() + # remove axis and grid
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    plot.title.position = "plot",
    legend.position = "bottom"
  )

texas_data <- subset(final_df, StateFull == "Texas")
county_map <- map_data("county")
texas_map <- subset(county_map, region == "texas")
texas_map_data <- merge(texas_map, texas_data, by.x = "subregion", by.y = "County", all.x = TRUE)
texas_map_data <- texas_map_data[order(texas_map_data$order), ]

#plotting for a specific state - Texas
ggplot(texas_map_data, aes(x = long, y = lat, group = group, fill = Percent_smoke)) +
  geom_polygon(color = "black", size = 0.05) +  # Borders for counties
  scale_fill_gradient(name = "Percent Smoking", low = "lightgray", high = "#4E2A84") +  # Color scale
  labs(title = "Percent Smoking by County in Texas",
       subtitle = "Color shows the percentage of smokers in each county") +
  theme_void() +  # Remove axis and grid
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    plot.title.position = "plot",
    legend.position = "bottom"
  )


ggplot(us_data_map, aes(x = long, y = lat, group = group, fill = Percent_obese)) +
  geom_polygon(color = "black", size = 0.05) + # borders for counties
  scale_fill_gradient(name = "Percent Obese", low = "lightgray", high = "#4E2A84") + # color scale
  labs(title = "Percent Obese by County in the U.S.",
       subtitle = "Color shows the percentage of Obese Individuals in each county") +
  theme_void() + # remove axis and grid
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    plot.title.position = "plot",
    legend.position = "bottom"
  )

ggplot(us_data_map, aes(x = long, y = lat, group = group, fill = Percent_uninsured)) +
  geom_polygon(color = "black", size = 0.05) + # borders for counties
  scale_fill_gradient(name = "Percent Uninsured", low = "lightgray", high = "#4E2A84") + # color scale
  labs(title = "Percent Uninsured by County in the U.S.",
       subtitle = "Color shows the percentage of uninsured Individuals in each county") +
  theme_void() + # remove axis and grid
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    plot.title.position = "plot",
    legend.position = "bottom"
  )


ggplot(us_data_map, aes(x = long, y = lat, group = group, fill = PovertyRate)) +
  geom_polygon(color = "black", size = 0.05) + # borders for counties
  scale_fill_gradient(name = "Poverty Rate", low = "lightgray", high = "#4E2A84") + # color scale
  labs(title = "Percent of Population in Poverty by County in the U.S.",
       subtitle = "Color shows the percentage of population in Poverty in each county") +
  theme_void() + # remove axis and grid
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 12),
    plot.title.position = "plot",
    legend.position = "bottom"
  )

#correlation matrix - some varaibles can be excluded 
numeric_data <- final_df[sapply(final_df, is.numeric)]
correlation_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
print(correlation_matrix)


#checking for correlation - VIF < 5: No multicollinearity , VIF 5-10: Moderate multicollinearity
# VIF > 10: High multicollinearity
#keep all variables except remove percent_inactive - as it has a high VIF value
vif_model <- lm(hd_mort ~ ., data = numeric_data)
vif_values <- vif(vif_model)
print(vif_values)


#linear regression model 
# R squared low - may be due to non-linear relationships 
#removed LowIncomeTracts, Percent_access_activity, TractAsian
lm_model <- lm(hd_mort ~ LowIncomeTracts + PovertyRate + LILATracts_1And10 + HUNVFlag +
                 Percent_smoke + TractWhite + TractBlack + TractAsian + TractHispanic + 
                 Percent_obese + Percent_access_activity + Percent_uninsured + 
                 PC_physician_rate + Percent_unemployed + Percent_limited_access_to_food + 
                 Region.Midwest + Region.Northeast + Region.South + Region.West, 
                 data = final_df)
summary(lm_model)

#modeling for ridge, lasso and elastic net 

#creating overall new df
myfulldf <- final_df %>%
  select(hd_mort, Percent_smoke, TractWhite, TractBlack, TractHispanic, LILATracts_1And10,
         Percent_obese, Percent_uninsured, Percent_limited_access_to_food,
         PC_physician_rate, Percent_unemployed, PovertyRate, HUNVFlag, Region.South,
         Region.Northeast)

#splitting the data into test training and validation

initial_splitter <- initial_split(myfulldf, prop = 0.8)
train.df <- training(initial_splitter)
remaining.df <- testing(initial_splitter)
final_splitter <- initial_split(remaining.df, prop = 0.5)
val.df <- training(final_splitter)
test.df <- testing(final_splitter)

glue::glue('Training data size: {dim(train.df)[1]} rows')
glue::glue('Validation data size: {dim(val.df)[1]} rows')
glue::glue('Test data size: {dim(test.df)[1]} rows')

#forumla for all of the models 
model_formula <- hd_mort ~ Percent_smoke + TractWhite + TractBlack + TractHispanic + 
  LILATracts_1And10 + Percent_obese + Percent_limited_access_to_food + Percent_uninsured + 
  PC_physician_rate + Percent_unemployed + PovertyRate + HUNVFlag

# k-fold cross validation test 
cv_folds <- vfold_cv(train.df)

#defining recipes for the models
ridge_recipe <- recipe(model_formula, data=train.df)  %>%
  step_center(all_predictors())  %>%
  step_scale(all_predictors())

lasso_recipe <- recipe(model_formula, data=train.df)  %>%
  step_center(all_predictors())  %>%
  step_scale(all_predictors())

elastic_net_recipe <- recipe(model_formula, data=train.df)  %>%
  step_center(all_predictors())  %>%
  step_scale(all_predictors())

#creating models 
ridge_mod <- linear_reg(penalty = tune(), mixture = 0) %>%
  set_engine("glmnet")

lasso_mod <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet")

elastic_net_mod <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")


#creating a tidymodels workflow for each model 
ridge_wkflw <- workflow() %>% 
  add_recipe(ridge_recipe) %>% 
  add_model(ridge_mod)

lasso_wkflw <- workflow() %>% 
  add_recipe(lasso_recipe) %>% 
  add_model(lasso_mod)

elastic_net_wkflw <- workflow() %>% 
  add_recipe(elastic_net_recipe) %>% 
  add_model(elastic_net_mod)

#tuning the models 
extract_parameter_set_dials(ridge_wkflw) 
extract_parameter_set_dials(lasso_wkflw)
extract_parameter_set_dials(elastic_net_wkflw)


ridge_grid <- grid_regular(penalty(), levels=30)
ridge_control <- control_grid(save_pred = TRUE, verbose = TRUE)
reg_metrics <-metric_set(rmse)
ridge_mod_tune <- tune_grid(ridge_wkflw,
                            resamples = cv_folds,
                            grid = ridge_grid,
                            metrics = reg_metrics,
                            control = ridge_control
)

lasso_grid <- grid_regular(penalty(), levels = 30)
lasso_control <- control_grid(save_pred = TRUE, verbose = TRUE)
lasso_mod_tune <- tune_grid(lasso_wkflw,
                            resamples = cv_folds,
                            grid = lasso_grid,
                            metrics = reg_metrics,
                            control = lasso_control)


elastic_net_grid <- grid_regular(penalty(), mixture(), levels = 30)
elastic_net_control <- control_grid(save_pred = TRUE, verbose = TRUE)
elastic_net_mod_tune <- tune_grid(elastic_net_wkflw,
                                  resamples = cv_folds,
                                  grid = elastic_net_grid,
                                  metrics = reg_metrics,
                                  control = elastic_net_control)


show_best(ridge_mod_tune, metric='rmse') %>% 
  mutate(penalty=format(penalty, digits=12),
         mean=format(mean, digits=12),
         std_err=format(std_err, digits=12))

show_best(lasso_mod_tune, metric='rmse') %>% 
  mutate(penalty=format(penalty, digits=12),
         mean=format(mean, digits=12),
         std_err=format(std_err, digits=12))

show_best(elastic_net_mod_tune, metric='rmse') %>% 
  mutate(penalty=format(penalty, digits=12),
         mean=format(mean, digits=12),
         std_err=format(std_err, digits=12))

#selecting best models 
best_ridge_model <- select_best(ridge_mod_tune,metric='rmse')
ridge_final_wkflw <- ridge_wkflw %>%
  finalize_workflow(best_ridge_model) %>%
  fit(data = train.df)
train_preds_ridge <- predict(ridge_final_wkflw, new_data=train.df)


best_lasso_model <- select_best(lasso_mod_tune,metric='rmse')
lasso_final_wkflw <- lasso_wkflw %>%
  finalize_workflow(best_lasso_model) %>%
  fit(data = train.df)
train_preds_lasso <- predict(lasso_final_wkflw, new_data=train.df)


best_elastic_net_model <- select_best(elastic_net_mod_tune,metric='rmse')
elastic_net_final_wkflw <- elastic_net_wkflw %>% 
  finalize_workflow(best_elastic_net_model) %>%
  fit(data = train.df)
train_preds_elastic_net <- predict(elastic_net_final_wkflw, new_data=train.df)

ridge_test_preds <- predict(ridge_final_wkflw, new_data = test.df)
lasso_test_preds <- predict(lasso_final_wkflw, new_data = test.df)
elastic_net_test_preds <- predict(elastic_net_final_wkflw, new_data = test.df)

# R-squared for all Model
rsq_train_ridge <- cor(train_preds_ridge$.pred, train.df$hd_mort)^2
rsq_test_ridge <- cor(ridge_test_preds$.pred, test.df$hd_mort)^2

rsq_train_lasso <- cor(train_preds_lasso$.pred, train.df$hd_mort)^2
rsq_test_lasso <- cor(lasso_test_preds$.pred, test.df$hd_mort)^2

rsq_train_elastic_net <- cor(train_preds_elastic_net$.pred, train.df$hd_mort)^2
rsq_test_elastic_net <- cor(elastic_net_test_preds$.pred, test.df$hd_mort)^2


train_preds_lasso_df <- bind_cols(
  truth = train.df$hd_mort,
  estimate = train_preds_lasso$.pred
)

# MAE and MSE for Lasso Model (Training Data)
train_lasso_mae <- mae(train_preds_lasso_df, truth = truth, estimate = estimate)

test_preds_lasso_df <- bind_cols(
  truth = test.df$hd_mort,
  estimate = lasso_test_preds$.pred
)

# MAE for Lasso Model (Test Data)
test_lasso_mae <- mae(test_preds_lasso_df, truth = truth, estimate = estimate)

train_lasso_diff <- train.df$hd_mort - train_preds_lasso$.pred

# Calculate squared differences and MAE for lasso
train_lasso_squared_diff <- train_lasso_diff^2
train_lasso_mse <- mean(train_lasso_squared_diff)
test_lasso_diff <- test.df$hd_mort - lasso_test_preds$.pred
test_lasso_squared_diff <- test_lasso_diff^2
test_lasso_mse <- mean(test_lasso_squared_diff)

cat("Lasso Model MSE (Training Data):\n")
cat("MSE:", train_lasso_mse, "\n\n")
cat("Lasso Model MSE (Test Data):\n")
cat("MSE:", test_lasso_mse, "\n")


cat("Lasso Model MAE  (Training Data):\n")
cat("MAE:", train_lasso_mae$.estimate, "\n")
cat("Lasso Model MAE  (Test Data):\n")
cat("MAE:", test_lasso_mae$.estimate, "\n")


train_preds_elastic_net_df <- bind_cols(
  truth = train.df$hd_mort,
  estimate = train_preds_elastic_net$.pred
)

# MAE for Elastic Net Model (Training Data)
train_elastic_net_mae <- mae(train_preds_elastic_net_df, truth = truth, estimate = estimate)

# Create dataframes for Elastic Net predictions (Test Data)
test_preds_elastic_net_df <- bind_cols(
  truth = test.df$hd_mort,
  estimate = elastic_net_test_preds$.pred
)

# MAE for Elastic Net Model (Test Data)
test_elastic_net_mae <- mae(test_preds_elastic_net_df, truth = truth, estimate = estimate)


# MSE for Elastic Net Model (Training Data)
train_elastic_net_diff <- train.df$hd_mort - train_preds_elastic_net$.pred
train_elastic_net_squared_diff <- train_elastic_net_diff^2
train_elastic_net_mse <- mean(train_elastic_net_squared_diff)

# MSE for Elastic Net Model (Test Data)
test_elastic_net_diff <- test.df$hd_mort - elastic_net_test_preds$.pred
test_elastic_net_squared_diff <- test_elastic_net_diff^2
test_elastic_net_mse <- mean(test_elastic_net_squared_diff)

# Print the results
cat("Elastic Net Model MSE (Training Data):\n")
cat("MSE:", train_elastic_net_mse, "\n\n")
cat("Elastic Net Model MSE (Test Data):\n")
cat("MSE:", test_elastic_net_mse, "\n")

cat("Elastic Net Model MAE (Training Data):\n")
cat("MAE:", train_elastic_net_mae$.estimate, "\n")

cat("Elastic Net Model MAE (Test Data):\n")
cat("MAE:", test_elastic_net_mae$.estimate, "\n")


cat("R Squared for Training Data:\n")
cat("Ridge Model:", rsq_train_ridge, "\n")
cat("Lasso Model:", rsq_train_lasso, "\n")
cat("Elastic Net Model:", rsq_train_elastic_net, "\n\n")

cat("R Squared for Test Data:\n")
cat("Ridge Model:", rsq_test_ridge, "\n")
cat("Lasso Model:", rsq_test_lasso, "\n")
cat("Elastic Net Model:", rsq_test_elastic_net, "\n")


#decision tree modeling 
#using same model formula from linear regression

model_recipe <- recipe(model_formula, data = train.df) %>%
  step_center(all_numeric_predictors())  %>%
  step_scale(all_numeric_predictors())

cv_folds1 <- vfold_cv(train.df, v = 5)

all_cores <- parallel::detectCores(logical= FALSE)
cl <- makePSOCKcluster(all_cores)
registerDoParallel(cl)

#defining random forest and gradient boost models
rf_mod <- rand_forest(trees = tune(), mtry = tune(), min_n = tune()) %>%
  set_engine("ranger") %>%
  set_mode("regression")

gb_mod <- boost_tree(trees = tune(), tree_depth = tune(), 
                     learn_rate = tune(), mtry = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

#setting workflows for RF and GB 
rf_wf <- workflow() %>%
  add_recipe(model_recipe) %>%
  add_model(rf_mod)

gb_wf <- workflow() %>%
  add_recipe(model_recipe) %>%
  add_model(gb_mod)


extract_parameter_set_dials(rf_wf)
extract_parameter_set_dials(gb_wf)

# Define the tuning grid for Random Forest

rf_grid <- grid_regular(
  mtry(range = c(3, 8)),
  trees(range = c(100, 500)), 
  min_n(range = c(1, 10)),
  levels = 3
)

gb_grid <- grid_regular(
  trees(range = c(50, 500)),       
  tree_depth(range = c(3, 8)),     
  learn_rate(range = c(0.005, 0.2)), 
  mtry(range = c(2, 10)), 
  levels = 5                      
)

#tuning the model 
rf_tune <- tune_grid(
  rf_wf,
  resamples = cv_folds1,
  grid = rf_grid, 
  control = control_grid(save_pred = TRUE, parallel_over = "everything"),
  metrics = metric_set(rmse, rsq, mae)
)

gb_tune <- tune_grid(
  gb_wf,
  resamples = cv_folds1,
  grid = gb_grid,
  metrics = metric_set(rmse, rsq, mae),
  control = control_grid(save_pred = TRUE, parallel_over = "everything")
)


#selecting best model 
best_rf_model <- select_best(rf_tune, metric = "rmse")  # or "mae" or "rsq"
rf_wf_final <- rf_wf %>%
  finalize_workflow(best_rf_model) %>%
  fit(data = train.df)

best_gb_model <- select_best(gb_tune, metric = "rmse")  # or "mae" or "rsq"
gb_wf_final <- gb_wf %>%
  finalize_workflow(best_gb_model) %>%
  fit(data = train.df)

#final fit of the data training 
final_rf_fit <- rf_wf_final %>%
  fit(data = train.df)

final_gb_fit <- gb_wf_final %>%
  fit(data = train.df)

#final for random forest 
train_rf_pred <- predict(final_rf_fit, new_data = train.df) %>%
  bind_cols(train.df %>% select(hd_mort))
test_rf_pred <- predict(final_rf_fit, new_data = test.df) %>%
  bind_cols(test.df %>% select(hd_mort))

#rmse for random forest
train_rmse <- rmse(train_rf_pred, truth = hd_mort, estimate = .pred)
print(train_rmse)
test_rmse <- rmse(test_rf_pred, truth = hd_mort, estimate = .pred)
print(test_rmse)

# r squared for random forest 
train_r_squared <- rsq(train_rf_pred, truth = hd_mort, estimate = .pred)
print(train_r_squared)
r_squared_result <- rsq(test_rf_pred, truth = hd_mort, estimate = .pred)
print(r_squared_result)

#gb model predictions 
train_gb_pred <- predict(final_gb_fit, new_data = train.df) %>%
  bind_cols(train.df %>% select(hd_mort))
test_gb_pred <- predict(final_gb_fit, new_data = test.df) %>%
  bind_cols(test.df %>% select(hd_mort))

# Calculating RMSE for the test and training data (Gradient Boosting)
train_gb_rmse <- rmse(train_gb_pred, truth = hd_mort, estimate = .pred)
print(train_gb_rmse)
test_gb_rmse <- rmse(test_gb_pred, truth = hd_mort, estimate = .pred)
print(test_gb_rmse)

# Calculating R-squared for the training data (Gradient Boosting)
train_gb_r_squared <- rsq(train_gb_pred, truth = hd_mort, estimate = .pred)
print(train_gb_r_squared)
test_gb_r_squared <- rsq(test_gb_pred, truth = hd_mort, estimate = .pred)
print(test_gb_r_squared)

# random forest better than gradient boost - gradient boost much lower - editing random forest
# to make it better 


#SVM modeling

svm_recipe <- recipe(model_formula, data=train.df) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors())

svm_spec <- svm_rbf(
  mode = "regression",
  cost = 1,
  rbf_sigma = 0.1
)

# Define the parameter grid for tuning
param_grid <- grid_regular(
  cost(range = c(0.1, 10)),         # Search over a range for cost
  rbf_sigma(range = c(0.01, 1)),    # Search over a range for rbf_sigma
  levels = 10                        # You can specify how many levels to test (i.e., number of values in the grid)
)

# Create a 10-fold cross-validation split
cv_folds <- vfold_cv(train.df, v = 10)

# Create the workflow as before
svm_wkflw <- workflow() %>%
  add_recipe(svm_recipe) %>%
  add_model(svm_spec)

# Tune the model using cross-validation
svm_tune_results <- tune_grid(
  svm_wkflw,
  resamples = cv_folds,    # Use cross-validation resamples
  grid = param_grid,       # The grid of hyperparameters
  metrics = metric_set(rmse, rsq),  # Metrics to evaluate performance
  control = control_grid(save_pred = TRUE) # Save predictions for inspection if needed
)

# View the tuning results
svm_tune_results

# Find the best hyperparameters based on RMSE or R-squared
best_svm_params <- select_best(svm_tune_results, metric = "rmse")  # or use "rsq" for R-squared

# Print the best hyperparameters
print(best_svm_params)

# Finalize the workflow with the best hyperparameters
final_svm_spec <- svm_rbf(
  mode = "regression",
  cost = best_svm_params$cost,
  rbf_sigma = best_svm_params$rbf_sigma
)

# Update the workflow with the best model specification
svm_wkflw_final <- svm_wkflw %>%
  finalize_workflow(best_svm_params)

# Fit the final model with the tuned hyperparameters
svm_fit_final <- fit(svm_wkflw_final, data = train.df)

# Summary of the final model
summary(svm_fit_final)

# Predict on the training and test data
train_preds <- predict(svm_fit_final, new_data = train.df) %>%
  bind_cols(train.df)  # Combine predictions with original training data
test_preds <- predict(svm_fit_final, new_data = test.df) %>%
  bind_cols(test.df)  # Combine predictions with original test data

# RMSE and R squared for training data
train_rmse <- rmse(train_preds, truth = hd_mort, estimate = .pred)
train_rsq <- rsq(train_preds, truth = hd_mort, estimate = .pred)

# RMSE and R squared for testing data
test_rmse <- rmse(test_preds, truth = hd_mort, estimate = .pred)
test_rsq <- rsq(test_preds, truth = hd_mort, estimate = .pred)

#calculating MAE for training and test data 
train_mae <- mae(train_preds, truth = hd_mort, estimate = .pred)
test_mae <- mae(test_preds, truth = hd_mort, estimate = .pred)

# Print the results
cat("Training RMSE:", train_rmse$.estimate, "\n")
cat("Training R-squared:", train_rsq$.estimate, "\n")
cat("Testing RMSE:", test_rmse$.estimate, "\n")
cat("Testing R-squared:", test_rsq$.estimate, "\n")
cat("Training MAE:", train_mae$.estimate, "\n")
cat("Testing MAE:", test_mae$.estimate, "\n")

train_mse_manual <- mean((train_preds$.pred - train_preds$hd_mort)^2)
test_mse_manual <- mean((test_preds$.pred - test_preds$hd_mort)^2)

# Print the manually calculated MSE
cat("Manual Training MSE:", train_mse_manual, "\n")
cat("Manual Testing MSE:", test_mse_manual, "\n")


#validating data using SVM modeling
val_preds <- predict(svm_fit_final, new_data = val.df) %>%
bind_cols(val.df)  # Combine predictions with the original validation data

val_rmse <- rmse(val_preds, truth = hd_mort, estimate = .pred)
val_rsq <- rsq(val_preds, truth = hd_mort, estimate = .pred)

# MAE for validation data
val_mae <- mae(val_preds, truth = hd_mort, estimate = .pred)

# Print the results for validation data
cat("Validation RMSE:", val_rmse$.estimate, "\n")
cat("Validation R-squared:", val_rsq$.estimate, "\n")
cat("Validation MAE:", val_mae$.estimate, "\n")
val_mse_manual <- mean((val_preds$.pred - val_preds$hd_mort)^2)

# Print the manually calculated MSE for validation data
cat("Manual Validation MSE:", val_mse_manual, "\n")

