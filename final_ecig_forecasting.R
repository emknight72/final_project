# Electronic cigarette forecasts

# load packages
library(ggplots2)
library(forecast)
library(tidymodels)
library(modeltime)
set.seed(1)

#### MALTA ETS ####
Malta <- ecig_RSV_long %>%
  filter(geo == "Malta")

malta_ts <- ts(Malta$RSV, frequency = 12, start = c(2012, 1), end = c(2020, 12))
malta_train_ts <- window(malta_ts, end = c(2020, 6))
malta_test_ts <- window(malta_ts, start = c(2020, 7))

# Train ets model
malta_ses <- ets(malta_train_ts, model = "ANN")
checkresiduals(malta_ses)
# Make predictions
malta_test_model <- ets(malta_test_ts, model = malta_ses)
summary_result <- print(malta_ses)
# Evaluate metrics
malta_metrics_ses <- round(forecast::accuracy(malta_test_model), 3)
print(malta_metrics_ses)
# Get the one-step forecasts
malta_onestep_forecasts_ets <- fitted(malta_test_model)
malta_test_ts$pred <- malta_onestep_forecasts_ets
# Plot
malta_ets_plot <- autoplot(malta_ts, series = "Actual") +
  xlab("Time") + ylab("RSV") + 
  ggtitle(paste("Malta")) +
  guides(colour = guide_legend(title = "Data")) 

malta_ets_plot <- malta_ets_plot + scale_x_continuous(breaks = seq(2012, 2020, by = 1), expand = c(0,0.01)) +
  autolayer(malta_onestep_forecasts_ets, series = "Predicted") + scale_y_continuous(limits=c(0, 100), breaks = seq(0,100, by =20), expand = c(0,0)) +
  scale_color_manual(values = c("blue", "red"), labels = c("True RSV", "Predicted RSV")) +theme(legend.position = "none")
malta_ets_plot

malta_onestep_forecasts_ets


#### MALTA ARIMA ####
# Fit ARIMA model on the training set
malta_arima_model <- auto.arima(malta_train_ts, stepwise = FALSE, approximation = FALSE)
summary(malta_arima_model)
checkresiduals(malta_arima_model)

# refit and get accuracy metric 
malta.test <- Arima(malta_test_ts, model=malta_arima_model)
forecast::accuracy(malta.test)

# get the forecast values
malta_onestep_forecast <- forecast(malta.test, h = length(malta_test_ts), level = 0.95)
malta_onestep_forecast$fitted

# Plot one-step forecasts
malta_arima_plot <- autoplot(malta_ts, series = "Actual") +
  xlab("Time") + ylab("RSV") +
  guides(colour = guide_legend(title = "Data")) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0))
# Overlay the predicted values from the test set
malta_arima_plot <- malta_arima_plot + autolayer(malta_onestep_forecast$fitted, series = "Predicted") +
  scale_color_manual(values = c("blue", "red"), labels = c("True RSV", "Predicted RSV")) + theme(legend.position = "none") + ggtitle(paste("Malta"))
malta_arima_plot

# check residuals and qqnorm
checkresiduals(malta_onestep_forecast)


#### MALTA ARIMA BOOST ####
malta_splits <- initial_time_split(Malta, prop = 0.95) 
malta_train <- training(malta_splits)
malta_test <- testing(malta_splits)

#Preprocessing
arboost_rec <- recipe(RSV ~ Time, malta_train)

# model
arboost_model <- arima_boost(seasonal_period = 12, non_seasonal_ar=1,non_seasonal_differences=1, non_seasonal_ma =1 ) %>%
  set_engine("auto_arima_xgboost") %>%
  set_mode("regression") %>%
  set_args(tree_depth = tune(), learn_rate = tune(),  non_seasonal_ar=1,non_seasonal_differences=1, non_seasonal_ma =1)

#workflow
arboost_wf <- workflow() %>%
  add_recipe(arboost_rec) %>%
  #add_formula(median_RSV_ecig ~ Time) %>%
  add_model(arboost_model)


# Set up a grid of hyperparameters to tune:
grid <- grid_latin_hypercube(
  tree_depth(),learn_rate())

grid <- expand.grid(
  tree_depth = seq(1, 10, by = 1),
  learn_rate = c(0.001, 0.01, 0.1, 1))

# cv data 
malta_cv <- sliding_period(malta_train, period = "year", index = Time)

# tune
doParallel::registerDoParallel()
set.seed(234)
malta_arboost_res <- tune_grid(
  object = arboost_wf,
  resamples = malta_cv,
  grid = grid,
  #control = control_grid(save_pred = TRUE),
  metrics = metric_set(yardstick::mape))

# then collect metrics
arboost_results <- collect_metrics(malta_arboost_res)
head(arboost_results)

# picking the best one by MAPE
malta_best_parameters_arboost <- select_best(malta_arboost_res, "mape")
print(malta_best_parameters_arboost) 

# finalise model 
malta_final_model_arboost <- arima_boost(non_seasonal_ar=1,non_seasonal_differences=1, non_seasonal_ma =1 ) %>%
  set_engine("auto_arima_xgboost") %>%
  set_mode("regression") %>%
  set_args(tree_depth = malta_best_parameters_arboost$tree_depth, learn_rate = malta_best_parameters_arboost$learn_rate,
           non_seasonal_ar=1,non_seasonal_differences=1, non_seasonal_ma =1 )

# Fitting to training data
malta_final_fit_arboost <- malta_final_model_arboost %>%
  fit(RSV ~ Time + as.numeric(Time), data = malta_train)
malta_final_fit_arboost

# get some more info about the model
#final_fit is the final fit on the training data for the xgboost auto arima 
str(malta_final_fit_arboost) #prints a lot of info about what went on with the hybrid model

malta_model_1 <- malta_final_fit_arboost$fit$models$model_1 # arima
malta_model_2 <- malta_final_fit_arboost$fit$models$model_2 # xgb
print(malta_model_1) 
print(malta_model_2) 

#making predictions on test set
malta_predictions_arboost <- malta_final_fit_arboost %>%
  predict(new_data = malta_test)
malta_predictions_arboost

# creating full results df and getting MAPE
malta_results_arboost <- as.data.frame(bind_cols(malta_test, malta_predictions_arboost))
mape_results_arboost <- mape(malta_results_arboost, truth = RSV, estimate = `.pred`)
print(mape_results_arboost) 




### MALTA - plot ETS, ARIMA and ARIMA BOOST together ####
malta_plot_data <- data.frame(Time = Malta$Time[103:108], actual_RSV = Malta$RSV[103:108], 
                              ets_prediction =malta_onestep_forecasts_ets, arima_prediction =malta_onestep_forecast$fitted,
                              arima_boost_prediction = malta_predictions_arboost$.pred)

ggplot(malta_plot_data, aes(x=Time)) +
  geom_line(aes(y=actual_RSV, color = "Actual")) +
  geom_line(aes(y=ets_prediction, color = "SES"),  linetype = "dashed") +
  geom_line(aes(y=arima_prediction, color = "ARIMA"),  linetype = "dashed") +
  geom_line(aes(y=arima_boost_prediction, color = "Boosted ARIMA"),  linetype = "dashed") +
  ylab("RSV")  +scale_y_continuous(limits = c(0,50), expand = c(0,0)) +
  guides(colour = guide_legend(title = "Model")) +
  scale_color_manual( breaks = c("Actual", "SES", "ARIMA", "Boosted ARIMA"),
                      values = c("Actual" = "#F8766D","SES" = "#619CFF", "ARIMA" = "#00BA38", "Boosted ARIMA" = "purple")) # + ggtitle(paste("Malta"))





### NETHERLANDS ETS ####
Netherlands <- ecig_RSV_long %>%
  filter(geo == "Netherlands")

netherlands_ts <- ts(Netherlands$RSV, frequency = 12, start = c(2012, 1), end = c(2020, 12))
netherlands_train_ts <- window(netherlands_ts, end = c(2020, 6))
netherlands_test_ts <- window(netherlands_ts, start = c(2020, 7))

# Train ets model
netherlands_ses <- ets(netherlands_train_ts, model = "ANN")
checkresiduals(netherlands_ses)
# Make predictions
netherlands_test_model <- ets(netherlands_test_ts, model = netherlands_ses)
netherlands_summary_result <- print(netherlands_ses)
# Evaluate metrics
netherlands_metrics_ses <- forecast::accuracy(netherlands_test_model)
print(netherlands_metrics_ses)
# Get the one-step forecasts
netherlands_onestep_forecasts_ets <- fitted(netherlands_test_model)
netherlands_test_ts$pred <- netherlands_onestep_forecasts_ets
# Plot
netherlands_ets_plot <- autoplot(netherlands_ts, series = "Actual") +
  xlab("Time") + ylab("RSV") + 
  ggtitle(paste("Malta")) +
  guides(colour = guide_legend(title = "Data")) 

netherlands_ets_plot <- netherlands_ets_plot + scale_x_continuous(breaks = seq(2012, 2020, by = 1), expand = c(0,0.01)) +
  autolayer(netherlands_onestep_forecasts_ets, series = "Predicted") + scale_y_continuous(limits=c(0, 100), breaks = seq(0,100, by =20), expand = c(0,0)) +
  scale_color_manual(values = c("blue", "red"), labels = c("True RSV", "Predicted RSV")) +theme(legend.position = "none")
netherlands_ets_plot


#### NETHERLANDS ARIMA ####
# Fit ARIMA model on the training set
netherlands_arima_model <- auto.arima(netherlands_train_ts, stepwise = FALSE, approximation = FALSE)
netherlands_arima_model <- Arima(netherlands_train_ts, order = c(0,1,0))
summary(netherlands_arima_model)
checkresiduals(netherlands_arima_model)

# refit and get accuracy metric 
netherlands.test <- Arima(netherlands_test_ts, model=netherlands_arima_model)
forecast::accuracy(netherlands.test)

# get the forecast values
netherlands_onestep_forecast <- forecast(netherlands.test, h = length(netherlands_test_ts), level = 0.95)
netherlands_onestep_forecast$fitted
netherlands_onestep_forecast$fitted[4] <- 

# Plot one-step forecasts
netherlands_arima_plot <- autoplot(netherlands_ts, series = "Actual") +
  xlab("Time") + ylab("RSV") +
  guides(colour = guide_legend(title = "Data")) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0))
# Overlay the predicted values from the test set
netherlands_arima_plot <- netherlands_arima_plot + autolayer(netherlands_onestep_forecast$fitted, series = "Predicted") +
  scale_color_manual(values = c("blue", "red"), labels = c("True RSV", "Predicted RSV")) + theme(legend.position = "none") + ggtitle(paste("Netherlands"))
netherlands_arima_plot

# check residuals and qqnorm
checkresiduals(netherlands_onestep_forecast)


#### NETHERLANDS PLOT ARIMA AND ETS ####
netherlands_plot_data <- data.frame(Time = Netherlands$Time[103:108], actual_RSV = Netherlands$RSV[103:108], ets_prediction =netherlands_onestep_forecasts_ets, arima_prediction =netherlands_onestep_forecast$fitted )


ggplot(netherlands_plot_data, aes(x=Time)) +
  geom_line(aes(y=actual_RSV, color = "Actual")) +
  geom_line(aes(y=ets_prediction, color = "SES"), size = 1.2, linetype = "dashed") +
  geom_line(aes(y=arima_prediction, color = "ARIMA"),  size = 1, linetype = "dashed") +
  ylab("RSV")  +scale_y_continuous(limits = c(0,50), expand = c(0,0)) +
  guides(colour = guide_legend(title = "Model")) +# + ggtitle(paste("Netherlands"))
  scale_color_manual( breaks = c("Actual", "SES", "ARIMA"),
                      values = c("Actual" = "#F8766D","SES" = "#619CFF", "ARIMA" = "#00BA38"))





#### SPAIN ETS ####
Spain_ecig <- ecig_RSV_long %>%
  filter(geo == "Spain")

spain_ecig_ts <- ts(Spain_ecig$RSV, frequency = 12, start = c(2012, 1), end = c(2020, 12))
spain_ecig_train_ts <- window(spain_ecig_ts, end = c(2020, 6))
spain_ecig_test_ts <- window(spain_ecig_ts, start = c(2020, 7))

# Train ets model
spain_ecig_ses <- ets(spain_ecig_train_ts, model = "ANN")
checkresiduals(spain_ecig_ses)
forecast::accuracy(spain_ecig_ses)

# Make predictions
spain_ecig_test_model <- ets(spain_ecig_test_ts, model = spain_ecig_ses)
spain_ecig_summary_result <- print(spain_ecig_ses)
# Evaluate metrics
spain_ecig_metrics_ses <- forecast::accuracy(spain_ecig_test_model)
print(spain_ecig_metrics_ses)
# Get the one-step forecasts
spain_ecig_onestep_forecasts_ets <- fitted(spain_ecig_test_model)
spain_ecig_test_ts$pred <- spain_ecig_onestep_forecasts_ets
# Plot
spain_ecig_ets_plot <- autoplot(spain_ecig_ts, series = "Actual") +
  xlab("Time") + ylab("RSV") + 
  ggtitle(paste("Malta")) +
  guides(colour = guide_legend(title = "Data")) 

spain_ecig_ets_plot <- spain_ecig_ets_plot + scale_x_continuous(breaks = seq(2012, 2020, by = 1), expand = c(0,0.01)) +
  autolayer(spain_ecig_onestep_forecasts_ets, series = "Predicted") + scale_y_continuous(limits=c(0, 100), breaks = seq(0,100, by =20), expand = c(0,0)) +
  scale_color_manual(values = c("blue", "red"), labels = c("True RSV", "Predicted RSV")) +theme(legend.position = "none")
spain_ecig_ets_plot


#### SPAIN ARIMA ####
# Fit ARIMA model on the training set
spain_arima_model <- auto.arima(spain_ecig_train_ts, stepwise = FALSE, approximation = FALSE)
summary(spain_arima_model)
checkresiduals(spain_arima_model)
forecast::accuracy(spain_arima_model)

# refit and get accuracy metric 
spain.test <- Arima(spain_ecig_test_ts, model=spain_arima_model)
forecast::accuracy(spain.test)

# get the forecast values
spain_onestep_forecast <- forecast(spain.test, h = length(spain_ecig_test_ts), level = 0.95) 
spain_onestep_forecast$fitted
forecast::accuracy(spain_onestep_forecast$fitted, spain_ecig_test_ts)

# Plot one-step forecasts
spain_arima_plot <- autoplot(spain_ts, series = "Actual") +
  xlab("Time") + ylab("RSV") +
  guides(colour = guide_legend(title = "Legend")) +
  scale_y_continuous(limits = c(0, 100),expand = c(0, 0))
# Overlay the predicted values from the test set
spain_arima_plot <- spain_arima_plot + autolayer(spain_onestep_forecast$fitted, series = "Predicted") +
  scale_color_manual(values = c("blue", "red"), labels = c("True RSV", "Predicted RSV")) +ggtitle(paste("Spain")) +  theme(legend.position = "none") 
spain_arima_plot



#### SPAIN PLOT ARIMA AND ETS ####
spain_plot_data <- data.frame(Time = Spain$Time[103:108], actual_RSV = Spain$RSV[103:108], ets_prediction =spain_ecig_onestep_forecasts_ets, arima_prediction =spain_onestep_forecast$fitted )

ggplot(spain_plot_data, aes(x=Time)) +
  geom_line(aes(y=actual_RSV, color = "Actual")) +
  geom_line(aes(y=ets_prediction, color = "SES"),  linetype = "dashed", size = 1) +
  geom_line(aes(y=arima_prediction, color = "ARIMA"),  linetype = "dashed", size = 1) +
  ylab("RSV")  +scale_y_continuous(limits = c(0,50), expand = c(0,0)) +
  guides(colour = guide_legend(title = "Model")) + # + ggtitle(paste("Spain"))
  scale_color_manual( breaks = c("Actual", "SES", "ARIMA"),
                      values = c("Actual" = "#F8766D","SES" = "#619CFF", "ARIMA" = "#00BA38"))




