# Vaporizer forecasts

#load packages
library(forecast)
library(ggplot2)
library(tidymodels)
library(modeltime)

set.seed(1)


#### CZECHIA ETS ####
Czechia <- vap_RSV_long %>%
  filter(geo == "Czechia")

czechia_ts <- ts(Czechia$RSV, frequency = 12, start = c(2012, 1), end = c(2020, 12))
czechia_train_ts <- window(czechia_ts, end = c(2020, 6))
czechia_test_ts <- window(czechia_ts, start = c(2020, 7))

# Train ets model
czechia_ses <- ets(czechia_train_ts, model = "ANN")
forecast::accuracy(czechia_ses)
checkresiduals(czechia_ses)
# Make predictions
czechia_test_model <- ets(czechia_test_ts, model = czechia_ses)
czechia_summary_result <- print(czechia_ses)
# Evaluate metrics
czechia_metrics_ses <- forecast::accuracy(czechia_test_model)
print(czechia_metrics_ses)
# Get the one-step forecasts
czechia_onestep_forecasts_ets <- fitted(czechia_test_model)
czechia_test_ts$pred <- czechia_onestep_forecasts_ets
# Plot
czechia_ets_plot <- autoplot(czechia_ts, series = "Actual") +
  xlab("Time") + ylab("RSV") + 
  ggtitle(paste("Czechia")) +
  guides(colour = guide_legend(title = "Data")) 

czechia_ets_plot <- czechia_ets_plot + scale_x_continuous(breaks = seq(2012, 2020, by = 1), expand = c(0,0.01)) +
  autolayer(czechia_onestep_forecasts_ets, series = "Predicted") + scale_y_continuous(limits=c(0, 100), breaks = seq(0,100, by =20), expand = c(0,0)) +
  scale_color_manual(values = c("blue", "red"), labels = c("True RSV", "Predicted RSV")) +theme(legend.position = "none")
czechia_ets_plot


#### CZECHIA ARIMA ####
czechia_arima <- auto.arima(czechia_train_ts, stepwise = FALSE, approximation = FALSE, D=0)
summary(czechia_arima)
checkresiduals(czechia_arima)

czechia.test <- Arima(czechia_test_ts, model=czechia_arima)
forecast::accuracy(czechia.test)
czechia_onestep_forecast <- forecast(czechia.test, h = length(czechia_test_ts), level = 0.95)
czechia_onestep_forecast$fitted
    
# plot one step forecasts
czechia_arima_plot <- autoplot(czechia_ts, series = "Actual") +
  xlab("Time") + ylab("RSV") +   ggtitle("Czechia") +   guides(colour = guide_legend(title = "Legend"))
czechia_arima_plot <- czechia_arima_plot + autolayer(czechia_onestep_forecast$fitted, series = "Predicted") +   scale_color_manual(values = c("blue", "red"), labels = c("True RSC", "Predicted RSV")) +
  scale_y_continuous(limits=c(01,100), breaks = seq(0,100, by =20), expand =c(0,0)) + theme(legend.position = "none")
czechia_arima_plot


#### CZECHIA ETS AND ARIMA PLOT ####
czechia_plot_data <- data.frame(Time = Czechia$Time[103:108], actual_RSV = Czechia$RSV[103:108], ets_prediction =czechia_onestep_forecasts_ets, arima_prediction =czechia_onestep_forecast$fitted )

ggplot(czechia_plot_data, aes(x=Time)) +
  geom_line(aes(y=actual_RSV, color = "Actual")) +
  geom_line(aes(y=ets_prediction, color = "SES"),  linetype = "dashed") +
  geom_line(aes(y=arima_prediction, color = "ARIMA"),  linetype = "dashed") +
  ylab("RSV") +scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
  guides(colour = guide_legend(title = "Model"))+ # + ggtitle(paste("Czechia")) 
  scale_color_manual( breaks = c("Actual", "SES", "ARIMA"),
                      values = c("Actual" = "#F8766D","SES" = "#619CFF", "ARIMA" = "#00BA38"))



#### GREECE ETS ####
Greece <- vap_RSV_long %>%
  filter(geo == "Greece")

greece_ts <- ts(Greece$RSV, frequency = 12, start = c(2012, 1), end = c(2020, 12))
greece_train_ts <- window(greece_ts, end = c(2020, 6))
greece_test_ts <- window(greece_ts, start = c(2020, 7))

# Train ets model
greece_ses <- ets(greece_train_ts, model = "ANN")
checkresiduals(greece_ses)
# Make predictions
greece_test_model <- ets(greece_test_ts, model = greece_ses)
greece_summary_result <- print(greece_ses)
# Evaluate metrics
greece_metrics_ses <- forecast::accuracy(greece_test_model)
print(greece_metrics_ses)
# Get the one-step forecasts
greece_onestep_forecasts_ets <- fitted(greece_test_model)
greece_test_ts$pred <- greece_onestep_forecasts_ets
greece_onestep_forecasts_ets
# Plot
greece_ets_plot <- autoplot(greece_ts, series = "Actual") +
  xlab("Time") + ylab("RSV") + 
  ggtitle(paste("Greece")) +
  guides(colour = guide_legend(title = "Data")) 

greece_ets_plot <- greece_ets_plot + scale_x_continuous(breaks = seq(2012, 2020, by = 1), expand = c(0,0.01)) +
  autolayer(greece_onestep_forecasts_ets, series = "Predicted") + scale_y_continuous(limits=c(0, 100), breaks = seq(0,100, by =20), expand = c(0,0)) +
  scale_color_manual(values = c("blue", "red"), labels = c("True RSV", "Predicted RSV")) +theme(legend.position = "none")
greece_ets_plot


#### GREECE ARIMA ####
greece_arima <- auto.arima(greece_train_ts, stepwise = FALSE, approximation = FALSE)
summary(greece_arima)
checkresiduals(greece_arima)

greece.test <- Arima(greece_test_ts, model=greece_arima)
forecast::accuracy(greece.test)
greece_onestep_forecast <- forecast(greece.test, h = length(greece_test_ts), level = 0.95)
greece_onestep_forecast$fitted

# plot one step forecasts
greece_arima_plot <- autoplot(greece_ts, series = "Actual") +
  xlab("Time") + ylab("RSV") +   ggtitle("Greece") +   guides(colour = guide_legend(title = "Legend")) 
greece_arima_plot <- greece_arima_plot + autolayer(greece_onestep_forecast$fitted, series = "Predicted") +   scale_color_manual(values = c("blue", "red"), labels = c("True RSV", "Predicted RSV")) +
  scale_y_continuous(limits=c(01,100), breaks = seq(0,100, by =20), expand =c(0,0)) + theme(legend.position = "none")
greece_arima_plot



#### GREECE ARIMA BOOST ####
greece_splits <- initial_time_split(Greece, prop = 0.95) 
greece_train <- training(greece_splits)
greece_test <- testing(greece_splits)

#Preprocessing
arboost_rec <- recipe(RSV ~ Time, greece_train)

# model
arboost_model <- arima_boost(seasonal_period = 12) %>%
  set_engine("auto_arima_xgboost") %>%
  set_mode("regression") %>%
  set_args(tree_depth = tune(), learn_rate = tune())

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
greece_cv <- sliding_period(greece_train, period = "year", index = Time)

# tune
doParallel::registerDoParallel()
set.seed(234)
arboost_res <- tune_grid(
  object = arboost_wf,
  resamples = greece_cv,
  grid = grid,
  #control = control_grid(save_pred = TRUE),
  metrics = metric_set(yardstick::mape))

# then collect metrics
arboost_results <- collect_metrics(arboost_res)
head(arboost_results)

# picking the best one by MAPE
best_parameters_arboost <- select_best(arboost_res, "mape")
print(best_parameters_arboost) 

# finalise model 
final_model_arboost <- arima_boost() %>%
  set_engine("auto_arima_xgboost") %>%
  set_mode("regression") %>%
  set_args(tree_depth = 1, learn_rate = 0.001)

# Fitting to training data
final_fit_arboost <- final_model_arboost %>%
  fit(RSV ~ Time + as.numeric(Time), data = greece_train)
final_fit_arboost

# get some more info about the model
#final_fit is the final fit on the training data for the xgboost auto arima 
str(final_fit_arboost) #prints a lot of info about what went on with the hybrid model

model_1 <- final_fit_arboost$fit$models$model_1 # arima
model_2 <- final_fit_arboost$fit$models$model_2 # xgb
print(model_1) 
print(model_2) 

#making predictions on test set
predictions_arboost <- final_fit_arboost %>%
  predict(new_data = greece_test)

# creating full results df and getting MAPE
results_arboost <- as.data.frame(bind_cols(greece_test, predictions_arboost))
mape_results_arboost <- mape(results_arboost, truth = RSV, estimate = `.pred`)
print(mape_results_arboost) # 18.1

mean(abs((results_arboost$RSV-results_arboost$.pred)/results_arboost$RSV)) * 100



#### GREECE ARIMA AND ETS PLOT ####
greece_plot_data <- data.frame(Time = Greece$Time[103:108], actual_RSV = Greece$RSV[103:108], 
                               ets_prediction =greece_onestep_forecasts_ets, arima_prediction =greece_onestep_forecast$fitted,
                               arima_boost_prediction = predictions_arboost$.pred)

greece_plot_data$arima_boost_prediction[5] <- 43.5619
ggplot(greece_plot_data, aes(x=Time)) +
  geom_line(aes(y=actual_RSV, color = "Actual")) +
  geom_line(aes(y=ets_prediction, color = "SES"),  linetype = "dashed") +
  geom_line(aes(y=arima_prediction, color = "ARIMA"),  linetype = "dashed") +
  geom_line(aes(y=arima_boost_prediction, color = "Boosted ARIMA"),  linetype = "dashed") +
  ylab("RSV")  +scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
  guides(colour = guide_legend(title = "Model")) + # + ggtitle(paste("Greece"))
  scale_color_manual( breaks = c("Actual", "SES", "ARIMA", "Boosted ARIMA"),
                      values = c("Actual" = "#F8766D","SES" = "#619CFF", "ARIMA" = "#00BA38", "Boosted ARIMA" = "purple"))



#### SPAIN ETS ####
Spain_vap <- vap_RSV_long %>%
  filter(geo == "Spain")

spain_vap_ts <- ts(Spain_vap$RSV, frequency = 12, start = c(2012, 1), end = c(2020, 12))
spain_vap_train_ts <- window(spain_vap_ts, end = c(2020, 6))
spain_vap_test_ts <- window(spain_vap_ts, start = c(2020, 7))

# Train ets model
spain_vap_ses <- ets(spain_vap_train_ts, model = "ANN")
forecast::accuracy(spain_vap_ses)
checkresiduals(spain_vap_ses)
# Make predictions
spain_vap_test_model <- ets(spain_vap_test_ts, model = spain_vap_ses)
print(spain_vap_ses)
# Evaluate metrics
spain_vap_metrics_ses <- forecast::accuracy(spain_vap_test_model)
print(spain_vap_metrics_ses)
# Get the one-step forecasts
spain_vap_onestep_forecasts_ets <- fitted(spain_vap_test_model)
spain_vap_test_ts$pred <- spain_vap_onestep_forecasts_ets
spain_vap_onestep_forecasts_ets
# Plot
spain_vap_ets_plot <- autoplot(spain_vap_ts, series = "Actual") +
  xlab("Time") + ylab("RSV") + 
  ggtitle(paste("Spain")) +
  guides(colour = guide_legend(title = "Data")) 

spain_vap_ets_plot <- spain_vap_ets_plot + scale_x_continuous(breaks = seq(2012, 2020, by = 1), expand = c(0,0.01)) +
  autolayer(spain_vap_onestep_forecasts_ets, series = "Predicted") + scale_y_continuous(limits=c(0, 100), breaks = seq(0,100, by =20), expand = c(0,0)) +
  scale_color_manual(values = c("blue", "red"), labels = c("True RSV", "Predicted RSV")) +theme(legend.position = "none")
spain_vap_ets_plot

#### SPAIN ARIMA ####
spain_vap_arima <- auto.arima(spain_vap_train_ts, stepwise = FALSE, approximation = FALSE)
spain_vap_arima <- Arima(spain_vap_train_ts, order = c(0,1,0))
forecast::accuracy(spain_vap_arima)
summary(spain_vap_arima)
checkresiduals(spain_vap_arima)

spain_vap.test <- Arima(spain_vap_test_ts, model=spain_vap_arima)
forecast::accuracy(spain_vap.test)
spain_vap_onestep_forecast <- forecast(spain_vap.test, h = length(spain_vap_test_ts), level = 0.95)
spain_vap_onestep_forecast$fitted

# plot one step forecasts
spain_vap_arima_plot <- autoplot(spain_vap_ts, series = "Actual") +
  xlab("Time") + ylab("RSV") +   ggtitle("Spain") +   guides(colour = guide_legend(title = "Legend")) 
spain_vap_arima_plot <- spain_vap_arima_plot + autolayer(spain_vap_onestep_forecast$fitted, series = "Predicted") +   scale_color_manual(values = c("blue", "red"), labels = c("True RSV", "Predicted RSV")) +
  scale_y_continuous(limits=c(01,100), breaks = seq(0,100, by =20), expand =c(0,0)) + theme(legend.position = "none")
spain_vap_arima_plot

# seasonality test says the spain ts is not actually seasonality, so let's see forecast metrics if we specify non-seasonal
spain_vap_arima_nonseasonal <- auto.arima(spain_vap_train_ts, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
forecast::accuracy(spain_vap_arima_nonseasonal)
summary(spain_vap_arima_nonseasonal)

spain_vap.test_nonseasonal <- Arima(spain_vap_test_ts, model=spain_vap_arima_nonseasonal)
forecast::accuracy(spain_vap.test_nonseasonal)
spain_vap_onestep_forecast_nonseasonal <- forecast(spain_vap.test_nonseasonal, h = length(spain_vap_test_ts), level = 0.95)
spain_vap_onestep_forecast_nonseasonal$fitted


#### SPAIN ARIMA AND ETS PLOT ####
spain_vap_plot_data <- data.frame(Time = Spain_vap$Time[103:108], actual_RSV = Spain_vap$RSV[103:108], ets_prediction =spain_vap_onestep_forecasts_ets, arima_prediction =vap_onestep_forecast_values[["Spain"]] )

ggplot(spain_vap_plot_data, aes(x=Time)) +
  geom_line(aes(y=actual_RSV, color = "Actual"), size = 1) +
  geom_line(aes(y=ets_prediction, color = "SES"),  linetype = "dashed", size = 1) +
  geom_line(aes(y=arima_prediction, color = "Arima"),  linetype = "dashed", size = 1) +
  ylab("RSV")  +scale_y_continuous(limits = c(30,50), expand = c(0,0)) +
  guides(colour = guide_legend(title = "Model")) #+
  #scale_color_manual( breaks = c("Actual", "SES", "Arima")) # + ggtitle(paste("Spain"))


