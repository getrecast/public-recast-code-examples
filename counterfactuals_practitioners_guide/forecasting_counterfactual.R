##------------------------------------------------------------------------------
##  LOADING REQUIRED PACKAGES
##------------------------------------------------------------------------------

#Loading packages
library(dplyr)
library(lubridate)
library(ggplot2)
library(forecast)

##------------------------------------------------------------------------------
##  READ IN AND VISUALIZE THE DATA
##------------------------------------------------------------------------------

#Creating a path for the marketing data
data_path <- "~/public-recast-code-examples/counterfactuals_practitioners_guide/mock_dma_gym_data.csv"

#Reading in the data
raw_data <- read.csv(data_path)

#Intervention start date
int_start_date <- as_date("2026-06-01")
int_end_date <- as_date ("2026-06-30")

#Reformatting data
daily_revenue <- raw_data %>%
  mutate(Date = as_date(Date)) %>%
  group_by(Date) %>%
  summarize(Total_Revenue = sum(Revenue)) %>%
  #Creating month and week variables for estimating effects
  mutate(week = week(Date),
         month = month(Date)) %>%
  #Grabbing this for the later join with ARIMA output
  mutate(row_id = row_number())

#Visualizing the data
ggplot(daily_revenue, aes(x = Date, y = Total_Revenue)) + 
  geom_line() + 
  geom_smooth() + 
  geom_vline(
    xintercept = c(int_start_date, int_end_date), 
    color = "red", 
    linetype = "dashed"
    ) + 
  ggtitle("Total Revenue Before and After Display Spend-Up Experiment (Red Line)") 

##------------------------------------------------------------------------------
##  FITTING A BASIC ARIMA MODEL TO THE PRE-INTERVENTION DATA
##------------------------------------------------------------------------------

#Training the ARIMA model on the data up to 30 days preceding the intervention
training_data <- daily_revenue %>%
  filter(Date < int_start_date - 30)

testing_data <- daily_revenue %>%
  filter(between(Date, int_start_date - 30, int_start_date))

#Train regressors
train_xreg <- as.matrix(
  cbind(
    fourier(ts(training_data$week, frequency = 7), K=3),
    fourier(ts(training_data$month, frequency = 365), K=5)
  )
)

#Test regressors
test_xreg <- as.matrix(
  cbind(
    fourier(ts(testing_data$week, frequency = 7), K=3),
    fourier(ts(testing_data$month, frequency = 365), K=5)
  )
)


#Running a simple auto ARIMA model
arima <- auto.arima(
  training_data$Total_Revenue, 
  seasonal = FALSE,
  xreg = train_xreg
  )

#Quickly checking on performance
summary(arima)

#Forecasting forward into time for the 30D holdout to check out of sample performance
forecast <- as.data.frame(
  forecast(
    arima, 
    h = 30,
    xreg = test_xreg
    )
) %>%
  #Creating a row_id for joining
  mutate(
    row_id = as.integer(
      rownames(.)
    )
  )

#Joining the dataframes together
forecast_check <- daily_revenue  %>%
  #Correcting the ARIMA's shift based on Fourier transform
  left_join(., forecast %>% mutate(row_id = row_id - 2), by = "row_id")

#Checking on MAPE to ensure strong out of sample performance
forecast_check %>%
  filter(!is.na(`Point Forecast`)) %>%
  mutate(ape = abs(Total_Revenue - `Point Forecast`) / `Total_Revenue`) %>%
  summarize(mape = mean(ape) * 100)

#Plotting the forecast check
forecast_check %>%
  filter(!is.na(`Point Forecast`)) %>%
  ggplot(., aes(x = Date)) + 
  geom_line(aes(y = Total_Revenue), color = "black") + 
  geom_line(aes(y = `Point Forecast`), color = "blue") + 
  geom_ribbon(aes(ymin = `Lo 95`, ymax = `Hi 95`), alpha = 0.2) + 
  ggtitle("Forecast Results (Blue) v. Actuals (Black) with 95% CIs (Grey)")

##------------------------------------------------------------------------------
##  REFITTING A BASIC ARIMA MODEL FOR FORECASTING AS COUNTERFACTUAL
##------------------------------------------------------------------------------

#Training the ARIMA model on the data up to 30 days preceding the intervention
train2_data <- daily_revenue %>%
  filter(Date < int_start_date)

counterfac_data <- daily_revenue %>%
  filter(between(Date, int_start_date, int_end_date))

#Train regressors
train2_xreg <- as.matrix(
  cbind(
    fourier(ts(train2_data$week, frequency = 7), K=3),
    fourier(ts(train2_data$month, frequency = 365), K=5)
  )
)

#Test regressors
counterfac_xreg <- as.matrix(
  cbind(
    fourier(ts(counterfac_data$week, frequency = 7), K=3),
    fourier(ts(counterfac_data$month, frequency = 365), K=5)
  )
)


#Running a simple auto ARIMA model
counterfac_arima <- auto.arima(
  train2_data$Total_Revenue, 
  seasonal = FALSE,
  xreg = train2_xreg
)

#Quickly checking on performance
summary(counterfac_arima)

#Forecasting forward into time for the 30D holdout to check out of sample performance
counterfac_forecast <- as.data.frame(
  forecast(
    counterfac_arima, 
    h = 30,
    xreg = counterfac_xreg
  )
) %>%
  #Creating a row_id for joining
  mutate(
    row_id = as.integer(
      rownames(.)
    )
  )

#Joining the dataframes together
counterfac <- daily_revenue  %>%
  #Correcting the ARIMA's shift based on Fourier transform
  left_join(., counterfac_forecast %>% mutate(row_id = row_id - 4), by = "row_id")

#Plotting the forecast check
counterfac %>%
  filter(!is.na(`Point Forecast`)) %>%
  ggplot(., aes(x = Date)) + 
  geom_line(aes(y = Total_Revenue), color = "black") + 
  geom_line(aes(y = `Point Forecast`), color = "blue") + 
  geom_ribbon(aes(ymin = `Lo 95`, ymax = `Hi 95`), alpha = 0.2) + 
  ggtitle("Forecast Results (Blue) v. Actuals (Black) with 95% CIs (Grey)")

#Calculating incremental lift of the intervention
counterfac %>%
  filter(!is.na(`Point Forecast`)) %>%
  mutate(diff = Total_Revenue - `Point Forecast`,
         ci_high = Total_Revenue - `Lo 95`,
         ci_low = Total_Revenue - `Hi 95`) %>%
  summarize(lift_estimate = sum(diff),
            standard_error = (sum(ci_high) - sum(ci_low)) / 3.92)