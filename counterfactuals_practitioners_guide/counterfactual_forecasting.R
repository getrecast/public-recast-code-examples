##------------------------------------------------------------------------------
##  LOADING REQUIRED PACKAGES
##------------------------------------------------------------------------------

library(dplyr)
library(lubridate)
library(ggplot2)
library(CausalImpact)
library(zoo)
library(here)

##------------------------------------------------------------------------------
##  READ IN AND VISUALIZE THE DATA
##------------------------------------------------------------------------------

#Creating a path for the marketing data
data_path <- here("counterfactuals_practitioners_guide", "mock_dma_gym_data.csv")

#Reading in the data
raw_data <- read.csv(data_path)

#Intervention start and end dates
int_start_date <- as_date("2026-06-01")
int_end_date   <- as_date("2026-06-30")

#Aggregating to daily revenue
daily_revenue <- raw_data %>%
  mutate(Date = as_date(Date)) %>%
  group_by(Date) %>%
  summarize(Total_Revenue = sum(Revenue))

#Visualizing the data
ggplot(daily_revenue, aes(x = Date, y = Total_Revenue)) +
  geom_line() +
  geom_smooth() +
  geom_vline(
    xintercept = c(int_start_date, int_end_date),
    color      = "red",
    linetype   = "dashed"
  ) +
  ggtitle("Total Revenue Before and After Display Spend-Up Experiment (Red Lines)")

##------------------------------------------------------------------------------
##  FITTING A CausalImpact MODEL
##
##  CausalImpact fits a Bayesian structural time series (BSTS) model on the
##  pre-intervention period and uses it as the counterfactual for the post-
##  intervention period. Pre-period model fit (analogous to the ARIMA holdout
##  check) is visible in the first panel of plot(impact) below.
##------------------------------------------------------------------------------

#Converting to a zoo time series, which CausalImpact expects
revenue_ts <- zoo(daily_revenue$Total_Revenue, daily_revenue$Date)

#Defining pre- and post-intervention windows
pre_period  <- c(min(daily_revenue$Date), int_start_date - 1)
post_period <- c(int_start_date, int_end_date)

#Running the CausalImpact model
impact <- CausalImpact(revenue_ts, pre_period, post_period)

##------------------------------------------------------------------------------
##  REVIEWING RESULTS
##------------------------------------------------------------------------------

#Plotting observed vs. counterfactual
plot(impact, "original") +
  scale_x_date(limits = c(as_date("2026-01-01"), max(as_date(raw_data$Date))))

#Printing a statistical summary of the estimated impact
summary(impact)

##------------------------------------------------------------------------------
##  EXTRACTING LIFT ESTIMATES
##------------------------------------------------------------------------------

#Pulling cumulative lift and confidence bounds from the model summary
impact_summary <- impact$summary

lift_estimate <- impact_summary["Cumulative", "AbsEffect"]
lift_ci_lower <- impact_summary["Cumulative", "AbsEffect.lower"]
lift_ci_upper <- impact_summary["Cumulative", "AbsEffect.upper"]

cat(
  sprintf(
    "Incremental Revenue Lift: $%.0f  (95%% CI: $%.0f to $%.0f)\n",
    lift_estimate, lift_ci_lower, lift_ci_upper
  )
)
