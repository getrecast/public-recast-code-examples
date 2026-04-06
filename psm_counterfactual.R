##------------------------------------------------------------------------------
##  LOADING REQUIRED PACKAGES
##------------------------------------------------------------------------------

#Loading packages
library(dplyr)
library(lubridate)
library(ggplot2)
library(here)

##------------------------------------------------------------------------------
##  READ IN AND VISUALIZE THE DATA
##------------------------------------------------------------------------------

#Creating a path for the marketing data
data_path <- here("counterfactuals_practitioners_guide", "mock_dma_gym_data.csv")

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
##  CREATING PROPENSITY SCORES
##------------------------------------------------------------------------------

