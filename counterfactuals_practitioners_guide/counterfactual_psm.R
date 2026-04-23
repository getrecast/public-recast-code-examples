library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(MatchIt)
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

#Aggregating total daily revenue across all DMAs to visualize the experiment
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
##  CONSTRUCTING PRE-INTERVENTION DMA FEATURES
##
##  PSM requires DMA-level covariates measured before the intervention.
##  We use all available factors: display metrics, market density, weather
##  composition, gym memberships sold, and baseline revenue.
##------------------------------------------------------------------------------

dma_data <- raw_data %>%
  mutate(Date = as_date(Date))

#Identify treated DMAs: those where spend_up == "spend_up" during the intervention
treated_flags <- dma_data %>%
  filter(Date >= int_start_date, Date <= int_end_date) %>%
  group_by(DMA) %>%
  summarize(treated = as.integer(any(spend_up == "spend_up")))

#Pre-intervention data for covariate construction
pre_int_data <- dma_data %>%
  filter(Date < int_start_date)

#Proportion of days with each weather condition per DMA (captures climate profile)
weather_props <- pre_int_data %>%
  count(DMA, Weather_Condition) %>%
  group_by(DMA) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  select(-n) %>%
  pivot_wider(
    names_from   = Weather_Condition,
    values_from  = prop,
    names_prefix = "pct_"
  )

#DMA-level averages of all numeric factors over the pre-intervention period
dma_features <- pre_int_data %>%
  group_by(DMA) %>%
  summarize(
    avg_impressions  = mean(Display_Impressions),
    avg_clicks       = mean(Display_Clicks),
    avg_spend        = mean(Display_Spend),
    gyms_per_sq_mile = first(Gyms_Per_Sq_Mile),
    avg_memberships  = mean(Gym_Memberships_Sold),
    avg_revenue      = mean(Revenue)
  ) %>%
  left_join(treated_flags, by = "DMA") %>%
  left_join(weather_props,  by = "DMA")

##------------------------------------------------------------------------------
##  PROPENSITY SCORE MATCHING
##
##  We estimate the probability of a DMA being selected for the spend-up
##  (its propensity score) using logistic regression on pre-intervention
##  characteristics, then match each treated DMA to its nearest control DMAs.
##
##  Note: with only 10 DMAs (2 treated, 8 control), we use a parsimonious
##  propensity score formula to avoid overfitting. ratio = 2 matches each
##  treated DMA to its 2 closest controls, giving us 4 control DMAs total
##  to construct the counterfactual.
##------------------------------------------------------------------------------

match_formula <- treated ~ avg_revenue + avg_spend + gyms_per_sq_mile +
  avg_memberships + avg_impressions + pct_Sunny + pct_Rainy

matchit_out <- matchit(
  match_formula,
  data    = dma_features,
  method  = "nearest",
  ratio   = 2,
  replace = FALSE
)

#Reviewing overall balance before and after matching
summary(matchit_out)

#Balance diagnostics: propensity score distributions and covariate balance
plot(matchit_out, type = "jitter", interactive = FALSE)
plot(matchit_out, type = "hist")

##------------------------------------------------------------------------------
##  EXTRACTING MATCHED DMAS
##------------------------------------------------------------------------------

matched_data <- match.data(matchit_out)

treated_dmas <- matched_data %>% filter(treated == 1) %>% pull(DMA)
control_dmas <- matched_data %>% filter(treated == 0) %>% pull(DMA)

cat("Treated DMAs:", paste(treated_dmas, collapse = ", "), "\n")
cat("Matched Control DMAs:", paste(control_dmas, collapse = ", "), "\n")

##------------------------------------------------------------------------------
##  CONSTRUCTING THE COUNTERFACTUAL
##
##  For each day in the intervention period, the average daily revenue of the
##  matched control DMAs serves as the counterfactual for the treated DMAs.
##  We compare this to the treated DMAs' actual average daily revenue.
##------------------------------------------------------------------------------

int_period_data <- dma_data %>%
  filter(
    Date >= int_start_date,
    Date <= int_end_date,
    DMA  %in% c(treated_dmas, control_dmas)
  ) %>%
  mutate(group = if_else(DMA %in% treated_dmas, "Treated", "Control"))

#Compute average daily revenue per group for each date
daily_comparison <- int_period_data %>%
  group_by(Date, group) %>%
  summarize(avg_revenue = mean(Revenue), .groups = "drop") %>%
  pivot_wider(names_from = group, values_from = avg_revenue) %>%
  rename(Actual = Treated, Counterfactual = Control) %>%
  mutate(Daily_Lift = Actual - Counterfactual)

##------------------------------------------------------------------------------
##  ESTIMATING LIFT
##------------------------------------------------------------------------------

n_treated      <- length(treated_dmas)
total_lift     <- sum(daily_comparison$Daily_Lift) * n_treated
avg_daily_lift <- mean(daily_comparison$Daily_Lift)
pct_lift       <- mean(daily_comparison$Daily_Lift / daily_comparison$Counterfactual) * 100

cat(sprintf("Total Revenue Lift (all treated DMAs): $%.0f\n",  total_lift))
cat(sprintf("Average Daily Lift per Treated DMA: $%.0f\n",  avg_daily_lift))
cat(sprintf("Average Daily Lift (%%): %.1f%%\n", pct_lift))

##------------------------------------------------------------------------------
##  VISUALIZING RESULTS
##------------------------------------------------------------------------------

#Actual vs. counterfactual average daily revenue per DMA
ggplot(daily_comparison, aes(x = Date)) +
  geom_line(aes(y = Actual,         color = "Treated (Actual)")) +
  geom_line(aes(y = Counterfactual, color = "PSM Counterfactual"), linetype = "dashed") +
  geom_ribbon(
    aes(
      ymin = pmin(Actual, Counterfactual),
      ymax = pmax(Actual, Counterfactual)
    ),
    alpha = 0.15,
    fill  = "steelblue"
  ) +
  scale_color_manual(
    values = c("Treated (Actual)" = "steelblue", "PSM Counterfactual" = "red")
  ) +
  labs(
    title  = "Actual vs. PSM Counterfactual Revenue — Treated DMAs (June 2026)",
    x      = "Date",
    y      = "Average Daily Revenue per DMA ($)",
    color  = NULL
  ) +
  theme_minimal()
