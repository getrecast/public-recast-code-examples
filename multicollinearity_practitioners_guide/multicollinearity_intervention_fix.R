##------------------------------------------------------------------------------
##  LOADING REQUIRED PACKAGES
##------------------------------------------------------------------------------

#Loading packages
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(magrittr)
library(corrplot)
library(jtools)

##------------------------------------------------------------------------------
##  READING IN SYNTHETIC DATA
##------------------------------------------------------------------------------

#Reading in synthetic marketing data--------------------------------------------
syndata_path <- "~/public-recast-code-examples/multicollinearity_practitioners_guide/marketing_data_2025.csv"
syndata <- read.csv(syndata_path)

##------------------------------------------------------------------------------
##  RUNNING A LINEAR MODEL, ILLUSTRATING BETA-TO-0 ISSUE
##------------------------------------------------------------------------------

#Setting up the basic LM
what_works <- lm(
  revenue ~
    linear_tv +
    ctv +
    meta_prospecting +
    paid_search +
    organic_search +
    tiktok +
    influencers,
  data = syndata
)

#Analyzing the results (jtools to make it purty)
jtools::summ(what_works)
jtools::plot_summs(what_works, ci_level = 0.75)

##------------------------------------------------------------------------------
##  CREATING AND VISUALIZING SOME SPEND VARIATION IN THE MODEL
##------------------------------------------------------------------------------

#Reading in the spend data with linear_tv and ctv variation
syndata_intervention_path <- "~/public-recast-code-examples/multicollinearity_practitioners_guide/modified_marketing_data_2025.csv"
syndata_intervention <- read.csv(syndata_intervention_path)

#Pivoting the dataframes long
syndata_mod <- syndata %>%
  select(date, revenue, linear_tv, ctv) %>%
  pivot_longer(., -date, names_to = "variable", values_to = "dollars") %>%
  mutate(data = "BAU")

syndata_int_mod <- syndata_intervention %>%
  select(date, revenue, linear_tv, ctv) %>%
  pivot_longer(., -date, names_to = "variable", values_to = "dollars") %>%
  mutate(data = "Spend Variation")

#Binding and rolling to the weekly level for simpler visualization
syndata_compare <- rbind(syndata_mod, syndata_int_mod) %>%
  mutate(spend_week = week(date)) %>%
  group_by(spend_week, data, variable) %>%
  summarize(weekly_dollars = sum(dollars))

#Plotting the results
syndata_compare %>%
  ggplot(., aes(x = spend_week, y = weekly_dollars, color = data, group = data)) +
  geom_line() +
  facet_wrap(~variable, scales = "free")

##------------------------------------------------------------------------------
##  CREATING AND VISUALIZING SOME SPEND VARIATION IN THE MODEL
##------------------------------------------------------------------------------

#Setting up the basic LM
intervention_works <- lm(
  revenue ~
    linear_tv +
    ctv +
    meta_prospecting +
    paid_search +
    organic_search +
    tiktok +
    influencers,
  data = syndata_intervention
)

#Analyzing the results (jtools to make it purty)
jtools::summ(intervention_works)
jtools::plot_summs(
  what_works,
  intervention_works,
  model.names = c("BAU", "Spend Variation"),
  ci_level = 0.75
)
