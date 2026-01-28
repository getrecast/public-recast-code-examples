##------------------------------------------------------------------------------
##  LOADING REQUIRED PACKAGES
##------------------------------------------------------------------------------

#Loading packages
library(dplyr)
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
##  VISUALIZING THE CORRELATION BETWEEN CTV AND LINEAR TV
##------------------------------------------------------------------------------

#First selecting the data for the correlation matrix
cor_data <- syndata %>%
  select(-c(date, revenue))

#Creating the correlation matrix
cor_matrix <- cor(cor_data)

#Printing matrix to console, Visualizing the matrix
cor_matrix
corrplot(cor_matrix, method = "circle")

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
