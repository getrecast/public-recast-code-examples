##------------------------------------------------------------------------------
##  LOADING REQUIRED PACKAGES
##------------------------------------------------------------------------------

#Loading packages
library(dplyr)
library(ggplot2)
library(magrittr)
library(corrplot)
library(jtools)
library(brms)
library(bayesplot)

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
frequentist <- lm(
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

#Creating another LM using Bayesian methods, weak priors
bayesian <- brm(
  revenue ~
    linear_tv +
    ctv +
    meta_prospecting +
    paid_search +
    organic_search +
    tiktok +
    influencers,
  data = syndata,
  family = gaussian(),
  prior = c(
    #Set all channel priors to 0.0x-7.0x ROI
    set_prior("normal(0,7)", lb = 0, ub = 7, class = "b"),
    #Set intercept prior to 0.0x-10.0x ROI
    set_prior("normal(0,10)", lb = 0, ub = 10, class = "Intercept")
  ),
  chains = 4,
  seed = 8675309
)

#Analyzing the results of the frequentist model (jtools to make it purty)
jtools::plot_summs(
  frequentist,
  coefs = c("linear_tv", "ctv"),
  ci_level = 0.75
  )

#Analyzing the results of the bayesian model (bayesplot for purtiness)
as.matrix(bayesian) %>%
  mcmc_areas(.,
             pars = c("b_linear_tv", "b_ctv"),
             prob = 0.75)
