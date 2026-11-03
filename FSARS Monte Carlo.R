##Louis Zielinski
##louis.zielinski@marquette.edu
##Marquette University
##SCR Monte Carlo Simulation

##clear your working directory
rm(list=ls())
##packages
library(tidyverse)
library(dplyr)
library(ggplot2)


##Files##
data <- read_xlsx("C:/Users/zieli/OneDrive/Desktop/Data Intelligence/Data sheets/FSARS Prediction Data/FSARS-v2-Bayesian Modeling-Final.xlsx")
data <- data %>%
  filter(SCR.Response <= 8)
##Log transform the data since so many 0s do exist
data_mc <- data %>%
  mutate(SCR_log = log1p(SCR.Response),
  Outcome = factor(Outcome))
##Making gen model
gen_model <- lm(
  SCR_log ~ SubTrial + CondTrial + Outcome,
  data = data_mc)
summary(gen_model)
##Simulation parameters
beta_hat <- coef(gen_model)
sigma_hat <- sd(residuals(gen_model))

##Creating variable functions and formula 
simulate_SCR <- function(n_trials = 50,
                         outcome,
                         condtrial_fun,
                         beta,
                         sigma) {SubTrial <- 1:n_trials
                         CondTrial <- condtrial_fun(SubTrial)
                         formula <- model.matrix(~ SubTrial + CondTrial + Outcome,
                           data = data.frame(SubTrial = SubTrial, CondTrial = CondTrial,
                             Outcome = factor(outcome, levels = levels(data_mc$Outcome))))
                         SCR_log_sim <- formula %*% beta + rnorm(n_trials, 0, sigma)
                         exp(SCR_log_sim)}

##Running the simulation for fear+safety trials
# CondTrial increases with exposure
set.seed(420)
n_sim <- 2000
mc_sims <- replicate(n_sim,
  simulate_SCR(outcome = -1,
    condtrial_fun = function(t) t,
    beta = beta_hat,
    sigma = sigma_hat))

##50 trial trajectory of mean and 95% confidence bands (to compare against the forecasting completed for my project)
    mc_summary <- data.frame(
      SubTrial = 1:50,
      mean = rowMeans(mc_sims),
      lower = apply(mc_sims, 1, quantile, 0.025),
      upper = apply(mc_sims, 1, quantile, 0.975))
    ##Visual of results
    ggplot(mc_summary, aes(SubTrial, mean)) +
      geom_line(size = 1) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25) +
      theme_minimal() +
      labs(y = "SCR.Response",
           title = "Monte Carlo Simulated SCR Trajectory",
           subtitle = "Shaded area = 95% simulation interval")


    
    
