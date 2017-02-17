###################################
## Script for converting alpha likelihood output to
## data frame with calculated confidence intervals of alpha
###################################
rm(list=ls())
library(tidyverse)
library(stringr)
library(lubridate)

sapply(c("R/fitting_fxns.R", "R/load_data.R", "R/scaling_analysis_fxns.R"), source)


load("data_produced/alpha_likelihoods/alpha_like_single_med_r0.rda")

alphas <- c(0.05, 0.01)

find_upper_alpha <- function(alpha, likelihood, sig_level){
  ## df should have column with alpha and likelihood
  ## Doesn't work if everything is below the significance level.

  alpha[likelihood >= sig_level] %>% tail(1)
}

hist(est_alphas_df$`2016-05-26`, breaks=100)

est_alphas_df %>% gather(date, likelihood, 2:ncol(est_alphas_df)) %>%
  mutate(date = ymd(date)) %>%
  group_by(date) %>%
  summarize(sig_0.05 = find_upper_alpha(alpha, likelihood, 0.05),
            sig_0.01 = find_upper_alpha(alpha, likelihood, 0.01)) -> test



























#
# ####################################
# ## Calculating statewide alpha from county importation data
# ####################################
# ## For every unique notification date, gather the imports and rnots summarized by month
# daily_parms <- unique(tx_data$notification_date) %>%
#   purrr::map(~get_alpha_parms(tx_data, curr_date=.x))
#
# # get_alpha_ci(parms = daily_parms[[110]], sig_level = 0.05)
#
# load("data_produced/dispersion_dt.rda")
# est_alphas <- purrr::map(daily_parms[1:2], get_alpha_ci, disp_dt = dispersion_dt,sig_level=0.05)
# ## Estimate alphas for each day that has an importation
# library(profvis)
# profvis({
#   est_alphas <- purrr::map(daily_parms[1:2], get_alpha_likes, disp_dt = dispersion_dt)
# })
# est_alphas <- purrr::map(daily_parms, get_alpha_ci, sig_level=0.01)
#
#
# est_alphas_df <- est_alphas %>% bind_rows() %>%
#                 mutate(info = unique(tx_data$notification_date))
#
# save(est_alphas_df, file = "data_produced/statewide_alphas_through_time.rda")
#
#
# ######################################
# ## Scaling County R0s by statewide alpha
# ######################################
# load("data_produced/statewide_alphas_through_time.rda")
# statewide_alpha_rnots <- est_alphas_df %>% mutate(month = lubridate::month(info, label=TRUE)) %>%
#   left_join(tx_county_rnots, by = "month")
# save(statewide_alpha_rnots, file = "data_produced/statewide_alphas_rnots.rda")
#
#
# ####################################
# ## Calculating county specific alpha scaling
# ####################################
#
# get_county_parms <- function(tx_data, desired_county){
#   ## Gets the parms for a specified county for calculating the alphas through time
#   county_data <- tx_data %>% filter(county==desired_county)
#   if(nrow(county_data)==0){
#     stop("Misspecified county")
#   }
#   unique(county_data$notification_date) %>%
#     purrr::map(~get_alpha_parms(county_data, curr_date=.x))
# }
#
# county_specific_parms <- unique(tx_data$county) %>% purrr::map(~get_county_parms(tx_data, desired_county=.x))
# county_notification_dates <- purrr::map(county_specific_parms, extract_notification_dates)
# county_est_alphas <- purrr::map(county_specific_parms, get_county_alphas, sig_level=0.01)
#
# county_est_alphas_df <- county_est_alphas %>% purrr::map( bind_rows)
#
# county_alphas <- data_frame(county = unique(tx_data$county)) %>%
#   mutate(alphas = county_est_alphas_df, date = county_notification_dates) %>%
#   unnest(alphas, date) %>%
#   mutate(month = month(date, label = T)) %>%
#   left_join(tx_county_rnots, by = c("county", "month"))
#
# save(county_alphas, file="data_produced/county_alphas_rnots.rda")

