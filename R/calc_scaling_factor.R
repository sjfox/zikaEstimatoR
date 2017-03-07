###################################
## Script for converting alpha likelihood output to
## data frame with calculated confidence intervals of alpha
## Also produced scaled R0 estimates through time for use in figures
###################################
rm(list=ls())
library(tidyverse)
library(stringr)
library(lubridate)

sapply(c("R/fitting_fxns.R", "R/scaling_analysis_fxns.R"), source)



####################################################################
## Create a data frame that holds alpha upperbounds through time
####################################################################
get_ub_alphas <- function(df, reporting_rate){
  find_upper_alpha <- function(alpha, likelihood, sig_level){
    ## df should have column with alpha and likelihood
    ## Doesn't work if everything is below the significance level.
    alpha[likelihood >= sig_level] %>% tail(1)
  }

  df %>% gather(date, likelihood, 2:ncol(df)) %>%
    mutate(date = ymd(date)) %>%
    group_by(date) %>%
    summarize(sig_0.05 = find_upper_alpha(alpha, likelihood, 0.05),
              sig_0.01 = find_upper_alpha(alpha, likelihood, 0.01))
}

load_return_df <- function(loc){
  load(loc)
  est_alphas_df
}

reporting_rates <- seq(0.1,1,by=.1)
# reporting_rates <- 1
# reporting_rates <- sprintf("%.1f", reporting_rates)
df_locs <- paste0("data_produced/alpha_likelihoods/alpha_like_rnot_dist_", reporting_rates, ".rda")

alpha_ubs <- df_locs %>% purrr::map(~load_return_df(.x)) %>%
  purrr::map(~get_ub_alphas(.x, )) %>%
  bind_rows() %>%
  mutate(reporting_rate = rep(reporting_rates, each = (ncol(load_return_df(df_locs[1]))-1) ))

alpha_ubs %>% ggplot(aes(date, sig_0.01, color = as.factor(reporting_rate))) + geom_line()

save(alpha_ubs, file="data_produced/alpha_likelihoods/alpha_uperbounds.rda")

####################################################################
## Create a data frame that holds the scaled R0 data through time
####################################################################
load("data_produced/county_r0_distributions.rda")

set.seed(12033)
sample_alphas <- function(alphas, likes){
  sample(x = alphas, size = 1000, replace = T, prob = likes)
}

scale_rnot_distribution <- function(rnot_dat, alpha_dat){
  ## Takes in a single row containing R0 distributions for a county in a specific month
  ## Returns a summarized dataframe for the R0s scaled for each date that data were estimated
  rnot_dat <- rnot_dat %>% gather(samp, rnot, 3:ncol(rnot_dat))
  n <- nrow(alpha_dat)/1000
  # browser()

  q_cols <- function(scaled_rnots){
    qs <- quantile(scaled_rnots, probs = c(0.005, 0.025, 0.5, 0.975, 0.995))
    data_frame(lower_r0 = qs[1],
               low_r0 = qs[2],
               med_r0 = qs[3],
               high_r0 = qs[4],
               higher_r0 = qs[5])
  }

  scaled <- bind_cols(c(alpha_dat, rnot_dat[rep(x = 1:1000, n),])) %>%
    mutate(scaled_rnot = alpha_samp*rnot) %>%
    group_by(county,month,date) %>%
    do(q_cols(.$scaled_rnot)) %>%
    rename(month_prediction = month, date_predicted = date)
}

get_reporting_rate <- function(loc){
  sub(strsplit(loc, split = "_")[[1]][length(strsplit(loc[1], split = "_")[[1]])], pattern = ".rda", replacement="")
}

get_scaled_dfs <- function(loc, county_r0_dist){
  est_alphas_df <- load_return_df(loc)
  alpha_dat <- est_alphas_df %>% gather(date, like, 2:ncol(est_alphas_df)) %>%
    group_by(date) %>%
    mutate(alpha_samp = sample_alphas(alpha, like)) %>%
    select(date, alpha_samp)

  r0_sum_df <- vector("list", nrow(county_r0_distributions))
  for(i in 1:nrow(county_r0_distributions)){
    r0_sum_df[[i]] <- scale_rnot_distribution(county_r0_distributions[i,], alpha_dat)
  }
  r0_scaled_df <- r0_sum_df %>% bind_rows() %>%
    mutate(reporting_rate = get_reporting_rate(loc))
  r0_scaled_df
}

r0_scaled_df <- df_locs %>% purrr::map(~get_scaled_dfs(.x, county_r0_distributions)) %>%
                            bind_rows()

save(r0_scaled_df, file = "data_produced/scaled_rnots.rda")









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

