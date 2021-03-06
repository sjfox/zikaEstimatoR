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




#################################################
## For posterior distributions

df_locs <- list.files("data_produced/posterior_estimates", pattern="alpha_mcmc_*", full.names = T)


get_ci_posterior_alpha <- function(path){
  load(path)
  parms <- get_parms(path)

  est_alphas_df %>% gather(date, alpha_samp, 1:ncol(est_alphas_df)) %>%
    group_by(date) %>%
    summarize(lowest = quantile(alpha_samp, 0.005),
              low = quantile(alpha_samp, 0.025),
              med = quantile(alpha_samp, 0.5),
              high = quantile(alpha_samp, 0.975),
              highest = quantile(alpha_samp, 0.995)) %>%
    mutate(reporting_rate = parms$reporting_rate,
           secondary_trans = parms$sec_trans)
}


post_alpha_ci <- df_locs %>% purrr::map(~get_ci_posterior_alpha(.x)) %>%
  bind_rows()


save(post_alpha_ci, file = "data_produced/post_alpha_ci.rda")



####################################################################
## Create a data frame that holds alpha upperbounds through time
## WORKS ON LIKELIHOODS - NOT USED ANYMORE
####################################################################
#
# # reporting_rates <- seq(0.1,1,by=.1)
# # reporting_rates <- c(0.02, 0.03, 0.05)
# # reporting_rates <- sprintf("%.1f", reporting_rates)
# # df_locs <- paste0("data_produced/alpha_likelihoods/alpha_like_rnot_dist_", reporting_rates, ".rda")
# df_locs <- list.files("data_produced/alpha_likelihoods", pattern="*.rda", full.names = T)
# # df_locs <- "data_produced/alpha_likelihoods/alpha_like_rnot_dist_0.0574sec_trans1.rda"
# # reporting_rates <- 0.0574
#
# alpha_ubs <- df_locs %>% purrr::map(~get_ub_alphas(.x)) %>%
#   bind_rows()
#
# alpha_ubs %>% ggplot(aes(date, sig_0.05, color = as.factor(reporting_rate))) + geom_line() + ylim(0,1) + facet_wrap(~as.factor(secondary_trans))
#
# save(alpha_ubs, file="data_produced/alpha_likelihoods/alpha_upperbounds.rda")
#
# ####################################################################
# ## Create a data frame that holds the scaled R0 data through time
# ####################################################################
# load("data_produced/county_r0_distributions.rda")
#
# set.seed(12033)
#
# r0_scaled_df <- df_locs %>% purrr::map(~get_scaled_df_summary(.x, county_r0_distributions)) %>%
#   bind_rows()
#
# save(r0_scaled_df, file = "data_produced/scaled_rnots_quants_sectrans.rda")
#
#
# ####################################
# ## Get Cameron county scaled distributions
# ####################################
# get_scaled_rnot_dist <-  function(rnot_dat, alpha_dat){
#   ## Takes in a single row containing R0 distributions for a county in a specific month
#   ## Returns a summarized dataframe for the R0s scaled for each date that data were estimated
#   rnot_dat <- rnot_dat %>% gather(samp, rnot, 3:ncol(rnot_dat))
#   n <- nrow(alpha_dat)/1000
#   # browser()
#
#   scaled <- bind_cols(c(alpha_dat, rnot_dat[rep(x = 1:1000, n),])) %>%
#     mutate(scaled_rnot = alpha_samp*rnot) %>%
#     rename(month_prediction = month, date_predicted = date)
# }
#
# mult_alpha_scaled<- function(loc, county_r0_dist){
#   est_alphas_df <- load_return_df(loc)
#   alpha_dat <- est_alphas_df %>% gather(date, like, 2:ncol(est_alphas_df)) %>%
#     group_by(date) %>%
#     mutate(alpha_samp = sample_alphas(alpha, like)) %>%
#     select(date, alpha_samp)
#
#   r0_sum_df <- vector("list", nrow(county_r0_dist))
#   for(i in 1:nrow(county_r0_dist)){
#     r0_sum_df[[i]] <- get_scaled_rnot_dist(county_r0_dist[i,], alpha_dat)
#   }
#   # browser()
#   r0_scaled_df <- r0_sum_df %>% bind_rows() %>%
#     mutate(reporting_rate = get_reporting_rate(loc))
#   r0_scaled_df
# }
#
#
# load("data_produced/calculated_cam_county_2016_rnots.rda")
# load("data_produced/county_r0_distributions.rda")
#
# set.seed(1000001)
# cam_hist_rnot <- county_r0_distributions %>% filter(county=="cameron", month %in% c("Nov"))
# cam_rnots <- bind_rows(cam_2016_rnot, cam_hist_rnot)
# cam_scaled_rnots <- mult_alpha_scaled(df_locs[3], cam_rnots)
#
# save(cam_scaled_rnots, file = "data_produced/cameron_scaled_rnot_dist.rda")
#
# temp %>% group_by(month(date_predicted)) %>% filter(date_predicted==min(date_predicted)) %>%
#   ggplot(aes(scaled_rnot, fill=interaction(county,month_prediction))) +
#   facet_wrap(~month(date_predicted)) +
#   geom_histogram(aes(y = ..density..), alpha=0.6, position="identity")
#
# #
#
#
#
#
#
# # ####################################
# # ## Calculating statewide alpha from county importation data
# # ####################################
# # ## For every unique notification date, gather the imports and rnots summarized by month
# # daily_parms <- unique(tx_data$notification_date) %>%
# #   purrr::map(~get_alpha_parms(tx_data, curr_date=.x))
# #
# # # get_alpha_ci(parms = daily_parms[[110]], sig_level = 0.05)
# #
# # load("data_produced/dispersion_dt.rda")
# # est_alphas <- purrr::map(daily_parms[1:2], get_alpha_ci, disp_dt = dispersion_dt,sig_level=0.05)
# # ## Estimate alphas for each day that has an importation
# # library(profvis)
# # profvis({
# #   est_alphas <- purrr::map(daily_parms[1:2], get_alpha_likes, disp_dt = dispersion_dt)
# # })
# # est_alphas <- purrr::map(daily_parms, get_alpha_ci, sig_level=0.01)
# #
# #
# # est_alphas_df <- est_alphas %>% bind_rows() %>%
# #                 mutate(info = unique(tx_data$notification_date))
# #
# # save(est_alphas_df, file = "data_produced/statewide_alphas_through_time.rda")
# #
# #
#
# # ######################################
# # ## Scaling County R0s by statewide alpha
# # ######################################
# # load("data_produced/statewide_alphas_through_time.rda")
# # statewide_alpha_rnots <- est_alphas_df %>% mutate(month = lubridate::month(info, label=TRUE)) %>%
# #   left_join(tx_county_rnots, by = "month")
# # save(statewide_alpha_rnots, file = "data_produced/statewide_alphas_rnots.rda")
# #
# #
# # ####################################
# # ## Calculating county specific alpha scaling
# # ####################################
# #
# # get_county_parms <- function(tx_data, desired_county){
# #   ## Gets the parms for a specified county for calculating the alphas through time
# #   county_data <- tx_data %>% filter(county==desired_county)
# #   if(nrow(county_data)==0){
# #     stop("Misspecified county")
# #   }
# #   unique(county_data$notification_date) %>%
# #     purrr::map(~get_alpha_parms(county_data, curr_date=.x))
# # }
# #
# # county_specific_parms <- unique(tx_data$county) %>% purrr::map(~get_county_parms(tx_data, desired_county=.x))
# # county_notification_dates <- purrr::map(county_specific_parms, extract_notification_dates)
# # county_est_alphas <- purrr::map(county_specific_parms, get_county_alphas, sig_level=0.01)
# #
# # county_est_alphas_df <- county_est_alphas %>% purrr::map( bind_rows)
# #
# # county_alphas <- data_frame(county = unique(tx_data$county)) %>%
# #   mutate(alphas = county_est_alphas_df, date = county_notification_dates) %>%
# #   unnest(alphas, date) %>%
# #   mutate(month = month(date, label = T)) %>%
# #   left_join(tx_county_rnots, by = c("county", "month"))
# #
# # save(county_alphas, file="data_produced/county_alphas_rnots.rda")
#
