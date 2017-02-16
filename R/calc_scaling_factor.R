###################################
## Script for running analysis on texas county scaling likelihoods
###################################
rm(list=ls())
library(tidyverse)
library(cowplot)
library(stringr)
library(lubridate)

sapply(c("R/fitting_fxns.R", "R/load_data.R", "R/scaling_analysis_fxns.R"), source)


tx_data <- tx_imports %>% mutate(month = factor(month, levels = month.abb)) %>%
  left_join(y = tx_county_rnots, by = c("county", "month")) %>%
  mutate(month = factor(month, unique(month))) %>%
  rename(rnot=med_r0)

####################################
## Calculating statewide alpha from county importation data
####################################
## For every unique notification date, gather the imports and rnots summarized by month
daily_parms <- unique(tx_data$notification_date) %>%
  purrr::map(~get_alpha_parms(tx_data, curr_date=.x))

# get_alpha_ci(parms = daily_parms[[110]], sig_level = 0.05)

## Estimate alphas for each day that has an importation
library(profvis)
profvis({
  est_alphas <- purrr::map(daily_parms[1:2], get_alpha_ci, sig_level=0.05)
})
est_alphas <- purrr::map(daily_parms, get_alpha_ci, sig_level=0.01)


est_alphas_df <- est_alphas %>% bind_rows() %>%
                mutate(info = unique(tx_data$notification_date))

save(est_alphas_df, file = "data_produced/statewide_alphas_through_time.rda")


######################################
## Scaling County R0s by statewide alpha
######################################
load("data_produced/statewide_alphas_through_time.rda")
statewide_alpha_rnots <- est_alphas_df %>% mutate(month = lubridate::month(info, label=TRUE)) %>%
  left_join(tx_county_rnots, by = "month")
save(statewide_alpha_rnots, file = "data_produced/statewide_alphas_rnots.rda")


####################################
## Calculating county specific alpha scaling
####################################

get_county_parms <- function(tx_data, desired_county){
  ## Gets the parms for a specified county for calculating the alphas through time
  county_data <- tx_data %>% filter(county==desired_county)
  if(nrow(county_data)==0){
    stop("Misspecified county")
  }
  unique(county_data$notification_date) %>%
    purrr::map(~get_alpha_parms(county_data, curr_date=.x))
}

county_specific_parms <- unique(tx_data$county) %>% purrr::map(~get_county_parms(tx_data, desired_county=.x))
county_notification_dates <- purrr::map(county_specific_parms, extract_notification_dates)
county_est_alphas <- purrr::map(county_specific_parms, get_county_alphas, sig_level=0.01)

county_est_alphas_df <- county_est_alphas %>% purrr::map( bind_rows)

county_alphas <- data_frame(county = unique(tx_data$county)) %>%
  mutate(alphas = county_est_alphas_df, date = county_notification_dates) %>%
  unnest(alphas, date) %>%
  mutate(month = month(date, label = T)) %>%
  left_join(tx_county_rnots, by = c("county", "month"))

save(county_alphas, file="data_produced/county_alphas_rnots.rda")




















# quarterly_rnot_preds <- rnot_dat %>% gather(quarterly_r0_pred, q0_info, q1_rnot:q3_rnot) %>%
#   select(county, quarterly_r0_pred, q0_info) %>%
#   mutate(q1_info = q0_info * est_alphas$high[1],
#          q2_info = q0_info * est_alphas$high[2],
#          q3_info = q0_info * est_alphas$high[3]) %>%
#   gather(info, rnot, q0_info:q3_info) %>%
#   mutate(quarterly_r0_pred = str_replace_all(quarterly_r0_pred, pattern = "_rnot", ""))
#
# devtools::use_data(quarterly_rnot_preds)



##########################################
## Plot the results
# ##########################################
# load("data/quarter_rnot_preds.rda")
#
# tx_sp <- map_data(map = "county") %>% filter(region=="texas")
# tx_sp <- tx_sp %>% mutate(subregion = ifelse(subregion =="de witt", "dewitt", subregion))
#
# merged_preds <- quarter_rnot_preds %>% left_join(tx_sp, by=c("county"="subregion"))
#
# county_r0_info_plot <- merged_preds %>% ggplot(aes(x=long, y=lat, fill = rnot, group = county)) +
#   geom_polygon(color = "gray", size=0.25) + facet_grid(info~quarterly_r0_pred, switch="y") +
#   scale_fill_continuous(low = "white", high = "red")+
#   theme(axis.line=element_blank(),
#         axis.text.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks=element_blank(),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank())
#
# save_plot("figs/county_rnot_info_plot.pdf", county_r0_info_plot, base_height = 10, base_aspect_ratio = 0.9)
#
#
#
