###########################################
## Scaling Factor helper functions
##
###########################################


get_alpha_parms <- function(tx_data, curr_date){
  tx_data <- tx_data %>% filter( notification_date <= curr_date) %>%
    group_by(county, month) %>%
    summarize(imports = n(), rnot = unique(rnot))

  subs_parms(list(rnot = tx_data$rnot, num_intros = tx_data$imports, distribution="nbinom", date=curr_date), zika_parms())
}


get_alpha_parms_r0_dist <- function(tx_data, curr_date, county_r0_dists){
  tx_data <- tx_data %>% filter( notification_date <= curr_date) %>%
    group_by(county, month) %>%
    summarize(imports = n())

  rnot_data <- left_join(tx_data, county_r0_dists, by = c("county", "month")) %>%
    ungroup() %>%
    select(starts_with("V"))
  subs_parms(list(rnot = NA, rnot_dist = rnot_data, num_intros = tx_data$imports, distribution="nbinom", date=curr_date), zika_parms())
}



get_county_alphas <- function(county_daily_parms, sig_level = 0.01){
  ## Runs alpha estimationg for a single set of county parameters
  purrr::map(county_daily_parms, get_alpha_ci, sig_level)
}

extract_notification_dates <- function(x){
  # Extract the dates, and then combine into a vector for return
  purrr::map(x, function(y) y$date) %>%
    do.call(what = c, args = .)
}
