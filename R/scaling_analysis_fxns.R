###########################################
## Scaling Factor helper functions
##
###########################################


get_alpha_parms <- function(tx_data, curr_date, reporting_rate){
  tx_data <- tx_data %>% filter( notification_date <= curr_date) %>%
    group_by(county, month) %>%
    summarize(imports = n(), rnot = unique(rnot))

  subs_parms(list(rnot = tx_data$rnot, num_intros = tx_data$imports, distribution="nbinom", date=curr_date, reporting_rate=reporting_rate), zika_parms())
}


get_alpha_parms_r0_dist <- function(tx_data, curr_date, county_r0_dists, reporting_rate){
  tx_data <- tx_data %>% filter( notification_date <= curr_date) %>%
    group_by(county, month) %>%
    summarize(imports = n())

  rnot_data <- left_join(tx_data, county_r0_dists, by = c("county", "month")) %>%
    ungroup() %>%
    select(starts_with("V"))
  subs_parms(list(rnot = NA, rnot_dist = rnot_data, num_intros = tx_data$imports, distribution="nbinom", date=curr_date, reporting_rate=reporting_rate), zika_parms())
}


get_alpha_ubs <- function(est_alpha_likes, sig_level){
  ## Function that returns upperbound estimates for alpha for all dates based on
  ## their estimated likelihoods
  browser()

  get_alpha_ub <- function(alpha, like, sig_level){
    alpha[rev(which(like > sig_level))[1]]
  }

  est_alpha_likes %>% gather(date, like, 2:ncol(est_alpha_likes)) %>%
    group_by(date) %>%
    summarise(alpha_ub = get_alpha_ub(alpha, like, sig_level))
}

# get_county_alphas <- function(county_daily_parms, sig_level = 0.01){
#   ## Runs alpha estimationg for a single set of county parameters
#   purrr::map(county_daily_parms, get_alpha_ci, sig_level)
# }

extract_notification_dates <- function(x){
  # Extract the dates, and then combine into a vector for return
  purrr::map(x, function(y) y$date) %>%
    do.call(what = c, args = .)
}
