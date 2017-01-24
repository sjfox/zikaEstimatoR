###########################################
## Scaling Factor helper functions
##
###########################################


get_alpha_parms <- function(tx_data, curr_date){
  # must have ordered month in tx_data
  # curr_month = as.numeric(curr_month)
  # browser()
  tx_data <- tx_data %>% filter( notification_date <= curr_date) %>%
    group_by(county, month) %>%
    summarize(imports = n(), rnot = unique(rnot))

  subs_parms(list(rnot = tx_data$rnot, num_intros = tx_data$imports, distribution="nbinom", date=curr_date), zika_parms())
}

# scale_rnot <- function(rnot_dat, alpha_df){
#   rnot_dat[, as.character(alpha_df$info)] <- data.frame(map(alpha_df$high, ~.x*rnot_dat$rnot))
#
#   rnot_dat %>% select(-(county:rnot))
# }

get_county_alphas <- function(county_daily_parms){
  ## Runs alpha estimationg for a single set of county parameters
  purrr::map(county_daily_parms, get_alpha_ci)
}

extract_notification_dates <- function(x){
  # Extract the dates, and then combine into a vector for return
  purrr::map(x, function(y) y$date) %>%
    do.call(what = c, args = .)
}
