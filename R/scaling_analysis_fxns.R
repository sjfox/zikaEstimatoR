###########################################
## Scaling Factor helper functions
##
###########################################


get_alpha_parms <- function(tx_data, curr_date, reporting_rate){
  tx_data <- tx_data %>% filter( notification_date <= curr_date) %>%
    group_by(county, month) %>%
    summarize(imports = n(), rnot = unique(rnot))

  subs_parms(list(rnot = tx_data$rnot,
                  num_intros = tx_data$imports,
                  distribution="nbinom",
                  date=curr_date,
                  reporting_rate=reporting_rate,
                  secondary_trans = tx_data$sec_trans), zika_parms())
}


get_alpha_parms_r0_dist <- function(tx_data, curr_date, county_r0_dists, reporting_rate){
  tx_data <- tx_data %>% filter( notification_date <= curr_date) %>%
    group_by(county, month, sec_trans) %>%
    summarize(imports = n())

  rnot_data <- left_join(tx_data, county_r0_dists, by = c("county", "month")) %>%
    ungroup() %>%
    select(starts_with("V"))

  subs_parms(list(rnot = NA,
                  rnot_dist = rnot_data,
                  num_intros = tx_data$imports,
                  distribution="nbinom",
                  date=curr_date,
                  reporting_rate=reporting_rate,
                  secondary_trans = tx_data$sec_trans), zika_parms())
}

extract_notification_dates <- function(x){
  # Extract the dates, and then combine into a vector for return
  purrr::map(x, function(y) y$date) %>%
    do.call(what = c, args = .)
}


get_parms <- function(path){
  temp <- strsplit(path, split="/")[[1]]
  temp <- temp[length(temp)]
  temp <- strsplit(temp, split="_")[[1]]
  # browser()
  sec_trans <- as.numeric(temp[5])
  reporting_rate <- as.numeric(strsplit(temp[6], "\\.rda")[[1]][1])

  list(reporting_rate = reporting_rate, sec_trans=sec_trans)
}

get_ub_alphas <- function(loc){
  find_upper_alpha <- function(alpha, likelihood, sig_level){
    alphas <- sample(x = alpha, size = 500000, replace = T, prob = likelihood)
    quantile(x= alphas, probs = (1-sig_level) )
    # if(max(likelihood) < sig_level){
    #
    # } else{
    #   alpha[likelihood >= sig_level] %>% tail(1)
    # }
  }
  load(loc)
  parms <- get_parms(loc)
  est_alphas_df %>% gather(date, likelihood, 2:ncol(est_alphas_df)) %>%
    mutate(date = ymd(date)) %>%
    group_by(date) %>%
    summarize(sig_0.05 = find_upper_alpha(alpha, likelihood, 0.05),
              sig_0.01 = find_upper_alpha(alpha, likelihood, 0.01)) %>%
    mutate(reporting_rate = parms$reporting_rate,
           secondary_trans = parms$sec_trans)
}

sample_alphas <- function(alphas, likes){
  sample(x = alphas, size = 1000, replace = T, prob = likes)
}

scale_rnot_summary <- function(rnot_dat, alpha_dat){
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
    # prob_aboves <- 1 - ecdf(scaled_rnots)(c(0.5, 1))
    # data_frame(lower_r0 = prob_aboves[1],
    #            med_r0 = prob_aboves[2])
  }

  scaled <- bind_cols(c(alpha_dat, rnot_dat[rep(x = 1:1000, n),])) %>%
    mutate(scaled_rnot = alpha_samp*rnot) %>%
    group_by(county,month,date) %>%
    do(q_cols(.$scaled_rnot)) %>%
    rename(month_prediction = month, date_predicted = date)
}

get_scaled_df_summary <- function(loc, county_r0_dist){
  load(loc)
  alpha_dat <- est_alphas_df %>% gather(date, like, 2:ncol(est_alphas_df)) %>%
    filter(date == max(date)) %>%
    group_by(date) %>%
    mutate(alpha_samp = sample_alphas(alpha, like)) %>%
    select(date, alpha_samp)

  r0_sum_df <- vector("list", nrow(county_r0_dist))
  for(i in 1:nrow(county_r0_distributions)){
    r0_sum_df[[i]] <- scale_rnot_summary(county_r0_dist[i,], alpha_dat)
  }

  parms <- get_parms(loc)

  r0_scaled_df <- r0_sum_df %>% bind_rows() %>%
    mutate(reporting_rate = parms$reporting_rate,
           secondary_trans = parms$sec_trans)
  r0_scaled_df
}

