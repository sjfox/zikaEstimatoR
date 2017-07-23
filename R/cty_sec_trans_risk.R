######################################
## Script getting the expected number of secondary cases for three different methods
## NEED TO FIX TO MAKE BETTER
#############
rm(list=ls())

library(tidyverse)
library(lubridate)
library(stringr)
source("R/fitting_fxns.R")
load("data_produced/posterior_estimates/county_posterior_rnots_1_0.0574.rda")
load("data_produced/dispersion_df.rda")
tx_imports <- read_csv("data/Zika Disease Cases by Notification Date as of 030617.csv")

tx_imports <- tx_imports %>% mutate(notification_date = mdy(`First Notification Date`)) %>%
  mutate(month = as.character(month(notification_date, label=TRUE, abbr = T)),
         county = tolower(str_replace_all(County, " County", ""))) %>%
  select(county, notification_date, month) %>%
  group_by(county,month) %>%
  summarise(total_imports = n()) %>%
  ungroup()

post_summary <- est_posterior %>% group_by(county, month) %>%
  summarise(rnot_samp = mean(rnot_samp, na.rm=T)) %>% ungroup()


group_df <- function(df, groups_used){
  ## groups_to_use should be character vector
  if(is_null(groups_used)){
    return(df)
  }
  df %>%
    group_by_(.dots = groups_used)
}

get_df_sample_by_group <- function(df){
  ## Function to take samples of a dataframe based on
  ## The county/month grouping of that data frame
  ## Number samples of each group is determined by the importation dataframe
  ## So group columns in df need to be same as import_df
  df %>% mutate(samples = map2(data, total_imports, sample_n, replace=T)) %>%
    unnest(samples)
}

get_n_samples_by_group <- function(df, import_df, group_vec, n){
  grouped_imports <- import_df %>%
    group_df(groups_used =group_vec) %>%
    summarise(total_imports = sum(total_imports, na.rm=TRUE))

  if(is_null(group_vec)){
    df <- df %>%
      nest(1:3) %>% mutate(total_imports = grouped_imports[1,]) %>%
      unnest(total_imports)
  }else{
    df <- df %>%
      group_df(groups_used = group_vec) %>%
      nest() %>%
      left_join(grouped_imports) %>%
      filter(!is.na(total_imports))
  }

  samps <- vector(mode = "list", length = n)
  for(i in 1:n){
    samps[[i]] <- get_df_sample_by_group(df)
  }
  samps
}


sample_df <- function(df, nsamples, n){
  # browser()
  samps <- vector(mode = "list", length = n)
  for(i in 1:n){
    samps[[i]] <- df %>% sample_n(size=nsamples, replace = T) %>%
                    mutate(samps = rnbinom(n=nsamples, mu = rnot_samp, size = ods)) %>%
                    select(samps)
  }
  samps
}

summary_rnot_vec <- function(l_samps){
  l_samps %>% purrr::map(~summarise(.data = .x, rnot_sum=sum(.x$rnot_samp))) %>%
    unlist() %>% as.numeric()
}

rename_cols <- function(df)  {
  colnames(df) = 1:ncol(df)
  df
}


ctymnth_group_imports <- group_df(tx_imports, groups_used = c("month","county")) %>%
  summarise(total_imports = sum(total_imports))

ctymnth_grouping <- est_posterior %>%
  group_by(month,county) %>%
  mutate(ods = find_rnot_ods(rnot_samp, dispersion_df)) %>%
  nest(rnot_samp, ods) %>%
  left_join(ctymnth_group_imports, by=c("county","month")) %>%
  filter(!is.na(total_imports)) %>%
  rowwise() %>%
  do(samples = sample_df(df = .$data, nsamples = .$total_imports, n = 1000))

samps2 <- ctymnth_grouping$samples %>% purrr::map(~bind_cols(.)) %>% purrr::map(~rename_cols(.)) %>%
  bind_rows()

samps2 <- as.numeric(colSums(samps2))

round(mean(samps2))

quantile(samps2, probs = c(0.025, 0.5, 0.975))
# 2.5%      50%    97.5%
# 37      67.10342 114

########-- The value above is the expected number of secondary cases from the model
#### It is used as the total number of opportunities for a detection in binomial distribution
####

exp_cases <- data_frame(reporting_rate = 0.0574,
           exp_sec_cases = c(no_group_mean_rnots, samps, samps2)*0.0574,
           granularity = rep(c("none", "month", "county_month"), each=10000))

save(exp_cases, file = "data_produced/exp_sec_cases.rda")


prob_sec <- est_posterior %>%
  summarise(med = quantile(prob_sec_trans, probs = 0.5),
            mean_prob = mean(prob_sec_trans, na.rm=T)) %>%
  mutate(month_num = match(month,month.abb))

save(prob_sec, file = "data_produced/posterior_prob_sec_trans.rda")


################# Getting the prob of secondary transmission data from r0 data
get_prob_sec_trans <- function(rnots, dispersion_df){
  ods <- find_rnot_ods(rnots, dispersion_df)
  1 - dnbinom(0, mu = rnots, size = ods) ## Probability of secondary transmission is 1 - probability of no transmission
}
est_posterior <- est_posterior %>%
                    group_by(county, month) %>%
                    mutate(prob_sec_trans = get_prob_sec_trans(rnot_samp, dispersion_df))

prob_sec <- est_posterior %>%
  summarise(med = quantile(prob_sec_trans, probs = 0.5),
            mean_prob = mean(prob_sec_trans, na.rm=T)) %>%
  mutate(month_num = match(month,month.abb))

save(prob_sec, file = "data_produced/posterior_prob_sec_trans.rda")



### NOT USED
# no_groups <- sample_df(est_posterior, 321, 10000)
# no_group_mean_rnots <- summary_rnot_vec(no_groups)
#
# mnth_group_imports <- group_df(tx_imports, groups_used = "month") %>%
#   summarise(total_imports = sum(total_imports))
# mnth_grouping <- est_posterior %>%
#   group_by(month) %>%
#   nest(rnot_samp) %>%
#   left_join(mnth_group_imports, by="month") %>%
#   rowwise() %>%
#   do(samples = sample_df(df = .$data, nsamples = .$total_imports, n = 10000))
#
# samps <- mnth_grouping$samples %>% purrr::map(~bind_cols(.)) %>% purrr::map(~rename_cols(.)) %>%
#   bind_rows()
#
# samps <- as.numeric(colSums(samps))
