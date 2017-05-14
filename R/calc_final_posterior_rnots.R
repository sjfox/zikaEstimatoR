#########################################
## Script to calculate final posterior Rnots
#########################################
rm(list=ls())

library(MASS)
library(tidyverse)
library(stringr)
library(lubridate)


base_url <- "zikaEstimatoR"
if(grepl('spencerfox', Sys.info()['login'])) setwd(file.path("~", "projects", base_url))
if(grepl('vagrant', Sys.info()['user'])) setwd( file.path("/vagrant", base_url) )
if(grepl('stampede', Sys.info()['nodename'])) setwd(file.path('/home1/02958/sjf826', base_url))
if(grepl('wrangler', Sys.info()['nodename'])) setwd(file.path('/home/02958/sjf826', base_url))

sapply(c("R/fitting_fxns.R", "R/scaling_analysis_fxns.R", "R/mcmc_sampling.R"), source)


######################################################
## Define parameters for run
set.seed(1023902)

include_trans <- 1
reporting_rate <- 0.0574

######################################################
## Load the importation data

tx_imports <- read_csv("data/Zika Disease Cases by Notification Date as of 030617.csv")
tx_imports <- tx_imports %>% mutate(notification_date = mdy(`First Notification Date`)) %>%
  arrange(notification_date)%>%
  mutate(month = as.character(month(notification_date, label=TRUE, abbr = T)),
         county = tolower(str_replace_all(County, " County", ""))) %>%
  select(county, notification_date, month)

######################################################
## Setup secondary transmission column

tx_imports$sec_trans <- 0
include_trans <- as.numeric(include_trans)
if(!is.na(include_trans)){
  tx_imports$sec_trans[which(tx_imports$notification_date== "2016-11-21" & tx_imports$county == "cameron")] <- include_trans
  tx_imports$sec_trans[which(tx_imports$notification_date== "2016-12-12" & tx_imports$county == "cameron")[1]] <- 1
}

######################################################
## Load prior county R0 distributions, and get parameters setup for posterior calculation

load("data_produced/county_r0_distributions.rda")
tx_data <- tx_imports  %>% mutate(month = factor(month, levels = month.abb))

## Only want the posterior for all the data we have
daily_parms <- unique(tx_data$notification_date)[length(unique(tx_data$notification_date))] %>%
  purrr::map(~get_alpha_parms_r0_dist_mcmc(tx_data, curr_date=.x, county_r0_dists = county_r0_distributions, reporting_rate=as.numeric(reporting_rate)))

load("data_produced/dispersion_df.rda")

## Get posterior distribution -- only returns posterior for county/months that had importations
est_posterior <- purrr::map(daily_parms, mcmc_zika_rnot,
                         alpha_tuning = .1,
                         rnot_tuning = .1,
                         disp_df = dispersion_df,
                         burnin = 100000,
                         N = 200000,
                         thin=10)

est_posterior <- est_posterior[[1]]$samples %>% as_data_frame() %>% select(-1)

colnames(est_posterior) <- c("alpha", daily_parms[[1]]$county_month)

# cameron months where importations happened are duplicated. in this case, remove the last instance of them
# Necessary to remove last instance, because first instance isn't subject to constraints of the importation
est_posterior <- est_posterior[,-which(duplicated(colnames(est_posterior), fromLast = TRUE))]

## Function for drawing samples and returning county R0 estimate
sample_rnot_alpha <- function(alphas, rnots, size_vec = 10000){
  sample(alphas,size = size_vec, replace = T) * sample(rnots, size=size_vec, replace = T)
}

## Now draw samples from the prior distribution and posterior alpha to fill in the county R0s that didn't experience importation
all_counties <- paste0(county_r0_distributions$county, "_", county_r0_distributions$month)
for(cty_mnth in all_counties){
  if(!cty_mnth %in% colnames(est_posterior)){
    ## For counties not already there, first extract the prior R0 samples
    county_rnots <- as.numeric(county_r0_distributions[which(cty_mnth==all_counties), -c(1,2)])

    ## Now sample rnot and alpha from distributions
    est_posterior[cty_mnth] <- sample_rnot_alpha(alphas = est_posterior$alpha, rnots = county_rnots)
  }
}

est_posterior <- est_posterior %>% gather(county, rnot_samp, 2:ncol(est_posterior)) %>%
  separate(col = county, c("county", "month"), sep="_")

save(est_posterior, file = paste0("data_produced/posterior_estimates/county_posterior_rnots_",
                                  ifelse(is.na(include_trans), 0, include_trans), "_", reporting_rate,".rda"))


#####################################################
## Generate posterior if using 2016 cameron temperatures
load("data_produced/calculated_cam_county_2016_rnots.rda")

## Substitute in 2016 temperature estimates for November
cam_2016_rnot$county <- "cameron"
cam_county_r0_dist <- county_r0_distributions
cam_county_r0_dist[which(cam_county_r0_dist$county == "cameron" & cam_county_r0_dist$month == "Nov"), ] <- cam_2016_rnot

daily_parms <- unique(tx_data$notification_date)[length(unique(tx_data$notification_date))] %>%
  purrr::map(~get_alpha_parms_r0_dist_mcmc(tx_data, curr_date=.x, county_r0_dists = cam_county_r0_dist, reporting_rate=as.numeric(reporting_rate)))

## Get posterior distribution -- only returns posterior for county/months that had importations
cam_est_posterior <- purrr::map(daily_parms, mcmc_zika_rnot,
                            alpha_tuning = .1,
                            rnot_tuning = .1,
                            disp_df = dispersion_df,
                            burnin = 100000,
                            N = 200000,
                            thin=10)

cam_est_posterior <- cam_est_posterior[[1]]$samples %>% as_data_frame() %>% select(-1)

colnames(cam_est_posterior) <- c("alpha", daily_parms[[1]]$county_month)

# cameron months where importations happened are duplicated. in this case, remove the last instance of them
# Necessary to remove last instance, because first instance isn't subject to constraints of the importation
cam_est_posterior <- cam_est_posterior[,-which(duplicated(colnames(cam_est_posterior), fromLast = TRUE))]

cam_est_posterior <- cam_est_posterior %>% gather(county, rnot_samp, 2:ncol(cam_est_posterior)) %>%
  separate(col = county, c("county", "month"), sep="_") %>%
  filter(county == "cameron", month == "Nov") %>%
  select(county,month, rnot_samp) %>%
  mutate(county = "cameron2016")

save(cam_est_posterior, file = paste0("data_produced/posterior_estimates/cam2016_posterior_rnots_",
                                      ifelse(is.na(include_trans), 0, include_trans), "_", reporting_rate,".rda"))

