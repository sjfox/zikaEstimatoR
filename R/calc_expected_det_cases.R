##################################
## Script to analyze rest of 2017 importation data to predict
## Number of expected detected cases
##################################
rm(list=ls())
library(tidyverse)
library(lubridate)
library(stringr)

tx_imports <- read_csv("data/Zika Disease Cases as of 09282017.csv")

## Only need to look at 201 importation cases
tx_imports <- tx_imports %>% mutate(notification_date = mdy(`First Notification Date`)) %>%
  mutate(month = as.character(month(notification_date, label=TRUE, abbr = T)),
         year = year(notification_date),
         county = tolower(str_replace_all(County, " County", ""))) %>%
  select(notification_date, county, month, year) %>%
  group_by(month, county, year) %>%
  summarise(num_imports = n()) %>%
  ungroup() %>%
  mutate(month= factor(month, levels = month.abb)) %>%
  filter(year==2017)


## Read in results estimated through 2016
load("data_produced/posterior_estimates/county_posterior_rnots_actual_1_false.rda")


## This function draws from the reporting rate distribution so each sample will have random reporting rate
draw_new_reporting_rate <- function(){
  reporting <- rnorm(n = 1, mean = 0.0574, sd = 0.0146)
  while(reporting<0){
    reporting <- rnorm(n = 1, mean = 0.0574, sd = 0.0146)
  }
  reporting
}



sim_outbreak <- function(R0, k=.12){
  ## Simulates outbreaks assuming a negative binomial distribution and branching process
  # if(R0 >1){
  #   stop("R0 > 1, so won't run because may cause infinite loop")
  # }
  currently_infected <- 1
  cases <- 1
  while(currently_infected > 0){
    new_infected <- rnbinom(currently_infected, mu = R0, size = k)
    currently_infected <- sum(new_infected)
    cases <-  cases + currently_infected
    if(cases > 100){
      break
    }
  }
  ## Subtract the imported cases
  cases - 1
}


single_samp <- function(rnots, num_imports, extra_imports = FALSE){
  ## Function generates a single simulated sample for the expected number
  ## of detected cases in a specific county-month
  ## rnots should be vector of rnot samples
  ## num_imports should be a single integer
  ## reportin_rate should be a single double

  ## First sample from the R0 and reporting rate distribution
  rr_samp <- draw_new_reporting_rate()
  if(extra_imports){
    num_imports <- round(num_imports / rr_samp)
  }
  rnot_samp <- sample(x = rnots, size = num_imports, replace = T)



  ## From those sampled values, sample secondary infections
  # browser()
  # sec_infections <- sum(rnbinom(n = num_imports, mu = rnot_samp, size = 0.12))
  sec_infections <- sum(map(rnot_samp, sim_outbreak, k=0.12) %>% flatten_dbl())
    # rnbinom(n = num_imports, mu = rnot_samp, size = 0.12))

  ## Now sample how many infections are detected
  rbinom(1, size = sec_infections, prob = rr_samp)
}


set.seed(101)
# Simulation steps:
n_iters <- nrow(tx_imports)
n_samples <- 10000
sample_exp_cases <- vector(mode = "list", length = n_iters*2)
for(extra_imports in c(TRUE, FALSE)){
  for(row in 1:nrow(tx_imports)){
    temp_df <- slice(tx_imports, rep(row,n_samples))

    rnot_samp <- est_posterior %>% filter(month==tx_imports$month[row],
                                          county == tx_imports$county[row],
                                          year==2016) %>%
      select(rnot_samp) %>% unlist() %>% as.numeric()

    samp_exp_cases_vec <- vector("numeric", n_samples)
    for(i in 1:n_samples){
      samp_exp_cases_vec[i] <- single_samp(rnot_samp, tx_imports$num_imports[row], extra_import=extra_imports)
    }
    temp_df$exp_case_samps <- samp_exp_cases_vec
    temp_df$extra_import <- extra_imports

    if(!extra_imports){
      sample_exp_cases[[row]] <- temp_df
    } else{
      sample_exp_cases[[nrow(tx_imports) + row]] <- temp_df
    }
  }
}

all_expected_cases <- sample_exp_cases %>%
  bind_rows() %>%
  mutate(index = rep(1:10000, 34)) %>%
  group_by(index, extra_import) %>%
  summarize(total_expected_cases = sum(exp_case_samps)) %>%
  ungroup()

save(file = "data_produced/all_expected_cases.rda", all_expected_cases)

# ## Combines the posterior rnots with the importation data to
# ## generate random samples from the negative binomial distribution for these months
# exp_case_dists <- est_posterior %>% mutate(year = as.numeric(year)) %>%
#   filter(year == 2016) %>%
#   left_join(tx_imports, by = c("county", "month")) %>%
#   filter(!is.na(num_imports)) %>%
#   mutate(index = rep(seq(1,10000), times = nrow(tx_imports))) %>%
#   rowwise() %>%
#   mutate(lb_exp_cases = sum(rnbinom(n = num_imports, mu = rnot_samp, size = 0.12)),
#          ub_exp_cases = sum(rnbinom(n = round(num_imports/0.0574), mu = rnot_samp, size = 0.12)))
#
#
#
# exp_case_dists %>%
#   group_by(index) %>%
#   summarize(lb_exp_cases = sum(lb_exp_cases)*0.0574,
#             ub_exp_cases = sum(ub_exp_cases)*0.0574) %>%
#   gather(key, value, lb_exp_cases:ub_exp_cases) %>%
#   ggplot(aes(value, fill = key)) + geom_histogram(position="identity", alpha=0.5, bins = 30)
#
################### Data from website
# end_feb <- c("Bexar - 1",
#              "Brazoria - 1",
#              "Cameron- 2",
#              "Lubbock - 1",
#              "Smith - 1")
#
# end_mar <- c("Bexar - 1",
#              "Brazoria - 1",
#              "Cameron - 2",
#              "Collin - 1",
#              "Dallas - 1",
#              "Denton - 1",
#              "Lubbock - 1",
#              "Smith - 1")
#
# end_apr <- c("Bexar - 1",
#              "Brazoria - 1",
#              "Brazos - 1",
#              "Cameron - 2",
#              "Collin - 1",
#              "Dallas - 1",
#              "Denton - 1",
#              "Lubbock - 1",
#              "Smith - 2")
# end_may <- c("Bexar - 1",
#              "Brazoria - 1",
#              "Brazos - 1",
#              "Cameron - 2",
#              "Collin - 1",
#              "Dallas - 1",
#              "Denton - 1",
#              "Harris - 1",
#              "Lubbock - 1",
#              "Smith - 2")
#
# end_jun <- c("Bexar - 1",
#              "Brazoria - 1",
#              "Brazos - 1",
#              "Cameron - 4",
#              "Collin - 1",
#              "Dallas - 1",
#              "Denton - 1",
#              "Harris - 1",
#              "Lubbock - 1",
#              "Smith - 2")
# end_jul <- c("Bexar - 2",
#              "Brazoria - 1",
#              "Brazos - 1",
#              "Cameron - 6",
#              "Collin - 1",
#              "Dallas - 1",
#              "Denton - 1",
#              "Harris - 3",
#              "Lubbock - 1",
#              "Smith - 2")
#
# end_aug <- c("Bexar - 2",
#              "Brazoria - 1",
#              "Brazos - 1",
#              "Cameron - 6",
#              "Collin - 1",
#              "Dallas - 2",
#              "Denton - 1",
#              "Harris - 5",
#              "Lubbock - 1",
#              "Smith - 2",
#              "Travis - 1")
# mar <- data_frame(month = rep("Mar", 1),
#                   county = c("Collin", "Dallas", "Denton"),
#                   cases = c(1, 1, 1))
#
# apr <- data_frame(month = rep("Apr", 1),
#                   county = c("Smith"),
#                   cases = c(1))
#
# may <- data_frame(month = rep("May", 1),
#                   county = c("Harris"),
#                   cases = c(1))
#
# jun <- data_frame(month = rep("Jun", 1),
#                   county = c("Cameron"),
#                   cases = c(2))
#
# jul <- data_frame(month = rep("Jul", 3),
#                   county = c("Bexar", "Cameron", "Harris"),
#                   cases = c(1, 2, 2))
#
# aug <- data_frame(month = rep("Aug", 3),
#                   county = c("Dallas", "Harris", "Travis"),
#                   cases = c(1, 2, 1))
#
# imports_new <- bind_rows(mar,apr,may,jun,jul,aug) %>% mutate(county = tolower(county)) %>%
#   rename(imported_cases=cases)
