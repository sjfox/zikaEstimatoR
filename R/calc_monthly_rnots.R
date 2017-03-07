## Generate R0s
rm(list=ls())

library(tidyverse)
library(stringr)

sapply(c("R/fitting_fxns.R", "R/r0_calc_fxns.R"), source)

########################################
## Texas county information for R0 calculation
########################################
tx_county <- read_csv(file = "data/county_r0_parameters.csv")

########################################
## Temperature Data
########################################

tx_temps <- read_csv("data/tx_county_temps.csv")
tx_temps <- tx_temps %>% mutate(month = factor(month, levels = month.abb)) %>%
  rename(county = subregion) %>%
  mutate(county = if_else(county=="de witt", "dewitt", county)) %>%
  spread(key = month, value = avg_temp)

########################################
## Perkins R0 functions with h=1
########################################

load("data_produced/vector_suitability/parms_fxns_r0.RData")


tx_county <- tx_county %>%
  left_join(tx_temps, by=c("county"))

tx_county <- tx_county %>% gather(key = "month", value = "avg_temperature", Jan:Dec)

## Only needed if never run before (next load fails)
county_r0_distributions <- rnot_calc_dist(tx_county$mosquito.abundance,
                 tx_county$gdp,
                 tx_county$avg_temperature,
                 a=a, b=b, c.r=c.r,
                 mort.fun.list=mort.fun,
                 eip.fun.list=eip.fun,
                 scam.est.list=scam.est.list)

county_r0_distributions <- county_r0_distributions %>% mutate(county = tx_county[["county"]], month=tx_county[["month"]]) %>%
  select(county, month, everything())

save(county_r0_distributions, file = "data_produced/county_r0_distributions.rda")


load("data_produced/county_r0_distributions.rda")
tx_county <- tx_county %>%
                mutate(low_r0 = apply(county_r0_distributions[,-(1:2)], 1, quantile, probs=c(0.025)),
                       med_r0 = apply(county_r0_distributions[,-(1:2)], 1, quantile, probs=c(0.5)),
                       high_r0 = apply(county_r0_distributions[,-(1:2)], 1, quantile, probs=c(0.975)))


tx_county_rnots <- tx_county %>% mutate(month = factor(month, levels = month.abb))

save(tx_county_rnots, file = "data_produced/calculated_tx_county_rnots.rda")


