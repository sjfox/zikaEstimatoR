################################
# Script used to convert CDC Wonder temps
# to average monthly temps for each county in Texas
################################
library(tidyverse)
library(cowplot)
library(stringr)

temps <- read_tsv("data/weather/tx_county_temps.txt")
temps <- temps %>% filter(is.na(Notes)) %>%
          rename(avg_max_temp = `Avg Daily Max Air Temperature (C)`,
                 avg_min_temp=`Avg Daily Min Air Temperature (C)`) %>%
          mutate(avg_temp = (avg_max_temp + avg_min_temp) / 2,
                 county = tolower(str_replace_all(County, pattern = " County, TX", ""))) %>%
          select(county, month=Month, month_code=`Month Code`, avg_temp)

# temps %>% ggplot(aes(month_code, avg_temp, color=county)) +
#   geom_line() +
#   theme(legend.position = "none")




eip <- function(temp, tau=4.9, b0=2.9, bt=-0.08){
  mu <- exp(b0 + bt * temp)
  exp(mu + tau^(-1)/2)
  # exp(-tau * (log(t) - mu)^2 / 2 ) * (1 / t) * sqrt(tau / 2 / pi)
}

