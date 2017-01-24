#################################################
## Loads in the data used for analysis
#################################################

########################################
## Importation Data
########################################

require(lubridate)
tx_imports <- read_csv("data/ZikaReportDates11032016.csv")

tx_imports <- tx_imports %>% mutate(notification_date = mdy(`First Notification Date`)) %>%
  mutate(month = as.character(month(notification_date, label=TRUE, abbr = T)),
         county = tolower(str_replace_all(County, " County", ""))) %>%
  select(county, notification_date, month)


########################################
## Temperature Data
########################################

tx_temps <- read_tsv("data/tx_county_temps.txt")
tx_temps <- tx_temps %>% filter(is.na(Notes)) %>%
  rename(avg_max_temp = `Avg Daily Max Air Temperature (C)`,
         avg_min_temp=`Avg Daily Min Air Temperature (C)`) %>%
  mutate(avg_temp = (avg_max_temp + avg_min_temp) / 2,
         county = tolower(str_replace_all(County, pattern = " County, TX", ""))) %>%
  arrange(`Month Code`) %>%
  mutate(Month = factor(Month, levels = unique(Month))) %>%
  select(county, month=Month, avg_temp) %>%
  spread(key = month, value = avg_temp)


########################################
## Perkins Sim data for R0 calculation
########################################
load("data/functions_R0_AR_random_draws.RData")


########################################
## Texas county information for R0 calculation
########################################
tx_county <- read_csv(file = "data/county_r0_parameters.csv")



