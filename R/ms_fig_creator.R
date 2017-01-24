##############################
## Plots for the Zika Paper
## Spencer Fox Jan 23, 2017
##############################
rm(list=ls())
library(cowplot)
library(tidyverse)
# library(stringr)
source("R/load_data.R")

###############################
## August R0 estimates for the states plot
###############################


load("data_produced/calculated_tx_county_rnots.rda")

aug_data <- tx_county_rnots %>% gather(rnot, value, low_r0:high_r0) %>%
  filter(month == "Aug")

rnot_plot <- map_data(map = "county") %>% filter(region=="texas") %>%
  mutate(subregion = if_else(subregion=="de witt", "dewitt", subregion)) %>%
  left_join(aug_data, by=c("subregion" = "county")) %>%
  mutate(rnot = case_when(.$rnot == "low_r0" ~ "Low", .$rnot == "med_r0" ~ "Median", .$rnot == "high_r0" ~ "High")) %>%
  mutate(rnot = factor(rnot, levels = c("Low", "Median", "High")),
         value = if_else(value<0.01, 0.0, value)) %>%
  ggplot(aes(x=long, y=lat, fill = value, group = subregion)) + facet_wrap(~rnot)+
    geom_polygon(color = "gray", size=0.1) +
    scale_fill_gradient2(na.value="black", low = "blue", mid = "white", high = "red", trans="log10", midpoint=0) +
    guides(fill = guide_colorbar(title=expression("R"[0]))) +
    theme_nothing()

save_plot("figs/august_rnot_distribution.pdf", plot = rnot_plot, base_height = 3, base_aspect_ratio = 3)

###############################
## Monthly R0 for state
###############################
rnot_by_month <- map_data(map = "county") %>% filter(region=="texas") %>%
  mutate(subregion = if_else(subregion=="de witt", "dewitt", subregion)) %>%
  left_join(tx_county_rnots, by=c("subregion" = "county")) %>%
  mutate(med_r0 = if_else(med_r0<0.01, 0.01, med_r0)) %>%
  ggplot(aes(x=long, y=lat, fill = med_r0, group = subregion)) + facet_wrap(~month)+
    geom_polygon(color = "gray", size=0.1) +
    scale_fill_gradient2(na.value="white",trans = "log10", low = "blue", mid = "white", high = "red", midpoint=0) +
    guides(fill = guide_colorbar(title=expression("R"[0]))) +
    theme_nothing()
rnot_by_month

save_plot("figs/monthly_rnot_state.pdf", plot = rnot_by_month, base_height = 5, base_aspect_ratio = 1.3)


###########################
# Negative binomial updating R0 figure
###########################
load("data_produced/nb_fitod_estimates.rda")

nb_fitod_plot <- nb_fitod_estimates %>% filter(alphas==0.01) %>%
  mutate(high=ifelse(is.na(high), 10, high)) %>%
  ggplot(aes(intros, mle)) + geom_point(size=2) +
  geom_errorbar(aes(ymin=low, ymax=high))+
  geom_hline(yintercept=1, lty=2) +
  coord_cartesian(xlim= c(0,20)) +
  labs(x = "Number of Importations", y = expression("Estimated R"[0]))

save_plot("figs/nbinom_rnot_updating.pdf", nb_fitod_plot, base_height = 4, base_aspect_ratio = 1.8)


############################
# Statewide scaling alpha results
############################
load("data_produced/statewide_alphas_through_time.rda")

alpha_plot <- est_alphas_df %>% ggplot(aes(info, mle)) +
  geom_point(size=1.5) +
  geom_errorbar(aes(ymin=low, ymax=high)) +
  labs(y = "Estimated Alpha", x = "Date")
alpha_plot
save_plot("figs/daily_alpha.pdf", alpha_plot, base_height = 4, base_aspect_ratio = 1.5)

############################
# Statewide scaling R0 results plot
############################

load("data_produced/statewide_alphas_rnots.rda")

## Only June and November plots (for combining into ms fig)
scaled_jun_nov_rnot_plot <- map_data(map = "county") %>% filter(region=="texas") %>%
  mutate(subregion = if_else(subregion=="de witt", "dewitt", subregion)) %>%
  left_join(statewide_alpha_rnots, by=c("subregion" = "county")) %>%
  group_by(month) %>%
  filter(info==min(info), month %in% c("Jun", "Nov")) %>%
  ungroup() %>%
  mutate(value = med_r0 * high) %>%
  mutate(value = if_else(value<0.01, 0, value)) %>%
  mutate(month = if_else(month=="Jun", "June", "November"))%>%
  ggplot(aes(x=long, y=lat, fill = value, group = subregion)) + facet_wrap(~month)+
  geom_polygon(color = "gray", size=0.1) +
  scale_fill_gradient2(na.value="white", low = "blue", mid = "white", high = "red", midpoint=1)+
  theme_nothing() + theme(legend.position=c(0.55,0.2))+
  guides(fill = guide_colorbar(title=expression("Scaled R"[0]), direction = "horizontal", title.position = "top", title.hjust = 0.5))
scaled_jun_nov_rnot_plot

## Combine with alpha plot above
fig3_statewide_alphas_rnots <- plot_grid(alpha_plot, scaled_jun_nov_rnot_plot, rel_widths = c(1,2), labels = "AUTO")

save_plot("ms_figs/fig3_statewide_alphas_rnots.pdf", fig3_statewide_alphas_rnots, base_height = 4, base_aspect_ratio = 3)


## For all of the months
statewide_month_scaled_plot <- map_data(map = "county") %>% filter(region=="texas") %>%
  mutate(subregion = if_else(subregion=="de witt", "dewitt", subregion)) %>%
  left_join(statewide_alpha_rnots, by=c("subregion" = "county")) %>%
  group_by(month) %>%
  filter(info==min(info)) %>%
  mutate(value = med_r0 * high) %>%
  mutate(value = if_else(value<0.01, 0, value)) %>%
  ggplot(aes(x=long, y=lat, fill = value, group = subregion)) + facet_wrap(~month)+
  geom_polygon(color = "gray", size=0.25) +
  scale_fill_gradient2(na.value="white", low = "blue", mid = "white", high = "red", midpoint=1)+
  theme_nothing()

save_plot("figs/statewide_month_scaled_plot.pdf", statewide_month_scaled_plot, base_height = 6, base_aspect_ratio = 1.3)



############################
# Individual county scaling alpha plot
############################

load("data_produced/county_alphas_rnots.rda")

## Select counties with highest importations and cameron county where loca transmission occurred
import_counties <- tx_imports %>% group_by(county) %>%
  summarize(imports = n()) %>% arrange(-imports) %>%
  head(8) %>% select(county) %>% add_row(county="cameron")

county_alpha_plot <- county_alphas %>% filter(county %in% import_counties$county) %>%
  ggplot(aes(date, mle)) + geom_point() + facet_wrap(~county) +
  geom_errorbar(aes(ymin=low, ymax=high)) +
  labs(y = "Estimated Alpha", x = "Date") +
  panel_border()

county_alpha_plot

save_plot("figs/daily_county_alpha.pdf", county_alpha_plot, base_height = 7, base_aspect_ratio = 1.3)


############################
# County alpha scaling R0 plot
############################
date_seq <- seq(from = min(county_alphas$date), to = max(county_alphas$date), by = "1 day")

county_alpha_rnot <- crossing(date=date_seq, county=county_alphas$county) %>% left_join(county_alphas, by=c("date", "county")) %>%
  group_by(county) %>%
  fill(high) %>%
  mutate(month = month(date, label=T)) %>%
  select(date, county, high, month) %>%
  group_by(month) %>%
  filter(date == min(date)) %>%
  right_join(tx_county_rnots, c("county", "month")) %>%
  mutate(high = if_else(is.na(high), 1, high))


county_rnot_jun_nov <- map_data(map = "county") %>% filter(region=="texas") %>%
  mutate(subregion = if_else(subregion=="de witt", "dewitt", subregion)) %>%
  left_join(county_alpha_rnot, by=c("subregion" = "county")) %>%
  mutate(value = med_r0 * high, value = if_else(value <0.01, 0.01, value)) %>%
  filter(month %in% c("Jun", "Nov")) %>%
  ggplot(aes(x=long, y=lat, fill = value, group = subregion)) +
  geom_polygon(color = "gray", size=0.25) + facet_wrap(~month) +
  scale_fill_gradient2(na.value="white", low = "blue", mid = "white", high = "red", midpoint=0, trans="log10")+
  theme_nothing()
county_rnot_jun_nov

save_plot("figs/november_county_scaled_rnot.pdf", county_rnot_max_date, base_height = 5, base_aspect_ratio = 1.3)

