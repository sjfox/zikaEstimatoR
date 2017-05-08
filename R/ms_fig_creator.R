##############################
## Plots for the Zika Paper
## Spencer Fox Jan 23, 2017
##############################
rm(list=ls())

library(cowplot)
library(tidyverse)
library(maps)
library(lubridate)
library(stringr)
library(scales)
sapply(c("R/fitting_fxns.R"), source)

min_date <- "2016-01-04"
max_date <- "2016-11-21"
date_range <- ymd( c(min_date,max_date))
###############################
## Conceptual figure 1
###############################

load("data_produced/fig1_data.rda")

## Expected secondary cases for Importation Month
exp_secondary_cases %>% filter(month=="Aug") %>%
  ggplot(aes(secondary_cases, probability)) +
  geom_bar(stat="identity", position="dodge") +
  coord_cartesian(expand=F) +
  theme(legend.position = "none", legend.title = element_blank()) +
  labs(x = "Average Secondary Cases", y = "Probability") -> secondary_dist_plot
secondary_dist_plot


fake_alpha_plot <- fake_alpha_dat  %>% filter(month =="Aug", month_scaled == "Aug", intros %in% c("10", "50")) %>%
  ggplot(aes(alpha, value, linetype = as.factor(intros))) +
  geom_line(size=1) +
  geom_hline(yintercept = 0.05, lty=3) +
  scale_color_brewer() +
  theme(legend.position = c(0.02, 0.2))+
  labs(x = "Alpha", y = "Likelihood") +
  guides(linetype=guide_legend(title="Importations"))
fake_alpha_plot

fake_alpha_dat %>% filter(intros %in% c(0, 10, 50)) %>%
  mutate(month_scaled = if_else(month_scaled=="Aug", "August", "October")) %>%
  ggplot(aes(scaled_rnot, fill = as.factor(intros), color = as.factor(intros))) +
  geom_density(alpha=0.6, size=1) +
  facet_wrap(~month_scaled, nrow = 1, scales = "free") +
  coord_cartesian(expand=F) +
  theme(legend.position = c(0.8,0.7), strip.background = element_rect(fill=NA),
        strip.text = element_text(size=18)) +
  labs(x = expression("R"[0]), y = "Density") +
  panel_border(colour="black")+
  guides(fill = guide_legend(title = "August\nImportations"), color = guide_legend(title = "August\nImportations"))+
  scale_fill_brewer(type = "qual", palette = 2)+
  scale_color_brewer(type = "qual", palette = 2) -> updated_rnot_plot
updated_rnot_plot

top_row <- plot_grid(secondary_dist_plot, fake_alpha_plot, labels = "AUTO")

fig1 <- plot_grid(top_row, updated_rnot_plot, nrow = 2, labels = c("", "C"))


save_plot(filename = "ms_figs/f1_scaling_example.png", plot = fig1, base_height = 6, base_aspect_ratio = 1.5)



##################################################################
## Final R0 estimates for the states plot by month -- Median
##################################################################
load("data_produced/scaled_rnots_quants_sectrans.rda")

scaled_rnot_temp <- r0_scaled_df %>% ungroup() %>%
                      filter(date_predicted == max(date_predicted), reporting_rate == 0.0574)

rnot_plot <- map_data(map = "county") %>% filter(region=="texas") %>%
  mutate(subregion = if_else(subregion=="de witt", "dewitt", subregion)) %>%
  left_join(scaled_rnot_temp, by=c("subregion" = "county"))  %>%
  mutate(month_prediction = factor(month_prediction, levels=month.abb)) %>%
  ggplot(aes(x=long, y=lat, fill = med_r0, group = subregion)) + facet_wrap(~month_prediction)+
    geom_polygon(color = "gray", size=0.1) +
    theme_nothing() +
    scale_fill_gradient(low="white", high="blue",
                        guide = guide_colorbar(title=expression("R"[0]), barheight=10))
rnot_plot


save_plot("ms_figs/f1_scaled_rnot_preds_ub.png", plot = rnot_plot, base_height = 5, base_aspect_ratio = 1.3)



###############################
## Monthly R0 for state - supplemental Figure
###############################
load("data_produced/calculated_tx_county_rnots.rda")
rnot_by_month <- map_data(map = "county") %>% filter(region=="texas") %>%
  mutate(subregion = if_else(subregion=="de witt", "dewitt", subregion)) %>%
  left_join(tx_county_rnots, by=c("subregion" = "county")) %>%
  # mutate(med_r0 = if_else(med_r0<0.01, 0.01, med_r0)) %>%
  ggplot(aes(x=long, y=lat, fill = med_r0, group = subregion)) + facet_wrap(~month)+
    geom_polygon(color = "gray", size=0.1) +
  scale_fill_gradientn(name = expression("R"[0]), na.value = "white",
                       colours = c("white", "blue", "yellow", "red"),
                       # breaks= c(0.1, 1, 10),
                       values = scales::rescale(c(0, 1, 1.000001, max(tx_county_rnots %>% select(med_r0)))),
                       guide = guide_colorbar(title=expression("R"[0]), barheight=10)) +
    theme_nothing()
rnot_by_month

save_plot("ms_figs/sf1_monthly_rnot_state.png", plot = rnot_by_month, base_height = 5, base_aspect_ratio = 1.3)

map_data(map = "county") %>% filter(region=="texas") %>%
  mutate(subregion = if_else(subregion=="de witt", "dewitt", subregion)) %>%
  left_join(tx_county_rnots, by=c("subregion" = "county")) %>%
  filter(month=="Jan") %>%
  ggplot(aes(x=long, y=lat, fill = mosquito.abundance, group = subregion)) + facet_wrap(~month)+
  geom_polygon(color = "gray", size=0.1) #+
  # scale_fill_continuous(trans="log10")

############################
# Statewide scaling alpha results - Figure
############################
load("data_produced/alpha_likelihoods/alpha_upperbounds.rda")

tx_imports <- read_csv("data/ZikaReportDates11032016.csv")



import_plot <- tx_imports %>% mutate(notification_date = mdy(`First Notification Date`)) %>%
  filter(notification_date<max_date) %>%
  mutate(month = as.character(month(notification_date, label=TRUE, abbr = T)),
         county = tolower(str_replace_all(County, " County", ""))) %>%
  select(county, notification_date, month) %>%
  group_by(notification_date) %>%
  summarise(num_imports = n()) %>%
  mutate(cum_imports = cumsum(num_imports)) %>%
  ggplot(aes(notification_date, num_imports)) +
    geom_bar(stat="identity", width=2) +
    labs(y="Importations", x = "Date") +
    scale_x_date(labels = date_format("%b"), limits = date_range, breaks=date_breaks("month"))+
    scale_y_continuous(expand=c(0,0))
import_plot

alpha_plot <- alpha_ubs %>% filter(reporting_rate %in% c("0.02","0.03", "0.05"), date <max_date) %>%
  ggplot(aes(date, sig_0.05)) +
    geom_point(aes(color = as.factor(reporting_rate)), size=2) +
    labs(y = "Scaling Factor Upperbound", x = "Date", color = "Reporting\nRate") +
    scale_color_brewer(palette ="YlGnBu", type = "seq") +
    theme(legend.position = c(0.2,0.5))+
    coord_cartesian(ylim=c(0,1)) +
    scale_x_date(labels = date_format("%b"), limits = date_range, breaks=date_breaks("month"))+
    guides(color = guide_legend(override.aes = list(size=5)))
alpha_plot

daily_alpha <- plot_grid(import_plot, alpha_plot, align="v", nrow=2, labels = "AUTO", rel_heights = c(1,1.3))

save_plot("ms_figs/f3_daily_alpha.png", daily_alpha, base_height = 6, base_aspect_ratio = 1.5)

############################
# Next fig should show the Cameron county R0 distribution
# Need to take scaling function and just give raw scaled results isntead of summaries
############################
load("data_produced/cameron_scaled_rnot_dist.rda")


cam_nov_plot <- cam_scaled_rnots %>% group_by(month(date_predicted, label=TRUE)) %>%
  filter(date_predicted==min(date_predicted), month(date_predicted, label=TRUE) %in% month.abb[6:11]) %>%
  mutate(county = if_else(county=="cameron", "Historic", "Actual")) %>%
  ggplot(aes(scaled_rnot, fill=county)) +
  facet_wrap(~month(date_predicted, label=T), scales="free_y") +
  geom_histogram(aes(y = ..density..), alpha=0.6, position="identity") +
  scale_fill_manual(values = c("black", "#bdbdbd")) +
  labs(x = expression("Scaled November Cameron R"[0])) +
  panel_border(colour="black")+
  theme(legend.position=c(0.8,0.85), legend.title = element_blank(), strip.background = element_rect(fill="NA"))
cam_nov_plot

save_plot("ms_figs/f4_cam_scaled_nov_rnot.png", cam_nov_plot, base_height = 4, base_aspect_ratio = 1.8)




############################
# Cameron County estimation plot
############################
# statewide_alpha_rnots %>%
#   filter(county=="cameron") %>%
#   ggplot(aes(info, med_r0*high)) + geom_point()+
#     geom_errorbar(aes(ymax = high_r0*high, ymin = low_r0*high)) +
#     scale_y_log10() +
#     geom_hline(yintercept=1, lty=2)
load("data_produced/alpha_likelihoods/alpha_like_rnot_dist_0.05.rda")
load("data_produced/scaled_rnots.rda")

county_r0_distributions %>% filter(county=="cameron", month =="Nov",) %>% as.numeric() %>% as_data_frame() %>%
  ggplot(aes(value))+geom_histogram() + labs(x="R0",title="November Predictions")


county_r0_distributions %>% filter(county=="cameron", month =="Nov") %>% as.numeric() %>% as_data_frame() %>%
  mutate(value = value*est_alphas_df$`2016-11-02`) %>%
  ggplot(aes(value))+geom_histogram() + labs(x="R0",title="November Predictions Scaled")

county_r0_distributions %>% filter(county=="cameron", month =="Oct") %>% as.numeric() %>% as_data_frame() %>%
  ggplot(aes(value))+geom_histogram() + labs(x="R0",title="2016 Nov Temp Predictions")


county_r0_distributions %>% filter(county=="cameron", month =="Oct") %>% as.numeric() %>% as_data_frame() %>%
  mutate(value = value*est_alphas_df$`2016-11-02`) %>%
  ggplot(aes(value))+geom_histogram() + labs(x="R0",title="2016 Nov Temp Predictions Scaled")




cameron_data <- statewide_alpha_rnots %>%
  filter(county=="cameron")

nov_data <- cameron_data %>% group_by(month) %>% filter(month=="Nov", info == min(info)) %>% ungroup() %>% select(low_r0:high_r0)
oct_data <- cameron_data %>% group_by(month) %>% filter(month=="Oct", info == min(info)) %>% ungroup() %>% select(low_r0:high_r0)

cam_nov_predict <- cameron_data %>% mutate(nov_med_r0 = nov_data$'med_r0',
                        nov_high_r0 = nov_data$'high_r0',
                        nov_low_r0 = nov_data$'low_r0',
                        oct_med_r0 = oct_data$'med_r0',
                        oct_high_r0 = oct_data$'high_r0',
                        oct_low_r0 = oct_data$'low_r0') %>%
  mutate_each(funs(`*`(., high) ), nov_med_r0:oct_low_r0)

oct_plot <- cam_nov_predict  %>%
            group_by(month) %>%
            filter(info == min(info))  %>%
            ggplot(aes(info, oct_med_r0)) + geom_point(size=2) +
              geom_errorbar(aes(ymax = oct_high_r0, ymin = oct_low_r0)) +
              coord_cartesian(ylim = c(0,4)) +
              geom_hline(yintercept=1, lty=2) +
  labs(x = "Month", y = expression("Predicted October R"[0]))

nov_plot <- cam_nov_predict  %>%
  group_by(month) %>%
  filter(info == min(info))  %>%
  ggplot(aes(info, nov_med_r0)) + geom_point(size=2) +
  geom_errorbar(aes(ymax = nov_high_r0, ymin = nov_low_r0)) +
  coord_cartesian(ylim = c(0,4)) +
  geom_hline(yintercept=1, lty=2) +
  labs(x = "Month", y = expression("Predicted November R"[0]))

cam_predicted_rnot <- plot_grid(oct_plot, nov_plot, labels = "AUTO")

save_plot("ms_figs/predicted_rnots_cameron.png", cam_predicted_rnot, base_height = 4, base_aspect_ratio = 2.3)


###########################
# Negative binomial updating R0 figure - supplemental Figure
###########################
load("data_produced/nb_fitod_estimates.rda")

nb_fitod_plot <- nb_fitod_estimates %>% filter(alphas==0.01) %>%
  mutate(high=ifelse(is.na(high), 10, high)) %>%
  ggplot(aes(intros, mle)) + geom_point(size=2) +
  geom_errorbar(aes(ymin=low, ymax=high))+
  geom_hline(yintercept=1, lty=2) +
  coord_cartesian(xlim= c(0,20)) +
  labs(x = "Number of Importations", y = expression("Estimated R"[0]))

save_plot("ms_figs/sf1_nbinom_update.png", nb_fitod_plot, base_height = 4, base_aspect_ratio = 1.8)


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
  labs(y = "Estimated Scaling Factor", x = "Date") +
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
  mutate(median_scaled = med_r0 * high, high_scaled = high_r0*high) %>%
  gather(key, value, med_r0, median_scaled, high_scaled) %>%
  filter(month %in% c("Jun", "Nov")) %>%
  mutate(value = if_else(value<0.001, 0, value),
         month = if_else(month=="Jun", "June", "November"),
         key = factor(key, levels = c("med_r0", "median_scaled", "high_scaled")))%>%
  ggplot(aes(x=long, y=lat, fill = value, group = subregion)) +
    geom_polygon(color = "gray", size=0.25) + facet_grid(month~key) +
    scale_fill_gradient2(na.value="black", low = "blue", mid = "white", high = "red", midpoint=0, trans="log10")+
    theme_nothing()+
    guides(fill = guide_colorbar(title=expression("R"[0])))
county_rnot_jun_nov

## Combine county scaled with alpha plot
fig4_county_alphas_rnots <- plot_grid(county_alpha_plot, county_rnot_jun_nov, rel_widths = c(1,2), labels = "AUTO", scale = c(1,1))

save_plot("ms_figs/fig4_county_alphas_rnots.png", fig4_county_alphas_rnots, base_height = 5, base_aspect_ratio = 3)




########################### Random things for wilke presentation
load("data_produced/county_r0_distributions.rda")
county_r0_distributions %>% filter(county=="cameron", month =="Aug") %>% as.numeric() %>%
  hist(main="", xlab = "R0")

load("data_produced/calculated_tx_county_rnots.rda")
map_data(map = "county") %>% filter(region=="texas") %>%
  mutate(subregion = if_else(subregion=="de witt", "dewitt", subregion)) %>%
  left_join(tx_county_rnots, by=c("subregion" = "county")) %>%
  filter(month=="Aug") %>% gather(key,value, low_r0:high_r0) %>%
  mutate(key = factor(key, levels = c("low_r0", "med_r0", "high_r0"))) %>%
  ggplot(aes(x=long, y=lat, fill = value, group = subregion)) + facet_wrap(~key)+
  geom_polygon(color = "gray", size=0.2) +
  scale_fill_gradientn(name = expression("R"[0]), na.value = "white",
                       colours = c("white", "blue", "yellow", "red"),
                       # breaks= c(0.1, 1, 10),
                       values = scales::rescale(c(0, 1, 1.000001, max(tx_county_rnots %>% select(high_r0)))),
                       guide = guide_colorbar(title=expression("R"[0]), barheight=10)) +
  theme_nothing() +theme(strip.background = element_blank(),
                         strip.text.x = element_blank())

hist(rnbinom(10000, mu = 2, size = 1), breaks = 30, right = T, main = "", xlab ="Secondary Cases")
plot(seq(2,0.25,length.out=1000), dnbinom(x = 0, mu = seq(2,0.25,length.out=1000), size = 1)^10, type = "l", xlim=c(2,0.2), ylab = "Likelihood", xlab = "R0")
abline(h = 0.05, lty=2)

tx_county_rnots %>% ggplot(aes(month, med_r0, group = county))+geom_line(alpha=0.5) + labs(y="Median R0")


###############################
# First test fitting a gamma R0
library(MASS)
aug_cam <- county_r0_distributions %>% filter(county=="cameron", month=="Jul")  %>% as.numeric()
est <- fitdistr(aug_cam[-c(1,2)], "gamma")
hist(aug_cam[-c(1,2)])
hist(rgamma(10000, shape = est$estimate["shape"], rate = est$estimate["rate"]))


####################################
## First conceptual figure 1 attempt
#####################################
# num_secondary <- 0:5
# like_data <- data_frame(secondary_cases = num_secondary,
#             low = dnbinom(x = num_secondary, mu = 0.5, size = find_overdispersion(0.5)),
#             mid = dnbinom(x = num_secondary, mu = 2, size = find_overdispersion(2)),
#             high = dnbinom(x = num_secondary, mu = 4, size = find_overdispersion(4))) %>%
#           gather(rnot_level, single_intro, low:high) %>%
#           mutate(rnot_level = case_when(.$rnot_level == "low" ~ "0.5",
#                                 .$rnot_level == "mid" ~ "2",
#                                 .$rnot_level == "high" ~ "4"))
#
# secondary_dist_plot <- like_data %>%
#   ggplot(aes(secondary_cases, single_intro, fill=rnot_level)) +
#     geom_bar(stat = "identity", position="dodge", color="black") +
#     geom_hline(yintercept=0.05, lty=2) +
#     scale_x_continuous(breaks = c(0:10)) +
#     scale_y_continuous(expand=c(0,0))+
#     theme(strip.background = element_rect(fill="white"))+
#     scale_fill_brewer(type = "qual", palette = 2) +
#     labs(x = "Number of Secondary Cases", y = "Likelihood")+
#     theme(legend.position = c(0.75,0.75))+
#     guides(fill = guide_legend(title=expression("R"[0]), keywidth = 1.5, keyheight=1.5, direction = "horizontal", label.position = "bottom", label.hjust = 0.5, title.vjust = .75))
# secondary_dist_plot
#
# prob_zero_plot <- like_data %>% filter(secondary_cases==0) %>%
#   mutate(three_intro = single_intro^3, ten_intro = single_intro^10) %>%
#   gather(intro_num, like, three_intro, ten_intro) %>%
#   mutate(mult_case_prob = 1 - like) %>%
#   gather(prob_type, value, like, mult_case_prob) %>%
#   mutate(prob_type = if_else(prob_type=="like", "0", ">0"),
#          intro_num = if_else(intro_num == "ten_intro", "10 Importations", "3 Importations"),
#          prob_type = factor(prob_type, levels = c("0", ">0")),
#          intro_num = factor(intro_num, levels = c("3 Importations", "10 Importations"))) %>%
#   ggplot(aes(prob_type, value, fill = rnot_level)) +
#     facet_wrap(~intro_num)+
#     geom_bar(stat = "identity", position="dodge", color="black") +
#     geom_hline(yintercept=0.05, lty=2) +
#     scale_y_continuous(expand=c(0,0))+
#     theme(strip.background = element_rect(fill="white"))+
#     scale_fill_brewer(type = "qual", palette = 2) +
#     labs(x = "Total Number of Secondary Cases", y = "Likelihood")+
#     theme(legend.position = "none")
# prob_zero_plot
#
# import_example_plot <- plot_grid(secondary_dist_plot, prob_zero_plot, rel_widths = c(1,1), labels = "AUTO")
#
# save_plot("ms_figs/likelihood_ex.png", import_example_plot, base_height = 4, base_aspect_ratio = 2)



