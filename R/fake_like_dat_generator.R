###############################################
## Generating Fake data for ms plots
###############################################
library(tidyverse)
sapply(c("R/fitting_fxns.R"), source)

set.seed(235443)

##############################################
## Panel A data
##############################################
load("data_produced/county_r0_distributions.rda")
load("data_produced/dispersion_df.rda")

avg_secondary_prob <- function(num_secondary, dist, dispersion_df){
  mean(dnbinom(x = num_secondary, mu = dist, size = find_rnot_ods(dist, dispersion_df)), na.rm=T)
}
months <- c("Aug", "Oct")
cam_rnot_dists <- county_r0_distributions %>% filter(county=="cameron", month %in% months) %>%
  gather(samp, rnot, V1:V1000) %>% mutate(month = factor(month, levels=months))


##############################################
## Panel B, C, & D data
##############################################

get_fake_parms <- function(dist, intros, reporting_rate){
  parms <- vector("list", length = length(intros)*length(reporting_rate))
  ii <- 1
  for(intro in intros){
    for(rate in reporting_rate){
      parms[[ii]] <- subs_parms(list(rnot = NA, rnot_dist = t(dist), num_intros = intro, distribution="nbinom", reporting_rate=rate), zika_parms())
      ii <- ii + 1
    }
  }
  parms
}
introductions <- c(10, 25, 50)
rates <- c(0.0574)
aug_parms <- cam_rnot_dists %>% filter(month =="Aug") %>% select(rnot) %>% get_fake_parms(intros = introductions, reporting_rate = rates)
# oct_parms <- cam_rnot_dists %>% filter(month =="Oct") %>% select(rnot) %>% get_fake_parms(intros = introductions, reporting_rate = rates)
# mar_parms <- cam_rnot_dists %>% filter(month =="Mar") %>% select(rnot) %>% get_fake_parms(intros = introductions, reporting_rate = rates)
parms <- c(aug_parms)

fake_alpha_likes <- parms %>% purrr::map(~get_alpha_likes_cpp(.x, dispersion_df))
fake_alpha_likes[2:length(fake_alpha_likes)] <-  fake_alpha_likes[2:length(fake_alpha_likes)] %>% purrr::map(function(x) as_data_frame(x[,2]))
fake_alpha_likes <- fake_alpha_likes %>% bind_cols()
colnames(fake_alpha_likes) <- c("alpha", as.character(interaction(rep(months[1], each=length(introductions)), introductions, sep="_")))
fake_alpha_likes <- fake_alpha_likes %>% gather(rates, value, 2:ncol(fake_alpha_likes)) %>%
  separate(rates, into = c("month", "intros"), sep="_") %>%
  mutate(intros = factor(intros, levels = introductions))


scale_fake_rnots <- function(alphas, alpha_likes, rnots){
  # browser()
  samp <- sample(x = alphas, size = 1000, replace = T, prob = alpha_likes)
  rnots*samp
}

get_rnots <- function(month_needed, rnot_dists){
  # browser()
  rnot_dists %>% filter(month==month_needed) %>%
    select(rnot) %>% unlist()
}

fake_alpha_likes <- fake_alpha_likes %>%
  group_by(intros, month) %>%
  mutate(Aug = get_rnots(unique(month), cam_rnot_dists),
         Oct = get_rnots(months[2], cam_rnot_dists)) %>%
  mutate(Aug = scale_fake_rnots(alphas=alpha, alpha_likes=value, rnots=Aug),
         Oct = scale_fake_rnots(alphas=alpha, alpha_likes=value, rnots=Oct)) %>%
  gather(month_scaled, scaled_rnot, Aug:Oct)

original_rnots <- cam_rnot_dists %>% mutate(alpha = NA, value = NA, month_scaled = month, scaled_rnot=rnot, intros="0") %>%
                    select(-samp, -county, -rnot)

fake_alpha_dat <- bind_rows(fake_alpha_likes, original_rnots) %>% ungroup() %>% mutate(intros = factor(intros, levels = c("0", introductions)))




# ##############################################
# ## Panel B data
# ##############################################
cam_rnot_dists %>% group_by(month) %>% summarise(`0` = avg_secondary_prob(0, rnot, dispersion_df),
                                                 `1` = avg_secondary_prob(1, rnot, dispersion_df),
                                                 `2` = avg_secondary_prob(2, rnot, dispersion_df),
                                                 `3` = avg_secondary_prob(3, rnot, dispersion_df),
                                                 `4` = avg_secondary_prob(4, rnot, dispersion_df),
                                                 `5` = avg_secondary_prob(5, rnot, dispersion_df)) %>%
  gather(secondary_cases, probability, `0`:`5`) %>%
  mutate(secondary_cases = as.numeric(secondary_cases)) -> exp_secondary_cases

save(fake_alpha_dat, exp_secondary_cases, file = "data_produced/fig1_data.rda")

