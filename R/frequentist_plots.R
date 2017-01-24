############################
## Initial script to test out functions and plots
############################
rm(list=ls())
library(tidyverse)
library(cowplot)
library(coda)

sapply(c("R/fitting_fxns.R", "R/plotting_fxns.R", "R/load_data.R"), source)

#######################################
## Plot Frequentist expection of maximum R0 for introduction numbers
#######################################


freq_rnot <- data.frame(intros=rep(seq(1, 25), times=3),
                        alphas = rep(c(0.5, 0.05, 0.01), each=25))

test_load <- try(load("data/freq_rnot_estimates.rda"))
if(class(test_load) == "try-error"){
  freq_rnot_estimates <- freq_rnot %>% rowwise() %>%
    do(data.frame(., get_rnot_ll_ci(.$alphas, .$intros, "pois")))
  devtools::use_data(freq_rnot_estimates, overwrite = TRUE)
}

freq_plot <- freq_rnot_estimates %>% filter(alphas==0.01) %>%
  ggplot(aes(intros, mle)) + geom_point() +
    geom_errorbar(aes(ymin=low, ymax=high))+
    coord_cartesian(xlim= c(0,15)) +
    theme(legend.position = c(0.8,0.8)) +
    geom_hline(yintercept=1, lty=2)+
    labs(x = "Number of Importations", y = expression("MLE and 99% CI for R"[0]))

save_plot(filename = "figs/pois_expected_rnot.pdf", plot = freq_plot, base_height = 4, base_aspect_ratio = 1.1)


###############################
## Plot for negative binomial sample
###############################
intros <- seq(0,100, by=5)
alphas <- c(0.01, 0.05)
overdisp <- c(0.01, 0.05, 0.1)
nb_freq_rnot <- as_data_frame(expand.grid(intros=intros, alphas=alphas, overdisp=overdisp))

test_load <- try(load("data/nb_freq_rnot_estimates.rda"))
if(class(test_load) == "try-error"){
  nb_freq_rnot_estimates <- nb_freq_rnot %>% rowwise() %>%
    do(data.frame(., get_rnot_ll_ci(.$alphas, .$intros, "nbinom", .$overdisp)))
  devtools::use_data(nb_freq_rnot_estimates, overwrite = T)
}

nb_freq_plot <- nb_freq_rnot_estimates %>% filter(alphas==0.01)%>%
  ggplot(aes(intros, mle)) + geom_point() + facet_grid(~overdisp) +
  geom_errorbar(aes(ymin=low, ymax=high))+
  coord_cartesian(xlim= c(0,100)) +
  labs(x = "Number of Introductions", y = "Median R0 and CI", linetype= "Confidence")

save_plot(filename = "figs/nb_freq_expected_rnot.pdf", plot = nb_freq_plot, base_height = 4, base_aspect_ratio = 1.8)


###############################
## Plot for negative binomial with fit overdispersion
###############################
intros <- seq(0,20, by=1)
alphas <- c(0.01, 0.05)

nb_fitod <- as_data_frame(expand.grid(intros=intros, alphas=alphas))

test_load <- try(load("data/nb_fitod_estimates.rda"))
if(class(test_load) == "try-error"){
  rnots <- seq(0, 10, length.out = 10000)
  ods <- unlist(purrr::map(rnots, ~find_overdispersion(.x)))
  nb_fitod_estimates <- nb_fitod %>% rowwise() %>%
    do(data.frame(., get_rnot_ll_ci(.$alphas, .$intros, "nbinom", overdispersion = ods, rnots=rnots)))
  devtools::use_data(nb_fitod_estimates, overwrite = T)
}

nb_fitod_plot <- nb_fitod_estimates %>% filter(alphas==0.01) %>%
  mutate(high=ifelse(is.na(high), 10, high)) %>%
  ggplot(aes(intros, mle)) + geom_point(size=2) +
  geom_errorbar(aes(ymin=low, ymax=high))+
  geom_hline(yintercept=1, lty=2) +
  coord_cartesian(xlim= c(0,20)) +
  labs(x = "Number of Importations", y = expression("MLE and 99% CI for R"[0]))#"MLE R0 and 99% CI")

plot(nb_fitod_plot)

save_plot(filename = "figs/nb_fitod_estimates.pdf", plot = nb_fitod_plot, base_height = 4, base_aspect_ratio = 1.8)


compare_dists_plot <- plot_grid(freq_plot, nb_fitod_plot, labels = "AUTO", align = "v")

save_plot("figs/compare_pois_fitnb.pdf", plot = compare_dists_plot, base_height = 4, base_aspect_ratio = 2.2)

