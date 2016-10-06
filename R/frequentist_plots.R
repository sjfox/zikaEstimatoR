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


freq_rnot <- data.frame(intros=rep(seq(0, 24), times=2),
                        alphas = rep(c(0.05, 0.50), each=25))

test_load <- try(load("data/freq_rnot_estimates.rda"))
if(class(test_load) == "try-error"){
  freq_rnot_estimates <- freq_rnot %>% rowwise() %>%
    do(data.frame(., get_rnot_ll_ci(.$alphas, .$intros, "pois")))
  devtools::use_data(freq_rnot_estimates)
}

freq_plot <- freq_rnot_estimates %>%
  ggplot(aes(intros, high, linetype=as.factor(alphas))) + geom_line(size=1.5) +
    coord_cartesian(xlim= c(0,15)) +
    theme(legend.position = c(0.8,0.8)) +
    labs(x = "Number of Introductions", y = "Highest Predicted R0", linetype= "Confidence")

save_plot(filename = "figs/freq_expected_rnot.pdf", plot = freq_plot, base_height = 4, base_aspect_ratio = 1.1)















test <- rnot_mcmc(init_rnot = 0.5,
                  introductions = 1,
                  prior_mu = 2,
                  prior_sd = .1000,
                  iters = 10000,
                  tuning = 0.05,
                  burnin=1000)
plot(test[[1]])
summary(test[[1]])

dnorm(0.1, mean = 2, sd = 10000, log=T)
log(intro_like(rnot = .3, num_intros = 1, distribution = "pois"))
exp(-.2)
