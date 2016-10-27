############################
## Initial script to test out functions and plots
############################
rm(list=ls())
library(tidyverse)
library(cowplot)
library(coda)

sapply(c("R/fitting_fxns.R", "R/plotting_fxns.R", "R/load_data.R"), source)

parms <- subs_parms(list(num_intros=3), zika_parms())

get_mcmc_summary <- function(parms, iters=10000, tuning=0.05, burnin=1000, thin=1) {
  mcmc_output <- rnot_mcmc(parms = parms,
                           rand_init = T,
                           iters = iters,
                           tuning = tuning,
                           burnin=burnin, thin=thin)

  sumstats <- summary(mcmc_output[[1]])$quantiles
  sumstats[1, ]

  temp_names <- c(names(unlist(parms)), c("q2.5", "q25", "q50", "q75", "q97.5"))
  df <- as.data.frame(matrix( c(unlist(parms), sumstats[1,]), nrow = 1), stringsAsFactors = F)
  colnames(df) <- temp_names

  df[temp_names[-5]] <- sapply(df[temp_names[-5]], as.numeric)
  df
}

get_all_mcmc_summary <- function(prior_rnots, prior_sds, num_intros,){
  df <- expand.grid(prior_rnot=prior_rnots, prior_sd=prior_sds, num_intros=num_intros, stringsAsFactors = F)

  df %>% rowwise() %>%
    do(rbind(get_mcmc_summary(subs_parms(list(prior_mu=.$prior_rnot, prior_sd=.$prior_sd, num_intros=.$num_intros), zika_parms()))))

}




test_load <- try(load("data/mcmc_rnot_estimates.rda"))
if(class(test_load) == "try-error"){
  prior_rnots <- c(0.5, 1.5, 2.5)
  prior_sds <- c(0.1, 0.25, 0.5, 1)
  num_intros <- c(seq(2, 20, by = 2), 50)

  mcmc_rnot_estimates <- get_all_mcmc_summary(prior_rnots = prior_rnots,
                                       prior_sds = prior_sds,
                                       num_intros = num_intros)
  devtools::use_data(mcmc_rnot_estimates)
}


mcmc_rnot_estimates %>% ggplot(aes(num_intros, q50)) +
  geom_point() + facet_grid(prior_mu~prior_sd, scales = "free_y") +
  panel_border() + geom_errorbar(aes(ymin=q2.5, ymax = q97.5))

