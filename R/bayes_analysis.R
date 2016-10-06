############################
## Initial script to test out functions and plots
############################
rm(list=ls())
library(tidyverse)
library(cowplot)
library(coda)

sapply(c("R/fitting_fxns.R", "R/plotting_fxns.R", "R/load_data.R"), source)


test <- rnot_mcmc(parms = zika_parms(),
                  rand_init = T,
                  iters = 10000,
                  tuning = 0.05,
                  burnin=1000)
plot(test[[1]])
summary(test[[1]])
