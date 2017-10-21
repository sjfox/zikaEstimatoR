library(tidyverse)
library(cowplot)
library(Rcpp)

##################################################
## Simulation functions
##################################################
sim_outbreak <- function(R0, k=.12){
  ## Simulates outbreaks assuming a negative binomial distribution and branching process
  if(R0 >1){
    stop("R0 > 1, so won't run because may cause infinite loop")
  }
  currently_infected <- 1
  cases <- 1
  while(currently_infected > 0){
    new_infected <- rnbinom(currently_infected, mu = R0, size = k)
    currently_infected <- sum(new_infected)
    cases <-  cases + currently_infected
  }
  cases
}

sim_detection <- function(cases_vec, reporting_rate){
  ## Function to simulate the detection process on a vector from the simulated outbreaks
  rbinom(n = length(cases_vec), size = cases_vec, prob = reporting_rate)
}

sim_known_import_detection <- function(cases_vec, reporting_rate){
  ## Function to simulate the detection process on a vector from the simulated outbreaks
  rbinom(n = length(cases_vec), size = cases_vec - 1, prob = reporting_rate) + 1
}



##################################################
## Analytical Calculation functions
##################################################

prob_outbreak_perf_det <- function(R0, j, k){
  exp(lgamma(k*j+j-1)-lgamma(k*j)-lgamma(j+1)+(j-1)*log(R0/k)-(k*j+j-1)*log(1+R0/k))
}

prob_outbreak_imperf_det <- function(R0, jj, k, reporting_rate){
  # jj is the detected number of cases in a chain
  # j_true becomes the possible true number of cases in a chain that gave rise to j detected cases

  num_calc = 1e4
  if(R0 == 0){
    j = 1:num_calc
    true_chain_pdf <- vector(mode = "numeric", length = num_calc)[1]
    true_chain_pdf[1] <- 1
  } else{
    j = 1:num_calc
    # log_real_chain_pdf = lgamma(k*j+j-1)-lgamma(k*j)-lgamma(j+1)+(j-1)*log(r0/k)-(k*j+j-1)*log(1+r0/k)
    true_chain_pdf = prob_outbreak_perf_det(R0, j, k)
  }
  if(jj ==0){
    return(NA)
  }
  prob0 = sum(exp(j*log(1-reporting_rate)+log(true_chain_pdf)))
  denominator = 1 - prob0

  l = jj:num_calc
  numerator = sum(exp(log(true_chain_pdf[l]) +  dbinom(x= jj, size = l, prob = reporting_rate, log = T)))

  numerator/denominator
}

prob_outbreak_imperf_det_import <- function(R0, jj, k, reporting_rate){
  # jj is the detected number of cases in a chain
  # the j vector becomes the possible true number of cases in a chain that gave rise to j detected cases
  # browser()
  num_calc = 1e4
  if(R0 == 0){
    j = 1:num_calc
    true_chain_pdf <- vector(mode = "numeric", length = num_calc)[1]
    true_chain_pdf[1] <- 1
  } else{
    j = 1:num_calc
    # log_real_chain_pdf = lgamma(k*j+j-1)-lgamma(k*j)-lgamma(j+1)+(j-1)*log(r0/k)-(k*j+j-1)*log(1+r0/k)
    true_chain_pdf = prob_outbreak_perf_det(R0, j, k)
  }
  if(jj == 0){
    return(NA)
  }
  prob0 = 0
  denominator = 1 - prob0

  l = jj:num_calc
  numerator = sum( exp(log(true_chain_pdf[l]) + dbinom(x = jj-1, size = l-1, prob = reporting_rate, log=T) ))

  numerator/denominator
}

rnot = 0.7
j = 0:100
k=0.12
rr = 0.5

10000 %>% rerun(sim_outbreak(rnot, k)) %>% unlist() -> outbreak_sizes


## Calculate the analytical expectation for the outbreak size probabilities
size_predictions <- data_frame(obs_size = j) %>%
                      mutate(`Perfect` = map(.x = obs_size, .f = prob_outbreak_perf_det, R0=rnot, k=k) %>% unlist(),
                             `Imperfect` = map(.x = obs_size, .f = prob_outbreak_imperf_det, R0 =rnot, k=k, reporting_rate = rr) %>% unlist(),
                             `Imperfect Import` = map(.x = obs_size, .f = prob_outbreak_imperf_det_import, R0 =rnot, k=k, reporting_rate = rr) %>% unlist()) %>%
  gather(key, value, 2:4) %>%
  mutate(key = factor(key, levels = c("Perfect", "Imperfect", "Imperfect Import")))

data_frame(ind = 1:length(outbreak_sizes),
           `Perfect` = outbreak_sizes,
           `Imperfect` = sim_detection(outbreak_sizes, rr),
           `Imperfect Import` = sim_known_import_detection(outbreak_sizes, rr)) %>%
  gather(key,value, 2:4) %>%
  filter(value !=0) %>%
  mutate(key = factor(key, levels = c("Perfect", "Imperfect", "Imperfect Import"))) %>%
  ggplot(aes(value)) +
  facet_wrap(~key) +
  geom_histogram(binwidth=1, aes(y=..density..)) +
  coord_cartesian(xlim = c(0,20)) +
  labs(x= "Outbreak Size", y = "Probability Density")+
  geom_point(data = size_predictions, aes ( x = obs_size, y = value), color = "red", size = 2) -> sim_v_analytic_plot
sim_v_analytic_plot

save_plot("ms_figs/sfigs/sfx_sim_v_analytic_plot.png", sim_v_analytic_plot, base_height = 5, base_aspect_ratio = 3)



