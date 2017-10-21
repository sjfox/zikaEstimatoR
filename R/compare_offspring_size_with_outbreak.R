library(tidyverse)
library(cowplot)
# Outbreak size probability
rj = function(R0, j, k){
  exp(lgamma(k*j+j-1)-lgamma(k*j)-lgamma(j+1)+(j-1)*log(R0/k)-(k*j+j-1)*log(1+R0/k))
}

# offspring number probability
si = function(R0, i, k){
  dnbinom(i,size=k,mu=R0)
}


rj_obs = function(R0, j, k, p){
  j_true = j : 1e3
  outbreak_size_pdf = rj(R0, j_true, k)
  sj = sum(outbreak_size_pdf * dbinom(j, j_true, p))
  # browser()
  s0 = sum( (1-p)^j_true * outbreak_size_pdf )
  sj / (1 - s0)
}

# Offspring observation process (p = reporting_rate)
rj_obs_import = function(R0, k, p){
  # j is total outbreak size (including importation)
  # k = dispersion (0.12)
  # p = reporting_rate (0.0574)
  # R0 = Rnot for the county
  j_true = 1 : 1e3
  outbreak_size_pdf = rj(R0, j_true, k)
  s0 = sum( (1-p)^j_true * outbreak_size_pdf )
  s0
}


# Setup parameters for no secondary transmission and plot
k = 0.12
j = 1
i = 0
r0 = seq(0.05, 3, length.out = 500)

no_sec_trans_results <- data_frame(rnot = r0) %>%
  mutate(outbreak_obs = map(.x = rnot, .f = rj_obs, j=j, k=k, p = 0.0574) %>% unlist(),
         outbreak = map(.x = rnot * 0.0574, .f = rj, j=j, k=k) %>% unlist(),
         offspring = map(.x = rnot * 0.0574, .f = si, i=i, k=k) %>% unlist(),
         outbreak_no_cases = map(.x = rnot, .f = rj_obs_import, k=k, p = 0.0574) %>% unlist())

no_sec_trans_results %>% gather(key,value, outbreak_obs:outbreak_no_cases) %>%
  ggplot(aes(rnot, value, color = key)) + geom_line()

# No compare them all for some transmission, and include observation

k = 0.12
j = 2
i = 1
r0 = seq(0.05, 3, length.out = 500)

sec_trans_results <- data_frame(rnot = r0) %>%
  mutate(outbreak_obs = map(.x = rnot, .f = rj_obs, j=j, k=k, p = 0.0574) %>% unlist(),
         outbreak = map(.x = rnot * 0.0574, .f = rj, j=j, k=k) %>% unlist(),
         offspring = map(.x = rnot * 0.0574, .f = si, i=i, k=k) %>% unlist())

sec_trans_results %>% gather(key,value, outbreak_obs:offspring) %>%
  ggplot(aes(rnot, value, color = key)) + geom_line()


