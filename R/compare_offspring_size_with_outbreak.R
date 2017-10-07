library(tidyverse)

# Outbreak size probability
rj = function(R0, j, k){
  exp(lgamma(k*j+j-1)-lgamma(k*j)-lgamma(j+1)+(j-1)*log(R0/k)-(k*j+j-1)*log(1+R0/k))
}

# offspring number probability
si = function(R0, i, k){
  dnbinom(i,size=k,mu=R0)
}

# Setup parameters for no secondary transmission and plot
k = 0.12
j = 1
i = 0
r0 = seq(0.05, 3, length.out = 500)

outbreak_size_probs <- r0 %>% map(.f = rj, j=j, k=k) %>% unlist()
offspring_num_probs <- r0 %>% map(.f = si, i=i, k=k) %>% unlist()

plot(outbreak_size_probs, offspring_num_probs)
abline(a = 0, b = 1)

all_equal(round(outbreak_size_probs, digits = 10), round(offspring_num_probs,digits=10))

# Setup parameters for single case of secondary transmission and plot

k = 0.12
j = 2
i = 1
r0 = seq(0.05, 3, length.out = 500)

outbreak_size_probs <- r0 %>% map(.f = rj, j=j, k=k) %>% unlist()
offspring_num_probs <- r0 %>% map(.f = si, i=i, k=k) %>% unlist()

plot(outbreak_size_probs, offspring_num_probs)
abline(a = 0, b = 1)


