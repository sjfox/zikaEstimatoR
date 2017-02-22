################################
## Calculate and save the overdispersion parameters for Rnots
################################
rm(list=ls())
library(tidyverse)

get_secondary_above_20 <- function(rnot){
  # Takes in an rnot value and returns the probability of seeing
  # > 20 secondary cases from that rnot
  p1 <- c(0.425806451612903, 0.8458765530605259)
  p2 <- c(4.341935483870967, 3.297197366921235)

  slope <- (p2[2] - p1[2]) / (p2[1] - p1[1])
  yint <- - slope * p1[1] + p1[2]
  if(rnot < yint){
    # warning("R0 is low and returning zero") # Happens very often, so not worth warning
    return(0)
  }

  prob <- (rnot - yint) / slope / 100
  if(prob > 1){
    return(1)
  }
  return(prob)
}

find_overdispersion <- function(rnot){
  # Find the overdispersion parameter for a given R0
  prob_above <- get_secondary_above_20(rnot)

  compare_ps <- function(x, prob_above, rnot){
    pnbinom(q = 20, mu = rnot, size = x, lower.tail = FALSE) - prob_above
  }
  # print(rnot)
  if(prob_above == 0){
    ## If Rnot is very low
    if(rnot==0) {
      return(1e-16)
    }
    # browser()
    seq_lower <- seq(1e-16, 0.5,length.out=1000)
    low <- -1
    prob_above <- 1e-5
    ps <- compare_ps(seq_lower, prob_above, rnot)
    while(max(ps, na.rm=T) < 0){
      prob_above <- prob_above/10
      ps <- compare_ps(seq_lower, prob_above, rnot)
    }
    low <- seq_lower[which.max(ps)]
    # print(rnot)
    # browser()
    overdisp <- uniroot(f = compare_ps, interval = c(low, 1),  rnot=rnot, prob_above= prob_above)
  } else {
    if(prob_above >= (1-1e-4) ){
      ## If rnot is very large
      seq_lower <- seq(0,100,length.out=1000)
      overdisp = list(root = seq_lower[which(abs(compare_ps(seq_lower, prob_above, rnot)) <= 1e-06)[1]])
    } else{
      seq_lower <- seq(0,100,length.out=10000)
      ps <- compare_ps(seq_lower, prob_above, rnot)

      if(all(diff(ps) > 0)){
        ## If Rnot is large but not very large
        overdisp <- uniroot(f = compare_ps, interval = c(0, 100), rnot=rnot, prob_above=prob_above)
      } else{
        ## If Rnot isn't miniscule, but is small (~0.5-1.5)
        ## Begin the search from the first positive difference.
        max_p <- which.max(ps)
        # browser()
        overdisp <- try(uniroot(f = compare_ps, interval = c(0, seq_lower[max_p]), rnot=rnot, prob_above=prob_above), silent = TRUE)
        if(class(overdisp) == "try-error"){
          overdisp <- uniroot(f = compare_ps, interval = c(seq_lower[max_p], 100), rnot=rnot, prob_above=prob_above)
        }
      }
    }

  }

  overdisp$root
}


rnots <- seq(0, 200, length.out= 200000)
ods <- unlist(purrr::map(rnots, ~find_overdispersion(.x)))


##################################
## Ensure that results make sense
##################################
rand_sample <- sample(x = c(1:100000), size = 10000, replace = F)
offset <- vector(mode="numeric", length = 10000)
for(rand_ind in rand_sample){
  prob_above <- get_secondary_above_20(rnots[rand_ind])
  offset[which(rand_ind==rand_sample)] <- pnbinom(q = 20, mu = rnots[rand_ind], size = ods[rand_ind], lower.tail = FALSE) - prob_above
}

summary(offset)
hist(offset, breaks=100)


################################
## Save to data frame and save for use
################################

dispersion_df <- data_frame(rnots = rnots, ods = ods)
# dispersion_dt <- data.table(dispersion_df, val=rnots)
# setattr(dispersion_dt, "sorted", "rnots")

save(dispersion_df, file = "data_produced/dispersion_df.rda")

##################################
## Testing speed of various lookups
##################################
## Switched to using data_frame, since it's sorted, and runs quickly on new Rcpp version. faster than data.table
# find_nearest_basic <- function(rnot, disp_df){
#   find_nearest <- function(rnot, disp_df){
#     disp_df$ods[which.min(abs(disp_df$rnots - rnot))]
#   }
#   unlist(purrr::map(rnot, ~find_nearest(.x, disp_df)))
# }
#
# find_nearest_dt <- function(rnot,disp_df){
#   require(data.table)
#   dt <- data.table(disp_df, val = rnots)
#   setattr(dt, "sorted", "rnots")
#   dt[J(rnot), roll = "nearest"]$ods
# }
#
# find_nearest_already_dt <- function(rnot,disp_dt){
#   disp_dt[J(rnot), roll = "nearest"]$ods
# }
# dispersion_dt <- data.table(dispersion_df, val=rnots)
# setattr(dispersion_dt, "sorted", "rnots")
#
# find_nearest_basic(1:10, dispersion_df)
# find_nearest_dt(1:10, dispersion_df)
#
#
# rnot_samp <- runif(100, min=0.000001, max = 100)
# library(microbenchmark)
# mbm <- microbenchmark(find_nearest_basic(rnot_samp, dispersion_df),
#                find_nearest_dt(rnot_samp, dispersion_df),
#                find_nearest_already_dt(rnot_samp, dispersion_dt))
# mbm
