############################################
## File holding all code necessary for running the mcmc
############################################


draw_zika_rnots <- function(gamma_parms){
  num_rnots <- nrow(rnot_dist)
  rnot_sample_inds <- sample(x = seq(1, ncol(rnot_dist)), size = num_rnots, replace = T)
  as.numeric(rnot_dist[1:num_rnots, rnot_sample_inds])
}


lprior <- function(alpha, parms){
  sum(dgamma(parms$rnot, shape = parms$rnot_dist$shape, rate = parms$rnot_dist$rate, log = T))
}

llprior <- function(alpha, parms){
  loglik <- try(scaling_loglike_cpp(alpha = alpha, params = parms))
  if(class(loglik) == "try-error"){browser()}
  prior <- lprior(alpha, parms)
  # print(paste0("loglik: ", loglik, " prior: ", prior))
}


draw_new_alpha <- function(alpha, tuning){
  runif(1)
  # plogis(qlogis(alpha) + rnorm(1, sd = tuning))
}

draw_new_rnots <- function(rnots, tuning){
  new_rnots <- exp(log(rnots) + rnorm(n = length(rnots), sd = tuning))
  ifelse(new_rnots<1e-16, 1e-16, new_rnots)
}


## MCMC pseudocode
mcmc_zika_rnot <- function (zika_parms,
                            alpha_tuning,
                            rnot_tuning,
                            burnin = 1000,
                            N= 10000,
                            thin = 1){
  accept <- 0

  ## rnot_cols stores # of columns necessary for storing posterior rnots
  ## Only estimating posterior proabbilities for necessary counties
  ## All other county posteriors can be retrospectively estimated
  ## straight from their prior distributions
  num_rnots <- nrow(zika_parms$rnot_dist)

  ###### Create matrix for saving
  ## adding 2 extra columns into estimate, alpha posterior and loglike
  saved_samps <- matrix(data = 0, nrow = (N-burnin)/thin, ncol = num_rnots+2)

  ###### Draw Rnots and alpha
  curr_rnots <- runif(num_rnots, min = 0, max = 2)
  ## Assumption that alpha is between 0 and 1, could change in future iterations
  curr_alpha <- runif(1)

  ## Make sure duplicated county/month Rnots are the same
  ## Make the last instances equal the first ones
  if(anyDuplicated(zika_parms$county_month)){
    curr_rnots[duplicated(zika_parms$county_month)] <- curr_rnots[duplicated(zika_parms$county_month, fromLast = TRUE)]
  }
  ###### Calc log like + log prior
  curr_parms <- subs_parms(list(rnot = curr_rnots), zika_parms)
  curr_llprior <- llprior(curr_alpha, curr_parms)

  ##### No longer save first results
  # saved_samps[1, ] <- c(curr_llprior, curr_alpha, curr_rnots)

  for( ii in 2:N){
    ###### Draw Proposed Rnots and alpha
    proposed_rnots <- draw_new_rnots(curr_rnots, rnot_tuning)
    ## link the duplicated months here as well
    if(anyDuplicated(zika_parms$county_month)){
      proposed_rnots[duplicated(zika_parms$county_month)] <- proposed_rnots[duplicated(zika_parms$county_month, fromLast = TRUE)]
    }


    ## Assumption that alpha is between 0 and 1, could change in future iterations
    proposed_alpha <- draw_new_alpha(curr_alpha, alpha_tuning)

    ###### Calc log like
    proposed_parms <- subs_parms(list(rnot = proposed_rnots), zika_parms)
    proposed_llprior <- llprior(proposed_alpha, proposed_parms)

    mh_prob <- proposed_llprior - curr_llprior
    if(is.na(mh_prob) | is.infinite(mh_prob)) browser()
    if(mh_prob >= 0 | (runif(1) <= exp(mh_prob))){
      curr_alpha <- proposed_alpha
      curr_rnots <- proposed_rnots
      curr_llprior <- proposed_llprior
      accept <- accept + 1
    }

    if(N > burnin & ii %% thin == 0){
      saved_samps[(ii - burnin)/thin, ] <- c(curr_llprior, curr_alpha, curr_alpha*curr_rnots)
    }

  }
  return(list(samples = saved_samps,
              aratio = accept/N))
}
