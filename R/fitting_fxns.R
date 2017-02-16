#################################################
## Functions for fitting R0s
#################################################

subs_parms <- function(sub_parms=NULL,
                       ref_parms) {
  within(ref_parms, {

    for(nm in names(sub_parms)) {
      assign(nm, sub_parms[[nm]])
    }
    rm(nm)
  })
}

find_rnot_ods <- function(rnot, disp_dt){
  ## Takes in a vector of rnot values and returns
  ## The dispersions for nbinom distribution
  ## disp_dt must be sorted data table, with one column as rnot and other as ods
  ## Can be obtained by running the calc_dispersion_table.R script
  disp_dt[J(rnot), roll = "nearest"]$ods
}


zika_parms <- function(rnot = 1.1,
                       num_intros = 1,
                       prior_mu = 1,
                       prior_sd = 1000,
                       distribution = "pois",
                       overdispersion=1,
                       date = NA){
  return(as.list(environment()))
}

normalize_vector <- function(values){
  ## Normalizes a vector to sum to 1
  values/sum(values,na.rm = T)
}

intro_like <- function(parms) {
  ## Calculates the likelihood based on an R0 and number of introductions
  switch(parms$distribution,
         pois = dpois(x = 0, lambda = parms$rnot)^parms$num_intros,
         nbinom = dnbinom(x = 0, mu = parms$rnot, size = parms$overdispersion)^parms$num_intros)
}
intro_loglike <- function(parms) {
  ## Calculates the likelihood based on an R0 and number of introductions
  switch(parms$distribution,
         pois = dpois(x = 0, lambda = parms$rnot, log = T)*parms$num_intros,
         nbinom = dnbinom(x = 0, mu = parms$rnot, size = parms$overdispersion, log=T)*parms$num_intros)
}

scaling_loglike <- function(alpha, parms, disp_dt){
  ## Returns the log likelihood through time for quarter
  ## Rnots and introductions.
  rnots <- parms$rnot * alpha
  # ods <- unlist(purrr::map(rnots, ~find_overdispersion(.x)))
  ods <- find_rnot_ods(rnots, disp_dt)
  parms <- subs_parms(list(rnot=rnots, overdispersion=ods), parms)
  -sum(intro_loglike(parms))
}

get_alpha_ci <- function(parms, disp_dt, sig_level=0.01){
  # For a set of parameters, finds the possible alphas
  alphas <- seq(0,1, length.out = 5000)
  nllikes <- unlist(purrr::map(alphas, ~scaling_loglike(., parms=parms, disp_dt)))
  likes <- exp(-nllikes)

  ## Extract the largest alpha that fulfills
  high <- alphas[rev(which(likes > sig_level))[1]]

  data_frame(mle=0, low=0, high=high)
}

get_alpha_likes <- function(parms, disp_dt){
  # Returns likelihood values for a variety of alphas, so that distributions can be calculated post-hoc
  alphas <- seq(0, 1, length.out = 5000)
  nllikes <- unlist(purrr::map(alphas, ~scaling_loglike(., parms=parms, disp_dt)))
  likes <- exp(-nllikes)

  df <- data_frame(alpha = alphas, likelihood = likes)
  colnames(df)[2] <- as.character(parms$date)
  df
}




get_rnot_ll_ci <- function(alpha, num_intros, distribution, overdispersion=1, rnots=NULL) {
  ## Returns the median and % confidence interval for likelihood of rnot based
  ## On number of introductions alone

  if(num_intros==0){
    warning("With 0 Introductions you have no information")
    return(data.frame(low = NA, median = NA,  high = NA))
  }
  if(length(num_intros)!=1){
    stop("Need to send single introduction number")
  }


  if(!is.null(rnots)){
    n <- length(rnots)
    ods <- overdispersion
  } else {
    n <- 10000
    max_rnot <- 10
    rnots <- seq(0, max_rnot, length.out = n)
    ods <- unlist(purrr::map(rnots, ~find_overdispersion(.x)))
  }

  parms <- subs_parms(list(rnot=rnots, num_intros=num_intros, distribution=distribution, overdispersion=ods), zika_parms())
  likelihoods <- intro_like(parms)

  # Find index for the low, mle and high
  low_ind <- 1
  mle_ind <- which.max(likelihoods)
  high_ind <- which(likelihoods < alpha)[1]
  if(length(high_ind)==0){
    high_ind <- n
  }

  data.frame(low = rnots[low_ind], mle = rnots[mle_ind],  high = rnots[high_ind])
}


lprior <- function(parms){
  dnorm(parms$rnot, mean = parms$prior_mu, sd = parms$prior_sd, log = T)
}














# llike_prior <- function(rnot, ref_parms){
#   parms <- subs_parms(c(rnot=rnot), ref_parms)
#   log(intro_like(parms)) + lprior(parms)
# }



# rnot_mcmc <- function(parms,
#                       rand_init=T,
#                       iters,
#                       tuning,
#                       burnin,
#                       thin = 10){
#   if(iters %% thin != 0 ){
#     stop("Thin needs to be a multiple of iters")
#   }
#
#   samples <- matrix(nrow=iters/thin, ncol=2)
#   curr_rnot <- runif(n = 1, min = 0, max = 2)
#
#   curr_lik <- llike_prior(curr_rnot, parms)
#
#   accept <- 0
#   for(ii in 1:(iters+burnin)){
#     prop_rnot <- exp(rnorm(1, mean = log(curr_rnot), sd = tuning))
#     prop_lik <- llike_prior(prop_rnot, parms)
#
#     lmh <- prop_lik - curr_lik
#
#     if ( (lmh >= 0) | (runif(n = 1,min = 0, max = 1) <= exp(lmh)) ) {
#       curr_rnot <- prop_rnot
#       accept <- accept + 1
#       curr_lik <- prop_lik
#     }
#     if(ii>burnin & (ii-burnin) %% thin==0) {
#       samples[(ii-burnin)/thin,] <- c(curr_rnot, curr_lik)
#     }
#   }
#   aratio <- accept/(iters+burnin)
#   colnames(samples) <- c("r_not", "ll")
#   samples <- as.mcmc(samples)
#   return(list(samples, aratio = aratio))
# }

# library(Rcpp)
# sourceCpp("cpp/cpp_fitting_fxns.cpp")
#
#
#
#
# find_overdispersion_R(1.5)
#
# all_equal(compare_ps(seq(0,0.5,length.out=1000), 0.01470807, 12),
#           compare_ps_R(seq(0,0.5,length.out=1000), 0.01470807, 12))



# library(Rcpp)
#
# sourceCpp("cpp/cpp_fitting_fxns.cpp")

