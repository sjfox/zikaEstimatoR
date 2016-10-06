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

zika_parms <- function(rnot = 1.1,
                       num_intros = 1,
                       prior_mu = 1,
                       prior_sd = 1000,
                       distribution = "pois"){
  return(as.list(environment()))
}

normalize_vector <- function(values){
  ## Normalizes a vector to sum to 1
  values/sum(values,na.rm = T)
}

intro_like <- function(parms) {
  ## Calculates the likelihood based on an R0 and number of introductions
  switch(parms$distribution,
         pois = dpois(x = 0, lambda = parms$rnot)^parms$num_intros)
}


get_rnot_ll_ci <- function(alpha, num_intros, distribution) {
  ## Returns the median and % confidence interval for likelihood of rnot based
  ## On number of introductions alone
  n <- 10000
  ## Create vector of rnot values as possible outputs
  rnots <- seq(0, 4, length.out = n)

  ## Get the likelihood for all of the rnots, and then normalize the vector to sum to 1
  norm_probs <- vector("numeric", length = n)
  for(ii in 1:n){
    parms <- subs_parms(list(rnot=rnots[ii], num_intros=num_intros, distribution=distribution), zika_parms())
    norm_probs[ii] <- intro_like(parms)
  }
  norm_probs <- normalize_vector(norm_probs)


  ## Find the rnot value that first crosses the alpha level
  high_ind <- which(cumsum(rev(norm_probs)) >= alpha/2)[1]
  low_ind <- which(cumsum(norm_probs) >= alpha/2)[1]
  med_ind <- which(cumsum(norm_probs) >= 0.5)[1]

  data.frame(low = rnots[low_ind], median = rnots[med_ind],  high = rev(rnots)[high_ind])
}


lprior <- function(parms){
  dnorm(parms$rnot, mean = parms$prior_mu, sd = parms$prior_sd, log = T)
}

llike_prior <- function(rnot, ref_parms){
  parms <- subs_parms(c(rnot=rnot), ref_parms)
  log(intro_like(parms)) + lprior(parms)
}



rnot_mcmc <- function(parms,
                      rand_init=T,
                      iters,
                      tuning,
                      burnin,
                      thin = 10){
  if(iters %% thin != 0 ){
    stop("Thin needs to be a multiple of iters")
  }

  samples <- matrix(nrow=iters/thin, ncol=2)
  curr_rnot <- runif(n = 1, min = 0, max = 2)

  curr_lik <- llike_prior(curr_rnot, parms)

  accept <- 0
  for(ii in 1:(iters+burnin)){
    prop_rnot <- exp(rnorm(1, mean = log(curr_rnot), sd = tuning))
    prop_lik <- llike_prior(prop_rnot, parms)

    lmh <- prop_lik - curr_lik

    if ( (lmh >= 0) | (runif(n = 1,min = 0, max = 1) <= exp(lmh)) ) {
      curr_rnot <- prop_rnot
      accept <- accept + 1
      curr_lik <- prop_lik
    }
    if(ii>burnin & (ii-burnin) %% thin==0) {
      samples[(ii-burnin)/thin,] <- c(curr_rnot, curr_lik)
    }
  }
  aratio <- accept/(iters+burnin)
  colnames(samples) <- c("r_not", "ll")
  samples <- as.mcmc(samples)
  return(list(samples, aratio = aratio))
}


