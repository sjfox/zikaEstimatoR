rm(list=ls())
library(tidyverse)
library(stringr)
library(lubridate)
# library(bbmle)

base_url <- "zikaEstimatoR"
if(grepl('spencerfox', Sys.info()['login'])) setwd(file.path("~", "projects", base_url))
if(grepl('vagrant', Sys.info()['user'])) setwd( file.path("/vagrant", base_url) )
if(grepl('sjf826', Sys.info()['login'])) setwd(file.path("/home1", "02958", "sjf826", base_url))
if(grepl('tacc', Sys.info()['nodename'])) setwd(file.path("/home1", "02958", "sjf826", base_url))

#####################################################
## Reads in the arguments from command line running
## Parameters required to be specified:
##    rnot_value - should be "low", "median", or "high"
##        - Determines which rnot estimate to use
##    single_rnot - "TRUE" or "FALSE"
##        - determines whether singular or distributional rnot estimate used
#####################################################

args <- (commandArgs(TRUE)) ## load arguments from R CMD BATCH

if(length(args)>0)  { ## Then cycle through each element of the list and evaluate the expressions.
  print(paste0('loading in ', args, ' from R CMD BATCH'))
  for(i in 1:length(args)) {
    eval(parse(text=args[[i]]))
  }
}

print(rnot_value)
print(single_rnot)


sapply(c("R/fitting_fxns.R", "R/load_data.R", "R/scaling_analysis_fxns.R"), source)

if(single_rnot){
  rnot_col_name <- switch(rnot_value, low = "low_r0", high = "high_r0", med = "med_r0", stop("Incorrect rnot value supplied, should be 'low', 'med', or 'high'."))
  tx_data <- tx_imports %>% mutate(month = factor(month, levels = month.abb)) %>%
    left_join(y = tx_county_rnots, by = c("county", "month")) %>%
    mutate(month = factor(month, unique(month)))

  ## Change correct column name to "rnot" for getting the parms
  colnames(tx_data)[which(colnames(tx_data) == rnot_col_name)] <- "rnot"

  daily_parms <- unique(tx_data$notification_date) %>%
    purrr::map(~get_alpha_parms(tx_data, curr_date=.x))
} else{
  stop("Can't handle distributional Rnot estimation yet.")
}

est_alphas <- purrr::map(daily_parms, get_alpha_likes)

est_alphas[2:length(est_alphas)] <- est_alphas[2:length(est_alphas)] %>% map(function(x) x[,2])

est_alphas_df <- est_alphas %>% bind_cols()

save(est_alphas_df, file = file.path("..","workfolder","data","ZikaEstimatoR_data", paste0("alpha_like_single_", rnot_col_name, ".rda")))
