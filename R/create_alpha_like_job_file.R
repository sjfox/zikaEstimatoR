base_url <- "zikaEstimatoR"
if(grepl('spencerfox', Sys.info()['login'])) setwd(file.path("~", "projects", base_url))
if(grepl('vagrant', Sys.info()['user'])) setwd( file.path("/vagrant", base_url) )
if(grepl('stampede', Sys.info()['nodename'])) setwd(file.path('/home1/02958/sjf826', base_url))
if(grepl('wrangler', Sys.info()['nodename'])) setwd(file.path('/home/02958/sjf826', base_url))


# rnot_values <- c("low","med", "high")
single_rnot <- FALSE
reporting_rate <- c(0.01, 0.0282, 0.0574, 0.0866, 0.1, 0.2)
include_trans <- c("NA", "1", "5")

sink('launcher/alpha_like_runs.txt')
for(ind in seq_along(reporting_rate)){
  for(trans_ind in seq_along(include_trans)){
    startCmd <- "R CMD BATCH --no-restore --no-save '--args"
    #fileCmd <- paste0(' single_rnot=', single_rnot, ' rnot_value="', rnot_values[ind], '"')
    fileCmd <- paste0(' single_rnot=', single_rnot, ' reporting_rate="', reporting_rate[ind], '"', ' include_trans="', include_trans[trans_ind], '"')
    endCmd <- "' ../R/calc_alpha_likelihoods.R"
    full_cmd <- paste0(startCmd, fileCmd, endCmd)
    # print(full_cmd)
    cat(full_cmd)               # add command
    cat('\n')              # add new line
  }
}
sink()
