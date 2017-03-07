base_url <- "zikaEstimatoR"
if(grepl('spencerfox', Sys.info()['login'])) setwd(file.path("~", "projects", base_url))
if(grepl('vagrant', Sys.info()['user'])) setwd( file.path("/vagrant", base_url) )
if(grepl('sjf826', Sys.info()['login'])) setwd(file.path("/home1", "02958", "sjf826", base_url))
if(grepl('tacc', Sys.info()['nodename'])) setwd(file.path("/home1", "02958", "sjf826", base_url))


# rnot_values <- c("low","med", "high")
single_rnot <- FALSE
reporting_rate <- seq(0.01, 0.015, 0.02, 0.04, 0.05, 0.1, 0.2, 1)
sink('launcher/alpha_like_runs.txt')
for(ind in seq_along(reporting_rate)){
  startCmd <- "R CMD BATCH --no-restore --no-save '--args"
  #fileCmd <- paste0(' single_rnot=', single_rnot, ' rnot_value="', rnot_values[ind], '"')
  fileCmd <- paste0(' single_rnot=', single_rnot, ' reporting_rate="', reporting_rate[ind], '"')
  endCmd <- "' ../R/calc_alpha_likelihoods.R"
  full_cmd <- paste0(startCmd, fileCmd, endCmd)
  # print(full_cmd)
  cat(full_cmd)               # add command
  cat('\n')              # add new line
}
sink()
