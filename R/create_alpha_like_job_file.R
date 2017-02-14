base_url <- "ZikaEstimatoR"
if(grepl('spencerfox', Sys.info()['login'])) setwd(file.path("~", "projects", base_url))
if(grepl('vagrant', Sys.info()['user'])) setwd( file.path("/vagrant", base_url) )
if(grepl('sjf826', Sys.info()['login'])) setwd(file.path("/home1", "02958", "sjf826", base_url))
if(grepl('tacc', Sys.info()['nodename'])) setwd(file.path("/home1", "02958", "sjf826", base_url))


rnot_values <- c("low","med", "high")
single_rnot <- TRUE
sink('launcher/alpha_like_runs.txt')
for(ind in seq_along(rnot_values)){
  startCmd <- "R CMD BATCH --no-restore --no-save '--args"
  fileCmd <- paste0(' single_rnot=', single_rnot, ' rnot_value="', rnot_values[ind], '"')
  endCmd <- "' ../R/calc_alpha_likelihoods.R"
  full_cmd <- paste0(startCmd, fileCmd, endCmd)
  # print(full_cmd)
  cat(full_cmd)               # add command
  cat('\n')              # add new line
}
sink()




