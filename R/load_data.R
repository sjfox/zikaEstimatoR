#################################################
## Loads in the data used for analysis
#################################################

texas_info <- read_csv("data/texas_county_info.csv")
texas_imports <- read_csv("data/texas_county_imports.csv")

get_county <- function(import_cty, cty_list){

  cty_list[grep(import_cty, x=cty_list, fixed = T)][1]
}


texas_data <- texas_imports %>% mutate(county =  unlist(sapply(county,
                                                      get_county,
                                                      cty_list=texas_info$county))) %>%
  mutate(imports = cases-ifelse(!is.na(local), local, 0)) %>%
  left_join(y=texas_info, by = "county")


rm(texas_info)
rm(texas_imports)
