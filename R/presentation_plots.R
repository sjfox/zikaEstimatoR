## Figures for presentation
library(tidyverse)
library(cowplot)
library(coda)
sapply(c("R/fitting_fxns.R", "R/plotting_fxns.R", "R/load_data.R"), source)


tx_imports <- read_csv("data/texas_county_imports.csv")
tx_imports <- tx_imports %>% mutate(county = tolower(county),
                                    local = ifelse(is.na(local), 0, local),
                                    imports = cases-local)

import_hist <- tx_imports %>%
  ggplot(aes(imports)) + geom_histogram() +
  labs(x = "Cumulative County Importations")

save_plot(filename = "figs/tx_import_hist.pdf", plot = import_hist, base_height = 4, base_aspect_ratio = 1.3)

dpois(x = 0, lambda = 1.5)^3

mle_ex <- data_frame(rnots = seq(0, 10, length.out = 100))

mle_ex_plot <- mle_ex %>% mutate(likelihood = map(rnots, ~intro_like_vec(rnot=., num_intros=3, distribution = "pois"))) %>%
  unnest(likelihood) %>%
  # mutate(likelihood = normalize_vector(likelihood)) %>%
  ggplot(aes(rnots, likelihood)) + geom_line()



overdisp_plot <- data_frame(rnot = seq(0.01, 1, length.out = 1000)) %>%
  mutate(overdispersion = unlist(purrr::map(rnot, ~find_overdispersion(.x)))) %>%
  ggplot(aes(rnot, overdispersion)) + geom_point(size=1) +
  geom_vline(xintercept=0.5777, lty=2)

save_plot(filename = "figs/overdisp_plot.pdf", plot = overdisp_plot, base_height = 4, base_aspect_ratio = 1.3)


tx_import_map <- map_data(map = "county") %>% filter(region=="texas") %>%
  left_join(tx_imports, by=c("subregion" = "county")) %>%
  ggplot(aes(x=long, y=lat, fill = imports, group = subregion)) +
  geom_polygon(color = "gray", size=0.25) +
  scale_fill_continuous(na.value="white", low = "gray", high = "red")+
  theme(axis.line=element_blank(),
     axis.text.x=element_blank(),
     axis.text.y=element_blank(),
     axis.ticks=element_blank(),
     axis.title.x=element_blank(),
     axis.title.y=element_blank(),
     legend.position = c(0.15, 0.8))
# plot(tx_import_map)

save_plot(filename = "figs/tx_import_map.pdf", plot = tx_import_map, base_height = 5, base_aspect_ratio = 1)

