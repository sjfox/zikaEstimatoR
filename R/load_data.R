#################################################
## Loads in the data used for analysis
#################################################

########################################
## Importation Data
########################################
require(tidyverse)
require(lubridate)
require(stringr)
tx_imports <- read_csv("data/ZikaReportDates11032016.csv")

tx_imports <- tx_imports %>% mutate(notification_date = mdy(`First Notification Date`)) %>%
  mutate(month = as.character(month(notification_date, label=TRUE, abbr = T)),
         county = tolower(str_replace_all(County, " County", ""))) %>%
  select(county, notification_date, month)

########################################
## Dispersion table
########################################

load("data_produced/dispersion_df.rda")

########################################
## Temperature Data
########################################

tx_temps <- read_csv("data/tx_county_temps.csv")
tx_temps <- tx_temps %>% mutate(month = factor(month, levels = month.abb)) %>%
  rename(county = subregion) %>%
  mutate(county = if_else(county=="de witt", "dewitt", county)) %>%
  spread(key = month, value = avg_temp)

## Old CDC WONDER data -- now using analyze_temperatures script with world data
# tx_temps2 <- read_tsv("data/tx_county_temps.txt")
# tx_temps2 <- tx_temps2 %>% filter(is.na(Notes)) %>%
#   rename(avg_max_temp = `Avg Daily Max Air Temperature (C)`,
#          avg_min_temp=`Avg Daily Min Air Temperature (C)`) %>%
#   mutate(avg_temp = (avg_max_temp + avg_min_temp) / 2,
#          county = tolower(str_replace_all(County, pattern = " County, TX", ""))) %>%
#   arrange(`Month Code`) %>%
#   mutate(Month = factor(Month, levels = unique(Month))) %>%
#   dplyr::select(county, month=Month, avg_temp) %>%
#   spread(key = month, value = avg_temp)


########################################
## Perkins Sim data for R0 calculation
########################################
load("data/functions_R0_AR_random_draws.RData")


########################################
## Texas county information for R0 calculation
########################################
tx_county <- read_csv(file = "data/county_r0_parameters.csv")

load("data_produced/calculated_tx_county_rnots.rda")

#######################################
## GGPLot2 function
#######################################
ggplot_dual_axis = function(plot1, plot2, which.axis = "y") {
  require(grid)
  require(gtable)
  # Update plot with transparent panel
  plot2 = plot2 + theme(panel.background = element_rect(fill = NA))

  grid.newpage()

  # Increase right margin if which.axis == "y"
  if(which.axis == "y") plot1 = plot1 + theme(plot.margin = unit(c(0.7, 1.5, 0.4, 0.4), "cm"))

  # Extract gtable
  g1 = ggplot_gtable(ggplot_build(plot1))

  g2 = ggplot_gtable(ggplot_build(plot2))

  # Overlap the panel of the second plot on that of the first
  pp = c(subset(g1$layout, name == "panel", se = t:r))

  g = gtable_add_grob(g1, g2$grobs[[which(g2$layout$name=="panel")]], pp$t, pp$l, pp$b, pp$l)

  # Steal axis from second plot and modify
  axis.lab = ifelse(which.axis == "x", "axis-b", "axis-l")

  ia = which(g2$layout$name == axis.lab)

  ga = g2$grobs[[ia]]

  ax = ga$children[[2]]

  # Switch position of ticks and labels
  if(which.axis == "x") ax$heights = rev(ax$heights) else ax$widths = rev(ax$widths)

  ax$grobs = rev(ax$grobs)

  if(which.axis == "x")

    ax$grobs[[2]]$y = ax$grobs[[2]]$y - unit(1, "npc") + unit(0.15, "cm") else

      ax$grobs[[1]]$x = ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")

  # Modify existing row to be tall enough for axis
  if(which.axis == "x") g$heights[[2]] = g$heights[g2$layout[ia,]$t]

  # Add new row or column for axis label
  if(which.axis == "x") {

    g = gtable_add_grob(g, ax, 2, 4, 2, 4)

    g = gtable_add_rows(g, g2$heights[1], 1)

    g = gtable_add_grob(g, g2$grob[[6]], 2, 4, 2, 4)

  } else {

    g = gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)

    g = gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

    g = gtable_add_grob(g, g2$grob[[7]], pp$t, length(g$widths), pp$b - 1)

  }

  # Draw it
  grid.draw(g)

}

