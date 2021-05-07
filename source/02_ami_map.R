# map outcome data ####

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

## dependencies ####

## tidyverse
library(dplyr)
library(readr)
library(ggplot2)
library(sf)

## spatial
library(tigris)

## other
library(cowplot)

## functions
source("source/functions/map_breaks.R")
source("source/functions/save_plots.R")
source("source/functions/sequoia_theme.R")

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

## load data ####
ami <- read_csv("data/ami_clean.csv", col_types = cols(GEOID = col_character()))
red <- st_read("data/STL_BOUNDARY_Tracts_1940.geojson")

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

## prepare data ####

## download
tracts <- tracts(state = 29, county = c(189, 510)) %>%
  select(GEOID)
counties <- counties(state = 29) %>%
  filter(GEOID %in% c("29189", "29510")) %>%
  select(GEOID)

## join
tracts <- left_join(tracts, ami, by = "GEOID")

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

## map st. louis rates ####
## create breaks
tracts <- map_breaks(tracts, var = "rate", newvar = "rate_breaks",
                     style = "fisher", classes = 5, dig_lab = 2)

## create maps
p <- ggplot(data = tracts) +
  geom_sf(mapping = aes(fill = rate_breaks), size = .2) +
  geom_sf(data = counties, fill = NA, size = .4, color = "black") +
  scale_fill_brewer(palette = "GnBu", name = "Rate per 1,000") +
  labs(
    title = "AMI Hospitalization Rates",
    subtitle = "St. Louis City and County (2011-2015)",
    caption = "Data via the State of Missouri and U.S. Census Bureau"
  ) +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

## save maps
save_plots(filename = "results/a_case_map.png", plot = p, preset = "lg")

## create maps
p <- ggplot(data = tracts) +
  geom_sf(mapping = aes(fill = rate_breaks), size = .2) +
  geom_sf(data = counties, fill = NA, size = .4, color = "black") +
  scale_fill_brewer(palette = "GnBu", name = "Rate per 1,000") +
  theme_map() +
  theme(
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 7)
  )


#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

tracts <- tracts %>%
  filter(is.na(rate) == FALSE) %>%
  select(rate)

queens <- spdep::poly2nb(tracts, queen = TRUE)
weights <- spdep::nb2listw(queens, style="W", zero.policy = TRUE)

local <- spdep::localmoran(x = tracts$rate, listw = weights)

quadrant <- vector(mode="numeric",length=nrow(local))

# centers the variable of interest around its mean
m.rate <- tracts$rate - mean(tracts$rate)     

# centers the local Moran's around the mean
m.local <- local[,1] - mean(local[,1])    

# significance threshold
signif <- 0.05 

# builds data quadrant
quadrant[m.rate >0 & m.local>0] <- 4  
quadrant[m.rate <0 & m.local<0] <- 1      
quadrant[m.rate <0 & m.local>0] <- 2
quadrant[m.rate >0 & m.local<0] <- 3
quadrant[local[,5]>signif] <- 0   

tracts_lisa <- cbind(tracts, quadrant) %>%
  mutate(val = case_when(
    quadrant == 0 ~ "insignificant",
    quadrant == 1 ~ "low-low",
    quadrant == 2 ~ "low-high",
    quadrant == 3 ~ "high-low",
    quadrant == 4 ~ "high-high"
  )) %>%
  mutate(val = forcats::fct_relevel(val, levels = c("insignificant","low-low","low-high","high-low","high-high")))

rm(quadrant, m.local, m.rate, queens, weights, local, signif)

cols <- c("insignificant" = "white", "low-low" = "blue", "low-high" = rgb(0,0,1,alpha=0.4),
         "high-low" = rgb(1,0,0,alpha=0.4), "high-high" = "red")

p2 <- ggplot() +
  geom_sf(data = counties, fill = "white") +
  geom_sf(data = tracts_lisa, mapping = aes(fill = val), size = .2) +
  geom_sf(data = counties, fill = NA, size = .4, color = "black") +
  scale_fill_manual(values = cols) +
  theme_map() +
  theme(
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 7)
  )


## save maps
save_plots(filename = "results/b_lisa_map.png", plot = p2, preset = "lg")

p_grid <- cowplot::plot_grid(p, p2, align = "v", ncol = 2, labels = c('A', 'B'), label_size = 12)

ggsave("results/ami_compare.png", p_grid, width = 7.5, height = 3, units = "in", dpi = 500)

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

## LISA map ####
## create maps
p2 <- ggplot() +
  geom_sf(data = counties, fill = "white") +
  geom_sf(data = tracts_lisa, mapping = aes(fill = val), size = .2) +
  geom_sf(data = counties, fill = NA, size = .4, color = "black") +
  scale_fill_manual(values = cols, name = "LISA Result") +
  labs(
    title = "LISA Analysis of AMI Hospitalization Rates",
    subtitle = "St. Louis City and County (2011-2015)",
    caption = "Data via the State of Missouri and U.S. Census Bureau"
  ) +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

## save maps
save_plots(filename = "results/b_lisa_map.png", plot = p2, preset = "lg")

#===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===# #===#

## map redlining ####
## create breaks
red <- map_breaks(red, var = "pct_red_40", newvar = "rate_breaks",
                     style = "fisher", classes = 5, dig_lab = 3)

## create maps
p <- ggplot(data = red) +
  geom_sf(mapping = aes(fill = rate_breaks), size = .2) +
  geom_sf(data = counties, fill = NA, size = .4, color = "black") +
  scale_fill_brewer(palette = "Reds", name = "% C/D HOLC Grades") +
  labs(
    title = "Home Owner Loan Corporation Grades",
    subtitle = "St. Louis City and County (1940)",
    caption = "Data via Colin Gordon, Ph.D."
  ) +
  sequoia_theme(base_size = 22, background = "white", map = TRUE)

## save maps
save_plots(filename = "results/c_holc_pct_map.png", plot = p, preset = "lg")
