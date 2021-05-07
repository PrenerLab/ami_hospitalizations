# tidy mophims data ####

#=== #=== #=== #=== #=== #=== #=== #=== #===

# dependencies ####

## tidyverse
library(dplyr)
library(readr)

## spatial
library(tidycensus)

#=== #=== #=== #=== #=== #=== #=== #=== #===

# load data ####
ami <- read_csv("data/mophims_output.csv", skip = 3)

#=== #=== #=== #=== #=== #=== #=== #=== #===

# initial data wrangling ####

ami %>%
  slice(1:(n()-5)) %>%
  rename(
    GEOID = `Census Tract`,
    count = X2
  ) %>%
  select(GEOID, count) %>%
  mutate(count = ifelse(count == "x", NA, count)) %>%
  mutate(count = as.numeric(count)) -> ami

#=== #=== #=== #=== #=== #=== #=== #=== #===

# calculate rates ####

## download total population
total_pop <- get_acs(geography = "tract", year = 2015, variables = "B01003_001", 
                     state = 29, county = c(189, 510)) %>%
  rename(
    total_pop = estimate,
    total_pop_moe = moe
  ) %>%
  select(GEOID, total_pop, total_pop_moe)

## calculate rates
left_join(ami, total_pop, by = "GEOID") %>%
  mutate(rate = count/total_pop*1000, .after = count) -> ami

#=== #=== #=== #=== #=== #=== #=== #=== #===

# write data ####
write_csv(ami, "data/ami_clean.csv")
