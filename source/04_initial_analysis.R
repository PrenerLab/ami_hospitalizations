# compare outcome with redlining and segregation ####

#===# #===# #===# #===# #===# #===# #===# #===# #===#

# dependencies ####

## tidyverse
library(dplyr)
library(readr)
library(ggplot2)
library(tibble)

## spatial
library(sf)
library(tigris)

## other
library(stargazer)

## functions
source("source/functions/corr_table.R")

#===# #===# #===# #===# #===# #===# #===# #===# #===#

# load data ####

## read
ami <- read_csv("data/ami_clean.csv", col_types = cols(GEOID = col_character()))
red <- st_read("data/STL_BOUNDARY_Tracts_1940.geojson")
ice <- st_read("data/STL_DEMOGRAPHY_ICE.geojson") %>%
  rename(GEOID = geoid)
demos <- read_csv("data/STL_DEMOGRAPHY_Current.csv", col_types = cols(GEOID = col_character()))

## remove geometry from ice
st_geometry(ice) <- NULL

## join
left_join(red, ice, by = "GEOID") %>%
  left_join(., demos, by = "GEOID") %>%
  left_join(., ami, by = "GEOID") -> ami_seg

## clean-up
rm(ami, ice, red, demos)

#===## #===## #===# #===# #===# #===# #===# #===# #===#

# descriptive STATS ####
## create data
corr_data <- ami_seg
st_geometry(corr_data) <- NULL

## create table
corr_data %>%
  select(rate, pct_red_40, ice_2010, poverty_pct, median_inc, 
         labor_pct, owner_occ_pct, vacant_pct) %>%
  stargazer(type = "html", 
            title = "Descriptive Statistics",
            summary.stat = c("n", "mean", "sd", "min", "max"),
            out = "results/table1_descriptives.html")

#===# #===# #===# #===# #===# #===# #===# #===# #===#

# correlations 1 ####
## create table
corr_data %>%
  select(pct_red_40, ice_1940:ice_2010) %>%
  corr_table(coef = "pearson") %>%
  select(pct_red_40) %>%
  rownames_to_column(var = "var") %>%
  filter(var %in% c("pct_red_40") == FALSE) %>%
  rename(value = pct_red_40) %>%
  mutate(var = case_when(
    var == "ice_1940" ~ "ICE, 1940",
    var == "ice_1950" ~ "ICE, 1950",
    var == "ice_1960" ~ "ICE, 1960",
    var == "ice_1970" ~ "ICE, 1970",
    var == "ice_1980" ~ "ICE, 1980",
    var == "ice_1990" ~ "ICE, 1990",
    var == "ice_2000" ~ "ICE, 2000",
    var == "ice_2010" ~ "ICE, 2010"
  )) -> corr_output

## write
stargazer(corr_output, title = "Redlining and Segregation, Pearson's r", 
          type = "html", summary = FALSE, rownames = FALSE,
          out = "results/table2_ice_corrs.html")

#===# #===# #===# #===# #===# #===# #===# #===# #===#

# correlations 2 ####
## correlation table
corr_data %>%
  select(rate, pct_red_40, ice_2010, median_inc, labor_pct,
           owner_occ_pct, vacant_pct) %>%
  corr_table(coef = "pearson") -> corr_output

## write
stargazer(corr_output, title = "Pearson's r Correlation Coefficints", 
          type = "html", summary = FALSE, rownames = TRUE,
          out = "results/table3_full_predictor_corrs.html")

## correlation table
corr_data %>%
  select(pct_red_40, rate, ice_2010, median_inc, labor_pct,
         owner_occ_pct, vacant_pct) %>%
  corr_table(coef = "pearson") %>%
  select(pct_red_40) %>%
  rownames_to_column(var = "var") %>%
  filter(var %in% c("pct_red_40") == FALSE) %>%
  rename(value = pct_red_40) %>%
  mutate(var = case_when(
    var == "rate" ~ "AMI Hospitalization Rate",
    var == "ice_2010" ~ "ICE, Race",
    var == "median_inc" ~ "Median Income",
    var == "labor_pct" ~ "% in Labor Force",
    var == "owner_occ_pct" ~ "% Owner Occupied",
    var == "vacant_pct" ~ "% Vacant"
  )) %>%
  rename(variable = var) -> corr_output

## write
stargazer(corr_output, title = "Pearson's r Correlation Coefficints", 
          type = "html", summary = FALSE, rownames = FALSE,
          out = "results/table3_predictor_corrs.html")

## correlation table
corr_data %>%
  select(rate, pct_red_40, ice_2010, median_inc, labor_pct,
         owner_occ_pct, vacant_pct) %>%
  corr_table(coef = "pearson") %>%
  select(rate) %>%
  rownames_to_column(var = "var") %>%
  filter(var %in% c("rate") == FALSE) %>%
  rename(value = rate) %>%
  mutate(var = case_when(
    var == "pct_red_40" ~ "% Redlined, 1940",
    var == "ice_2010" ~ "ICE, Race",
    var == "median_inc" ~ "Median Income",
    var == "labor_pct" ~ "% in Labor Force",
    var == "owner_occ_pct" ~ "% Owner Occupied",
    var == "vacant_pct" ~ "% Vacant"
  )) %>%
  rename(variable = var) -> corr_output

## write
stargazer(corr_output, title = "Pearson's r Correlation Coefficints", 
          type = "html", summary = FALSE, rownames = FALSE,
          out = "results/table3_ami_corrs.html")

## clean-up
rm(corr_data, corr_output, corr_table)

#===# #===# #===# #===# #===# #===# #===# #===# #===#

# linear models ####
## model set-up
model_data <- as_Spatial(ami_seg)

model_formulas <- list(
  f1 = as.formula("rate ~ pct_red_40"),
  f2 = as.formula("rate ~ pct_red_40 + ice_2010"),
  f3 = as.formula("rate ~ pct_red_40 + ice_2010 + median_inc + labor_pct + owner_occ_pct + vacant_pct")
)

## fit models
model_output <- list(
  model_1_ols = lm(model_formulas$f1, data = model_data@data),
  model_2_ols = lm(model_formulas$f2, data = model_data@data),
  model_3_ols = lm(model_formulas$f3, data = model_data@data)
)

# car::outlierTest(model_output$model_3_ols)
# cooksD <- which(cooks.distance(model_output$model_3_ols) > 1)
# car::influencePlot(model_output$model_3_ols)
# car::qqPlot(model_output$model_3_ols)
# lmtest::bptest(model_output$model_3_ols)
# lmtest::bptest(model_output$model_3_ols, ~ pct_red_40 * ice_2010 * median_inc * labor_pct * owner_occ_pct * vacant_pct + 
                 # I(pct_red_40^2) + I(ice_2010^2) + I(median_inc^2) + I(labor_pct^2) + I(owner_occ_pct^2) + I(vacant_pct^2),
               # data = model_data@data)
# plot(model_output$model_3_ols, which = 1)
# car::durbinWatsonTest(model_output$model_3_ols)
# sandwich::vcovHC(model_output$model_3_ols, "HC3")

# x <- filter(model_data@data, row_number() %in% c(26, 43, 61, 271, 69) == FALSE)

# model_4_ols = lm(model_formulas$f3, data = x)

## 43, 61, 271, 69

## test spatial
queens <- spdep::poly2nb(model_data, queen = TRUE)
weights <- spdep::nb2listw(queens, style="W", zero.policy = TRUE)

spdep::lm.morantest(model_output$model_3_ols, weights, alternative="two.sided")

## create model tables
stargazer(model_output$model_1_ols, model_output$model_2_ols, model_output$model_3_ols,
          header=FALSE, type='latex',
          title = "Acute MI and Redlining",
          add.lines = list(
            c("AIC", round(AIC(model_output$model_1_ols), digits = 3), round(AIC(model_output$model_2_ols), digits = 3), round(BIC(model_output$model_3_ols), digits = 3)),
            c("BIC", round(BIC(model_output$model_1_ols), digits = 3), round(BIC(model_output$model_2_ols), digits = 3), round(BIC(model_output$model_3_ols), digits = 3))
            ),
          covariate.labels = c("% Redlined, 1940", "ICE, Race", 
                               "Median Income", "% in Labor Force", 
                               "% Owner Occupied", "% Vacant"),
          dep.var.caption  = "Acute MI Rate per 1,000",
          dep.var.labels   = "OLS Models",
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit.stat = "rsq",
          single.row = TRUE,
          digits = 3,
          out = "results/table4_models.html")
