#################################################################################################################
## use_to_npc_conversion.r
## Amelia Bertozzi-Villa
## August 2020
## 
## Analysis for Pete Winskill at Imperial College for the GTS refresh. For each country, what 
## steady-state nets-per-capita is required to attain a given net use?
## Assumptions: 
##  - Net use rates stays at 2019 levels for all countries
##  - When converting from access to NPC, use the 2019 loess curve fit of access-NPC at the country-month level.
##  - When determining the number of nets to distribute in each mass campaign, assume mass campaigns occur every 
##    three years.
##################################################################################################################

rm(list=ls())

library(data.table)
library(ggplot2)
library(pracma)

############ ----------------------------------------------------------------------------------------------------------------------
## Inputs  ----------------------------------------------------------------------------------------------------------------------
############ ----------------------------------------------------------------------------------------------------------------------

analysis_year <- 2019
cube_indir <- "../data/coverage_metrics/"

write_results <- F

############ ----------------------------------------------------------------------------------------------------------------------
## Data Prep  ---------------------------------------------------------------------------------------------------------
############ ----------------------------------------------------------------------------------------------------------------------

# TODO: Do we need to convert to population-at-risk?

cube_nat_level_fnames <- list.files(cube_indir, "aggregated", full.names = T)
cube_nat_level_fnames <- cube_nat_level_fnames[!cube_nat_level_fnames %like% "mean_ONLY"]
cube_nat_level <- rbindlist(lapply(cube_nat_level_fnames, fread))
cube_nat_level <- cube_nat_level[iso3!="AFR" &  year %in% analysis_year]

cube_nat_level_annual <- cube_nat_level[is.na(month)]
cube_nat_level_annual <- dcast.data.table(cube_nat_level_annual, iso3 + year ~ variable, value.var="mean")

cube_nat_level <- cube_nat_level[!is.na(time)]


# find access-npc curve
cube_nat_level <- dcast.data.table(cube_nat_level, iso3 + month ~ variable, value.var="mean")
curve_fit <- loess(access ~ percapita_nets, data=cube_nat_level)

loess_for_prediction <- cube_nat_level[, list(iso3, month, access, percapita_nets, loess=predict(curve_fit))]
loess_for_prediction <- loess_for_prediction[order(loess)]

rm(cube_nat_level_fnames, cube_nat_level, curve_fit)

############ ----------------------------------------------------------------------------------------------------------------------
## Find new NPC  ---------------------------------------------------------------------------------------------------------
############ ----------------------------------------------------------------------------------------------------------------------

target_uses <- seq(0.1, 0.9, 0.1)

targets_all <- lapply(target_uses, function(this_use){
  targets_dt <- cube_nat_level_annual[, list(iso3, year, access, use, use_rate, percapita_nets, target_use=this_use)]
  targets_dt[, target_access:= target_use/use_rate] # TODO: what if target_access > 1?
  targets_dt <- targets_dt[order(target_access)]
  targets_dt[target_access<=max(loess_for_prediction$loess), target_percapita_nets:= interp1(loess_for_prediction$loess, loess_for_prediction$percapita_nets, target_access)] 
  return(targets_dt[order(iso3)])
})

targets_all <- rbindlist(targets_all)
targets_all[, target_use:=factor(target_use)]

targets_plot <- ggplot(targets_all, aes(x=use, y=target_percapita_nets, color=target_use)) +
                  geom_text(aes(label=iso3)) +
                  theme_bw()

if (write_results){
  write.csv(targets_all, "../use_to_npc_targets.csv", row.names = F)
}

############ ----------------------------------------------------------------------------------------------------------------------
## Find number of nets to distribute  ---------------------------------------------------------------------------------------------------------
############ ----------------------------------------------------------------------------------------------------------------------

## For this step, you would need to: 
## 1. Read in a dataset of LLIN half-lives for each country.
## 2. Integrate the net loss function (see Bhatt eLife 2015 supplement) from 0-3 years and calculate the initial 
##    nets-per-capita such that the average NPC is equal to the target value found above. 
## 3. Convert this initial NPC to a net count, choosing a population denominator that accounts for expected population growth. 











