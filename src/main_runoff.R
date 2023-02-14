#main_runoff.R

#Author: Anna Mikulis, University of New Hampshire
#Last Updated: 1/26/2022

#Purpose: Estimate load from coastal runoff using Lamprey concentrations and coastal watershed area

#Load packages
Packages <- c("readr", "dplyr", "measurements")

lapply(Packages, library, character.only = TRUE)

#Read in Lamprey Riverine Loads
lmp <- read.csv("results/main_load_calc/FW_Loads/LR_Annual_Loads.csv")

#Coastal Area 36km2
GB_Coastal_Area <- 37 #estimated using Google Earth mapping tools

#Load in kg/year, need to normalize it to runoff.
#Lamprey River Watershed Area 

lmp_Area <- 54897.39 #hectares
lmp_Area_km2 <- conv_unit(lmp_Area, "hectare", "km2")

#Scale lmp loads by watershed area
lmp <- lmp %>%
  select(-X) %>%
  mutate(across(FW_TP:FW_TSS, ~./lmp_Area_km2))

#Great Bay watershed area runoff

GRB_CR <- lmp %>%
  select(-Flow_l_year) %>%
  mutate(across(FW_TP:FW_TSS, ~. * GB_Coastal_Area))

GRB_CR <- GRB_CR %>%
  select(-FW_TP)


write.csv(GRB_CR, "results/main_runoff/runoff_estimate_kgyr.csv")

#Repeat to get monthly coastal runoff
lmp_monthly <- read.csv("results/main_load_calc/FW_Loads/LR_MLoads_kg_month.csv")

lmp_monthly <- lmp_monthly %>%
  select(-X, - flow_month) %>%
  mutate(across(FW_TP:FW_TSS, ~. /lmp_Area_km2))

GRB_CR_monthly <- lmp_monthly %>%
  mutate(across(FW_TP:FW_TSS, ~. * GB_Coastal_Area))

write.csv(GRB_CR_monthly, "results/main_runoff/runoff_monthly.csv")
