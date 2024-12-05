#main_precipitation_format.R

#Author: Anna Mikulis, University of New Hampshire
#Last Updated: 12/05/2024

#Load packages
Packages <- c("readxl", "dplyr", "ggplot2", "measurements", "plotly", "lubridate", 
              "cowplot", "viridis", "agricolae", "tidyr")

lapply(Packages, library, character.only = TRUE)

#Load QC'd precipitation concentrations
precip <- read.csv("data/precipitation/qc_precipitation/precip_concentrations.csv")

precip <- precip %>%
  select(-X) %>%
  mutate(Collection_Date = as.Date(Collection_Date))

ggplot(precip, aes(Collection_Date, DOC_MGL)) + geom_point() + 
  geom_line() +
  scale_x_date(date_breaks = "2 year", date_labels="%b-%y")

#Calculate Precipitation-Weighted Concentrations for each solute by first multiplying each measured concentration by the rainfall for that ~weekly time period
#Create a second column for rainfall totals on days a solute was measured - accounts for weeks where there wasn't enough sample to run all of the analyses 
precip_fw <- precip %>%
  rename(CY= Year)

precip_fw$TDN_MGL_mm <- precip_fw$TDN_MGL * precip_fw$Rainfall_mm
precip_fw$TDN_mm <- ifelse(!is.na(precip_fw$TDN_MGL), precip_fw$Rainfall_mm, NA)

precip_fw$DOC_MGL_mm <- precip_fw$DOC_MGL * precip_fw$Rainfall_mm
precip_fw$DOC_mm <- ifelse(!is.na(precip_fw$DOC_MGL), precip_fw$Rainfall_mm, NA)

precip_fw$NO3_MGL_mm <- precip_fw$NO3_MGL * precip_fw$Rainfall_mm 
precip_fw$NO3_mm <- ifelse(!is.na(precip_fw$NO3_MGL), precip_fw$Rainfall_mm, NA)

precip_fw$NH4_UGL_mm <- precip_fw$NH4_UGL * precip_fw$Rainfall_mm 
precip_fw$NH4_mm <- ifelse(!is.na(precip_fw$NH4_UGL), precip_fw$Rainfall_mm, NA)

precip_fw$PO4_UGL_mm <- precip_fw$PO4_UGL * precip_fw$Rainfall_mm
precip_fw$PO4_mm <- ifelse(!is.na(precip_fw$PO4_UGL), precip_fw$Rainfall_mm, NA)

precip_fw$DON_MGL_mm <- precip_fw$DON_MGL_Final_MDL * precip_fw$Rainfall_mm
precip_fw$DON_mm <- ifelse(!is.na(precip_fw$DON_MGL_Final_MDL), precip_fw$Rainfall_mm, NA)

precip_fw$DIN_MGL_mm <- precip_fw$DIN_MGL * precip_fw$Rainfall_mm
precip_fw$DIN_mm <- ifelse(!is.na(precip_fw$DIN_MGL), precip_fw$Rainfall_mm, NA)
#_________________________________________________________________________________________________________________________________________________________
#CY: First sum columns by calendar year

#CY: Rainfall-weighted concentrations(FWC)
precip_cy_fwc <- precip_fw %>%
  select(- Field_Notes, -Lab_Notes) %>%
  filter(CY > 2007 & CY < 2024) %>%
  group_by(CY) %>%
  summarise_if(is.numeric, sum, na.rm =T) %>%
  select(CY, Rainfall_mm, TDN_MGL_mm:DIN_mm)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#WY: Repeat the summation of columns, now grouped by water year

#WY: Rainfall-weighted concentrations(FWC)
#precip_wy_fwc <- precip_fw %>%
#  select(-pH, - Field.Notes, -Lab_Notes) %>%
#  filter(WY < 2020) %>%
#  group_by(WY) %>%
#  summarise_if(is.numeric, sum, na.rm =T) %>%
 # select(WY, Rainfall_mm, TDN_MGL_mm:DIN_mm)

#_________________________________________________________________________________________________________________________________________________________
#Figure out how many samples per year
precip_cycount <- precip_fw %>%
  select(CY, Rainfall_mm, NH4_UGL:DOC_MGL, DON_MGL_Final_MDL, DIN_MGL) %>%
  filter(CY < 2024 & CY > 2007) %>%
  group_by(CY) %>%
  summarize(NH4_count = sum(!is.na(NH4_UGL)),
            NO3_count = sum(!is.na(NO3_MGL)),
            TDN_count = sum(!is.na(TDN_MGL)),
            DOC_count = sum(!is.na(DOC_MGL)),
            PO4_count = sum(!is.na(PO4_UGL)),
            DON_count = sum(!is.na(DON_MGL_Final_MDL)),
            DIN_count = sum(!is.na(DIN_MGL)))
#_________________________________________________________________________________________________________________________________________________________
#Finish calculation of flow-weighted concentrations across calendar & water years and months each year

#CY: Divide the summation of C*Rainfall by the summation of rainfall for each solute
precip_cy_finalfwc <- precip_cy_fwc %>%
  mutate(FW_TDN_MGL = TDN_MGL_mm/TDN_mm,
         FW_DOC_MGL = DOC_MGL_mm/DOC_mm,
         FW_NO3_MGL = NO3_MGL_mm/NO3_mm,
         FW_NH4_UGL = NH4_UGL_mm/NH4_mm,
         FW_PO4_UGL = PO4_UGL_mm/PO4_mm,
         FW_DON_MGL = DON_MGL_mm/DON_mm,
         FW_DIN_MGL = DIN_MGL_mm/DIN_mm)

write.csv(precip_cy_finalfwc, "results/main_precipitation_format/precip_fwc.csv")

#Table of Average Annual FWC of solutes for deposition
precip_fw_avg_annual <- precip_cy_finalfwc %>%
  select(FW_TDN_MGL:FW_DIN_MGL) %>%
  mutate(FW_NH4_MGL = conv_unit(FW_NH4_UGL, "ug", "mg")) %>%
  summarize(across(everything(), ~mean(.)))

print(round(precip_fw_avg_annual[,],3))

precip_fw_sd_annual <- precip_cy_finalfwc %>%
  select(FW_TDN_MGL:FW_DIN_MGL) %>%
  mutate(FW_NH4_MGL = conv_unit(FW_NH4_UGL, "ug", "mg")) %>%
  summarize(across(everything(), ~sd(.)))

print(round(precip_fw_sd_annual[,],2))

#CY: Calculate CY average concentration and compare to precip-weighted concentration 
precip_cy_avg_conc <- precip_fw %>%
  filter(CY > 2007 & CY < 2024) %>%
  group_by(CY) %>%
  summarize(across(NH4_UGL:DIN_MGL, \(x) mean(x, na.rm=TRUE)))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#WY: Divide the summation of C*Rainfall by the summation of rainfall for each solute
#precip_wy_finalfwc <- precip_wy_fwc %>%
 # mutate(FW_TDN_MGL = TDN_MGL_mm/TDN_mm,
  #       FW_DOC_MGL = DOC_MGL_mm/DOC_mm,
   #      FW_NO3_MGL = NO3_MGL_mm/NO3_mm,
    #     FW_NH4_UGL = NH4_UGL_mm/NH4_mm,
     #    FW_PO4_UGL = PO4_UGL_mm/PO4_mm,
      #   FW_DON_MGL = DON_MGL_mm/DON_mm,
       #  FW_DIN_MGL = DIN_MGL_mm/DIN_mm)

#WY: Calculate WY average concentration and compare to precip-weighted concentration
#precip_wy_Avg_Conc <- precip_fw %>%
 # group_by(WY) %>%
  #summarize(across(.cols=everything(), mean, na.rm=T))

#_________________________________________________________________________________________________________________________________________________________
#Calculate Solute Loads for Precipitation by multiplying precip-weighted annual concentration by total rainfall that year
#Resolve units as necessary

#CY: Product of Precip-Weighted Annual Concentration (mg/L) and Annual rainfall (mm)
#Calendar Year TF2 Rainfall Totals (mm rainfall)
cy_rainfall <- read.csv("./data/precipitation/CY_Rainfall_24update.csv") %>%
  select(CY= year, Rainfall_mm = total_rainfall_mm) %>%
  filter(CY >2007 & CY < 2024)

#Plot Rainfall
p_rainfall_cy <- ggplot(cy_rainfall, aes(CY, Rainfall_mm)) + geom_point(size=3) + 
  geom_line() +
  scale_x_continuous(breaks=seq(from=2008,to=2023,by=1)) + 
  scale_y_continuous(limits=c(0,1800), breaks=seq(from=0,to=1800, by=200))+ 
  theme_cowplot()
p_rainfall_cy 

#CY: Convert concentrations from mg/L or ug/L to kg/L
precip_cy_loads <- precip_cy_finalfwc %>%
  select(CY, FW_TDN_MGL:FW_DIN_MGL) %>%
  mutate(FW_TDN_KGL = conv_unit(FW_TDN_MGL, "mg", "kg"),
         FW_DOC_KGL = conv_unit(FW_DOC_MGL, "mg", "kg"),
         FW_NO3_KGL = conv_unit(FW_NO3_MGL, "mg", "kg"),
         FW_NH4_KGL = conv_unit(FW_NH4_UGL, "ug", "kg"),
         FW_PO4_KGL = conv_unit(FW_PO4_UGL, "ug", "kg"),
         FW_DON_KGL = conv_unit(FW_DON_MGL, "mg", "kg"),
         FW_DIN_KGL = conv_unit(FW_DIN_MGL, "mg", "kg"))

#CY: Join the precip-weighted concentrations dataframe with the annual rainfall dataframe by calendar year
precip_cy_loads <- full_join(precip_cy_loads, cy_rainfall, by = "CY")

#1L H20 = 1,000,000 mm^3 H20
#CY: Calculate Loads in kg/mm2/yr (divide by 1000000 mm3 to get kg/mm3 * mm/year = kg/mm2/year)
precip_cy_loads <- precip_cy_loads %>%
  mutate(TDN_kg_mm2_yr = (FW_TDN_KGL/1000000) * Rainfall_mm,
         DOC_kg_mm2_yr = (FW_DOC_KGL/1000000) * Rainfall_mm,
         NO3_kg_mm2_yr = (FW_NO3_KGL/1000000) * Rainfall_mm,
         NH4_kg_mm2_yr = (FW_NH4_KGL/1000000) * Rainfall_mm,
         PO4_kg_mm2_yr = (FW_PO4_KGL/1000000) * Rainfall_mm,
         DIN_kg_mm2_yr = (FW_DIN_KGL/1000000) * Rainfall_mm,
         DON_kg_mm2_yr = (FW_DON_KGL/1000000) * Rainfall_mm)

#CY: Convert Loads from kg/mm^2/yr to kg/ha/yr by multiplying by 1e^10 mm2 in 1 ha
precip_cy_loads <- precip_cy_loads %>%
  mutate(TDN_kg_ha_yr = TDN_kg_mm2_yr * (1*10^10),
         DOC_kg_ha_yr = DOC_kg_mm2_yr * (1*10^10),
         NO3_kg_ha_yr = NO3_kg_mm2_yr * (1*10^10),
         NH4_kg_ha_yr = NH4_kg_mm2_yr * (1*10^10),
         PO4_kg_ha_yr = PO4_kg_mm2_yr * (1*10^10),
         DIN_kg_ha_yr = DIN_kg_mm2_yr * (1*10^10),
         DON_kg_ha_yr = DON_kg_mm2_yr * (1*10^10))
#Unit conversion check
conv_unit(1, "mm2", "hectare")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#Water Year TF2 Rainfall Totals (mm rainfall)
#wy_rainfall <- read_excel("data/precipitation/TF2_CalYr_WaterYr_precipsum.xlsx", sheet =4) %>%
 # filter(WY > 2007 & WY < 2019)


#WY: Convert concentrations from mg/L or ug/L to kg/L
#precip_wy_loads <- precip_wy_finalfwc %>%
 # select(WY, FW_TDN_MGL:FW_DIN_MGL) %>%
#  mutate(FW_TDN_KGL = conv_unit(FW_TDN_MGL, "mg", "kg"),
 #        FW_DOC_KGL = conv_unit(FW_DOC_MGL, "mg", "kg"),
  #       FW_NO3_KGL = conv_unit(FW_NO3_MGL, "mg", "kg"),
  #      FW_NH4_KGL = conv_unit(FW_NH4_UGL, "ug", "kg"),
  #       FW_PO4_KGL = conv_unit(FW_PO4_UGL, "ug", "kg"),
  #       FW_DON_KGL = conv_unit(FW_DON_MGL, "mg", "kg"),
  #       FW_DIN_KGL = conv_unit(FW_DIN_MGL, "mg", "kg"))

#WY: Join the precip-weighted concentrations dataframe with the annual rainfall dataframe by water year

#precip_wy_loads <- full_join(precip_wy_loads, wy_rainfall, by = "WY")

#1L H20 = 1,000,000 mm^3 H20
#WY:Calculate Loads in kg/mm2/yr (divide by 1000000 mm3 to get kg/mm3 * mm/year = kg/mm2/year)

#precip_wy_loads <- precip_wy_loads %>%
 # mutate(TDN_kg_mm2_yr = (FW_TDN_KGL/1000000) * Rainfall_mm,
  #       DOC_kg_mm2_yr = (FW_DOC_KGL/1000000) * Rainfall_mm,
  #      NO3_kg_mm2_yr = (FW_NO3_KGL/1000000) * Rainfall_mm,
  #       NH4_kg_mm2_yr = (FW_NH4_KGL/1000000) * Rainfall_mm,
  #       PO4_kg_mm2_yr = (FW_PO4_KGL/1000000) * Rainfall_mm,
  #       DIN_kg_mm2_yr = (FW_DIN_KGL/1000000) * Rainfall_mm,
  #       DON_kg_mm2_yr = (FW_DON_KGL/1000000) * Rainfall_mm) # divide by 1000000 mm3 to get kg/mm3 * mm/year = kg/mm2/year

#WY: Convert Loads from kg/mm^2/yr to kg/ha/yr by multiplying by 1e^10 mm2 in 1 ha

#precip_wy_loads <- precip_wy_loads %>%
 # mutate(TDN_kg_ha_yr = TDN_kg_mm2_yr * (1*10^10),
  #       DOC_kg_ha_yr = DOC_kg_mm2_yr * (1*10^10),
  #       NO3_kg_ha_yr = NO3_kg_mm2_yr * (1*10^10),
  #      NH4_kg_ha_yr = NH4_kg_mm2_yr * (1*10^10),
  #       PO4_kg_ha_yr = PO4_kg_mm2_yr * (1*10^10),
  #       DIN_kg_ha_yr = DIN_kg_mm2_yr * (1*10^10),
  #      DON_kg_ha_yr = DON_kg_mm2_yr * (1*10^10))

#___________________________________________________________________________________________________________________________________________________________________________
#Create final tables of CY and WY loads

#Add in Great Bay area
#Google Earth Rough Area of Great Bay 1677.21 hectares

GB_Area_ha <- 1700

#CY: Loads table
colnames(precip_cy_loads)
precip_cy_loads_final <- precip_cy_loads %>%
  select(CY, Rainfall_mm,TDN_kg_ha_yr:DON_kg_ha_yr)

#Multiply normalized area by Great Bay surface area
precip_cy_loads_final <- precip_cy_loads_final %>%
  mutate(TDN_kg_yr = TDN_kg_ha_yr * GB_Area_ha,
         DOC_kg_yr = DOC_kg_ha_yr * GB_Area_ha,
         NO3_kg_yr = NO3_kg_ha_yr * GB_Area_ha,
         NH4_kg_yr = NH4_kg_ha_yr * GB_Area_ha,
         PO4_kg_yr = PO4_kg_ha_yr * GB_Area_ha,
         DIN_kg_yr = DIN_kg_ha_yr * GB_Area_ha,
         DON_kg_yr = DON_kg_ha_yr * GB_Area_ha)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#WY: Loads table
#precip_wy_loads_final <- precip_wy_loads %>%  select(WY, Rainfall_mm, TDN_kg_ha_yr:DON_kg_ha_yr)

#Multiply normalized area by Great Bay surface area

#precip_wy_loads_final <- precip_wy_loads_final %>%
 # mutate(TDN_kg_yr = TDN_kg_ha_yr * GB_Area_ha,
 #        DOC_kg_yr = DOC_kg_ha_yr * GB_Area_ha,
#         NO3_kg_yr = NO3_kg_ha_yr * GB_Area_ha,
#         NH4_kg_yr = NH4_kg_ha_yr * GB_Area_ha,
#         PO4_kg_yr = PO4_kg_ha_yr * GB_Area_ha,
#         DIN_kg_yr = DIN_kg_ha_yr * GB_Area_ha,
#         DON_kg_yr = DON_kg_ha_yr * GB_Area_ha) %>%
#  filter(WY < 2019)

#___________________________________________________________________________________________________________________________________________
#Plot loads over time

#Pivot the CY Loads table to be long

cy_loads_final_long <- precip_cy_loads_final %>%
  select(CY, TDN_kg_yr:DON_kg_yr) %>%
  pivot_longer(cols =  TDN_kg_yr:DON_kg_yr,
               names_to = "Solute",
               values_to = "Load_kg_yr")

plot_cy_loads <- ggplot(cy_loads_final_long, aes(CY, Load_kg_yr)) + geom_point(aes(color=Solute), size = 3) +
  scale_x_continuous(limits=c(2008,2023), breaks=seq(from=2008, to=2024, by= 1)) + 
  scale_color_viridis_d() +
  facet_wrap(~Solute, scales="free") + theme_cowplot() +
  theme(axis.text.x = element_text(angle=90, hjust=1))
plot_cy_loads

#Pivot the WY Loads table to be long

#wy_loads_final_long <- precip_wy_loads_final %>%
 # select(WY, TDN_kg_yr:DON_kg_yr) %>%
  #pivot_longer(cols =  TDN_kg_yr:DON_kg_yr,
   #            names_to = "Solute",
    #           values_to = "Load_kg_yr")

#_______________________________________________________________________________________________________________________________________________
#Average of each concentration over the decade long period of record

avg_conc <- precip_fw %>%
  select(NH4_UGL:DOC_MGL, DON_MGL_Final_MDL, DIN_MGL) %>%
  summarize_all(mean, na.rm=T) %>%
  pivot_longer(cols = c(NH4_UGL:DIN_MGL),
               names_to = "Parameter",
               values_to = "Mean")

avg_conc[,2] <- signif(avg_conc[,2],3)

sd_conc <- precip_fw %>%
  select(NH4_UGL:DOC_MGL, DON_MGL_Final_MDL, DIN_MGL) %>%
  summarize_all(sd, na.rm=T) %>%
  pivot_longer(cols = c(NH4_UGL:DIN_MGL),
               names_to = "Parameter",
               values_to = "SD")

count <- precip_fw %>%
  select(NH4_UGL:DOC_MGL, DON_MGL_Final_MDL, DIN_MGL) %>%
  summarize(across(everything(), ~ sum(!is.na(.)))) %>%
  pivot_longer(cols = c(NH4_UGL:DIN_MGL),
               names_to = "Parameter",
               values_to = "Count")

combined_data <- avg_conc %>%
  left_join(sd_conc, by = c("Parameter")) %>%
  left_join(count, by = c("Parameter")) %>%
  mutate(Result = paste0(signif(Mean, 2), " ± ", signif(SD, 2), " (", Count, ")")) %>%
  select(Parameter, Result) 

custom_order <- c("DOC_MGL","PO4_UGL", "NH4_UGL", "NO3_MGL", "DIN_MGL", "DON_MGL_Final_MDL", "TDN_MGL")
combined_data$name <- factor(combined_data$Parameter, levels = custom_order)

combined_data <- combined_data %>%
  arrange(name)

write.csv(combined_data, file=paste0("./results/manuscript_figures/supplemental/table_s1/avg_concentrations_supplemental_precip.csv")) #includes flow-weighted river data

#___________________________________________
#____________________________________________
#_______________________________________________

 #Monthly Loads
#___________________________________________________________________________________________________________________________________________________________________________
#Month-by-month precipitation loads (within a calendar year)
#Figure out how many samples per month
precip_fw$Month_end <- month(precip_fw$Collection_Date)

#Assign a DatetimeEnd (sample collect) DateTime begin (sample deploy)
precip_fw$DateTime.end = precip_fw$Collection_Date
precip_fw$DateTime.begin = lag(precip_fw$DateTime.end, 1)
precip_fw$Month_start <- month(precip_fw$DateTime.begin)

precip_fw <- precip_fw 


# Function to check if a date is the last day of the month
is_last_day_of_month <- function(date) {
  month(date) != month(date + days(1))
}

# Function to check if a date is the first day of the month
is_first_day_of_month <- function(date) {
  month(date) != month(date - days(1))
}

precip_fw$Month_start_a <-ifelse(
  is_last_day_of_month(precip_fw$DateTime.begin),
  format(precip_fw$DateTime.begin+ months(1), "%m"),
  NA
)

precip_fw$Month_end_a <- as.numeric(ifelse(
  is_first_day_of_month(precip_fw$DateTime.end),
  format(precip_fw$DateTime.end - months(1), "%m"),
  NA
))


precip_fw$Month_START <- ifelse(!is.na(precip_fw$Month_start_a),
                              precip_fw$Month_start_a, precip_fw$Month_start)

precip_fw$Month_END <- ifelse(!is.na(precip_fw$Month_end_a),
                                    precip_fw$Month_end_a, precip_fw$Month_end)

precip_fw$Month_END <- as.numeric(precip_fw$Month_END)
precip_fw$Month_START <- as.numeric(precip_fw$Month_START)

precip_fw$interval <- ifelse(precip_fw$Month_start != precip_fw$Month_end, "2", "1")
precip_fw$interval_rev <- ifelse(precip_fw$Month_START != precip_fw$Month_END, "2", "1")

precip_fw$MONTH <- ifelse(precip_fw$Month_START == precip_fw$Month_END, precip_fw$Month_END, precip_fw$Month_end)


precip_fw$MONTHCOMP <- ifelse(precip_fw$MONTH == precip_fw$Month_end, "SAME","DIFFERENT")

ggplot(subset(precip_fw, !is.na(MONTH) & CY > 2007), aes(MONTH, Rainfall_mm, color=as.factor(Month_end))) +
  geom_point(position=position_jitter(width=0.15), aes(shape=MONTHCOMP)) +
  scale_x_continuous(limits=c(0,12), breaks=seq(from=1,to=12,by=1)) +
  facet_wrap(~CY, scales="free")

precip_fw$Month <- month(precip_fw$Collection_Date)

precip_monthly_count <- precip_fw %>%
  select(CY, Month, Rainfall_mm, NH4_UGL:DOC_MGL, DON_MGL_Final_MDL, DIN_MGL) %>%
  filter(CY < 2024 & CY > 2007) %>%
  group_by(CY, Month) %>%
  summarize(NH4_count = sum(!is.na(NH4_UGL)),
            NO3_count = sum(!is.na(NO3_MGL)),
            TDN_count = sum(!is.na(TDN_MGL)),
            DOC_count = sum(!is.na(DOC_MGL)),
            PO4_count = sum(!is.na(PO4_UGL)),
            DON_count = sum(!is.na(DON_MGL_Final_MDL)),
            DIN_count = sum(!is.na(DIN_MGL)))

#Monthly Loads: Group by CY and Month and sum  
precip_month_fwc <- precip_fw %>%
  filter(CY > 2007 & CY < 2024) %>%
  group_by(CY, Month) %>%
  summarise_if(is.numeric, sum, na.rm =T) %>%
  select(CY, Month, Rainfall_mm, TDN_MGL_mm:DIN_mm)

#Month: Divide the summation of C*Rainfall by the summation of rainfall for each solute
precip_month_fwc_final <- precip_month_fwc %>%
  mutate(FW_TDN_MGL = TDN_MGL_mm/TDN_mm,
         FW_DOC_MGL = DOC_MGL_mm/DOC_mm,
         FW_NO3_MGL = NO3_MGL_mm/NO3_mm,
         FW_NH4_UGL = NH4_UGL_mm/NH4_mm,
         FW_PO4_UGL = PO4_UGL_mm/PO4_mm,
         FW_DON_MGL = DON_MGL_mm/DON_mm,
         FW_DIN_MGL = DIN_MGL_mm/DIN_mm)

average_monthly_fwc <- precip_month_fwc_final %>%
 filter(CY < 2024) 

summary(average_monthly_fwc)

#Month: Convert concentrations from mg/L or ug/L to kg/L
precip_month_loads <- precip_month_fwc_final %>%
  select(CY, Month, FW_TDN_MGL:FW_DIN_MGL) %>%
  mutate(FW_TDN_KGL = conv_unit(FW_TDN_MGL, "mg", "kg"),
         FW_DOC_KGL = conv_unit(FW_DOC_MGL, "mg", "kg"),
         FW_NO3_KGL = conv_unit(FW_NO3_MGL, "mg", "kg"),
         FW_NH4_KGL = conv_unit(FW_NH4_UGL, "ug", "kg"),
         FW_PO4_KGL = conv_unit(FW_PO4_UGL, "ug", "kg"),
         FW_DON_KGL = conv_unit(FW_DON_MGL, "mg", "kg"),
         FW_DIN_KGL = conv_unit(FW_DIN_MGL, "mg", "kg"))


#Add in Monthly Rainfall
monthly_precip_mm <- read.csv("data/precipitation/Monthly_Rainfall_24update.csv") %>%
  rename(Rainfall_mm = total_rainfall_mm, 
         Month = month,
         Year = year) %>% 
  select(-X)

monthly_precip_mm<- monthly_precip_mm %>%
  select(CY=Year, Month, Rainfall_mm)

#monthly_precip_mm$Month <- ifelse(monthly_precip_mm$Month == "JAN", 1, 
 #                                 ifelse(monthly_precip_mm$Month == "FEB", 2,
  #                                       ifelse(monthly_precip_mm$Month == "MAR", 3,
   #                                             ifelse(monthly_precip_mm$Month == "APR", 4,
    #                                                   ifelse(monthly_precip_mm$Month == "MAY", 5,
     #                                                         ifelse(monthly_precip_mm$Month == "JUN", 6,
      #                                                               ifelse(monthly_precip_mm$Month == "JUL", 7,
       #                                                                     ifelse(monthly_precip_mm$Month == "AUG", 8,
        #                                                                           ifelse(monthly_precip_mm$Month == "SEP", 9,
                                                                          #               ifelse(monthly_precip_mm$Month == "OCT", 10,
                                                                                          #       ifelse(monthly_precip_mm$Month == "NOV", 11,
                                                                                            #            ifelse(monthly_precip_mm$Month == "DEC", 12, NA))))))))))))


#Merge Monthly Rainfall with Monthly flow-weighted concentrations
colnames(precip_month_loads)
colnames(precip_month_loads)

precip_month_loads <- full_join(precip_month_loads, monthly_precip_mm, by=c("CY", "Month")) %>%
  select(CY, Month, FW_TDN_KGL:FW_DIN_KGL, Rainfall_mm) %>%
  filter(CY < 2024 & CY > 2007)

#1L H20 = 1,000,000 mm^3 H20
#Month:Calculate Loads in kg/mm2/month (divide by 1000000 mm3 to get kg/mm3 * mm/month = kg/mm2/month)
precip_month_loads <- precip_month_loads %>%
  mutate(TDN_kg_mm2_month = (FW_TDN_KGL/1000000) * Rainfall_mm,
         DOC_kg_mm2_month = (FW_DOC_KGL/1000000) * Rainfall_mm,
         NO3_kg_mm2_month = (FW_NO3_KGL/1000000) * Rainfall_mm,
         NH4_kg_mm2_month = (FW_NH4_KGL/1000000) * Rainfall_mm,
         PO4_kg_mm2_month = (FW_PO4_KGL/1000000) * Rainfall_mm,
         DIN_kg_mm2_month = (FW_DIN_KGL/1000000) * Rainfall_mm,
         DON_kg_mm2_month = (FW_DON_KGL/1000000) * Rainfall_mm) 

#Month: Convert Loads from kg/mm^2/month to kg/ha/month by multiplying by 1e^10 mm2 in 1 ha
precip_month_loads  <- precip_month_loads  %>%
  mutate(TDN_kg_ha_month = TDN_kg_mm2_month * (1*10^10),
         DOC_kg_ha_month = DOC_kg_mm2_month * (1*10^10),
         NO3_kg_ha_month = NO3_kg_mm2_month * (1*10^10),
         NH4_kg_ha_month = NH4_kg_mm2_month * (1*10^10),
         PO4_kg_ha_month = PO4_kg_mm2_month * (1*10^10),
         DIN_kg_ha_month = DIN_kg_mm2_month * (1*10^10),
         DON_kg_ha_month = DON_kg_mm2_month * (1*10^10))

#Month: Normalize by Great Bay Area
precip_month_loads_final <- precip_month_loads  %>%
  mutate(TDN_kg_month = TDN_kg_ha_month * GB_Area_ha,
         DOC_kg_month = DOC_kg_ha_month * GB_Area_ha,
         NO3_kg_month = NO3_kg_ha_month * GB_Area_ha,
         NH4_kg_month = NH4_kg_ha_month * GB_Area_ha,
         PO4_kg_month = PO4_kg_ha_month * GB_Area_ha,
         DIN_kg_month = DIN_kg_ha_month * GB_Area_ha,
         DON_kg_month = DON_kg_ha_month * GB_Area_ha)

precip_month_loads_final <- precip_month_loads_final %>%
  select(CY, Month, Rainfall_mm, TDN_kg_ha_month:DON_kg_ha_month, TDN_kg_month:DON_kg_month)


#Assume TN = TDN
precip_cy_loads_final$TN_kg_yr <- precip_cy_loads_final$TDN_kg_yr
precip_cy_loads_final$TN_kg_ha_yr <- precip_cy_loads_final$TDN_kg_ha_yr

#precip_wy_loads_final$TN_kg_yr <- precip_wy_loads_final$TDN_kg_yr
#precip_wy_loads_final$TN_kg_ha_yr <- precip_wy_loads_final$TDN_kg_ha_yr

precip_month_loads_final$TN_kg_month <- precip_month_loads_final$TDN_kg_month
precip_month_loads_final$TN_kg_ha_month <- precip_month_loads_final$TDN_kg_ha_month

#Write 3 csv files: WY Loads, CY Loads, and Monthly Loads

write.csv(precip_cy_loads_final, "results/main_precipitation_format/cy_precip_loads24.csv")
#write.csv(precip_wy_loads_final, "results/main_precipitation_format/wy_precip_loads.csv")
write.csv(precip_month_loads_final, "results/main_precipitation_format/month_precip_loads24.csv")

write.csv(cy_rainfall, "results/main_precipitation_format/CY_Rainfall.csv")

