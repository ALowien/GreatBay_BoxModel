# main_dataformat.R
# Biogeochemical Stressors and Ecological Response in Great Bay Estuary

# Author: Anna Mikulis, University of New Hampshire
# Last Updated: 8/23/2022

# Purpose: Read and process raw solute concentration data for the three tidal tributaries (head-of-tide monitoring stations) that flow into Great Bay.
  # Also process raw solute concentrations from the estuary, at Adams Point (high and low tide). Prepare discharge data for flux calculations.
  # Create physiochemical characteristics tables. 

# Data was sourced from the NH Department of Environmental Services, Environmental Monitoring Database (EMD). 
# Data was pulled for grab samples and physical chemistry based on assigned water body IDs.

#Data Dictionary

  #EMD data files include metadata on each water body of interest, sampling date and time, measured concentrations of nutrients, carbon, total suspended solids, 
    #tidal stage, and physio-chemical parameters. Physio-chemical parameters include snapshot measures of a dissolved oxygen, pH, temperature, and specific conductivity
  
  #Sites
    #05-LMP: Lamprey River (LMP)
    #09-EXT: Squamscott River (SQR)
    #09-EXT-DAMMED: Squamscott River pre dam removal in 2016
    #02-WNC: Winnicut River (WNC)
    #GRBAP: Great Bay Adams Point (Estuarine Monitoring Site)

#To update solute budgets, first download a recent copy of the NH DES EMD database for the above listed sites. 
#Place the new files into the Original_EMD subfolder (make sure to delete the older file - so as to not have duplicated data).
#Replace the discharge files with up-to-date versions.
#Run this script to format the EMD dataset and the discharge dataset for later scripts.

#Load required packages.

Packages <- c("readxl", "dplyr", "ggplot2", "tidyquant", "cowplot", "RColorBrewer",
              "tidyr","stringr",  "plotly", "measurements", "viridis", "moments")

lapply(Packages, library, character.only = TRUE)


# Import DES Site Data ------------------------------------------------
#Files have been amended to put columns names as the first row.
subdir <- "data/original_files/Original_EMD"
files <- list.files(path = subdir, pattern = ".xlsx", full.names = T)  
df.list <- lapply(files, read_excel)

df <- bind_rows(df.list)
unique(df$'STATION ID')
colnames(df)

#Replace spaces in column names with "_"
names(df) <- gsub(" ", "_", names(df))
names(df) <- gsub("/", "_", names(df))
#Convert START_DATE column to date class
df$START_DATE <- as.Date(df$START_DATE)

#Select for columns of interest; filter for sites of interest and time period of interest
df <- df %>%
  select(STATION_ID:SAMPLE_SIZE) %>%
  filter(STATION_ID == "05-LMP" | STATION_ID == "02-WNC" | STATION_ID == "09-EXT" | STATION_ID == "09-EXT-DAMMED" |
           STATION_ID == "GRBAP") %>%
  filter(SAMPLE_COLLECTION_METHOD_ID == "TIDALWQ") %>% #Project Name for Tidal Tribs and Adams Point Sampling
  filter(START_DATE > "2008-01-01")

unique(df$STATION_ID)

#Distinguish GRBAP sampling by low and high tide events
AP_Tide <- df %>%
  select(STATION_ID, START_DATE, START_TIME, PARAMETER_ANALYTE, QUALIFIER_AND_RESULTS) %>%
  filter(STATION_ID == "GRBAP") %>%
  filter(PARAMETER_ANALYTE == "TIDE STAGE") %>%
  filter(!is.na(QUALIFIER_AND_RESULTS))

#Categorize "Tide Stage" PARAMETER To be either High or Low Tide (e.x. ebb tides are re-classified as low tide samples)
AP_Tide$QUALIFIER_AND_RESULTS <- ifelse(AP_Tide$PARAMETER_ANALYTE == "TIDE STAGE" & AP_Tide$QUALIFIER_AND_RESULTS == "EBB", "LOW", AP_Tide$QUALIFIER_AND_RESULTS)
AP_Tide$QUALIFIER_AND_RESULTS <- ifelse(AP_Tide$PARAMETER_ANALYTE == "TIDE STAGE" & AP_Tide$QUALIFIER_AND_RESULTS == "FLOOD", "HIGH", AP_Tide$QUALIFIER_AND_RESULTS)

AP_Tide <- AP_Tide %>%
  select(-PARAMETER_ANALYTE)

#Rename Great Bay Adams Point (GRBAP) STATION ID based on high/low tide (GRBAPL indicates low tide sample; GRBAPH indicates high tide sample)
AP_Tide$STATION_ID <- ifelse(AP_Tide$QUALIFIER_AND_RESULTS == "LOW", "GRBAPL", "GRBAPH")

#Remove duplicates
deduped.APTIDE <- AP_Tide[!duplicated(AP_Tide[,1:4]),]

deduped.APTIDE <- deduped.APTIDE %>%
  select(-QUALIFIER_AND_RESULTS)

#Join df with deduped.APTIDE
#Replace Station ID in AP data frame
df <- left_join(df, deduped.APTIDE, by = c("START_DATE", "START_TIME"))

df$STATION_ID <- ifelse(!is.na(df$STATION_ID.y), df$STATION_ID.y, df$STATION_ID.x) 

df <- df %>%
  select(-STATION_ID.x, -STATION_ID.y)

#Couple of missing High and Low Tide IDS
df$STATION_ID <- ifelse(df$START_DATE == "2008-06-11" & df$START_TIME == "09:35", "GRBAPH",
                            ifelse(df$START_DATE == "2009-06-29" & df$START_TIME == "08:31", "GRBAPL", 
                                   ifelse(df$START_DATE == "2009-07-13" & df$START_TIME == "14:17", "GRBAPH",
                                          ifelse(df$START_DATE == "2011-08-22" & df$START_TIME == "07:10", "GRBAPH", df$STATION_ID))))

colnames(df)

### END Import DES Data ###
#### Resolve Variable and Column Header Names ####

#Fix columns that aren't numeric, but should be
df$RDL <- as.numeric(df$RDL)
df$METHOD_DETECTION_LIMIT <- as.numeric(df$METHOD_DETECTION_LIMIT)

#Dam removal on Squamscott River occurred in 2016; resulting in two different site ids depending on whether sampling occurred before or after the dam removal
#Fix station ID names, 09-EXT and 09-EXT-DAMMED are the same site, but differ over time
df$STATION_ID <- ifelse(df$STATION_ID == "09-EXT-DAMMED", "09-EXT", df$STATION_ID)

#Subset for columns of interest
df <- df %>%
  select(STATION_ID, WATERBODY_ID, RIVER_NAME, ACTIVITY_TYPE, START_DATE:FRACTION_TYPE)

#Delete the 119 occurrences where results are not valid 
df %>% count(RESULT_VALID)

df <- df %>%
  filter(RESULT_VALID == "Y" | is.na(RESULT_VALID))

#Remove result valid column and filter out unnecessary parameters for the box model
remove_parms <- c("CLOSTRIDIUM PERFRINGENS", "ENTEROCOCCUS", "ESCHERICHIA COLI","TOTAL FECAL COLIFORM", "WIND DIRECTION", "WIND VELOCITY", "SECCHI DISK TRANSPARENCY","COLORED DISSOLVED ORGANIC MATTER (CDOM)", "TURBIDITY","DEPTH", "TIDE STAGE")

df <- subset(df, !(PARAMETER_ANALYTE %in% remove_parms))

df <- df %>%
  select(STATION_ID:ACTIVITY_COMMENTS, PARAMETER = PARAMETER_ANALYTE, QUALIFIER_AND_RESULTS:FRACTION_TYPE)

#For instances where result is NA instead of 1/2 of method detection limit, run this line of code
df$QUALIFIER_AND_RESULTS <- ifelse(is.na(df$QUALIFIER_AND_RESULTS), df$RDL/2, df$QUALIFIER_AND_RESULTS)

#Figure out Parameter Methods and Rename to clarify

#PHOSPHORUS AS P, fraction type is "Total"; which means PHOSPHORUS samples are TOTAL PHOSPHORUS ("TP")
df$PARAMETER <- ifelse(df$PARAMETER == "PHOSPHORUS AS P", "TP", df$PARAMETER)

#PHOSPHORUS, ORTHOPHOSPHATE AS P is PO4 molecule
df$PARAMETER <- ifelse(df$PARAMETER == "PHOSPHORUS, ORTHOPHOSPHATE AS P", "PO4", df$PARAMETER)

#NITROGEN Method is for TOTAL NITROGEN
df$PARAMETER <- ifelse(df$PARAMETER == "NITROGEN", "TN", df$PARAMETER)

#NITROGEN, DISSOLVED is "TOTAL DISSOLVED NITROGEN" (TDN)
df$PARAMETER <- ifelse(df$PARAMETER == "NITROGEN, DISSOLVED", "TDN", df$PARAMETER)

#NITROGEN, AMMONIA as N - is technically ammonium (the method measures ammonia, but there isn't ammonia in streams really)
#Per conversation with Jody over Slack July 2020
df$PARAMETER <- ifelse(df$PARAMETER == "NITROGEN, AMMONIA AS N", "NH4", df$PARAMETER)

#NITROGEN, INORGANIC (AMMONIA, NITRATE AND NITRITE) is Dissolved Inorganic Nitrogen (DIN)
df$PARAMETER <- ifelse(df$PARAMETER == "NITROGEN, INORGANIC (AMMONIA, NITRATE AND NITRITE)", "DIN", df$PARAMETER)

#"NITROGEN, NITRITE (NO2) + NITRATE (NO3) AS N"  is NO3+NO2
df$PARAMETER <- ifelse(df$PARAMETER == "NITROGEN, NITRITE (NO2) + NITRATE (NO3) AS N", "NO3_NO2", df$PARAMETER)

#"NITROGEN, NITRITE (NO2) AS N" is really nitrate
df$PARAMETER <- ifelse(df$PARAMETER == "NITROGEN, NITRITE (NO2) AS N", "NO3", df$PARAMETER)

#"NITROGEN, ORGANIC" is DON, calculated as TDN-DIN
df$PARAMETER <- ifelse(df$PARAMETER == "NITROGEN, ORGANIC", "DON", df$PARAMETER)

#NITROGEN, SUSPENDED is Particulate N
df$PARAMETER <- ifelse(df$PARAMETER == "NITROGEN, SUSPENDED", "PN", df$PARAMETER)

#Carbon, SUSPENDED is Particulate Carbon
df$PARAMETER <- ifelse(df$PARAMETER == "CARBON, SUSPENDED", "PC", df$PARAMETER)

#CARBON, ORGANIC is Dissolved Organic Carbon
df$PARAMETER <- ifelse(df$PARAMETER == "CARBON, ORGANIC", "DOC", df$PARAMETER)

#Misc. condensing of Parameter Names to eliminate spaces
df$PARAMETER <- ifelse(df$PARAMETER == "LIGHT ATTENUATION COEFFICIENT", "Light_Atten_Coeff", 
                               ifelse(df$PARAMETER == "DISSOLVED OXYGEN SATURATION", "DO_sat", 
                                      ifelse(df$PARAMETER == "TEMPERATURE WATER", "Temp_Water", df$PARAMETER)))

df$PARAMETER <- ifelse(df$PARAMETER == "SPECIFIC CONDUCTANCE", "SPC", 
                        ifelse(df$PARAMETER == "DISSOLVED OXYGEN", "DO", 
                               ifelse(df$PARAMETER == "SOLIDS, SUSPENDED", "TSS", 
                                      ifelse(df$PARAMETER == "TIDE STAGE", "Tide_Stage", df$PARAMETER))))

df$PARAMETER <- ifelse(df$PARAMETER == "CHLOROPHYLL A, CORRECTED FOR PHEOPHYTIN", "CHLA_corrected_pheophytin",
                        ifelse(df$PARAMETER == "SILICA AS SIO2", "SIO2", df$PARAMETER))
                      
unique(df$PARAMETER)

#Look at fraction types measured
unique(df$FRACTION_TYPE)

#Filter for Dissolved, Suspended, and Total Fractions (representative of dissolved and particulate forms of nutrients/carbon)
df <- df %>%
  filter(FRACTION_TYPE == "DISSOLVED" | FRACTION_TYPE == "SUSPENDED" | FRACTION_TYPE == "TOTAL" | is.na(FRACTION_TYPE))

#Combine Analytical Method and Source Method ID columns
df <- df %>%
  unite("Analytical_Method", ANALYTICAL_METHOD_SOURCE_ID:ANALYTICAL_METHOD, sep = "_")

#Combine detection limit and comments columns
df %>% count(DETECTION_LIMIT_COMMENTS)
df <- df %>%
  unite("Result_DL", RDL:DETECTION_LIMIT_COMMENTS, na.rm=TRUE, sep = "_")

df %>% count(Result_DL)

### END RESOLVE VARIABLE AND COLUMN HEADER NAMES ###
#### Tidy Results (saved intermediate step)####

#Remove additional columns, now that we've filtered down to desired project and sites
df <- df %>%
  select(-WATERBODY_ID, -SAMPLE_COLLECTION_METHOD_ID)

#Activity Comments Review and Assessment
unique(df$ACTIVITY_COMMENTS) #Activity Comments are mostly UNH IDs and weather at GRBAP - removal of column for purposes of solute analysis is OK

unique(df$ACTIVITY_TYPE) # Removing duplicates to see if that fixes spread issue)

df <- df %>%
  select(-ACTIVITY_COMMENTS, - FRACTION_TYPE) %>%
  filter(ACTIVITY_TYPE == "SAMPLE - ROUTINE")

#How many instances of each parameter being measured?
df_count <- df%>% count(df$PARAMETER)

#Get ride of "<" from the QUALIFIER AND RESULTS Column
df$RESULTS_clean <- stringr::str_replace(df$QUALIFIER_AND_RESULTS, '\\<', '')

#ID those 226 rows that had an "<" so that we know the value is less than the detection limit
df <- df %>% 
  mutate(DETECTION_LIMIT = if_else(str_starts(QUALIFIER_AND_RESULTS, "<"), "BELOW", "NA")) #Below == "<"

df %>% count(df$DETECTION_LIMIT) #226 occurrences where measurement is recorded as being less than/equal to instrument detection limit

df$RESULTS_clean <- as.numeric(df$RESULTS_clean) # this converts everything to numbers

#If result is below detection limit, set to 1/2 of detection limit
#226 results that are below detection limit

df$RESULT <- ifelse(df$DETECTION_LIMIT == "BELOW", df$RESULTS_clean / 2, df$RESULTS_clean)

#Compare results to results_clean
df$Results_Comp <- df$RESULTS_clean - df$RESULT

### END TIDY RESULTS ###
#_____________________________________________________________________________________________________________

#### Convert Dataframe from Long to Wide ####
#Thin out to columns necessary for converting from long to wide
df <- df %>%
  select(STATION_ID, START_DATE, PARAMETER, UNITS, RESULT) %>%
  unite("PARAMETER", PARAMETER:UNITS, sep= "_")

unique(df$PARAMETER)
#Unit Conversions
#convert all ug/L to mg/L

#NH4 reported in MG/L and UG/L; convert the MG/L to UG/L
df$RESULT <- ifelse(df$PARAMETER == "NH4_MG/L", conv_unit(df$RESULT, "mg", "ug"), df$RESULT)
df$PARAMETER <- ifelse(df$PARAMETER == "NH4_MG/L", "NH4_UG/L", df$PARAMETER)

df$RESULT <- ifelse(df$PARAMETER == "PO4_MG/L", conv_unit(df$RESULT, "mg", "ug"), df$RESULT)
df$PARAMETER <- ifelse(df$PARAMETER == "PO4_MG/L", "PO4_UG/L", df$PARAMETER)

df$RESULT <- ifelse(df$PARAMETER == "TP_MG/L", conv_unit(df$RESULT, "mg", "ug"), df$RESULT)
df$PARAMETER <- ifelse(df$PARAMETER == "TP_MG/L", "TP_UG/L", df$PARAMETER)

#units for SPC are the same 1 US/CM = 1 UMHO/CM
df$PARAMETER <- ifelse(df$PARAMETER == "SPC_US/CM", "SPC_UMHO/CM", df$PARAMETER)

unique(df$PARAMETER)

# Replace "/" with "_"

df_count <- df %>% count(PARAMETER)

df$PARAMETER <- ifelse(df$PARAMETER == "TP_UG/L", "TP_UGL", 
                        ifelse(df$PARAMETER == "TN_MG/L", "TN_MGL", df$PARAMETER))

df$PARAMETER <- ifelse(df$PARAMETER == "TSS_MG/L", "TSS_MGL", 
                        ifelse(df$PARAMETER == "PH_NONE", "pH",
                               ifelse(df$PARAMETER == "DO_sat_%", "DO_sat", 
                                      ifelse(df$PARAMETER == "SPC_UMHO/CM", "SPC_UMHO_CM", df$PARAMETER))))

df$PARAMETER <- ifelse(df$PARAMETER == "DO_MG/L", "DO_MGL", 
                        ifelse(df$PARAMETER == "Temp_Water_DEG C", "TEMP_WATER_DEGC", 
                               ifelse(df$PARAMETER == "TDN_MG/L", "TDN_MGL", df$PARAMETER)))

df$PARAMETER <- ifelse(df$PARAMETER == "NH4_UG/L", "NH4_UGL", 
                        ifelse(df$PARAMETER == "NO3_NO2_MG/L", "NO3_NO2_MGL", df$PARAMETER))

df$PARAMETER <- ifelse(df$PARAMETER == "PO4_UG/L", "PO4_UGL", 
                        ifelse(df$PARAMETER == "DON_MG/L", "DON_MGL", 
                               ifelse(df$PARAMETER == "DOC_MG/L", "DOC_MGL", 
                                      ifelse(df$PARAMETER == "PN_MG/L", "PN_MGL", df$PARAMETER))))

df$PARAMETER <- ifelse(df$PARAMETER == "DIN_MG/L", "DIN_MGL",
                        ifelse(df$PARAMETER == "CHLA_corrected_pheophytin_UG/L", "CHLA_corr_pheo_UGL", 
                               ifelse(df$PARAMETER == "NO3_MG/L", "NO3_MGL", 
                                      ifelse(df$PARAMETER == "Tide_Stage_NA", "Tide_Stage", df$PARAMETER))))

df$PARAMETER <- ifelse(df$PARAMETER == "SIO2_MG/L","SIO2_MGL", 
                        ifelse(df$PARAMETER == "PC_MG/L", "PC_MGL", 
                               ifelse(df$PARAMETER == "PHEOPHYTIN-A_UG/L", "PHEOPHYTIN_A_UGL", 
                                      ifelse(df$PARAMETER == "Light_Atten_Coeff_1/M", "Light_Attten_1_m", df$PARAMETER))))


df$START_DATE <- as.Date(df$START_DATE)

unique(df$PARAMETER)

#___________________________________Make data frame wide instead of long __________________________________________________________

df <- df %>%
  pivot_wider(names_from = PARAMETER, values_from = RESULT, 
              values_fn = mean)

#Reorganize columns in df
colnames(df)

df <- df %>%
  select(STATION_ID, START_DATE, TP_UGL, PO4_UGL, PN_MGL, TN_MGL, TDN_MGL, NH4_UGL, NO3_MGL, NO3_NO2_MGL, DIN_MGL, 
         DON_MGL, DOC_MGL, PC_MGL, SIO2_MGL, TSS_MGL:DO_sat, DO_MGL, SPC_UMHO_CM, SALINITY_PSS, TEMP_WATER_DEGC, CHLA_corr_pheo_UGL,
         PHEOPHYTIN_A_UGL, Light_Attten_1_m)


df$TP_MGL <- conv_unit(df$TP_UGL, "ug", "mg")
df$PO4_MGL <- conv_unit(df$PO4_UGL, "ug", "mg")
df$NH4_MGL <- conv_unit(df$NH4_UGL, "ug", "mg")

df <- df %>%
  select(STATION_ID, START_DATE, TP_MGL, PO4_MGL, PN_MGL, TN_MGL, TDN_MGL, NH4_MGL, NO3_MGL, NO3_NO2_MGL, DIN_MGL, 
         DON_MGL, DOC_MGL, PC_MGL, SIO2_MGL, TSS_MGL:DO_sat, DO_MGL, SPC_UMHO_CM, SALINITY_PSS, TEMP_WATER_DEGC, CHLA_corr_pheo_UGL,
         PHEOPHYTIN_A_UGL, Light_Attten_1_m)

#Summarize physicochemical parameter columns for Appendix Tables
df_DO <- df %>%
  group_by(STATION_ID) %>%
  summarize(mean_DO_sat = mean(DO_sat, na.rm=T), sd_DO_sat = sd(DO_sat, na.rm=T), count = n())

df_DOmgl <- df %>%
  group_by(STATION_ID) %>%
  summarize(mean_DO_mgl = mean(DO_MGL, na.rm=T), sd_DO_mgl = sd(DO_MGL, na.rm=T), count = n())

df_temp <- df %>%
  group_by(STATION_ID) %>%
  summarize(mean_temp = mean(TEMP_WATER_DEGC, na.rm=T), sd_temp = sd(TEMP_WATER_DEGC, na.rm=T), count = n())


#Calculate DIN and DON in instances where it is missing
df$DIN_MGL_calc <- df$NH4_MGL + df$NO3_NO2_MGL
df$DON_MGL_calc <- df$TDN_MGL - df$NO3_NO2_MGL - df$NH4_MGL

#METHOD DETECTION LIMITS FOR DON (b/c not included in EMD)
df$DON_MDL <- (df$TDN_MGL + df$NO3_NO2_MGL + df$NH4_MGL) * 0.05

#Is DON below MDL?
df$DON_B_MDL <- ifelse(df$DON_MGL_calc < df$DON_MDL, "BDL", "G") 

length(which(df$DON_B_MDL == "BDL")) #9 instances below detection limit

#Set those DON values below detection limit to 1/2 of the method detection limit

df$DON_MGL_calc_final <- ifelse(df$DON_MGL_calc < df$DON_MDL, df$DON_MDL/2, df$DON_MGL_calc)


df <- df %>%
  select(STATION_ID:NO3_NO2_MGL, DIN_MGL = DIN_MGL_calc, DON_MGL = DON_MGL_calc_final, DOC_MGL:Light_Attten_1_m)
#this is now corrected for MDLs across all solutes

### END Convert Dataframe from Long to Wide ###
#assess normality of TSS values
skewness(df$TSS_MGL, na.rm=T)
kurtosis(df$TSS_MGL, na.rm=T)

#Remove the three high TSS concentrations at Adams Point Low Tide due to anecdotal knowledge from Jackson Estuarine Laboratory that winter values are worse for TSS b/c dock is out of the water
df <- df %>%
 mutate(TSS_MGL = ifelse(START_DATE == "2010-03-29" & STATION_ID == "GRBAPL", NA, TSS_MGL)) %>%
  mutate(TSS_MGL = ifelse(START_DATE == "2008-11-25" & STATION_ID == "GRBAPL", NA, TSS_MGL)) %>%
  mutate(TSS_MGL = ifelse(START_DATE == "2012-01-30" & STATION_ID == "GRBAPL", NA, TSS_MGL))
  
skewness(df$TSS_MGL, na.rm=T) #notable improvement in skewness from 12.6 to 2.1
kurtosis(df$TSS_MGL, na.rm=T) #notable improvement in kurtosis from 224 to 11

#### Discharge Data Formatting ####
#______________________________________________________________________________________________________________________________________
#________________________________________________________________________________________________________________________________________
#___________________________________________________________________________________________________________________________________________
#Format Discharge Data from USGS Gauges
#Read in discharge data from the three USGS gauges
LR_Q <- read.csv("data/Discharge/Daily_Mean/LR_Q.csv")
SQR_Q <- read.csv("data/Discharge/Daily_Mean/SQR_Q.csv")
WNC_Q <- read.csv("data/Discharge/Daily_Mean/WNC_Q.csv")

#QAQC KEY
#A approved for publication
#P provisional, subject to revision
#e Value has been estimated

#Join all the discharge data frames
Q <- union(LR_Q, SQR_Q)
Q <- union(Q, WNC_Q)

#Rename site numbers to station IDS
names(Q)[names(Q)== "site_no"] <- "STATION_ID"

Q <- Q %>% 
  select(STATION_ID, START_DATE, Q_mean_cfs, QAQC)

#Rename Stations to Sampling ID Name
Q$STATION_ID <- ifelse(Q$STATION_ID == 1073500, "05-LMP", ifelse(
  Q$STATION_ID == 1073587, "09-EXT", ifelse(
    Q$STATION_ID == 1073785, "02-WNC", NA)))

#Convert flow to daily average in cubic m/s
Q$flow <- conv_unit(Q$Q_mean_cfs, "ft3", "m3")

#Create a new date time column that is POSIXct format from the START_DATE column
Q$datetime <- as.POSIXct(Q$START_DATE, format ="%Y-%m-%d")
Q$datetime <- as.POSIXct(Q$datetime, format = "%Y-%m-%d")

#Write csv of discharge
write.csv(Q, "results/main_dataformat/Q_tidal_tribs.csv")

#Average + standard deviation of each solute for each river
avg_conc <- df %>%
  group_by(STATION_ID) %>%
  summarize(across(TP_MGL:TEMP_WATER_DEGC, mean, na.rm=T))

write.csv(avg_conc, "results/main_dataformat/avg_solute_conc_site.csv")

std_conc <- df %>%
  group_by(STATION_ID) %>%
  summarize(across(TP_MGL:TEMP_WATER_DEGC, sd, na.rm=T))

write.csv(std_conc, "results/main_dataformat/std_solute_conc_site.csv")
 
#This data frame has final solute concentrations that can be used for further load analysis
write.csv(df, "results/main_dataformat/df_conc.csv")
