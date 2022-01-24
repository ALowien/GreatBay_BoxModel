# main_dataformat.R
# Biogeochemical Stressors and Ecological Response in Great Bay Estuary

# Author: Anna Lowien, University of New Hampshire
# Last Updated: 1/24/2022

# Purpose: Read and process raw solute concentration data for the three tidal tributaries (head-of-tide monitoring stations) that flow into Great Bay.
  # Also process raw solute concentrations from the estuary, at Adams Point (high and low tide). Prepare discharge data for flux calculations.
  # Create physiochemical parameter appendix tables. 

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

#Replace spaces in colnames with "_"
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

#Fix "Tide Stage" PARAMETER To be either High or Low Tide
AP_Tide$QUALIFIER_AND_RESULTS <- ifelse(AP_Tide$PARAMETER_ANALYTE == "TIDE STAGE" & AP_Tide$QUALIFIER_AND_RESULTS == "EBB", "LOW", AP_Tide$QUALIFIER_AND_RESULTS)
AP_Tide$QUALIFIER_AND_RESULTS <- ifelse(AP_Tide$PARAMETER_ANALYTE == "TIDE STAGE" & AP_Tide$QUALIFIER_AND_RESULTS == "FLOOD", "HIGH", AP_Tide$QUALIFIER_AND_RESULTS)

AP_Tide <- AP_Tide %>%
  select(-PARAMETER_ANALYTE)

#Rename Great Bay Adams Point (GRBAP) STATION ID based on high/low tide
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

#Dam removal on Squamscott occurred in 2016
#Fix station ID names, 09-EXT and 09-EXT-DAMMED are the same site, but differ over time
df$STATION_ID <- ifelse(df$STATION_ID == "09-EXT-DAMMED", "09-EXT", df$STATION_ID)

unique(df$STATION_ID) #check for 05-LMP, 02-WNC, 09-EXT, and GRBAPH and GRBAPL

#Removing blank/unnecessary columns from data frame
colnames(df)

df <- df %>%
  select(STATION_ID, WATERBODY_ID, RIVER_NAME, ACTIVITY_TYPE, START_DATE:FRACTION_TYPE)

colnames(df)

unique(df$RESULT_VALID)

df %>% count(RESULT_VALID)

#Delete the 119 occurrences where results are not valid 
df <- df %>%
  filter(RESULT_VALID == "Y" | is.na(RESULT_VALID))


#Remove result valid column and filter out unnecessary parameters
df <- df %>%
  select(-RESULT_VALID) %>%
  filter(PARAMETER_ANALYTE != "CLOSTRIDIUM PERFRINGENS" & PARAMETER_ANALYTE != "ENTEROCOCCUS") %>%
  filter(PARAMETER_ANALYTE != "ESCHERICHIA COLI" & PARAMETER_ANALYTE != "TOTAL FECAL COLIFORM") %>%
  filter(PARAMETER_ANALYTE != "WIND DIRECTION" & PARAMETER_ANALYTE  != "WIND VELOCITY" & PARAMETER_ANALYTE != "SECCHI DISK TRANSPARENCY") %>%
  filter(PARAMETER_ANALYTE != "COLORED DISSOLVED ORGANIC MATTER (CDOM)" & 
           PARAMETER_ANALYTE != "TURBIDITY" & 
           PARAMETER_ANALYTE != "DEPTH") %>%
  filter(PARAMETER_ANALYTE != "TIDE STAGE")


unique(df$PARAMETER_ANALYTE)

df %>% count(PARAMETER_ANALYTE)

df <- df %>%
  select(STATION_ID:ACTIVITY_COMMENTS, PARAMETER = PARAMETER_ANALYTE, QUALIFIER_AND_RESULTS:FRACTION_TYPE)

#Fix the issue where NH DES starting putting NA instead of 1/2 of MDL
sum(is.na(df$QUALIFIER_AND_RESULTS)) #22 NAs that should be 1/2 of the MDL

df$QUALIFIER_AND_RESULTS <- ifelse(is.na(df$QUALIFIER_AND_RESULTS), df$RDL/2, df$QUALIFIER_AND_RESULTS)

sum(is.na(df$QUALIFIER_AND_RESULTS)) 

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

df %>% count(FRACTION_TYPE)

#6 samples measured for Volatile Solids at 09-EXT-DAMMED; not needed
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

#saving df as an intermediate step; 
#at this point dataframe character issues are resolved and results below MDL are set to 1/2 the MDL
write.csv(df, "results/main_dataformat/df.csv")
### END TIDY RESULTS ###
#_____________________________________________________________________________________________________________

#### Convert Dataframe from Long to Wide ####
df <- read.csv("results/main_dataformat/df.csv")

df <- df %>%
  select(-X)

#Thin out to columns necessary for converting from long to wide
df5 <- df %>%
  select(STATION_ID, START_DATE, PARAMETER, UNITS, RESULT) %>%
  unite("PARAMETER", PARAMETER:UNITS, sep= "_")

unique(df5$PARAMETER)
#Unit Conversions
#convert all ug/L to mg/L

#NH4 reported in MG/L and UG/L; convert the MG/L to UG/L
df5$RESULT <- ifelse(df5$PARAMETER == "NH4_MG/L", conv_unit(df5$RESULT, "mg", "ug"), df5$RESULT)
df5$PARAMETER <- ifelse(df5$PARAMETER == "NH4_MG/L", "NH4_UG/L", df5$PARAMETER)

df5$RESULT <- ifelse(df5$PARAMETER == "PO4_MG/L", conv_unit(df5$RESULT, "mg", "ug"), df5$RESULT)
df5$PARAMETER <- ifelse(df5$PARAMETER == "PO4_MG/L", "PO4_UG/L", df5$PARAMETER)

df5$RESULT <- ifelse(df5$PARAMETER == "TP_MG/L", conv_unit(df5$RESULT, "mg", "ug"), df5$RESULT)
df5$PARAMETER <- ifelse(df5$PARAMETER == "TP_MG/L", "TP_UG/L", df5$PARAMETER)

#units for SPC are the same 1 US/CM = 1 UMHO/CM
df5$PARAMETER <- ifelse(df5$PARAMETER == "SPC_US/CM", "SPC_UMHO/CM", df5$PARAMETER)

unique(df5$PARAMETER)

# Replace "/" with "_"

df5_count <- df5 %>% count(PARAMETER)

df5$PARAMETER <- ifelse(df5$PARAMETER == "TP_UG/L", "TP_UGL", 
                        ifelse(df5$PARAMETER == "TN_MG/L", "TN_MGL", df5$PARAMETER))

df5$PARAMETER <- ifelse(df5$PARAMETER == "TSS_MG/L", "TSS_MGL", 
                        ifelse(df5$PARAMETER == "PH_NONE", "pH",
                               ifelse(df5$PARAMETER == "DO_sat_%", "DO_sat", 
                                      ifelse(df5$PARAMETER == "SPC_UMHO/CM", "SPC_UMHO_CM", df5$PARAMETER))))

df5$PARAMETER <- ifelse(df5$PARAMETER == "DO_MG/L", "DO_MGL", 
                        ifelse(df5$PARAMETER == "Temp_Water_DEG C", "TEMP_WATER_DEGC", 
                               ifelse(df5$PARAMETER == "TDN_MG/L", "TDN_MGL", df5$PARAMETER)))

df5$PARAMETER <- ifelse(df5$PARAMETER == "NH4_UG/L", "NH4_UGL", 
                        ifelse(df5$PARAMETER == "NO3_NO2_MG/L", "NO3_NO2_MGL", df5$PARAMETER))

df5$PARAMETER <- ifelse(df5$PARAMETER == "PO4_UG/L", "PO4_UGL", 
                        ifelse(df5$PARAMETER == "DON_MG/L", "DON_MGL", 
                               ifelse(df5$PARAMETER == "DOC_MG/L", "DOC_MGL", 
                                      ifelse(df5$PARAMETER == "PN_MG/L", "PN_MGL", df5$PARAMETER))))

df5$PARAMETER <- ifelse(df5$PARAMETER == "DIN_MG/L", "DIN_MGL",
                        ifelse(df5$PARAMETER == "CHLA_corrected_pheophytin_UG/L", "CHLA_corr_pheo_UGL", 
                               ifelse(df5$PARAMETER == "NO3_MG/L", "NO3_MGL", 
                                      ifelse(df5$PARAMETER == "Tide_Stage_NA", "Tide_Stage", df5$PARAMETER))))

df5$PARAMETER <- ifelse(df5$PARAMETER == "SIO2_MG/L","SIO2_MGL", 
                        ifelse(df5$PARAMETER == "PC_MG/L", "PC_MGL", 
                               ifelse(df5$PARAMETER == "PHEOPHYTIN-A_UG/L", "PHEOPHYTIN_A_UGL", 
                                      ifelse(df5$PARAMETER == "Light_Atten_Coeff_1/M", "Light_Attten_1_m", df5$PARAMETER))))


df5$START_DATE <- as.Date(df5$START_DATE)

unique(df5$PARAMETER)

#___________________________________Make data frame wide instead of long __________________________________________________________

df6 <- df5 %>%
  pivot_wider(id_cols = c(STATION_ID, START_DATE, PARAMETER), names_from = PARAMETER, values_from = RESULT, 
              values_fn = list(RESULT = mean, na.rm = T))

#Reorganize columns in df6
colnames(df6)

df6 <- df6 %>%
  select(STATION_ID, START_DATE, TP_UGL, PO4_UGL, PN_MGL, TN_MGL, TDN_MGL, NH4_UGL, NO3_MGL, NO3_NO2_MGL, DIN_MGL, 
         DON_MGL, DOC_MGL, PC_MGL, SIO2_MGL, TSS_MGL:DO_sat, DO_MGL, SPC_UMHO_CM, SALINITY_PSS, TEMP_WATER_DEGC, CHLA_corr_pheo_UGL,
         PHEOPHYTIN_A_UGL, Light_Attten_1_m)


df6$TP_MGL <- conv_unit(df6$TP_UGL, "ug", "mg")
df6$PO4_MGL <- conv_unit(df6$PO4_UGL, "ug", "mg")
df6$NH4_MGL <- conv_unit(df6$NH4_UGL, "ug", "mg")


df6 <- df6 %>%
  select(STATION_ID, START_DATE, TP_MGL, PO4_MGL, PN_MGL, TN_MGL, TDN_MGL, NH4_MGL, NO3_MGL, NO3_NO2_MGL, DIN_MGL, 
         DON_MGL, DOC_MGL, PC_MGL, SIO2_MGL, TSS_MGL:DO_sat, DO_MGL, SPC_UMHO_CM, SALINITY_PSS, TEMP_WATER_DEGC, CHLA_corr_pheo_UGL,
         PHEOPHYTIN_A_UGL, Light_Attten_1_m)

#Summarize physicochemical parameter columns for Appendix Tables
df6_DO <- df6 %>%
  group_by(STATION_ID) %>%
  summarize(mean_DO_sat = mean(DO_sat, na.rm=T), sd_DO_sat = sd(DO_sat, na.rm=T), count = n())

df6_DOmgl <- df6 %>%
  group_by(STATION_ID) %>%
  summarize(mean_DO_mgl = mean(DO_MGL, na.rm=T), sd_DO_mgl = sd(DO_MGL, na.rm=T), count = n())

df6_temp <- df6 %>%
  group_by(STATION_ID) %>%
  summarize(mean_temp = mean(TEMP_WATER_DEGC, na.rm=T), sd_temp = sd(TEMP_WATER_DEGC, na.rm=T), count = n())


#Calculate DIN and DON in instances where it is missing
df6$DIN_MGL_calc <- df6$NH4_MGL + df6$NO3_NO2_MGL
df6$DON_MGL_calc <- df6$TDN_MGL - df6$NO3_NO2_MGL - df6$NH4_MGL

#METHOD DETECTION LIMITS FOR DON (b/c not included in EMD)

df6$DON_MDL <- (df6$TDN_MGL + df6$NO3_NO2_MGL + df6$NH4_MGL) * 0.05

#Is DON below MDL?
df6$DON_B_MDL <- ifelse(df6$DON_MGL_calc < df6$DON_MDL, "BDL", "G") #7 instances where calculate DON is less than 5% lab error MDL

#Set those 7 values to 1/2 of the method detection limit

df6$DON_MGL_calc_final <- ifelse(df6$DON_MGL_calc < df6$DON_MDL, df6$DON_MDL/2, df6$DON_MGL_calc)


df6 <- df6 %>%
  select(STATION_ID:NO3_NO2_MGL, DIN_MGL = DIN_MGL_calc, DON_MGL = DON_MGL_calc_final, DOC_MGL:Light_Attten_1_m)
#this is now corrected for MDLs across all solutes

### END Convert Dataframe from Long to Wide ###

skewness(df6$TSS_MGL, na.rm=T)
kurtosis(df6$TSS_MGL, na.rm=T)

#Remove the three high TSS concentrations at Adams Point Low Tide due to anecodotal knowledge from Tom Gregory that winter values are worse for TSS b/c dock is out of the water
df6 <- df6 %>%
 mutate(TSS_MGL = ifelse(START_DATE == "2010-03-29" & STATION_ID == "GRBAPL", NA, TSS_MGL)) %>%
  mutate(TSS_MGL = ifelse(START_DATE == "2008-11-25" & STATION_ID == "GRBAPL", NA, TSS_MGL)) %>%
  mutate(TSS_MGL = ifelse(START_DATE == "2012-01-30" & STATION_ID == "GRBAPL", NA, TSS_MGL))
  
skewness(df6$TSS_MGL)
kurtosis(df6$TSS_MGL)

#See how many months in given year are sampled at eah site
df6$Month <- month(df6$START_DATE)
df6$Year <- year(df6$START_DATE)

df6_months <- df6 %>%
  group_by(STATION_ID, Month) %>%
  tally()

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

avg_conc <- df6 %>%
  group_by(STATION_ID) %>%
  summarize(across(TP_MGL:TEMP_WATER_DEGC, mean, na.rm=T))

write.csv(avg_conc, "results/main_dataformat/avg_solute_conc_site.csv")

std_conc <- df6 %>%
  group_by(STATION_ID) %>%
  summarize(across(TP_MGL:TEMP_WATER_DEGC, sd, na.rm=T))

write.csv(std_conc, "results/main_dataformat/std_solute_conc_site.csv")
 
#This data frame has final solute concentrations that can be used for further load analysis
write.csv(df6, "results/main_dataformat/df6.csv")
