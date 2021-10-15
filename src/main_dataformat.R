# main_dataformat.R

# Author: Anna Lowien, University of New Hampshire
# Last Updated: 8/6/2021

# Purpose: Read and process raw solute concentration data for the three tidal tributaries (head-of-tide monitoring stations) that flow into Great Bay.
  # Also, processes raw solute concentrations from the estuary, at Adams Point (high and low tide).

#Data was sourced from the NH Department of Environmental Services, Environmental Monitoring Database (EMD). Data was pulled based on assigned water body IDs.

#Data Dictionary!

  #EMD data files include metadata on each water body of interest, sampling date and time, measured concentrations of nutrients, carbon, total suspended solids, 
    #tidal stage, and physio-chemical parameters. Physio-chemical parameters include snapshot measures of a dissolved oxygen, pH, temperature, and specific conductivity
  
  #Sites
    #05-LMP: LAmprey River (LMP)
    #09-EXT: Squamscott River (SQR)
    #09-EXT-DAMMED: Squamscott River pre dam removal in 2016
    #02-WNC: Winnicut River WNC)

#Load required packages.
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyquant)
library(cowplot)
library(RColorBrewer)
library(tidyr)
library(stringr)
library(plotly)
library(measurements)
library(viridis)
library(moments)

# Initial Data Exploration ------------------------------------------------
#Read in the thinned out excel files from NHDES EMD
LMP <- read_excel("data/original_files/031020_Lamprey_Tidal_Trib.xlsx", guess_max = 40000) #Lamprey River (Site ID: 05-LMP)
WNC <- read_excel("data/original_files/043020_Winnicut_Tidal_Trib.xlsx", guess_max = 10000) #Winnicut River (Site ID: 02-WNC)
SQR <- read_excel("data/original_files/101220_Squamscott_Dammed.xlsx", guess_max = 3500) #Squamscott River (Site ID: 09-EXT)
AP <- read_excel("data/original_files/043020_AdamsPoint.xlsx", guess_max = 55800) #Adams Point (GRBAP) (high + low tide)

#Fix class of date column
LMP$START_DATE <- as.Date(LMP$START_DATE)
AP$START_DATE <- as.Date(AP$START_DATE)
SQR$START_DATE <- as.Date(SQR$START_DATE)
WNC$START_DATE <- as.Date(WNC$START_DATE)


sapply(AP,class) #Method Detection Limit Column is empty for this spreadsheet; returns logical as class by default
sapply(LMP, class)
sapply(WNC, class)
sapply(SQR,class)

#TIDALWQ is the project name for PREP sample collection at monthly time-step
LMP <- LMP %>%
  filter(STATION_ID == "05-LMP") %>%
  filter(SAMPLE_COLLECTION_METHOD_ID == "TIDALWQ") %>%
  filter(START_DATE > "2008-01-01") #filter for time period of interest

AP <- AP %>%
  filter(STATION_ID == "GRBAP") %>%
  filter(SAMPLE_COLLECTION_METHOD_ID == "TIDALWQ") %>%
  filter(START_DATE > "2008-01-01")

AP_Tide <- AP %>%
  select(STATION_ID, START_DATE, START_TIME, PARAMETER, QUALIFIER_AND_RESULTS) %>%
  filter(PARAMETER == "TIDE STAGE")

#Fix "Tide Stage" PARAMETER To be either High or Low Tide
AP_Tide$QUALIFIER_AND_RESULTS <- ifelse(AP_Tide$PARAMETER == "TIDE STAGE" & AP_Tide$QUALIFIER_AND_RESULTS == "EBB", "LOW", AP_Tide$QUALIFIER_AND_RESULTS)
AP_Tide$QUALIFIER_AND_RESULTS <- ifelse(AP_Tide$PARAMETER == "TIDE STAGE" & AP_Tide$QUALIFIER_AND_RESULTS == "FLOOD", "HIGH", AP_Tide$QUALIFIER_AND_RESULTS)

AP_Tide <- AP_Tide %>%
  select(-PARAMETER)

#Rename STATION ID based on high/low tide
AP_Tide$STATION_ID <- ifelse(AP_Tide$QUALIFIER_AND_RESULTS == "LOW", "GRBAPL", "GRBAPH")

#Remove duplicates
deduped.APTIDE <- unique( AP_Tide[ , 1:4 ] )

deduped.APTIDE <- deduped.APTIDE %>%
  select(-QUALIFIER_AND_RESULTS)

#Join Adams Point df with the Adams Point Tide Stage df
#Replace Station ID in AP data frame
AP_new <- left_join(AP, deduped.APTIDE, by = c("START_DATE", "START_TIME"))

AP_new$STATION_ID <- AP_new$STATION_ID.y

AP_new <- AP_new %>%
  select(-STATION_ID.x, -STATION_ID.y, STATION_ID, LATITUDE_DECIMAL_DEGREE:SAMPLE_SIZE)

#Couple of missing High and Low Tide IDS
AP_new$STATION_ID <- ifelse(AP_new$START_DATE == "2008-06-11" & AP_new$START_TIME == "09:35", "GRBAPH",
                            ifelse(AP_new$START_DATE == "2009-06-29" & AP_new$START_TIME == "08:31", "GRBAPL", 
                                   ifelse(AP_new$START_DATE == "2009-07-13" & AP_new$START_TIME == "14:17", "GRBAPH",
                                          ifelse(AP_new$START_DATE == "2011-08-22" & AP_new$START_TIME == "07:10", "GRBAPH", AP_new$STATION_ID))))


SQR <- SQR %>%
  filter(SAMPLE_COLLECTION_METHOD_ID == "TIDALWQ") %>%
  filter(START_DATE > "2008-01-01")

WNC <- WNC %>%
  filter(STATION_ID == "02-WNC") %>%
  filter(SAMPLE_COLLECTION_METHOD_ID == "TIDALWQ") %>%
  filter(START_DATE > "2008-01-01")

#Merge sites into one data frame
df <- full_join(LMP, AP_new)
df <- full_join(df, SQR)
df <- full_join(df, WNC)

colnames(df)

### END Initial Data Exploration ###
#### Resolve Variable and Column Header Names ####

#Fix columns that aren't numeric, but should be
df$RDL <- as.numeric(df$RDL)
df$METHOD_DETECTION_LIMIT <- as.numeric(df$METHOD_DETECTION_LIMIT)

#Dam removal on Squamscott occurred in 2016
#Fix station ID names, 09-EXT and 09-EXT-DAMMED are the same site, but differ over time
df$STATION_ID <- ifelse(df$STATION_ID == "09-EXT-DAMMED", "09-EXT", df$STATION_ID)

unique(df$STATION_ID)

#Removing blank/unneccesary columns from data frame
df <- df %>%
  select(-LATITUDE_DECIMAL_DEGREE, -LONGITUDE_DECIMAL_DEGREE, - REPLICATE_NUMBER_REFERENCE,
         -DATA_STATUS, -UPPER_DEPTH, -LOWER_DEPTH, -DEPTH_RANGE_UNITS, - LAB_QUALIFIER,
         -STATISTIC_TYPE, -SAMPLE_SIZE, -RIVER_NAME, -MEDIUM, -DEPTH_ZONE, -RAIN_PRIOR_3_DAYS)

colnames(df)

unique(df$RESULT_VALID)

df %>% count(RESULT_VALID)

#Delete the 119 occurrences where results are not valid 
df2 <- df %>%
  filter(RESULT_VALID == "Y" | is.na(RESULT_VALID))

#Remove result valid column and filter out unncessary parameters
df2 <- df2 %>%
  select(-RESULT_VALID) %>%
  filter(PARAMETER != "CLOSTRIDIUM PERFRINGENS") %>%
  filter(PARAMETER != "ENTEROCOCCUS") %>%
  filter(PARAMETER != "ESCHERICHIA COLI") %>%
  filter(PARAMETER != "TOTAL FECAL COLIFORM") %>%
  filter(PARAMETER != "WIND DIRECTION") %>%
  filter(PARAMETER != "WIND VELOCITY") %>%
  filter(PARAMETER != "SECCHI DISK TRANSPARENCY") %>%
  filter(PARAMETER != "COLORED DISSOLVED ORGANIC MATTER (CDOM)") %>%
  filter(PARAMETER != "TURBIDITY") %>%
  filter(PARAMETER != "DEPTH")

df3 <- df2 

unique(df3$PARAMETER)

df3 %>% count(PARAMETER)

#Fix the issue where NH DES starting putting NA instead of 1/2 of MDL
sum(is.na(df3$QUALIFIER_AND_RESULTS)) #22 NAs that should be 1/2 of the MDL

df3$QUALIFIER_AND_RESULTS <- ifelse(is.na(df3$QUALIFIER_AND_RESULTS), df3$RDL/2, df3$QUALIFIER_AND_RESULTS)

#Figure out Parameter Methods and Rename to clarify

#PHOSPHORUS AS P, fraction type is "Total"; which means PHOSPHORUS samples are TOTAL PHOSPHORUS ("TP")

df3$PARAMETER <- ifelse(df3$PARAMETER == "PHOSPHORUS AS P", "TP", df3$PARAMETER)

#PHOSPHORUS, ORTHOPHOSPHATE AS P is PO4 molecule
df3$PARAMETER <- ifelse(df3$PARAMETER == "PHOSPHORUS, ORTHOPHOSPHATE AS P", "PO4", df3$PARAMETER)

#NITROGEN Method is for TOTAL NITROGEN
df3$PARAMETER <- ifelse(df3$PARAMETER == "NITROGEN", "TN", df3$PARAMETER)

#NITROGEN, DISSOLVED is "TOTAL DISSOLVED NITROGEN" (TDN)
df3$PARAMETER <- ifelse(df3$PARAMETER == "NITROGEN, DISSOLVED", "TDN", df3$PARAMETER)

#NITROGEN, AMMONIA as N - is technically ammonium (the method measures ammonia, but there isn't ammonia in streams really)
#Per conversation with Jody over Slack July 2020
df3$PARAMETER <- ifelse(df3$PARAMETER == "NITROGEN, AMMONIA AS N", "NH4", df3$PARAMETER)

#NITROGEN, INORGANIC (AMMONIA, NITRATE AND NITRITE) is Dissolved Inorganic Nitrogen (DIN)
df3$PARAMETER <- ifelse(df3$PARAMETER == "NITROGEN, INORGANIC (AMMONIA, NITRATE AND NITRITE)", "DIN", df3$PARAMETER)

#"NITROGEN, NITRITE (NO2) + NITRATE (NO3) AS N"  is NO3+NO2
df3$PARAMETER <- ifelse(df3$PARAMETER == "NITROGEN, NITRITE (NO2) + NITRATE (NO3) AS N", "NO3_NO2", df3$PARAMETER)

#"NITROGEN, NITRITE (NO2) AS N" is really nitrate
df3$PARAMETER <- ifelse(df3$PARAMETER == "NITROGEN, NITRITE (NO2) AS N", "NO3", df3$PARAMETER)

#"NITROGEN, ORGANIC" is DON, calculated as TDN-DIN
df3$PARAMETER <- ifelse(df3$PARAMETER == "NITROGEN, ORGANIC", "DON", df3$PARAMETER)

#NITROGEN, SUSPENDED is Particulate N
df3$PARAMETER <- ifelse(df3$PARAMETER == "NITROGEN, SUSPENDED", "PN", df3$PARAMETER)

#Carbon, SUSPENDED is Particulate Carbon
df3$PARAMETER <- ifelse(df3$PARAMETER == "CARBON, SUSPENDED", "PC", df3$PARAMETER)

#CARBON, ORGANIC is Dissolved Organic Carbon
df3$PARAMETER <- ifelse(df3$PARAMETER == "CARBON, ORGANIC", "DOC", df3$PARAMETER)

#Misc. condensing of Parameter Names to eliminate spaces
df3$PARAMETER <- ifelse(df3$PARAMETER == "LIGHT ATTENUATION COEFFICIENT", "Light_Atten_Coeff", 
                               ifelse(df3$PARAMETER == "DISSOLVED OXYGEN SATURATION", "DO_sat", 
                                      ifelse(df3$PARAMETER == "TEMPERATURE WATER", "Temp_Water", df3$PARAMETER)))

df3$PARAMETER <- ifelse(df3$PARAMETER == "SPECIFIC CONDUCTANCE", "SPC", 
                        ifelse(df3$PARAMETER == "DISSOLVED OXYGEN", "DO", 
                               ifelse(df3$PARAMETER == "SOLIDS, SUSPENDED", "TSS", 
                                      ifelse(df3$PARAMETER == "TIDE STAGE", "Tide_Stage", df3$PARAMETER))))

df3$PARAMETER <- ifelse(df3$PARAMETER == "CHLOROPHYLL A, CORRECTED FOR PHEOPHYTIN", "CHLA_corrected_pheophytin",
                        ifelse(df3$PARAMETER == "SILICA AS SIO2", "SIO2", df3$PARAMETER))
                      
unique(df3$PARAMETER)

#Look at fraction types measured
unique(df3$FRACTION_TYPE)

df3 %>% count(FRACTION_TYPE)

#6 samples measured for Volatile Solids at 09-EXT-DAMMED; not needed
df3 <- df3 %>%
  filter(FRACTION_TYPE == "DISSOLVED" | FRACTION_TYPE == "SUSPENDED" | FRACTION_TYPE == "TOTAL" | is.na(FRACTION_TYPE))


#Not going to worry too much about FRACTION, and instead go off of method and parameter to check solutes

#Combine columns so that it becomes easier to convert dataframe from "long" to "wide"
df3 %>% count(DEPTH_UNITS) # only meters (M) or NA, so can combine depth and depth units into one column

names(df3)[names(df3)=="DEPTH"] <- "Depth_m"
#Remove DEPTH UNITS Column

df3 <- df3 %>%
  select(-DEPTH_UNITS)

#Combine Analytical Method and Source Method ID columns
df3 <- df3 %>%
  unite("Analytical_Method", ANALYTICAL_METHOD_SOURCE_ID:ANALYTICAL_METHOD, sep = "_")

#Combine detection limit and comments columns
df3 %>% count(DETECTION_LIMIT_COMMENTS)
df3 <- df3 %>%
  unite("Result_DL", RDL:DETECTION_LIMIT_COMMENTS, na.rm=TRUE, sep = "_")

df3 %>% count(Result_DL)

### END RESOLVE VARIABLE AND COLUMN HEADER NAMES ###
#### Tidy Results (saved intermediate step)####

#Remove additional columns, now that we've filtered down to desired project and sites
df4 <- df3 %>%
  select(-WATERBODY_ID, -SAMPLE_COLLECTION_METHOD_ID)

#Activity Comments Review and Assessment
unique(df4$ACTIVITY_COMMENTS) #Activity Comments are mostly UNH IDs and weather at GRBAP - removal of column for purposes of solute analysis is OK

unique(df4$ACTIVITY_TYPE) # Removing duplicates to see if that fixes spread issue)

df4 <- df4 %>%
  select(-ACTIVITY_COMMENTS, - FRACTION_TYPE) %>%
  filter(ACTIVITY_TYPE == "SAMPLE - ROUTINE")

#How many instances of each parameter being measured?
df4_count <- df4%>% count(df4$PARAMETER)

#Get ride of "<" from the QUALIFIER AND RESULTS Column
df4$RESULTS_clean <- stringr::str_replace(df4$QUALIFIER_AND_RESULTS, '\\<', '')

#ID those 226 rows that had an "<" so that we know the value is less than the detection limit
df4 <- df4 %>% 
  mutate(DETECTION_LIMIT = if_else(str_starts(QUALIFIER_AND_RESULTS, "<"), "BELOW", "NA")) #Below == "<"

df4 %>% count(df4$DETECTION_LIMIT) #226 occurrences where measurement is recorded as being less than/equal to instrument detection limit

df4$RESULTS_clean <- as.numeric(df4$RESULTS_clean) # this converts everything to numbers

#If result is below detection limit, set to 1/2 of detection limit
#226 results that are below detection limit

df4$RESULT <- ifelse(df4$DETECTION_LIMIT == "BELOW", df4$RESULTS_clean / 2, df4$RESULTS_clean)

#Compare results to results_clean
df4$Results_Comp <- df4$RESULTS_clean - df4$RESULT

#saving df4 as an intermediate step; 
#at this point dataframe character issues are resolved and results below MDL are set to 1/2 the MDL
write.csv(df4, "results/main_dataformat/df4.csv")
### END TIDY RESULTS ###
#_____________________________________________________________________________________________________________

#### Convert Dataframe from Long to Wide ####
df4 <- read.csv("results/main_dataformat/df4.csv")

df4 <- df4 %>%
  select(-X)

#Thin out to columns necessary for converting from long to wide
df5 <- df4 %>%
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
         PHEOPHYTIN_A_UGL, Light_Attten_1_m, Tide_Stage)


df6$TP_MGL <- conv_unit(df6$TP_UGL, "ug", "mg")
df6$PO4_MGL <- conv_unit(df6$PO4_UGL, "ug", "mg")
df6$NH4_MGL <- conv_unit(df6$NH4_UGL, "ug", "mg")


df6 <- df6 %>%
  select(STATION_ID, START_DATE, TP_MGL, PO4_MGL, PN_MGL, TN_MGL, TDN_MGL, NH4_MGL, NO3_MGL, NO3_NO2_MGL, DIN_MGL, 
         DON_MGL, DOC_MGL, PC_MGL, SIO2_MGL, TSS_MGL:DO_sat, DO_MGL, SPC_UMHO_CM, SALINITY_PSS, TEMP_WATER_DEGC, CHLA_corr_pheo_UGL,
         PHEOPHYTIN_A_UGL, Light_Attten_1_m, Tide_Stage)

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
  select(STATION_ID:NO3_NO2_MGL, DIN_MGL = DIN_MGL_calc, DON_MGL = DON_MGL_calc_final, DOC_MGL:Tide_Stage)
#this is now corrected for MDLs across all solutes

### END Convert Dataframe from Long to Wide ###

skewness(df6$TSS_MGL, na.rm=T)
kurtosis(df6$TSS_MGL, na.rm=T)

#Remove the three high TSS concentrations at Adams Point Low Tide due to anecodotal knowledge from Tom Gregory that winter values are worse for TSS b/c dock is out of the water
df6 <- df6 %>%
 mutate(TSS_MGL = ifelse(START_DATE == "2010-03-29" & STATION_ID == "GRBAPL", NA, TSS_MGL)) %>%
  mutate(TSS_MGL = ifelse(START_DATE == "2008-11-25" & STATION_ID == "GRBAPL", NA, TSS_MGL)) %>%
  mutate(TSS_MGL = ifelse(START_DATE == "2012-01-30" & STATION_ID == "GRBAPL", NA, TSS_MGL))
  
skewness(df6$TSS_MGL, na.rm=T)
kurtosis(df6$TSS_MGL, na.rm=T)

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
