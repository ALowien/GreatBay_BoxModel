# main_dataformat.R
# Biogeochemical Stressors and Ecological Response in Great Bay Estuary

# Author: Anna Mikulis, University of New Hampshire
# Last Updated: 7/31/2024

#R Version 4.3.2 (2023-10-31 ucrt) -- "Eye Holes"

# Purpose: Read and process raw solute concentration data for the three tidal tributaries (head-of-tide monitoring stations) that flow into Great Bay.
  # Also process raw solute concentrations from the estuary, at Adams Point (high and low tide). Prepare discharge data for flux calculations.
  # Create physiochemical characteristics tables. 

# Data was sourced from the NH Department of Environmental Services, Environmental Monitoring Database (EMD) by email request. 
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
              "tidyr","stringr",  "plotly", "measurements", "viridis", "moments", "flextable")

lapply(Packages, library, character.only = TRUE)

# Import DES Site Data ------------------------------------------------
#Files have been amended to put columns names as the first row.
subdir <- "data/original_files/Updated_EMD"
#subdir <- "data/original_files/Original_EMD"
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

#___________________________________________
#Select for columns of interest; filter for sites of interest and time period of interest
df <- df %>%
  select(STATION_ID:SAMPLE_SIZE)%>%
  filter(STATION_ID == "05-LMP" | STATION_ID == "02-WNC" | STATION_ID == "09-EXT" | STATION_ID == "09-EXT-DAMMED" |
           STATION_ID == "GRBAP") %>%
  filter(SAMPLE_COLLECTION_METHOD_ID == "TIDALWQ") %>% #Project Name for Tidal Tribs and Adams Point Sampling
  filter(START_DATE > "2008-01-01") #Tidal Tribs sampling program began in 2008

unique(df$STATION_ID) #check for correct site names

#Distinguish GRBAP sampling by low and high tide events; ap_tide stands for Adams Point Tide data
ap_tide <- df %>%
  select(STATION_ID, START_DATE, START_TIME, PARAMETER_ANALYTE, QUALIFIER_AND_RESULTS, RESULT_COMMENTS) %>%
  filter(STATION_ID == "GRBAP") %>%
  filter(PARAMETER_ANALYTE == "TIDE STAGE") # %>%
  #filter(!is.na(QUALIFIER_AND_RESULTS)) originally did this way, but 2019 forward data has the tide stage in the result comment column


#Pull high/low tide into the RESULTS from the comments. 
ap_tide$QUALIFIER_AND_RESULTS <- ifelse(is.na(ap_tide$QUALIFIER_AND_RESULTS),
                                        str_extract(ap_tide$RESULT_COMMENTS, "(?i)LOW|HIGH"),
                                        ap_tide$QUALIFIER_AND_RESULTS)

# If you want to extract only the first occurrence of LOW or HIGH from RESULT_COMMENTS
#ap_tide$QUALIFIER_AND_RESULTS <- ifelse(is.na(ap_tide$QUALIFIER_AND_RESULTS),
 #                                       str_extract(ap_tide$RESULT_COMMENTS, "(?i)LOW|HIGH")[1],
  #                                      ap_tide$QUALIFIER_AND_RESULTS)



#Categorize "Tide Stage" PARAMETER To be either High or Low Tide (e.x. ebb tides are re-classified as low tide samples)
ap_tide$QUALIFIER_AND_RESULTS <- ifelse(ap_tide$PARAMETER_ANALYTE == "TIDE STAGE" & ap_tide$QUALIFIER_AND_RESULTS == "EBB", "LOW", ap_tide$QUALIFIER_AND_RESULTS)
ap_tide$QUALIFIER_AND_RESULTS <- ifelse(ap_tide$PARAMETER_ANALYTE == "TIDE STAGE" & ap_tide$QUALIFIER_AND_RESULTS == "FLOOD", "HIGH", ap_tide$QUALIFIER_AND_RESULTS)

ap_tide <- ap_tide %>%
  select(-PARAMETER_ANALYTE, -RESULT_COMMENTS)

#Rename Great Bay Adams Point (GRBAP) STATION ID based on high/low tide (GRBAPL indicates low tide sample; GRBAPH indicates high tide sample)
ap_tide$STATION_ID <- ifelse(ap_tide$QUALIFIER_AND_RESULTS == "LOW", "GRBAPL", "GRBAPH")

#Remove duplicates of tide codes
ap_tide <- ap_tide[!duplicated(ap_tide[,1:4]),]

ap_tide <- ap_tide %>%
  select(-QUALIFIER_AND_RESULTS)

#Join df with deduped.APTIDE
#Replace Station ID in AP data frame
df <- left_join(df, ap_tide, by = c("START_DATE", "START_TIME"))
#If "Station_ID.y" is not NA, (i.e says "GRBAPL" or "GRBAPH"), use STATION_ID.y as the STATION ID, else use the original station id from "Station_ID.x 
df$STATION_ID <- ifelse(!is.na(df$STATION_ID.y), df$STATION_ID.y, df$STATION_ID.x) 

df <- df %>%
  select(-STATION_ID.x, -STATION_ID.y) #remove extra Station ID columns now that everything has been summarized into the STATION_ID column

#Couple of missing High and Low Tide IDS
df$STATION_ID <- ifelse(df$START_DATE == "2008-06-11" & df$START_TIME == "09:35", "GRBAPH",
                            ifelse(df$START_DATE == "2009-06-29" & df$START_TIME == "08:31", "GRBAPL", 
                                   ifelse(df$START_DATE == "2009-07-13" & df$START_TIME == "14:17", "GRBAPH",
                                          ifelse(df$START_DATE == "2011-08-22" & df$START_TIME == "07:10", "GRBAPH", df$STATION_ID))))

### END Import DES Data ###
#### Resolve Variable and Column Header Names ####

#Fix columns that aren't numeric, but should be
df$RDL <- as.numeric(df$RDL) #Result Detection Limit

#Dam removal on Squamscott River occurred in 2016; resulting in two different site ids depending on whether sampling occurred before or after the dam removal
#Fix station ID names, 09-EXT and 09-EXT-DAMMED are the same site, but differ over time
df$STATION_ID <- ifelse(df$STATION_ID == "09-EXT-DAMMED", "09-EXT", df$STATION_ID)

#Subset for columns of interest
df <- df %>%
  select(STATION_ID, WATERBODY_ID, ACTIVITY_ID, ACTIVITY_TYPE, START_DATE:FRACTION_TYPE)

#Filter out unnecessary parameters for the box model
remove_parms <- c("CLOSTRIDIUM PERFRINGENS", "ENTEROCOCCUS", "ESCHERICHIA COLI","TOTAL FECAL COLIFORM", "WIND DIRECTION", "WIND VELOCITY", "SECCHI DISK TRANSPARENCY","COLORED DISSOLVED ORGANIC MATTER (CDOM)", "TURBIDITY","DEPTH", "TIDE STAGE", "LIGHT ATTENUATION COEFFICIENT", "SILICA AS SIO2", "PHEOPHYTIN-A", "CARBON, SUSPENDED")

df <- subset(df, !(PARAMETER_ANALYTE %in% remove_parms))

df <- df %>%
  select(STATION_ID:ACTIVITY_COMMENTS, PARAMETER = PARAMETER_ANALYTE, QUALIFIER_AND_RESULTS:FRACTION_TYPE)
#_____________________________________________________
#Clarify Parameter Methods by Renaming

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
#df$PARAMETER <- ifelse(df$PARAMETER == "CARBON, SUSPENDED", "PC", df$PARAMETER)

#CARBON, ORGANIC is Dissolved Organic Carbon
df$PARAMETER <- ifelse(df$PARAMETER == "CARBON, ORGANIC", "DOC", df$PARAMETER)

#Misc. condensing of Parameter Names to eliminate spaces
df$PARAMETER <- ifelse(df$PARAMETER == "DISSOLVED OXYGEN SATURATION", "DO_sat", 
                       ifelse(df$PARAMETER == "TEMPERATURE WATER", "Temp_Water", df$PARAMETER))

df$PARAMETER <- ifelse(df$PARAMETER == "SPECIFIC CONDUCTANCE", "SPC", 
                       ifelse(df$PARAMETER == "DISSOLVED OXYGEN", "DO", 
                              ifelse(df$PARAMETER == "SOLIDS, SUSPENDED", "TSS", 
                                     ifelse(df$PARAMETER == "TIDE STAGE", "Tide_Stage", df$PARAMETER))))

df$PARAMETER <- ifelse(df$PARAMETER == "CHLOROPHYLL A, CORRECTED FOR PHEOPHYTIN", "CHLA_corrected_pheophytin", df$PARAMETER)

unique(df$PARAMETER)

#How many values are valid vs invalid?
df %>% count(RESULT_VALID)
#Assess the 104 occurrences where results are flagged as not valid 
df_invalid <- df %>%
  filter(RESULT_VALID == "N") 

ggplot(df_invalid, aes(x=PARAMETER)) +
  geom_histogram(stat="count", aes(fill=STATION_ID)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=7.5, angle=90),
        legend.position = "bottom",
        plot.margin = margin(b = -0.1, unit = "lines",
                             t = 0.5)) +
  ylab("# of Samples & Field Replicates Marked as Not Valid")

ggplot(df, aes(START_DATE, as.numeric(QUALIFIER_AND_RESULTS))) + geom_point(aes(color=STATION_ID)) +
  facet_wrap(~PARAMETER, scales="free_y")

df_na <- df %>%
  filter(is.na(QUALIFIER_AND_RESULTS)) #NO NA values at the moment

#For instances where result is NA instead of 1/2 of method detection limit, run these next 2 lines of code 
#na <- subset(df, is.na(QUALIFIER_AND_RESULTS))
#df$QUALIFIER_AND_RESULTS <- ifelse(is.na(df$QUALIFIER_AND_RESULTS), df$RDL/2, df$QUALIFIER_AND_RESULTS)

#Combine Analytical Method and Source Method ID columns
df <- df %>%
  unite("Analytical_Method", ANALYTICAL_METHOD_SOURCE_ID:ANALYTICAL_METHOD, sep = "_")

#Combine detection limit and comments columns
df %>% count(DETECTION_LIMIT_COMMENTS)


### END RESOLVE VARIABLE AND COLUMN HEADER NAMES ###
#### Tidy Results (saved intermediate step)####

#Remove additional columns, now that we've filtered down to desired project and sites
df <- df %>%
  select(-WATERBODY_ID, -SAMPLE_COLLECTION_METHOD_ID)

#Activity Comments Review and Assessment
unique(df$ACTIVITY_COMMENTS) 

unique(df$ACTIVITY_TYPE) # Removing field duplicates, as they are for QC purposes only (after QC Done)

#How many instances of each parameter being measured?
df_count <- df%>% count(df$PARAMETER)

#Get ride of "<" from the QUALIFIER AND RESULTS Column
df$RESULTS_clean <- stringr::str_replace(df$QUALIFIER_AND_RESULTS, '\\<', '')

#ID those 324 rows that had an "<" so that we know the value is less than the detection limit
df <- df %>% 
  mutate(DETECTION_LIMIT = if_else(str_starts(QUALIFIER_AND_RESULTS, "<"), "BELOW", "NA")) #Below == "<"

df %>% count(df$DETECTION_LIMIT) #324 occurrences where measurement is recorded as being less than/equal to instrument detection limit

df$RESULTS_clean <- as.numeric(df$RESULTS_clean) # this converts everything to numbers

#If result is below detection limit, set to 1/2 of detection limit
#324 results that are below detection limit

df$RESULT <- ifelse(df$DETECTION_LIMIT == "BELOW", df$RESULTS_clean / 2, df$RESULTS_clean)
df$DETECTION_LIMIT<- ifelse(!is.na(df$DETECTION_LIMIT) & df$RESULTS_clean < df$RDL, "BELOW DETECTION LIMIT", df$DETECTION_LIMIT)

#df <- df %>%
 # unite("Result_DL", RDL:DETECTION_LIMIT_COMMENTS, na.rm=TRUE, sep = "_")
#___________________
#QC based on field duplicates and method detection limits
#added today 5/14/2024
test <- df 
#Group by STATION_ID, START_DATE, and PARAMETER
df_grouped <- test %>%
  group_by(STATION_ID, START_DATE, PARAMETER) %>%
  mutate(duplicate_count = n()) %>%
  ungroup()

# Filter rows with duplicate_count > 1
df_duplicates <- df_grouped %>%
  filter(duplicate_count > 1)

df_calculated <- df_duplicates %>%
  group_by(STATION_ID, START_DATE, PARAMETER) %>%
  #separate(Result_DL, into = c("dl", "unit"), sep = "_")  %>%
  mutate(
    num_duplicates = n(),
    x1 = ifelse(sum(ACTIVITY_TYPE == "SAMPLE - ROUTINE") == 1, RESULT[ACTIVITY_TYPE == "SAMPLE - ROUTINE"], NA_real_),
    x2 = ifelse(sum(ACTIVITY_TYPE == "QUALITY CONTROL SAMPLE-FIELD DUPLICATE") >= 1, RESULT[ACTIVITY_TYPE == "QUALITY CONTROL SAMPLE-FIELD DUPLICATE"][1], NA_real_),
    x3 = ifelse(sum(ACTIVITY_TYPE == "QUALITY CONTROL SAMPLE-FIELD DUPLICATE") == 2, RESULT[ACTIVITY_TYPE == "QUALITY CONTROL SAMPLE-FIELD DUPLICATE"][2], NA_real_),
    mean = round(as.numeric(mean(RESULT)),3),
    RPD = ifelse(num_duplicates == 2, abs(x1 - x2) / ((x1 + x2) / 2) * 100, NA),
    SD = ifelse(num_duplicates == 3, sd(RESULT), NA),
    RSD = ifelse(num_duplicates == 3, sd(RESULT) / mean(RESULT) * 100, NA),
    mdl_10 = as.numeric(RDL) * 10
  ) %>%
  select(STATION_ID, START_DATE, ACTIVITY_TYPE, PARAMETER, RESULT, mean, x1, x2, x3, RPD, RSD, SD, num_duplicates, RDL, DETECTION_LIMIT_COMMENTS, mdl_10)

# Output the result
print(df_calculated)

df_calculated_failures <- df_calculated

#if mean of the sample and field duplicate is less than 10x the MDL it is valid even if the RPD is > 20%
df_calculated_failures$mdl_check <- ifelse(df_calculated_failures$mean < df_calculated_failures$mdl_10, "VALID", "CHECK")

df_calculated_failures$RPD_check <- ifelse(df_calculated_failures$RPD > 20 & df_calculated_failures$START_DATE > "2018-01-01", "FAIL", 
                                           ifelse(df_calculated_failures$START_DATE < "2018-01-01" & df_calculated_failures$RPD > 30, "FAIL", NA))

df_calculated_failures$RSD_check <- ifelse(df_calculated_failures$RSD > 20 & df_calculated_failures$START_DATE > "2018-01-01", "FAIL", 
                                           ifelse(df_calculated_failures$START_DATE < "2018-01-01" & df_calculated_failures$RSD > 30, "FAIL", NA))

df_calculated_failures2 <- df_calculated_failures %>%
  filter(RPD_check == "FAIL" | RSD_check == "FAIL") %>%
  filter(mdl_check != "VALID")

unique_rows <- df_calculated_failures2 %>% 
  distinct(STATION_ID, START_DATE, PARAMETER, RPD, RSD,  .keep_all = TRUE)

#Remove the values that have been flagged as bad due to field duplicate failure
df <- anti_join(df, unique_rows) #12003 - 74 should be 11929 rows remaining

df_invalid_v2 <- df %>%
  filter(RESULT_VALID == "N") 

ggplot(df_invalid_v2, aes(x=PARAMETER)) +
  geom_histogram(stat="count", aes(fill=STATION_ID)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=7.5, angle=90),
        legend.position = "bottom",
        plot.margin = margin(b = -0.1, unit = "lines",
                             t = 0.5)) +
  ylab("# of Samples & Field Replicates Marked as Not Valid")

#Now we fix the DOC for GRBAPH and GRBAPL

# df <- df %>% filter(RESULT_VALID == "Y" | is.na(RESULT_VALID))


#REMOVE FIELD DUPLICATES
#df <- df %>%
# select(-ACTIVITY_COMMENTS, - FRACTION_TYPE) %>%
#  filter(ACTIVITY_TYPE == "SAMPLE - ROUTINE")



###############################################################################################################
ggplot(subset(df, STATION_ID == "02-WNC"), aes(START_DATE, RESULTS_clean)) +
  geom_point(aes(color=RESULT_VALID), size=2) +
  scale_x_date(date_breaks="1 year", date_labels = "%Y") +
  facet_wrap(~PARAMETER, scales="free_y")

ggplot(subset(df, STATION_ID == "09-EXT"), aes(START_DATE, RESULTS_clean)) +
  geom_point(aes(color=RESULT_VALID), size=2) +
  scale_x_date(date_breaks="1 year", date_labels = "%Y") +
  facet_wrap(~PARAMETER, scales="free_y")

ggplot(subset(df, STATION_ID == "05-LMP"), aes(START_DATE, RESULTS_clean)) +
  geom_point(aes(color=RESULT_VALID), size=2) +
  scale_x_date(date_breaks="1 year", date_labels = "%Y") +
  facet_wrap(~PARAMETER, scales="free_y") +
  ggtitle("05-LMP")

ggplot(subset(df, STATION_ID == "GRBAPL"), aes(START_DATE, RESULTS_clean)) +
  geom_point(aes(color=RESULT_VALID), size=2) +
  scale_x_date(date_breaks="1 year", date_labels = "%Y") +
  facet_wrap(~PARAMETER, scales="free_y") +
  ggtitle("GRBAPL")


ggplot(subset(df, STATION_ID == "GRBAPH"), aes(START_DATE, RESULTS_clean)) +
  geom_point(aes(color=RESULT_VALID), size=2) +
  scale_x_date(date_breaks="1 year", date_labels = "%Y") +
  facet_wrap(~PARAMETER, scales="free_y")


#Why is GRBAP DOC all Not valid in 2011?
grbap <- df %>%
  filter(STATION_ID == "GRBAPH" | STATION_ID == "GRBAPL")

ggplot(subset(grbap, PARAMETER == "DOC"), aes(START_DATE, RESULTS_clean)) +
  geom_point(aes(color=RESULT_VALID), size=2) +
  scale_x_date(date_breaks="1 year", date_labels = "%Y") +
  scale_y_continuous(limits=c(0,10)) +
  ylab("DOC mg-C/L at GRBAP") +
  xlab("Sampling Date") +
  theme_bw()

ggplot(subset(grbap, PARAMETER == "DOC" & START_DATE < "2012-01-01" &  START_DATE > "2011-01-01"), 
       aes(START_DATE, RESULTS_clean)) +
  geom_point(aes(color=RESULT_VALID, shape=STATION_ID), size=2.5) +
  scale_x_date(date_breaks="1 month", date_labels = "%Y-%m-%d") +
  scale_y_continuous(limits=c(0,10)) +
  ylab("DOC mg-C/L at GRBAP") +
  xlab("Sampling Date") +
  theme_bw() +
  scale_shape_manual(labels=c("High Tide", "Low Tide"), values=c(19,17))



#look through remaining not valid data
invalid <- df %>%
  filter(RESULT_VALID == "N")

#keep DOC, 2011
invalid$NEW_RESULT_VALID <- ifelse(invalid$PARAMETER == "DOC" & invalid$START_DATE > 
                                     "2011-01-01" & invalid$START_DATE < "2011-12-30", 
                              "Y", "N")

#FAILED RPD, but mean of sample+duplicate <  10MDL
invalid$NEW_RESULT_VALID <- ifelse(invalid$PARAMETER == "NH4" & invalid$ACTIVITY_ID ==
                                     "TTMP03231602" | 	invalid$ACTIVITY_ID == "TTMP03231603", "Y", invalid$NEW_RESULT_VALID) 

#FAILED RPD, but mean of sample+duplicate <  10MDL
invalid$NEW_RESULT_VALID <- ifelse(invalid$PARAMETER == "NH4" & invalid$ACTIVITY_ID ==
                                     "TTMP10241802" | 	invalid$ACTIVITY_ID == "TTMP10241803", "Y", invalid$NEW_RESULT_VALID)

#FAILED RPD, but low MDL
invalid$NEW_RESULT_VALID <- ifelse(invalid$PARAMETER == "PN" & invalid$ACTIVITY_ID ==
                                     "JEL04230801" | 	invalid$ACTIVITY_ID == "	
JEL04230802", "Y", invalid$NEW_RESULT_VALID)

#FAILED RPD, but low MDL
invalid$NEW_RESULT_VALID <- ifelse(invalid$PARAMETER == "PO4" & invalid$ACTIVITY_ID ==
                                     "TTMP11231503" | 	invalid$ACTIVITY_ID == "TTMP11231502", "Y", invalid$NEW_RESULT_VALID)

#Only keep the values that are at not valid in the invalid dataframe
invalid <- invalid %>%
  filter(NEW_RESULT_VALID == "N") %>%
  select(-NEW_RESULT_VALID)

#remove the invalid data rows from df

df <- anti_join(df, invalid)

#Look at concentrations over time at stations
# Get unique STATION_IDs
station_ids <- unique(df$STATION_ID)

# Loop through each unique STATION_ID 
for (station_id in station_ids) {
  # Subset data for the current STATION_ID
  station_data <- df[df$STATION_ID == station_id, ]
  
  # Plot for the current STATION_ID
  plots <- ggplot(station_data, aes(START_DATE, RESULT)) +
    geom_point() +
    facet_wrap(~PARAMETER, scales = "free_y") +
    ggtitle(paste("Station ID:", station_id))  
  
  # Print each plot
  print(plots)
}

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

df_count <- df %>% count(PARAMETER)

#Rename parameters to get ride of "/" in the names
variables_renamed <- c("TP_UG/L" = "TP_UGL",
                  "TN_MG/L" = "TN_MGL",
                  "TSS_MG/L" = "TSS_MGL",
                  "PH_NONE" = "pH",
                  "DO_sat_%" = "DO_sat",
                  "SPC_UMHO/CM" = "SPC_UMHO_CM",
                  "DO_MG/L" = "DO_MGL",
                  "Temp_Water_DEG C" = "TEMP_WATER_DEGC",
                  "TDN_MG/L" = "TDN_MGL",
                  "NH4_UG/L" = "NH4_UGL",
                  "NO3_NO2_MG/L" = "NO3_NO2_MGL",
                  "PO4_UG/L" = "PO4_UGL",
                  "DON_MG/L" = "DON_MGL",
                  "DOC_MG/L" = "DOC_MGL",
                  "PN_MG/L" = "PN_MGL",
                  "DIN_MG/L" = "DIN_MGL",
                  "CHLA_corrected_pheophytin_UG/L" = "CHLA_corr_pheo_UGL",
                  "NO3_MG/L" = "NO3_MGL",
                  "PC_MG/L" = "PC_MGL",
                  "PHEOPHYTIN-A_UG/L" = "PHEOPHYTIN_A_UGL",
                  "SALINITY_PSS" = "SALINITY_PSS")

# Replace values in PARAMETER column based on lookup table
df$PARAMETER <- variables_renamed[df$PARAMETER]
unique(df$PARAMETER) #check
# Print the updated dataframe
print(df)
summary(df)

#fix Date column class
df$START_DATE <- as.Date(df$START_DATE)

#___________________________________Make data frame wide instead of long __________________________________________________________

df <- df %>%
  pivot_wider(names_from = PARAMETER, values_from = RESULT,
              values_fn = mean)

#Reorganize columns in df
colnames(df)

df <- df %>%
  select(STATION_ID, START_DATE, TP_UGL, PO4_UGL, PN_MGL, TN_MGL, TDN_MGL, NH4_UGL, NO3_MGL, NO3_NO2_MGL, DIN_MGL, DON_MGL, DOC_MGL, TSS_MGL:DO_sat, DO_MGL, SPC_UMHO_CM, SALINITY_PSS, TEMP_WATER_DEGC, CHLA_corr_pheo_UGL)


df$TP_MGL <- conv_unit(df$TP_UGL, "ug", "mg")
df$PO4_MGL <- conv_unit(df$PO4_UGL, "ug", "mg")
df$NH4_MGL <- conv_unit(df$NH4_UGL, "ug", "mg")

#remove UG/L concentration columns
df <- df %>%
  select(STATION_ID, START_DATE, PO4_MGL, PN_MGL, TN_MGL, TDN_MGL, NH4_MGL, NO3_MGL, NO3_NO2_MGL, DIN_MGL, DON_MGL, DOC_MGL,  TSS_MGL:DO_sat, DO_MGL, SPC_UMHO_CM, SALINITY_PSS, TEMP_WATER_DEGC, CHLA_corr_pheo_UGL)

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
df$DIN_MGL <- ifelse(is.na(df$DIN_MGL) & !is.na(df$NH4_MGL) & !is.na(df$NO3_NO2_MGL), df$NH4_MGL + df$NO3_NO2_MGL, df$DIN_MGL)
df$DON_MGL_calc <- df$TDN_MGL - df$NO3_NO2_MGL - df$NH4_MGL

#METHOD DETECTION LIMITS FOR DON (b/c not included in EMD)
df$DON_MDL <- (df$TDN_MGL + df$NO3_NO2_MGL + df$NH4_MGL) * 0.05

#Is DON below MDL?
df$DON_B_MDL <- ifelse(df$DON_MGL_calc < df$DON_MDL, "BDL", "G") 

length(which(df$DON_B_MDL == "BDL")) #9 instances below detection limit

#Set those DON values below detection limit to 1/2 of the method detection limit
df$DON_MGL_calc_final <- ifelse(df$DON_MGL_calc < df$DON_MDL, df$DON_MDL/2, df$DON_MGL_calc)
df$DON_MGL <- df$DON_MGL_calc_final

#Calculate TN as PN + TDN, as we stopped measuring TN directly around ~2015
df$TN_MGL <- ifelse(is.na(df$TN_MGL) & !is.na(df$PN_MGL) & !is.na(df$TDN_MGL), df$PN_MGL + df$TDN_MGL, df$TN_MGL)


df <- df %>%
  select(-DON_MGL_calc, -DON_MDL, - DON_B_MDL, -DON_MGL_calc_final)


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

#Count of concentration measurements
#### Discharge Data Formatting ####
#______________________________________________________________________________________________________________________________________
#________________________________________________________________________________________________________________________________________
#___________________________________________________________________________________________________________________________________________
#Format Discharge Data from USGS Gauges
#Read in discharge data from the three USGS gauges
LR_Q <- read.csv("data/Discharge/LR_Q.csv")
SQR_Q <- read.csv("data/Discharge/SQR_Q.csv")
WNC_Q <- read.csv("data/Discharge/WNC_Q.csv")

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


#Average discharge (m3 s^-1)
Q$year <- year(Q$datetime)

Q_summary <- Q %>%
  group_by(STATION_ID) %>%
  filter(year > 2007 & year < 2019) %>%
  summarize(mean_flow_m3s = round(mean(flow, na.rm=T),2)) 

#Average + standard deviation of each solute for each river
avg_conc <- df %>%
  select(STATION_ID, START_DATE, PO4_MGL:NH4_MGL, NO3_NO2_MGL, DIN_MGL, DOC_MGL, TSS_MGL:DO_MGL, TEMP_WATER_DEGC) %>%
  group_by(STATION_ID) %>%
  summarize(across(PO4_MGL:TEMP_WATER_DEGC, mean, na.rm=T)) %>%
  mutate(PO4_UGL = conv_unit(PO4_MGL, "mg", "ug"),
         NH4_UGL = conv_unit(NH4_MGL, "mg", "ug")) %>%
  select(-PO4_MGL, -NH4_MGL)

avg_conc[,2:11] <- signif(avg_conc[,2:11], 3)

avg_conc <- avg_conc %>%
  pivot_longer(cols=c(PN_MGL:NH4_UGL),names_to = "Solute", values_to = "Concentration")

avg_conc <- avg_conc %>%
  pivot_wider(names_from= "STATION_ID", values_from = "Concentration")

write.csv(avg_conc, "results/main_dataformat/avg_solute_conc_site_2024.csv")

std_conc <- df %>%
  select(STATION_ID, START_DATE, PO4_MGL:NH4_MGL, NO3_NO2_MGL, DIN_MGL, DOC_MGL, TSS_MGL:DO_MGL, TEMP_WATER_DEGC) %>%
  group_by(STATION_ID) %>%
  summarize(across(PO4_MGL:TEMP_WATER_DEGC, sd, na.rm=T))

std_conc <- std_conc %>%
  mutate(PO4_UGL = conv_unit(PO4_MGL, "mg", "ug"),
         NH4_UGL = conv_unit(NH4_MGL, "mg", "ug")) %>%
  select(-PO4_MGL, -NH4_MGL)
std_conc[,2:11] <- signif(std_conc[,2:11], 3)

std_conc <- std_conc %>%
  pivot_longer(cols=c(PN_MGL:NH4_UGL),names_to = "Solute", values_to = "Concentration")

std_conc <- std_conc %>%
  pivot_wider(names_from= "STATION_ID", values_from = "Concentration")

write.csv(std_conc, "results/main_dataformat/std_solute_conc_site_2024.csv")

#Tally of values (n)
tally <- df %>%
  select(STATION_ID, START_DATE, PO4_MGL:NH4_MGL, NO3_NO2_MGL, DIN_MGL, DOC_MGL, TSS_MGL:DO_MGL, TEMP_WATER_DEGC) %>%
  group_by(STATION_ID) %>%
  summarize(across(PO4_MGL:TEMP_WATER_DEGC,function(x) sum(!is.na(x))))
#This data frame has final solute concentrations that can be used for further load analysis
write.csv(df, "results/main_dataformat/df_conc.csv")
