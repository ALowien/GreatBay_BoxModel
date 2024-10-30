# main_dataformat.R

# Author: Anna Mikulis, University of New Hampshire
# Last Updated: 10/29/2024

#R Version 4.4.1 (2024-06-14 ucrt) -- "Race for Your Life"

# Purpose: Read, qaqc, and process raw solute concentration data for the three tidal tributaries (head-of-tide monitoring stations) that flow into Great Bay.
# Also process raw solute concentrations from the estuary, at Adams Point (high and low tide). Prepare discharge data for flux calculations.
# Create physiochemical characteristics tables. 

# Data was sourced from the NH Department of Environmental Services, Environmental Monitoring Database (EMD) by email request. 
# Data was pulled for grab samples and physical chemistry based on assigned water body IDs.

#Data Dictionary

  #EMD data files include metadata on each water body of interest, sampling date and time, measured concentrations of nutrients, carbon, total suspended solids, tidal stage, and physio-chemical parameters. Physio-chemical parameters include snapshot measures of a dissolved oxygen, pH, temperature, and specific conductivity
  
  #Sites
    #05-LMP: Lamprey River (LMP)
    #09-EXT: Squamscott River (SQR)
    #09-EXT-DAMMED: Squamscott River pre dam removal in 2016
    #02-WNC: Winnicut River (WNC)
    #GRBAP: Great Bay Adams Point (Estuarine Monitoring Site)

#To update solute budgets, first request a recent copy of the NH DES EMD database for the above listed sites. 
#Place the new files into the data/emd subfolder (make sure to delete the older file - so as to not have duplicated data).
#Replace the discharge files with up-to-date versions using USGS stream gauge data (waterdata.usgs.gov)
#Run this script to format the EMD dataset and the discharge dataset for later scripts.

#Load required packages.
Packages <- c("readxl", "dplyr", "ggplot2", "tidyquant", "cowplot", "RColorBrewer",
              "stringr",  "plotly", "measurements", "viridis", "moments", "flextable")

lapply(Packages, library, character.only = TRUE)

#### Import DES Site Data ------------------------------------------------
#Files have been amended to put columns names as the first row.
subdir <- "data/emd"
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

#pull metadata out
metadata <- df %>%
  select(STATION_ID, TOWN, STATE, LATITUDE_DECIMAL_DEGREE, LONGITUDE_DECIMAL_DEGREE, LOCATION_DATUM, WATERBODY_NAME, WATERBODY_ID, HUC_12_CODE) %>%
  distinct()

#Select for columns of interest; filter for sites of interest and time period of interest
df <- df %>%
  select(STATION_ID:SAMPLE_SIZE) %>%
  filter(STATION_ID == "05-LMP" | STATION_ID == "02-WNC" | STATION_ID == "09-EXT" | STATION_ID == "09-EXT-DAMMED" |
           STATION_ID == "GRBAP") %>%
  filter(SAMPLE_COLLECTION_METHOD_ID == "TIDALWQ") %>% #Project Name for Tidal Tribs and Adams Point Sampling
  filter(START_DATE > "2008-01-01") #Tidal Tribs sampling program began in 2008

unique(df$STATION_ID) #check for correct site names

#Distinguish GRBAP sampling by low and high tide events; ap_tide stands for Adams Point Tide data
ap_tide <- df %>%
  select(STATION_ID, START_DATE, START_TIME, PARAMETER_ANALYTE, QUALIFIER_AND_RESULTS, RESULT_COMMENTS) %>%
  filter(STATION_ID == "GRBAP") %>%
  filter(PARAMETER_ANALYTE == "TIDE STAGE")

#Pull high/low tide into the RESULTS from the comments. 
ap_tide$QUALIFIER_AND_RESULTS <- ifelse(is.na(ap_tide$QUALIFIER_AND_RESULTS),
                                        str_extract(ap_tide$RESULT_COMMENTS, "(?i)LOW|HIGH"),
                                        ap_tide$QUALIFIER_AND_RESULTS)

# If you want to extract only the first occurrence of LOW or HIGH from RESULT_COMMENTS
#ap_tide$QUALIFIER_AND_RESULTS <- ifelse(is.na(ap_tide$QUALIFIER_AND_RESULTS), str_extract(ap_tide$RESULT_COMMENTS, "(?i)LOW|HIGH")[1], ap_tide$QUALIFIER_AND_RESULTS)

#Categorize "Tide Stage" PARAMETER To be either High or Low Tide (e.x. ebb tides are re-classified as low tide samples)
ap_tide$QUALIFIER_AND_RESULTS <- ifelse(ap_tide$PARAMETER_ANALYTE == "TIDE STAGE" & ap_tide$QUALIFIER_AND_RESULTS == "EBB", "LOW", ap_tide$QUALIFIER_AND_RESULTS)
ap_tide$QUALIFIER_AND_RESULTS <- ifelse(ap_tide$PARAMETER_ANALYTE == "TIDE STAGE" & ap_tide$QUALIFIER_AND_RESULTS == "FLOOD", "HIGH", ap_tide$QUALIFIER_AND_RESULTS)

ap_tide <- ap_tide %>%
  select(-PARAMETER_ANALYTE, -RESULT_COMMENTS)

#Rename Great Bay Adams Point (GRBAP) STATION ID based on high/low tide (GRBAPL indicates low tide sample; GRBAPH indicates high tide sample)
ap_tide$STATION_ID <- ifelse(ap_tide$QUALIFIER_AND_RESULTS == "LOW", "GRBAPL", 
                             ifelse(ap_tide$QUALIFIER_AND_RESULTS == "HIGH", "GRBAPH", NA))

#missing tide code for the high tide sample on 6/29/2009, there were two samples collected that day one at 8:31AM and 11:02 AM. 
#The 11:02AM sample is marked as low tide, so the one earlier is the flood/high tide sample. 
ap_tide[nrow(ap_tide) +1, 1] <- "GRBAPH"
ap_tide[469, 2] <- as.Date("2009-06-29")
ap_tide[469, 3] <- "08:31"
ap_tide[469, 4] <- "High"

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
  select(-STATION_ID.x, -STATION_ID.y) #remove extra Station ID columns now that everything has been combined into one column

# Add the missing High and Low Tide IDS
df$STATION_ID <- ifelse(df$START_DATE == "2008-06-11" & df$START_TIME == "09:35", "GRBAPH",
                        ifelse(df$START_DATE == "2009-06-29" & df$START_TIME == "08:31", "GRBAPH", 
                               ifelse(df$START_DATE == "2009-07-13" & df$START_TIME == "14:17", "GRBAPH",
                                      ifelse(df$START_DATE == "2011-08-22" & df$START_TIME == "07:10", "GRBAPH", df$STATION_ID))))
### END Import DES Data ###

#### Resolve Variable and Column Header Names ------------------------------------------------
#Fix columns that aren't numeric, but should be
df$RDL <- as.numeric(df$RDL) #Result Detection Limit

#Dam removal on Squamscott River occurred in August of 2016; resulting in two different site ids depending on whether sampling occurred before or after the dam removal
#Fix station ID names, 09-EXT and 09-EXT-DAMMED are the same site, just pre and post dam removal
df$STATION_ID <- ifelse(df$STATION_ID == "09-EXT-DAMMED", "09-EXT", df$STATION_ID)

#Subset for columns of interest
df <- df %>%
  select(STATION_ID, WATERBODY_ID, ACTIVITY_ID, ACTIVITY_TYPE, START_DATE:FRACTION_TYPE)

#Filter out unnecessary parameters for the box model
remove_parms <- c("CLOSTRIDIUM PERFRINGENS", "ENTEROCOCCUS", "ESCHERICHIA COLI","TOTAL FECAL COLIFORM", "WIND DIRECTION", "WIND VELOCITY", "SECCHI DISK TRANSPARENCY","COLORED DISSOLVED ORGANIC MATTER (CDOM)", "TURBIDITY","DEPTH", "TIDE STAGE", "LIGHT ATTENUATION COEFFICIENT", "SILICA AS SIO2", "PHEOPHYTIN-A", "CARBON, SUSPENDED", "NITROGEN, NITRITE (NO2) AS N")

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

#any NAs instead of a result value?
sum(is.na(df$QUALIFIER_AND_RESULTS))

#Combine Analytical Method and Source Method ID columns
df <- df %>%
  tidyr::unite("Analytical_Method", ANALYTICAL_METHOD_SOURCE_ID:ANALYTICAL_METHOD, sep = "_")

#Combine detection limit and comments columns
df %>% count(DETECTION_LIMIT_COMMENTS)

### END RESOLVE VARIABLE AND COLUMN HEADER NAMES ###

#### Tidy Results ------------------------------------------------
#Remove additional columns, now that we've filtered down to desired project and sites
df <- df %>%
  select(-WATERBODY_ID, -SAMPLE_COLLECTION_METHOD_ID)

#Activity Comments Review and Assessment
unique(df$ACTIVITY_COMMENTS) 
unique(df$ACTIVITY_TYPE) # Eventually need to remove field duplicates, as they are for QC purposes only (after QC Done)

#How many instances of each parameter being measured?
df_count <- df%>% count(df$PARAMETER)

#Get ride of "<" from the QUALIFIER AND RESULTS Column
df$RESULTS_clean <- stringr::str_replace(df$QUALIFIER_AND_RESULTS, '\\<', '')

#ID those rows that had an "<" so that we know the value is less than the detection limit
df <- df %>% 
  mutate(DETECTION_LIMIT = if_else(str_starts(QUALIFIER_AND_RESULTS, "<"), "BELOW", "NA")) #Below == "<"

df %>% count(df$DETECTION_LIMIT) #296 occurrences where measurement is recorded as being less than/equal to instrument detection limit

df$RESULTS_clean <- as.numeric(df$RESULTS_clean) # this converts everything to numbers
sum(is.na(df$RESULTS_clean)) #no NAs
summary(df)

df_below <- df %>%
  filter(DETECTION_LIMIT == "BELOW")

ggplot(df_below, aes(year(START_DATE))) + 
  geom_histogram(stat="count", aes(fill=PARAMETER)) +
  facet_wrap(~STATION_ID) +
  ylab("# values below detection limit") +
  xlab("year") +
  scale_x_continuous(breaks=seq(from=2008,to=2022,by=2))

df_below$RDL_vs_Qualifer <- df_below$RDL == df_below$RESULTS_clean
#If result is below detection limit, set to 1/2 of detection limit
#296 results that are below detection limit as noted by EMD already
df$RESULT <- ifelse(df$DETECTION_LIMIT == "BELOW", df$RESULTS_clean / 2, df$RESULTS_clean)

df$DETECTION_LIMIT_v2<- ifelse(df$RESULTS_clean < df$RDL, "BELOW RDL", "NA") #5 additional BDLs
#4 out of 5 actual BDL, NH4 at GRBAPH 7/16/2020 rounds to exactly the detection limit, so leave it there to round up
df$RESULTS_clean <- ifelse(df$ACTIVITY_ID == "JEL07162001" & df$PARAMETER == "NH4", signif(df$RESULT, 1), df$RESULTS_clean) #rounding the one that is at detection limit

df$DETECTION_LIMIT_v2<- ifelse(df$RESULTS_clean < df$RDL, "BELOW RDL", "NA")

df$RESULT <- ifelse(df$DETECTION_LIMIT_v2 == "BELOW RDL" & !is.na(df$RDL), df$RDL/ 2, df$RESULT)
summary(df)

### END Tidy Results ###

#### QC ------------------------------------------------
# QC based on existing invalid flags, field duplicates, and method detection limits
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

flextable::flextable(unique_rows)

#Remove the values that have been flagged as bad due to field duplicate failure for RPD or RSD
df <- anti_join(df, unique_rows) #11955 - 76 should be 11879 rows remaining

#How many values are valid vs invalid?
df %>% count(RESULT_VALID)

#Assess the 88 occurrences where results are flagged as not valid and were not removed as part of the above QC lines
df_invalid <- df %>%
  filter(RESULT_VALID == "N") 

ggplot(df_invalid, aes(x=PARAMETER)) +
  geom_histogram(stat="count", aes(fill=STATION_ID)) +
  theme_bw() +
  theme(axis.text.y = element_text(size=13),
        axis.text.x = element_text(size=10, angle=90),
        legend.position = "bottom",
        plot.margin = margin(b = -0.1, unit = "lines",
                             t = 0.5)) +
  ylab("# of Samples & Field Replicates Marked as Not Valid")

ggplot(df_invalid, aes(START_DATE, RESULT, color=STATION_ID)) +
  geom_point() +
  scale_x_date(date_labels = "%m-%y") +
  facet_wrap(~PARAMETER, scales = "free") + xlab("Sample Date month-year")

#Why is DOC invalid for all of 2011 at GRBAP (high and low tide)?
#all flagged, but only some re-run and need to have DOC values replaced with the rerun value
grbap <- df %>%
  filter(STATION_ID == "GRBAPH" | STATION_ID == "GRBAPL") %>%
  filter(PARAMETER == "DOC" | PARAMETER == "TDN") %>%
  select(STATION_ID:ACTIVITY_COMMENTS, PARAMETER, RESULTS_clean) %>%
  tidyr::pivot_wider(names_from="PARAMETER", values_from = "RESULTS_clean") %>%
  filter(START_DATE < "2012-01-01") %>%
  filter(START_DATE > "2010-12-31")

ggplot(grbap, aes(START_DATE, DOC)) + geom_point(aes(color=STATION_ID),size=3) +
  scale_x_date(date_breaks="1 month", date_labels="%b-%Y") + ylab("DOC mgC/L")

ggplot(subset(df, STATION_ID == "GRBAPH" & PARAMETER == "DOC"), 
       aes(START_DATE, RESULT)) + geom_point(size=3, aes(color=RESULT_VALID)) +
  scale_x_date(date_breaks="1 year", date_labels="%b-%Y") + ylab("DOC mgC/L")

#SWMP file of DOC data from Lara Martin
l <- read_excel("./data/misc_data/120516_SWMP_Query_tg.xlsx") %>%
  rename(UNH_ID = "UNH ID #",
         START_DATE = "Collection Date", 
         DOC = "NPOC (mg C/L)",
         TDN = "TDN (mg N/L)") %>%
  select(UNH_ID:`new TDN`, -Lab_Notes, -Project) %>%
  filter(str_starts(`Sample Name`, "A")) %>% 
  filter(!is.na(`New NPOC (mg C/L)`)) %>%
  mutate(DOC = round(DOC, 2),
         TDN = round(TDN, 3))

ggplot(l, aes(DOC, `New NPOC (mg C/L)`)) + geom_point() +
  scale_x_continuous(limits = c(0,12)) +
  scale_y_continuous(limits=c(0,12)) +
  geom_abline(slope=1, intercept=0, color="red") +
  theme(axis.title = element_text(size=15),
        axis.text = element_text(size=12)) +
  ylab("Rerun DOC mg C/L") +
  xlab("Original DOC mg C/L")

docreplace <- left_join(l, grbap)

#use docreplace to hard code the shifts in DOC & one TDN value below 
df$RESULT <- ifelse(df$STATION_ID == "GRBAPH" & df$ACTIVITY_ID == "NER08221102" & df$PARAMETER == "DOC", 5.17, df$RESULT)

df$RESULT <- ifelse(df$STATION_ID == "GRBAPH" & df$ACTIVITY_ID == "NER08221102" & df$PARAMETER == "TDN", 0.408, df$RESULT)

df$RESULT<- ifelse(df$STATION_ID == "GRBAPL" & df$ACTIVITY_ID == "NER08221105" & df$PARAMETER == "DOC", 2.589, df$RESULT)

df$RESULT<- ifelse(df$STATION_ID == "GRBAPH" & df$ACTIVITY_ID == "NER09261104" & df$PARAMETER == "DOC", 2.589, df$RESULT)

df$RESULT <- ifelse(df$STATION_ID == "GRBAPL" & df$ACTIVITY_ID == "NER09261111" & df$PARAMETER == "DOC",3.110, df$RESULT)

#Mark replaced values as valid now
df$RESULT_VALID <- ifelse(df$STATION_ID == "GRBAPH" & df$ACTIVITY_ID == "NER08221102" & df$PARAMETER == "DOC", "Y", df$RESULT_VALID)
df$RESULT_VALID <- ifelse(df$STATION_ID == "GRBAPH" & df$ACTIVITY_ID == "NER08221102" & df$PARAMETER == "TDN", "Y", df$RESULT_VALID)
df$RESULT_VALID <- ifelse(df$STATION_ID == "GRBAPL" & df$ACTIVITY_ID == "NER08221105" & df$PARAMETER == "DOC", "Y", df$RESULT_VALID)
df$RESULT_VALID <- ifelse(df$STATION_ID == "GRBAPH" & df$ACTIVITY_ID == "NER09261104" & df$PARAMETER == "DOC", "Y", df$RESULT_VALID)
df$RESULT_VALID <- ifelse(df$STATION_ID == "GRBAPL" & df$ACTIVITY_ID == "NER09261111" & df$PARAMETER == "DOC", "Y", df$RESULT_VALID)

#flag salinity on 7/20/2015 at low tide as invalid (way to low to be real)
df$RESULT_VALID <- ifelse(df$ACTIVITY_ID == "JEL07201502" & df$PARAMETER == "SALINITY", 
                          "N", df$RESULT_VALID)
#Flag DO saturation on 6/22/2015 at GRBAPH because its 9.6% when all other samples are 73-128%
df$RESULT_VALID <- ifelse(df$ACTIVITY_ID == "JEL06221508" & df$PARAMETER == "DO_sat", 
                          "N", df$RESULT_VALID)

#look through remaining not valid data
invalid <- df %>%
  filter(RESULT_VALID == "N")

#keep 2011 DOC from GRBAP unless above 7 mg/L
invalid$NEW_RESULT_VALID<- ifelse(invalid$PARAMETER == "DOC" & invalid$START_DATE > 
                                    "2011-01-01" & invalid$START_DATE < "2011-12-30" & invalid$RESULT < 7.00, 
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

#REMOVE FIELD DUPLICATES Now that QC is done
df <- df %>%
 filter(ACTIVITY_TYPE == "SAMPLE - ROUTINE") #11877 to 9799 rows

### END QC ### 

#### Salinity ------------------------------------------------
# Did dam removal at 09-EXT affect chemistry? In other words, is there tidal influence after 2016 seen in the salinity record?
salinity <- df %>%
  filter(STATION_ID == "09-EXT") %>%
  filter(PARAMETER == "SPC" | PARAMETER == "SALINITY")

salinity$Salinity <- ifelse(salinity$PARAMETER == "SPC", 
                                 salinity$RESULT* 0.00064, salinity$RESULT)

#fit linear model salinity over time
salinity$numeric_date <- as.numeric(salinity$START_DATE)
model <- lm(Salinity ~ numeric_date, data = salinity)
summary(model)
seg_model <- segmented::segmented(model, 
                       seg.Z = ~numeric_date)
summary(seg_model)

plot(salinity$START_DATE, salinity$Salinity, pch = 16, 
    main = "Breakpoint Linear Regression with Salinity")
lines(salinity$numeric_date, fitted(seg_model), col = "red", lwd = 2)

t <- segmented::davies.test(model, k=500)
print(t) #breakpoint is not significant

salinity$fitted_values <- fitted(seg_model)

break_point_plot <- ggplot(salinity, aes(x = START_DATE, y = Salinity)) +
  geom_point(size = 2) +  # Scatter points
  geom_vline(xintercept = as.Date("2016-08-12"), linetype="dashed", linewidth=1) +
  scale_x_date(date_breaks="1 year",date_labels = "%Y") +
  geom_line(aes(y = fitted_values), color = "red", linewidth = 1) +  # Fitted line
  labs(x = "Date", 
       y = "Salinity (psu)") +
  theme_cowplot() +
  theme(axis.title = element_text(size=18),
       axis.text=element_text(size=15))
break_point_plot
ggsave(break_point_plot, file=paste0("./results/figures/supplemental/EXT_salinity_breakpoint.jpeg"), dpi=300, bg="white")

median_SPC <- median(salinity$RESULTS_clean)
mean_SPC <- mean(salinity$RESULTS_clean)
sd_SPC <- sd(salinity$RESULTS_clean)

salinity$Dam <- ifelse(salinity$START_DATE < "2016-08-12", "pre removal", "post removal")
salinity$Dam <- factor(salinity$Dam, levels = c("pre removal", "post removal"))

salplot <- ggplot(salinity, aes(year(START_DATE),RESULTS_clean, group=year(START_DATE))) + geom_boxplot(outlier.shape = NA) +
  geom_point(aes(color=Dam), position=position_jitter(width=0.15)) +
  theme_cowplot() +
  scale_x_continuous(limits=c(2007.5,2021.5), breaks=seq(from=2008,to=2021,by=1)) +
  ylab("Specific Conductance") +
  xlab("Sample Year") +
  scale_color_manual(values=c("black", "blue1"), name="Dam Removal") +
  scale_y_continuous(limits=c(0,372), breaks=seq(from=0,to=350,by=50)) +
  theme(axis.title = element_text(size=18),
        axis.text=element_text(size=15),
        legend.title = element_text(size=18),
        legend.text = element_text(size=15))
salplot

ggsave(salplot, file=paste0("./results/figures/supplemental/Salinity_ext.jpeg"), dpi=300, bg="white")

salinity_annual <- salinity %>%
  mutate(year=year(START_DATE)) %>%
  group_by(year) %>%
  summarize(mean=mean(RESULTS_clean),
            sd=sd(RESULTS_clean))

sqr_annual_spc <- ggplot(salinity_annual, aes(year, mean)) + geom_point(size=3) + 
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) + ggtitle("Squamscott River") +
  scale_y_continuous(limits=c(0,575), breaks = seq(from=0,to=600,by=100)) + 
  xlab("Year") +
  scale_x_continuous(limits=c(2007.75,2021.25), breaks=seq(from=2008,to=2021, by=1))  + ylab("Annual Mean Specific Conductance") +
  theme_cowplot() +
  theme(axis.title = element_text(size=16),
        axis.text=element_text(size=12),
        axis.text.x = element_text(angle=90,hjust=1, vjust=0.1),
        axis.title.y = element_blank())
sqr_annual_spc 

#Does Lamprey show simliar pre-2016 increase in salinity or specific conductance? 
salinity_lmp <- df %>%
   filter(STATION_ID == "05-LMP") %>%
   filter(PARAMETER == "SPC") %>%
   rename(SPC=PARAMETER)

ggplot(salinity_lmp, aes(START_DATE, RESULTS_clean)) + geom_point() + geom_line() +
  geom_vline(xintercept = as.Date("2016-08-12"), linetype="dashed") 

salinity_annual_lmp <- salinity_lmp %>%
     mutate(year=year(START_DATE)) %>%
     group_by(year) %>%
     summarize(mean=mean(RESULTS_clean),  sd=sd(RESULTS_clean))

lmp_annual_spc <- ggplot(salinity_annual_lmp, aes(year, (mean))) + geom_point(size=3) + 
  scale_y_continuous(limits=c(0,575), breaks = seq(from=0,to=600,by=100)) + 
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
  ggtitle("Lamprey River") +  xlab("Year") +
  scale_x_continuous(limits=c(2007.75,2021.25), breaks=seq(from=2008,to=2021, by=1)) + 
  ylab("Annual Mean Specific Conductance") +
  theme_cowplot() +
  theme(axis.title = element_text(size=16),
        axis.text=element_text(size=12),
        axis.text.x = element_text(angle=90,hjust=1)) 
lmp_annual_spc

#Does Winnicut show temporal increase?
salinity_wnc <- df %>%
  filter(STATION_ID == "02-WNC") %>%
  filter(PARAMETER == "SPC") %>%
  rename(SPC=PARAMETER)

ggplot(salinity_wnc, aes(START_DATE, RESULTS_clean)) + geom_point() + geom_line() +
  geom_vline(xintercept = as.Date("2016-08-12"), linetype="dashed") 

salinity_annual_wnc<- salinity_wnc %>%
  mutate(year=year(START_DATE)) %>%
  group_by(year) %>%
  summarize(mean=mean(RESULTS_clean),  sd=sd(RESULTS_clean))

wnc_annual_spc <- ggplot(salinity_annual_wnc, aes(year, (mean))) + geom_point(size=3) + 
  scale_y_continuous(limits=c(0,575), breaks = seq(from=0,to=600,by=100)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) + ggtitle("Winnicut River") +
  scale_x_continuous(limits=c(2007.75,2021.25), breaks=seq(from=2008,to=2021, by=1)) + ylab("Mean Annual Specific Conductance") +
  xlab("Year") +
  theme_cowplot() +
  theme(axis.title = element_text(size=16),
        axis.text=element_text(size=12),
        axis.text.x = element_text(angle=90,hjust=1),
        axis.title.y = element_blank())
wnc_annual_spc

summary(lm(mean ~ year, data=salinity_annual_wnc))

summary(lm(mean ~ year, data=salinity_annual_lmp))

sqr_annual_mod <- lm(mean ~ year, data=salinity_annual)
seg_model.sqr <- segmented::segmented(sqr_annual_mod, 
                       seg.Z = ~year,
                       psi = 2016)
summary(seg_model.sqr) # thinks 2020 is breakpoint in annual mean on SQR

annual_salinity_plot <-  gridExtra::grid.arrange(lmp_annual_spc, sqr_annual_spc, wnc_annual_spc, ncol=3 )
ggsave(annual_salinity_plot, file=paste0("./results/figures/supplemental/salinitybyriver.jpeg"), dpi=300, bg="white")

### END Salinity ### 

#### Convert Dataframe from Long to Wide ------------------------------------------------
#Thin out to columns necessary for converting from long to wide
df <- df %>%
  select(STATION_ID, START_DATE, PARAMETER, UNITS, RESULT) %>%
  tidyr::unite("PARAMETER", PARAMETER:UNITS, sep= "_")

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
#check that unit conversion and renaming worked by plotting PO4
ggplot(subset(df, PARAMETER == "PO4_UGL"), aes(START_DATE, RESULT, color=STATION_ID)) + geom_point() +
  facet_wrap(~STATION_ID)

#Check for duplicates prior to pivoting dataframe, as duplicates complicate things
duplicates <- df %>%
  group_by(STATION_ID, START_DATE, PARAMETER) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1L) #no duplicates once field replicates are removed

#Plot solutes over time
plots_list <- list()

# Loop over each unique STATION_ID
for (station in unique(df$STATION_ID)) {
  
  # Filter the dataframe for the current station
  station_df <- df[df$STATION_ID == station, ]
  
  # Create the plot for the current station
  p <- ggplot(station_df, aes(START_DATE, RESULT)) +
    geom_point() +
    facet_wrap(~PARAMETER, scales = "free_y", nrow = 2) +
    scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(paste("Station:", station))
  
  # Save the plot to the list with the station ID as the name
  plots_list[[station]] <- p
}

# Display a plot (for example, for the first station)
print(plots_list[[5]]) #change number sequentially and rerun (1-5) to get all 5 station id plots

#Make data frame wide instead of long 
df <- df %>%
  tidyr::pivot_wider(id_cols = c(STATION_ID, START_DATE), 
              names_from = PARAMETER, values_from = RESULT)

#Reorganize columns in df
colnames(df)

df <- df %>% #remove pH column
  select(STATION_ID, START_DATE, TP_UGL, PO4_UGL, PN_MGL, TN_MGL, TDN_MGL, NH4_UGL, NO3_NO2_MGL, 
         DIN_MGL, DON_MGL, DOC_MGL, TSS_MGL:DO_sat, 
         DO_MGL, SPC_UMHO_CM, SALINITY_PSS, TEMP_WATER_DEGC, CHLA_corr_pheo_UGL)
### END Convert Dataframe from Long to Wide ###

#### Final QC ------------------------------------------------
#make everything mg/L for solute load calculations later
df$TP_MGL <- conv_unit(df$TP_UGL, "ug", "mg")
df$PO4_MGL <- conv_unit(df$PO4_UGL, "ug", "mg")
df$NH4_MGL <- conv_unit(df$NH4_UGL, "ug", "mg")

#remove UG/L concentration columns, remove TP (not enough stations measured it)
df <- df %>%
  select(STATION_ID, START_DATE, PO4_MGL, PN_MGL, TN_MGL, TDN_MGL, NH4_MGL, NO3_NO2_MGL, DIN_MGL, DON_MGL, DOC_MGL, 
         TSS_MGL:DO_sat, DO_MGL, SPC_UMHO_CM, SALINITY_PSS, TEMP_WATER_DEGC, CHLA_corr_pheo_UGL)

#Plug in early DOC data from 2008-2009
DOC_data <- read.csv("./data/misc_data/DOC_tidaltribs_pre2010.csv") %>%
  select(-X) %>%
  rename(DOC_MGL.trib = DOC_MGL) 

DOC_data$START_DATE <- as.Date(DOC_data$START_DATE)

df <- full_join(df, DOC_data)

df$DOC_MGL <- ifelse(is.na(df$DOC_MGL), df$DOC_MGL.trib, df$DOC_MGL)

df <- df %>% select(-DOC_MGL.trib)

#Data from Water Quality Analysis Lab until EMD updates
#ADD in winter tidal tribs data 2020 - 2022 
#ADD Tidal Tribs 2022 and 2023 data from Michelle
#ADD Adams point 2023 data and winter data.
update <- read.csv("./data/misc_data/updated_2023.csv") %>%
  select(-X,-Project,-UNH_ID)

update$START_DATE <- as.Date(update$START_DATE)
colnames(update)
colnames(df)
update <- update %>% select(-CMass, -NMass)

df <- full_join(df, update)

#Recalculate DIN & DON
df$DIN_MGL <- round(df$DIN_MGL,3)
df$DIN_MGL_calc <- round(ifelse(!is.na(df$NH4_MGL) & !is.na(df$NO3_NO2_MGL), df$NH4_MGL + df$NO3_NO2_MGL, NA),3)
df$DON_MGL_calc <- round(ifelse(!is.na(df$NH4_MGL) & !is.na(df$NO3_NO2_MGL) & !is.na(df$TDN_MGL),  df$TDN_MGL - df$NO3_NO2_MGL - df$NH4_MGL, NA),3)

#compare DIN to calculated DIN
din_comparison <- ggplot(df, aes(DIN_MGL, DIN_MGL_calc, color=DIN_MGL == DIN_MGL_calc)) + geom_point() +
  geom_point(aes(text = paste("Date:", START_DATE, STATION_ID))) + 
  geom_abline(slope=1, intercept = 0, color="black", alpha=0.5)
din_comparison
ggplotly(din_comparison)

din_df <- df %>%
  select(STATION_ID, START_DATE, TDN_MGL, DIN_MGL, DIN_MGL_calc, NH4_MGL, NO3_NO2_MGL) %>%
  filter(DIN_MGL != DIN_MGL_calc) %>%
  mutate(percent_diff = abs((DIN_MGL - DIN_MGL_calc))/DIN_MGL * 100)

#use recalculated DIN
df$DIN_MGL <- df$DIN_MGL_calc
  
df <- df %>%
  select(-DIN_MGL_calc)

don_df <- df %>%
  select(STATION_ID, START_DATE, TDN_MGL, DIN_MGL, DON_MGL, DON_MGL_calc, NH4_MGL, NO3_NO2_MGL) %>%
  filter(DON_MGL != DON_MGL_calc) %>%
  mutate(percent_diff = abs((DON_MGL - DON_MGL_calc))/DON_MGL * 100)

don_comparison <- ggplot(don_df, aes(DON_MGL, DON_MGL_calc, color= percent_diff)) + geom_point() +
  geom_point(aes(text = paste("Date:", START_DATE, STATION_ID))) + 
  geom_abline(slope=1, intercept = 0, color="black", alpha=0.5)
don_comparison

ggplotly(don_comparison)

#METHOD DETECTION LIMITS FOR DON (b/c not included in EMD)
df$DON_MDL <- (df$TDN_MGL + df$NO3_NO2_MGL + df$NH4_MGL) * 0.05

#Is DON below MDL?
df$DON_B_MDL <- ifelse(df$DON_MGL_calc < df$DON_MDL, "BDL", "G") 

length(which(df$DON_B_MDL == "BDL")) #12 instances below detection limit

#Set those DON values below detection limit to 1/2 of the method detection limit
df$DON_MGL_calc_final <- ifelse(df$DON_MGL_calc < df$DON_MDL, df$DON_MDL/2, df$DON_MGL_calc)
df$DON_MGL <- df$DON_MGL_calc_final

df <- df %>%
  select(-DON_MGL_calc, -DON_MDL, - DON_B_MDL, -DON_MGL_calc_final)

#Calculate TN as PN + TDN, as we stopped measuring TN directly around ~2015
ggplot(df, aes(START_DATE, TN_MGL)) + geom_point(aes(color=STATION_ID))

#Incorporate this test into this section of code
#Use calculated TN wherever possible
#df$TN_CALC_MGL <- df$TDN_MGL + df$PN_MGL
#df$TN_combo <- ifelse(!is.na(df$TN_CALC_MGL), df$TN_CALC_MGL, df$TN_MGL)
#df$TN_combo_notes <- ifelse(!is.na(df$TN_CALC_MGL) & !is.na(df$TN_combo), "calc", "measure")

#ggplot(df, aes(START_DATE, TN_combo, color=TN_combo_notes)) + geom_point() +
  #facet_wrap(~STATION_ID)

#summary_TN_combo <- df %>%
 # group_by(STATION_ID,TN_combo_notes) %>%
 # summarise(count = n())

#summary_TN_combo

df$TN_MGL_calc <- ifelse(!is.na(df$PN_MGL) & !is.na(df$TDN_MGL), df$PN_MGL + df$TDN_MGL, df$TN_MGL)

ggplot(df, aes(TN_MGL, TN_MGL_calc)) + geom_point() +
  geom_abline(slope=1, intercept = 0, color="black", alpha=0.5)

ggplot(df, aes(START_DATE, TN_MGL_calc)) + geom_point(aes(color=STATION_ID))

df$TN_MGL <- df$TN_MGL_calc

df <- df %>%
  select(-TN_MGL_calc)

#assess normality of TSS values
skewness(df$TSS_MGL, na.rm=T)
kurtosis(df$TSS_MGL, na.rm=T)

#Remove the three high TSS concentrations at Adams Point Low Tide due to anecdotal knowledge from Jackson Estuarine Laboratory that winter values are worse for TSS b/c dock is out of the water
df <- df %>%
 mutate(TSS_MGL = ifelse(START_DATE == "2010-03-29" & STATION_ID == "GRBAPL", NA, TSS_MGL)) %>%
  mutate(TSS_MGL = ifelse(START_DATE == "2008-11-25" & STATION_ID == "GRBAPL", NA, TSS_MGL)) %>%
  mutate(TSS_MGL = ifelse(START_DATE == "2012-01-30" & STATION_ID == "GRBAPL", NA, TSS_MGL))
  
skewness(df$TSS_MGL, na.rm=T) #notable improvement in skewness from 13 to 2
kurtosis(df$TSS_MGL, na.rm=T) #notable improvement in kurtosis from 251 to 9


#Plot a couple of parameters
ggplot(df, aes(START_DATE, PO4_MGL)) + geom_point() +
  scale_x_date(date_breaks="2 year", date_labels =  "%Y") +
  facet_wrap(~STATION_ID)

#number of values at 1/2 of Po4 detection limit
count_po4 <- sum(df$PO4_MGL == 0.0025, na.rm=T)
count_po4

#pull high PO4 values because Seal throws errors sometimes
df$PO4_MGL <- ifelse(df$STATION_ID == "02-WNC" & df$START_DATE == "2022-08-24", NA, df$PO4_MGL)

df$PO4_MGL <- ifelse(df$STATION_ID == "GRBAPH" & df$START_DATE == "2019-07-22", NA, df$PO4_MGL)

df$PO4_MGL <- ifelse(df$STATION_ID == "GRBAPH" & df$START_DATE == "2021-08-10", NA, df$PO4_MGL)

#Final QC
#Tally of values (n)
tally <- df %>%
  mutate(Year = year(START_DATE)) %>%
  select(STATION_ID, Year, PO4_MGL:NH4_MGL, NO3_NO2_MGL, DIN_MGL, DOC_MGL, TSS_MGL:DO_MGL, TEMP_WATER_DEGC) %>%
  group_by(STATION_ID, Year) %>%
  summarize(across(PO4_MGL:TEMP_WATER_DEGC,function(x) sum(!is.na(x))))

tally_piv <- tally %>%
  tidyr::pivot_longer(cols=c(PO4_MGL:TEMP_WATER_DEGC), names_to="Parameter", values_to="count")

ggplot(tally_piv, aes(Year, count)) +
  geom_col(aes(fill=STATION_ID), position="dodge") +
  geom_hline(yintercept = 8) +
  facet_wrap(~Parameter) +
  scale_x_continuous(breaks=seq(from=2008,to=2023, by=1)) +
  theme(axis.text.x = element_text(angle=90))

ggplot(df, aes(START_DATE, (PO4_MGL*1000), color=STATION_ID)) + geom_point() +
  facet_wrap(~STATION_ID) +
  ylab("PO4 ug/L") +
  scale_x_date(date_breaks="1 year", date_labels = "%Y") +
  theme(legend.position = "none")

ggplot(df, aes(START_DATE, DOC_MGL, color=STATION_ID)) + geom_point() +
  facet_wrap(~STATION_ID, scales="free") +
  ylab("DOC mg/L") +
  scale_x_date(date_breaks="1 year", date_labels = "%Y") +
  theme(legend.position = "none")

ggplot(df, aes(START_DATE, TSS_MGL, color=STATION_ID)) + geom_point() +
  facet_wrap(~STATION_ID) +
  ylab("TSS mg/L") +
  scale_x_date(date_breaks="1 year", date_labels = "%Y") +
  theme(legend.position = "none")


qa_df <- df %>%
  filter(STATION_ID != "GRBAPH" & STATION_ID != "GRBAPL")

#TN values
qa_df$TN_CALC_MGL <- qa_df$TDN_MGL + qa_df$PN_MGL

qa_df$TN_combo <- ifelse(!is.na(qa_df$TN_CALC_MGL), qa_df$TN_CALC_MGL, qa_df$TN_MGL)
qa_df$TN_combo_notes <- ifelse(!is.na(qa_df$TN_CALC_MGL), "calc", "measure")

ggplot(qa_df, aes(START_DATE, TN_combo, color=TN_combo_notes)) + 
  geom_point(aes(y=TN_MGL), color="black", size=3) +
  geom_point(size=3) +
  facet_wrap(~STATION_ID)

ggplot(qa_df, 
       aes(TN_MGL, TN_CALC_MGL, color=TSS_MGL)) +
  geom_point(size=3) +
  scale_color_viridis() +
  geom_abline(slope=1, intercept=0) +
  facet_wrap(~STATION_ID, scales = "free") +
  xlab("TN mg/L (Analytical Measure)") +
  ylab("TN mg/L (Calculated)")



df <- df %>%
  select(-TN_MGL, - TN_CALC_MGL, TN_combo_notes) %>%
  rename(TN_MGL = TN_combo)

df$DIN_greater <- df$DIN_MGL > df$TDN_MGL



#### Discharge Data Formatting ------------------------------------------------
#Format Discharge Data from USGS Gauges
#Read in discharge data from the three USGS gauges
LR_Q <- read.csv("data/discharge/LR_Q.csv")
SQR_Q <- read.csv("data/discharge/SQR_Q.csv")
WNC_Q <- read.csv("data/discharge/WNC_Q.csv")

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
  filter(year > 2007 & year < 2024) %>%
  summarize(mean_flow_m3s = round(mean(flow, na.rm=T),2)) 
Q_summary #for study sites section in methods


#02-WNC
wnc <- df %>%
  filter(STATION_ID == "02-WNC")

WNC_Q$START_DATE <- as.Date(WNC_Q$START_DATE)

wnc <- full_join(wnc, WNC_Q) %>%
  filter(START_DATE > "2008-01-01")

ggplot(wnc, aes(Q_mean_cfs, TSS_MGL)) + geom_point()

ggplot(wnc, aes(Q_mean_cfs, TN_MGL)) + geom_point()

ggplot(wnc, aes(Q_mean_cfs, DOC_MGL)) + geom_point() +
  scale_y_continuous(limits=c(0,18))

ggplot(wnc, aes(Q_mean_cfs, TN_MGL, color=year(START_DATE))) + geom_point() +
  scale_x_continuous(limits=c(0,250)) +
  scale_y_continuous(limits=c(0,2.6)) +
  ylab("TN mg/L") +
  xlab("Mean Instantaneous Discharge (cfs)") +
  labs(color="Year") +
  theme(axis.text = element_text(size=15),
        axis.title = element_text(size=20))
  
ggplot(wnc, aes(START_DATE, Q_mean_cfs)) +  geom_point(color="blue", size=2) + 
  geom_line(color="blue") + 
  geom_point(aes(y=TSS_MGL*10), color="black", size=3) +
  scale_y_continuous(limits=c(0,856),
                     sec.axis = sec_axis(~./10 , name = "TSS mgL")) +
  ggtitle("02-WNC Discharge and TSS data")

ggplot(wnc, aes(START_DATE, Q_mean_cfs)) + geom_point(color="blue", size=2) + 
  geom_line(color="blue") + 
  geom_point(aes(y=TN_MGL*332), color="black", size=3) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Show each year as x-axis label
  scale_y_continuous(limits=c(0,855),
                     sec.axis = sec_axis(~./332 , name = "TN mgL")) +
  ggtitle("02-WNC Discharge and TN data") +
  theme(axis.text = element_text(size=15),
        axis.title = element_text(size=15)) +
  ylab("Discharge (cfs)") +
  xlab("Time")

ggplot(wnc, aes(START_DATE, Q_mean_cfs)) + geom_point(color="blue", size=2) + 
  geom_line(color="blue") + 
  geom_point(aes(y=PN_MGL*2898), color="black", size=3) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Show each year as x-axis label
  scale_y_continuous(limits=c(0,855),
                     sec.axis = sec_axis(~./2898 , name = "PN mgL")) +
  ggtitle("02-WNC Discharge and PN data")

lmp <- df %>%
  filter(STATION_ID == "05-LMP")

LR_Q$START_DATE <- as.Date(LR_Q$START_DATE)

lmp <- full_join(lmp, LR_Q) %>%
  filter(START_DATE > "2008-01-01")

ggplot(lmp, aes(Q_mean_cfs, TSS_MGL)) + geom_point() +
  scale_x_continuous(limits=c(0,5000)) +
  scale_y_continuous(limits=c(0,15)) +
  ggtitle("05-LMP")

ggplot(lmp, aes(Q_mean_cfs, TN_MGL)) + geom_point()


ggplot(lmp, aes(START_DATE, Q_mean_cfs)) +  geom_point(color="blue", size=2) + 
  geom_line(color="blue") + 
  geom_point(aes(y=TSS_MGL*506), color="black", size=3) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Show each year as x-axis label
  scale_y_continuous(limits=c(0,6550),
                     sec.axis = sec_axis(~./506 , name = "TSS mgL")) +
  ggtitle("05 LMP Discharge and TSS data")


ggplot(lmp, aes(START_DATE, Q_mean_cfs)) +  geom_point(color="blue", size=2) + 
  geom_line(color="blue") + 
  geom_point(aes(y=DOC_MGL*500), color="black", size=3) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Show each year as x-axis label
  scale_y_continuous(limits=c(0,6550),
                     sec.axis = sec_axis(~./500 , name = "DOC mgL")) +
  ggtitle("05 LMP Discharge and DOC data")


ggplot(lmp, aes(START_DATE, Q_mean_cfs)) +  geom_point(color="blue", size=2) + 
  geom_line(color="blue") + 
  geom_point(aes(y=PO4_MGL*214516), color="black", size=3) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Show each year as x-axis label
  scale_y_continuous(limits=c(0,6550),
                     sec.axis = sec_axis(~./214516 , name = "PO4 mgL")) +
  ggtitle("05 LMP Discharge and PO4 data")


ext <- df %>%
  filter(STATION_ID == "09-EXT")

SQR_Q$START_DATE <- as.Date(SQR_Q$START_DATE)

ext <- full_join(ext, SQR_Q) %>%
  filter(START_DATE > "2008-01-01")

ggplot(ext, aes(Q_mean_cfs, TSS_MGL)) + geom_point() +
  ggtitle("09-EXT")

ggplot(ext, aes(START_DATE, Q_mean_cfs)) +  geom_point(color="blue", size=2) + 
  geom_line(color="blue") + 
  geom_point(aes(y=TSS_MGL*210), color="black", size=3) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Show each year as x-axis label
  scale_y_continuous(limits=c(0,2700),
                     sec.axis = sec_axis(~./210, name = "TSS mgL")) +
  ggtitle("09 EXT Discharge and TSS data")

#Final QAQC ROUND
columns_of_interest <- c("PO4_MGL", "PN_MGL", "TN_MGL", "TDN_MGL", "NH4_MGL", 
                         "NO3_MGL", "NO3_NO2_MGL", "DIN_MGL", "DON_MGL", 
                         "DOC_MGL", "TSS_MGL", "TEMP_WATER_DEGC", "DO_MGL", 
                         "SPC_UMHO_CM", "DO_sat", "SALINITY_PSS", "CHLA_corr_pheo_UGL", 
                         "Turbidity")

# Filter for the specific stations
df_filtered <- df %>%
  filter(STATION_ID %in% c("02-WNC", "05-LMP", "09-EXT", "GRBAPH", "GRBAPL"))

# Initialize a list to store the plots
plot_list <- list()

# Loop over each parameter and create a plot
for (param in columns_of_interest) {
  
  # Create a ggplot for each parameter
  p <- ggplot(df_filtered, aes(x = START_DATE, y = .data[[param]], color = STATION_ID)) +
    geom_line() +
    geom_point() +
    facet_wrap(~ STATION_ID, scales = "free_y") +  # Facet by Station ID, free scales for y-axis
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Show each year as x-axis label
    theme_minimal() +
    labs(x = "Date", y = param, title = paste("Time Series of", param, "Faceted by Station")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Convert to interactive plot using plotly
  interactive_plot <- ggplotly(p, tooltip = c("x", "y"))
  
  # Store the plot in the list
  plot_list[[param]] <- interactive_plot
}

# Access individual plots using plot_list
plot_list[["NO3_NO2_MGL"]]


#SUMMARIZE FINAL DATAFRAME AND SAVE IT

#Average + standard deviation of each solute for each river
colnames(df)

avg_conc <- df %>%
  select(STATION_ID, START_DATE, TN_MGL, PO4_MGL:DO_MGL, DO_sat) %>%
  mutate(PO4_UGL = conv_unit(PO4_MGL, "mg", "ug"),
         NH4_UGL = conv_unit(NH4_MGL, "mg", "ug")) %>%
  select(-PO4_MGL, -NH4_MGL) %>%  
  group_by(STATION_ID) %>%
  summarize(across(TN_MGL:NH4_UGL,  \(x) mean(x, na.rm=T)) )

avg_conc[,2:15] <- signif(avg_conc[,2:15], 3)

avg_conc <- avg_conc %>%
  tidyr::pivot_longer(cols=c(TN_MGL:NH4_UGL),names_to = "Solute", values_to = "Concentration")

avg_conc <- avg_conc %>%
  tidyr::pivot_wider(names_from= "STATION_ID", values_from = "Concentration")

write.csv(avg_conc, "results/main_dataformat/avg_solute_conc_site_2024.csv")

std_conc <- df %>%
  select(STATION_ID, START_DATE, TN_MGL, PO4_MGL:DO_MGL, DO_sat) %>%
  mutate(PO4_UGL = conv_unit(PO4_MGL, "mg", "ug"),
         NH4_UGL = conv_unit(NH4_MGL, "mg", "ug")) %>%
  select(-PO4_MGL, -NH4_MGL) %>%  
  group_by(STATION_ID) %>%
  summarize(across(TN_MGL:NH4_UGL, \(x) sd(x, na.rm=T)))

std_conc[,2:15] <- signif(std_conc[,2:15], 3)

std_conc <- std_conc %>%
  tidyr::pivot_longer(cols=c(TN_MGL:NH4_UGL),names_to = "Solute", values_to = "Concentration")

std_conc <- std_conc %>%
  tidyr::pivot_wider(names_from= "STATION_ID", values_from = "Concentration")

write.csv(std_conc, "results/main_dataformat/std_solute_conc_site_2024.csv")


summary_tally <- df %>%
  select(STATION_ID, START_DATE, TN_MGL, PO4_MGL:DO_MGL, DO_sat) %>%
  group_by(STATION_ID) %>%
  summarize(across(TN_MGL:DO_sat, ~ sum(!is.na(.))))

tally <- summary_tally %>%
  tidyr::pivot_longer(cols=c(TN_MGL:DO_sat), names_to = "Parameter", values_to="Count") %>%
  tidyr::pivot_wider(names_from = STATION_ID,
              values_from = Count)

tally$Parameter <- ifelse(tally$Parameter == "NH4_MGL", "NH4_UGL", tally$Parameter)
tally$Parameter <- ifelse(tally$Parameter == "PO4_MGL", "PO4_UGL", tally$Parameter)

# Pivot avg_conc to long format
avg_conc_long <- avg_conc %>%
  tidyr::pivot_longer(cols = -Solute, names_to = "Source", values_to = "Mean") %>%
  rename(Parameter = Solute)

# Pivot std_conc to long format
std_conc_long <- std_conc %>%
  tidyr::pivot_longer(cols = -Solute, names_to = "Source", values_to = "SD") %>%
  rename(Parameter = Solute)

# Pivot tally to long format
tally_long <- tally %>%
  tidyr::pivot_longer(cols = -Parameter, names_to = "Source", values_to = "Count")

# Combine the dataframes
combined_data <- avg_conc_long %>%
  left_join(std_conc_long, by = c("Parameter", "Source")) %>%
  left_join(tally_long, by = c("Parameter", "Source")) %>%
  mutate(Result = paste0(signif(Mean, 2), " ± ", signif(SD, 2), " (", Count, ")")) %>%
  select(Parameter, Source, Result) %>%
  tidyr::pivot_wider(names_from = Source, values_from = Result)

combined_data <- combined_data %>% filter(Parameter != "NO3_MGL")

#create levels
custom_order <- c("DOC_MGL","PO4_UGL", "NH4_UGL", "NO3_NO2_MGL", "DIN_MGL", "DON_MGL", "TDN_MGL", "PN_MGL", "TN_MGL",  "TSS_MGL", "DO_MGL", "DO_sat"  ,"TEMP_WATER_DEGC")

combined_data$name <- factor(combined_data$Parameter, levels = custom_order)

combined_data <- combined_data %>%
  arrange(name)

write.csv(combined_data, file=paste0("./results/manuscript_figures/supplemental/table_s1/avg_concentrations_supplemental.csv")) 

#This data frame has final solute concentrations that can be used for further load analysis
write.csv(df, "results/main_dataformat/df_conc.csv")


#Summarize physical-chemical parameter columns for Appendix Tables
ggplot(df, aes(START_DATE, DO_sat)) + geom_point() + facet_wrap(~STATION_ID)

df_DO <- df %>%
  group_by(STATION_ID) %>%
  summarize(mean_DO_sat = mean(DO_sat, na.rm=T), sd_DO_sat = sd(DO_sat, na.rm=T), count = n())

ggplot(df, aes(START_DATE, DO_MGL)) + geom_point() + facet_wrap(~STATION_ID)
df_DOmgl <- df %>%
  group_by(STATION_ID) %>%
  summarize(mean_DO_mgl = mean(DO_MGL, na.rm=T), sd_DO_mgl = sd(DO_MGL, na.rm=T), count = n())

ggplot(df, aes(START_DATE, TEMP_WATER_DEGC)) + geom_point() + facet_wrap(~STATION_ID)
df_temp <- df %>%
  group_by(STATION_ID) %>%
  summarize(mean_temp = mean(TEMP_WATER_DEGC, na.rm=T), sd_temp = sd(TEMP_WATER_DEGC, na.rm=T), count = n())

#Metadata version
#Combine metadata back in with the QC'd df dataframe
metadata
df
