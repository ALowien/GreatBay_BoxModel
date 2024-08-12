#main_load_calc.R

#Author: Anna Mikulis, University of New Hampshire
#Last Updated: 8/12/2024

#This script calculates annual (calendar year) and monthly loads for the three tidal tributaries (Lamprey, Squamscott, and Winnicut) to Great Bay.
#This script pulls in products created in the main_dataformat.R script, including measured water quality concentrations and discharge readings. Start with an empty environment. 

#Load required packages.
Packages <- c("readxl", "dplyr", "ggplot2", "measurements", "plotly", "lubridate","cowplot", "ggpubr", "dataRetrieval", "gridExtra", "tidyr", "viridis")
lapply(Packages, library, character.only = TRUE)

#Read in cleaned up concentration data frame (df_conc.csv) from the main_dataformat.R script
  #Saved in results/main_dataformat
#the dataframe is tidal tributary solute concentrations over time and includes site id, sample collection date, solute concentrations, and water chemistry (pH, DO)
conc <- read.csv("results/main_dataformat/df_conc.csv")

conc$START_DATE<- as.POSIXct(conc$START_DATE) #fix class of date column

names(conc)[names(conc)== "START_DATE"] <- "datetime"

conc$Month <- month(conc$datetime)
conc$Year <- year(conc$datetime)

#Build data frame with site id, date of sample collection, and solutes concentrations (Ex. NH4_UGL, TDN_MGL)
conc_sub <- conc %>%
  select(STATION_ID:TSS_MGL, Month, Year) %>%
  select(-NO3_MGL) #delete NO3, SiO2, and PC columns b/c they are empty 

count <- conc_sub %>%
  group_by(STATION_ID, Year) %>%
  summarize(across(PO4_MGL:TSS_MGL, ~ sum(!is.na(.x)))) %>%
  pivot_longer(cols=c(PO4_MGL:TSS_MGL), names_to= "Parameter", values_to="Count") 
  
ggplot(count, aes(Year, Count, fill=STATION_ID)) + geom_bar(stat="identity", position="dodge") + facet_wrap(~Parameter) +
  geom_hline(yintercept=7) + theme_bw() +
  scale_x_continuous(limits=c(2007, 2024), breaks=seq(from=2007, to=2024, by=2))

#make a table of years with insufficient concentration data
years_to_omit <- count %>%
  filter(Count < 7)

write.csv(years_to_omit, "./results/main_load_calc/years_to_omit.csv")
#___________________________________________________________________
#______________________________________________________________________
#Read in discharge data frame created in main_dataformat.R
Q <- read.csv("results/main_dataformat/Q_tidal_tribs.csv")

Q <- Q %>% #flow is daily mean Q in m^3/s
  select(STATION_ID, datetime, flow)

Q$datetime <- as.POSIXct(Q$datetime) #fix class of date column
#_________________________________________________________________________________________________________________________
#___________________________________________________________________________________________________________________________

#Separate concentration and flow data by Station IDs to facilitate correct union of conc and flow
#Separate out Lamprey River Flow and Solute Concentrations 
flow.LR <- Q %>% #Separates discharge for Lamprey
  filter(STATION_ID == "05-LMP") %>%
  select(datetime, flow)

#Split out concentrations to just Lamprey
conc.LR <- conc_sub %>% #separates concentrations for Lamprey
  filter(STATION_ID == "05-LMP") %>%
  select(datetime, PO4_MGL:TSS_MGL)

#Date formatting for each dataframe
flow.LR$datetime <- as.POSIXct(flow.LR$datetime, format = "%Y-%m-%d %H:%M:%S")
conc.LR$datetime <- as.POSIXct(conc.LR$datetime, format = "%Y-%m-%d %H:%M:%S")

#Add arbitrary time stamp to dates to make date matching easier
flow.LR$datetime <-lubridate::ymd_hm(paste(flow.LR$datetime, "6:00 PM"))

conc.LR$datetime <-lubridate::ymd_hm(paste(conc.LR$datetime, "6:00 PM"))

head(flow.LR)
head(conc.LR)

union.LR <- left_join(flow.LR, conc.LR)
write.csv(union.LR, "results/main_load_calc/union.LR.csv") #Saving a file of discharge & LMP concentrations saved together
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#Repeat for Squamscott River
#Separate out Squamscott River Flow and Solute Concentrations 
flow.SQR <- Q %>%
  filter(STATION_ID == "09-EXT") %>%
  select(datetime, flow) 

#Split out concentrations to just Squamscott
conc.SQR <- conc_sub %>%
  filter(STATION_ID == "09-EXT") %>%
  select(datetime, PO4_MGL:TSS_MGL) 

#Date formatting
flow.SQR$datetime <- as.POSIXct(flow.SQR$datetime, format = "%Y-%m-%d %H:%M:%S")
conc.SQR$datetime <- as.POSIXct(conc.SQR$datetime, format = "%Y-%m-%d %H:%M:%S")

#Add arbitrary time stamp to dates
flow.SQR$datetime <-lubridate::ymd_hm(paste(flow.SQR$datetime, "6:00 PM"))
conc.SQR$datetime <-lubridate::ymd_hm(paste(conc.SQR$datetime, "6:00 PM"))

union.SQR <- left_join(flow.SQR, conc.SQR)
write.csv(union.SQR, "results/main_load_calc/union.SQR.csv") #Saving a file of discharge & SQR concentrations saved together
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#Repeat this for Winnicut River
flow.WNC <- Q %>%
  filter(STATION_ID == "02-WNC") %>%
  select(datetime, flow)        

#Split out conc to just Winnicut
conc.WNC <- conc_sub %>%
  filter(STATION_ID == "02-WNC") %>%
  select(datetime, PO4_MGL:TSS_MGL)

#Date formatting
flow.WNC$datetime <- as.POSIXct(flow.WNC$datetime, format = "%Y-%m-%d %H:%M:%S")
conc.WNC$datetime <- as.POSIXct(conc.WNC$datetime, format = "%Y-%m-%d %H:%M:%S")

#Add arbitrary time stamp to dates
flow.WNC$datetime <-lubridate::ymd_hm(paste(flow.WNC$datetime, "6:00 PM"))
conc.WNC$datetime <-lubridate::ymd_hm(paste(conc.WNC$datetime, "6:00 PM"))

union.WNC <- left_join(flow.WNC, conc.WNC)
write.csv(union.WNC, "results/main_load_calc/union.WNC.csv") #Saving a file of discharge & WNC concentrations saved together

#Flow_Multipliers to resolve difference in location b/t stream gauge and head-of-tide
#These are calculated as the ratio of watershed area to watershed area at the stream gauge; multipliers sourced from PREP State of Our Estuary 2018 Report
LMP_Flow_Multiplier <- 1.145435
SQR_Flow_Multiplier <- 1.683529
WNC_Flow_Multiplier <- 1.005443

# Flow Weighted Load Calculations Lamprey River (05-LMP) (ANNUAL ESTIMATES) ----------------------
#ANNUAL ESTIMATE (CY)
#Add Calendar Year
union.LR$CY <- year(union.LR$datetime)

#Scale flow units to m3/day and correct with the flow-multiplier

#Convert flow from m3/s to m3/day by multiplying by 86,400 seconds/day; 
union.LR$flow_m3_day <- union.LR$flow * 86400
#Multiply the flow m3/day by the lamprey flow multiplier
union.LR$flow_m3_day_cor <- union.LR$flow_m3_day * LMP_Flow_Multiplier

#convert units from m3 to liters
union.LR$flow_l_day <- conv_unit(union.LR$flow_m3_day_cor, "m3", "l")

#Test m3 to L conversion
conv_unit(1, "m3", "l") # good returns 1000L

union.LR <- union.LR %>%
  select(datetime, CY, flow_l_day, PO4_MGL:TSS_MGL) %>%
  filter(CY > 2007 & CY < 2024) # have sampling data through annual year 2018 currently

#Flow Weighted Concentrations Lamprey River
#Flow Weighted Flux calculated by multiplying conc * daily average discharge of sampling day (corrected by flow multiplier)
#Sum those and divide by sum of discharge on sample days
#Multiply concentration (mg/L) by daily average flow (L/day) = mg/day

#flow in LR_FW_conc is the flow-multiplier corrected daily average flow (l/day)
LR_FW_conc <- union.LR %>%
  mutate(PO4 = PO4_MGL * flow_l_day,
         PN = PN_MGL * flow_l_day,
         TN = TN_MGL * flow_l_day,
         TDN = TDN_MGL * flow_l_day,
         NH4 = NH4_MGL * flow_l_day,
         NO3_NO2 = NO3_NO2_MGL * flow_l_day,
         DIN = DIN_MGL * flow_l_day,
         DON = DON_MGL * flow_l_day,
         DOC = DOC_MGL * flow_l_day,
         TSS = TSS_MGL * flow_l_day) %>%
  select(datetime, CY, flow_l_day, PO4:TSS)

#Remove empty columns and rows where solutes were not measured
LR_FW_conc <- LR_FW_conc %>%
  filter_at(.vars = vars(PO4:TSS), .vars_predicate = any_vars(!is.na(.)))  #only want days where we sampled

#pull out Month for grouping purposes
LR_FW_conc$Month <- month(LR_FW_conc$datetime) #Extract month

LR_FW_conc <- LR_FW_conc %>%
 select(datetime, CY, Month, flow_l_day, PO4:TSS)

LR_FW_conc$Empty <- NA 

LR_FW_conc$PO4_flow <- ifelse(LR_FW_conc$PO4, LR_FW_conc$flow_l_day, LR_FW_conc$Empty)
LR_FW_conc$PN_flow <- ifelse(LR_FW_conc$PN, LR_FW_conc$flow_l_day, LR_FW_conc$Empty)
LR_FW_conc$TN_flow <- ifelse(LR_FW_conc$TN, LR_FW_conc$flow_l_day, LR_FW_conc$Empty)
LR_FW_conc$TDN_flow <- ifelse(LR_FW_conc$TDN, LR_FW_conc$flow_l_day, LR_FW_conc$Empty)
LR_FW_conc$NH4_flow <- ifelse(LR_FW_conc$NH4, LR_FW_conc$flow_l_day, LR_FW_conc$Empty)
LR_FW_conc$NO3_NO2_flow <- ifelse(LR_FW_conc$NO3_NO2, LR_FW_conc$flow_l_day, LR_FW_conc$Empty)
LR_FW_conc$DIN_flow <- ifelse(LR_FW_conc$DIN, LR_FW_conc$flow_l_day, LR_FW_conc$Empty)
LR_FW_conc$DON_flow <- ifelse(LR_FW_conc$DON, LR_FW_conc$flow_l_day, LR_FW_conc$Empty)
LR_FW_conc$DOC_flow <- ifelse(LR_FW_conc$DOC, LR_FW_conc$flow_l_day, LR_FW_conc$Empty)
LR_FW_conc$TSS_flow <- ifelse(LR_FW_conc$TSS, LR_FW_conc$flow_l_day, LR_FW_conc$Empty)

#CY ANNUAL ESTIMATE
#Group by CY and sum
LR_FW_CY_Sums <- LR_FW_conc %>%
  select(-Month) %>%
  group_by(CY) %>%
  summarise_if(is.numeric, sum, na.rm =T)

LR_FW_CY <- LR_FW_CY_Sums %>%
  mutate(FW_PO4 = PO4/PO4_flow,
         FW_PN = PN/PN_flow,
         FW_TN = TN/TN_flow,
         FW_TDN = TDN/TDN_flow,
         FW_NH4 = NH4/NH4_flow,
         FW_NO3_NO2 = NO3_NO2/NO3_NO2_flow,
         FW_DIN = DIN/DIN_flow,
         FW_DON = DON/DON_flow,
         FW_DOC = DOC/DOC_flow,
         FW_TSS = TSS/TSS_flow) %>%
  select(CY, FW_PO4:FW_TSS)

LR_FW_CY$STATION_ID <- "Lamprey"

#save flow weighted concentrations
write.csv(LR_FW_CY, "results/main_load_calc/FWC/CY_FW_Conc_LMP.csv")

LR_FW_CY <- LR_FW_CY %>% select(-STATION_ID)

for (i in 2:11) {
  LR_FW_CY[,i] <-conv_unit(LR_FW_CY[,i], "mg", "kg")
}
conv_unit(1, "mg", "kg")
#Take CY Flow-Weighted Concentrations and multiply by annual flow (corrected with the flow-multiplier)

#Multiply CY Flow_Weighted Concentrations by Annual Flow (multiplied by flow-multiplier)
#[kg/L] * L/year

flow.LR$CY <- year(flow.LR$datetime)

flow.LR$flow <- conv_unit(flow.LR$flow, "m3", "l") * LMP_Flow_Multiplier #l/s
flow.LR$flow_day <- flow.LR$flow * 86400 #L/s * 86400 s/day = L/day

CY.flow.LR <- flow.LR %>%
  group_by(CY) %>%
  summarize(Flow_l_year = sum(flow_day))


#Join Flow-weighted concentrations with annual CY loads
FW_Solutes <- c("FW_PO4", "FW_PN", "FW_TN", "FW_TDN", "FW_NH4", "FW_NO3_NO2", "FW_DIN", "FW_DON", "FW_DOC", "FW_TSS")

LR_CY_Loads <- left_join(LR_FW_CY, CY.flow.LR)

LR_CY_Loads <- LR_CY_Loads %>% #Loads are in kg/year
  mutate(across(all_of(FW_Solutes), ~ . * Flow_l_year))

#Calendar Year Annual FW Load (uses annual discharge and concentrations March-December)
write.csv(LR_CY_Loads, "results/main_load_calc/FW_Loads/LR_Annual_Loads24.csv")

#_______________________________________________________________________________________________
#________________________________________________________________________________________________________________________
#Monthly Loads for Lamprey River
LR_FW_MSums <- LR_FW_conc %>% #LR_FW_conc is mg/L * L/day, not yet divided by L/day to FW 
  group_by(CY, Month) %>%
  summarise_if(is.numeric, sum, na.rm =T)

LR_FW_M <- LR_FW_MSums %>% 
  mutate(FW_PO4 = PO4/PO4_flow, #solutes is mg/day Flow is l/day 
         FW_PN = PN/PN_flow,
         FW_TN = TN/TN_flow,
         FW_TDN = TDN/TDN_flow,
         FW_NH4 = NH4/NH4_flow,
         FW_NO3_NO2 = NO3_NO2/NO3_NO2_flow,
         FW_DIN = DIN/DIN_flow,
         FW_DON = DON/DON_flow,
         FW_DOC = DOC/DOC_flow,
         FW_TSS = TSS/TSS_flow) %>%
  select(CY, Month, FW_PO4:FW_TSS) #doesn't really do anything because only one concentration per month, you end up dividing by what you multiplied by 

#Multiply monthly FW conc in
for (i in 3:11) {
  LR_FW_M[,i] <-conv_unit(LR_FW_M[,i], "mg", "kg")
}

flow.LR.m <- flow.LR #flow_day is l/day (corrected for the flow multiplier)
flow.LR.m$Month <- month(flow.LR.m$datetime)
summary(flow.LR.m)

flow.LR.m <- flow.LR.m %>%
  select(datetime, Month, CY, flow_day) %>%
  group_by(Month, CY) %>%
  summarize(flow_month = sum(flow_day)) #L/month

LR_M_Loads <- left_join(LR_FW_M, flow.LR.m)

LR_M_Loads <- LR_M_Loads %>%
  mutate(across(all_of(FW_Solutes), ~ . * flow_month))

#As a test, if we sum the monthly loads by CY, do we get similar estimate as the LR CY Loads?
LR_M_Loads_CY_Test <- LR_M_Loads %>%
  group_by(CY) %>%
  summarize(across(FW_Solutes, sum, na.rm=T)) #lower than the CY estimate, which makes sense because CY estimate used Jan - Dec in Annual discharge, whereas summing the months, we get March -Dec

write.csv(LR_M_Loads, "results/main_load_calc/FW_Loads/LR_MLoads_kg_month.csv")

#### end #### 

# Flow Weighted Load Calculations Squamscott River (09-EXT) (ANNUAL ESTIMATES) --------
#ANNUAL ESTIMATE (CY)
#Add Calendar Year
union.SQR$CY <- year(union.SQR$datetime)

#Scale flow units to m3/day and correct with the flow_multiplier 

#Convert flow from m3/s to m3/day by multiplying by 86,400 seconds/day
union.SQR$flow_m3_day <- union.SQR$flow * 86400
#Multiply the flow m3/day by the Squamscott River Flow Multiplier
union.SQR$flow_m3_day_cor <- union.SQR$flow_m3_day * SQR_Flow_Multiplier

#Convert from m3 to liters
union.SQR$flow_l_day <- conv_unit(union.SQR$flow_m3_day_cor, "m3", "l")


union.SQR <- union.SQR %>%
  select(datetime, CY, flow_l_day, PO4_MGL:TSS_MGL) %>%
  filter(CY > 2007 & CY < 2024) # have sampling data through annual year 2018 currently

#Flow Weighted Concentrations Squamscott River
#Flow Weighted Flux calculated by multiplying conc * daily average discharge of sampling day (corrected by flow multiplier).
#Sum those and divide by sum of discharge on sample days
#Multiply concentration (mg/L) by daily average flow (L/day) = mg/day
SQR_FW_conc <- union.SQR %>%
  mutate(PO4 = PO4_MGL * flow_l_day,
         PN = PN_MGL * flow_l_day,
         TN = TN_MGL * flow_l_day,
         TDN = TDN_MGL * flow_l_day,
         NH4 = NH4_MGL * flow_l_day,
         NO3_NO2 = NO3_NO2_MGL * flow_l_day,
         DIN = DIN_MGL * flow_l_day,
         DON = DON_MGL * flow_l_day,
         DOC = DOC_MGL * flow_l_day,
         TSS = TSS_MGL * flow_l_day) %>%
  select(datetime, CY, flow_l_day, PO4:TSS)

#Remove empty columns and rows where solutes were not measured
SQR_FW_conc <- SQR_FW_conc %>%
  filter_at(.vars = vars(PO4:TSS), .vars_predicate = any_vars(!is.na(.)))  #only want days where we sampled

#Pull out Month for grouping purposes
SQR_FW_conc$Month <- month(SQR_FW_conc$datetime) #Extract month

SQR_FW_conc <- SQR_FW_conc %>%
  select(datetime, CY, Month, flow_l_day, PO4:TSS)

SQR_FW_conc$Empty <- NA 

SQR_FW_conc$PO4_flow <- ifelse(SQR_FW_conc$PO4, SQR_FW_conc$flow_l_day, SQR_FW_conc$Empty)
SQR_FW_conc$PN_flow <- ifelse(SQR_FW_conc$PN, SQR_FW_conc$flow_l_day, SQR_FW_conc$Empty)
SQR_FW_conc$TN_flow <- ifelse(SQR_FW_conc$TN, SQR_FW_conc$flow_l_day, SQR_FW_conc$Empty)
SQR_FW_conc$TDN_flow <- ifelse(SQR_FW_conc$TDN, SQR_FW_conc$flow_l_day, SQR_FW_conc$Empty)
SQR_FW_conc$NH4_flow <- ifelse(SQR_FW_conc$NH4, SQR_FW_conc$flow_l_day, SQR_FW_conc$Empty)
SQR_FW_conc$NO3_NO2_flow <- ifelse(SQR_FW_conc$NO3_NO2, SQR_FW_conc$flow_l_day, SQR_FW_conc$Empty)
SQR_FW_conc$DIN_flow <- ifelse(SQR_FW_conc$DIN, SQR_FW_conc$flow_l_day, SQR_FW_conc$Empty)
SQR_FW_conc$DON_flow <- ifelse(SQR_FW_conc$DON, SQR_FW_conc$flow_l_day, SQR_FW_conc$Empty)
SQR_FW_conc$DOC_flow <- ifelse(SQR_FW_conc$DOC, SQR_FW_conc$flow_l_day, SQR_FW_conc$Empty)
SQR_FW_conc$TSS_flow <- ifelse(SQR_FW_conc$TSS, SQR_FW_conc$flow_l_day, SQR_FW_conc$Empty)

#CY ANNUAL ESTIAMTE
#Group by CY and sum
SQR_FW_CY_Sums <- SQR_FW_conc %>%
  select(-Month) %>%
  group_by(CY) %>%
  summarise_if(is.numeric, sum, na.rm =T)

SQR_FW_CY <- SQR_FW_CY_Sums %>%
  mutate(FW_PO4 = PO4/PO4_flow,
         FW_PN = PN/PN_flow,
         FW_TN = TN/TN_flow,
         FW_TDN = TDN/TDN_flow,
         FW_NH4 = NH4/NH4_flow,
         FW_NO3_NO2 = NO3_NO2/NO3_NO2_flow,
         FW_DIN = DIN/DIN_flow,
         FW_DON = DON/DON_flow,
         FW_DOC = DOC/DOC_flow,
         FW_TSS = TSS/TSS_flow) %>%
  select(CY, FW_PO4:FW_TSS)


SQR_FW_CY$STATION_ID <- "Squamscott"
write.csv(SQR_FW_CY, "results/main_load_calc/FWC/CY_FW_Conc_SQR.csv")

SQR_FW_CY <- SQR_FW_CY %>%
  select(-STATION_ID)

for (i in 2:11) {
  SQR_FW_CY[,i] <-conv_unit(SQR_FW_CY[,i], "mg", "kg")
}

#Take CY Flow-Weighted Concentrations and multiply by annual flow (corrected with the flow-multiplier)

#Multiply CY Flow-Weighted Concentrations by Annual Flow (multiplied by flow-multiplier)
#[kg/L] * [L/year]

flow.SQR$CY <- year(flow.SQR$datetime)

flow.SQR$flow <- conv_unit(flow.SQR$flow, "m3", "l") * SQR_Flow_Multiplier #l/s
flow.SQR$flow_day <- flow.SQR$flow * 86400 #l/s to 86400s/day = l/day

CY.flow.SQR <- flow.SQR %>%
  group_by(CY) %>%
  summarize(Flow_l_year = sum(flow_day))

#Join Flow-weighted concentrations with annual CY loads
SQR_CY_Loads <- left_join(SQR_FW_CY, CY.flow.SQR)

SQR_CY_Loads <- SQR_CY_Loads %>% #Loads are in kg/year
  mutate(across(all_of(FW_Solutes), ~ . * Flow_l_year))
#Calendar Year Annual FW Load (uses annual discharge and concentrations March-December)
write.csv(SQR_CY_Loads, "results/main_load_calc/FW_Loads/SQR_Annual_Loads24.csv")

#________________________________________________________________________________________________
#______________________________________________________________________________________________________________________
#Monthly Loads for the Squamscott 
SQR_FW_MSums <- SQR_FW_conc %>% #SQR_FW_conc is mg/L * L/day, not yet divided by L/day to FW 
  group_by(CY, Month) %>%
  summarise_if(is.numeric, sum, na.rm =T)

SQR_FW_M <- SQR_FW_MSums %>% 
  mutate(FW_PO4 = PO4/PO4_flow, #TP is mg/day Flow is l/day 
         FW_PN = PN/PN_flow,
         FW_TN = TN/TN_flow,
         FW_TDN = TDN/TDN_flow,
         FW_NH4 = NH4/NH4_flow,
         FW_NO3_NO2 = NO3_NO2/NO3_NO2_flow,
         FW_DIN = DIN/DIN_flow,
         FW_DON = DON/DON_flow,
         FW_DOC = DOC/DOC_flow,
         FW_TSS = TSS/TSS_flow) %>%
  select(CY, Month, FW_PO4:FW_TSS) #doesn't really do anything because only one concentration per month, you end up dividing by what you multiplied by 

#Multiply monthly FW conc in
for (i in 3:12) {
  SQR_FW_M[,i] <-conv_unit(SQR_FW_M[,i], "mg", "kg")
}

flow.SQR.m <- flow.SQR #flow_day is l/day (corrected for the flow multiplier)
flow.SQR.m$Month <- month(flow.SQR.m$datetime)
summary(flow.SQR.m)

flow.SQR.m <- flow.SQR.m %>%
  select(datetime, Month, CY, flow_day) %>%
  group_by(Month, CY) %>%
  summarize(flow_month = sum(flow_day)) #L/month

SQR_M_Loads <- left_join(SQR_FW_M, flow.SQR.m)

SQR_M_Loads <- SQR_M_Loads %>%
  mutate(across(FW_Solutes,  ~.* flow_month))

#As a test, if we sum the monthly loads by CY, do we get similiar estimate as the SQR CY Loads?

SQR_M_Loads_CY_Test <- SQR_M_Loads %>%
  group_by(CY) %>%
  summarize(across(FW_Solutes, sum, na.rm=T)) #lower that the CY estimate, which makes sense because CY estimate used Jan - Dec in Annual discharge, whereas summing the months, we get March -Dec

write.csv(SQR_M_Loads, "results/main_load_calc/FW_Loads/SQR_MLoads_kg_month.csv")

#### end Squamscott Loads #### 


# Flow Weighted Load Calculations Winnicut River (02-WNC) (ANNUAL ESTIMATES)  --------
#ANNUAL ESTIMATE (CY)

#Add Calendar Year
union.WNC$CY <- year(union.WNC$datetime)

#Scale flow units to m3/day and correct with the flow_multiplier 

#Convert flow from m3/s to m3/day by multiplying by 86,400 seconds/day
union.WNC$flow_m3_day <- union.WNC$flow * 86400
#Multiply the flow m3/day by the Squamscott River Flow Multiplier
union.WNC$flow_m3_day_cor <- union.WNC$flow_m3_day * WNC_Flow_Multiplier

#Convert from m3 to liters
union.WNC$flow_l_day <- conv_unit(union.WNC$flow_m3_day_cor, "m3", "l")


union.WNC <- union.WNC %>%
  select(datetime, CY, flow_l_day, PO4_MGL:TSS_MGL) %>%
  filter(CY > 2007 & CY < 2024) # have sampling data through annual year 2018 currently

#Flow Weighted Concentrations Squamscott River
#Flow Weighted Flux calculated by multiplying conc * daily average discharge of sampling day (corrected by flow multiplier).
#Sum those and divide by sum of discharge on sample days
#Multiply concentration (mg/L) by daily average flow (L/day) = mg/day
WNC_FW_conc <- union.WNC %>%
  mutate(PO4 = PO4_MGL * flow_l_day,
         PN = PN_MGL * flow_l_day,
         TN = TN_MGL * flow_l_day,
         TDN = TDN_MGL * flow_l_day,
         NH4 = NH4_MGL * flow_l_day,
         NO3_NO2 = NO3_NO2_MGL * flow_l_day,
         DIN = DIN_MGL * flow_l_day,
         DON = DON_MGL * flow_l_day,
         DOC = DOC_MGL * flow_l_day,
         TSS = TSS_MGL * flow_l_day) %>%
  select(datetime, CY, flow_l_day, PO4:TSS)

#Remove empty columns and rows where solutes were not measured
WNC_FW_conc <- WNC_FW_conc %>%
  filter_at(.vars = vars(PO4:TSS), .vars_predicate = any_vars(!is.na(.)))  #only want days where we sampled

#Pull out Month for grouping purposes
WNC_FW_conc$Month <- month(WNC_FW_conc$datetime) #Extract month

WNC_FW_conc <- WNC_FW_conc %>%
  select(datetime, CY, Month, flow_l_day, PO4:TSS)

WNC_FW_conc$Empty <- NA 

WNC_FW_conc$PO4_flow <- ifelse(WNC_FW_conc$PO4, WNC_FW_conc$flow_l_day, WNC_FW_conc$Empty)
WNC_FW_conc$PN_flow <- ifelse(WNC_FW_conc$PN, WNC_FW_conc$flow_l_day, WNC_FW_conc$Empty)
WNC_FW_conc$TN_flow <- ifelse(WNC_FW_conc$TN, WNC_FW_conc$flow_l_day, WNC_FW_conc$Empty)
WNC_FW_conc$TDN_flow <- ifelse(WNC_FW_conc$TDN, WNC_FW_conc$flow_l_day, WNC_FW_conc$Empty)
WNC_FW_conc$NH4_flow <- ifelse(WNC_FW_conc$NH4, WNC_FW_conc$flow_l_day, WNC_FW_conc$Empty)
WNC_FW_conc$NO3_NO2_flow <- ifelse(WNC_FW_conc$NO3_NO2, WNC_FW_conc$flow_l_day, WNC_FW_conc$Empty)
WNC_FW_conc$DIN_flow <- ifelse(WNC_FW_conc$DIN, WNC_FW_conc$flow_l_day, WNC_FW_conc$Empty)
WNC_FW_conc$DON_flow <- ifelse(WNC_FW_conc$DON, WNC_FW_conc$flow_l_day, WNC_FW_conc$Empty)
WNC_FW_conc$DOC_flow <- ifelse(WNC_FW_conc$DOC, WNC_FW_conc$flow_l_day, WNC_FW_conc$Empty)
WNC_FW_conc$TSS_flow <- ifelse(WNC_FW_conc$TSS, WNC_FW_conc$flow_l_day, WNC_FW_conc$Empty)

#CY ANNUAL ESTIAMTE
#Group by CY and sum
WNC_FW_CY_Sums <- WNC_FW_conc %>%
  select(-Month) %>%
  group_by(CY) %>%
  summarise_if(is.numeric, sum, na.rm =T)

WNC_FW_CY <- WNC_FW_CY_Sums %>%
  mutate(FW_PO4 = PO4/PO4_flow,
         FW_PN = PN/PN_flow,
         FW_TN = TN/TN_flow,
         FW_TDN = TDN/TDN_flow,
         FW_NH4 = NH4/NH4_flow,
         FW_NO3_NO2 = NO3_NO2/NO3_NO2_flow,
         FW_DIN = DIN/DIN_flow,
         FW_DON = DON/DON_flow,
         FW_DOC = DOC/DOC_flow,
         FW_TSS = TSS/TSS_flow) %>%
  select(CY, FW_PO4:FW_TSS)

WNC_FW_CY$STATION_ID <- "Winnicut"
write.csv(WNC_FW_CY, "results/main_load_calc/FWC/CY_FW_Conc_WNC.csv")

WNC_FW_CY <- WNC_FW_CY %>% select(-STATION_ID)
for (i in 2:11) {
  WNC_FW_CY[,i] <-conv_unit(WNC_FW_CY[,i], "mg", "kg")
}

#Take CY Flow-Weighted Concentrations and multiply by annual flow (corrected with the flow-multiplier)

#Multiply CY Flow-Weighted Concentrations by Annual Flow (multiplied by flow-multiplier)
#[kg/L] * [L/year]

flow.WNC$CY <- year(flow.WNC$datetime)

flow.WNC$flow <- conv_unit(flow.WNC$flow, "m3", "l") * WNC_Flow_Multiplier #l/s
flow.WNC$flow_day <- flow.WNC$flow * 86400 #l/s to 86400s/day = l/day

CY.flow.WNC <- flow.WNC %>%
  group_by(CY) %>%
  summarize(Flow_l_year = sum(flow_day))

#Join Flow-weighted concentrations with annual CY loads
WNC_CY_Loads <- left_join(WNC_FW_CY, CY.flow.WNC)

WNC_CY_Loads <- WNC_CY_Loads %>% #Loads are in kg/year
  mutate(across(all_of(FW_Solutes), ~ . * Flow_l_year))

#Calendar Year Annual FW Load (uses annual discharge and concentrations March-December)
write.csv(WNC_CY_Loads, "results/main_load_calc/FW_Loads/WNC_Annual_Loads24.csv")

#________________________________________________________________________________________________
#______________________________________________________________________________________________________________________
#Monthly Loads for the Winnicut
WNC_FW_MSums <- WNC_FW_conc %>% #WNC_FW_conc is mg/L * L/day, not yet divided by L/day to FW 
  group_by(CY, Month) %>%
  summarise_if(is.numeric, sum, na.rm =T)

WNC_FW_M <- WNC_FW_MSums %>% 
  mutate(FW_PO4 = PO4/PO4_flow, #TP is mg/day Flow is l/day 
         FW_PN = PN/PN_flow,
         FW_TN = TN/TN_flow,
         FW_TDN = TDN/TDN_flow,
         FW_NH4 = NH4/NH4_flow,
         FW_NO3_NO2 = NO3_NO2/NO3_NO2_flow,
         FW_DIN = DIN/DIN_flow,
         FW_DON = DON/DON_flow,
         FW_DOC = DOC/DOC_flow,
         FW_TSS = TSS/TSS_flow) %>%
  select(CY, Month, FW_PO4:FW_TSS) #doesn't really do anything because only one concentration per month, you end up dividing by what you multiplied by 

#Multiply monthly FW conc in
for (i in 3:12) {
  WNC_FW_M[,i] <-conv_unit(WNC_FW_M[,i], "mg", "kg")
}

flow.WNC.m <- flow.WNC #flow_day is l/day (corrected for the flow multiplier)
flow.WNC.m$Month <- month(flow.WNC.m$datetime)
summary(flow.WNC.m)

flow.WNC.m <- flow.WNC.m %>%
  select(datetime, Month, CY, flow_day) %>%
  group_by(Month, CY) %>%
  summarize(flow_month = sum(flow_day)) #L/month

WNC_M_Loads <- left_join(WNC_FW_M, flow.WNC.m)

WNC_M_Loads <- WNC_M_Loads %>%
  mutate(across(all_of(FW_Solutes), ~ . * flow_month))

#As a test, if we sum the monthly loads by CY, do we get similiar estimate as the WNC CY Loads?

WNC_M_Loads_CY_Test <- WNC_M_Loads %>%
  group_by(CY) %>%
  summarize(across(FW_Solutes, sum, na.rm=T)) #lower that the CY estimate, which makes sense because CY estimate used Jan - Dec in Annual discharge, whereas summing the months, we get March -Dec

write.csv(WNC_M_Loads, "results/main_load_calc/FW_Loads/WNC_MLoads_kg_month.csv")

#### end Winnicut Loads #### 

#Combine the three "annual"  CY load data frames into one
LR_CY_Loads$Station_ID <- "Lamprey"
SQR_CY_Loads$Station_ID <- "Squamscott"
WNC_CY_Loads$Station_ID <- "Winnicut"

Tidal_Trib_CY_Loads <- union(LR_CY_Loads, SQR_CY_Loads)
Tidal_Trib_CY_Loads <- union(Tidal_Trib_CY_Loads, WNC_CY_Loads)

Tidal_Trib_CY_Loads <- Tidal_Trib_CY_Loads %>%
  select(Station_ID, CY, FW_PO4:FW_TSS)

#Calculate watershed area normalized load; start with areas from  PREP State of Our Estuary 2018
#Lamprey 211.91 sq miles
#Winnicut 14.18 sq miles
#Squamscott 106.90 sq miles

LMP_Area_km2 <- conv_unit(211.91, "mi2", "km2")
WNC_Area_km2 <- conv_unit(14.18, "mi2", "km2")
SQR_Area_km2 <- conv_unit(106.90, "mi2", "km2")


Tidal_Trib_CY_Loads$Watershed_Area_km2 <- ifelse(Tidal_Trib_CY_Loads$Station_ID == "Lamprey", LMP_Area_km2,
                                              ifelse(Tidal_Trib_CY_Loads$Station_ID == "Squamscott",SQR_Area_km2,
                                                     ifelse(Tidal_Trib_CY_Loads$Station_ID == "Winnicut", WNC_Area_km2, NA)))

Tidal_Trib_CY_Loads$Watershed_Area_ha <- conv_unit(Tidal_Trib_CY_Loads$Watershed_Area_km2, "km2", "hectare")


Tidal_Trib_Normalized_CY_Loads <- Tidal_Trib_CY_Loads %>%
  mutate(across(FW_PO4:FW_TSS, ~. / Watershed_Area_ha))


write.csv(Tidal_Trib_Normalized_CY_Loads, "results/main_load_calc/FW_Loads/Tidal_Trib_CY_Loads_kg_ha_yr24.csv")


#Average annual river concentration across rivers (TABLE FOR MANUSCRIPT NUMBERS)
#Combine river flow-weighted concentrations into one table
LR_FW_CY$River <- "Lamprey"
SQR_FW_CY$River <- "Squamscott"
WNC_FW_CY$River <- "Winnicut"
FY_combinedtable <- full_join(LR_FW_CY, SQR_FW_CY)
FY_combinedtable <- full_join(FY_combinedtable, WNC_FW_CY)

FY_combinedtable <- FY_combinedtable %>%
  mutate(across(FW_PO4:FW_TSS,  ~conv_unit(., "kg","mg")))

avg_river_fw_conc <- FY_combinedtable %>%
  summarize(across(FW_PO4:FW_TSS, ~mean(., na.rm=T)))

print(round(avg_river_fw_conc[,], 3))

avg_river_sd_conc <- FY_combinedtable %>%
  summarize(across(FW_PO4:FW_TSS, ~sd(., na.rm=T)))

print(round(avg_river_sd_conc[,], 3))

