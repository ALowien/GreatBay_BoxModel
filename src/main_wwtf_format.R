#main_wwtf_format.R
#Author: Anna Mikulis, University of New Hampshire
#Last Updated: 2/23/2023

#This script calculates monthly and annual loads for waste water treatment facilities that discharge to Great Bay Estuary below the head-of-tide monitoring stations on the rivers. 
#Data has been sourced from the EPA's ECHO (Enforcement and Compliance History) Monitoring Database, annual town reports, published reports, and direct wwtf effluent samples analyzed by the Water Quality Analysis Lab at the University of Hampshire. 
#ECHO Website: https://echo.epa.gov/trends/loading-tool/get-data/monitoring-data-download

#flow units: mgd (millions gallons per day)

#Load packages
Packages <- c("readxl", "dplyr", "ggplot2", "measurements", "plotly", "lubridate", "cowplot",
              "ggpubr", "viridis", "stats", "tidyr", "moments", "tidyr", "readr", "stringr")

lapply(Packages, library, character.only = TRUE)

#Read in csv files downloaded from echo.epa.gov (2014-2018 monthly average TN loads for Exeter and Newmarket WWTF, monthly flow for Newfields )
#Files have been amended to put columns names as the first row.
subdir <- "data/npdes_wwtf"
files <- list.files(path = subdir, pattern = ".csv", full.names = T)  
df.list <- lapply(files, read.csv)

df <- bind_rows(df.list)
colnames(df)
df <- as.data.frame(df)

unique(df$Parameter.Description)

df$WWTF <- ifelse(df$NPDES.Permit.Number == "NH0100871", "Exeter", 
                  ifelse(df$NPDES.Permit.Number == "NH0100196", "Newmarket", "Newfields"))

#Pull out the Newfields WWTF in order to calculate TN and DIN loads using the flow data from ECHO
newfields <- df %>%
  filter(WWTF == "Newfields")

df <- df %>%
  filter(Parameter.Description == "Nitrogen, total (as N)" | Parameter.Description == "Solids, total suspended" |
           Parameter.Description == "Flow, in conduit or thru treatment plant") %>%
  filter(Limit.Type == "MO AVG") %>%
  filter(Monitoring.Location.Code != "G") #G code means influent, we want effluent, which is code 1

df <- df %>%
  select(NPDES.Permit.Number, Parameter.Description:WWTF) %>%
  filter(WWTF != "Newfields")


df$DMR.Value.Clean <-str_replace_all(df$DMR.Value, "<=|<|.<=","") #let's us keep values that are reported at the detection limit
df$DMR.Value.Clean <- as.numeric(df$DMR.Value.Clean)

df <- df %>%
  filter(!is.na(DMR.Value)) # NAs (i.e. no data value provided) were removed 

df$Monitoring.Period.Date <- as.Date(df$Monitoring.Period.Date, format="%m/%d/%Y")
df$day <- day(df$Monitoring.Period.Date)
df$year <- year(df$Monitoring.Period.Date)
df$month <- month(df$Monitoring.Period.Date)

df_count <- df %>%
  group_by(NPDES.Permit.Number, year, Limit.Type, Parameter.Description, DMR.Value.Unit) %>%
  tally()

ggplot(subset(df_count, Parameter.Description != "Flow, in conduit or thru treatment plant"), aes(year, n, color=Parameter.Description)) + geom_point(aes(shape=DMR.Value.Unit)) + facet_wrap(~NPDES.Permit.Number)

#rename parameters to be more succinct 
df$Parameter.Description <- ifelse(df$Parameter.Description == "Flow, in conduit or thru treatment plant", "Flow",
                                   ifelse(df$Parameter.Description == "Nitrogen, total (as N)", "TN",
                                          ifelse(df$Parameter.Description == "Solids, total suspended", "TSS", NA)))
#Create one column that describes the parameter measured and the units
df$Parameter.Units <- paste(df$Parameter.Description, df$DMR.Value.Unit, sep="_")

df_piv <- df %>%
  select(NPDES.Permit.Number, WWTF, year, month, day, Parameter.Units, DMR.Value.Clean) %>%
  pivot_wider(id_cols=c(NPDES.Permit.Number, WWTF, year, month, day),
              names_from = Parameter.Units,
              values_from = DMR.Value.Clean)

df_piv$TN_lb_month <- df_piv$`TN_lb/d` * df_piv$day
df_piv$TSS_lb_month <- df_piv$`TSS_lb/d` * df_piv$day

df_summary <- df_piv %>%
  select(NPDES.Permit.Number, WWTF, year, month, Flow_MGD:TSS_lb_month) %>%
  group_by(NPDES.Permit.Number,WWTF, year) %>%
  summarize(TN_lbyr = sum(TN_lb_month),
            TSS_lbyr = sum(TSS_lb_month))

df_summary$TSS_kgyr <- conv_unit(df_summary$TSS_lbyr, "lbs", "kg")
df_summary$TN_kgyr <- conv_unit(df_summary$TN_lbyr, "lbs", "kg")

df_summary <- df_summary %>% select(-TN_lbyr, -TSS_lbyr)

#2014-2018  monthly loads (kg/month for Exeter and Newmarket)
monthlyloads <- df_piv %>%
  select(NPDES.Permit.Number:day, `TN_lb/d`, `TN_mg/L`, Flow_MGD, TN_lb_month) %>%
  filter(year > 2013) #inconsistent monthly data before 2014, so will build up with literature and reported values outside of ECHO database

#convert DMR Value.month from lbs/month to kg/month
monthlyloads$TN_kgmonth <- round(conv_unit(monthlyloads$TN_lb_month,"lbs", "kg"),0)

###Exeter and Newmarket pre-2014 data
#Data source Exeter: Annual Town Reports Monthly Loads for 2012 and 2013
exeter_monthly <- read_excel("./data/wwtf/exeter_monthly_12_13.xlsx")

exeter_monthly <- exeter_monthly %>%
  select(NPDES.Permit.Number, WWTF, year, month, TN_lbmonth)

exeter_monthly$TN_kgmonth <- round(conv_unit(exeter_monthly$TN_lbmonth,"lbs", "kg"),0)
exeter_monthly <- exeter_monthly %>% select(-TN_lbmonth)

exeter_annual <- exeter_monthly %>%
  group_by(NPDES.Permit.Number, WWTF, year) %>%
  summarize(TN_kgyr =sum(TN_kgmonth))

exeter_annual$Notes <- "sum of monthly loads from town reports"

monthlyloads <- bind_rows(monthlyloads, exeter_monthly)

#Pre-2014 Annual Loads Need Exeter 2008 - 2011; Newmarket 2008 - 2013
annual <- read.csv("./data/wwtf/WWTF_AnnualReportData.csv") 
annual$TN_kgyr <- conv_unit(annual$TN_tonsyr, "metric_ton", "kg")
annual$DIN_kgyr <- conv_unit(annual$DIN_tonyrs, "metric_ton", "kg")
annual <- annual %>% select(-TN_tonsyr, - DIN_tonyrs, -URL, -Data.Source)

annual <- bind_rows(annual, exeter_annual)
colnames(annual)[colnames(annual) == "TN_kgyr"] <- "TN_kgyr.z"


#put together annual_summary and annual
wwtf_annual <- full_join(df_summary, annual)

wwtf_annual$TN_kgyr <- ifelse(is.na(wwtf_annual$TN_kgyr),wwtf_annual$TN_kgyr.z, wwtf_annual$TN_kgyr)

wwtf_annual <- wwtf_annual %>%
  select(NPDES.Permit.Number:TN_kgyr, DIN_kgyr, Notes)

wwtf_annual$Notes <- ifelse(wwtf_annual$year > 2013, "calculated with monthly ECHO data", wwtf_annual$Notes)

#Newfields WWTF
#Concentrations for TN and DIN sourced from 2018 State of Our Estuaries
Newfields_TN_mgL <- 21.53 
Newfields_DIN_mgL <- 18.96

newfields$Monitoring.Period.Date <- as.Date(newfields$Monitoring.Period.Date, format="%m/%d/%Y")
newfields$DMR.Value <- as.numeric(newfields$DMR.Value)

newfields <- newfields %>%
  select(NPDES.Permit.Number, Parameter.Description:DMR.Value.Unit) %>%
  filter(Parameter.Description == "Flow, in conduit or thru treatment plant" | Parameter.Description == "Solids, total suspended") %>%
  filter(Limit.Type == "MO AVG") %>%
  filter(DMR.Value.Unit == "MGD" | DMR.Value.Unit == "lb/d")

newfields$day <- day(newfields$Monitoring.Period.Date)
newfields$year <- year(newfields$Monitoring.Period.Date)
newfields$month <- month(newfields$Monitoring.Period.Date)

newfields_count <- newfields %>%
  group_by(NPDES.Permit.Number, year, Limit.Type, Parameter.Description) %>%
  tally() #12 monthly flow values for each year between 2008 and 2018

newfields$DMR.Value.month <- ifelse(newfields$Parameter.Description == "Flow, in conduit or thru treatment plant", newfields$DMR.Value *newfields$day * 1000000, NA) #gallons per month

newfields$TN_kgmonth <- conv_unit(Newfields_TN_mgL, "mg", "kg") * (conv_unit(newfields$DMR.Value.month, "us_gal", "l"))

newfields$DIN_kgmonth <- conv_unit(Newfields_DIN_mgL, "mg", "kg") * (conv_unit(newfields$DMR.Value.month, "us_gal", "l"))

newfields$TSS_kgmonth <- ifelse(newfields$Parameter.Description == "Solids, total suspended", (conv_unit(newfields$DMR.Value, "lbs", "kg") * newfields$day), NA)

newfields_monthly <- newfields %>%
  select(NPDES.Permit.Number,year, month, TN_kgmonth, DIN_kgmonth) %>%
  filter(!is.na(TN_kgmonth) & !is.na(DIN_kgmonth))

newfields_tss <- newfields %>%
  select(NPDES.Permit.Number, year, month, TSS_kgmonth) %>%
  filter(!is.na(TSS_kgmonth))

newfields_monthly <- full_join(newfields_monthly, newfields_tss)
newfields_monthly$WWTF <- "Newfields"
newfields_annual <- newfields_monthly %>%
  group_by(NPDES.Permit.Number, year) %>%
  summarize(TN_kgyr = sum(TN_kgmonth),
            DIN_kgyr = sum(DIN_kgmonth),
            TSS_kgyr = sum(TSS_kgmonth))

newfields_annual[,3:5] <- round(newfields_annual[,3:5], digits=0)

newfields_annual$WWTF <- "Newfields"

wwtf_annual <- bind_rows(wwtf_annual, newfields_annual)

#Put together monthly loads
monthlyloads <- monthlyloads %>%
  select(NPDES.Permit.Number:month, TN_kgmonth)

monthlyloads <- bind_rows(monthlyloads, newfields_monthly)

#Estimate monthly loads for newmarket (2008-2013) and for exeter (2008-2011) as 1/12 of the annual load
years <- c(2008:2011)
months <- 1:12
monthly_estimates <- expand.grid(year = years, month = months)
monthly_estimates$WWTF <- "Exeter"

monthly_estimates_exeter <- left_join(monthly_estimates, wwtf_annual)
monthly_estimates_exeter$TN_kgmonth <- monthly_estimates_exeter$TN_kgyr/12
monthly_estimates_exeter$DIN_kgmonth <- monthly_estimates_exeter$DIN_kgyr/12

monthly_estimates_exeter <- monthly_estimates_exeter %>%
  select(year:NPDES.Permit.Number, TN_kgmonth, DIN_kgmonth)

years <- c(2008:2013)
monthly_estimates_Newmarket <- expand.grid(year = years, month = months)
monthly_estimates_Newmarket$WWTF <- "Newmarket"
monthly_estimates_Newmarket <- left_join(monthly_estimates_Newmarket, wwtf_annual)
monthly_estimates_Newmarket$TN_kgmonth <- monthly_estimates_Newmarket$TN_kgyr/12
monthly_estimates_Newmarket$DIN_kgmonth <- monthly_estimates_Newmarket$DIN_kgyr/12

monthly_estimates_Newmarket <- monthly_estimates_Newmarket %>%
  select(year:NPDES.Permit.Number, TN_kgmonth, DIN_kgmonth)

#combine
monthly_estimates <- full_join(monthly_estimates_exeter, monthly_estimates_Newmarket)

monthlyloads<- bind_rows(monthlyloads, monthly_estimates)


#Estimate DIN as 78.5% for TN for years < 2012 per 2012 PREP SOOE Tech Report and as 84.1% for years > 2012 per the 2018 SOOE
wwtf_annual$DIN_kgyr <- ifelse(is.na(wwtf_annual$DIN_kgyr) & wwtf_annual$year < 2012, wwtf_annual$TN_kgyr*0.785,
                               ifelse(is.na(wwtf_annual$DIN_kgyr) * wwtf_annual$year >= 2012, wwtf_annual$TN_kgyr*0.841, wwtf_annual$DIN_kgyr))

#Repeat for monthly
monthlyloads$DIN_kgmonth <- ifelse(is.na(monthlyloads$DIN_kgmonth) & monthlyloads$year < 2012, monthlyloads$TN_kgmonth*0.785,
                                   ifelse(is.na(monthlyloads$DIN_kgmonth) & monthlyloads$year >= 2012, monthlyloads$TN_kgmonth*0.841, monthlyloads$DIN_kgmonth))

#Discrete samples of effluent for DOC, PO4, etc. 
Lit_WWTF <- read_excel("data/wwtf/Literature_WWTF_Values.xlsx")

#Calculate molar concentrations
Lit_WWTF$DOC_gL <- conv_unit(Lit_WWTF$DOC_MGL, "mg", "g")
Lit_WWTF$DOC_molL <- Lit_WWTF$DOC_gL / 12.0107

Lit_WWTF$TDN_gL <- conv_unit(Lit_WWTF$TDN_MGL, "mg", "g")
Lit_WWTF$TDN_molL <- Lit_WWTF$TDN_gL / 14.01

#molar ratio
Lit_WWTF$CN_mol <- Lit_WWTF$DOC_molL / Lit_WWTF$TDN_molL

#Repeat with phosphate to get N:P ratio
Lit_WWTF$PO4_gL <- conv_unit(Lit_WWTF$PO4_UGL, "ug", "g")
Lit_WWTF$PO4_molL <- Lit_WWTF$PO4_gL / 30.97

#molar ratio
Lit_WWTF$NP_mol <- Lit_WWTF$TDN_molL / Lit_WWTF$PO4_molL

#concentration ratios
Lit_WWTF$CN <- Lit_WWTF$DOC_MGL / Lit_WWTF$TDN_MGL
Lit_WWTF$NP <-Lit_WWTF$TDN_MGL / (conv_unit(Lit_WWTF$PO4_gL, "g", "mg"))
Lit_WWTF$NP_g <- Lit_WWTF$TDN_gL / Lit_WWTF$PO4_gL
#summarize by wastewater treatment plant
WWTF_molar_ratios <- Lit_WWTF %>%
  select(WWTF, CN_mol, NP_mol) %>% 
  group_by(WWTF) %>%
  summarize(across(CN_mol:NP_mol, mean, na.rm =T))

WWTF_conc_ratios <- Lit_WWTF %>%
  select(WWTF, CN, NP) %>% 
  group_by(WWTF) %>%
  summarize(across(CN:NP, mean, na.rm =T))

meanCN <- mean(Lit_WWTF$CN, na.rm = T)

#Estimate DOC and PO4 now using above concentration ratios
wwtf_annual$DOCkgyr <- ifelse(wwtf_annual$WWTF == "Exeter", wwtf_annual$DIN_kgyr * 1.1272703,
                              ifelse(wwtf_annual$WWTF == "Newfields", wwtf_annual$DIN_kgyr * 0.6292253,
                                     ifelse(wwtf_annual$WWTF == "Newmarket", wwtf_annual$DIN_kgyr * 0.8381033, NA)))

wwtf_annual$PO4kgyr <- wwtf_annual$DIN_kgyr / 7.848139

monthlyloads$PO4_kgmonth <- monthlyloads$DIN_kgmonth / 7.848139
monthlyloads$DOC_kgmonth <- ifelse(monthlyloads$WWTF == "Exeter", monthlyloads$DIN_kgmonth * 1.1272703,
                                   ifelse(monthlyloads$WWTF == "Newfields", monthlyloads$DIN_kgmonth * 0.6292253,
                                          ifelse(monthlyloads$WWTF == "Newmarket", monthlyloads$DIN_kgmonth * 0.8381033, NA)))

#Assume DIN=TDN
wwtf_annual$TDN_kgyr <- wwtf_annual$DIN_kgyr

#Save things
write.csv(wwtf_annual, "results/main_wwtf_loads/redone/wwtf_annual_loads.csv")
write.csv(monthlyloads, "results/main_wwtf_loads/redone/monthlyloads.csv")