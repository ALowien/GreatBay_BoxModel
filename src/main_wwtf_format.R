#main_wwtf_format.R
#Author: Anna Mikulis, University of New Hampshire
#Last Updated: 2/25/2025

#This script calculates monthly and annual loads for waste water treatment facilities that discharge to Great Bay Estuary below the head-of-tide monitoring stations on the rivers. 
#Data has been sourced from the EPA's ECHO (Enforcement and Compliance History) Monitoring Database, annual town reports, published reports, and effluent samples analyzed by the Water Quality Analysis Lab at the University of Hampshire. 

#ECHO Website: https://echo.epa.gov/trends/loading-tool/get-data/monitoring-data-download

#flow units: mgd (millions gallons per day)

#Load packages
Packages <- c("readxl", "dplyr", "ggplot2", "measurements", "plotly", "lubridate", "cowplot",
              "ggpubr", "viridis", "stats", "tidyr", "moments", "tidyr", "readr", "stringr")

lapply(Packages, library, character.only = TRUE)

#Read in csv files downloaded from echo.epa.gov (2008-2023 monthly average TN loads for Exeter and Newmarket WWTF, monthly flow for Newfields)
#Files have been amended to put columns names as the first row.
subdir <- "data/npdes_wwtf"
files <- list.files(path = subdir, pattern = ".csv", full.names = T)  
df.list <- lapply(files, read.csv)

df <- bind_rows(df.list)
df <- as.data.frame(df)

#Identify treatment plants based on permit numbers
df$WWTF <- ifelse(df$NPDES.Permit.Number == "NH0100871", "Exeter", 
                  ifelse(df$NPDES.Permit.Number == "NH0100196", "Newmarket", 
                         ifelse(df$NPDES.Permit.Number == "NH0101192", "Newfields", NA)))

unique(df$Parameter.Code)
#parameter codes of interest for box model (nitrogen, suspended solids, flow)
codes <- c("00600","00610", "50050", "00530")

df <- df %>% 
  filter(Monitoring.Location.Code =="1") %>%  #outfalls only
  filter(!str_detect(Parameter.Code, "[a-zA-Z]")) %>%  #removing non-numeric codes
  filter(Parameter.Code %in% codes)

#DMR is a character, so we have to fix it by removing symbols
df$DMR.Value <-str_replace_all(df$DMR.Value, "<=|<|.<=","")
df$DMR.Value <- as.numeric(df$DMR.Value) #set as numeric, NAs are from rows where no data codes are in the column (ex. NODI:9)

df$Monitoring.Period.Date <- as.Date(df$Monitoring.Period.Date, format="%m/%d/%Y")
df$Parameter.Code  <- as.integer(df$Parameter.Code) #to match df_recent object class
df$Monitoring.Location.Code <- as.integer(df$Monitoring.Location.Code) #to match df_recent object class
#data since 2020 General Permit  
subdir <- "data/npdes_wwtf/generalpermit"
files <- list.files(path = subdir, pattern = ".csv", full.names = T)  
df.list <- lapply(files, read.csv)
df_recent <- bind_rows(df.list)
colnames(df_recent)
df_recent <- as.data.frame(df_recent)

df_recent$WWTF <- ifelse(df_recent$NPDES.Permit.Number == "NHG58A004", "Exeter", 
                  ifelse(df_recent$NPDES.Permit.Number == "NHG58A008", "Newmarket", 
                         ifelse(df_recent$NPDES.Permit.Number == "NHG58A012", "Newfields", 
                                ifelse(df_recent$NPDES.Permit.Number == "NHG580013", "Newmarket",
                                       ifelse(df_recent$NPDES.Permit.Number == "NHG580011", "Newfields", NA)))))

unique(df_recent$Parameter.Code)
unique(df_recent$Parameter.Description)

df_recent$Monitoring.Period.Date <- as.Date(df_recent$Monitoring.Period.Date, format="%m/%d/%Y")


#parameter codes of interest, integer class results in leading zeros being dropped
codes <- c("600","610", "625", "630", "50050", "530")
df_recent <- df_recent %>% 
  filter(Monitoring.Location.Code =="1") %>%  #outfalls only
  filter(Parameter.Code %in% codes)

wwtf <- full_join(df, df_recent)

wwtf <- wwtf %>%
  select(WWTF, NPDES.Permit.Number, Monitoring.Period.Date, Monitoring.Location.Code, Parameter.Code, Parameter.Description, DMR.Value.Type, Statistical.Base, DMR.Value, DMR.Value.Unit) %>%
  filter(!is.na(DMR.Value)) #remove the 12 NAs

#monthly averages only
wwtf <- wwtf %>%
  filter(Statistical.Base == "MO AVG")

wwtf$DMR.Value.Unit <- gsub("/", "_", wwtf$DMR.Value.Unit)

wwtf$Parameter.Description <- gsub(",", "", wwtf$Parameter.Description)

#join description and units into same column

wwtf$Parameter <- paste(wwtf$Parameter.Description, wwtf$DMR.Value.Unit, sep = "_")

unique(wwtf$Parameter)

#clean up parameter names 
wwtf$Parameter <- ifelse(wwtf$Parameter ==  "Nitrogen total (as N)_lb_d", "TN_lbday",
                         ifelse(wwtf$Parameter == "Nitrogen ammonia total (as N)_mg_L", "NH4_mgL",
                         ifelse(wwtf$Parameter == "Nitrite plus nitrate total 1 det. (as N)_mg_L", "NO3_NO2_mgL",
                         ifelse(wwtf$Parameter ==  "Nitrogen total (as N)_mg_L", "TN_mgL",
                                ifelse(wwtf$Parameter == "Flow in conduit or thru treatment plant_MGD", "Flow_MGD",
                                       ifelse(wwtf$Parameter == "Solids total suspended_mg_L", "TSS_mgL",
                                              ifelse(wwtf$Parameter == "Solids total suspended_lb_d", "TSS_lbday",
                                                     wwtf$Parameter)))))))
                                      

wwtf_piv <- wwtf %>%
  select(WWTF, Monitoring.Period.Date, Parameter, DMR.Value) %>%
  filter(Monitoring.Period.Date < "2024-01-01")%>%
  pivot_wider(names_from = Parameter,
              values_from = DMR.Value)


#Gaps where sometimes have TN concentration (mg/L) and sometimes have TN load (lb per day), use flow to get concentration from lb/day

# Use pounds per day load to calculate the monthly concentration of TN
galtoliter <- 3.785412  #1 gallon = 3.78541 L
conv_unit(1, "us_gal", "l")
conv_unit(1, "lbs", "mg")
lbtomg <- 454592.4

wwtf_piv$TN_mgL_calc <- wwtf_piv$TN_lbday / (wwtf_piv$Flow_MGD * 1000000) / galtoliter * lbtomg

wwtf_piv$month <- month(wwtf_piv$Monitoring.Period.Date)
wwtf_piv$year <- year(wwtf_piv$Monitoring.Period.Date)
wwtf_piv$Monitoring.Period.Date <- as.Date(wwtf_piv$Monitoring.Period.Date)

ggplot(wwtf_piv, aes(year, TN_mgL_calc, color=as.factor(month)))+
  geom_point(size=2) +
  scale_x_continuous(limits=c(2013, 2023.5), breaks=seq(from=2013,to=2023,by=1)) +
  ylab("TN (mg/L) ECHO") +
  scale_color_discrete(name="Month") +
  facet_wrap(~WWTF)

wwtf_piv$TN_mgL_calc <- round(wwtf_piv$TN_mgL_calc, 1)

compplot <- ggplot(wwtf_piv, aes(`TN_mgL`, TN_mgL_calc)) + 
  geom_abline(slope=1, intercept=0) +
  geom_point(aes(color=WWTF, text=Monitoring.Period.Date)) 
plotly::ggplotly(compplot) #rounding differences most likely during the calculations

#replace the three Exeter mismatched values that are all 9 mg/L with the monthly concentrations published in the 2013/2014 Town Reports, which match the calculated values for TN  concentration (using the lb/day load)
wwtf_piv$TN_mgL <- ifelse(wwtf_piv$WWTF == "Exeter" & wwtf_piv$Monitoring.Period.Date == as.Date("2013-11-30"),
                               25.4, wwtf_piv$TN_mgL)
wwtf_piv$TN_mgL <- ifelse(wwtf_piv$WWTF == "Exeter" & wwtf_piv$Monitoring.Period.Date == as.Date("2013-12-31"),
                               31.6, wwtf_piv$TN_mgL)
wwtf_piv$TN_mgL<- ifelse(wwtf_piv$WWTF == "Exeter" & wwtf_piv$Monitoring.Period.Date == as.Date("2014-01-31"),
                               23.5, wwtf_piv$TN_mgL)

wwtf_piv$Final_TN_mgL <- ifelse(is.na(wwtf_piv$TN_mgL), wwtf_piv$TN_mgL_calc, wwtf_piv$TN_mgL)

#assign pre, during, and post upgrade status to years
wwtf_piv$period <- ifelse(wwtf_piv$year < 2017 & wwtf_piv$WWTF != "Newfields", "pre-upgrade",
                          ifelse(wwtf_piv$WWTF == "Exeter" & wwtf_piv$year > 2020, "post-upgrade", 
                                 ifelse(wwtf_piv$WWTF == "Newmarket" & wwtf_piv$year > 2017, "post-upgrade", NA)))

wwtf_piv$period <- ifelse(wwtf_piv$WWTF == "Newmarket" & is.na(wwtf_piv$period), "upgrade period", wwtf_piv$period)

wwtf_piv$period <- ifelse(wwtf_piv$WWTF == "Exeter" & is.na(wwtf_piv$period) & wwtf_piv$year < 2019, "pre-upgrade", wwtf_piv$period)

wwtf_piv$period <- ifelse(wwtf_piv$WWTF == "Exeter" & is.na(wwtf_piv$period), "upgrade period", wwtf_piv$period)

wwtf_piv$period <- ifelse(wwtf_piv$WWTF == "Newfields", "no upgrades", wwtf_piv$period)


ggplot(wwtf_piv, aes(year, Final_TN_mgL, color=period)) + geom_point() +
  scale_x_continuous(limits=c(2007.5, 2023.5), breaks=seq(from=2008,to=2023,by=3)) +
  facet_wrap(~WWTF)

flow <- wwtf_piv %>%
  select(WWTF, Flow_MGD) %>%
  group_by(WWTF) %>%
  summarize(mean = mean(Flow_MGD, na.rm = T),
            n= sum(!is.na(Flow_MGD)),
            sd = sd(Flow_MGD, na.rm=T))

flow[1,]
flow[2,]
0.0847 / 1.75 * 100
0.0189/ 0.485 * 100

wwtf_avg_overall <-  wwtf_piv %>%
  select(WWTF,period, TSS_mgL, NH4_mgL, NO3_NO2_mgL, Final_TN_mgL) %>%
  group_by(WWTF) %>%
  summarize(TSS_mgL_mean = signif(mean(TSS_mgL, na.rm=T), 2),
            TSS_mgL_sd = signif(sd(TSS_mgL, na.rm=T), 2),
            NH4_mgL_mean = signif(mean(NH4_mgL, na.rm=T),2),
            NH4_mgL_sd = signif(sd(NH4_mgL, na.rm=T), 2),
            NO3_NO2_mgL_mean = signif(mean(NO3_NO2_mgL, na.rm=T),3),
            NO3_NO2_mgL_sd = signif(sd(NO3_NO2_mgL, na.rm=T),3),
            TN_mgL_mean = signif(mean(`Final_TN_mgL`, na.rm=T),2),
            TN_mgL_sd = signif(sd(Final_TN_mgL, na.rm=T),2),
            TN_n = sum(!is.na(`Final_TN_mgL`)),
            TSS_n = sum(!is.na(TSS_mgL)),
            NH4_n = sum(!is.na(NH4_mgL)),
            NO3_n = sum(!is.na(NO3_NO2_mgL)))


wwtf_avg_byperiod <-  wwtf_piv %>%
  select(WWTF,period, TSS_mgL, NH4_mgL, NO3_NO2_mgL, Final_TN_mgL) %>%
  group_by(WWTF, period) %>%
  summarize(TSS_mgL_mean = signif(mean(TSS_mgL, na.rm=T), 2),
            TSS_mgL_sd = signif(sd(TSS_mgL, na.rm=T), 2),
            NH4_mgL_mean = signif(mean(NH4_mgL, na.rm=T),2),
            NH4_mgL_sd = signif(sd(NH4_mgL, na.rm=T), 2),
            NO3_NO2_mgL_mean = signif(mean(NO3_NO2_mgL, na.rm=T),3),
            NO3_NO2_mgL_sd = signif(sd(NO3_NO2_mgL, na.rm=T),3),
            TN_mgL_mean = signif(mean(`Final_TN_mgL`, na.rm=T),2),
            TN_mgL_sd = signif(sd(Final_TN_mgL, na.rm=T),2),
            TN_n = sum(!is.na(`Final_TN_mgL`)),
            TSS_n = sum(!is.na(TSS_mgL)),
            NH4_n = sum(!is.na(NH4_mgL)),
            NO3_n = sum(!is.na(NO3_NO2_mgL)))

wwtf_avg_combined <- wwtf_avg_byperiod %>%
  mutate(
    TSS = paste0(TSS_mgL_mean, " ± ", TSS_mgL_sd, " (", TSS_n, ")"),
    NH4 = paste0(NH4_mgL_mean, " ± ", NH4_mgL_sd, " (", NH4_n, ")"),
    NO3_NO2 = paste0(NO3_NO2_mgL_mean, " ± ", NO3_NO2_mgL_sd, " (", NO3_n, ")"),
    TN = paste0(TN_mgL_mean, " ± ", TN_mgL_sd, " (", TN_n, ")")) %>%
  select(WWTF, period, TSS, NH4, NO3_NO2, TN) 

write.csv(wwtf_avg_combined, file=paste0("./results/manuscript_figures/supplemental/wwtf_pre_postupgrades.csv"))

#Add in pre-monitoring average concentration for Newfields using State of our Estuary published values
wwtf_piv$Final_TN_mgL <- ifelse(wwtf_piv$WWTF == "Newfields" & wwtf_piv$year >= 2017 &
                          is.na(wwtf_piv$Final_TN_mgL), 20.0, wwtf_piv$Final_TN_mgL)

wwtf_piv$Final_TN_mgL <- ifelse(wwtf_piv$WWTF == "Newfields" & wwtf_piv$year< 2017 &
                                is.na(wwtf_piv$Final_TN_mgL), 21.5, wwtf_piv$Final_TN_mgL)

wwtf_piv$Final_TN_mgL_method <- ifelse(is.na(wwtf_piv$TN_mgL) & !is.na(wwtf_piv$TN_mgL_calc), "Calculated",
                                    ifelse(!is.na(wwtf_piv$TN_mgL), "Average", NA))

#save this intermediary step for use in main_site_conc_comparison.R script
write.csv(wwtf_piv, "results/main_wwtf_loads/wwtf_concentrations.csv")
#write.csv(wwtf_piv, "data/wwtf/wwtf_concentrations.csv") #version uploaded to EDI

#__________________________________________________________________________________
#inconsistent monthly data before 2014, so will build up with literature and reported values outside of ECHO database
###Exeter and Newmarket pre-2014 data
#Data source Exeter: Annual Town Reports Monthly Loads for 2012 and 2013
exeter_monthly <- read.csv("./data/wwtf/exeter_2012_2013.csv")

exeter_monthly <- exeter_monthly %>%
  select(WWTF, year, month, TN_mgL, MonthlyAverageFlowmgd, Data_Source) %>%
  rename(Final_TN_mgL.ex = TN_mgL,
         Flow_MGD.ex = MonthlyAverageFlowmgd)

exeter_monthly$Date <- as.Date(paste(exeter_monthly$year, exeter_monthly$month,  "01", sep="-"), 
                               format = "%Y-%m-%d")
                          
exeter_monthly$day <- days_in_month(exeter_monthly$Date)

exeter_monthly <- exeter_monthly %>% select(-Date)

#add in the monthly exeter values to the main dataframe
monthlyloads <- full_join(wwtf_piv, exeter_monthly)

monthlyloads$Final_TN_mgL <- ifelse(is.na(monthlyloads$Final_TN_mgL) & !is.na(monthlyloads$Final_TN_mgL.ex),
                                    monthlyloads$Final_TN_mgL.ex, monthlyloads$Final_TN_mgL)

monthlyloads <- monthlyloads %>%
  select(-Final_TN_mgL.ex, -Flow_MGD.ex)


#Discrete samples of effluent for DOC, PO4, etc. 
Lit_WWTF <- read.csv("data/wwtf/literature_wwtf_concentrations.csv")
Lit_WWTF$Date <- as.Date(Lit_WWTF$Date, format = "%m/%d/%Y")

Lit_WWTF$year <- year(Lit_WWTF$Date)
Lit_WWTF$month <- month(Lit_WWTF$Date)
#pull out TN 2008 data and join to main dataframe
tn_2008 <- Lit_WWTF %>%
  select(WWTF, year, month, TN_MGL) %>%
  filter(!is.na(TN_MGL))

monthlyloads <- full_join(monthlyloads, tn_2008)

monthlyloads$Final_TN_mgL <- ifelse(is.na(monthlyloads$Final_TN_mgL) & !is.na(monthlyloads$TN_MGL), monthlyloads$TN_MGL, monthlyloads$Final_TN_mgL)

df_summary <- monthlyloads %>%
  select(WWTF, Monitoring.Period.Date, year, month, period, Final_TN_mgL, TSS_mgL, Flow_MGD) %>%
  filter(year < 2024) %>%
  mutate(TN_kgL = conv_unit(Final_TN_mgL, "mg", "kg"),
         FLOW_galday = Flow_MGD * 1000000,
         FLOW_lday = conv_unit(FLOW_galday, "us_gal", "l"),
         TSS_kgL = conv_unit(`TSS_mgL`, "mg", "kg"))

df_summary$day <- days_in_month(df_summary$Monitoring.Period.Date)

df_summary$TN_kgmonth <- df_summary$TN_kgL * df_summary$FLOW_lday * df_summary$day
df_summary$TSS_kgmonth <- df_summary$TSS_kgL * df_summary$FLOW_lday * df_summary$day

ggplot(subset(df_summary, year == 2008 & WWTF != "Newfields"), 
       aes(month, TN_kgmonth)) + geom_point() +
  facet_wrap(~WWTF, nrow = 3) +
  scale_x_continuous(limits=c(1,12), breaks=seq(from=1,to=12,by=1))



df_annual_2008 <- df_summary %>%
  filter(year < 2009) %>%
  group_by(WWTF) %>%
  summarize(mean_TN = mean(TN_kgmonth, na.rm=T))

#patch in missing 2008 months as average of months we have 

df_summary$TN_kgmonth <- ifelse(df_summary$year == 2008 & df_summary$WWTF == "Exeter" & is.na(df_summary$TN_kgmonth),  3174, df_summary$TN_kgmonth)


df_summary$TN_kgmonth <- ifelse(df_summary$year == 2008 & df_summary$WWTF == "Newmarket" & is.na(df_summary$TN_kgmonth),  2215, df_summary$TN_kgmonth)



df_annual <- df_summary %>%
  group_by(WWTF, year) %>%
  summarise(TN_kgyr = sum(TN_kgmonth),
            TSS_kgyr = sum(TSS_kgmonth))

#Pre-2014 Annual Loads Need Exeter 2008 - 2011; Newmarket 2008 - 2013
annual <- read.csv("./data/wwtf/wwtf_annualloads.csv") 
annual$TN_kgyr.x <- annual$TN_tonsyr *907.185 #us tons to kg
annual$DIN_kgyr <- annual$DIN_tonyrs * 907.185
annual <- annual %>% select(-TN_tonsyr, - DIN_tonyrs)

colnames(df_annual)
colnames(annual)

wwtf_annual <- full_join(df_annual,annual)

wwtf_annual$TN_kgyr <- ifelse(is.na(wwtf_annual$TN_kgyr), wwtf_annual$TN_kgyr.x, wwtf_annual$TN_kgyr)

wwtf_annual$Notes <- ifelse(wwtf_annual$year > 2013, "calculated with monthly ECHO data", "Literature Value")

#Estimate DIN as 78.5% for TN for years < 2012 per 2012 PREP SOOE Tech Report and as 84.1% for years > 2012 per the 2018 SOOE, 74.9% for 2017-2020...
wwtf_annual$DIN_kgyr <- ifelse(is.na(wwtf_annual$DIN_kgyr) & wwtf_annual$year < 2012, wwtf_annual$TN_kgyr*0.785,
                               ifelse(is.na(wwtf_annual$DIN_kgyr) & wwtf_annual$year >= 2012 & wwtf_annual$year < 2017, wwtf_annual$TN_kgyr*0.841, 
                                      ifelse(is.na(wwtf_annual$DIN_kgyr) & wwtf_annual$year >= 2017, wwtf_annual$TN_kgyr*0.749, wwtf_annual$DIN_kgyr)))

wwtf_annual <- wwtf_annual %>% select(-TN_kgyr.x)

ggplot(wwtf_annual, aes(year, TN_kgyr, color=WWTF)) + geom_point() +
  facet_wrap(~WWTF, scales = "free_y")

ggplot(wwtf_annual, aes(year, DIN_kgyr, color=WWTF)) + geom_point() +
  facet_wrap(~WWTF, scales = "free_y")


#ESTIMATE DOC and PO4 Loads based on molar ratios
#Calculate molar concentrations
Lit_WWTF$DOC_gL <- conv_unit(Lit_WWTF$DOC_MGL, "mg", "g")
Lit_WWTF$DOC_molL <- Lit_WWTF$DOC_gL / 12.0107

Lit_WWTF$TDN_gL <- conv_unit(Lit_WWTF$TDN_MGL, "mg", "g")
Lit_WWTF$TDN_molL <- Lit_WWTF$TDN_gL / 14.01

mean(Lit_WWTF$DOC_MGL, na.rm=T) 
sd(Lit_WWTF$DOC_MGL, na.rm=T)
summary(Lit_WWTF)

mean(Lit_WWTF$PO4_UGL/1000,na.rm=T)
sd(Lit_WWTF$PO4_UGL/1000,na.rm=T)


#Ratio of TDN to TN
Lit_WWTF$TN_molL <- conv_unit(Lit_WWTF$TN_MGL, "mg", "g") / 14.01

Lit_WWTF$CN_mol_est <- Lit_WWTF$DOC_molL / Lit_WWTF$TN_molL

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
  select(WWTF, CN_mol, CN_mol_est, NP_mol) %>% 
  group_by(WWTF) %>%
  summarize(across(CN_mol:NP_mol, \(x) mean(x, na.rm =T)))

WWTF_conc_ratios <- Lit_WWTF %>%
  select(WWTF, CN, NP) %>% 
  group_by(WWTF) %>%
  summarize(across(CN:NP,\(x) mean(x, na.rm =T)))

meanCN <- mean(Lit_WWTF$CN, na.rm = T)

#Estimate DOC and PO4 now using above concentration ratios
wwtf_annual$DOCkgyr <- ifelse(wwtf_annual$WWTF == "Exeter", wwtf_annual$DIN_kgyr * 0.8752803,
                              ifelse(wwtf_annual$WWTF == "Newfields", wwtf_annual$DIN_kgyr * 0.6292253,
                                     ifelse(wwtf_annual$WWTF == "Newmarket", wwtf_annual$DIN_kgyr * 1.1556989, NA)))

wwtf_annual$PO4kgyr <- wwtf_annual$DIN_kgyr / 7.841301

#Assume DIN=TDN
wwtf_annual$TDN_kgyr <- wwtf_annual$DIN_kgyr

ggplot(wwtf_annual, aes(year, TN_kgyr, color=WWTF)) +
  geom_point(size=2) +
  scale_x_continuous(limits=c(2008,2023), breaks=seq(from=2008, to=2023, by =1)) +
  scale_y_continuous(limits=c(0,65000), breaks=seq(from=0, to=65000, by =10000)) +
  theme_bw()
  

#Save things
write.csv(wwtf_annual, "results/main_wwtf_loads/redone/wwtf_annual_loads.csv")


#Monthly Loads
monthly_load_summary <- df_summary %>%
  select(WWTF, year, month, day, Final_TN_mgL, TN_kgL, TSS_kgL, TN_kgmonth, TSS_kgmonth, FLOW_lday)

monthly_missing <- monthly_load_summary %>%
  filter(is.na(TN_kgmonth))

monthly_gapfill <- annual %>%
  mutate(
    TN_kgmonth = TN_kgyr.x / 12,
    DIN_kgmonth = DIN_kgyr / 12
  ) %>%
  expand(WWTF, year, month = 1:12) %>%
  left_join(annual, by = c("WWTF", "year")) %>%
  mutate(
    TN_kgmonth = signif(TN_kgyr.x / 12, 4),
    DIN_kgmonth = signif(DIN_kgyr / 12,4)) %>%
  select(WWTF, year, month, TN_kgmonth, DIN_kgmonth)

monthly_load_summary$DIN_kgmonth <- NA

# Fill NAs in monthly_load_summary with values from monthly_gapfill
monthly_load_summary<- monthly_load_summary %>%
  left_join(monthly_gapfill, by = c("WWTF", "year", "month")) %>%
  mutate(
    TN_kgmonth = ifelse(is.na(TN_kgmonth.x), TN_kgmonth.y, TN_kgmonth.x),
    DIN_kgmonth = ifelse(is.na(DIN_kgmonth.x), DIN_kgmonth.y, DIN_kgmonth.x)
  ) %>%
  select(-TN_kgmonth.x, -TN_kgmonth.y, -DIN_kgmonth.x, -DIN_kgmonth.y)



monthly_load_summary$DIN_kgmonth <- ifelse(monthly_load_summary$year < 2012, monthly_load_summary$TN_kgmonth*0.785,
                               ifelse(monthly_load_summary$year >= 2012, monthly_load_summary$TN_kgmonth*0.841, NA))

monthly_load_summary$PO4_kgmonth <- monthly_load_summary$DIN_kgmonth / 7.841301
monthly_load_summary$DOC_kgmonth <- ifelse(monthly_load_summary$WWTF == "Exeter", monthly_load_summary$DIN_kgmonth * 0.8752803,
                                   ifelse(monthly_load_summary$WWTF == "Newfields", monthly_load_summary$DIN_kgmonth * 0.6292253,
       ifelse(monthly_load_summary$WWTF == "Newmarket", monthly_load_summary$DIN_kgmonth * 1.1556989, NA)))


monthly_load_summary <- monthly_load_summary %>% 
  select(WWTF, year, month, TN_kgmonth, DIN_kgmonth, TSS_kgmonth, DOC_kgmonth, PO4_kgmonth)

write.csv(monthly_load_summary, "results/main_wwtf_loads/redone/monthlyloads.csv")

