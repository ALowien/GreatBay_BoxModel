#main_wwtf_format.R
#Author: Anna Mikulis, University of New Hampshire
#Last Updated: 12/9/2024

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
subdir <- "data/npdes_wwtf/updated"
files <- list.files(path = subdir, pattern = ".csv", full.names = T)  
df.list <- lapply(files, read.csv)

df <- bind_rows(df.list)
colnames(df)
df <- as.data.frame(df)

df$WWTF <- ifelse(df$NPDES.Permit.Number == "NH0100871", "Exeter", 
                  ifelse(df$NPDES.Permit.Number == "NH0100196", "Newmarket", 
                         ifelse(df$NPDES.Permit.Number == "NH0101192", "Newfields", NA)))

df <- df %>% 
  filter(Monitoring.Location.Code =="1") %>%  #outfalls only
  filter(!str_detect(Parameter.Code, "[a-zA-Z]"))

df$Monitoring.Location.Code <- as.numeric(df$Monitoring.Location.Code)
df$Parameter.Code <- as.numeric(df$Parameter.Code)

#DMR is a character, so we have to fix it by removing symbols
df$DMR.Value.Clean <-str_replace_all(df$DMR.Value, "<=|<|.<=","")
df$DMR.Value.Clean <- as.numeric(df$DMR.Value.Clean)

#data since 2020 General Permit 
subdir <- "data/npdes_wwtf/updated/generalpermit"
files <- list.files(path = subdir, pattern = ".csv", full.names = T)  
df.list <- lapply(files, read.csv)
df_recent <- bind_rows(df.list)
colnames(df_recent)
df_recent <- as.data.frame(df_recent)
 

ggplot(df_piv, aes((`Nitrogen, Kjeldahl, total (as N)_mg/L_MO AVG` + `NO3_NO2_mg/L_MO AVG`), `TN_mg/L_MO AVG`, color=WWTF)) + geom_point() +
  geom_abline(intercept=0, slope=1) #so yes there is PN in the TKN if unfiltered.... bc it matches TN...

ggplot(df_piv, aes(WWTF, `NO3_NO2_mg/L_MO AVG`)) + geom_point()
ggplot(df_piv, aes(WWTF, `Nitrogen, ammonia total (as N)_mg/L_MO AVG`)) + geom_point()

ggplot(df_piv, aes(Monitoring.Period.Date, `Nitrogen, ammonia total (as N)_mg/L_MO AVG`, color=WWTF)) + 
  geom_point() +
  geom_line() +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") 

ggplot(df_piv, aes(Monitoring.Period.Date, `NO3_NO2_mg/L_MO AVG`)) +   geom_point() + geom_line() +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  facet_wrap(~WWTF, nrow=3)

# Use pounds per day load to calculate the monthly concentration of TN
galtoliter <- 3.78541  #1 gallon = 3.78541 L
lbtomg <- 453592 #1lb = 453492 mg
 
df_piv$TN_mgL_MoAVG_calc <- df_piv$`TN_lb/d_MO AVG` / (df_piv$`Flow, in conduit or thru treatment plant_MGD_MO AVG` * 1000000) / galtoliter * lbtomg

ggplot(df_piv, aes(year, TN_mgL_MoAVG_calc, color=as.factor(month)))+
  geom_point(size=2) +
  scale_x_continuous(limits=c(2013, 2023.5), breaks=seq(from=2013,to=2023,by=1)) +
  ylab("TN (mg/L) ECHO") +
  scale_color_discrete(name="Month") +
  facet_wrap(~WWTF)

df_piv$TN_mgL_MoAVG_calc <- round(df_piv$TN_mgL_MoAVG_calc, 1)

compplot <- ggplot(df_piv, aes(`TN_mg/L_MO AVG`, TN_mgL_MoAVG_calc)) + 
  geom_point(aes(color=WWTF)) +
  geom_text(aes(label=Monitoring.Period.Date))
plotly::ggplotly(compplot)


df_piv <- df_piv %>%
  rename(Flow_MGD = `Flow, in conduit or thru treatment plant_MGD_MO AVG`)

df_piv$Final_TN_mgL <- ifelse(is.na(df_piv$`TN_mg/L_MO AVG`), df_piv$TN_mgL_MoAVG_calc, df_piv$`TN_mg/L_MO AVG`)

df_avg <-  df_piv %>%
  select(WWTF:`TSS_mg/L_MO AVG`, `Nitrogen, ammonia total (as N)_mg/L_MO AVG`, `NO3_NO2_mg/L_MO AVG`, `Final_TN_mgL`) %>%
  group_by(WWTF) %>%
  summarize(TSS_mgL = signif(mean(`TSS_mg/L_MO AVG`, na.rm=T), 2),
            TSS_mgL_sd = signif(sd(`TSS_mg/L_MO AVG`, na.rm=T), 2),
            NH4_mgL = mean(`Nitrogen, ammonia total (as N)_mg/L_MO AVG`, na.rm=T),
            NH4_mgL_sd = signif(sd(`Nitrogen, ammonia total (as N)_mg/L_MO AVG`, na.rm=T), 2),
            NO3_NO2_mgL = signif(mean(`NO3_NO2_mg/L_MO AVG`, na.rm=T),3),
            NO3_NO2_mgL_sd = signif(sd(`NO3_NO2_mg/L_MO AVG`, na.rm=T),3),
            TN_mgL = signif(mean(`Final_TN_mgL`, na.rm=T),2),
            TN_mgL_sd = sd(`Final_TN_mgL`, na.rm=T),
            TN_n = sum(!is.na(`Final_TN_mgL`)),
            TSS_n = sum(!is.na(`TSS_mg/L_MO AVG`)),
            NH4_n = sum(!is.na(`Nitrogen, ammonia total (as N)_mg/L_MO AVG`)),
            NO3_n = sum(!is.na(`NO3_NO2_mg/L_MO AVG`)))



df_piv$Final_TN_mgL <- ifelse(df_piv$WWTF == "Newfields" & df_piv$year >= 2017 &
                          is.na(df_piv$Final_TN_mgL), 20.0, df_piv$Final_TN_mgL)

df_piv$Final_TN_mgL <- ifelse(df_piv$WWTF == "Newfields" & df_piv$year< 2017 &
                                is.na(df_piv$Final_TN_mgL), 21.5, df_piv$Final_TN_mgL)

df_piv$Final_TN_mgL_method <- ifelse(is.na(df_piv$`TN_mg/L_MO AVG`) & !is.na(df_piv$TN_mgL_MoAVG_calc), "Calculated",
                                    ifelse(!is.na(df_piv$`TN_mg/L_MO AVG`), "Average", NA))


ggplot(df_piv, aes(Monitoring.Period.Date, Final_TN_mgL, color=Final_TN_mgL_method)) +
  geom_point() +
  facet_wrap(~WWTF, scales="free_x", nrow=3) +
  scale_x_date(breaks="1 year", date_labels="%Y")

ggplot(subset(df_piv, year > 2020), aes(Monitoring.Period.Date, Final_TN_mgL)) +
  geom_point() +
  geom_line() +
  facet_wrap(~WWTF, scales="free_x") 

ggplot(df_piv, aes(as.factor(year), Final_TN_mgL)) +
  geom_boxplot() +
  facet_wrap(~WWTF)

#save this intermediary step for use in main_site_conc_comparison.R script
write.csv(df_piv, "results/main_wwtf_loads/redone/wwtf_cleandataframe.csv")

#___________________
#Average annual concentration across all wwtf and years
df_avg_conc <- df_piv %>%
  select(WWTF:`TSS_mg/L_MO AVG`, `Nitrogen, ammonia total (as N)_mg/L_MO AVG`, `NO3_NO2_mg/L_MO AVG`, `Final_TN_mgL`) %>%
  group_by(WWTF, year) %>%
  summarize(TSS_mgL = mean(`TSS_mg/L_MO AVG`, na.rm=T),
            TSS_mgL_sd = sd(`TSS_mg/L_MO AVG`, na.rm=T),
            NH4_mgL = mean(`Nitrogen, ammonia total (as N)_mg/L_MO AVG`, na.rm=T),
            NH4_mgL_sd = sd(`Nitrogen, ammonia total (as N)_mg/L_MO AVG`, na.rm=T),
            NO3_NO2_mgL = mean(`NO3_NO2_mg/L_MO AVG`, na.rm=T),
            NO3_NO2_mgL_sd = sd(`NO3_NO2_mg/L_MO AVG`, na.rm=T),
            TN_mgL = mean(`Final_TN_mgL`, na.rm=T),
            TN_mgL_sd = sd(`Final_TN_mgL`, na.rm=T),
            TN_n = sum(!is.na(`Final_TN_mgL`)),
            TSS_n = sum(!is.na(`TSS_mg/L_MO AVG`)),
            NH4_n = sum(!is.na(`Nitrogen, ammonia total (as N)_mg/L_MO AVG`)),
            NO3_n = sum(!is.na(`NO3_NO2_mg/L_MO AVG`)))


df_avg_conc$TN_mgL <- ifelse(df_avg_conc$WWTF == "Newfields" & df_avg_conc$year >= 2017 &
                               is.na(df_avg_conc$TN_mgL), 20.0, df_avg_conc$TN_mgL)

df_avg_conc$TN_mgL <- ifelse(df_avg_conc$WWTF == "Newfields" & df_avg_conc$year < 2017 &
                               is.na(df_avg_conc$TN_mgL), 21.5, df_avg_conc$TN_mgL)

#__________________________________________________________________________________
###Exeter and Newmarket pre-2014 data
#Data source Exeter: Annual Town Reports Monthly Loads for 2012 and 2013
exeter_monthly <- read_excel("./data/wwtf/exeter_monthly_12_13.xlsx")

exeter_monthly <- exeter_monthly %>%
  select(WWTF, year, month, TN_mgL, MonthlyAverageFlowmgd, Data_Source) %>%
  rename(Final_TN_mgL.ex = TN_mgL,
         Flow_MGD.ex = MonthlyAverageFlowmgd)

exeter_monthly$Date <- as.Date(paste(exeter_monthly$year, exeter_monthly$month,  "01", sep="-"),
                               format = "%Y-%m-%d"
)

exeter_monthly$day <- days_in_month(exeter_monthly$Date)

exeter_monthly <- exeter_monthly %>% select(-Date)

#_____________________________________________________________
df_piv <- df_piv %>%
  rename(`TN_lb/d` = `TN_lb/d_MO AVG`,
         `TN_mg/L` = `TN_mg/L_MO AVG`,
         TSS_mgL = `TSS_mg/L_MO AVG`) 
       
#2014-2023  monthly loads (kg/month for Exeter and Newmarket)
monthlyloads <- df_piv %>%
  select(WWTF:day, `TN_lb/d`, `TN_mg/L`, Flow_MGD, TN_mgL_MoAVG_calc, Final_TN_mgL, Final_TN_mgL_method, TSS_mgL)  #inconsistent monthly data before 2014, so will build up with literature and reported values outside of ECHO database


monthlyloads <- full_join(monthlyloads, exeter_monthly)

monthlyloads$Final_TN_mgL <- ifelse(is.na(monthlyloads$Final_TN_mgL) & !is.na(monthlyloads$Final_TN_mgL.ex),
                                    monthlyloads$Final_TN_mgL.ex, monthlyloads$Final_TN_mgL)

monthlyloads <- monthlyloads %>%
  select(-Final_TN_mgL.ex, -Flow_MGD.ex)

ggplot(monthlyloads, aes(year, Final_TN_mgL, color="month")) + geom_point() +
  facet_wrap(~WWTF)

ggplot(monthlyloads, aes(year, TSS_mgL, color="month")) + geom_point() +
  facet_wrap(~WWTF)


df_summary <- monthlyloads %>%
  mutate(TN_kgL = conv_unit(Final_TN_mgL, "mg", "kg"),
         FLOW_galday = Flow_MGD * 1000000,
         FLOW_lday = conv_unit(FLOW_galday, "us_gal", "l"),
         TSS_kgL = conv_unit(`TSS_mgL`, "mg", "kg"))

df_summary$TN_kgmonth <- df_summary$TN_kgL * df_summary$FLOW_lday * df_summary$day
df_summary$TSS_kgmonth <- df_summary$TSS_kgL * df_summary$FLOW_lday * df_summary$day

df_annual <- df_summary %>%
  group_by(WWTF, year) %>%
  summarise(TN_kgyr = sum(TN_kgmonth),
            TSS_kgyr = sum(TSS_kgmonth))

#Pre-2014 Annual Loads Need Exeter 2008 - 2011; Newmarket 2008 - 2013
annual <- read.csv("./data/wwtf/WWTF_AnnualReportData.csv") 
annual$TN_kgyr.x <- conv_unit(annual$TN_tonsyr, "metric_ton", "kg")
annual$DIN_kgyr <- conv_unit(annual$DIN_tonyrs, "metric_ton", "kg")
annual <- annual %>% select(-TN_tonsyr, - DIN_tonyrs, -URL, -Data.Source, -NPDES.Permit.Number)

colnames(df_annual)
colnames(annual)

wwtf_annual <- full_join(df_annual,annual)

wwtf_annual$TN_kgyr <- ifelse(is.na(wwtf_annual$TN_kgyr), wwtf_annual$TN_kgyr.x, wwtf_annual$TN_kgyr)

wwtf_annual$Notes <- ifelse(wwtf_annual$year > 2013, "calculated with monthly ECHO data", wwtf_annual$Notes)

#Estimate DIN as 78.5% for TN for years < 2012 per 2012 PREP SOOE Tech Report and as 84.1% for years > 2012 per the 2018 SOOE, 74.9% for 2017-2020...
wwtf_annual$DIN_kgyr <- ifelse(is.na(wwtf_annual$DIN_kgyr) & wwtf_annual$year < 2012, wwtf_annual$TN_kgyr*0.785,
                               ifelse(is.na(wwtf_annual$DIN_kgyr) & wwtf_annual$year >= 2012 & wwtf_annual$year < 2017, wwtf_annual$TN_kgyr*0.841, 
                                      ifelse(is.na(wwtf_annual$DIN_kgyr) & wwtf_annual$year >= 2017, wwtf_annual$TN_kgyr*0.749, wwtf_annual$DIN_kgyr)))

wwtf_annual <- wwtf_annual %>% select(-TN_kgyr.x)

ggplot(wwtf_annual, aes(year, TN_kgyr, color=WWTF)) + geom_point() +
  facet_wrap(~WWTF, scales = "free_y")


ggplot(wwtf_annual, aes(year, DIN_kgyr, color=WWTF)) + geom_point() +
  facet_wrap(~WWTF, scales = "free_y")
#Discrete samples of effluent for DOC, PO4, etc. 
Lit_WWTF <- read_excel("data/wwtf/Literature_WWTF_Values.xlsx")

Lit_WWTF$DINcheck <- Lit_WWTF$DIN_MGL > Lit_WWTF$TDN_MGL

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
write.csv(wwtf_annual, "results/main_wwtf_loads/redone/wwtf_annual_loads24.csv")


#Monthly Loads
monthly_load_summary <- df_summary %>%
  select(WWTF, year, month, day, Final_TN_mgL, TN_kgL, TSS_kgL, TN_kgmonth, TSS_kgmonth, FLOW_lday)

monthly_load_summary$DIN_kgmonth <- ifelse(monthly_load_summary$year < 2012, monthly_load_summary$TN_kgmonth*0.785,
                               ifelse(monthly_load_summary$year >= 2012, monthly_load_summary$TN_kgmonth*0.841, NA))

monthly_load_summary$PO4_kgmonth <- monthly_load_summary$DIN_kgmonth / 7.841301
monthly_load_summary$DOC_kgmonth <- ifelse(monthly_load_summary$WWTF == "Exeter", monthly_load_summary$DIN_kgmonth * 0.8752803,
                                   ifelse(monthly_load_summary$WWTF == "Newfields", monthly_load_summary$DIN_kgmonth * 0.6292253,
       ifelse(monthly_load_summary$WWTF == "Newmarket", monthly_load_summary$DIN_kgmonth * 1.1556989, NA)))


monthly_load_summary <- monthly_load_summary %>% 
  select(WWTF, year, month, TN_kgmonth, DIN_kgmonth, TSS_kgmonth, DOC_kgmonth, PO4_kgmonth)

write.csv(monthly_load_summary, "results/main_wwtf_loads/redone/monthlyloads24.csv")


exeterwwtf <- ggplot(subset(wwtf_annual, WWTF == "Exeter"), aes(year, TN_kgyr)) +
  geom_point(size=4) + geom_line(linewidth=1) +
  geom_vline(xintercept = 2019.5, linetype="dashed") +
  scale_x_continuous(limits=c(2009,2023), breaks=seq(from=2009, to=2023, by =1)) +
  scale_y_continuous(limits=c(0,65000), breaks=seq(from=0, to=65000, by =10000)) +
  theme_bw() +
  ylab("Total Nitrogen Load (kg-N/yr)") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle=45, hjust=1),
        axis.text = element_text(size=16),
        axis.title = element_text(size=20))

exeterwwtf
ggsave(exeterwwtf, filename=paste0("./results/exeter_wwtf_annual.jpg"), dpi=300, width=8, height=5, units = "in")