#main_wwtf_format.R

#Author: Anna Lowien, University of New Hampshire
#Last Updated: 11/29/2021

#This script calculates loads from waste water treatment facilities (wwtf) that flow into Great Bay Estuary 
#Data compiled by the UNH Water Quality Analysis Lab, for PREP State of Our Estuaries

#flow units: mgd (millions gallons per day)

#Load packages
Packages <- c("readxl", "dplyr", "ggplot2", "measurements", "plotly", "lubridate", "cowplot",
              "ggpubr", "viridis", "stats", "tidyr", "moments", "tidyr")

lapply(Packages, library, character.only = TRUE)

#Read in .xslx file of Exeter and Newmarket WWTFs [TN], [TSS] and Flow data
wwtf <- read_excel("data/WWTF/WWTF_data.xlsx")

summary(wwtf)
unique(wwtf$WWTF)
colnames(wwtf)

#remove empty/unnecessary columns
wwtf <- wwtf %>%
  select(WWTF, MonPdEndDate, Year, Month, Month_Year, MonthlyAverageFlowmgd, DAILYMXTSSmgL, MOAVGTSSmgL,
         MOAVGTSSlbd, MOAVGNH3mgNL, DAILYMXTNmgNL:MOAVGTNmgNL, MOAVGTNlbd) %>%
  filter(MonPdEndDate > "2007-12-31") %>% #want 2008 and up
  filter(WWTF == "Exeter" | WWTF == "Newmarket") #Exeter is on Squamscott and Newmarket is on Lamprey
#___________________________________________________________________________________________
# Load Newfields WWTF Flow Data + TSS data
wwtf_newfields <- read_excel("data/WWTF/Newfields_WWTF_FlowData.xlsx")
Newfields_TN_mgL <- 21.53 #concentrations from 2018 State of Our Estuaries
Newfields_DIN_mgL <- 18.96

wwtf_newfields$WWTF <- "Newfields"
wwtf_newfields$Month <- month(wwtf_newfields$MonPdEndDate)
wwtf_newfields$Year <- year(wwtf_newfields$MonPdEndDate)
wwtf_newfields$Month_Year <- floor_date(wwtf_newfields$MonPdEndDate, unit="month")

wwtf_newfields <- wwtf_newfields %>%
  select(WWTF, MonPdEndDate, Month, Year, Month_Year, MonthlyAverageFlowmgd = FLOW_MGD_MOAVG, DAILYMXTSSmgL = TSS_mgL_DAILYMX, MOAVGTSSmgL = TSS_mgL_MOAVG,
         DAILYMXTSSlbd = TSS_lbd_DAILYMX, MOAVGTSSlbd = TSS_lbd_MOAVG) %>%
  filter(MonPdEndDate > "2007-12-31")

summary(wwtf_newfields)

#Calculate Newfields TN and DIN Loads using the reported concentrations and the monthly average flow (mgd)

wwtf_newfields$FLOW_GD_MOAVG <- wwtf_newfields$MonthlyAverageFlowmgd * 1000000
wwtf_newfields$FLOW_LD_MOAVG <- conv_unit(wwtf_newfields$FLOW_GD_MOAVG, "us_gal", "l")

#TN (MG/L) * Flow L/day
wwtf_newfields$MOAVGTNmgday <- wwtf_newfields$FLOW_LD_MOAVG * Newfields_TN_mgL
wwtf_newfields$MOAVGTNlbd <- conv_unit(wwtf_newfields$MOAVGTNmgday, "mg", "lbs")

#DIN (Mg/L) * Flow L/day
wwtf_newfields$MOAVGDINmgday <- wwtf_newfields$FLOW_LD_MOAVG * Newfields_DIN_mgL
wwtf_newfields$MOAVGDINlbd <- conv_unit(wwtf_newfields$MOAVGDINmgday, "mg", "lbs")

#Combine wwwtf_newfields with wwtf dataframe
colnames(wwtf)
colnames(wwtf_newfields)

wwtf_newfields <- wwtf_newfields %>%
  select(WWTF, MonPdEndDate, Year, Month, Month_Year, 
         MonthlyAverageFlowmgd, DAILYMXTSSmgL:MOAVGDINlbd, -DAILYMXTSSlbd)

#Make sub_wwtf and Newfields dataframes structure match (same column names and size)
wwtf$FLOW_GD_MOAVG <- wwtf$MonthlyAverageFlowmgd * 1000000
wwtf$FLOW_LD_MOAVG <- conv_unit(wwtf$FLOW_GD_MOAVG, "us_gal", "l")
wwtf$MOAVGTNmgday <- wwtf$FLOW_LD_MOAVG * wwtf$MOAVGTNmgNL
wwtf$MOAVGDINlbd <- NA
wwtf$MOAVGDINmgday <- NA 

wwtf_newfields$DAILYMXTNmgNL <- NA
wwtf_newfields$DAILYMXTNlbd <- NA
wwtf_newfields$MOAVGTNmgNL <- NA
wwtf_newfields$MOAVGNH3mgNL <- NA

#Combined dataframe has Exeter, Newmarket, and Newfields
wwtf_comb <- union(wwtf, wwtf_newfields) %>%
  select(WWTF:MonthlyAverageFlowmgd, FLOW_GD_MOAVG, FLOW_LD_MOAVG, DAILYMXTSSmgL:MOAVGTNlbd, MOAVGNH3mgNL, MOAVGTNmgday, MOAVGDINlbd)
#________________________________________________________________________________________________________________
#Compare Newfields flow to Exeter flow

p_flowmo <- ggplot(wwtf_comb, aes(Month_Year, MonthlyAverageFlowmgd)) + geom_point(aes(color=WWTF)) + 
  geom_line(aes(color=WWTF)) + theme_cowplot() + xlab ("Month-Year") + 
  ylab("Monthly Average Effluent Flow (millions of gallons per day)") +
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(breaks=seq(from=0, to=4, by=0.5))
p_flowmo

#Make a dataframe of water flow for each wwtf
#Calcuate ratio of flow for Newfields to Exeter
flow <- wwtf_comb %>%
  select(WWTF, MonPdEndDate,Year, FLOW_LD_MOAVG) %>%
  pivot_wider(names_from= WWTF, values_from = FLOW_LD_MOAVG) %>%
  mutate(Ratio = Newfields/Exeter * 100) # %

#Annual average flow ratio for Newfields to Exeter 
flow_annual_avg <- flow %>%
  group_by(Year) %>%
  summarize(mean_Ratio = mean(Ratio, na.rm =T), sdratio = sd(Ratio, na.rm=T)) #%

flow_ratio_avg <- flow %>%
  summarize(mean_Ratio = mean(Ratio, na.rm =T), sdratio = sd(Ratio, na.rm=T)) #%
#____________________________________________________________________________________

#Pull number of days in a month (for given year) [accounts for leap years]
wwtf_comb$day <- day(wwtf_comb$MonPdEndDate)

#Reorganize 
wwtf_comb <- wwtf_comb %>%
  select(WWTF, MonPdEndDate, Month, Year, Month_Year, day, MonthlyAverageFlowmgd:MOAVGDINlbd)

#Calculate Loads for the wwtf_comb
#Calculate monthly TN load (reported in lb/day) by multiplying by number of days in month
wwtf_comb$TNlbmonth <- wwtf_comb$MOAVGTNlbd * wwtf_comb$day

#Calculate monthly TSS load
wwtf_comb$TSSlbmonth <- wwtf_comb$MOAVGTSSlbd * wwtf_comb$day

#Calculate monthly NH3 Load
wwtf_comb$NH3lbmonth <- wwtf_comb$FLOW_LD_MOAVG * conv_unit(wwtf_comb$MOAVGNH3mgNL, "mg", "lbs") * wwtf_comb$day

#Calculate monthly DIN Load
wwtf_comb$DINlbmonth <- wwtf_comb$MOAVGDINlbd * wwtf_comb$day

#------------------------------------------------------------------------------------------------------------------------------------------
#Check above calculations by calculating monthly load using monthly average concentration and monthly average flow
summary(wwtf_comb)

#Convert to TN mg/day to lbs/day
wwtf_comb$MOAVGTNlbd_calc <- conv_unit(wwtf_comb$MOAVGTNmgday, "mg", "lbs")

#Compare Reported lb/day load to the calculated lbs/day load
summary(lm(MOAVGTNlbd_calc ~ MOAVGTNlbd, data=wwtf_comb))

p_comp <- ggplot(subset(wwtf_comb, Year > 2012), aes(MOAVGTNlbd, MOAVGTNlbd_calc)) + geom_point(aes(shape=as.factor(Year))) +
  geom_abline(slope=1,intercept=0) + 
  geom_smooth(method = "lm") +
  xlab("WWTF Reported Monthly Avg. TN Load (lb/day)") + ylab("Calculated Monthly Avg TN Load (lb/day)")
p_comp
ggplotly(p_comp)

#percent difference in reported and calculated load
wwtf_comb$percentdiff <- (wwtf_comb$MOAVGTNlbd_calc - wwtf_comb$MOAVGTNlbd)/wwtf_comb$MOAVGTNlbd * 100

percdiff <- ggplot(wwtf_comb, aes(as.Date(MonPdEndDate), percentdiff, color=WWTF)) + geom_point() +
  geom_hline(yintercept=-20) +
  geom_hline(yintercept =20) +
  scale_y_continuous(breaks=seq(from=-80,to=20, by=10)) 
percdiff

ggplotly(percdiff)

#Monthly Loads
wwtf_month <- wwtf_comb %>%
  select(WWTF:day, MonthlyAverageFlowmgd, FLOW_LD_MOAVG, TNlbmonth, TSSlbmonth, DINlbmonth)

write.csv(wwtf_month, "results/main_wwtf_loads/WWTF_monthly.csv")

#__________________________________________________________________________________________________________________________________________________
#ANNUAL LOADS

#Calculate annual load by summing the monthly average
wwtf_annual_load <- wwtf_comb %>%
  group_by(WWTF, Year) %>%
  summarize(TN_lb_year = sum(TNlbmonth),
            TSS_lb_year = sum(TSSlbmonth),
            NH3_lb_year = sum(NH3lbmonth),
            DIN_lb_year = sum(DINlbmonth))

wwtf_annual_load$TNkgyr <-conv_unit(wwtf_annual_load$TN_lb_year, "lbs", "kg")
wwtf_annual_load$TSSkgyr <-conv_unit(wwtf_annual_load$TSS_lb_year, "lbs", "kg")
wwtf_annual_load$NH3kgyr <- conv_unit(wwtf_annual_load$NH3_lb_year, "lbs", "kg")
wwtf_annual_load$DINkgyr <- conv_unit(wwtf_annual_load$DIN_lb_year, "lbs", "kg")

wwtf_annual_load$TN_tonsyr <- conv_unit(wwtf_annual_load$TNkgyr, "kg", "metric_ton")
wwtf_annual_load$TSS_tonsyr <- conv_unit(wwtf_annual_load$TSSkgyr, "kg", "metric_ton")
wwtf_annual_load$NH3_tonsyr <- conv_unit(wwtf_annual_load$NH3kgyr, "kg", "metric_ton")
wwtf_annual_load$DIN_tonsyr <- conv_unit(wwtf_annual_load$DINkgyr, "kg", "metric_ton")


#Experiment where we say DIN is 84% of the TN WWTF Loads based on SOE (except for Newfields, where we already have DIN)

wwtf_annual_load$DINkgyr <- ifelse(wwtf_annual_load$WWTF != "Newfields", wwtf_annual_load$TNkgyr * 84/100, wwtf_annual_load$DINkgyr)
wwtf_annual_load$DIN_tonsyr <- ifelse(wwtf_annual_load$WWTF != "Newfields", wwtf_annual_load$TN_tonsyr * 84/100, wwtf_annual_load$DIN_tonsyr)

#Remove the NH3 loads 

wwtf_annual_load <- wwtf_annual_load %>%
  select(-NH3_lb_year, - NH3kgyr, - NH3_tonsyr)



#Compare/add additional data
#Data Sources: https://scholars.unh.edu/cgi/viewcontent.cgi?article=1267&context=prep
#______________________________________________________________________________

#Source: https://www.exeternh.gov/sites/default/files/fileattachments/public_works/page/38381/2019-exeter-tn-annual_report-20200131-final.pdf
#Exeter Annual Report Data
wwtf_loadreport <- read.csv("data/wwtf/WWTF_AnnualReportData.csv")
#_______________________________________
#_________________________________________________
#______________________________________________________________

#Join calculated annual loads with reported annual loads

WWTF_join <- full_join(wwtf_annual_load, wwtf_loadreport, by = c("WWTF", "Year"))

#subset Exeter
WWTF_join_ex <- WWTF_join %>%
  filter(WWTF == "Exeter")

comp <- ggplot(WWTF_join_ex, aes(TN_lb_year.x, TN_lb_year.y)) + geom_point(aes(color=Year), size =3) +
  scale_color_viridis() +
  geom_abline(slope=1, intercept=0) +
  geom_smooth(method = "lm") + xlab("Reported TN Load (lb/year)") + ylab("Annual Town Reported TN Load (lb/year)")
comp

ggplotly(comp)

summary(lm(TN_lb_year.y ~ TN_lb_year.x, data=WWTF_join_ex))


wwtf_TN <- WWTF_join %>%
  select(Year, TN_calc = TN_lb_year.x, TN_reported = TN_lb_year.y)

wwtf_TNlong <- wwtf_TN %>%
  pivot_longer(cols = c(TN_calc:TN_reported),
               names_to = "Method",
               values_to = "TN_lbyear")

wwtf_tnplot <- ggplot(wwtf_TNlong, aes(Year, TN_lbyear, colour = Method)) + geom_point() + geom_line() +
  scale_x_continuous(breaks=seq(from=2008, to=2019, by=1)) + 
  scale_y_continuous(limits = c(80000,145000), breaks=seq(from=80000,to=145000, by =20000))
wwtf_tnplot

ggplotly(wwtf_tnplot)


#Percent difference between "reported lbs/day converted to lbs/month and then year" vs "annual reported value from TN reports"

wwtf_TN$TN_perdff <- (wwtf_TN$TN_reported - wwtf_TN$TN_calc)/wwtf_TN$TN_calc *100 

wwtf_tn_percdiff <- ggplot(wwtf_TN, aes(Year, TN_perdff)) + geom_point() +
  geom_hline(yintercept = 0) +
  scale_x_continuous(limits = c(2014, 2019), breaks=seq(from=2014, to=2019, by=1)) +
  scale_y_continuous(limits=c(-8,8), breaks=seq(from=-8,to=8,by=2)) + theme_cowplot() +
  ylab("% difference b/t reported & calculated annual TN load")
wwtf_tn_percdiff

#Use reported TN loads for Exeter, calculate DIN Exeter load as 84% of that TN load
#Use calcualted TN, TSS loads for Newfields and Newmarket
#use Calculated TSS loads for Exeter

wwtf_annual_loads_finalized <- WWTF_join %>%
  select(WWTF, Year, TNkgyr, TSSkgyr, DINkgyr, TN_lb_year.y, TN_tonsyr.y)

wwtf_annual_loads_finalized$TNkgyr.y <- conv_unit(wwtf_annual_loads_finalized$TN_lb_year.y, "lbs", "kg")

#Replace Exeter TN kg/yr with the TN kgyr.y column
wwtf_annual_loads_finalized$TNkgyr <- ifelse(wwtf_annual_loads_finalized$WWTF == "Exeter",
                                             wwtf_annual_loads_finalized$TNkgyr.y, 
                                             wwtf_annual_loads_finalized$TNkgyr)
#Remove those extra y columns
wwtf_annual_loads_finalized <- wwtf_annual_loads_finalized %>%
  select(-TNkgyr.y, -TN_lb_year.y) %>%
  filter(Year < 2020)

#Add 2008-2011 Newmarket loads from the lbs_year.y column to the kg_yr TN column
wwtf_annual_loads_finalized$TNkgyr <- ifelse(wwtf_annual_loads_finalized$WWTF == "Newmarket" & 
                                               wwtf_annual_loads_finalized$Year < 2014, conv_unit(wwtf_annual_loads_finalized$TN_tonsyr.y, "metric_ton", "kg"),
                                             wwtf_annual_loads_finalized$TNkgyr)
#Remove the last y column
wwtf_annual_loads_finalized <- wwtf_annual_loads_finalized %>%
  select(-TN_tonsyr.y)

#Recalculate DIN as 84/100

wwtf_annual_loads_finalized$DINkgyr <- ifelse(wwtf_annual_loads_finalized$WWTF != "Newfields", wwtf_annual_loads_finalized$TNkgyr * 84/100,
                                              wwtf_annual_loads_finalized$DINkgyr)
#_______________________________________________________________________________________________________
#Determine a DOC, DON, and Phosphate Estimate for effluent using literature values
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

mean(Lit_WWTF$CN, na.rm = T)

#Estimate DOC and PO4 now 
wwtf_annual_loads_finalized$DOCkgyr <- ifelse(wwtf_annual_loads_finalized$WWTF == "Exeter", wwtf_annual_loads_finalized$DINkgyr * 1.1272703,
                                              ifelse(wwtf_annual_loads_finalized$WWTF == "Newfields", wwtf_annual_loads_finalized$DINkgyr * 0.6292253,
                                                     ifelse(wwtf_annual_loads_finalized$WWTF == "Newmarket", wwtf_annual_loads_finalized$DINkgyr * 0.8381033, NA)))

wwtf_annual_loads_finalized$P04kgyr <- wwtf_annual_loads_finalized$DINkgyr / 7.848139

#Assume DIN=TDN
wwtf_annual_loads_finalized$TDNkgyr <- wwtf_annual_loads_finalized$DINkgyr
 

write.csv(wwtf_annual_loads_finalized, "results/main_wwtf_loads/wwtf_annual_loads.csv")
write.csv(wwtf_comb, "results/main_wwtf_loads/WWTF_conc.csv")
