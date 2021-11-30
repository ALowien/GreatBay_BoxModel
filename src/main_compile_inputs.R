#main_compile_inputs.R

#Author: Anna Lowien, University of New Hampshire
#Last Updated: 11/30/2021

#Purpose: Compile calendar year fluxes for budget calculations

Packages <- c("readr", "dplyr", "tidyr", "cowplot", "viridis", "ggplot2",
              "plotly", "measurements")

lapply(Packages, library, character.only = TRUE)


#River Inputs
#NHDES Head-of-Tide Stations
#Annual Load Estimates
#Values in kg X solute/year

LMP_Annual_Loads <- read.csv("results/main_load_calc/FW_Loads/LR_Annual_Loads.csv")

SQR_Annual_Loads <- read.csv("results/main_load_calc/FW_Loads/SQR_Annual_Loads.csv")

WNC_Annual_Loads <- read.csv("results/main_load_calc/FW_Loads/WNC_Annual_Loads.csv")

#Thin load estimates based on QAQC (exclude years with <9 WQ samples)

#Lamprey River
LMP_Annual_Loads <- LMP_Annual_Loads %>%
  select(-X, -Flow_l_year, TP = FW_TP, PO4 = FW_PO4, PN=FW_PN, TN=FW_TN, TDN=FW_TDN, NH4 = FW_NH4, NO3_NO2 = FW_NO3_NO2, 
         DIN=FW_DIN, DON=FW_DON, DOC=FW_DOC, TSS=FW_TSS)

LMP_Annual_Loads$DIN <- ifelse(LMP_Annual_Loads$CY == 2008, NA, LMP_Annual_Loads$DIN)

LMP_Annual_Loads$DON <- ifelse(LMP_Annual_Loads$CY == 2008, NA, LMP_Annual_Loads$DON)

LMP_Annual_Loads$NH4 <- ifelse(LMP_Annual_Loads$CY == 2008, NA, LMP_Annual_Loads$NH4)

LMP_Annual_Loads$NO3_NO2 <- ifelse(LMP_Annual_Loads$CY == 2008, NA, LMP_Annual_Loads$NO3_NO2)

#Squamscott River
SQR_Annual_Loads <- SQR_Annual_Loads %>%
  select(-X, -Flow_l_year, TP = FW_TP, PO4 = FW_PO4, PN=FW_PN, TN=FW_TN, TDN=FW_TDN, NH4 = FW_NH4, NO3_NO2 = FW_NO3_NO2, 
         DIN=FW_DIN, DON=FW_DON, DOC=FW_DOC, TSS=FW_TSS)

SQR_Annual_Loads$DIN <- ifelse(SQR_Annual_Loads$CY == 2008, NA, SQR_Annual_Loads$DIN)

SQR_Annual_Loads$DON <- ifelse(SQR_Annual_Loads$CY == 2008, NA, SQR_Annual_Loads$DON)

SQR_Annual_Loads$NH4 <- ifelse(SQR_Annual_Loads$CY == 2008, NA, SQR_Annual_Loads$NH4)

SQR_Annual_Loads$NO3_NO2 <- ifelse(SQR_Annual_Loads$CY == 2008, NA, SQR_Annual_Loads$NO3_NO2)

SQR_Annual_Loads$PN <- ifelse(SQR_Annual_Loads$CY == 2011, NA, SQR_Annual_Loads$PN)

#Winnicut River
WNC_Annual_Loads <- WNC_Annual_Loads %>%
  select(-X, -Flow_l_year, TP = FW_TP, PO4 = FW_PO4, PN=FW_PN, TN=FW_TN, TDN=FW_TDN, NH4 = FW_NH4, NO3_NO2 = FW_NO3_NO2, 
         DIN=FW_DIN, DON=FW_DON, DOC=FW_DOC, TSS=FW_TSS)

WNC_Annual_Loads$DIN <- ifelse(WNC_Annual_Loads$CY == 2008, NA, WNC_Annual_Loads$DIN)

WNC_Annual_Loads$DON <- ifelse(WNC_Annual_Loads$CY == 2008, NA, WNC_Annual_Loads$DON)

WNC_Annual_Loads$NH4 <- ifelse(WNC_Annual_Loads$CY == 2008, NA, WNC_Annual_Loads$NH4)

WNC_Annual_Loads$NO3_NO2 <- ifelse(WNC_Annual_Loads$CY == 2008, NA, WNC_Annual_Loads$NO3_NO2)

#Combine into one tidal tributary dataframe of annual loads

LMP_Annual_Loads$Site <- "Lamprey"
SQR_Annual_Loads$Site <- "Squamscott"
WNC_Annual_Loads$Site <- "Winnicut"

Tidal_Tribs <- union(LMP_Annual_Loads, SQR_Annual_Loads)
Tidal_Tribs <- union(Tidal_Tribs, WNC_Annual_Loads)

Tidal_Tribs_long <- Tidal_Tribs %>%
  select(Year = CY, Site, TP:TSS) %>%
  pivot_longer(Tidal_Tribs, cols= c(TP:TSS), names_to = "Solute", values_to = "Load_kgyr")

#Precipitation 
Precip <- read.csv("results/main_precipitation_format/cy_precip_loads.csv")

Precip$Site <- "Precipitation"

summary(Precip)

#Precip is measured for NO3, but we call it NO32 for purposes of budget
Precip <- Precip %>%
  select(-X, Site, Year= CY, DOC=DOC_kg_yr, TDN=TDN_kg_yr, TN=TN_kg_yr, NO3_NO2 = NO3_kg_yr, NH4 = NH4_kg_yr, PO4 = PO4_kg_yr, DIN=DIN_kg_yr, DON=DON_kg_yr)

Precip_long <- Precip %>%
  select(Site, Year, TDN:TN) %>%
  pivot_longer(cols=c(TDN:TN),
               names_to = "Solute", 
               values_to = "Load_kgyr")

#Waste water Treatment Plants

WWTF <- read.csv("results/main_wwtf_loads/wwtf_annual_loads.csv")

WWTF <- WWTF %>%
  select(Site = WWTF, Year, TN = TNkgyr, DIN = DINkgyr, TDN=TDNkgyr, TSS=TSSkgyr, DOC=DOCkgyr, PO4=P04kgyr)

WWTF$Site <- ifelse(WWTF$Site == "Exeter", "Exeter WWTF",
                    ifelse(WWTF$Site == "Newmarket", "Newmarket WWTF",
                           ifelse(WWTF$Site == "Newfields", "Newfields WWTF", NA)))

WWTF_long <- WWTF %>%
  pivot_longer(cols=c(TN:PO4),
               names_to = "Solute",
               values_to = "Load_kgyr")

AP_Flux <- read.csv("results/main_estuarine_load_calc/AP_Flux_kgyr.csv")


AP_Flux <- AP_Flux %>%
  select(Year, Site = STATION_ID, PO4:NH4, NO3_NO2 = NO32, DIN:TSS, - PC, - SIO2) %>%
  filter(Year < 2019)

AP_Flux$DIN <- ifelse(AP_Flux$Year == 2008, NA, AP_Flux$DIN)
AP_Flux$DOC <- ifelse(AP_Flux$Year == 2009, NA, AP_Flux$DOC)
AP_Flux$NH4 <- ifelse(AP_Flux$Year == 2008, NA, AP_Flux$NH4)
AP_Flux$DON <- ifelse(AP_Flux$Year == 2008, NA, AP_Flux$DON)
AP_Flux$TSS <- ifelse(AP_Flux$Year == 2017, NA, AP_Flux$TSS)


AP_Flux_long <- AP_Flux %>%
  pivot_longer(cols=c(PO4:TSS),
               names_to = "Solute", 
               values_to = "Load_kgyr")
#Adams Point TN Loads

AP_TNLoad <- ggplot(AP_Flux, aes(Year, TN)) + geom_point(aes(color=Site))
AP_TNLoad
#Coastal runoff

Runoff <- read.csv("results/main_runoff/runoff_estimate_kgyr.csv") %>%
  select(Year = CY, PO4 = FW_PO4, PN=FW_PN, TN=FW_TN, TDN=FW_TDN, NH4 = FW_NH4, NO3_NO2 = FW_NO3_NO2, 
         DIN=FW_DIN, DON=FW_DON, DOC=FW_DOC, TSS=FW_TSS)

Runoff$Site <- "Runoff"

Runoff_long <- Runoff %>%
  pivot_longer(cols=c(PO4:TSS),
               names_to = "Solute",
               values_to = "Load_kgyr")

#Groundwater loads are in kg/year
Groundwater <- data.frame(Site = "Groundwater",
                          Year = c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018),
                          DIN = 6800)

#Assume DIN = TDN = TN for groundwater

Groundwater$TN <- Groundwater$DIN
Groundwater$TDN <- Groundwater$DIN

Groundwater_long <- Groundwater %>%
  pivot_longer(cols = c(DIN:TDN),
               names_to ="Solute",
               values_to = "Load_kgyr")

#combine all the long dataframes
colnames(AP_Flux_long)
colnames(Tidal_Tribs_long)
colnames(Runoff_long)
colnames(Groundwater_long)
colnames(Precip_long)
colnames(WWTF_long)

Budget <- full_join(AP_Flux_long, Tidal_Tribs_long)
Budget <- full_join(Budget, Precip_long)
Budget <- full_join(Budget, WWTF_long)
Budget <- full_join(Budget, Runoff_long)
Budget <- full_join(Budget, Groundwater_long)

Budget_wide <- Budget %>%
  pivot_wider(names_from = "Solute",
              values_from = "Load_kgyr") %>%
  select(-TP) #only measured at Tidal Tribs

#kg/yr
Budget_wide$Component <- ifelse(Budget_wide$Site == "GRBAPL", "Output" ,"Input")

#Site averages
Budget_wide_siteavg <- Budget_wide %>%
  group_by(Site) %>%
  summarize(across(PO4:TSS, mean,na.rm=T))

write.csv(Budget_wide_siteavg, "results/main_compile_inputs/Avg.Load.Site.kgyr.csv")

#Site averages
Budget_wide_sitesd <- Budget_wide %>%
  group_by(Site) %>%
  summarize(across(PO4:TSS, sd,na.rm=T))

write.csv(Budget_wide_sitesd, "results/main_compile_inputs/Stdev.Load.Site.kgyr.csv")

#calculate standard error
library(plotrix)
budget.s.error <- Budget_wide %>%
  select(Site, PO4:TSS) %>%
  group_by(Site) %>%
  summarize(across(PO4:TSS, std.error, na.rm=F))


#calculate standard error as % of mean
budget.percent.error <- cbind(budget.s.error[1], round(budget.s.error[-1]/Budget_wide_siteavg[-1],2))
budget.percent.error2 <- budget.percent.error %>%
  mutate(across(PO4:TSS, ~.*100)) %>%
  filter(Site != "Groundwater" & Site != "Runoff")


#Black Box Calculations for Each Solute
summary(Budget)
summary(Budget_wide)

Budget <- Budget %>%
  filter(Solute != "SIO2" & Solute != "NO3" & Solute != "PC" & Solute != "TP") #not measured at all sites

M1 <- ggplot(Budget, aes(Site, Load_kgyr, color=Site)) + geom_boxplot() +
  facet_wrap(~Solute, scales="free") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size=9, angle=90),
        axis.text.y = element_text(size=9),
        legend.position = "none")
M1

ggplotly(M1)

#Combine M1 Budget (Head-of-tide river stations) into input categories:
# 1) Tributary Load
# 2) WWTF Load
# 3) Precipitation
# 4) Groundwater
# 5) High and Low Tide
# 6) Runoff
summary(Budget)

#Make a new column to enable grouping
Budget$Type <- ifelse(Budget$Site == "Lamprey", "Riverine", ifelse(
  Budget$Site == "Squamscott", "Riverine", ifelse(
    Budget$Site == "Winnicut", "Riverine", ifelse(
      Budget$Site == "Exeter WWTF", "WWTF", ifelse(
        Budget$Site == "Newmarket WWTF", "WWTF", ifelse(
          Budget$Site == "GRBAPH", "APH", ifelse(
            Budget$Site == "GRBAPL", "APL", ifelse(
              Budget$Site == "Runoff", "Runoff", ifelse(
                Budget$Site == "Newfields WWTF", "WWTF", ifelse(
                  Budget$Site == "Precipitation", "Precip", ifelse(
                    Budget$Site == "Groundwater", "Groundwater", NA) ))))))))))
 

unique(Budget$Site)
unique(Budget$Type)

#Grouped by Type and Year

Budget_Components <- Budget %>%
  group_by(Type, Year, Solute) %>%
  summarize(Load_kgyr = sum(Load_kgyr, na.rm=T)) %>%
  filter(Load_kgyr > 0) # remove any rows that are zero

P_Components <- ggplot(Budget_Components, aes(Year, Load_kgyr, color=Type)) + geom_point(size=2) +
  scale_color_viridis_d() +
  scale_x_continuous(limits=c(2008,2018),breaks=seq(from=2008, to=2018, by=2)) +
  facet_wrap(~Solute, scales="free") + theme_cowplot()
P_Components


#Write an Input/Output Column
Budget_Components$Balance <- ifelse(Budget_Components$Type == "APL", "Output", "Input")

Budget_Components_Summary <- Budget_Components %>%
  group_by(Balance, Solute, Year) %>%
  summarize(Loadkgyr = sum(Load_kgyr, na.rm=T))

#Remove DOC 2011 because no DOC measurements at GRBAPH or GRBAPL
Budget_Components_Summary$Loadkgyr <- ifelse(Budget_Components_Summary$Solute == "DOC" & Budget_Components_Summary$Year == "2011", NA, Budget_Components_Summary$Loadkgyr)


Plot_Summary <- ggplot(Budget_Components_Summary, aes(Year, Loadkgyr, colour = Balance)) + geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_continuous(limits=c(2008,2018),breaks=seq(from=2008, to=2018, by=2)) +
  facet_wrap(~Solute, scales="free")+ theme_cowplot()

Plot_Summary


write.csv(Budget_Components, "results/main_compile_inputs/Budget_Components.csv")


#Table of each input as a % of total input

Budget_Components_Percent <- Budget_Components %>%
  pivot_wider(names_from= "Solute",
              values_from="Load_kgyr")

Budget_Components_Summary_wide <- Budget_Components_Summary %>%
  pivot_wider(names_from = "Solute",
              values_from = "Loadkgyr") %>%
  filter(Year < 2019) %>%
  select(Balance, Year, DIN_annual = DIN, DOC_annual = DOC, DON_annual=DON, NH4_annual = NH4, NO3_NO2_annual = NO3_NO2, PN_annual = PN,
         PO4_annual = PO4, TDN_annual = TDN, TN_annual = TN, TSS_annual = TSS)


#Join Annual Inputs to the site inputs in order to calculate percentage

Budget_Components_Percent2 <- full_join(Budget_Components_Percent, Budget_Components_Summary_wide, by = c("Year", "Balance"))

#Summarize as % of the total input for each type of input and year

Inputs_Percent <- Budget_Components_Percent2 %>%
  filter(Balance == "Input") %>%
  mutate(DIN_per = DIN/DIN_annual * 100,
         TN_per = TN/TN_annual * 100,
         DOC_per = DOC/DOC_annual * 100, 
         PN_per = PN/PN_annual * 100,
         PO4_per = PO4/PO4_annual * 100,
         TSS_per = TSS/TSS_annual * 100) %>%
  select(Balance, Type, Year, DIN_per:TSS_per)


Inputs_Percent$DIN_per <- ifelse(Inputs_Percent$Year == 2008, NA, Inputs_Percent$DIN_per)
Inputs_Percent$DOC_per <- ifelse(Inputs_Percent$Year == 2011, NA,
                                 ifelse(Inputs_Percent$Year < 2010, NA, Inputs_Percent$DOC_per))  

write.csv(Inputs_Percent, "results/main_compile_inputs/inputsaspercentage.csv")
  

#WHY DOC INCREASING?
c <- ggplot(subset(Budget_Components, Solute == "DOC"), aes(Year, Load_kgyr, color=Type))+ geom_point() + geom_line()
ggplotly(c)


