#main_estuarine_load_calc.R

#Author: Anna Mikulis, University of New Hampshire
#Last Updated 1/26/2022

#Purpose: Calculates high and low tide flux of solutes based on river input of freshwater and known tidal prism

Packages <- c("readr", "dplyr", "lubridate", "ggplot2", "cowplot", 
              "tidyr","viridis",  "plotly", "measurements", "zoo", "car", "agricolae")

lapply(Packages, library, character.only = TRUE)

#Great Bay Tidal Prism
#Tidal Prism: Volume of water in an estuary b/t mean high tide and mean low tide

#Great Bay Estuary has a tidal prism of 79*10^6 m^3/day (Trowbridge, 2007)
  #Since two tidal cycles per day = 178*10^6 m^3/day enters and leaves

#Scale estuary prism to the Great Bay prism using ratio of surface area at high tide
#Great Bay: Great Bay Estuary = 16.7km^2:54.66km^2

scalar_ratio <- 16.7/54.66

round(scalar_ratio,2)
(178*10^6)*(0.31)

GB_Prism_m3day <- 55180000
GB_Prism_m3_year <- GB_Prism_m3day * 365

#Load formatted data frame "df6" and filter for Adams Point Stations
AP <- read.csv("results/main_dataformat/df_conc.csv") %>%
  select(-X, -pH, -SPC_UMHO_CM, -TP_MGL, -NO3_MGL) %>% #
  filter(STATION_ID == "GRBAPH" | STATION_ID == "GRBAPL")
#Tide Stage was used to renname GRBAP to High or Low Tide
AP$START_DATE <- as.POSIXct(AP$START_DATE)

#Calculate TN as PN + TDN

AP$TN_MGL <- AP$PN_MGL + AP$TDN_MGL

#Tidal Residence Time 5-20 days
#Tidal Prism Volume: 178m^3
#Local RT: 20.4 days.... works great for our monthly solute mass balance

#Over the course of a day 178 x 10^6 m3 of ocean water enters and leaves Great Bay Estuary
#Average freshwater input during the day is 4.26x10^6 m3;
#ratio of FW input to Tidal Prism Exchange is 2.4%
#______________________________________________
#Let's plot salinity against River Discharge
#Load Q
WNC_Q <- read.csv("data/Discharge/Daily_Mean/WNC_Q.csv") %>%
  select(START_DATE, Q_mean_cfs) %>%
  mutate(Site = "WNC")

SQR_Q <- read.csv("data/Discharge/Daily_Mean/SQR_Q.csv") %>%
  select(START_DATE, Q_mean_cfs) %>%
  mutate(Site = "SQR")

LR_Q <- read.csv("data/Discharge/Daily_Mean/LR_Q.csv") %>%
  select(START_DATE, Q_mean_cfs) %>%
  mutate(Site = "LMP")

AP_salinity <- AP %>%
  select(START_DATE, SALINITY_PSS, Site = STATION_ID)

Q <- union(LR_Q, WNC_Q)
Q <- union(Q, SQR_Q)

Q$START_DATE <- as.POSIXct(Q$START_DATE)
Q$m3s <- measurements::conv_unit(Q$Q_mean_cfs, "ft3", "m3")

#scale instantaneous mean average daily Q to be m3/day
Q$m3_day <- Q$m3s * 86400

#Multiply flow at each gauge by the flow multiplier
Q$m3_day_fm <- ifelse(Q$Site == "LMP", Q$m3_day * 1.145435,
                      ifelse(Q$Site == "SQR", Q$m3_day * 1.683529,
                             ifelse(Q$Site == "WNC", Q$m3_day * 1.005443, NA)))

#Plot Daily Q as a summation 
Q_daily <- Q %>%
  select(START_DATE, m3_day_fm) %>%
  group_by(START_DATE) %>%
  summarise(m3_day = sum(m3_day_fm)) #sum of flow multiplied daily Q

write.csv(Q_daily, "results/main_estuarine_load_calc/total_q_day.csv")

#Join salinity data with discharge as m3/s or m3/day but not summed across rivers
Salinity <- full_join(AP_salinity, Q)

Salinity <- Salinity %>%
  filter(START_DATE > "2008-01-01" & START_DATE < "2019-01-01")

Sal <- ggplot(Salinity, aes(START_DATE, m3s)) + geom_line(aes(color=Site), size=1, alpha=0.5) +
  geom_line(aes(y = (SALINITY_PSS*6.75) ,color = Site), size = 1, alpha=0.9) +
  scale_y_continuous(limits = c(0,200),
                     sec.axis = sec_axis(~./6.75 , name = "Adams Point Salinity PSS")) + 
  ylab("River Discharge m3/s") +
  scale_color_viridis_d() +
  theme_cowplot()
Sal

#Join salinity data with Sum of FW input as m3/day
Daily_Sal <- full_join(AP_salinity, Q_daily) #has the flow multiplied Q

#Plot Freshwater Discharge against Salinity
Daily_Sal_hightide <- Daily_Sal %>%
  select(m3_day, SALINITY_PSS, Site) %>%
  filter(Site == "GRBAPH")

Daily_Sal_lowtide <- Daily_Sal %>%
  select(m3_day, SALINITY_PSS, Site) %>%
  filter(Site == "GRBAPL")

SalvsQ_HT <- ggplot(Daily_Sal_hightide, aes(m3_day, SALINITY_PSS)) + geom_point(color="darkblue", size=2) +
  #geom_smooth(method="lm", se=T) +
  stat_smooth(method="lm", formula=y~log(x), se=T) +
  scale_x_log10() + ylab("High Tide Salinity (pss)") + xlab("Total Freshwater Input"~m^3/day)+
  theme_cowplot()
SalvsQ_HT

SalvsQ_LT <- ggplot(Daily_Sal_lowtide, aes(m3_day, SALINITY_PSS)) + geom_point(color="black", size=2) +
  stat_smooth(method="lm", formula=y~log(x), color="black") +
  scale_x_log10() + 
  #scale_y_log10() + 
  ylab("Low Tide Salinity (PSS)") +xlab("Total Freshwater Discharge"~m^3~day^-1) +
  theme_cowplot() 
SalvsQ_LT

#SalvsQ_LT +  annotation_logticks(sides="b")

#Save freshwater discharge vs low tide salinity plot
ggsave(SalvsQ_LT, file=paste0("results/figures/lowtide_salvsdischarge.png"),
       width=8, height=6, units="in", dpi=300, bg="white")

#Regression between salinity and discharge
#Hightide
HT_lm <- lm(SALINITY_PSS ~ m3_day, Daily_Sal_hightide)
summary(HT_lm)

plot(HT_lm)

ncvTest(HT_lm)#suggests no heteroskedasticity p > 0.05; fail to reject null of homoskedasticity 

LT_lm <- lm(SALINITY_PSS ~ m3_day, Daily_Sal_lowtide)
summary(LT_lm)

#plot(LT_lm)

ncvTest(LT_lm) #suggests heteroskedasticity p < 0.05; reject null of homoskedasticity 

#Plot Adams Point Solute Concentrations Against Freshwater Input
AP_Solutes <- AP %>%
  select(Site = STATION_ID, START_DATE:TSS_MGL)

AP_All <- full_join(AP_Solutes, Q_daily)

AP_All_LT <- AP_All %>%
  filter(Site == "GRBAPL")

AP_All_LT_long <- AP_All_LT %>%
  pivot_longer(cols=PO4_MGL:TSS_MGL, values_to = "Concentration", names_to = "Solute") %>%
  filter(Solute != "PC_MGL") %>%
  filter(Solute != "SIO2_MGL") %>%
  filter(Solute != "DON_MGL") %>%
  filter(Solute != "TDN_MGL" & Solute != "NO3_NO2_MGL") %>%
  filter(Solute != "NH4_MGL")

AP_All_HT <- AP_All %>%
  filter(Site == "GRBAPH")

AP_All_HT_long <- AP_All_HT %>%
  pivot_longer(cols=PO4_MGL:TSS_MGL, values_to = "Concentration", names_to = "Solute")

CQ_HT <- ggplot(AP_All_HT_long, aes(m3_day, Concentration)) + geom_point() +
  geom_smooth(data=subset(AP_All_HT_long, Solute == "DIN_MGL") ,method="lm", se=T, colour = "red", size = 1) +
  geom_smooth(data=subset(AP_All_HT_long, Solute == "DOC_MGL") ,method="lm", se=T, colour = "red", size = 1) +
  geom_smooth(data=subset(AP_All_HT_long, Solute == "DIN_MGL") ,method="lm", se=T, colour = "red", size = 1) +
  geom_smooth(data=subset(AP_All_HT_long, Solute == "NO3_NO2_MGL") ,method="lm", se=T, colour = "red", size = 1) +
  geom_smooth(data=subset(AP_All_HT_long, Solute == "SIO2_MGL") ,method="lm", se=T, colour = "red", size = 1) +
  geom_smooth(data=subset(AP_All_HT_long, Solute == "PN_MGL") ,method="lm", se=T, colour = "red", size = 1) +
 # geom_smooth(data=subset(AP_All_HT_long, Solute == "PO4_MGL") ,method="lm", se=T, colour = "blue", size = 1) +
  geom_smooth(data=subset(AP_All_HT_long, Solute == "TDN_MGL") ,method="lm", se=T, colour = "red", size = 1) +
  geom_smooth(data=subset(AP_All_HT_long, Solute == "DON_MGL") ,method="lm", se=T, colour = "red", size = 1) +
  geom_smooth(data=subset(AP_All_HT_long, Solute == "TN_MGL") ,method="lm", se=T, colour = "red", size = 1) +
  scale_x_log10() + ylab("Adams Point High Tide Concentration") + xlab("Freshwater Input m3/day") +
  facet_wrap(~Solute, scales = "free") +
  theme_cowplot()
CQ_HT

#Regression between DIN and discharge
ht.din.lm <- lm(DIN_MGL ~ m3_day, AP_All_HT)
summary(ht.din.lm)

ht.doc.lm <- lm(DOC_MGL ~ m3_day, AP_All_HT)
summary(ht.doc.lm)

ht.nh4.lm <- lm(NH4_MGL ~ m3_day, AP_All_HT)
summary(ht.nh4.lm) #not significant

ht.no32.lm <- lm(NO3_NO2_MGL ~ m3_day, AP_All_HT)
summary(ht.no32.lm)

ht.sio2.lm <- lm(SIO2_MGL ~ m3_day, AP_All_HT)
summary(ht.sio2.lm)

ht.tdn.lm <- lm(TDN_MGL ~ m3_day, AP_All_HT)
summary(ht.tdn.lm)

ht.tn.lm <- lm(TN_MGL ~ m3_day, AP_All_HT)
summary(ht.tn.lm)

ht.don.lm <- lm(DON_MGL ~ m3_day, AP_All_HT)
summary(ht.don.lm)

ht.pn.lm <- lm(PN_MGL ~ m3_day, AP_All_HT)
summary(ht.pn.lm)

ht.po4.lm <- lm(PO4_MGL ~ m3_day, AP_All_HT)
summary(ht.po4.lm) #not significant

ht.pc.lm <- lm(PC_MGL ~ m3_day, AP_All_HT)
summary(ht.pc.lm) #not significant

ht.tss.lm <- lm(TSS_MGL ~ m3_day, AP_All_HT)
summary(ht.tss.lm) #not significant regression

#LOW TIDE
AP_All_LT_long$Solute <- ifelse(AP_All_LT_long$Solute == "PO4_MGL", "PO4", AP_All_LT_long$Solute)
AP_All_LT_long$Solute <- ifelse(AP_All_LT_long$Solute == "DIN_MGL", "DIN", ifelse(
  AP_All_LT_long$Solute == "DOC_MGL", "DOC", AP_All_LT_long$Solute))

AP_All_LT_long$Solute <- ifelse(AP_All_LT_long$Solute == "TN_MGL", "TN", ifelse(
  AP_All_LT_long$Solute == "PN_MGL", "PN", ifelse(
    AP_All_LT_long$Solute == "TSS_MGL", "TSS", AP_All_LT_long$Solute
  )
))

AP_LT_wide <- AP_All_LT_long %>%
  pivot_wider(names_from="Solute",
              values_from="Concentration")

skewness(AP_LT_wide$DOC)

skewness(AP_LT_wide$DIN)

skewness(AP_LT_wide$PN)

skewness(AP_LT_wide$PO4)

skewness(AP_LT_wide$TN)

skewness(AP_LT_wide$TSS)

cor(AP_LT_wide[,3:9], use="na.or.complete")

CQ_LT <- ggplot(AP_All_LT_long, aes(m3_day, Concentration)) + geom_point() +
  geom_smooth(data=subset(AP_All_LT_long, Solute == "DIN") ,method="lm", se=T, colour = "red", size = 1) +
  geom_smooth(data=subset(AP_All_LT_long, Solute == "DOC") ,method="lm", se=T, colour = "red", size = 1) +
  geom_smooth(data=subset(AP_All_LT_long, Solute == "TN") ,method="lm", se=T, colour = "red", size = 1) +
  geom_smooth(data=subset(AP_All_LT_long, Solute == "PN") ,method="lm", se=T, colour = "red", size = 1) +
  geom_smooth(data=subset(AP_All_LT_long, Solute == "PO4") ,method="lm", se=T, colour = "blue", size = 1) +
  scale_x_log10() + 
  #scale_y_log10() + 
  ylab(expression('Low Tide Concentration mg L'^{-1})) +
  xlab(expression('Freshwater Input m'^{3}~day^{-1})) +
  facet_wrap(~Solute, scales = "free") +
  theme_cowplot()
CQ_LT

ggsave(CQ_LT, file=paste0("results/figures/lowtide_cq.png"),
       width=8, height=6, dpi=300, units="in")

#Regression between concentration & discharge
lt.din.lm <- lm(DIN_MGL ~ m3_day, AP_All_LT)
summary(lt.din.lm)

lt.doc.lm <- lm(DOC_MGL ~ m3_day, AP_All_LT)
summary(lt.doc.lm)

lt.nh4.lm <- lm(NH4_MGL ~ m3_day, AP_All_LT)
summary(lt.nh4.lm)

lt.no32.lm <- lm(NO3_NO2_MGL ~ m3_day, AP_All_LT)
summary(lt.no32.lm)

lt.sio2.lm <- lm(SIO2_MGL ~ m3_day, AP_All_LT)
summary(lt.sio2.lm)

lt.tdn.lm <- lm(TDN_MGL ~ m3_day, AP_All_LT)
summary(lt.tdn.lm)

lt.tn.lm <- lm(TN_MGL ~ m3_day, AP_All_LT)
summary(lt.tn.lm)

lt.don.lm <- lm(DON_MGL ~ m3_day, AP_All_LT)
summary(lt.don.lm) #not significant

lt.pn.lm <- lm(PN_MGL ~ m3_day, AP_All_LT)
summary(lt.pn.lm)

lt.pc.lm <- lm(PC_MGL ~ m3_day, AP_All_LT)
summary(lt.pc.lm) 

lt.tss.lm <- lm(TSS_MGL ~ m3_day, AP_All_LT)
summary(lt.tss.lm) #not significant

lt.po4.lm <- lm(PO4_MGL ~ m3_day, AP_All_LT)
summary(lt.po4.lm)

#__________________________________________________________________________________________________________________________________________
#Monthly Discharge Estimate
Q$Month <- month(Q$START_DATE)
Q$Year <- year(Q$START_DATE)
Q$daysinmonth <- days_in_month(Q$START_DATE)
Q$Year_Month <- as.yearmon(paste(Q$Year, Q$Month), "%Y %m")

#Sum daily average instantaneous, scaled to m3/day, by year and month
Q_Month <- Q %>%
  select(START_DATE, Year, Month, Site, m3s, m3_day, m3_day_fm) %>%
  group_by(Year, Month) %>%
  summarize(m3_month = sum(m3_day_fm))

Q_Month$Year_Month <- as.yearmon(paste(Q_Month$Year, Q_Month$Month), "%Y %m")

#Average per month * number of days in month
daysinmonth <- Q %>%
  select(Year, Month, daysinmonth)  %>%
  unique()

Q_Month_est <- Q %>%
  select(Year, Month, m3_day_fm, daysinmonth) %>%
  group_by(Year, Month) %>%
  summarize(avg_m3_day = mean(m3_day_fm, na.rm=T))

Q_Month_est <- full_join(Q_Month_est,daysinmonth)

Q_Month_est$m3_month <- Q_Month_est$daysinmonth * Q_Month_est$avg_m3_day

plot(Q_Month$m3_month, Q_Month_est$avg_m3_day) # they match, go with the summation for each month

#Annual Q Estimate
Q_Year <- Q %>%
  filter(Year >2007) %>%
  filter(Year < 2020) %>%
  group_by(Year) %>%
  summarize(Q_m3_year = sum(m3_day))

Q_Year$L_Year <- conv_unit(Q_Year$Q_m3_year, "m3", "l")

plot(Q_Year$Year, Q_Year$Q_m3_year)

Annual_FW_Input <- ggplot(Q_Year, aes(Year, Q_m3_year)) + geom_point() +
  theme_cowplot()
Annual_FW_Input

##Adams Point Flux

#Tidal Prism is the reported prism with annual discharge backed out; so as to get just the ocean water flux
Tidal_Prism <- Q_Year

Tidal_Prism$Leap_Year <- leap_year(Tidal_Prism$Year)

Tidal_Prism$daysinyear <- ifelse(Tidal_Prism$Leap_Year == T, 366, 365)

Tidal_Prism$TP_m3_year <- Tidal_Prism$daysinyear * GB_Prism_m3day

#Tidal Prism with FW Input backed out
Tidal_Prism$Ocean_m3_year <- Tidal_Prism$TP_m3_year - Tidal_Prism$Q_m3_year



#Average Annual AP Concentrations
AP_Annual <- AP 
  
AP_Annual$Year <- year(AP_Annual$START_DATE)

AP_Annual <- AP_Annual %>%
  group_by(Year, STATION_ID) %>%
  summarize(across(PO4_MGL:TSS_MGL, mean, na.rm=T))

AP_Decadal <- AP_Annual %>%
  select(-NH4_MGL, -NO3_NO2_MGL, -DON_MGL) %>%
  group_by(STATION_ID) %>%
  summarize(across(PO4_MGL:TSS_MGL, mean, na.rm=T))

round(AP_Decadal[,2:10], 2)

AP_Decadal_sd <- AP_Annual %>%
  select(-NH4_MGL, -NO3_NO2_MGL, -DON_MGL) %>%
  group_by(STATION_ID) %>%
  summarize(across(PO4_MGL:TSS_MGL, sd, na.rm=T))


round(AP_Decadal_sd[,2:10], 2)

AP_Flux <- full_join(AP_Annual, Tidal_Prism)

colnames(AP_Flux)

AP_Flux$Ocean_L_year <- conv_unit(AP_Flux$Ocean_m3_year, "m3", "l")
avg <- mean(AP_Flux$Ocean_L_year, na.rm=T)
print(avg)

AP_Flux$PO4 <- AP_Flux$PO4_MGL * AP_Flux$Ocean_L_year
AP_Flux$TN <- AP_Flux$TN_MGL * AP_Flux$Ocean_L_year
AP_Flux$PN <- AP_Flux$PN_MGL * AP_Flux$Ocean_L_year
AP_Flux$TDN <- AP_Flux$TDN_MGL * AP_Flux$Ocean_L_year
AP_Flux$NH4 <- AP_Flux$NH4_MGL * AP_Flux$Ocean_L_year
AP_Flux$NO32 <- AP_Flux$NO3_NO2_MGL * AP_Flux$Ocean_L_year
AP_Flux$DIN <- AP_Flux$DIN_MGL * AP_Flux$Ocean_L_year
AP_Flux$DON <- AP_Flux$DON_MGL * AP_Flux$Ocean_L_year
AP_Flux$DOC <- AP_Flux$DOC_MGL * AP_Flux$Ocean_L_year
AP_Flux$PC <- AP_Flux$PC_MGL * AP_Flux$Ocean_L_year
AP_Flux$SIO2 <- AP_Flux$SIO2_MGL * AP_Flux$Ocean_L_year
AP_Flux$TSS <- AP_Flux$TSS_MGL * AP_Flux$Ocean_L_year


for (i in 22:33) {
  AP_Flux[,i] <-conv_unit(AP_Flux[,i], "mg", "kg")
}


#AP Flux now in kg/year
write.csv(AP_Flux, "results/main_estuarine_load_calc/AP_Flux_kgyr.csv")


#MONTHLY ADAMS POINT LOADS

#Tidal Prism is the reported prism with monthly discharge backed out; so as to get just the ocean water flux
Tidal_Prism_Monthly <- Q_Month

Tidal_Prism_Monthly <- full_join(Tidal_Prism_Monthly, daysinmonth)

Tidal_Prism_Monthly$Prism_m3_month <- Tidal_Prism_Monthly$daysinmonth * GB_Prism_m3day

#Tidal Prism with FW Input backed out
Tidal_Prism_Monthly$Ocean_m3_month <- Tidal_Prism_Monthly$Prism_m3_month - Tidal_Prism_Monthly$m3_month


#Average Monthly AP Concentrations
AP_Monthly <- AP 

AP_Monthly$Month <- month(AP_Monthly$START_DATE)
AP_Monthly$Year <- month(AP_Monthly$START_DATE)

AP_Monthly <- AP_Monthly %>%
  group_by(Year, Month, STATION_ID) %>%
  summarize(across(PO4_MGL:TSS_MGL, mean, na.rm=T))

AP_MFlux <- full_join(AP_Monthly, Tidal_Prism_Monthly)

colnames(AP_MFlux)

AP_MFlux$Ocean_L_month <- conv_unit(AP_MFlux$Ocean_m3_month, "m3", "l")

AP_MFlux$PO4 <- AP_MFlux$PO4_MGL * AP_MFlux$Ocean_L_month
AP_MFlux$TN <- AP_MFlux$TN_MGL * AP_MFlux$Ocean_L_month
AP_MFlux$PN <- AP_MFlux$PN_MGL * AP_MFlux$Ocean_L_month
AP_MFlux$TDN <- AP_MFlux$TDN_MGL * AP_MFlux$Ocean_L_month
AP_MFlux$NH4 <- AP_MFlux$NH4_MGL * AP_MFlux$Ocean_L_month
AP_MFlux$NO32 <- AP_MFlux$NO3_NO2_MGL * AP_MFlux$Ocean_L_month
AP_MFlux$DIN <- AP_MFlux$DIN_MGL * AP_MFlux$Ocean_L_month
AP_MFlux$DON <- AP_MFlux$DON_MGL * AP_MFlux$Ocean_L_month
AP_MFlux$DOC <- AP_MFlux$DOC_MGL * AP_MFlux$Ocean_L_month
AP_MFlux$PC <- AP_MFlux$PC_MGL * AP_MFlux$Ocean_L_month
AP_MFlux$SIO2 <- AP_MFlux$SIO2_MGL * AP_MFlux$Ocean_L_month
AP_MFlux$TSS <- AP_MFlux$TSS_MGL * AP_MFlux$Ocean_L_month


for (i in 22:33) {
  AP_MFlux[,i] <-conv_unit(AP_MFlux[,i], "mg", "kg")
}


#Clean up monthly flux
AP_MFlux_Final <- AP_MFlux %>%
  select(STATION_ID, Month, Year, Q_m3_month=m3_month, Prism_m3_month, Ocean_m3_month, Ocean_L_month, PO4:TSS)


write.csv(AP_MFlux_Final, "results/main_estuarine_load_calc/AP_Monthly_Flux_kgmonth.csv")
