#main_dilution_calculation.R
#Author: Anna Lowien, University of New Hampshire

#Last Updated: 11/30/2021

#Purpose: Calculate the expected dilution of river inputs into the Great Bay basin

#Load packages

#Load packages
Packages <- c("readr", "dplyr", "measurements", "lubridate")

lapply(Packages, library, character.only = TRUE)

#Daily Average Q for each river; ft^3/s
LR_Q <- read.csv("data/Discharge/Daily_Mean/LR_Q.csv")
SQR_Q <- read.csv("data/Discharge/Daily_Mean/SQR_Q.csv")
WNC_Q <- read.csv("data/Discharge/Daily_Mean/WNC_Q.csv")

#Group together
All_Q <- full_join(LR_Q, SQR_Q)
All_Q <- full_join(All_Q, WNC_Q)

#Filter for 2008-2018
All_Q$START_DATE <- as.POSIXct(All_Q$START_DATE)

All_Q$year <- year(All_Q$START_DATE)

All_Q <- All_Q %>%
  filter(year > 2007 & year < 2019) %>%
  select(site_no:year) %>%
  mutate(site=site_no) %>%
  select(-site_no)

All_Q$site <- ifelse(All_Q$site ==1073500, "lmp",
                     ifelse(All_Q$site == 1073587, "sqr", "wnc"))

#Calculate a 10 year average and standard deviation for each river

dec_avg <- All_Q %>%
  group_by(site) %>%
  summarize(mean_cfs = mean(Q_mean_cfs, na.rm=T),
            sd_cfs = sd(Q_mean_cfs, na.rm=T))

dec_avg$mean_m3s <- conv_unit(dec_avg$mean_cfs, "ft3", "m3")
dec_avg$sd_m3s <- conv_unit(dec_avg$sd_cfs, "ft3", "m3")

GBE_tidalprism_m3day <- 178*10^6
SA_GBE_km2 <- 54.66
SA_GB_km2 <- 16.7
SA_ratio <- SA_GB_km2/SA_GBE_km2

GB_tidalprism_m3day <- GBE_tidalprism_m3day * SA_ratio

#Flow-weighted river concentrations
lmp <- read.csv("results/main_load_calc/FWC/CY_FW_Conc_LMP.csv")
sqr <- read.csv("results/main_load_calc/FWC/CY_FW_Conc_SQR.csv")
wnc <- read.csv("results/main_load_calc/FWC/CY_FW_Conc_WNC.csv")

#units mg/L
lmp <- lmp %>%
  select(-X, -FW_TP, -FW_NH4, -FW_NO3_NO2) %>%
  mutate(site = "lmp")

dec_lmp <- lmp %>%
  summarize(TN = mean(FW_TN, na.rm=T),
            PN = mean(FW_PN, na.rm=T),
            DIN = mean(FW_DIN, na.rm=T),
            TSS= mean(FW_TSS, na.rm=T),
            PO4 = mean(FW_PO4, na.rm=T),
            DOC = mean(FW_DOC, na.rm=T))

dec_lmp$stat <- "mean"

sd_lmp <- lmp %>%
  summarize(TN = sd(FW_TN, na.rm=T),
            PN = sd(FW_PN, na.rm=T),
            DIN = sd(FW_DIN, na.rm=T),
            TSS= sd(FW_TSS, na.rm=T),
            PO4 = sd(FW_PO4, na.rm=T),
            DOC = sd(FW_DOC, na.rm=T))

sd_lmp$stat <- "sd"

round(dec_lmp[,], 2)
round(sd_lmp[,], 2)

dec_lmp_mean <- full_join(dec_lmp, sd_lmp)
dec_lmp_mean$site <- "lmp"


sqr <- sqr %>%
  select(-X, -FW_TP, -FW_NH4, -FW_NO3_NO2) %>%
  mutate(site = "sqr")

dec_sqr <- sqr %>%
  summarize(TN = mean(FW_TN, na.rm=T),
            PN = mean(FW_PN, na.rm=T),
            DIN = mean(FW_DIN, na.rm=T),
            TSS= mean(FW_TSS, na.rm=T),
            PO4 = mean(FW_PO4, na.rm=T),
            DOC = mean(FW_DOC, na.rm=T))



dec_sqr$stat <- "mean"

sd_sqr <- sqr %>%
  summarize(TN = sd(FW_TN, na.rm=T),
            PN = sd(FW_PN, na.rm=T),
            DIN = sd(FW_DIN, na.rm=T),
            TSS= sd(FW_TSS, na.rm=T),
            PO4 = sd(FW_PO4, na.rm=T),
            DOC = sd(FW_DOC, na.rm=T))
sd_sqr$stat <- "sd"

round(dec_sqr[,], 2)
round(sd_sqr[,], 2)

dec_sqr_mean <- full_join(dec_sqr, sd_sqr)
dec_sqr_mean$site <- "sqr"



wnc <- wnc %>%
  select(-X, -FW_TP, -FW_NH4, -FW_NO3_NO2) %>%
  mutate(site = "wnc")

dec_wnc <- wnc %>%
  summarize(TN = mean(FW_TN, na.rm=T),
            PN = mean(FW_PN, na.rm=T),
            DIN = mean(FW_DIN, na.rm=T),
            TSS= mean(FW_TSS, na.rm=T),
            PO4 = mean(FW_PO4, na.rm=T),
            DOC = mean(FW_DOC, na.rm=T))
dec_wnc$stat <- "mean"

sd_wnc <- wnc %>%
  summarize(TN = sd(FW_TN, na.rm=T),
            PN = sd(FW_PN, na.rm=T),
            DIN = sd(FW_DIN, na.rm=T),
            TSS= sd(FW_TSS, na.rm=T),
            PO4 = sd(FW_PO4, na.rm=T),
            DOC = sd(FW_DOC, na.rm=T))

sd_wnc$stat <- "sd"

round(dec_wnc[,], 2)
round(sd_wnc[,], 2)

dec_wnc_mean <- full_join(dec_wnc, sd_wnc)
dec_wnc_mean$site <- "wnc"

#combine these three dataframes
dec_river <- full_join(dec_lmp_mean, dec_sqr_mean)
dec_river <- full_join(dec_river, dec_wnc_mean)

write.csv(dec_river, "results/dec_river.csv")

#LMP average FW concentration: 0.436367 mg/L
#LMP average Q:8.9263006m3/s

lmp_dec_Q_Lday <- conv_unit(8.9263006, "m3", "l") * 86400

lmp_load_mg_day <-  0.436367 * lmp_dec_Q_Lday

GB_tidalprism_lday <- conv_unit(GB_tidalprism_m3day, "m3","l")

GBE_mid_volume_m3 <- 196*10^6

GB_mid_volume_L <- conv_unit(GBE_mid_volume_m3 * SA_ratio, "m3", "l")

#fx <- fraction of fw at any location in  estuary (vol fw / total water volume)

fx <- lmp_dec_Q_Lday / GB_mid_volume_L

cx <- fx*(lmp_load_mg_day/lmp_dec_Q_Lday)

cx <- round(cx, 4)

#SQR average TN FW concentration:0.4584633 mg/L
#SQR average Q:3.1786970m3/s

sqr_dec_Q_Lday <- conv_unit(3.1786970, "m3", "l") * 86400

sqr_load_mg_day <-  0.4584633 * sqr_dec_Q_Lday

#fx <- fraction of fw at any location ine stuary (vol fw / total water volume)

fx_sqr <- sqr_dec_Q_Lday / GB_mid_volume_L

cx_sqr <- fx_sqr*(sqr_load_mg_day/sqr_dec_Q_Lday)

cx_sqr <- round(cx_sqr, 4)

#WNC average TN FW concentration:0.4584633 mg/L
#WNC average Q:3.1786970m3/s

wnc_dec_Q_Lday <- conv_unit(0.7838303, "m3", "l") * 86400

wnc_load_mg_day <-  0.6046714 * wnc_dec_Q_Lday

#fx <- fraction of fw at any location ine stuary (vol fw / total water volume)

fx_wnc <- wnc_dec_Q_Lday / GB_mid_volume_L

cx_wnc <- fx_wnc*(wnc_load_mg_day/wnc_dec_Q_Lday)

cx_wnc <- round(cx_wnc, 5)

cx + cx_sqr + cx_wnc

library(scales)

scientific(sqr_dec_Q_Lday, digits =3)
scientific(lmp_dec_Q_Lday, digits=3)
scientific(wnc_dec_Q_Lday, digits=3)

#Diluted TN concentration
dil.tn.mgl <- ((0.44 * lmp_dec_Q_Lday) +(0.46 * sqr_dec_Q_Lday) + (0.60 * wnc_dec_Q_Lday)) / 
  (lmp_dec_Q_Lday +sqr_dec_Q_Lday + wnc_dec_Q_Lday + GB_tidalprism_lday)

round(dil.tn.mgl, 3)

#Diluted DIN concentration
dil.din.mgl <- ((0.13 * lmp_dec_Q_Lday) + (0.12 * sqr_dec_Q_Lday) + (0.19 * wnc_dec_Q_Lday)) /
  (lmp_dec_Q_Lday + sqr_dec_Q_Lday + wnc_dec_Q_Lday + GB_tidalprism_lday)

round(dil.din.mgl, 3)
