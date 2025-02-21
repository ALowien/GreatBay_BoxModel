#main_estuarine_load_calc.R

#Author: Anna Mikulis, University of New Hampshire
#Last Updated 2/13/2025

#Purpose: Calculates high and low tide flux of solutes based on river input of freshwater and known tidal prism

Packages <- c("readr", "dplyr", "lubridate", "ggplot2", "cowplot", 
              "tidyr","viridis",  "plotly", "measurements", "zoo", "car", "agricolae", "psych")

lapply(Packages, library, character.only = TRUE)

#Great Bay Tidal Prism
#Tidal Prism: Volume of water in an estuary b/t mean high tide and mean low tide

#Great Bay Estuary has a tidal prism of 79*10^6 m^3/day (Trowbridge, 2007)
  #Since two tidal cycles per day = 178*10^6 m^3/day enters and leaves

#Scale estuary prism to the Great Bay prism using ratio of surface area at high tide
#Great Bay: Great Bay Estuary = 17km^2:54.66km^2

scalar_ratio <- 17/54.66
round(scalar_ratio,2)

tidalprism_m3day <- 178*10^6

GB_Prism_m3day <- tidalprism_m3day*round(scalar_ratio,2)
GB_Prism_m3_year <- GB_Prism_m3day * 365

#Load formatted data frame "df6" and filter for Adams Point Stations
AP <- read.csv("results/main_dataformat/df_conc.csv") %>%
  select(-X, -SPC_UMHO_CM) %>% #
  filter(STATION_ID == "GRBAPH" | STATION_ID == "GRBAPL")
#Tide Stage was used to rename GRBAP to High or Low Tide
AP$START_DATE <- as.POSIXct(AP$START_DATE)

#plot concentrations over time
AP_piv <- AP %>%
  select(STATION_ID:TSS_MGL, TN_MGL, CHLA_corr_pheo_UGL) %>%
  pivot_longer(cols=c(PO4_MGL:CHLA_corr_pheo_UGL), names_to = "Solute", values_to = "Measure") 

ggplot(AP_piv, aes(START_DATE, Measure, color=STATION_ID)) + geom_point() +
  geom_line() +
  facet_wrap(~Solute, scales="free_y")

AP_summary <- AP
AP_summary$year <- year(AP$START_DATE)

AP_summary <- AP_summary %>%
  select(STATION_ID, year, PO4_MGL:TSS_MGL, TN_MGL)%>%
  pivot_longer(cols= PO4_MGL:TN_MGL, names_to = "Solute", values_to="Concentration") %>%
  group_by(STATION_ID, Solute) %>%
  summarize(mean= mean(Concentration, na.rm=T),
            sd = sd(Concentration, na.rm=T))

#Tidal Residence Time 5-20 days
#Tidal Prism Volume: 178m^3
#Local RT: 20.4 days.... works great for our monthly solute mass balance

#Over the course of a day 178 x 10^6 m3 of ocean water enters and leaves Great Bay Estuary
#Average freshwater input during the day is 4.26x10^6 m3;
#ratio of FW input to Tidal Prism Exchange is 2.4%
#______________________________________________
#Let's plot salinity against River Discharge
Q <- read_csv("results/main_dataformat/Q_tidal_tribs.csv") #flow column units are m3/s

AP_salinity <- AP %>%
  select(START_DATE, SALINITY_PSS, Site = STATION_ID)

Q <- Q[,2:7]
Q$m3s <- Q$flow #flow column units are m3/s

#scale instantaneous mean average daily Q to be m3/day
Q$m3_day <- Q$m3s * 86400

#Multiply flow at each gauge by the flow multiplier for full watershed
Q$m3_day_fm <- ifelse(Q$STATION_ID == "05-LMP", Q$m3_day * 1.156576,
                      ifelse(Q$STATION_ID == "09-EXT", Q$m3_day * 2.012195,
                             ifelse(Q$STATION_ID == "02-WNC", Q$m3_day * 1.235616, NA)))

ggplot(Q, aes(m3_day, m3_day_fm, color=STATION_ID)) + geom_point() +
  geom_abline(slope=1, intercept = 0) +
  facet_wrap(~STATION_ID)

#Plot Daily Q as a summation of the three rivers flow 
Q_daily <- Q %>%
  select(START_DATE, m3_day_fm) %>%
  group_by(START_DATE) %>%
  summarise(m3_day = sum(m3_day_fm)) #sum of flow multiplied daily Q , went from 18993 obs to 6331 

#summing flow
write.csv(Q_daily, "results/main_estuarine_load_calc/total_q_day.csv")

#Join salinity data with summed river freshwater input as m3/day
Daily_Sal <- full_join(AP_salinity, Q_daily) #has the flow multiplied Q

#Plot Freshwater Discharge against Salinity
Daily_Sal_hightide <- Daily_Sal %>%
  select(m3_day, SALINITY_PSS, Site) %>%
  filter(Site == "GRBAPH")

Daily_Sal_lowtide <- Daily_Sal %>%
  select(m3_day, SALINITY_PSS, Site) %>%
  filter(Site == "GRBAPL") %>%
  filter(!is.na(SALINITY_PSS))

SalvsQ_HT <- ggplot(Daily_Sal_hightide, aes(m3_day, SALINITY_PSS)) + geom_point(color="darkblue", size=2) +
  #geom_smooth(method="lm", se=T) +
  stat_smooth(method="lm", formula=y~log(x), se=T) +
  scale_x_log10() + ylab("High Tide Salinity (pss)") + xlab("Total Freshwater Input"~m^3~day^-1)+ #corrected for Flow multiplier
  theme_cowplot()
SalvsQ_HT

SalvsQ_LT <- ggplot(Daily_Sal_lowtide, aes(m3_day, SALINITY_PSS)) + geom_point(color="black", size=2) +
  stat_smooth(method="lm", formula=y~(x), color="black") +
  #geom_abline(slope=  -7.5562, intercept= 66.1782  ) + log-linear regression line matches what we plot using log scale and simple linear regression
  scale_x_log10() + 
  ylab("Low Tide Salinity (PSS)") +xlab("Total River Discharge"~m^3~day^-1) +
  theme_cowplot() +
  annotation_logticks(sides="b")
SalvsQ_LT

#Save freshwater discharge vs low tide salinity plot
ggsave(SalvsQ_LT, file=paste0("results/figures/lowtide_salvsdischarge.png"),
       width=8, height=6, units="in", dpi=300, bg="white")

describe(Daily_Sal_lowtide)

skewness(log10(Daily_Sal_lowtide$m3_day))
kurtosis(log10(Daily_Sal_lowtide$m3_day))

LT_lm <- lm(SALINITY_PSS ~ log10(m3_day), Daily_Sal_lowtide)
summary(LT_lm)
ncvTest(LT_lm) #suggests homoskedasticity p < 0.05

#Plot Low Tide Adams Point Solute Concentrations Against Freshwater Input
AP_Solutes <- AP %>%
  select(Site = STATION_ID, START_DATE:TSS_MGL, TN_MGL)

AP_All <- full_join(AP_Solutes, Q_daily)

AP_All_LT <- AP_All %>%
  filter(Site == "GRBAPL")

AP_All_LT_long <- AP_All_LT %>%
  pivot_longer(cols=PO4_MGL:TSS_MGL, values_to = "Concentration", names_to = "Solute") %>%
  filter(Solute != "NO3_NO2_MGL" & Solute != "NH4_MGL") 

#LOW TIDE Solutes

#remove units for plotting purposes
remove_MGL_suffix <- function(solute_column) {
  solute_column <- gsub("_MGL", "", solute_column)
  return(solute_column)
}

AP_All_LT_long$Solute <- remove_MGL_suffix(AP_All_LT_long$Solute)

AP_LT_wide <- AP_All_LT_long %>%
  pivot_wider(names_from="Solute",
              values_from="Concentration")

psych::describe(AP_LT_wide)

stats_AP_LT <- describe(AP_LT_wide[,3:ncol(AP_LT_wide)])
stats_AP_LT

cor(AP_LT_wide[,3:10], use="na.or.complete")

solute_vars <- c("DIN_MGL", "DOC_MGL",  "PN_MGL", "TN_MGL", 
                 "TSS_MGL", "PO4_MGL")

# Initialize a list to store results
results <- data.frame(Solute = character(),
                      Slope = numeric(),
                      Intercept = numeric(),
                      R2 = numeric(),
                      P_value = numeric(),
                      stringsAsFactors = FALSE)

# Loop over each solute variable, perform regression, and store results
for (solute in solute_vars) {
  # Perform linear regression
  formula <- as.formula(paste(solute, "~ m3_day"))
  model <- lm(formula, data = AP_All_LT)
  
  # Extract relevant statistics
  slope <- coef(model)[["m3_day"]]
  intercept <- coef(model)[["(Intercept)"]]
  r2 <- round(summary(model)$r.squared, 2)
  p_value <- formatC(summary(model)$coefficients[2, 4], format = "f", digits = 3)
  
  # Add to results table
  results <- rbind(results, data.frame(Solute = solute,
                                       Slope = slope,
                                       Intercept = intercept,
                                       R2 = r2,
                                       P_value = p_value,
                                       stringsAsFactors = FALSE))
}

# Print the results
print(results)   

cq_results <- data.frame(Solute = character(),
                      Slope = numeric(),
                      Intercept = numeric(),
                      R2 = numeric(),
                      AdjR2 = numeric(),
                      P_value = numeric(),
                      stringsAsFactors = FALSE)

AP_All_LT$m3_day_log <- log10(AP_All_LT$m3_day)

for (solute in solute_vars) {
     log_solute_name <- paste0(solute, "_log")
     AP_All_LT[[log_solute_name]] <- log10(AP_All_LT[[solute]])
     
       # Perform linear regression
       formula <- as.formula(paste(log_solute_name, "~ m3_day_log"))
       model <- lm(formula, data = AP_All_LT)
       ncv_result <- ncvTest(model)
       
        # Extract relevant statistics from regression: slope (b), intercept (a), r2 and p value
         slope <- coef(model)[["m3_day_log"]]
         intercept <- coef(model)[["(Intercept)"]]
         r2 <- round(summary(model)$r.squared, 2)
         adj_r2 <- round(summary(model)$adj.r.squared, 2)
         p_value <- formatC(summary(model)$coefficients[2, 4], format = "f", digits = 4)
         ncv_test_pvalue <- formatC(ncv_result$p, format = "f", digits = 4)
         
           # Add to results table
           cq_results <- rbind(cq_results, data.frame(Solute = solute,
                                                      Slope = slope,
                                                      Intercept = intercept,
                                                      R2 = r2,
                                                      AdjR2 = adj_r2,
                                                      P_value = p_value,
                                                      ncv_pvalue=ncv_test_pvalue,
                                                      stringsAsFactors = FALSE))
           }

# Print the results
print(cq_results)
#are p values below signficance threshold?
cq_results$signif_ncvTest <- cq_results$ncv_pvalue < 0.05

cq_results$signif_mod <- cq_results$P_value < 0.05

#slope chemostatic?
cq_results$chemostatic <- ifelse(cq_results$Slope >= -0.2 & cq_results$Slope <= 0.2, 'chemostatic', 'chemodynamic')
print(cq_results)

library(flextable)
flextable(cq_results)
#CVc/CVq

#DOC 
mean_c <- mean(AP_All_LT$DOC_MGL_log, na.rm=T)
sd_c <- sd(AP_All_LT$DOC_MGL_log, na.rm=T)

mean_q <- mean(AP_All_LT$m3_day_log)
sd_q <- sd(AP_All_LT$m3_day_log)

covcq <- (mean_q * sd_c)/(mean_c * sd_q)
covcq

doc_lm <- lm(DOC_MGL_log ~ m3_day_log, data=AP_All_LT)
summary(doc_lm)
#plot(doc_lm)

#plot inspired by godsey et al 2009
cq_slopes <- ggplot(cq_results, aes(Solute, Slope)) + geom_point(aes(shape=signif_mod), size=3) + theme_bw() +
  geom_abline(slope=0, intercept=0, color="grey") +
  scale_shape_manual(values=c(1, 16)) +
  ylab("CQ log-log slope") +
  theme(legend.position = "none")
cq_slopes


adj_r2_values <- data.frame(
  Solute = c("DIN", "DOC", "TN", "PO4"),
  AdjR2 = c(0.24, 0.33, 0.31, 0.17))  # From cq_results

CQ_LT <- ggplot(subset(AP_All_LT_long, Solute != "DON" & Solute != "TDN"), 
                 aes(m3_day, Concentration)) + geom_point() +
  geom_smooth(data=subset(AP_All_LT_long, Solute == "DIN"), method="lm", se=T, colour = "red") +
  geom_smooth(data=subset(AP_All_LT_long, Solute == "DOC"), method="lm", se=T, colour = "red") +
  geom_smooth(data=subset(AP_All_LT_long, Solute == "TN"), method="lm", se=T, colour = "red") +
  geom_smooth(data=subset(AP_All_LT_long, Solute == "PO4"), method="lm", se=T, colour = "blue") +
  scale_x_log10(labels =scales::math_format(format=log10)) +
  scale_y_log10() + 
  ylab(expression('Low Tide Estuarine Concentration mg L'^{-1})) +
  xlab(expression('Total River Discharge m'^{3}~day^{-1})) +
  annotation_logticks() +
  facet_wrap(~Solute, scales = "free", ncol=3) +
  geom_text(data = adj_r2_values, aes(label = paste("Adj R² =", round(AdjR2, 2))),
            x = Inf, y = Inf, hjust = 1.95, vjust = 18, size = 4, inherit.aes = FALSE) +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=14))

CQ_LT

ggsave(CQ_LT, file=paste0("results/manuscript_figures/Figure3.png"),
       width=8, height=6, dpi=300, units="in", bg="white")


#____________________________________________
#Monthly Discharge Estimate
Q$Month <- month(Q$START_DATE)
Q$Year <- year(Q$START_DATE)
Q$daysinmonth <- days_in_month(Q$START_DATE)
Q$Year_Month <- as.yearmon(paste(Q$Year, Q$Month), "%Y %m")

#Sum daily average instantaneous, scaled to m3/day, by year and month
Q_Month <- Q %>%
  select(START_DATE, Year, Month, STATION_ID, m3s, m3_day, m3_day_fm) %>%
  group_by(Year, Month) %>%
  summarize(m3_month = sum(m3_day_fm))

Q_Month$Year_Month <- as.yearmon(paste(Q_Month$Year, Q_Month$Month), "%Y %m")
#Average per month * number of days in month 
daysinmonth <- Q %>%
 select(Year, Month, daysinmonth)  %>%
  unique()

#Annual Q Estimate
Q_Year <- Q %>%
  filter(Year >2007) %>%
  filter(Year < 2024) %>%
  group_by(Year) %>%
  summarize(Q_m3_year = sum(m3_day),
            Q_m3_year_fm = sum(m3_day_fm))

Q_Year$L_Year <- conv_unit(Q_Year$Q_m3_year, "m3", "l")

Q_Year$Q_millionsm3_year <- Q_Year$Q_m3_year / 1000000
Q_Year$Q_millionsm3_year_fm <- Q_Year$Q_m3_year_fm/ 1000000
ggplot(Q_Year, aes(Year, Q_m3_year)) + geom_point() + theme_cowplot() +
  scale_x_continuous(limits=c(2007.5,2024.5), breaks=seq(from=2008,to=2024, by=2))

ggplot(Q_Year, aes(Year, Q_m3_year_fm)) + geom_point() + theme_cowplot() +
  scale_x_continuous(limits=c(2007.5,2024.5), breaks=seq(from=2008,to=2024, by=2))

ggplot(Q_Year, aes(Q_m3_year, Q_m3_year_fm, color=Year)) + geom_point() +
  geom_abline(intercept=0, slope=1)


ggplot(Q_Year, aes(Year, Q_millionsm3_year)) + geom_point() + theme_cowplot() +
  scale_x_continuous(limits=c(2007.5,2024.5), breaks=seq(from=2008,to=2024, by=2)) +
  scale_y_continuous(limits=c(0, 650))

ggplot(Q_Year, aes(Year, Q_millionsm3_year_fm)) + geom_point() + theme_cowplot() +
  scale_x_continuous(limits=c(2007.5,2024.5), breaks=seq(from=2008,to=2024, by=2)) +
  scale_y_continuous(limits=c(0, 850))


##Adams Point Flux

#Tidal Prism is the reported prism with annual discharge backed out; so as to get just the ocean water flux
Tidal_Prism <- Q_Year

Tidal_Prism$Leap_Year <- leap_year(Tidal_Prism$Year)

Tidal_Prism$daysinyear <- ifelse(Tidal_Prism$Leap_Year == T, 366, 365)

Tidal_Prism$TP_m3_year <- Tidal_Prism$daysinyear * GB_Prism_m3day

#Tidal Prism with FW Input backed out
Tidal_Prism$Ocean_m3_year <- Tidal_Prism$TP_m3_year - Tidal_Prism$Q_m3_year
Tidal_Prism$Ocean_m3_yearv2 <- Tidal_Prism$TP_m3_year - Tidal_Prism$Q_m3_year_fm

ggplot(Tidal_Prism, aes(Ocean_m3_year, Ocean_m3_yearv2)) + geom_point() +
  geom_abline(intercept = 0, slope=1)

Tidal_Prism <- Tidal_Prism %>% select(-Ocean_m3_year)
#Average Annual AP Concentrations
AP_Annual <- AP 
  
AP_Annual$Year <- year(AP_Annual$START_DATE)

AP_Annual <- AP_Annual %>%
  select(Year, STATION_ID, PO4_MGL,TN_MGL, PN_MGL:TSS_MGL) %>%
  group_by(Year, STATION_ID) %>%
  summarize(across(PO4_MGL:TSS_MGL, \(x) mean(x, na.rm=T)))

AP_Decadal <- AP_Annual %>%
  group_by(STATION_ID) %>%
  summarize(across(PO4_MGL:TSS_MGL, \(x) mean(x, na.rm=T)))

round(AP_Decadal[,2:11], 4)

AP_Decadal_sd <- AP_Annual %>%
  group_by(STATION_ID) %>%
  summarize(across(PO4_MGL:TSS_MGL, sd, na.rm=T))

round(AP_Decadal_sd[,2:11], 3)

AP_Flux <- full_join(AP_Annual, Tidal_Prism)

colnames(AP_Flux)

AP_Flux$Ocean_L_year <- conv_unit(AP_Flux$Ocean_m3_yearv2, "m3", "l")
avg <- mean(AP_Flux$Ocean_L_year, na.rm=T)
print(avg)

ggplot(AP_Flux, aes(Year, DOC_MGL)) + geom_point(aes(color=STATION_ID))

AP_Flux$PO4 <- AP_Flux$PO4_MGL * AP_Flux$Ocean_L_year
AP_Flux$TN <- AP_Flux$TN_MGL * AP_Flux$Ocean_L_year
AP_Flux$PN <- AP_Flux$PN_MGL * AP_Flux$Ocean_L_year
AP_Flux$TDN <- AP_Flux$TDN_MGL * AP_Flux$Ocean_L_year
AP_Flux$NH4 <- AP_Flux$NH4_MGL * AP_Flux$Ocean_L_year
AP_Flux$NO32 <- AP_Flux$NO3_NO2_MGL * AP_Flux$Ocean_L_year
AP_Flux$DIN <- AP_Flux$DIN_MGL * AP_Flux$Ocean_L_year
AP_Flux$DON <- AP_Flux$DON_MGL * AP_Flux$Ocean_L_year
AP_Flux$DOC <- AP_Flux$DOC_MGL * AP_Flux$Ocean_L_year
#AP_Flux$SIO2 <- AP_Flux$SIO2_MGL * AP_Flux$Ocean_L_year
AP_Flux$TSS <- AP_Flux$TSS_MGL * AP_Flux$Ocean_L_year


for (i in 23:32) {
  AP_Flux[,i] <-conv_unit(AP_Flux[,i], "mg", "kg")
}


#AP Flux now in kg/year
write.csv(AP_Flux, "results/main_estuarine_load_calc/AP_Flux_kgyr24.csv")


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
AP_Monthly$Year <- year(AP_Monthly$START_DATE)

AP_Monthly <- AP_Monthly %>%
  select(Year, Month, STATION_ID, PO4_MGL,TN_MGL, PN_MGL:TSS_MGL) %>%
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
#AP_MFlux$PC <- AP_MFlux$PC_MGL * AP_MFlux$Ocean_L_month
#AP_MFlux$SIO2 <- AP_MFlux$SIO2_MGL * AP_MFlux$Ocean_L_month
AP_MFlux$TSS <- AP_MFlux$TSS_MGL * AP_MFlux$Ocean_L_month


for (i in 20:29) {
  AP_MFlux[,i] <-conv_unit(AP_MFlux[,i], "mg", "kg")
}


#Clean up monthly flux
AP_MFlux_Final <- AP_MFlux %>%
  select(STATION_ID, Month, Year, Q_m3_month=m3_month, Prism_m3_month, Ocean_m3_month, Ocean_L_month, PO4:TSS)


write.csv(AP_MFlux_Final, "results/main_estuarine_load_calc/AP_Monthly_Flux_kgmonth.csv")

