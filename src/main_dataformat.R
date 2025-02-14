# main_dataformat.R

# Author: Anna Mikulis, University of New Hampshire
# Last Updated: 2/13/2025

#R Version 4.4.2 (2024-10-31) -- "Pile of Leaves"

# Purpose: Read in and summarize solute concentration data for the three tidal tributaries (head-of-tide monitoring stations) that flow into Great Bay and the tidal samples at Adams Point (high and low tide). Prepare discharge data for flux calculations.
# Create physiochemical characteristics tables. 

# Data was sourced from the NH Department of Environmental Services, Environmental Monitoring Database (EMD) by email request.
# Data was pulled for grab samples and physical chemistry based on assigned water body IDs.

  #Sites
    #05-LMP: Lamprey River (LMP)
    #09-EXT: Squamscott River (SQR)
    #02-WNC: Winnicut River (WNC)
    #GRBAP: Great Bay Adams Point (Estuarine Monitoring Site)

#Run this script to format the EMD dataset and the discharge dataset for later scripts. And create summary tables. 

#Load required packages.
Packages <- c("readxl", "dplyr", "ggplot2", "tidyquant", "cowplot", "stringr",  "plotly", "measurements", "moments", "flextable")

lapply(Packages, library, character.only = TRUE)

df <- read.csv("./results/main_dataformat/df_conc.csv")

df$START_DATE <- as.Date(df$START_DATE)

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

#Average + standard deviation of each solute for each river (Table S1, in Supplemental Information)
colnames(df)

avg_conc <- df %>%
  select(STATION_ID, START_DATE, TN_MGL, PO4_MGL:DO_MGL, DO_sat) %>%
  mutate(PO4_UGL = conv_unit(PO4_MGL, "mg", "ug"),
         NH4_UGL = conv_unit(NH4_MGL, "mg", "ug")) %>%
  select(-PO4_MGL, -NH4_MGL) %>%  
  group_by(STATION_ID) %>%
  summarize(across(TN_MGL:NH4_UGL,  \(x) mean(x, na.rm=T)) )

avg_conc[,2:13] <- signif(avg_conc[,2:13], 3)

avg_conc <- avg_conc %>%
  tidyr::pivot_longer(cols=c(TN_MGL:NH4_UGL),names_to = "Solute", values_to = "Concentration")

avg_conc <- avg_conc %>%
  tidyr::pivot_wider(names_from= "STATION_ID", values_from = "Concentration")

write.csv(avg_conc, "results/main_dataformat/avg_solute_conc_site.csv")

std_conc <- df %>%
  select(STATION_ID, START_DATE, TN_MGL, PO4_MGL:DO_MGL, DO_sat) %>%
  mutate(PO4_UGL = conv_unit(PO4_MGL, "mg", "ug"),
         NH4_UGL = conv_unit(NH4_MGL, "mg", "ug")) %>%
  select(-PO4_MGL, -NH4_MGL) %>%  
  group_by(STATION_ID) %>%
  summarize(across(TN_MGL:NH4_UGL, \(x) sd(x, na.rm=T)))

std_conc[,2:13] <- signif(std_conc[,2:13], 3)

std_conc <- std_conc %>%
  tidyr::pivot_longer(cols=c(TN_MGL:NH4_UGL),names_to = "Solute", values_to = "Concentration")

std_conc <- std_conc %>%
  tidyr::pivot_wider(names_from= "STATION_ID", values_from = "Concentration")

write.csv(std_conc, "results/main_dataformat/std_solute_conc_site.csv")


#number of samples per solute per site
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

write.csv(combined_data, 
          file=paste0("./results/manuscript_figures/supplemental/table_s1_raw_csv_files/avg_concentrations_supplemental.csv")) 

#phyisco chemical parameters
df_DO <- df %>%
  group_by(STATION_ID) %>%
  summarize(mean_DO_sat = mean(DO_sat, na.rm=T), sd_DO_sat = sd(DO_sat, na.rm=T), count = n())

ggplot(df, aes(START_DATE, DO_MGL)) + geom_point() + facet_wrap(~STATION_ID) +
  scale_x_date(date_breaks="1 year", date_labels = "%y")

df_DOmgl <- df %>%
  group_by(STATION_ID) %>%
  summarize(mean_DO_mgl = mean(DO_MGL, na.rm=T), sd_DO_mgl = sd(DO_MGL, na.rm=T), count = n())

df_temp <- df %>%
  group_by(STATION_ID) %>%
  summarize(mean_temp = mean(TEMP_WATER_DEGC, na.rm=T), sd_temp = sd(TEMP_WATER_DEGC, na.rm=T), count = n())


