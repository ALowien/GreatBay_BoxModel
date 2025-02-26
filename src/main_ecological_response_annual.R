#main_ecological_response_annual.R

#Author: Anna Mikulis, University of New Hampshire
#Last Updated 2/25/2025

#Purpose: Is there a pattern in water chemistry that explains ecological response variables? (i.e. eelgrasss acreage)
#Code for manuscript Figures 1 and 6. 
#This script includes the calculations of nitrogen uptake by eelgrass, as used in the discussion section. 

#Load packages
Packages <- c("readr", "dplyr", "ggplot2", "lubridate", "readxl", "measurements", "cowplot", 
              "viridis", "scales", "tidyr", "moments", "tidyr", "car", "plotly", "SWMPr", "ggbreak", "psych")

lapply(Packages, library, character.only = TRUE)


GB_hectares <- 1700 #surface area, hectares

#Annual estimate of eelgrass coverage (in Great Bay proper)
eelgrass <- read.csv("./data/ecological_response/eelgrass_acres.csv") %>%
  rename(year = Year)

#convert acres to hectares
eelgrass <- eelgrass %>%
  mutate(across(c(GB_ZM_acres:GBE_ZM_acres), 
                ~ conv_unit(., "acre", "hectare"), 
                .names = "{.col}_ha")) %>%
  rename(GB_ZM_ha = GB_ZM_acres_ha,
         GBE_ZM_ha = GBE_ZM_acres_ha)

#Plot of seagrassc overage over time in Great Bay
eel.lm.mod <-lm(GB_ZM_ha ~ year, eelgrass)
summary(eel.lm.mod)
cor(eelgrass$year, eelgrass$GB_ZM_ha)

#1996 to 2023 Great Bay seagrass loss
e96 <- eelgrass[26,4]
e96

e23 <- eelgrass[1,4]
e23

#%loss in Great Bay 
(e96 - e23) / e96 * 100


#Great Bay Estuary wide
e_gbe_96 <- eelgrass[26,5]
e_gbe_23 <- eelgrass[1,5]

(e_gbe_96 - e_gbe_23) / e_gbe_96 * 100


eelgrass_plot <- ggplot(eelgrass, aes(year, GB_ZM_ha)) + geom_point(size=3) +
  xlab("Year") + ylab("Seagrass Meadow Area (ha)") + 
  geom_smooth(method="lm", color="black") +
  scale_y_continuous(limits=c(0,1025)) +
  scale_x_continuous(limits=c(1996,2023), 
                     breaks=seq(from=1996,to=2023,by=1), # Set breaks for every 1 year
                     labels=ifelse(seq(from=1996,to=2023,by=1) %% 2 == 0, seq(from=1996,to=2023,by=1), "")) +  
  theme_bw() +
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=15),
        axis.title.x = element_blank())

eelgrass_plot

# Annual Rainfall Trends
rainfall <- read.csv("./data/precipitation/CY_Rainfall_24update.csv") %>%
  select(year, rainfall_mm = total_rainfall_mm) %>%
  filter(year > 2001)

greenland_rainfall <- read.csv("./data/precipitation/greenland_nh_annualprecipitation.csv")

greenland_rainfall$Annual_in <- as.numeric(greenland_rainfall$Annual_in)
greenland_rainfall$rainfall_mm <- conv_unit(greenland_rainfall$Annual_in, "in", 'mm')

ggplot(greenland_rainfall, aes(Year, rainfall_mm)) + geom_point()

ggplot(rainfall, aes(as.integer(year), rainfall_mm)) + geom_point() +
  scale_y_continuous(limits=c(1000,1900))

#pull 1996-2001 rainfall from Greenland ,NH NOAA NOW data
greenland_rainfall <- greenland_rainfall %>%
  filter(Year < 2002) %>%
  rename(year= Year) %>%
  select(year, rainfall_mm)

rainfall <- full_join(rainfall, greenland_rainfall)

rainfall$year <- as.numeric(rainfall$year)
mean_rain <- mean(rainfall$rainfall_mm)
sd_rain <- sd(rainfall$rainfall_mm)

rainfall_plot <- ggplot(rainfall, aes(year, rainfall_mm)) + 
  annotate("rect", xmin=-Inf,xmax = Inf, 
           ymin=mean_rain - sd_rain, ymax=mean_rain + sd_rain, color="lightgrey", alpha=0.25) +
  geom_hline(yintercept = mean_rain, linetype="dashed", linewidth=1, color="grey2") +
  geom_point(size=3) +
  geom_line() +
  xlab("Year") + ylab("Annual Precipitation (mm)") + 
  scale_y_continuous(limits=c(800,1800), breaks =seq(from=800,to=1800, by=200)) +
  scale_x_continuous(limits=c(1996,2023), 
                       breaks=seq(from=1996,to=2023,by=1), 
                      labels=ifelse(seq(from=1996,to=2023,by=1) %% 2 == 0, seq(from=1996,to=2023,by=1), "")) + 
  theme_bw() +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 15),
    axis.title.x = element_blank()) 

rainfall_plot

rainfall_month <- read.csv("./data/precipitation/Monthly_Rainfall_24update.csv") %>%
  select(year, month, Date, rainfall_mm = total_rainfall_mm) %>%
  filter(year > 2002)

rainfall_month$Date <- as.Date(rainfall_month$Date)
 
rainfall_month$Season <- ifelse(rainfall_month$month > 4 & rainfall_month$month < 9, "Growing Season", NA) #growing season

rainfall_growing <- rainfall_month %>%
  filter(Season == "Growing Season") %>%
  group_by(year) %>%
  summarize(rainfall_mm = sum(rainfall_mm))

#Inputs, Outputs, and Storage Results
Budget_Components <- read.csv("results/main_compile_inputs/Budget_Components.csv") %>%
  select(type=Type, year=Year, solute=Solute, Load_kgyr, Balance) %>%
  filter(solute != "NH4" & solute != "NO3_NO2")

#eelgrass, wwtf plant data 
wwtf <- Budget_Components %>%
  filter(type == "WWTF") %>%
  filter(Balance == "Input") %>%
  filter(solute == "DIN" | solute == "TN") %>%
  mutate(Load_kghayr = Load_kgyr/1700) %>%
  select(year, solute, Load_kghayr) %>%
  pivot_wider(values_from = "Load_kghayr",
              names_from = "solute") 

wwtf$Period <- ifelse(wwtf$year < 2017, "Pre-upgrades",
                      ifelse(wwtf$year > 2020, "Post-upgrades", "Upgrade period"))

wwtf <- wwtf %>%
  select(year, DIN, TN, Period) %>%
  pivot_longer(cols=c(DIN:TN),
               names_to = "variable",
               values_to = "measurement")


wwtfplot <- ggplot(subset(wwtf, variable == "TN"), aes(year, measurement)) + 
  annotate("rect", xmin = 2017.5, xmax = 2020.5, ymin = 0, ymax = 50, fill = "lightgrey", alpha = 0.75) +
  annotate("segment", x = 2017.5, xend = 2020.5, y = 0, yend = 0, color = "black", size = 0.7) +
  annotate("text", x = 2019, y = 2, label = "Upgrade Period", size = 3.5) +
  annotate("segment", x = 2017.5, xend = 2017.5, y = -0.5, yend = 1, color = "black", size = 0.7) +
  annotate("segment", x = 2020.5, xend = 2020.5, y = -0.5, yend = 1, color = "black", linewidth = 0.7) +
  geom_point(size=3) +
  geom_line() +
  xlab("Year") + 
  ylab(expression(TN~Load~(kg~N~ha^-1~year^-1))) +
  scale_x_continuous(limits=c(1996,2023), 
                     breaks=seq(from=1996,to=2023,by=1), 
                     labels=ifelse(seq(from=1996,to=2023,by=1) %% 2 == 0, seq(from=1996,to=2023,by=1), "")) + 
  scale_y_continuous(limits=c(-0.5,50)) +
  theme_bw() +
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=15))

wwtfplot

figure1 <- cowplot::plot_grid(eelgrass_plot, rainfall_plot, wwtfplot, ncol = 1,
          labels = c("a", "b", "c"), 
          align = "v",
          axis = "l",
          label_x = 0.1,   # Adjust horizontal position (1 = right)
          label_y = 0.95,   # Adjust vertical position (1 = top)
          hjust = 0,      
          vjust = 1,
          label_size=15
)
figure1 

ggsave(figure1, filename=paste0("./results/manuscript_figures/Figure1.jpg"), 
       dpi=300, height=11, width = 9, units = "in")

#Calculate Inputs and Outputs
Budget_Components$Identifier <- paste(Budget_Components$year, Budget_Components$solute, sep="_")
  
results <- data.frame(
  Identifier = character(),
  year = numeric(),
  Input = numeric(),
  Output = numeric(),
  stringsAsFactors = FALSE
)

ids <- unique(Budget_Components$Identifier)
for (i in ids) {
  # Subset the data for the current identifier
  df_id <- subset(Budget_Components[Budget_Components$Identifier == i, ])
  print(df_id)
  
  # Check if there are any NA Load_kgyr values
  if (all(!is.na(df_id$Load_kgyr))) {
    # Get the year (assuming all rows have the same year for each identifier)
    year <- unique(df_id$year)
    
    # Sum inputs
    Input <- sum(df_id$Load_kgyr[df_id$Balance == "Input"], na.rm = TRUE)
    
    # Sum outputs
    Output <- sum(df_id$Load_kgyr[df_id$Balance == "Output"], na.rm = TRUE)
  } else {
    # Set Input and Output to NA if any Load_kgyr values are NA
    year <- as.numeric(unique(df_id$year))
    Input <- NA
    Output <- NA
  }
  
  # Add results to the results data frame
  results <- rbind(results, data.frame(Identifier = i, year = year, Input = Input, Output = Output))
}

# Print the results
print(results)

annual_budget <- results %>%
  separate(Identifier, into =c("Year", "Solute"), sep="_") %>%
  select(year, Solute, Input, Output) %>%
  mutate(Storage = Input - Output)

annual_budget$Input_kghayr <- annual_budget$Input / GB_hectares

annual_budget$Output_kghayr <- annual_budget$Output / GB_hectares

annual_budget$Storage_kghayr <- annual_budget$Storage /GB_hectares

annual_budget_clean <- annual_budget %>%
  filter(!is.na(Input_kghayr) & !is.na(Storage_kghayr) & Solute != "TDN")

annual_budget_clean <- left_join(annual_budget_clean, eelgrass)

annual_budget <- annual_budget %>%
  pivot_longer(cols = c(Input, Output, Storage, Input_kghayr, Output_kghayr, Storage_kghayr),
               names_to = "Type",
               values_to = "Value") %>%
  mutate(Type = paste(Type, Solute, sep = "_")) %>%
  select(-Solute) %>%
  pivot_wider(names_from = Type, values_from = Value)

#Combine inputs, outputs, and storage with eelgrass, rainfall, new, chla
annual_norm <- annual_budget %>%
  select(year, Input_kghayr_DIN:Storage_kghayr_DIN, Input_kghayr_DOC:Storage_kghayr_DOC, Input_kghayr_PN:Storage_kghayr_PN,
        Input_kghayr_PO4:Storage_kghayr_PO4, Input_kghayr_TN:Storage_kghayr_TN, Input_kghayr_TSS:Storage_kghayr_TSS)

ecoresponse <- full_join(annual_norm, eelgrass) 
ecoresponse <- full_join(ecoresponse, rainfall)

#keep 2008 forward
ecoresponse <- ecoresponse %>%
  filter(year > 2007) %>%
  select(-GBE_ZM_acres, -GBE_ZM_ha, -GB_ZM_acres)

#Assess normality 
summary_table <- describe(ecoresponse,  type=2, quant=NULL)
summary_table$vars <- row.names(summary_table) 

summary_table <- as.data.frame(summary_table) %>%
  select(-mad, -range) %>%
  mutate_if(is.numeric, function(x) round(x, digits = 2))

summary_table 
library(flextable)
summary_table_ft <- flextable(summary_table)
summary_table_ft


library(corrplot)

# Calculate the correlation matrix
corr_matrix <- ecoresponse 

colnames(corr_matrix) <- gsub("kghayr_", "", colnames(corr_matrix))

corr_matrix <- corr_matrix %>%
  rename(Zostera=GB_ZM_ha) %>%
  rename('Annual Precip' = rainfall_mm)

inputs_df <- corr_matrix[, c("Input_DIN","Input_DOC", "Input_PN", "Input_PO4", "Input_TN", "Input_TSS", "Zostera", "Annual Precip")]
colnames(inputs_df) <- gsub("Input_", "", colnames(inputs_df))


outputs_df <- corr_matrix[, c("Output_DIN","Output_DOC", "Output_PN", "Output_PO4", "Output_TN", "Output_TSS", "Zostera", "Annual Precip")]
colnames(outputs_df) <- gsub("Output_", "", colnames(outputs_df))
storage_df <- corr_matrix[,c("Storage_DIN","Storage_DOC", "Storage_PN", "Storage_PO4", "Storage_TN", "Storage_TSS", "Zostera", "Annual Precip")]
colnames(storage_df) <- gsub("Storage_", "", colnames(storage_df))
corr_input <- cor(inputs_df[,1:8], use = "pairwise.complete.obs")
corr_input

corrplot(corr_input,
         method='ellipse', 
         type='lower', 
         order='alphabet',
         tl.col="black", 
         tl.srt=45,
         diag=FALSE, 
         sig.level=0.05, 
         insig='label_sig')

# Calculate the correlation matrix and obtain p-values
correlation_results <- Hmisc::rcorr((as.matrix(corr_input)))

# Extract correlation matrix
correlation_matrix_full <- correlation_results$r
print(correlation_matrix_full)

# Extract p-values matrix
p_values_matrix <- correlation_results$P

# Display the p-values matrix
print(p_values_matrix)

significance_level <- 0.05

# Create a matrix indicating whether p-values are below the significance level
significant_matrix <- p_values_matrix < significance_level


png("results/manuscript_figures/correlation_plot_figure6a.png", width = 1000, height = 900, res = 100)  # Adjust dimensions and resolution as needed
corrplot(
  corr_input, 
  method='ellipse', 
  type='lower', 
  order='alphabet',
  tl.col="black", 
  tl.srt=45, 
  tl.cex = 1.3,
  diag=FALSE, 
  sig.level=0.05,  
  insig='label_sig',  
  p.mat = p_values_matrix,
  cl.cex = 1.25)
dev.off() 

#Outputs
corr_output <- cor(outputs_df[,1:8], use = "pairwise.complete.obs")
corr_output

corrplot(corr_output,
         method='ellipse', 
         type='lower', 
         order='alphabet',
         tl.col="black", 
         tl.srt=45,
         diag=FALSE, 
         sig.level=0.05, 
         insig='label_sig')

# Calculate the correlation matrix and obtain p-values
correlation_results <- Hmisc::rcorr((as.matrix(corr_output)))

# Extract correlation matrix
correlation_matrix_full <- correlation_results$r
print(correlation_matrix_full)

# Extract p-values matrix
p_values_matrix <- correlation_results$P
p_values_matrix

significance_level <- 0.05

# Create a matrix indicating whether p-values are below the significance level
significant_matrix <- p_values_matrix < significance_level


png("results/manuscript_figures/correlation_plot_figure6b.png", width = 1000, height = 900, res = 100)  # Adjust dimensions and resolution as needed
corrplot(
  corr_output, 
  method='ellipse', 
  type='lower', 
  order='alphabet',
  tl.col="black", 
  tl.srt=45, 
  tl.cex = 1.3,  # 
  diag=FALSE, 
  sig.level=0.05,  # You can adjust the significance level as needed
  insig='label_sig',  # Display non-significant correlations as blank
  p.mat = p_values_matrix,
  cl.cex = 1.25)
dev.off() 

#Storage
corr_storage <- cor(storage_df[,1:8], use = "pairwise.complete.obs")
corr_storage

corrplot(corr_storage,
         method='ellipse', 
         type='lower', 
         order='alphabet',
         tl.col="black", 
         tl.srt=45,
         diag=FALSE, 
         sig.level=0.05, 
         insig='label_sig')

# Calculate the correlation matrix and obtain p-values
correlation_results <- Hmisc::rcorr((as.matrix(corr_storage)))

# Extract correlation matrix
correlation_matrix_full <- correlation_results$r
print(correlation_matrix_full)

# Extract p-values matrix
p_values_matrix <- correlation_results$P

# Display the p-values matrix
print(p_values_matrix)

significance_level <- 0.05

# Create a matrix indicating whether p-values are below the significance level
significant_matrix <- p_values_matrix < significance_level

png("results/manuscript_figures/correlation_plot_figure6c.png", width = 1000, height = 900, res = 100)  # Adjust dimensions and resolution as needed
##Storage Correlation
corrplot(
  corr_storage, 
  method='ellipse', 
  type='lower', 
  order='alphabet',
  tl.col="black", 
  tl.srt=45, 
  tl.cex = 1.3, 
  diag=FALSE, 
  sig.level=0.05,  
  insig='label_sig',  # Display non-significant correlations as blank
  p.mat = p_values_matrix,
  cl.cex = 1.25)
dev.off()


doc.lm <- lm(GB_ZM_ha ~ Input_kghayr_DOC, ecoresponse)
summary(doc.lm) #almost sig

doc.lm.out<- lm(GB_ZM_ha ~ Output_kghayr_DOC, ecoresponse)
summary(doc.lm.out) #sig

po4.in <- lm(GB_ZM_ha ~ Input_kghayr_PO4, ecoresponse)
summary(po4.in) #sig

pn.lm<- lm(GB_ZM_ha ~ Input_kghayr_PN, ecoresponse)
summary(pn.lm) #sig

ggplot(ecoresponse, aes(Input_kghayr_PN, GB_ZM_ha)) + geom_point()

tss.lm<- lm(GB_ZM_ha ~ Input_kghayr_TSS, ecoresponse)
summary(tss.lm) # sig

ggplot(ecoresponse, aes(Input_kghayr_TSS, GB_ZM_ha)) + geom_point()

tss.lm<- lm(GB_ZM_ha ~ Storage_kghayr_TSS, ecoresponse)
summary(tss.lm) 

pn.st.lm<- lm(GB_ZM_ha ~ Storage_kghayr_PN, ecoresponse)
summary(pn.st.lm) # sig

po4.st.lm<- lm(GB_ZM_ha ~ Storage_kghayr_PO4, ecoresponse)
summary(po4.st.lm) # sig


ggplot(ecoresponse, aes(Storage_kghayr_PO4, GB_ZM_ha)) + geom_point()


ggplot(ecoresponse, aes(Storage_kghayr_PN, GB_ZM_ha)) + geom_point()
#______________________________________________________________________
#________________________________________________________________________
#Estimating DIN/TN uptake by eelgrass based on acres of eelgrass each year
#345 kg/ha/year from 34.5 gN/m2/year, converted to kg/m2 to year and then to kg/ha/year. 

#34.5 g/m2/yr * 1kg/1000g = 0.0345 kg/m2/yr
#1 ha = 10,000 m2
#0.0345kg/m2/yr * 10,000m2/ha = 345 kg/ha/yr

Inputs.eel <- eelgrass
Inputs.eel$uptake <- 345 * Inputs.eel$GB_ZM_ha #kg N / ha /year * GB_ZM_ha of eelgrass = kg/year

Inputs.eel$uptake.normalized <- Inputs.eel$uptake / 1700 #GB_ZM_ha GB

mean(Inputs.eel$uptake, na.rm =T) 
pedersen <- mean(Inputs.eel$uptake.normalized, na.rm =T) 
pedersen 
pedersen/132*100 #mean DIN retention 132 kg/ha/yr in Great Bay



#2.62 g/m2/year to 26.2 kg/ha/year
Inputs.eel$uptake.aoki <- 26.2 * Inputs.eel$GB_ZM_ha
Inputs.eel$uptake.normalized.aoki <- Inputs.eel$uptake.aoki / 1700 #GB_ZM_ha GB

aoki <- mean(Inputs.eel$uptake.normalized.aoki, na.rm =T) 
aoki
aoki/132*100
mean(Inputs.eel$uptake.aoki) #GB_ZM_ha of GB

ggplot(Inputs.eel, aes(year, uptake.normalized.aoki)) + geom_point() + theme_cowplot() +
  ylab("Eelgrass N uptake kg-N/ha/year") +
  geom_hline(aes(yintercept=mean(Inputs.eel$uptake.normalized.aoki,na.rm =T)))

ggplot(Inputs.eel, aes(year, uptake.normalized)) + geom_point() + theme_cowplot() +
  ylab("Eelgrass N uptake kg-N/ha/year") +
  geom_hline(aes(yintercept=mean(Inputs.eel$uptake.normalized,na.rm =T)))


#_____________________________________________________________________________________________
#______________________________________________________________________________________________

#Estimate dentrification based on Aoki et al. 2020

#0.62 g N per m^2 per year in eelgrass bed is denitrified

dnf.kg.ha.year <- conv_unit(0.62, "g", "kg") * 10000 
#6.2 kg/ha/year

inputs.dnf <- ecoresponse %>%
  select(year, Input_kghayr_DOC, Input_kghayr_DIN, Input_kghayr_TN, GB_ZM_ha)

#multiply y GB_ZM_ha of eelgrass then divide by GB to normalize

inputs.dnf$dnf.eelgrass.kg.ha.year <- dnf.kg.ha.year * inputs.dnf$GB_ZM_ha / GB_hectares

mean(inputs.dnf$dnf.eelgrass.kg.ha.year, na.rm=T)
sd(inputs.dnf$dnf.eelgrass.kg.ha.year, na.rm=T)

#normalize inputs and outputs by Great Bay area
library(measurements)

storage.norm <- Storage[,1:10] / GB_acres

storage.norm$year <- storage.norm$year * GB_acres

plot(storage.norm)

#Estimation of Nitrogen Fixation
#mg N/m2/day
fixation <- 4.2
fixation_kg_m2_year <- (conv_unit(fixation, "mg", "kg")*365)

fixation_kg_ha_year <- fixation_kg_m2_year * 10000

fixation.df <- Inputs.eel

fixation.df$fixation.kg.year <- fixation.df$GB_ZM_ha * fixation_kg_ha_year
