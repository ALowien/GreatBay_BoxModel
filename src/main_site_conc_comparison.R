#main_site_conc_comparison.R

#Author: Anna Mikulis, University of New Hampshire
#Last Updated: 2/25/2024

#Purpose: 
#Summarizing annual concentrations (flow-weighted when possible) across inputs to Great Bay
#Creates summary data for Table

#Load required packages.
Packages <- c("readxl", "dplyr", "lubridate", "moments", "measurements", "stats", "ggplot2", 
              "cowplot", "tidyr")

lapply(Packages, library, character.only = TRUE)

# Import Tributary Flow Weighted Concentrations from main_load_calc results file
subdir <- "results/main_load_calc/FWC"
files <- list.files(path = subdir, pattern = ".csv", full.names = T)  
df.list <- lapply(files, read.csv)

df <- bind_rows(df.list)
colnames(df)

df <- df %>%
  select(STATION_ID, CY, PO4_MGL = FW_PO4, PN_MGL = FW_PN, TN_MGL= FW_TN, NH4_MGL = FW_NH4, NO3_MGL = FW_NO3_NO2, DIN_MGL= FW_DIN, DON_MGL = FW_DON, TDN_MGL = FW_TDN, DOC_MGL = FW_DOC, TSS_MGL = FW_TSS)

df$Sample <- "River"

#Adams Point high and low tide
ap_conc <- read.csv("data/emd/surfacewaterchemistry_conc.csv")
ap_conc$START_DATE <- as.Date(ap_conc$START_DATE)
ap_conc$CY <- year(ap_conc$START_DATE)

ap_conc <- ap_conc %>%
  select(STATION_ID, CY, PO4_MGL, PN_MGL, TN_MGL, DON_MGL, NH4_MGL, NO3_MGL = NO3_NO2_MGL, DIN_MGL, TDN_MGL, DOC_MGL, TSS_MGL) %>%
  filter(STATION_ID == "GRBAPL" | STATION_ID == "GRBAPH")

ap_conc$Sample <- ap_conc$STATION_ID

ap_conc <- ap_conc %>%
  group_by(STATION_ID, Sample, CY) %>%
  summarize(across(PO4_MGL:TSS_MGL, \(x) mean(x, na.rm=T)))

ap_conc$DOC_MGL <- ifelse(ap_conc$CY == 2009, NA, ap_conc$DOC_MGL)

#precipitation (fully QAQCd and method detection limits fixed)
Precipitation <- read.csv("results/main_precipitation_format/precip_fwc.csv")

colnames(Precipitation)

Precip <- Precipitation %>%
  select(CY, FW_TDN_MGL, FW_DOC_MGL, FW_PO4_UGL, FW_NH4_UGL, FW_NO3_MGL, FW_DIN_MGL, FW_DON_MGL)  

Precip$FW_TN_MGL <- Precip$FW_TDN_MGL #assume TN = TDN for precip 
  
Precip$FW_PO4_MGL <- conv_unit(Precip$FW_PO4_UGL, "ug", "mg")

Precip$FW_NH4_MGL <- conv_unit(Precip$FW_NH4_UGL, "ug", "mg")

Precip <- Precip %>% select(-FW_PO4_UGL, -FW_NH4_UGL)

Precip$STATION_ID <- "Precipitation"
Precip$Sample <- "Precipitation"
 #Add a PN and TSS column that are zero to Precip dataframe
Precip <- Precip %>%
  mutate(FW_PN_MGL = 0,
         FW_TSS_MGL = 0)

colnames(Precip)

Precip <- Precip %>%
  select(STATION_ID, Sample, CY, FW_TDN_MGL:FW_NH4_MGL, FW_PN_MGL, FW_TSS_MGL) %>%
  rename(PO4_MGL = FW_PO4_MGL, PN_MGL = FW_PN_MGL, TN_MGL= FW_TN_MGL, TDN_MGL = FW_TDN_MGL, NH4_MGL = FW_NH4_MGL, NO3_MGL = FW_NO3_MGL,DIN_MGL= FW_DIN_MGL, DON_MGL = FW_DON_MGL, DOC_MGL = FW_DOC_MGL, TSS_MGL = FW_TSS_MGL)

#Wastewater
wwtf <- read.csv("results/main_wwtf_loads/wwtf_concentrations.csv")

colnames(wwtf)

wwtf_summary <- wwtf %>%
  select(WWTF, year, month, Final_TN_mgL, TSS_mgL, NO3_NO2_mgL, NH4_mgL) %>%
  filter(year < 2024) %>%
  group_by(WWTF, year) %>%
  summarize(TN_mean_mgL = mean(Final_TN_mgL, na.rm=T),
            TN_sd_mgL = sd(Final_TN_mgL, na.rm=T),
            TN_count= sum(!is.na(Final_TN_mgL)),
            NO3_MGL = mean(NO3_NO2_mgL, na.rm=T),
            NO3_mgL_sd = sd(NO3_NO2_mgL, na.rm=T),
            NO3_count= sum(!is.na(NO3_NO2_mgL)),
            NH4_MGL = mean(NH4_mgL, na.rm=T),
            NH4_mgL_sd = sd(NH4_mgL, na.rm=T),
            NH4_count= sum(!is.na(NH4_mgL)),
            TSS_mgL = mean(TSS_mgL, na.rm = T),
            TSS_mgL_sd = sd(TSS_mgL, na.rm = T))

head(wwtf_summary)


wwtf_summary$Sample <- "WWTF"

wwtf_summary <- wwtf_summary %>%
  rename(CY = year,
         TN_MGL = TN_mean_mgL,
         STATION_ID = WWTF,
         TSS_MGL = TSS_mgL) %>%
  select(STATION_ID, Sample, CY, TN_MGL, TSS_MGL, NO3_MGL, NH4_MGL)

wwtf_summary$TN_MGL <- ifelse(wwtf_summary$STATION_ID == "Exeter" & wwtf_summary$CY == 2013, NA, wwtf_summary$TN_MGL) #only 6 values and we used the report values for 2013, so misleading to include here


#Discrete samples of effluent for DOC, PO4, etc. 
LIT_WWTF <- read.csv("./data/wwtf/literature_wwtf_concentrations.csv")
head(LIT_WWTF)
LIT_WWTF$PO4_MGL <- measurements::conv_unit(LIT_WWTF$PO4_UGL, "ug", "mg")
LIT_WWTF$Date <- as.Date(LIT_WWTF$Date, format="%m/%d/%Y")
LIT_WWTF$CY <- year(LIT_WWTF$Date)

LIT_WWTF_filtered <- LIT_WWTF %>%
  select(WWTF, CY, TDN_MGL, DOC_MGL, PO4_MGL, TN_MGL) %>%
  rename(STATION_ID = WWTF) 

LIT_WWTF_filtered$Sample <- "WWTF"

mean(LIT_WWTF_filtered$PO4_MGL, na.rm=T)
sd(LIT_WWTF_filtered$PO4_MGL, na.rm=T)

avg_wwtf <- LIT_WWTF_filtered %>%
  select(STATION_ID,TDN_MGL:TN_MGL) %>%
  pivot_longer(cols=c(TDN_MGL:TN_MGL),
               names_to="Solute", 
               values_to = "Concentration") %>%
  filter(!is.na(Concentration))

LIT_WWTF_filtered <- LIT_WWTF_filtered %>%
  select(STATION_ID, Sample, CY, TDN_MGL:TN_MGL) %>%
  group_by(STATION_ID, Sample, CY) %>%
  summarize(across(TDN_MGL:TN_MGL, \(x) mean(x, na.rm=T)))

sd(LIT_WWTF_filtered$PO4_MGL, na.rm=T)
 
wwtf_summary2 <- full_join(wwtf_summary, LIT_WWTF_filtered)

#Table for Manuscript
#Volume-weighted freshwater input concentrations 
concentrations_table <- full_join(df, ap_conc)
concentrations_table <- full_join(concentrations_table, Precip)
concentrations_table <- full_join(concentrations_table, wwtf_summary2)

concentrations_table$PO4_UGL <- measurements::conv_unit(concentrations_table$PO4_MGL, "mg", "ug")

concentrations_table <- concentrations_table %>%
  select(Sample, CY, STATION_ID, PO4_UGL, PN_MGL:TSS_MGL)


summary_mean <- concentrations_table %>%
  select(Sample, CY, STATION_ID, PO4_UGL:TSS_MGL) %>%
  group_by(Sample) %>%
  summarize(across(PO4_UGL:TSS_MGL, \(x) mean(x, na.rm=T)))

summary_mean_piv <- summary_mean %>%
  pivot_longer(cols=c(PO4_UGL:TSS_MGL), names_to = "Parameter", values_to="Mean") %>%
  pivot_wider(names_from = Sample,
              values_from = Mean)

summary_mean_piv[,2:6] <-  signif(summary_mean_piv[,2:6],2)

summary_mean_piv

summary_sd<- concentrations_table %>%
  select(Sample, CY, STATION_ID, PO4_UGL:TSS_MGL) %>%
  group_by(Sample) %>%
  summarize(across(PO4_UGL:TSS_MGL, \(x) sd(x, na.rm=T)))


summary_sd_piv <- summary_sd %>%
  pivot_longer(cols=c(PO4_UGL:TSS_MGL), names_to = "Parameter", values_to="SD") %>%
  pivot_wider(names_from = Sample,
              values_from = SD)

summary_sd_piv[,2:6] <- signif(summary_sd_piv[,2:6],2)

summary_sd_piv

summary_tally <- concentrations_table %>%
  select(Sample, CY, STATION_ID, PO4_UGL:TSS_MGL) %>%
  group_by(Sample) %>%
  summarize(across(PO4_UGL:TSS_MGL, ~ sum(!is.na(.))))


summary_tally_piv <- summary_tally %>%
  pivot_longer(cols=c(PO4_UGL:TSS_MGL), names_to = "Parameter", values_to="Count") %>%
  pivot_wider(names_from = Sample,
              values_from = Count)

summary_tally_piv 

library(flextable)
combined_data <- summary_mean_piv %>%
  pivot_longer(cols = -Parameter, names_to = "Source", values_to = "Mean") %>%
  left_join(pivot_longer(summary_sd_piv, cols = -Parameter, names_to = "Source", values_to = "SD"),
            by = c("Parameter", "Source")) %>%
  left_join(pivot_longer(summary_tally_piv, cols = -Parameter, names_to = "Source", values_to = "Count"),
            by = c("Parameter", "Source")) %>%
  mutate(Result = paste0(round(Mean, 3), " ± ", round(SD, 3), " (", Count, ")")) %>%
  select(Parameter, Source, Result) %>%
  pivot_wider(names_from = Source, values_from = Result)

custom_order <- c("DOC_MGL","PO4_UGL", "NH4_MGL", "NO3_MGL", "DIN_MGL", "DON_MGL", "TDN_MGL", "PN_MGL", "TN_MGL","TSS_MGL")
combined_data$name <- factor(combined_data$Parameter, levels = custom_order)
combined_data <- combined_data %>%
  arrange(name)
write.csv(combined_data, file=paste0("./results/manuscript_figures/tables/avg_concentrations.csv")) #includes flow-weighted river data

# Create the flextable
flex_table <- flextable(combined_data)

# Display the flextable
flex_table %>% autofit()


#split wwtf into pre and post ugrades, put split 1t 2017
wwtf_summary2$upgrade <- ifelse(wwtf_summary2$CY < 2020, "pre",  "post")

#Table 1 WWTF pre/post
wwtf_table1_split <- wwtf_summary2 %>%
  pivot_longer(cols=c(TN_MGL:PO4_MGL),
               names_to = "solute",
               values_to = "concentration") %>%
  group_by(upgrade, solute) %>%
  summarize(
    mean = mean(concentration, na.rm = TRUE),
    sd = sd(concentration, na.rm = TRUE),
    n = sum(!is.na(concentration)),
    .groups = "drop") %>%
  mutate(summary = paste0(round(mean, 2), " ± ", round(sd, 2), " (n=", n, ")")) %>% # Combine mean, sd, and n into a single summary column
  pivot_wider(names_from = upgrade, values_from = summary) %>%   # Pivot wider to create columns for each upgrade (post, pre)
  ungroup() %>%
   select(solute, pre, post)

wwtf_table1_final <- wwtf_table1_split %>%
  # Group by solute to handle duplicates
  group_by(solute) %>%
  # Fill NA values in the pre and post columns using the non-NA values
  summarize(
    pre = coalesce(pre[!is.na(pre)], pre[is.na(pre)]),
    post = coalesce(post[!is.na(post)], post[is.na(post)]),
    .groups = "drop"
  )  

write.csv(wwtf_table1_final, file = "./results/manuscript_figures/tables/wwtf_split_conc.csv")

#Statistics by Source and Solute
library(agricolae) # For skewness and kurtosis

normality <- concentrations_table %>%
  select(Sample, CY, STATION_ID, PO4_UGL:TSS_MGL) %>%
  pivot_longer(cols = PO4_UGL:TSS_MGL, names_to = "Solute", values_to = "Value") %>%
  group_by(Sample, Solute) %>%
  summarize(
    Skewness = agricolae::skewness(na.omit(Value)),
    Kurtosis = agricolae::kurtosis(na.omit(Value)),
    COV = sd(Value, na.rm = TRUE) / mean(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = Skewness:COV, names_to = "Metric", values_to = "Value")

normality 

#Adams Point - high vs low tide
#everything but PN looks pretty good for normality 
#TDN and PN failed shapiro wilk

#_____________________________________________________
lapply(ap_conc[,4:13], skewness)
lapply(ap_conc[,4:13], kurtosis)
lapply(ap_conc[,4:13], shapiro.test)

ap_conc$PN_MGL_log <- log(ap_conc$PN_MGL)
lapply(ap_conc[,4:14], shapiro.test)
lapply(ap_conc[,4:14], skewness) #0.94 to 0.36 skewness PN
lapply(ap_conc[,4:14], kurtosis) #3.43 to 2.67 kurtosis PN

ap_conc$STATION_ID <- as.factor(ap_conc$STATION_ID)

leveneTest(PN_MGL_log ~ STATION_ID, data=ap_conc)
lapply(ap_conc[,14], skewness) #0.94 to 0.36 skewness PN
lapply(ap_conc[,14], kurtosis)

leveneTest(DOC_MGL ~ STATION_ID, data=ap_conc)
leveneTest(TSS_MGL ~ STATION_ID, data=ap_conc)

t <- t.test(ap_conc$DOC_MGL[ap_conc$STATION_ID == "GRBAPH"], 
            ap_conc$DOC_MGL[ap_conc$STATION_ID == "GRBAPL"], 
            paired = TRUE)
t


solute_columns <- colnames(ap_conc)[c(4:11, 12,13, 14)]

# Apply paired t-test for each column
pt_test_results <- lapply(solute_columns, function(column_name) {
  # Perform the paired t-test
  t_result <- t.test(
    ap_conc[[column_name]][ap_conc$STATION_ID == "GRBAPH"], 
    ap_conc[[column_name]][ap_conc$STATION_ID == "GRBAPL"], 
    paired = TRUE
  )
  
  # Extract key results
  list(
    variable = column_name,
    t_statistic = t_result$statistic,
    df = t_result$parameter,  # Degrees of freedom
    p_value = t_result$p.value,
    mean_diff = t_result$estimate[1], # Difference in means
    conf_int_lower = t_result$conf.int[1],  # Lower bound of the confidence interval
    conf_int_upper = t_result$conf.int[2]  # Upper bound of the confidence interval
  )
})

# Check the results
pt_test_results
pt_test_summary <- do.call(rbind, lapply(pt_test_results, function(result) {
  data.frame(
    Variable = result$variable,
    T_Statistic = result$t_statistic,
    DF = result$df,
    P_Value = result$p_value,
    Mean_Difference = result$mean_diff,
    Conf_Int_Lower = result$conf_int_lower,
    Conf_Int_Upper = result$conf_int_upper
  )
}))

# Print the summary table
print(pt_test_summary)

#calculate % diff bt/ high and low tide
summary_mean_piv
per <- summary_mean_piv %>%
  mutate(Percent_Diff = ((GRBAPL - GRBAPH) / GRBAPL) * 100)


ggplot(ap_conc, aes(STATION_ID, TSS_MGL)) + 
  geom_boxplot() +
  geom_point()




wilcox.test(ap_conc$TN_MGL ~ ap_conc$STATION_ID)

ggplot(ap_conc, aes(STATION_ID, TN_MGL)) + geom_boxplot() + theme_cowplot() +
  stat_summary(fun=mean, geom="point", shape=20, size=6, color="blue") +
  theme(axis.title.x = element_blank())

wilcox.test(ap_conc$DIN_MGL ~ ap_conc$STATION_ID)

wilcox.test(ap_conc$PN_MGL ~ ap_conc$STATION_ID)

wilcox.test(ap_conc$TSS_MGL ~ ap_conc$STATION_ID)

wilcox.test(ap_conc$DOC_MGL ~ ap_conc$STATION_ID)

wilcox.test(ap_conc$PO4_MGL ~ ap_conc$STATION_ID)


ap_conc_avg <- ap_conc %>%
  select(STATION_ID, CY, PO4_MGL, DOC_MGL, DIN_MGL, TN_MGL, PN_MGL) %>%
  group_by(STATION_ID, CY) %>%
  summarize(across(PO4_MGL:PN_MGL, mean, na.rm=T))

#percent difference in TN b/t high and low tide
abs(ap_conc_avg[2,5] - ap_conc_avg[1,5]) / ap_conc_avg[1,5] * 100
#percent difference in PN b/t high and low tide
abs(ap_conc_avg[2,6] - ap_conc_avg[1,6]) / ap_conc_avg[1,6] * 100





#Supplemental Table 1, averages by treatmentplant.
wwtf_conc <- wwtf %>%
  select(WWTF, Final_TN_mgL, TSS_mgL) %>%
  rename(TN_MGL = Final_TN_mgL,
         TSS_MGL = TSS_mgL) %>%
  pivot_longer(cols = c(TN_MGL:TSS_MGL),
               names_to="Parameter",
               values_to = "Concentration") %>%
  group_by(WWTF, Parameter) %>%
  summarize(Mean = signif(mean(Concentration, na.rm=T), 3),
            SD = signif(sd(Concentration, na.rm=T), 3),
            Count = sum(!is.na(Concentration)))

wwtf_s1 <- wwtf_conc %>%
  rename(Source = WWTF) %>%
  mutate(Result = paste0(round(Mean, 3), " ± ", round(SD, 3), " (", Count, ")")) %>%
  select(Parameter, Source, Result) %>%
  pivot_wider(names_from = Source, values_from = Result)
wwtf_s1

newfields <- wwtf %>%
  filter(WWTF == "Newfields") %>%
  filter(year > 2020) %>%
  filter(Final_TN_mgL_method == "Average")

summary(newfields)

mean(newfields$Final_TN_mgL)
sd(newfields$Final_TN_mgL)
(sum(!is.na(newfields$Final_TN_mgL)))


#Average from Literature
lit_avg <- LIT_WWTF %>%
  select(-Source) %>%
  pivot_longer(cols = c(TDN_MGL:TN_MGL),
               names_to="Parameter",
               values_to = "Concentration") %>%
  group_by(WWTF, Parameter) %>%
  summarize(Mean = signif(mean(Concentration, na.rm=T), 2),
            SD = signif(sd(Concentration, na.rm=T), 2),
            Count = sum(!is.na(Concentration)))
