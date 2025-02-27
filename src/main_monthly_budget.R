#main_monthly_budget.R

#Author: Anna Mikulis, University of New Hampshire
#Last updated: 2/18/2025
#Purpose: Compile monthly fluxes for monthly budget calculations

Packages <- c("readr", "ggplot2", "measurements", "plotly", "lubridate", "cowplot", "viridis", 
              "agricolae", "tibble", "readxl", "dplyr", "tidyr", "stats", "car")

lapply(Packages, library, character.only = TRUE)
#River Inputs; NHDES Head-of-Tide Stations
#Annual Load Estimates
#Values in kg X solute/month
lmp_monthly <- read.csv("results/main_load_calc/FW_Loads/LR_MLoads_kg_month.csv") %>%
  select(Year=CY, Month, PO4=FW_PO4, PN=FW_PN, TN=FW_TN, TDN=FW_TDN, NH4=FW_NH4, NO3_NO2 = FW_NO3_NO2,
         DIN=FW_DIN, DON=FW_DON, DOC=FW_DOC, TSS=FW_TSS) %>%
  mutate(Site = "LMP")

sqr_monthly <- read.csv("results/main_load_calc/FW_Loads/SQR_MLoads_kg_month.csv") %>%
  select(Year=CY, Month, PO4=FW_PO4, PN=FW_PN, TN=FW_TN, TDN=FW_TDN, NH4=FW_NH4, NO3_NO2 = FW_NO3_NO2,
         DIN=FW_DIN, DON=FW_DON, DOC=FW_DOC, TSS=FW_TSS) %>%
  mutate(Site = "SQR")

wnc_monthly <- read.csv("results/main_load_calc/FW_Loads/WNC_MLoads_kg_month.csv") %>%
  select(Year=CY, Month, PO4=FW_PO4, PN=FW_PN, TN=FW_TN, TDN=FW_TDN, NH4=FW_NH4, NO3_NO2 = FW_NO3_NO2,
         DIN=FW_DIN, DON=FW_DON, DOC=FW_DOC, TSS=FW_TSS) %>%
  mutate(Site = "WNC")

#Precipitation Monthly Loads (units kg per month)
precip_mloads <- read.csv("results/main_precipitation_format/month_precip_loads24.csv") %>%
  select(Year=CY, Month, Rainfall_mm, TDN=TDN_kg_month, DOC=DOC_kg_month, NO3_NO2=NO3_kg_month, NH4=NH4_kg_month,
         PO4=PO4_kg_month, DIN=DIN_kg_month, DON=DON_kg_month, TN=TN_kg_month) %>%
  mutate(Site = "Rainfall") %>%
  mutate(PN = 0,
         TSS= 0)

#WWTF Monthly Loads
wwtf_mloads <- read.csv("results/main_wwtf_loads/redone/monthlyloads.csv") %>%
  select(Site=WWTF, Year = year, Month = month,
          DIN=DIN_kgmonth, TN=TN_kgmonth, PO4 = PO4_kgmonth, DOC=DOC_kgmonth,
         TSS=TSS_kgmonth) %>%
  mutate(PN= 0)

#Adams Point
ap <- read.csv("results/main_estuarine_load_calc/AP_Monthly_Flux_kgmonth.csv") %>%
  select(Site = STATION_ID, Year, Month, PO4:NH4, NO3_NO2=NO32, DIN, DON, DOC, TSS) %>%
  filter(Year < 2024 & Year >2007) %>%
  filter(!is.na(Site))
#Runoff Estimates
runoff <- read.csv("results/main_runoff/runoff_estimate_kgmonth.csv") %>%
  select(Year=CY, Month, PO4=FW_PO4, PN=FW_PN, TN=FW_TN, TDN=FW_TDN, NH4=FW_NH4, NO3_NO2 = FW_NO3_NO2,
         DIN=FW_DIN, DON=FW_DON, DOC=FW_DOC, TSS=FW_TSS) %>%
  mutate(Site = "Runoff")

#Combine the dataframes into one big dataframe
monthly <- full_join(ap, runoff)
monthly <- full_join(monthly, lmp_monthly)
monthly <- full_join(monthly, sqr_monthly)
monthly <- full_join(monthly, wnc_monthly)
monthly <- full_join(monthly, precip_mloads)
monthly <- full_join(monthly, wwtf_mloads)

#groundwater 

# Create the groundwater dataframe
groundwater <- expand_grid(
  Site = "Groundwater",
  year = 2008:2023,
  month = 1:12) %>%
  mutate(TN = signif(6800 / 12 ,2), 
        DIN = signif(6800 / 12 ,2))

monthly <- full_join(monthly,groundwater)

unique(monthly$Site)

#Define input and outputs here
monthly$Component <- ifelse(monthly$Site == "GRBAPL", "Output" ,"Input")

#Make a new column to enable grouping
monthly$Type <- ifelse(monthly$Site == "LMP", "Riverine", ifelse(
  monthly$Site == "SQR", "Riverine", ifelse(
    monthly$Site == "WNC", "Riverine", ifelse(
      monthly$Site == "Exeter", "WWTF", ifelse(
        monthly$Site == "Newmarket", "WWTF", ifelse(
          monthly$Site == "GRBAPH", "APH", ifelse(
            monthly$Site == "GRBAPL", "APL", ifelse(
              monthly$Site == "Runoff", "Runoff", ifelse(
                monthly$Site == "Newfields", "WWTF", ifelse(
                  monthly$Site == "Rainfall", "Precip", ifelse(
                    monthly$Site == "Groundwater", "Groundwater", NA) ))))))))))

monthly <- monthly  %>%
  select(Site, Type, Year, Month, Component, PO4:TSS)

GB_hectares <- 1700

monthly_budget <- monthly %>%
  filter(Year < 2024) %>%
  group_by(Year, Month, Component) %>%
  summarize(across(PO4:TSS, ~ sum(., na.rm = FALSE), .names = "{.col}"))

ggplot(monthly_budget, aes(Year, DIN)) + geom_point() +
  facet_wrap(~Component)

monthly_final <- monthly_budget %>%
  # Separate data into input and output rows
  group_by(Year, Month) %>%
  # Pivot to get separate columns for input and output of each variable
  pivot_wider(names_from = Component, values_from = c(PO4, TN, PN, TDN, NH4, NO3_NO2, DIN, DON, DOC, TSS)) %>%
  # Create storage columns by subtracting output from input
  mutate(
    PO4_storage = PO4_Input - PO4_Output,
    TN_storage = TN_Input - TN_Output,
    PN_storage = PN_Input - PN_Output,
    TDN_storage = TDN_Input - TDN_Output,
    NH4_storage = NH4_Input - NH4_Output,
    NO3_NO2_storage = NO3_NO2_Input - NO3_NO2_Output,
    DIN_storage = DIN_Input - DIN_Output,
    DON_storage = DON_Input - DON_Output,
    DOC_storage = DOC_Input - DOC_Output,
    TSS_storage = TSS_Input - TSS_Output
  ) %>%
  # Reshape the data so that we have input, output, and storage rows for each variable
  pivot_longer(cols = starts_with("PO4") | starts_with("TN") | starts_with("PN") | starts_with("TDN") | 
                 starts_with("NH4") | starts_with("NO3_NO2") | starts_with("DIN") | starts_with("DON") | 
                 starts_with("DOC") | starts_with("TSS"),
               names_to = c(".value", "type"), names_pattern = "(.*)_(.*)") %>%
  # Arrange by Year and Month and type
  arrange(Year, Month, type)

# View the result
head(monthly_final)

monthly_final <- monthly_final %>%
  pivot_longer(cols = c(PO4:TSS), names_to = "Solute", values_to="Load_kgmonth")

monthly_final$Load_kghamonth <- monthly_final$Load_kgmonth / GB_hectares

#monthly inputs
monthly_inputs <- monthly_final %>%
  filter(type == "Input") %>%
  mutate(Month = as.factor(Month)) %>%
  filter(Solute %in% c("DIN", "DOC", "PN","PO4", "TN", "TSS"))

ggplot(monthly_inputs, aes(Month, Load_kghamonth)) + 
  geom_boxplot(aes(group=Month)) + geom_point() +
  facet_wrap(~Solute, scales = "free_y")


#### PO4 Monthly Budget ####
PO4_Budget <- monthly %>%
  select(Type, Year, Month, Component, PO4) %>%
  filter(Year <2024) 

PO4_Budget_Final <- PO4_Budget %>%
  group_by(Year, Month, Component) %>%
  summarize(PO4_Load =sum(PO4, na.rm=F)) %>%
  pivot_wider(names_from = "Component",
              values_from = "PO4_Load")

#Filter out a row if input or output is na
PO4_Budget_Final <- PO4_Budget_Final %>%
  filter(!is.na(Input)) %>%
  filter(!is.na(Output))

PO4_Budget_Final$Storage <- PO4_Budget_Final$Input - PO4_Budget_Final$Output
PO4_Budget_Final$Storage_normalized <- PO4_Budget_Final$Storage / GB_hectares 

PO4_Budget_Final$Input_normalized <- PO4_Budget_Final$Input / GB_hectares
PO4_Budget_Final$Output_normalized <- PO4_Budget_Final$Output / GB_hectares

PO4_Budget_Final_long <- PO4_Budget_Final %>%
  select(Year, Month, Storage_normalized:Output_normalized) %>%
  pivot_longer(cols=c(Storage_normalized:Output_normalized),
               names_to="Component",
               values_to = "PO4_kghamonth")

#Assess skewness and kurtosis
apply(PO4_Budget_Final, 2, skewness)
apply(PO4_Budget_Final, 2, kurtosis)

PO4.budget.final.colmean<-apply(PO4_Budget_Final, 2, mean)
print(PO4.budget.final.colmean)
PO4.budget.final.colsd<-apply(PO4_Budget_Final, 2, sd)
PO4.budget.final.COV<-PO4.budget.final.colsd/PO4.budget.final.colmean
PO4.budget.final.COV

PO4_Budget_Final$Month <- as.factor(PO4_Budget_Final$Month)
PO4_Budget_Final_long$Month <- as.factor(PO4_Budget_Final_long$Month)

po4.model <- lm(PO4_Budget_Final$Storage_normalized ~ PO4_Budget_Final$Month)
po4.anova <- aov(po4.model, data=PO4_Budget_Final)

summary(po4.anova) #no difference


ggplot(PO4_Budget_Final, aes(x=Month, Storage_normalized)) + geom_boxplot() + 
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits=c(-20,60), breaks=seq(from=-20, to=60, by=10)) +
  ylab(expression(PO[4]~Storage~(kg-P~ha^{-1}~month^{-1}))) 

#PO4 input and output anova,tukey test
po4.i.aov <- aov(Input_normalized ~ Month, data =PO4_Budget_Final)
summary(po4.i.aov)

po4.i.tukey <- HSD.test(po4.i.aov, "Month", group=T)
summary(po4.i.tukey)

print(po4.i.tukey$groups)
plot(po4.i.tukey, las =2)
str(po4.i.tukey)

po4.i.g <- po4.i.tukey$groups
library(tibble)
po4.i.g <- rownames_to_column(po4.i.g, "Month")

po4.o.aov <- aov(Output_normalized ~ Month, data =PO4_Budget_Final)
summary(po4.o.aov)

po4.o.tukey <- HSD.test(po4.o.aov, "Month", group=T)
summary(po4.o.tukey)

print(po4.o.tukey$groups)
plot(po4.o.tukey, las =2)
str(po4.o.tukey)

po4.o.g <- po4.o.tukey$groups

po4.o.g <- rownames_to_column(po4.o.g, "Month")


PO4.input <- ggplot(subset(PO4_Budget_Final_long, Component == "Input_normalized"), 
                    aes(x=Month, y=PO4_kghamonth, color=Component)) + 
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", shape=19, size=3.5, position=position_dodge(width=0.6)) +
  theme_cowplot() +
  scale_color_manual(values = c("darkslateblue"), labels=c("Input")) +
  ylab(expression(PO[4]~Yields~(kg-P~ha^{-1}~month^{-1})))  +
  annotate("text", x = po4.i.g$Month, y = -0.05, label = po4.i.g$groups, size = 5, color="darkslateblue")+
  theme(legend.position = "none",
        axis.title.x = element_blank())
PO4.input

PO4.output <- ggplot(subset(PO4_Budget_Final_long, Component == "Output_normalized"), 
                     aes(x=Month, y=PO4_kghamonth, color=Component)) + 
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", shape=19, size=3.5, position=position_dodge(width=0.6)) +
  theme_cowplot() +
  scale_color_manual(values = c("darkslategray3"), labels=c("Output")) +
  ylab(expression(PO[4]~Yields~(kg-P~ha^{-1}~month^{-1})))  +
  annotate("text", x = po4.o.g$Month, y = 0, label = po4.o.g$groups, size = 5, color="darkslategray3") +
  theme(legend.position = "none",
        axis.title.x = element_blank()) 
PO4.output

### end PO4 Monthly Budget ###

#Create a freshwater inputs N:P graph and saltwater output N:P graph
#PO4 Monthly Freshwater Inputs
PO4_FW_Budget <- monthly %>%
  select(Type, Year, Month, Component, PO4) %>%
  filter(Year <2024) %>%
  filter(Component == "Input") %>%
  filter(Type != "APH")

PO4_FW_Budget <- PO4_FW_Budget %>%
  group_by(Year, Month, Component) %>%
  summarize(PO4_Load =sum(PO4, na.rm=F)) %>%
  pivot_wider(names_from = "Component",
              values_from = "PO4_Load")

#Create a freshwater inputs N:P graph and saltwater output N:P graph
#DIN Monthly Freshwater Inputs
DIN_FW_Budget <- monthly %>%
  select(Type, Year, Month, Component, DIN) %>%
  filter(Year <2024) %>%
  filter(Component == "Input") %>%
  filter(Type != "APH")

DIN_FW_Budget <- DIN_FW_Budget %>%
  group_by(Year, Month, Component) %>%
  summarize(DIN_Load =sum(DIN, na.rm=F)) %>%
  pivot_wider(names_from = "Component",
              values_from = "DIN_Load")

#Combine DIN and PO4 into one data frame
#FW stands for freshwater here
DIN_FW_Final <- DIN_FW_Budget %>%
  select(Year, Month, DIN.Input = Input)

ggplot(DIN_FW_Final, aes(Month, DIN.Input, color=Year)) + geom_point() + scale_x_continuous(breaks=seq(from=0,to=12,by=1))

PO4_FW_Final <- PO4_FW_Budget %>%
  select(Year, Month, PO4.Input = Input)

ggplot(PO4_FW_Final, aes(Month, PO4.Input, color=Year)) + geom_point() + scale_x_continuous(breaks=seq(from=0,to=12,by=1))

din.po4.fresh <- full_join(DIN_FW_Final, PO4_FW_Final)

din.po4.fresh <- din.po4.fresh %>% filter(!is.na(DIN.Input) & !is.na(PO4.Input))

din.po4.fresh$DIN.Input.molar <- din.po4.fresh$DIN.Input * 1000 /14.0067
din.po4.fresh$PO4.Input.molar <- din.po4.fresh$PO4.Input * 1000 /30.9738

din.po4.fresh$mol.ratio <- din.po4.fresh$DIN.Input.molar / din.po4.fresh$PO4.Input.molar

din.po4.fresh$Month <- as.factor(din.po4.fresh$Month)

#The ANOVA 
np_freshwater_mod<-lm(mol.ratio ~ Month, data=din.po4.fresh)
anova(np_freshwater_mod) #significant ANOVA
#TESTING ASSUMPTIONS
#Generate residual and predicted values
din.po4.fresh$resids <- residuals(np_freshwater_mod) 
#TESTING ASSUMPTIONS
shapiro.test(din.po4.fresh$resids) #reject null that data is normally distributed
#Perform Levene's Test for homogeneity of variances
leveneTest(mol.ratio ~ as.factor(Month), data = din.po4.fresh) #reject the null

skewness(din.po4.fresh$mol.ratio)
kurtosis(din.po4.fresh$mol.ratio) #some peakedness

#log-transformation
din.po4.fresh$mol.ratio.log <- log(din.po4.fresh$mol.ratio)
np_freshwater_mod.log<-lm(mol.ratio.log ~ Month, data=din.po4.fresh)
anova(np_freshwater_mod.log) #significant ANOVA
#TESTING ASSUMPTIONS
#Generate residual and predicted values
din.po4.fresh$resids <- residuals(np_freshwater_mod.log) 
#TESTING ASSUMPTIONS
shapiro.test(din.po4.fresh$resids) #pass shapiro
#Perform Levene's Test for homogeneity of variances
leveneTest(mol.ratio.log ~ Month, data = din.po4.fresh) #pass levene

skewness(din.po4.fresh$mol.ratio.log)
kurtosis(din.po4.fresh$mol.ratio.log) #

#Tukey HSD
in.fw.ratio.tukey <- HSD.test(np_freshwater_mod, "Month", group=T)
summary(in.fw.ratio.tukey)

plot(in.fw.ratio.tukey, las =2)

in.fw.ratio.g <- in.fw.ratio.tukey$groups

in.fw.ratio.g <- rownames_to_column(in.fw.ratio.g, "Month")

din.po4.input.fw<- ggplot(din.po4.fresh,aes(x=as.factor(Month), y=mol.ratio)) + 
  geom_boxplot() +
  theme_cowplot() +
  scale_y_continuous(limits=c(-1,80), breaks=seq(from=0,to=80,by=20)) +
  ylab("Freshwater Input Load N:P") + xlab("Month") +
  annotate("text", x = in.fw.ratio.g$Month, y = -1, label = in.fw.ratio.g$groups, size = 5) +
  geom_hline(yintercept = 16, color="red") +
  theme(axis.title.x= element_blank(),
        axis.text=element_text(size=16),
        axis.title=element_text(size=16)) 
din.po4.input.fw


din.po4.out <- monthly %>%
  select(Type, Year, Month, Component,DIN, PO4) %>%
  filter(Year <2024) %>%
  filter(Component == "Output") 

din.po4.out

din.po4.out$DIN.Input.molar <- din.po4.out$DIN * 1000 /14.0067
din.po4.out$PO4.Input.molar <- din.po4.out$PO4 * 1000 /30.9738

din.po4.out$mol.ratio <- din.po4.out$DIN.Input.molar / din.po4.out$PO4.Input.molar

din.po4.out$Month <- as.factor(din.po4.out$Month)

din.po4.out <- na.omit(din.po4.out)

#The ANOVA 
np_out_mod <-lm(mol.ratio ~ Month, data=din.po4.out)
anova(np_out_mod) #significant ANOVA
#TESTING ASSUMPTIONS
#Generate residuals
din.po4.out$resids <- residuals(np_out_mod) 
#TESTING ASSUMPTIONS
shapiro.test(din.po4.out$resids) #fail
#Perform Levene's Test for homogeneity of variances
leveneTest(mol.ratio ~ Month, data = din.po4.out) #pass

#log-transformation
din.po4.out$mol.ratio.log <- log(din.po4.out$mol.ratio)

np_out_mod.log<-lm(mol.ratio.log ~ Month, data=din.po4.out)
anova(np_out_mod.log) #significant ANOVA
#TESTING ASSUMPTIONS
#Generate residual and predicted values
din.po4.out$resids <- residuals(np_out_mod.log) 
#TESTING ASSUMPTIONS
shapiro.test(din.po4.out$resids) #still fail
#Perform Levene's Test for homogeneity of variances
leveneTest(mol.ratio.log ~ Month, data = din.po4.out) #pass levenes

skewness(din.po4.out$mol.ratio.log)
kurtosis(din.po4.out$mol.ratio.log) #

#Tukey HSD
out.fw.ratio.tukey <- HSD.test(np_out_mod.log, "Month", group=T)
summary(out.fw.ratio.tukey)

plot(out.fw.ratio.tukey, las =2)
out.fw.ratio.g <- out.fw.ratio.tukey$groups

out.fw.ratio.g <- rownames_to_column(out.fw.ratio.g, "Month")

din.po4.output<- ggplot(din.po4.out,aes(x=Month, y=mol.ratio)) + 
  geom_boxplot() +
  theme_cowplot() +
  scale_y_continuous(limits=c(-1,80), breaks=seq(from=0,to=80,by=20)) +
  ylab("Low Tide Output Load N:P") + xlab("Month") +
  annotate("text", x = out.fw.ratio.g$Month, y = -1, label = out.fw.ratio.g$groups, size = 5) +
  geom_hline(yintercept = 16, color="red") +
  theme(axis.title.x= element_blank(),
        axis.text=element_text(size=16),
        axis.title=element_text(size=16)) 
din.po4.output


#Freshwater inputs to saltwater outputs
NP_Ratio.fresh <- plot_grid(din.po4.input.fw, din.po4.output, labels = "auto", rel_heights = c(2, 2, 2), label_x = 0.1, ncol = 1, align = "v")
NP_Ratio.fresh

ggsave(NP_Ratio.fresh, file=paste0("results/manuscript_figures/Figure5_NP_monthlyratio.png"),
       width=8, height=6, units="in", dpi=400, bg="white")

