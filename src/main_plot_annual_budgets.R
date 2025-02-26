#main_plot_annual_budgets.R

#Author: Anna Mikulis, University of New Hampshire

#Purpose: Calculate Input, Output and Delta Storage Terms for each solute of interest. Plot results.

#Load packages
Packages <- c("readr", "dplyr", "ggplot2", "measurements", "plotly", "lubridate", "cowplot",
              "agricolae", "viridis", "stats", "tidyr", "moments", "tidyr", "tibble", "stringr")

lapply(Packages, library, character.only = TRUE)


budget_components <- read.csv("results/main_compile_inputs/Budget_Components.csv") %>%
  select(-X) 
#Budget_Components Wide
GB_hectares <- 1700

budget_components_w <- budget_components %>%
  pivot_wider(names_from = "Solute",
              values_from = c("Load_kgyr"))

print(budget_components_w)

budget_final <- budget_components_w %>%
  group_by(Year, Balance) %>%
  summarize(across(DIN:TSS, sum, na.rm = T))

yearstoomit <- read.csv("results/main_load_calc/years_to_omit.csv")

yearstoomit <- yearstoomit %>%
  select(Year, Parameter) %>%
  filter(Parameter != "DON_MGL") %>%
  rename("Solute" = "Parameter") 

yearstoomit$Solute <- str_sub(yearstoomit$Solute, 1, str_length(yearstoomit$Solute)-4)

yearstoomit <- unique(yearstoomit)

yearstoomit$Flag <- "flag"

budget_final_long <- budget_final %>%
  pivot_longer(cols = c(DIN:TSS),
               names_to = "Solute",
               values_to = "Load")

budget_final_long <- full_join(budget_final_long, yearstoomit)

budget_final_long$Flag <- as.factor(budget_final_long$Flag)

budget_final_long$Loadv2 <- ifelse(is.na(budget_final_long$Flag), budget_final_long$Load, NA)

budget_final_long$Load <- budget_final_long$Loadv2
                                   
budget_final <- budget_final_long %>%
  select(-Flag, - Loadv2) %>%
  pivot_wider(names_from="Solute",
              values_from = "Load")

budget_final_with_storage <- budget_final %>%
  group_by(Year) %>% # Calculate the storage for each year
  summarise(across(DIN:TSS, ~ .[Balance == "Input"] - .[Balance == "Output"], .names = "Storage_{.col}")) %>%
  # Reshape the storage data to match the original format
  pivot_longer(cols = starts_with("Storage"), names_to = "solute", values_to = "value") %>%
  mutate(Balance = "Storage", solute = sub("Storage_", "", solute)) %>%
  pivot_wider(names_from = solute, values_from = value) %>%
  bind_rows(budget_final) %>%  # Bind the original data and the storage data
   arrange(Year, Balance)  # Arrange by year and Balance type (Input, Output, Storage)

# View the resulting dataframe
print(budget_final_with_storage)

table4 <- budget_final_with_storage %>%
  group_by(Balance) %>%
  summarise(across(DIN:TSS, list(
    min = ~ signif(min(.x, na.rm = TRUE), 3),
    max = ~ signif(max(.x, na.rm = TRUE), 3),
    mean = ~ signif(mean(.x, na.rm = TRUE), 3),
    sd = ~ signif(sd(.x, na.rm = TRUE), 3)
  ), .names = "{.col}_{.fn}")) %>%
  pivot_longer(
    cols = -Balance,
    names_to = c("Variable", "Statistic"),
    names_pattern = "^(.*)_(.*)$",
    values_to = "Value") %>%
  pivot_wider(names_from = Statistic, values_from = Value) %>%
  mutate(Combined = paste0(
    formatC(mean, format = "e", digits = 2), " ± ", 
    formatC(sd, format = "e", digits = 2), 
    " (", formatC(min, format = "e", digits = 2), ", ", 
    formatC(max, format = "e", digits = 2), ")"
  )) %>%
  select(Balance, Variable, Combined) %>%
  pivot_wider(names_from = Variable, values_from = Combined) %>%
  pivot_longer(cols = -Balance, names_to = "Solute", values_to = "Value") %>%
  pivot_wider(names_from = Balance, values_from = Value)

table4
write.csv(table4, file = paste0("./results/manuscript_figures/tables/table4.csv"))

# View the resulting long-format summary table
print(summary_stats_long)

#Normalized dataframe
budget_final_normalized <- budget_final_with_storage %>%
  mutate(across(DIN:TSS, ~ . / GB_hectares))

summary(budget_final_normalized)
head(budget_final_normalized)


summary_stats_long <- budget_final_normalized %>%
  group_by(Balance) %>%
  summarise(across(DIN:TSS, list(
    min = ~ signif(min(.x, na.rm = TRUE),3),
    max = ~ signif(max(.x, na.rm = TRUE),3),
    mean = ~ signif(mean(.x, na.rm = TRUE),3),
    sd = ~ signif(sd(.x, na.rm=TRUE),3)
  ), .names = "{.col}_{.fn}")) %>%
  pivot_longer(
    cols = -Balance,
    names_to = c("Variable", "Statistic"),
    names_pattern = "^(.*)_(.*)$",
    values_to = "Value"
  )

# View the resulting long-format summary table
print(summary_stats_long)

summary_table <- summary_stats_long %>%
  pivot_wider(names_from = Statistic, values_from = Value) %>% # Pivot to make Statistic columns
  mutate(Combined = paste0(round(mean, 2), " ± ", round(sd, 2), 
                           " (", round(min, 2), ", ", round(max, 2), ")")) %>% # Create the combined cell
  select(Balance, Variable, Combined) %>% # Keep only relevant columns
  pivot_wider(names_from = Variable, values_from = Combined) # Pivot again to make Variables columns

# View the result
summary_table <- budget_final_normalized %>%
  group_by(Balance) %>%
  summarise(across(DIN:TSS, list(
    min = ~ signif(min(.x, na.rm = TRUE), 3),
    max = ~ signif(max(.x, na.rm = TRUE), 3),
    mean = ~ signif(mean(.x, na.rm = TRUE), 3),
    sd = ~ signif(sd(.x, na.rm = TRUE), 3)
  ), .names = "{.col}_{.fn}")) %>%
  pivot_longer(
    cols = -Balance,
    names_to = c("Variable", "Statistic"),
    names_pattern = "^(.*)_(.*)$",
    values_to = "Value"
  ) %>%
  pivot_wider(names_from = Statistic, values_from = Value) %>%
  mutate(Combined = paste0(
    formatC(mean, format = "e", digits = 2), " ± ", 
    formatC(sd, format = "e", digits = 2), 
    " (", formatC(min, format = "e", digits = 2), ", ", 
    formatC(max, format = "e", digits = 2), ")"
  )) %>%
  select(Balance, Variable, Combined) %>%
  pivot_wider(names_from = Variable, values_from = Combined) %>%
  pivot_longer(cols = -Balance, names_to = "Solute", values_to = "Value") %>%
  pivot_wider(names_from = Balance, values_from = Value)

print(summary_table)

write.csv(summary_table, file = paste0("./results/manuscript_figures/tables/table4_norm.csv"))



#Load summary table (kg/yr)
load_summary <- budget_final %>%
  group_by(Balance) %>%
  summarise(across(DIN:TSS, list(
    min = ~ signif(min(.x, na.rm = TRUE),3),
    max = ~ signif(max(.x, na.rm = TRUE),3),
    mean = ~ signif(mean(.x, na.rm = TRUE),3),
    sd = ~ signif(sd(.x, na.rm=TRUE),3)
  ), .names = "{.col}_{.fn}")) %>%
  pivot_longer(
    cols = -Balance,
    names_to = c("Variable", "Statistic"),
    names_pattern = "^(.*)_(.*)$",
    values_to = "Value"
  )


load_summary2 <- load_summary %>%
  pivot_wider(names_from = Statistic, values_from = Value) %>% # Pivot to make Statistic columns
  mutate(Combined = paste0(round(mean, 2), " ± ", round(sd, 2), 
                           " (", round(min, 2), ", ", round(max, 2), ")")) %>% # Create the combined cell
  select(Balance, Variable, Combined) %>% # Keep only relevant columns
  pivot_wider(names_from = Variable, values_from = Combined) # Pivot again to make Variables columns

load_summary2

n_budget_long <- budget_final_normalized %>%
  select(Year, Balance, DIN, PN, TN) %>%
  pivot_longer(cols=c(DIN:TN),
               names_to = "Solute",
               values_to = "Load_kghayr")

storage_a <- ggplot(subset(n_budget_long, Balance == "Storage"), aes(Solute, Load_kghayr)) +
  geom_boxplot() +
  geom_point(position="jitter", aes(color=Year)) +
  geom_hline(yintercept = 0, color="black", alpha=0.5) +
  # annotate("text", x="DIN_kghayr", y=-650, label= "Net Export") + 
  #annotate("text", x="TN_kghayr",y=300, label= "Net Import") +
  theme_cowplot() + 
  ylab(Storage~kg~N~ha^-1~year^-1) +
  scale_x_discrete(labels=c("DIN_kghayr" = "DIN", "PN_kghayr"= "PN", "TN_kghayr"= "TN")) +
  theme(axis.text=element_text(size=15),
        axis.title = element_text(size=18),
        axis.title.x = element_blank())
storage_a



full_combo <- budget_final_normalized %>%
  select(Year, Balance, DIN, PN, TN, PO4, DOC, TSS) %>%
  pivot_longer(cols=c(DIN:TSS),
               names_to = "Solute",
               values_to = "Load_kghayr")

full_combo$type <- ifelse(full_combo$Solute == "PO4", "Phosphate",
                          ifelse(full_combo$Solute == "DOC", "Dissolved Organic Carbon",
                                 ifelse(full_combo$Solute == "TSS", "Total Suspended Solids", "Nitrogen")))

full_combo$type <- as.factor(full_combo$type)
full_combo$Solute <- as.factor(full_combo$Solute)

solute_vector <- c("a.nitrogen", "b.phosphate", "c.carbon", "d.solids")

full_combo$solute_vector <- ifelse(full_combo$type == "Phosphate", "B",
                                   ifelse(full_combo$type == "Dissolved Organic Carbon", "C", 
                                          ifelse(full_combo$type == "Nitrogen", "A", "D")))



#FIGURE FOR MANUSCRIPT
panel_fig <- ggplot(subset(full_combo, Balance=="Storage"), 
                    aes(as.factor(Solute), Load_kghayr)) + 
  geom_boxplot(width=0.1,outlier.shape = NA) +
  geom_point(aes(color=as.factor(Year)), 
             position = position_jitterdodge(jitter.width=.5, dodge.width = 0.4),
             size=1.5) + 
  geom_hline(yintercept = 0, color="black", alpha=0.8) +
  scale_x_discrete(labels=c("PO4_kghayr"= ~PO[4], "DIN_kghayr" = "DIN",
                            "PN_kghayr"= "PN", "TN_kghayr"= "TN", 
                            "DOC_kghayr"= "DOC", "TSS_kghayr" = "TSS")) +
  scale_color_viridis_d(option="viridis", name="Year", direction=-1) +
  theme_bw() +
  ylab(Net~Retention~kg~ha^-1~year^-1) +
  facet_wrap(~solute_vector, scales="free", strip.position = "top",
             labeller = labeller(solute_vector = tolower)) +
  theme(axis.title.x = element_blank(), 
        axis.text=element_text(size=15, color="black"),
        axis.title = element_text(size=18),
        strip.background = element_blank(),
        strip.placement= "inside",
        strip.text = element_text(size=14, face="bold", hjust=0, vjust=-1),
        legend.text = element_text(size=12),
        legend.title = element_text(size=18))
panel_fig

##FIGURE FOR MANUSCRIPT points colored by input loads
n_full <- full_combo %>%
  filter(Solute == "DIN" | Solute == "PN" | Solute == "TN") %>%
  filter(Balance == "Input" | Balance == "Storage")


wwtf_inputs <- budget_components %>%
  filter(Type == "WWTF" & Solute == "TN") %>%
  select(Year, Load_kgyr) %>%
  rename("TN_WWTF_Load_kgyr" = Load_kgyr)

n_full2 <- full_join(n_full, wwtf_inputs, by="Year")

n_full2$TN_WWTF_Load_kghayr <- n_full2$TN_WWTF_Load_kgyr / GB_hectares


panel_figA <- ggplot(subset(n_full2, Balance=="Storage"), aes(as.factor(Solute), Load_kghayr)) + 
  geom_hline(yintercept = 0, color="black", alpha=0.8) +
  geom_boxplot(width=0.1,outlier.shape = NA) +
  geom_point(aes(color=TN_WWTF_Load_kghayr), 
             position = position_jitterdodge(jitter.width=0.5),
             size=2) + 
  scale_color_viridis(option="viridis", direction=-1) +
  labs(colour = expression(atop('WWTF TN Inputs', 
                                '(kg N ha'^-1 ~ yr^-1 * ')'))) +
  theme_bw() +
  ylab(Net~Retention~kg~ha^-1~year^-1) +
  facet_wrap(~solute_vector, scales="free", strip.position = "top",
             labeller = labeller(solute_vector = tolower)) +
  theme(axis.title.x = element_blank(), 
        axis.text=element_text(size=14, color="black"),
        axis.title = element_text(size=15),
        strip.background = element_blank(),
        strip.placement= "inside",
        strip.text = element_text(size=14, face="bold", hjust=0, vjust=-1),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12))
panel_figA


p_full <- full_combo %>%
  filter(Solute == "PO4") %>%
  filter(Balance == "Input" | Balance == "Storage") %>%
  pivot_wider(names_from = "Balance",
              values_from = "Load_kghayr")

panel_figB <- ggplot(p_full, aes(as.factor(Solute), Storage)) + 
  geom_hline(yintercept = 0, color="black", alpha=0.8) +
  geom_boxplot(width=0.1,outlier.shape = NA) +
  geom_point(aes(color=Input), 
             position = position_jitterdodge(jitter.width=0.5),
             size=2) + 
  scale_color_viridis(option="viridis", direction=-1) +
  labs(colour = expression(atop('PO4 Inputs', 
                                '(kg P ha'^-1 ~ yr^-1 * ')'))) +
  theme_bw() +
  ylab(Net~Retention~kg~ha^-1~year^-1) +
  facet_wrap(~solute_vector, scales="free", strip.position = "top",
             labeller = labeller(solute_vector = tolower)) +
  theme(axis.title.x = element_blank(), 
        axis.text=element_text(size=14, color="black"),
        axis.title = element_text(size=15),
        strip.background = element_blank(),
        strip.placement= "inside",
        strip.text = element_text(size=14, face="bold", hjust=0, vjust=-1),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12))
panel_figB

c_full <- full_combo %>%
  filter(Solute == "DOC") %>%
  filter(Balance == "Input" | Balance == "Storage") %>%
  pivot_wider(names_from = "Balance",
              values_from = "Load_kghayr")

panel_figC <- ggplot(c_full, aes(as.factor(Solute), Storage)) + 
  geom_hline(yintercept = 0, color="black", alpha=0.8) +
  geom_boxplot(width=0.1,outlier.shape = NA) +
  geom_point(aes(color=Input), 
             position = position_jitterdodge(jitter.width=0.5),
             size=2) + 
  scale_color_viridis(option="viridis", direction=-1) +
  labs(colour = expression(atop('DOC Inputs', 
                                '(kg C ha'^-1 ~ yr^-1 * ')'))) +
  theme_bw() +
  ylab(Net~Retention~kg~ha^-1~year^-1) +
  facet_wrap(~solute_vector, scales="free", strip.position = "top",
             labeller = labeller(solute_vector = tolower)) +
  theme(axis.title.x = element_blank(), 
        axis.text=element_text(size=14, color="black"),
        axis.title = element_text(size=15),
        strip.background = element_blank(),
        strip.placement= "inside",
        strip.text = element_text(size=14, face="bold", hjust=0, vjust=-1),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12))
panel_figC

tss_full <- full_combo %>%
  filter(Solute == "TSS") %>%
  filter(Balance == "Input" | Balance == "Storage") %>%
  pivot_wider(names_from = "Balance",
              values_from = "Load_kghayr")

panel_figD <- ggplot(tss_full, aes(as.factor(Solute), Storage)) + 
  geom_hline(yintercept = 0, color="black", alpha=0.8) +
  geom_boxplot(width=0.1,outlier.shape = NA) +
  geom_point(aes(color=Input), 
             position = position_jitterdodge(jitter.width=0.5),
             size=2) + 
  scale_color_viridis(
    option = "viridis", 
    direction = -1) +
  labs(colour = expression(atop('TSS Inputs', 
                                '(kg ha'^-1 ~ yr^-1 * ')'))) +
  theme_bw() +
  ylab(Net~Retention~kg~ha^-1~year^-1) +
  facet_wrap(~solute_vector, scales="free", strip.position = "top",
             labeller = labeller(solute_vector = tolower)) +
  theme(axis.title.x = element_blank(), 
        axis.text=element_text(size=14, color="black"),
        axis.title = element_text(size=15),
        strip.background = element_blank(),
        strip.placement= "inside",
        strip.text = element_text(size=14, face="bold", hjust=0, vjust=-1),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12)) #+
 # scale_y_continuous(labels = scientific_format()) +  # Format y-axis labels in scientific notation
 panel_figD


library(cowplot)

# Assuming your individual plots are panel_figA, panel_figB, panel_figC, and panel_figD
combined_plot <- plot_grid(
  panel_figA, 
  panel_figB + theme(axis.title.y = element_blank()),  
  panel_figC, 
  panel_figD + theme(axis.title.y = element_blank()),  
  ncol = 2,  # Arrange in two columns (or adjust as needed)
  align = "hv"  # Align horizontally and vertically
)

combined_plot

ggsave(combined_plot, filename= "./results/manuscript_figures/Figure4.jpeg", bg="white", height = 8, width = 12, units = "in", dpi=300)


ggplot(subset(n_full2, Balance=="Storage"), aes(TN_WWTF_Load_kghayr, Load_kghayr, color=Balance)) + geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(~Solute)



full_combo_v2 <- full_combo %>%
  pivot_wider(id_cols=c(Year, Solute, type, solute_vector),
              names_from = "Balance",
              values_from = "Load_kghayr")


n <- full_combo_v2 %>%
  filter(type == "Nitrogen")

summary(n)

din_model <- lm(Storage ~ Input, data = subset(n, Solute =="DIN"))
summary(din_model)

tn_model <- lm(Storage ~ Input, data = subset(n, Solute =="TN"))
summary(tn_model)

tss_model <- lm(Storage ~ Input, data = subset(full_combo_v2, Solute =="TSS"))
summary(tss_model)

ggplot(subset(n, Solute == "DIN") , aes(Year, Storage)) + 
  geom_point(aes(color=Input), size=6) + 
  geom_vline(xintercept = 2017.5, color="red") +
  geom_vline(xintercept = 2020.5, color="red") +
  geom_smooth(method="lm", se =FALSE) +
  geom_hline(yintercept = 0, color="black", alpha=0.8) +
  scale_color_viridis(option="viridis", name="Input", direction=1) +
  theme_bw() +
  scale_x_continuous(limits=c(2008, 2023), breaks=seq(from=2008,to=2023,by=1)) +
  ylab(Net~Retention~kg~ha^-1~year^-1) +
  facet_wrap(~Solute, scales="free", strip.position = "top") +
  theme(axis.title.x = element_blank(), 
        axis.text=element_text(size=15, color="black"),
        axis.title = element_text(size=18),
        strip.background = element_blank(),
        strip.placement= "inside",
        strip.text = element_text(size=14, face="bold", hjust=0, vjust=-1),
        legend.text = element_text(size=12),
        legend.title = element_text(size=18))


ggplot(subset(full_combo_v2, Solute == "TSS") , aes(Year, Storage)) + 
  geom_point(aes(color=Input), size=6) + 
  geom_smooth(method="lm", se =FALSE) +
  geom_hline(yintercept = 0, color="black", alpha=0.8) +
  scale_color_viridis(option="viridis", name="Input", direction=1) +
  theme_bw() +
  scale_x_continuous(limits=c(2008, 2023), breaks=seq(from=2008,to=2023,by=1)) +
  ylab(Net~Retention~kg~ha^-1~year^-1) +
  facet_wrap(~Solute, scales="free", strip.position = "top") +
  theme(axis.title.x = element_blank(), 
        axis.text=element_text(size=15, color="black"),
        axis.title = element_text(size=18),
        strip.background = element_blank(),
        strip.placement= "inside",
        strip.text = element_text(size=14, face="bold", hjust=0, vjust=-1),
        legend.text = element_text(size=12),
        legend.title = element_text(size=18))



#Net Retention Over Time
ggplot(subset(full_combo, Balance == "Storage"), aes(Year, Load_kghayr)) +
  geom_point(size=3) +
  geom_line() +
  geom_hline(yintercept =  0) +
  ylab(Net~Retention~kg~ha^-1~year^-1) +
  facet_wrap(~Solute, scales="free_y") +
  scale_x_continuous(limits=c(2008,2023), breaks=seq(from=2008, to=2023, by =2)) +
  theme_bw() +
  theme(axis.title.x = element_blank(), 
        axis.text=element_text(size=12, color="black"),
        axis.title = element_text(size=18),
        strip.background = element_blank(),
        strip.text = element_text(size=14, face="bold", hjust=0, vjust=-1),
        legend.text = element_text(size=12),
        legend.title = element_text(size=18))



#N:P Ratio of Inputs and Outputs
#Inputs and Outputs N:P Ratios
#DIN:PO4 of Freshwater inputs
Freshwater_inputs <- budget_components %>%
  filter(Type != "APH" & Type !="APL") %>%
  filter(Solute == "DIN" | Solute == "PO4") %>%
  filter(Year != 2012 & Year !=2013 & Year !=2008 & Year !=2009) %>% #no PO4 for rivers measured
  group_by(Solute, Year) %>%
  summarize(mean_kgyr= mean(Load_kgyr, na.rm=T)) %>%
  pivot_wider(names_from ="Solute", values_from="mean_kgyr")

Freshwater_inputs$DIN_kghayr <- Freshwater_inputs$DIN / GB_hectares
Freshwater_inputs$DIN_molyr <- Freshwater_inputs$DIN * 1000 /14.0067
Freshwater_inputs$PO4_molyr <- Freshwater_inputs$PO4 * 1000 /30.9738

Freshwater_inputs$DIN_molhayr <- Freshwater_inputs$DIN_molyr / GB_hectares
Freshwater_inputs$PO4_molhayr <- Freshwater_inputs$PO4_molyr / GB_hectares

Freshwater_inputs$NP_mol <- Freshwater_inputs$DIN_molhayr / Freshwater_inputs$PO4_molhayr
mean(Freshwater_inputs$NP_mol)
sd(Freshwater_inputs$NP_mol)

#Annual DIN:PO4 of Output Tide
tidal_np<- budget_components %>%
  filter(Type == "APH" | Type =="APL") %>%
  filter(Solute == "DIN" | Solute == "PO4") %>%
  group_by(Type,Solute, Year) %>%
  summarize(mean_kgyr= mean(Load_kgyr, na.rm=T)) %>%
  pivot_wider(names_from ="Solute", values_from="mean_kgyr")

tidal_np$DIN_kghayr <- tidal_np$DIN / GB_hectares
tidal_np$DIN_molyr <- tidal_np$DIN * 1000 /14.0067
tidal_np$PO4_molyr <- tidal_np$PO4 * 1000 /30.9738

tidal_np$DIN_molhayr <- tidal_np$DIN_molyr / GB_hectares
tidal_np$PO4_molhayr <- tidal_np$PO4_molyr / GB_hectares

tidal_np$NP_mol <- tidal_np$DIN_molhayr / tidal_np$PO4_molhayr

tidal_np_summary <- tidal_np %>%
  group_by(Type) %>%
  summarize(mean_np = round(mean(NP_mol, na.rm=T),0),
            sd = round(sd(NP_mol, na.rm=T),1))



