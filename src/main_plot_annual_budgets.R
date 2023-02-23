#main_plot_annual_budgets.R

#Author: Anna Mikulis, University of New Hampshire
#Last Updated 1/27/2023

#Purpose: Calculate Input, Output and Delta Storage Terms for each solute of interest. Plot results.

#Load packages
Packages <- c("readr", "dplyr", "ggplot2", "measurements", "plotly", "lubridate", "cowplot",
              "agricolae", "viridis", "stats", "tidyr", "moments", "tidyr", "tibble")

lapply(Packages, library, character.only = TRUE)


budget_components <- read.csv("results/main_compile_inputs/Budget_Components.csv") %>%
   select(-X)
  
#Budget_Components Wide
GB_hectares <- 1677.21

budget_components_w <- budget_components %>%
  pivot_wider(names_from = "Solute",
              values_from = "Load_kgyr")

#Total Nitrogen Budget Plots
#Subset out TN numbers
tn_budget <- budget_components_w %>%
  select(Type, Year, Balance, TN) %>%
  filter(Year <2019) 

#Normalize TN Budget
tn_budget$TN_normalized_kghayr <- tn_budget$TN/GB_hectares
  
tn_budget_final <- tn_budget %>%
  group_by(Year, Balance) %>%
  summarize(TN_Load =sum(TN, na.rm=F)) %>%
  pivot_wider(names_from = "Balance",
              values_from = "TN_Load")
  
tn_budget_final$Storage <- tn_budget_final$Input - tn_budget_final$Output
#Calculate GB area normalized storage, because we did not use normalized storage in the calculation of storage
tn_budget_final$Storage_normalized <- tn_budget_final$Storage / GB_hectares 
  
tn_budget_final$Input_normalized <- tn_budget_final$Input / GB_hectares
tn_budget_final$Output_normalized <- tn_budget_final$Output / GB_hectares

#average storage of tn over period of record
tn.average.storage <- mean(tn_budget_final$Storage_normalized)
  
tn_budget_final_long <- tn_budget_final %>%
  select(Year, Storage_normalized:Output_normalized) %>%
  pivot_longer(cols=c(Storage_normalized:Output_normalized),
                 names_to="Component",
                 values_to = "TN_kghayr")
  
TN_norm_avg <- tn_budget_final_long %>%
  group_by(Component) %>%
  summarize(mean=mean(TN_kghayr, na.rm=T))
  
tn.storage <- ggplot(tn_budget_final, aes(x=Year)) + 
  geom_col(aes(y=Storage_normalized), fill="darkseagreen3") +
  geom_hline(yintercept=0) +
  geom_line(aes(y=- 276.81), color="darkseagreen2", linetype = 'dotdash', size =1.5) +
  scale_x_continuous(limits=c(2007.5,2018.5), breaks=seq(from=2008,to=2018, by=1)) +
 # scale_y_continuous(limits=c(-725, 300), breaks=seq(from=-700,to=300,by=100)) +
  ylab(expression(TN~Storage~(kg-N-ha^{-1}~year^{-1}))) +
  theme_cowplot() +
  annotate("text", x = 2013, y = -20, label = "-1.24", size = 4, color="darkseagreen3") +
  theme(axis.title.x = element_blank())
tn.storage
#If you want to save the plot, remove hashtag & run these next few lines of code
#ggsave(tn.storage, 
 #        file=paste0("results/figures/annual_box_model/tn.storage.png"),
  #       width = 8, height = 6, units = "in", dpi = 300)
  
tn.inputs.outputs <- ggplot(subset(tn_budget_final_long, Component != "Storage_normalized"), 
                            aes(Year, TN_kghayr, fill= Component)) + 
  geom_bar(position = 'dodge', stat='identity') +
   geom_hline(yintercept =0) +
  geom_line(aes(y=3985.55), color="darkslateblue", linetype = 'dashed', size=1.5) +
  geom_line(aes(y=4262.36), color="darkslategray3", linetype = 'dotted', size=1.5) +
  scale_x_continuous(limits=c(2007.5,2018.5), breaks=seq(from=2008, to=2018, by =1)) +
  scale_y_continuous(limits=c(0, 5800), labels=scales::comma, breaks=seq(from=-800, to=5800, by=800)) +
  scale_fill_manual(values = c("darkslateblue", "darkslategray3", "darkseagreen3"), labels=c("Input", "Output", "Storage")) +
  theme_cowplot() + theme(legend.position = c(0.65, 0.9)) +
  ylab(expression(TN~Yields~(kg-N~ha^{-1}~year^{-1}))) +
  theme(axis.title.x = element_blank()) +
  theme(legend.title = element_blank())
tn.inputs.outputs
  
#ggsave(tn.inputs.outputs, 
 #        file=paste0("results/figures/annual_box_model/tn.inputs.outputs.png"),
  #       width = 8, height = 6, units = "in", dpi = 300)
  
#Create combined pannel of TN inputs/outputs and storage
tn.annual_panel <- plot_grid(tn.inputs.outputs, tn.storage, labels = "AUTO")
tn.annual_panel
  
#ggsave(tn.annual_panel, file=paste0("results/figures/annual_box_model/tn.annual.panel.png"),
         #width=12, height=6, units="in", dpi=300)
  
#PN
pn_budget <- budget_components_w %>%
    select(Type, Year, Balance, PN) %>%
    filter(Year <2019) 

pn_budget_final <- pn_budget %>%
    group_by(Year, Balance) %>%
    summarize(PN_Load =sum(PN, na.rm=F)) %>%
    pivot_wider(names_from = "Balance",
                values_from = "PN_Load")
  
pn_budget_final$Storage <- pn_budget_final$Input - pn_budget_final$Output
pn_budget_final$Storage_normalized <- pn_budget_final$Storage / GB_hectares 
pn_budget_final$Input_normalized <- pn_budget_final$Input / GB_hectares
pn_budget_final$Output_normalized <- pn_budget_final$Output / GB_hectares
  
pn.average.storage<- mean(pn_budget_final$Storage_normalized)
  
pn_budget_final_long <- pn_budget_final %>%
  select(Year, Storage_normalized:Output_normalized) %>%
  pivot_longer(cols=c(Storage_normalized:Output_normalized),
               names_to="Component",
               values_to = "PN_kghayr")
  
PN_norm_avg <- pn_budget_final_long %>%
  group_by(Component) %>%
  summarize(mean=mean(PN_kghayr, na.rm=T))
  
pn.storage <- ggplot(pn_budget_final, aes(x=Year)) + 
   geom_col(aes(y=Storage_normalized), fill="darkseagreen3") +
   geom_hline(yintercept=0) +
   geom_line(aes(y=-283.9282), color="darkseagreen2", linetype = 'dotdash', size =1.5) +
   scale_x_continuous(limits=c(2007.5,2018.5), breaks=seq(from=2008,to=2018, by=1)) +
   scale_y_continuous(limits=c(-600,100), breaks=seq(from=-600,to=100, by=100)) +
   ylab(expression(PN~Storage~(kg-N~ha^{-1}~year^{-1}))) +
   theme_cowplot()
pn.storage
  
pn.inputs.outputs <- ggplot(subset(pn_budget_final_long, Component != "Storage_normalized"), 
                          aes(Year, PN_kghayr, fill= Component)) + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_hline(yintercept =0) +
  geom_line(aes(y=879.6243), color="darkslateblue", linetype = 'dashed', size=1.5) +
  geom_line(aes(y=1163.5525), color="darkslategray3", linetype = 'dotted', size=1.5) +
  scale_x_continuous(limits=c(2007.5,2018.5), breaks=seq(from=2008, to=2018, by =1)) +
  scale_y_continuous(limits=c(0, 1600), labels=scales::comma, breaks=seq(from=0, to=1600, by=200)) +
  scale_fill_manual(values = c("darkslateblue", "darkslategray3", "darkseagreen3"), labels=c("Input", "Output", "Storage")) +
  theme_cowplot() + theme(legend.position = c(0.65, 0.95)) +
  ylab(expression(PN~Yields~(kg-N~ha^{-1}~year^{-1}))) +
  theme(legend.title = element_blank())

pn.inputs.outputs 
  
#ggsave(pn.inputs.outputs, 
 #      file=paste0("results/figures/annual_box_model/pn.inputs.outputs.png"),
  #       width = 8, height = 6, units = "in", dpi = 300)
  
pn.annual.panel <- plot_grid(pn.inputs.outputs, pn.storage, labels = "AUTO")
pn.annual.panel

#ggsave("results/figures/annual_box_model/pn.figure.png", pn.annual.panel, 
 #      width=12, height=6, units="in", dpi=300)
#_______________________

#Combine tn and pn into one big figure 
tn.pn <- plot_grid(tn.inputs.outputs,tn.storage,pn.inputs.outputs, pn.storage, labels = "AUTO",
                   label_x = 0.2)
tn.pn
#ggsave(tn.pn, "results/figures/annual_box_model/tn.pn.figure.png", tn.pn, width=12, height=7, units="in", dpi=300)
#_______________________
#______________________________________________________
#DIN Budget
din_budget <- budget_components_w %>%
  select(Type, Year, Balance, DIN) %>%
  filter(Year < 2019 & Year > 2008) 

din_budget_final <- din_budget %>%
  group_by(Year, Balance) %>%
  summarize(DIN_Load =sum(DIN, na.rm=F)) %>%
  pivot_wider(names_from = "Balance",
              values_from = "DIN_Load")

din_budget_final$Storage <- din_budget_final$Input - din_budget_final$Output
din_budget_final$Storage_normalized <- din_budget_final$Storage / GB_hectares 
din_budget_final$Input_normalized <- din_budget_final$Input / GB_hectares
din_budget_final$Output_normalized <- din_budget_final$Output / GB_hectares

din.average.storage<- mean(din_budget_final$Storage_normalized)

din_budget_final_long <- din_budget_final %>%
  select(Year, Storage_normalized:Output_normalized) %>%
  pivot_longer(cols=c(Storage_normalized:Output_normalized),
               names_to="Component",
               values_to = "DIN_kghayr")

DIN_norm_avg <- din_budget_final_long %>%
  group_by(Component) %>%
  summarize(mean=mean(DIN_kghayr, na.rm=T))

DIN_avg <- din_budget_final %>%
  ungroup() %>%
  select(Input, Output, Storage) %>%
  summarize(across(Input:Storage, mean, na.rm=T))


din.storage <- ggplot(din_budget_final, aes(x=Year)) + 
  geom_col(aes(y=Storage_normalized), fill="darkseagreen3") +
  geom_hline(yintercept=0) +
  geom_line(aes(y=145.83), color="darkseagreen2", linetype = 'dotdash', size =1.5) +
  scale_x_continuous(limits=c(2008.5,2018.5), breaks=seq(from=2009,to=2018, by=1)) +
  scale_y_continuous(limits=c(-80,380)) +
  ylab(expression(DIN~Storage~(kg-N~ha^{-1}~year^{-1}))) +
  theme_cowplot()
din.storage

#ggsave(din.storage, 
 #      file=paste0("results/figures/annual_box_model/din.storage.png"),
  #     width = 8, height = 6, units = "in", dpi = 300)

din.inputs.outputs <- ggplot(subset(din_budget_final_long, Component != "Storage_normalized"), 
                             aes(Year, DIN_kghayr, fill= Component)) + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_hline(yintercept =0) +
  geom_line(aes(y=1448.95), color="darkslateblue", linetype = 'dashed', size=1.5) +
  geom_line(aes(y=1303.12), color="darkslategray3", linetype = 'dotted', size=1.5) +
  scale_x_continuous(limits=c(2008.5,2018.5), breaks=seq(from=2009, to=2018, by =1)) +
  scale_y_continuous(limits=c(0,2200), labels=scales::comma, breaks=seq(from=0, to=2200, by=200)) +
  scale_fill_manual(values = c("darkslateblue", "darkslategray3"), labels=c("Input", "Output")) +
  theme_cowplot() + 
  ylab(expression(DIN~Yields~(kg-N~ha^{-1}~year^{-1}))) +
  theme(legend.title = element_blank(),
        legend.position = c(0.1,0.9))
din.inputs.outputs

#ggsave(din.inputs.outputs, 
 #      file=paste0("results/figures/annual_box_model/din.inputs.outputs.png"),
  #     width = 8, height = 6, units = "in", dpi = 300)

din.annual.panel <- plot_grid(din.inputs.outputs, din.storage, labels = "AUTO")
din.annual.panel

#ggsave(din.annual.panel, file=paste0("results/figures/annual_box_model/din.annual.panel.png"),
 #      width=12, height=6, units="in", dpi=300)

#TDN Budget
TDN_Budget <- budget_components_w %>%
  select(Type, Year, Balance, TDN) %>%
  filter(Year < 2019)

TDN_Budget_Final <- TDN_Budget %>%
  group_by(Year, Balance) %>%
  summarize(TDN_Load =sum(TDN, na.rm=F)) %>%
  pivot_wider(names_from = "Balance",
              values_from = "TDN_Load")

TDN_Budget_Final$Storage <- TDN_Budget_Final$Input - TDN_Budget_Final$Output
TDN_Budget_Final$Storage_Normalized <- TDN_Budget_Final$Storage / GB_hectares

tdn.storage <- ggplot(TDN_Budget_Final, aes(x=Year, Storage_Normalized)) + 
  geom_point(size=2) + geom_line() +
  geom_hline(yintercept=0) +
  scale_x_continuous(limits=c(2007.5,2018.5), breaks=seq(from=2008,to=2018, by=1)) +
  scale_y_continuous(limits=c(-500,500)) +
  ylab("TDN Storage (kg-N/ha/year)") +
  theme_cowplot()
tdn.storage

#ggsave(tdn.storage, 
 #      file=paste0("results/figures/annual_box_model/tdn.storage.png"),
  #     width = 8, height = 6, units = "in", dpi = 300)


#PO4 Budget
po4_budget <- budget_components_w %>%
  select(Type, Year, Balance, PO4) %>%
  filter(Year < 2019 & Year > 2009) %>%
  filter(Year != 2012 & Year != 2013)

po4_budget_final <- po4_budget %>%
  group_by(Year, Balance) %>%
  summarize(PO4_Load =sum(PO4, na.rm=F)) %>%
  pivot_wider(names_from = "Balance",
              values_from = "PO4_Load")

po4_budget_final$Storage <- po4_budget_final$Input - po4_budget_final$Output
po4_budget_final$Storage_normalized <- po4_budget_final$Storage / GB_hectares
po4_budget_final$Input_normalized <- po4_budget_final$Input / GB_hectares
po4_budget_final$Output_normalized <- po4_budget_final$Output / GB_hectares

po4.storage <- ggplot(po4_budget_final, aes(x=Year))  + 
  geom_col(aes(y=Storage_normalized), fill="darkseagreen3") +
  geom_hline(yintercept=0) +
  geom_line(aes(y=19.26482), color="darkseagreen2", linetype = 'dotdash', size =1.5) +
  scale_x_continuous(limits=c(2009.5,2018.5), breaks=seq(from=2010,to=2018, by=1)) +
  scale_y_continuous(limits=c(-30,62), breaks=seq(from=-30,to=62, by=10)) +
  ylab(expression(PO[4]~Storage~(kg-P~ha^{-1}~year^{-1}))) +
  theme_cowplot()
po4.storage

#ggsave(po4.storage, 
 #      file=paste0("results/figures/annual_box_model/po4.storage.png"),
  #     width = 8, height = 6, units = "in", dpi = 300)

po4_budget_final_long <- po4_budget_final %>%
  select(Year, Storage_normalized:Output_normalized) %>%
  pivot_longer(cols=c(Storage_normalized:Output_normalized),
               names_to="Component",
               values_to = "PO4_kghayr")

PO4_norm_avg <- po4_budget_final_long %>%
  group_by(Component) %>%
  summarize(mean=mean(PO4_kghayr, na.rm=T))


po4.inputs.outputs <- ggplot(subset(po4_budget_final_long, Component != "Storage_normalized"), 
                             aes(Year, PO4_kghayr, fill= Component)) + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_hline(yintercept =0) +
  geom_line(aes(y=240.39), color="darkslateblue", linetype = 'dashed', size=1.5) +
  geom_line(aes(y=221.13), color="darkslategray3", linetype = 'dotted', size=1.5) +
  scale_x_continuous(limits=c(2009.5,2018.5), breaks=seq(from=2009, to=2018, by =1)) +
  scale_y_continuous(limits=c(0, 320), labels=scales::comma, breaks=seq(from=0, to=320, by=50)) +
  scale_fill_manual(values = c("darkslateblue", "darkslategray3"), labels=c("Input", "Output")) +
  theme_cowplot() +
  ylab(expression(PO[4]~Yields~(kg-P~ha^{-1}~year^{-1}))) +
  theme(legend.title = element_blank(),
        legend.position=c(0.8,0.9))

po4.inputs.outputs

#ggsave(po4.inputs.outputs, 
 #      file=paste0("results/figures/annual_box_model/po4.inputs.outputs.png"),
  #     width = 8, height = 6, units = "in", dpi = 300)


po4.annual.panel <- plot_grid(po4.inputs.outputs, po4.storage, labels="AUTO")
po4.annual.panel

#ggsave(po4.annual.panel, file=paste0("results/figures/annual_box_model/po4.annual.panel.png"),
 #     width=12, height=6, units="in", dpi=300)

#DOC Budget
doc_budget <- budget_components_w %>%
  select(Type, Year, Balance, DOC) %>%
  filter(Year < 2019 & Year > 2009) %>%
  filter(Year != 2011)

doc_budget_final <- doc_budget %>%
  group_by(Year, Balance) %>%
  summarize(DOC_Load =sum(DOC, na.rm=F)) %>%
  pivot_wider(names_from = "Balance",
              values_from = "DOC_Load")


doc_budget_final$Storage <- doc_budget_final$Input - doc_budget_final$Output
doc_budget_final$Storage_normalized <- doc_budget_final$Storage / GB_hectares 
doc_budget_final$Input_normalized <- doc_budget_final$Input / GB_hectares
doc_budget_final$Output_normalized <- doc_budget_final$Output / GB_hectares

DOC.average.storage<- mean(doc_budget_final$Storage_normalized)

doc_budget_final_long <- doc_budget_final %>%
  select(Year, Storage_normalized:Output_normalized) %>%
  pivot_longer(cols=c(Storage_normalized:Output_normalized),
               names_to="Component",
               values_to = "DOC_kghayr")

DOC_norm_avg <- doc_budget_final_long %>%
  group_by(Component) %>%
  summarize(mean=mean(DOC_kghayr, na.rm=T))


doc.storage <- ggplot(doc_budget_final, aes(x=Year)) + 
  geom_col(aes(y=Storage_normalized), fill="darkseagreen3")+
  geom_hline(yintercept=0) +
  geom_line(aes(y=-83.65), color="darkseagreen2", linetype = 'dotdash', size =2) +
  scale_x_continuous(limits=c(2009.5,2018.5), breaks=seq(from=2010,to=2018, by=1)) +
  scale_y_continuous(limits=c(-3820,2200), breaks=seq(from=-4000,to=2200,by=1000)) +
  ylab(expression(DOC~Storage~(kg-C~ha^{-1}~year^{-1}))) +
  theme_cowplot()
doc.storage

#ggsave(doc.storage, 
  #     file=paste0("results/figures/annual_box_model/doc.storage.png"),
   #    width = 8, height = 6, units = "in", dpi = 300)


doc.inputs.outputs <- ggplot(subset(doc_budget_final_long, Component != "Storage_normalized"), 
                             aes(Year, DOC_kghayr, fill= Component)) + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_hline(yintercept =0) +
  geom_line(aes(y=32676.61), color="darkslateblue", linetype = 'dashed', size=2) +
  geom_line(aes(y=32760.26), color="darkslategray3", linetype = 'dotted', size=2) +
  scale_x_continuous(limits=c(2009.5,2018.5), breaks=seq(from=2009, to=2018, by =1)) +
  scale_y_continuous(limits=c(0, 45000), labels=scales::comma, breaks=seq(from=0, to=44000, by=4000)) +
  scale_fill_manual(values = c("darkslateblue", "darkslategray3"), labels=c("Input", "Output")) +
  theme_cowplot() +
  #ylab(expression(DOC~Yields~(kg-C~ha^{-1}~year^{-1}))) +
  theme(legend.title = element_blank(),
        legend.position = c(0.1,0.9))
doc.inputs.outputs
ggplotly(doc.inputs.outputs)
#ggsave(doc.inputs.outputs, 
 #      file=paste0("results/figures/annual_box_model/doc.inputs.outputs.png"),
  #     width = 8, height = 6, units = "in", dpi = 300)

doc.annual.panel <- plot_grid(doc.inputs.outputs,doc.storage, labels = "AUTO")
doc.annual.panel

#ggsave(doc.annual.panel, 
 #      file=paste0("results/figures/annual_box_model/doc.annual.panel.png"),
  #     width = 12, height = 6, units = "in", dpi = 300)


#TSS Budget
TSS_Budget <- budget_components_w %>%
  select(Type, Year, Balance, TSS) %>%
  filter(Type != "Precip") # assume no TSS in precip

TSS_Budget_Final <- TSS_Budget %>%
  group_by(Year, Balance) %>%
  summarize(TSS_Load =sum(TSS, na.rm=F)) %>%
  pivot_wider(names_from = "Balance",
              values_from = "TSS_Load")

TSS_Budget_Final$Storage <- TSS_Budget_Final$Input - TSS_Budget_Final$Output
TSS_Budget_Final$Storage_normalized <- TSS_Budget_Final$Storage / GB_hectares
TSS_Budget_Final$Input_normalized <- TSS_Budget_Final$Input / GB_hectares
TSS_Budget_Final$Output_normalized <- TSS_Budget_Final$Output / GB_hectares

tss.storage <- ggplot(TSS_Budget_Final, aes(x=Year)) + 
  geom_col(aes(y=Storage_normalized), fill="darkseagreen3")+
  geom_hline(yintercept=0) +
  geom_line(aes(y=-50163.1), color="darkseagreen2", linetype = 'dotdash', size =1.5) +
  scale_x_continuous(limits=c(2007.5,2018.5),breaks=seq(from=2008,to=2018, by=1))+
  scale_y_continuous(labels=scales::comma)+
  ylab(expression(TSS~Storage~(kg~ha^{-1}~year^{-1}))) +
  theme_cowplot()
tss.storage

TSS_Budget_Final_long <- TSS_Budget_Final %>%
  select(Year, Storage_normalized:Output_normalized) %>%
  pivot_longer(cols=c(Storage_normalized:Output_normalized),
               names_to="Component",
               values_to = "TSS_kghayr")

TSS_norm_avg <- TSS_Budget_Final_long %>%
  group_by(Component) %>%
  summarize(mean=mean(TSS_kghayr, na.rm=T))

tss.inputs.outputs <- ggplot(subset(TSS_Budget_Final_long, Component != "Storage_normalized"), 
                             aes(Year, TSS_kghayr, fill= Component)) + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_hline(yintercept =0) +
  geom_line(aes(y=187055.4), color="darkslateblue", linetype = 'dashed', size=1.5) +
  geom_line(aes(y=237218.5), color="darkslategray3", linetype = 'dotted', size=1.5) +
  scale_x_continuous(limits=c(2007.5,2018.5), breaks=seq(from=2008, to=2018, by =1)) +
  scale_y_continuous(limits=c(0, 300000), 
                     labels=scales::comma, breaks=seq(from=0, to=300000, by=50000)) +
  scale_fill_manual(values = c("darkslateblue", "darkslategray3"), labels=c("Input", "Output")) +
  theme_cowplot() +
#  ylab(expression(TSS~Yields~(kg~ha^{-1}~year^{-1}))) +
  theme(legend.title = element_blank(),
        legend.position = c(0.1,0.9))

tss.inputs.outputs

ggplotly(tss.inputs.outputs)

#ggsave(tss.inputs.outputs, 
 #      file=paste0("results/figures/annual_box_model/tss.inputs.outputs.png"),
  #     width = 8, height = 6, units = "in", dpi = 300)


tss.annual.panel <- plot_grid(tss.inputs.outputs, tss.storage, labels = "AUTO")
tss.annual.panel

#ggsave(tss.annual.panel,
 #      file=paste0("results/Budget/annual_yield/tss.annual.panel.png"),
  #     width = 12, height = 6, units = "in", dpi = 300)


#TN, DIN and PN as percentages
TN_Budget_Final2<- tn_budget_final %>%
  select(Year, Input, Output, Storage)

PN_Budget_Final2<- pn_budget_final %>%
  select(Year, Input, Output, Storage)

DIN_Budget_Final2 <- din_budget_final %>%
  select(Year, Input, Output, Storage)

#PN as percentage of TN
PN_perc_TN <- PN_Budget_Final2[,2:3] / TN_Budget_Final2[,2:3] * 100
DIN_perc_TN <- DIN_Budget_Final2[,2:3] / TN_Budget_Final2[2:11,2:3] * 100


mean(PN_perc_TN$Input)
sd(PN_perc_TN$Input)

mean(DIN_perc_TN$Input)
sd(DIN_perc_TN$Input)

mean(PN_perc_TN$Output)
sd(PN_perc_TN$Output)

mean(DIN_perc_TN$Output)
sd(DIN_perc_TN$Output)

Budget_Components_PN <- budget_components %>%
  filter(Solute == "PN") %>%
  filter(Type != "APL") %>%
  filter(Year > 2012) %>%
  filter(Type != "Runoff") %>%
  select(-Balance, - Solute) %>%
  pivot_wider(names_from = "Type", 
              values_from = "Load_kgyr")

Budget_Components_PN$PercentRiver <- Budget_Components_PN$Riverine / Budget_Components_PN$APH * 100

#plot delta storage values for TN, PN, and DIN
#Combine them
head(pn_budget_final_long)

n_budget_combined <- full_join(pn_budget_final_long, tn_budget_final_long)
n_budget_combined <- full_join(n_budget_combined, din_budget_final_long)

#make long
n_budget_long <- n_budget_combined %>%
  pivot_longer(cols = PN_kghayr:DIN_kghayr, names_to = "Solute", values_to="Load_kghayr") %>%
  filter(Component == "Storage_normalized")


storage_a <- ggplot(n_budget_long, aes(Solute, Load_kghayr)) +
  geom_boxplot() +
  geom_point(position="jitter") +
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

#Combine the non nitrogen storage terms into a dataframe
other_budget_combined <- full_join(po4_budget_final_long, doc_budget_final_long)
other_budget_combined <- full_join(other_budget_combined, TSS_Budget_Final_long)


#make long
other_budget_long <- other_budget_combined %>%
  pivot_longer(cols = PO4_kghayr:TSS_kghayr, names_to = "Solute", values_to="Load_kghayr") %>%
  filter(Component == "Storage_normalized")

full_combo <- full_join(other_budget_long, n_budget_long)

type <- c("Nitrogen", "Nitrogen", "Nitrogen", "Phosphate", "Suspended Solids", "Dissolved Organic Carbon")
Solute <- c("DIN_kghayr", "PN_kghayr", "TN_kghayr", "PO4_kghayr", "TSS_kghayr", "DOC_kghayr")

key <- cbind(type, Solute)
key <- as.data.frame(key)

full_combo <- full_join(full_combo, key, by="Solute")

full_combo$type <- as.factor(full_combo$type)
full_combo$Solute <- as.factor(full_combo$Solute)

solute_vector <- c("a.nitrogen", "b.phosphate", "c.carbon", "d.solids")

full_combo$solute_vector <- ifelse(full_combo$type == "Phosphate", "B",
                                   ifelse(full_combo$type == "Dissolved Organic Carbon", "C", 
                                          ifelse(full_combo$type == "Nitrogen", "A", "D")))
  
#FIGURE FOR MANUSCRIPT
panel_fig <- ggplot(full_combo, aes(as.factor(Solute), Load_kghayr)) + 
  geom_boxplot(width=0.1,outlier.shape = NA) +
  geom_point(aes(color=as.factor(Year)), 
             position = position_jitterdodge(jitter.width=.1, dodge.width = 0.2),
             size=1.8) + 
  geom_hline(yintercept = 0, color="black", alpha=0.8) +
  scale_x_discrete(labels=c("PO4_kghayr"= ~PO[4], "DIN_kghayr" = "DIN", "PN_kghayr"= "PN", "TN_kghayr"= "TN", "DOC_kghayr"= "DOC", "TSS_kghayr" = "TSS")) +
   scale_color_viridis_d(option="viridis", name="Year", direction=-1) +
   theme_bw() +
   ylab(Storage~kg~ha^-1~year^-1) +
   facet_wrap(~solute_vector, scales="free", strip.position = "top") +
   theme(axis.title.x = element_blank(), 
        axis.text=element_text(size=15, color="black"),
        axis.title = element_text(size=18),
        strip.background = element_blank(),
        strip.placement= "inside",
        strip.text = element_text(size=14, face="bold", hjust=0, vjust=-1),
        legend.text = element_text(size=12),
        legend.title = element_text(size=18))
panel_fig

ggsave("./results/figures/manuscript_figures/panel_fig.jpg", plot=panel_fig,
       width=7, height = 6, units = "in", dpi=300)
