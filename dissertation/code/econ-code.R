library(dplyr)
library(lubridate)
library(plm)
library(ggplot2)
library(cowplot)
library(dotwhisker)
library(mgcv)
library(ggpubr)


## Harvard dataset cleaning
library(states)
library(countrycode)
data <- data %>% mutate(iso = countrycode(cowcode, "cown", "iso3c"))
data$iso[which(data$cowcode==345)] <- "SRB"
data$country_name[which(data$cowcode==345)] <- "Serbia"
data <- data %>% select(-date,-iso3c) %>%  select(cowcode,iso,everything())


## combine coop_money 合同数额表
coop_money_3 <- coop_money %>% select(-PlateType,-Cntrnm) %>% 
  filter(CoopMode==3)
coop_money_2 <- coop_money %>% select(-PlateType,-Cntrnm) %>% 
  filter(CoopMode==2)
coop_money_1 <- coop_money %>% select(-PlateType,-Cntrnm) %>% 
  filter(CoopMode==1)
data <- left_join(data, coop_money_3, by = c("iso" = "CountryCode", "year" = "SgnYear"))

## combine coop_turnover 合同完成额

coop_turnover_3 <- coop_turnover %>% select(-PlateType,-Cntrnm) %>% 
  filter(CoopMode==3)
coop_turnover_2 <- coop_turnover %>% select(-PlateType,-Cntrnm) %>% 
  filter(CoopMode==2)
coop_turnover_1 <- coop_turnover %>% select(-PlateType,-Cntrnm) %>% 
  filter(CoopMode==1)
data <- left_join(data, coop_turnover_3, by = c("iso" = "CountryCode", "year" = "SgnYear"))
data <- data %>% select(-CoopMode, -CoopMode.x, -CoopMode.y, -Turnover.y)

## combine CSMAR FDI 
FDI_mod <- FDI %>% select(-PlateType,-Cntrnm) 
data <- left_join(data, FDI_mod, by = c("iso" = "CountryCode", "year" = "SgnYear"))

## combine coop_person
coop_person_3 <- coop_person %>% select(-PlateType,-Cntrnm) %>% 
  filter(CoopMode==3)
coop_person_2 <- coop_person %>% select(-PlateType,-Cntrnm) %>% 
  filter(CoopMode==2)
coop_person_1 <- coop_person %>% select(-PlateType,-Cntrnm) %>% 
  filter(CoopMode==1)
data <- left_join(data, coop_person_3, by = c("iso" = "CountryCode", "year" = "SgnYear"))

## combine coop_person_flow
coop_person_flow <- coop_person_flow %>% rename("personnel_flow"="Personnel")
coop_person_flow_3 <- coop_person_flow %>% select(-PlateType,-Cntrnm) %>% 
  filter(CoopMode==3)
coop_person_flow_2 <- coop_person_flow %>% select(-PlateType,-Cntrnm) %>% 
  filter(CoopMode==2)
coop_person_flow_1 <- coop_person_flow %>% select(-PlateType,-Cntrnm) %>% 
  filter(CoopMode==1)
data <- left_join(data, coop_person_flow_3, by = c("iso" = "CountryCode", "year" = "SgnYear"))

## data frame modify
data$bri_dum <- factor(data$bri_dum, labels = c("Non-BRI","BRI"))
====================================
data$bri_dum <- as.numeric(data$bri_dum)
data$bri_dum[which(data$bri_dum==1)] <- 0
data$bri_dum[which(data$bri_dum==2)] <- 1

data <- data  %>% 
  mutate(ctr_aveproject_value = ContractVal/ContractNum) %>% 
  mutate(ctr_aveperson_value = ContractVal/Personnel) %>% 
  mutate(turnover_aveproject = Turnover.x/ContractNum, 
         turnover_aveperson = Turnover.x/Personnel)

#data_mod <- data %>% select(cowcode,iso,country_name,year,bri_dum,idealdist_chn,
                        v2x_polyarchy,diffchnv2x_polyarchy,minidist_chn,minidist_chnlog,
                        vae,pve,gee,rqe,rle,cce,ContractNum,ContractVal,
                        Turnover.x,FDIFlows,FDIStock,Personnel,personnel_flow) %>% 

  
# descriptive
  
## boxplot 
box_turnover <- ggplot(data = data, mapping = aes(x=bri_dum, y= log(Turnover.x))) +
    geom_boxplot()
box_personnel <- ggplot(data = data, mapping = aes(x=bri_dum, y= log(Personnel))) +
  geom_boxplot()
box_value <- ggplot(data = data, mapping = aes(x=bri_dum, y= log(ContractVal))) +
  geom_boxplot()
box_number <- ggplot(data = data, mapping = aes(x=bri_dum, y= ContractNum)) +
  geom_boxplot()
box_fdistock <- ggplot(data = data, mapping = aes(x=bri_dum, y= log(FDIStock))) +
  geom_boxplot()

boxplot<-ggarrange(box_turnover,box_personnel,box_value,box_number,box_fdistock, 
          ncol = 2, nrow = 3)
ggsave(boxplot, filename = "boxplot.png")


## Loess
### turnover
l_vae_turnover <- ggplot(data = data, aes(x = log(Turnover.x), y = vae, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="loess",se=TRUE) +
  labs(x = "Turnover", y = "Voice & Accountability")
  
l_pve_turnover <-ggplot(data = data, aes(x = log(Turnover.x), y = pve, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="loess",se=TRUE) +
  labs(x = "Turnover", y = "Stability")

l_gee_turnover <-ggplot(data = data, aes(x = log(Turnover.x), y = gee, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="loess",se=TRUE) +
  labs(x = "Turnover", y = "Government Effectiveness")

l_rqe_turnover <-ggplot(data = data, aes(x = log(Turnover.x), y = rqe, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="loess",se=TRUE) +
  labs(x = "Turnover", y = "Regulatory Quality")

l_rle_turnover <-ggplot(data = data, aes(x = log(Turnover.x), y = rle, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="loess",se=TRUE) +
  labs(x = "Turnover", y = "Rule of law")

l_cce_turnover <-ggplot(data = data, aes(x = log(Turnover.x), y = cce, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="loess",se=TRUE) +
  labs(x = "Turnover", y = "Corruption Control")

ggsave(ggarrange(l_vae_turnover, l_pve_turnover, l_gee_turnover, l_rqe_turnover,
                 l_rle_turnover, l_cce_turnover,legend = "none"), filename = "l_turnover.png")

### FDI

l_vae_turnover <- ggplot(data = data, aes(x = log(FDIStock), y = vae, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="loess",se=TRUE) +
  labs(x = "FDIStock", y = "Voice & Accountability")

l_pve_turnover <-ggplot(data = data, aes(x = log(FDIStock), y = pve, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="loess",se=TRUE) +
  labs(x = "FDIStock", y = "Stability")

l_gee_turnover <-ggplot(data = data, aes(x = log(FDIStock), y = gee, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="loess",se=TRUE) +
  labs(x = "FDIStock", y = "Government Effectiveness")

l_rqe_turnover <-ggplot(data = data, aes(x = log(FDIStock), y = rqe, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="loess",se=TRUE) +
  labs(x = "FDIStock", y = "Regulatory Quality")

l_rle_turnover <-ggplot(data = data, aes(x = log(FDIStock), y = rle, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="loess",se=TRUE) +
  labs(x = "FDIStock", y = "Rule of law")

l_cce_turnover <- ggplot(data = data, aes(x = log(FDIStock), y = cce, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="loess",se=TRUE) +
  labs(x = "FDIStock", y = "Corruption Control")

ggsave(ggarrange(l_vae_turnover, l_pve_turnover, l_gee_turnover, l_rqe_turnover,
                 l_rle_turnover, l_cce_turnover,legend = "none"), filename = "l_FDI.png")

### ContractValue
l_vae_turnover <- ggplot(data = data, aes(x = log(ContractVal), y = vae, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="loess",se=TRUE) +
  labs(x = "ContractVal", y = "Voice & Accountability")

l_pve_turnover <-ggplot(data = data, aes(x = log(ContractVal), y = pve, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="loess",se=TRUE) +
  labs(x = "ContractVal", y = "Stability")

l_gee_turnover <-ggplot(data = data, aes(x = log(ContractVal), y = gee, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="loess",se=TRUE) +
  labs(x = "ContractVal", y = "Government Effectiveness")

l_rqe_turnover <-ggplot(data = data, aes(x = log(ContractVal), y = rqe, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="loess",se=TRUE) +
  labs(x = "ContractVal", y = "Regulatory Quality")

l_rle_turnover <-ggplot(data = data, aes(x = log(ContractVal), y = rle, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="loess",se=TRUE) +
  labs(x = "ContractVal", y = "Rule of law")

l_cce_turnover <-ggplot(data = data, aes(x = log(ContractVal), y = cce, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="loess",se=TRUE) +
  labs(x = "ContractVal", y = "Corruption Control")

ggsave(ggarrange(l_vae_turnover, l_pve_turnover, l_gee_turnover, l_rqe_turnover,
                 l_rle_turnover, l_cce_turnover,legend = "none"), filename = "l_value.png")

### personnel

l_vae_turnover <- ggplot(data = data, aes(x = log(Personnel), y = vae, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="loess",se=TRUE) +
  labs(x = "Personnel", y = "Voice & Accountability")

l_pve_turnover <-ggplot(data = data, aes(x = log(Personnel), y = pve, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="loess",se=TRUE) +
  labs(x = "Personnel", y = "Stability")

l_gee_turnover <-ggplot(data = data, aes(x = log(Personnel), y = gee, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="loess",se=TRUE) +
  labs(x = "Personnel", y = "Government Effectiveness")

l_rqe_turnover <-ggplot(data = data, aes(x = log(Personnel), y = rqe, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="loess",se=TRUE) +
  labs(x = "Personnel", y = "Regulatory Quality")

l_rle_turnover <-ggplot(data = data, aes(x = log(Personnel), y = rle, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="loess",se=TRUE) +
  labs(x = "Personnel", y = "Rule of law")

l_cce_turnover <-ggplot(data = data, aes(x = log(Personnel), y = cce, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="loess",se=TRUE) +
  labs(x = "Personnel", y = "Corruption Control")

ggsave(ggarrange(l_vae_turnover, l_pve_turnover, l_gee_turnover, l_rqe_turnover,
                 l_rle_turnover, l_cce_turnover,legend = "none"), filename = "l_person.png")



## GAM
### turnover

gam_vae_turnover <- ggplot(data = data, aes(x = log(Turnover.x), y = vae)) +  
  geom_point() + 
  geom_smooth(method="gam",se=TRUE, formula = y ~ s(x, bs = "cs")) +
  theme(legend.position = 'none')+
  labs(x = "Turnover", y = "Voice & Accountability")
 
gam_pve_turnover <-ggplot(data = data, aes(x = log(Turnover.x), y = pve, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="gam",se=TRUE, formula = y ~ s(x, bs = "cs")) + 
  theme(legend.position = 'none')+
  labs(x = "Turnover", y = "Stability")

gam_gee_turnover <-ggplot(data = data, aes(x = log(Turnover.x), y = gee, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="gam",se=TRUE, formula = y ~ s(x, bs = "cs")) +
  theme(legend.position = 'none')+
  labs(x = "Turnover", y = "Government Effectiveness")

gam_rqe_turnover <-ggplot(data = data, aes(x = log(Turnover.x), y = rqe, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="gam",se=TRUE, formula = y ~ s(x, bs = "cs")) + 
  theme(legend.position = 'none')+
  labs(x = "Turnover", y = "Regulatory Quality")

gam_rle_turnover <-ggplot(data = data, aes(x = log(Turnover.x), y = rle, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="gam",se=TRUE, formula = y ~ s(x, bs = "cs")) + 
  theme(legend.position = 'none')+
  labs(x = "Turnover", y = "Rule of law")

gam_cce_turnover <-ggplot(data = data, aes(x = log(Turnover.x), y = cce, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="gam",se=TRUE, formula = y ~ s(x, bs = "cs")) +
  theme(legend.position = 'none')+
  labs(x = "Turnover", y = "Corruption Control")

ggsave(ggarrange(gam_vae_turnover, gam_pve_turnover, gam_gee_turnover, gam_rqe_turnover,
          gam_rle_turnover, gam_cce_turnover,legend = "none"), filename = "gam_turnover.png")

---------------------------------------------------------
### FDI

gam_vae_fdi <- ggplot(data = data, aes(x = log(FDIStock), y = vae, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="gam",se=TRUE, formula = y ~ s(x, bs = "cs")) +
  theme(legend.position = 'none')+
  labs(x = "FDIStock", y = "Voice & Accountability")
  
gam_pve_fdi <-ggplot(data = data, aes(x = log(Turnover.x), y = pve, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="gam",se=TRUE, formula = y ~ s(x, bs = "cs")) + 
  theme(legend.position = 'none')+
  labs(x = "FDIStock", y = "Stability")

gam_gee_fdi <-ggplot(data = data, aes(x = log(Turnover.x), y = gee, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="gam",se=TRUE, formula = y ~ s(x, bs = "cs")) +
  theme(legend.position = 'none')+
  labs(x = "FDIStock", y = "Government Effectiveness")

gam_rqe_fdi <-ggplot(data = data, aes(x = log(Turnover.x), y = rqe, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="gam",se=TRUE, formula = y ~ s(x, bs = "cs")) + 
  theme(legend.position = 'none')+
  labs(x = "FDIStock", y = "Regulatory Quality")

gam_rle_fdi <-ggplot(data = data, aes(x = log(Turnover.x), y = rle, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="gam",se=TRUE, formula = y ~ s(x, bs = "cs")) + 
  theme(legend.position = 'none')+
  labs(x = "FDIStock", y = "Rule of law")

gam_cce_fdi <-ggplot(data = data, aes(x = log(Turnover.x), y = cce, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="gam",se=TRUE, formula = y ~ s(x, bs = "cs")) +
  theme(legend.position = 'none')+
  labs(x = "FDIStock", y = "Corruption Control")

ggsave(ggarrange(gam_vae_fdi, gam_pve_fdi, gam_gee_fdi, gam_rqe_fdi,
                 gam_rle_fdi, gam_cce_fdi,legend = "none"), filename = "gam_fdi.png")

# contract number
gam_vae_fdi <- ggplot(data = data, aes(x = log(ContractVal), y = vae, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="gam",se=TRUE, formula = y ~ s(x, bs = "cs")) +
  theme(legend.position = 'none')+
  labs(x = "ContractVal", y = "Voice & Accountability")

gam_pve_fdi <-ggplot(data = data, aes(x = log(ContractVal), y = pve, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="gam",se=TRUE, formula = y ~ s(x, bs = "cs")) + 
  theme(legend.position = 'none')+
  labs(x = "ContractVal", y = "Stability")

gam_gee_fdi <-ggplot(data = data, aes(x = log(ContractVal), y = gee, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="gam",se=TRUE, formula = y ~ s(x, bs = "cs")) +
  theme(legend.position = 'none')+
  labs(x = "ContractVal", y = "Government Effectiveness")

gam_rqe_fdi <-ggplot(data = data, aes(x = log(ContractVal), y = rqe, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="gam",se=TRUE, formula = y ~ s(x, bs = "cs")) + 
  theme(legend.position = 'none')+
  labs(x = "ContractVal", y = "Regulatory Quality")

gam_rle_fdi <-ggplot(data = data, aes(x = log(ContractVal), y = rle, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="gam",se=TRUE, formula = y ~ s(x, bs = "cs")) + 
  theme(legend.position = 'none')+
  labs(x = "ContractVal", y = "Rule of law")

gam_cce_fdi <-ggplot(data = data, aes(x = log(ContractVal), y = cce, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="gam",se=TRUE, formula = y ~ s(x, bs = "cs")) +
  theme(legend.position = 'none')+
  labs(x = "ContractVal", y = "Corruption Control")

ggsave(ggarrange(gam_vae_fdi, gam_pve_fdi, gam_gee_fdi, gam_rqe_fdi,
                 gam_rle_fdi, gam_cce_fdi,legend = "none"), filename = "gam_value.png")


### personnel
gam_vae_fdi <- ggplot(data = data, aes(x = log(Personnel), y = vae, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="gam",se=TRUE, formula = y ~ s(x, bs = "cs")) +
  theme(legend.position = 'none')+
  labs(x = "Personnel", y = "Voice & Accountability")

gam_pve_fdi <-ggplot(data = data, aes(x = log(Personnel), y = pve, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="gam",se=TRUE, formula = y ~ s(x, bs = "cs")) + 
  theme(legend.position = 'none')+
  labs(x = "Personnel", y = "Stability")

gam_gee_fdi <-ggplot(data = data, aes(x = log(Personnel), y = gee, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="gam",se=TRUE, formula = y ~ s(x, bs = "cs")) +
  theme(legend.position = 'none')+
  labs(x = "Personnel", y = "Government Effectiveness")

gam_rqe_fdi <-ggplot(data = data, aes(x = log(Personnel), y = rqe, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="gam",se=TRUE, formula = y ~ s(x, bs = "cs")) + 
  theme(legend.position = 'none')+
  labs(x = "Personnel", y = "Regulatory Quality")

gam_rle_fdi <-ggplot(data = data, aes(x = log(Personnel), y = rle, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="gam",se=TRUE, formula = y ~ s(x, bs = "cs")) + 
  theme(legend.position = 'none')+
  labs(x = "Personnel", y = "Rule of law")

gam_cce_fdi <-ggplot(data = data, aes(x = log(Personnel), y = cce, color = bri_dum)) +  
  geom_point() + 
  geom_smooth(method="gam",se=TRUE, formula = y ~ s(x, bs = "cs")) +
  theme(legend.position = 'none')+
  labs(x = "Personnel", y = "Corruption Control")

ggsave(ggarrange(gam_vae_fdi, gam_pve_fdi, gam_gee_fdi, gam_rqe_fdi,
                 gam_rle_fdi, gam_cce_fdi,legend = "none"), filename = "gam_person.png")





## basic frame reconstruct
basic_frame <- data %>% select(cowcode,country_name,year,iso3c,bri_dum)
basic_frame <- basic_frame %>% mutate(iso = 
                                        countrycode(cowcode, "cown", "iso3c"))
basic_frame$iso[which(basic_frame$cowcode==345)] <- "SRB"
basic_frame$country_name[which(basic_frame$cowcode==345)] <- "Serbia"
basic_frame <- data %>% 
  select(cowcode,country_name,year,iso,bri_dum, idealdist_chn,
         v2x_polyarchy, diffchnv2x_polyarchy, minidist_chnlog, )


