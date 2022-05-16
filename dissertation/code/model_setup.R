library(dplyr)
library(plm)
library(ggplot2)
library(cowplot)
library(dotwhisker)
library(mgcv)
library(stargazer)
library(broom)
library(panelView)

stargazer(data[1:5], title = "描述性统计分析", type = "html",
          no.space = TRUE, summary.stat = c('median','mean','sd','min','max'),
          out = "./descriptive.html")


# model setup
## aggregated
form_vae <- as.formula("vae ~ bri_dum + log(gdppc)  +
                            idealdist_chn + v2x_polyarchy + 
                            log(ContractVal) + bri_dum:log(ContractVal) + 
                            log(FDIStock) + bri_dum:log(FDIStock) + 
                            log(Turnover.x) + bri_dum:log(Turnover.x) +
                            log(Personnel) + bri_dum:log(Personnel)" )
form_pve <- as.formula("pve ~ bri_dum + log(gdppc)  +
                            idealdist_chn + v2x_polyarchy + 
                            log(ContractVal) + bri_dum:log(ContractVal) + 
                            log(FDIStock) + bri_dum:log(FDIStock) + 
                            log(Turnover.x) + bri_dum:log(Turnover.x) +
                            log(Personnel) + bri_dum:log(Personnel)" )
form_gee <- as.formula("gee ~ bri_dum + log(gdppc)  +
                            idealdist_chn + v2x_polyarchy + 
                            log(ContractVal) + bri_dum:log(ContractVal) + 
                            log(FDIStock) + bri_dum:log(FDIStock) + 
                            log(Turnover.x) + bri_dum:log(Turnover.x) +
                            log(Personnel) + bri_dum:log(Personnel)" )
form_rqe <- as.formula("rqe ~ bri_dum + log(gdppc)  +
                            idealdist_chn + v2x_polyarchy + 
                            log(ContractVal) + bri_dum:log(ContractVal) + 
                            log(FDIStock) + bri_dum:log(FDIStock) + 
                            log(Turnover.x) + bri_dum:log(Turnover.x) +
                            log(Personnel) + bri_dum:log(Personnel)" )
form_rle <- as.formula("rle ~ bri_dum + log(gdppc)  +
                            idealdist_chn + v2x_polyarchy + 
                            log(ContractVal) + bri_dum:log(ContractVal) + 
                            log(FDIStock) + bri_dum:log(FDIStock) + 
                            log(Turnover.x) + bri_dum:log(Turnover.x) +
                            log(Personnel) + bri_dum:log(Personnel)" )
form_cce <- as.formula("cce ~ bri_dum + log(gdppc)  +
                            idealdist_chn + v2x_polyarchy + 
                            log(ContractVal) + bri_dum:log(ContractVal) + 
                            log(FDIStock) + bri_dum:log(FDIStock) + 
                            log(Turnover.x) + bri_dum:log(Turnover.x) +
                            log(Personnel) + bri_dum:log(Personnel)" )

# DID
library(did)
example_attgt <- att_gt(yname = "vae",
                        tname = "year",
                        idname = "id",
                        gname = "treat_year",
                        xformla = ~ age + sexo + employ + edu_year + wealth +
                          econ + left,
                        panel = FALSE,
                        allow_unbalanced_panel = TRUE,
                        data = data)
ggdid(example_attgt, ylim = c(-.3,.3))
summary(example_attgt)
mw.dyn <- aggte(example_attgt, type = "dynamic")
summary(mw.dyn)
ggdid(mw.dyn, ylim = c(-.3,.3))


# fixed effect model
library(fect)
library(panelView)
panelview(pve ~ bri_dum, data = data, index = c("cowcode","year"), 
          axis.lab = "time", xlab = "Time", ylab = "Unit", 
          background = "white", main = "Treatment Status")


fect.vae <- fect(vae ~ bri_dum + log(gdppc)  +
                   idealdist_chn + v2x_polyarchy, data = data, index = c("cowcode","year"), 
                 method = "fe", force = "two-way", se = TRUE, parallel = TRUE, nboots = 1000)
fect_vae <- plot(fect.vae, main = "Estimated ATT (VA)", 
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
test_vae <- plot(fect.vae, type = "equiv", ylim = c(-0.1,0.1), 
     cex.legend = 0.6, main = "Pre-trend Test (VA)", cex.text = 0.8)

fect.pve <- fect(pve ~ bri_dum + log(gdppc)  +
                   idealdist_chn + v2x_polyarchy, data = data, index = c("cowcode","year"), 
                 method = "fe", force = "two-way", se = TRUE, parallel = TRUE, nboots = 1000)
fect_pve <- plot(fect.pve, main = "Estimated ATT (PV)",  
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
test_pve <-plot(fect.pve, type = "equiv", ylim = c(-0.1,0.1), 
     cex.legend = 0.6, main = "Pre-trend Test (PV)", cex.text = 0.8)


fect.gee <- fect(gee ~ bri_dum + log(gdppc)  +
                   idealdist_chn + v2x_polyarchy, data = data, index = c("cowcode","year"), 
                 method = "fe", force = "two-way", se = TRUE, parallel = TRUE, nboots = 1000)
fect_gee <- plot(fect.gee, main = "Estimated ATT (GE)",
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
test_gee <- plot(fect.gee, type = "equiv", ylim = c(-0.1,0.1), 
     cex.legend = 0.6, main = "Pre-trend Test (GE)", cex.text = 0.8)


fect.rqe <- fect(rqe ~ bri_dum + log(gdppc)  +
                   idealdist_chn + v2x_polyarchy, data = data, index = c("cowcode","year"), 
                 method = "fe", force = "two-way", se = TRUE, parallel = TRUE, nboots = 1000)
fect_rqe <- plot(fect.rqe, main = "Estimated ATT (RQ)", 
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
test_rqe <- plot(fect.rqe, type = "equiv", ylim = c(-0.1,0.1), 
     cex.legend = 0.6, main = "Pre-trend Test (RQ)", cex.text = 0.8)


fect.rle <- fect(rle ~ bri_dum + log(gdppc)  +
                   idealdist_chn + v2x_polyarchy, data = data, index = c("cowcode","year"), 
                 method = "fe", force = "two-way", se = TRUE, parallel = TRUE, nboots = 1000)
fect_rle <- plot(fect.rle, main = "Estimated ATT (RL)", 
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
test_rle <- plot(fect.rle, type = "equiv", ylim = c(-0.1,0.1), 
     cex.legend = 0.6, main = "Pre-trend Test (RL)", cex.text = 0.8)


fect.cce <- fect(cce ~ bri_dum + log(gdppc)  +
                   idealdist_chn + v2x_polyarchy, data = data, index = c("cowcode","year"), 
                 method = "fe", force = "two-way", se = TRUE, parallel = TRUE, nboots = 1000)
fect_cce <- plot(fect.cce, main = "Estimated ATT (CC)", 
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
test_cce <- plot(fect.cce, type = "equiv", ylim = c(-0.1,0.1), 
     cex.legend = 0.6, main = "Pre-trend Test (CC)", cex.text = 0.8)

library(ggpubr)
ggsave(ggarrange(fect_vae, fect_pve, fect_gee, fect_rqe,
                 fect_rle, fect_cce,legend = "none"), filename = "fect_result.png")
ggsave(ggarrange(test_vae, test_pve, test_gee, test_rqe,
                 test_rle, test_cce,legend = "none"), filename = "fect_test.png")


fix_vae <- plm(form_vae, data = data, index = c("cowcode", "year"), 
          model = "within", effect = "twoways")
fix_pve <- plm(form_pve, data = data, index = c("cowcode", "year"), 
               model = "within", effect = "twoways")
fix_gee <- plm(form_gee, data = data, index = c("cowcode", "year"), 
               model = "within", effect = "twoways")
fix_rqe <- plm(form_rqe, data = data, index = c("cowcode", "year"), 
               model = "within", effect = "twoways")
fix_rle <- plm(form_rle, data = data, index = c("cowcode", "year"), 
               model = "within", effect = "twoways")
fix_cce <- plm(form_cce, data = data, index = c("cowcode", "year"), 
               model = "within", effect = "twoways")

stargazer(fix_pve, fix_gee, fix_rqe, fix_vae, fix_rle, fix_cce,
          title = "对治理公平性的回归", type = "html", align = TRUE,
          no.space = FALSE, digits= 3, font.size = "small",
          dep.var.labels=c("政治稳定","政府效率","市场监管质量","政民互动",
                           "法治程度","反腐力度"),
          covariate.labels=c("BRI协议", "人均GDP(对数)","与中国外交立场距离(对数)",
                             "政体","合同额(对数)", "对外直接投资(对数)","营业额(对数)","派出人数(对数)",
                             "BRI协议×合同额(对数)","BRI协议×对外直接投资(对数)","BRI协议×营业额(对数)",
                             "BRI协议×派出人数(对数)"), out = "./fix.html")



fix_plot <- dwplot(list(fix_vae,fix_pve,fix_gee,fix_rqe,fix_rle,fix_cce)) + theme_bw()
ggsave(fix_plot, filename = "fix_flot_nonse.png")
summary(fix_vae)
summary(fix_pve)
summary(fix_gee)
summary(fix_rqe)
summary(fix_rle)
summary(fix_cce)

rob_se <- list(sqrt(diag(vcovHC(fix_vae, type = "HC1"))),
               sqrt(diag(vcovHC(fix_pve, type = "HC1"))),
               sqrt(diag(vcovHC(fix_gee, type = "HC1"))),
               sqrt(diag(vcovHC(fix_rqe, type = "HC1"))),
               sqrt(diag(vcovHC(fix_rle, type = "HC1"))),
               sqrt(diag(vcovHC(fix_cce, type = "HC1"))))



# loess model
l_vae <-loess(vae ~ bri_dum + ContractVal + Turnover.x + FDIStock, data = data)
l_pve <- loess(form_pve, data = data)
l_gee <- loess(form_gee, data = data)
l_rqe <- loess(form_rqe, data = data)
l_rle <- loess(form_rle, data = data)
l_cce <- loess(form_cce, data = data)

# GAM model
gam_vae <- gam(form_vae, data = data)
gam_pve <- gam(form_pve, data = data)
gam_gee <- gam(form_gee, data = data)
gam_rqe <- gam(form_rqe, data = data)
gam_rle <- gam(form_rle, data = data)
gam_cce <- gam(form_cce, data = data)

summary(gam_vae)
summary(gam_pve)
summary(gam_gee)
summary(gam_rqe)
summary(gam_rle)
summary(gam_cce)

stargazer(gam_pve, gam_gee, gam_rqe, gam_vae, gam_rle, gam_cce,
          title = "广义相加模型", type = "html", align = TRUE,
          no.space = FALSE, digits= 3, font.size = "small",
          dep.var.labels=c("政治稳定","政府效率","市场监管质量","政民互动",
                           "法治程度","反腐力度"),
          covariate.labels=c("BRI协议", "人均GDP(对数)","与中国外交立场距离(对数)",
                             "政体","合同额(对数)", "对外直接投资(对数)","营业额(对数)","派出人数(对数)",
                             "BRI协议×合同额(对数)","BRI协议×对外直接投资(对数)","BRI协议×营业额(对数)",
                             "BRI协议×派出人数(对数)"), out = "./gam.html")

## GAM coefficient plot
terms_pve <- predict(gam_pve, type = "terms")
terms_pve <- cbind(pve = data$pve, terms_pve)
tframe_pve <- as.data.frame(scale(terms_pve, scale = FALSE))
tframe_pve <- tframe_pve %>% mutate(number= row.names(tframe_pve))
colnames(tframe_pve) <- gsub('[()]','', colnames(tframe_pve))

vars = c("Personnel", "ContractVal", "FDIStock", "Turnover.x")
data_gam <- data %>% mutate(number= row.names(data)) %>% 
  select(number,Personnel,ContractVal,FDIStock,Turnover.x)
pframe_pve <- left_join(tframe_pve,data_gam, by= c("number"="number"))
ggplot(pframe_pve, aes(x=log(Personnel))) +
  geom_point(aes(y= logPersonnel)) +
  geom_smooth(aes(y = pve), se = FALSE)

# GLS
gls_vae <- gls(vae ~ bri_dum + gdppc, data = data, na.action = na.omit(data))
