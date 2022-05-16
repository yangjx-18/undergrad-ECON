library(states)
library(countrycode)
library(dplyr)
library(lubridate)
base_cow <- state_panel(2014, 2019, by = "year", partial = "any",useGW = FALSE)
base_cow <- base_cow%>%
  mutate(country_name = 
           countrycode(cowcode, "cown", "country.name")) %>%
  mutate(theyear=year(date)) %>% 
  mutate(iso3c=countrycode(gwcode,"gwn","iso3c"))
#Clean data of corruption
corruption <- X2012_2019 %>% select(-contains("Rank"),-contains("error"),-contains("Sources"),-contains("2012"),-contains("2013")) %>% 
  mutate(ccode = countrycode(ISO3, "iso3c", "cown"))
corruption<-corruption %>% rename("2019"=`CPI score 2019`,"2018"=`CPI score 2018`,"2017"=`CPI score 2017`,"2016"=`CPI score 2016`,"2015"=`CPI score 2015`,"2014"=`CPI score 2014`)
corruption<- corruption %>% mutate(year=as.integer(year))
#无代码：HKG, KSV, SRB

#Clean GDPPC_GROWTH
gdppc_gro<-gdppc_growth %>% select(-starts_with("19")) %>% select(-"2020")
gdppc_gro<-gather(gdppc_gro,key = "year",value = "number",'2000':'2019')
gdppc_gro<-gdppc_gro %>% select(`Country Name`,year,everything()) %>% filter(year>=2014)%>%
  mutate(ccode = countrycode(`Country Code`, "iso3c", "cown"))
#无代码：

#Clean GDP_PER_2019
gdppc<- GDP_per_2019%>% select(-starts_with("19")) %>% select(-"2020")
gdppc<-gather(gdppc,key = "year",value = "number",'2000':'2019')
gdppc<-gdppc %>% select(`Country Name`,year,everything()) %>% filter(year>=2014)%>%
  mutate(ccode = countrycode(`Country Code`, "iso3c", "cown"))
#无代码：

#Clean export_all
export_all<-export_all %>% select(Year,`Partner Code`,Partner,`Partner ISO`,`Trade Value (US$)`) %>% mutate(ccode = countrycode(`Partner ISO`, "iso3c", "cown"))

#general base
base_df <- left_join(base_df, corruption, by = c("cowcode" = "ccode", "year" = "year"))
base_df <- left_join(base_df, export_all, by = c("cowcode" = "ccode", "year" = "Year"))
base_df <- left_join(base_df, gdppc, by = c("cowcode" = "ccode", "year" = "year"))
base_df <- left_join(base_df, pts_set, by = c("cowcode" = "COW_Code_N", "year" = "Year"))
base_df <- left_join(base_df, gdppc_gro, by = c("cowcode" = "ccode", "year" = "year"))
base_df <- left_join(base_df, unvote_set, by = c("cowcode" = "ccode2", "year" = "year"))
base_df <- base_df %>% select(cowcode,country_name,year,ISO3,corrupt,`Trade Value (US$)`,gdppc,gdppc_gro,PTS_A,PTS_H,PTS_S,agree,IdealPointDistance)

#bri model2
bri_econ<- left_join(bri_export, bri_fdi, by = c("ccode" = "ccode", "SgnYear" = "SgnYear"))
bri_econ<- left_join(bri_econ, bri_revenue, by = c("ccode" = "ccode", "SgnYear" = "SgnYear"))
bri_econ<-bri_econ %>% filter(SgnYear>=2014) %>% select(-CountryCode.y,-Cntrnm.y,-CountryCode,-Cntrnm)
model2 <- left_join(bri_econ,top2, by = c("ccode" = "cowcode", "SgnYear" = "year"))


