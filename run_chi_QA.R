#compare old data in SQL with new data 
rm(list=ls())
pacman::p_load(dplyr, foreign, odbc, DBI, dbplyr, epiDisplay, data.table, janitor, rads, naniar, openxlsx)
setwd("c:/pers_linsong/CHI_brfss") 

# connect to sql----
db50 <- odbc::dbConnect(odbc::odbc(),
                        Driver = "SQL Server",
                        Server = "KCITSQLPRPDBM50",
                        Database = "PHExtractStore")
# pull data from sql ----
olddata_raw <- setDT(DBI::dbGetQuery(conn = db50, statement = "SELECT * FROM [PHExtractStore].[APDE].[brfss_results]"))
# close sql connection ----
DBI::dbDisconnect(db50)
rm(db50)
names(olddata_raw)
tab1(olddata_raw$indicator_key, graph = F)

#Note: disab2 and SSB cannot be compared.
olddata <- subset(olddata_raw, indicator_key!="_pastaer_v1" & indicator_key!="_pastaer_v2" &
                               indicator_key!="dislimit" & indicator_key!="emtsuprt_v1")

olddata <- subset(olddata, cat1_varname!="hracode")
olddata <- subset(olddata, cat1_varname!="non-Hisp")
olddata <- olddata %>% mutate(indicator_key  = case_when(indicator_key=='crcscrn2' ~'x_crcrec', 
                                                         indicator_key=='ecigs_v1' ~'ecignow1', 
                                                         indicator_key=='menthlth' ~'fmd', 
                                                         TRUE ~indicator_key))

olddata <- olddata %>% mutate(cat1  = case_when(cat1=='Bigcities' ~'Big cities',
                                                cat1=="Race/ethnicity" & cat1_varname=="race3" ~"Race",
                                                cat1=="Race/ethnicity" & cat1_varname=="race3" & cat1_group=="Hispanic" ~"Ethnicity",
                                                cat1=='Household Income' ~'Household income',
                                                        TRUE ~cat1))

olddata <- olddata %>% mutate(cat1_varname  = case_when(indicator_key=='x_crcrec' & cat1=='Age' ~'age_crc',
                                                        indicator_key=='mam2yrs'  & cat1=='Age' ~'age_mam', 
                                                        indicator_key=='pap3yrs'  & cat1=='Age' ~'age_pap',
                                                         cat1=='Age' ~'age5_v2', 
                                                         cat1=='Gender' ~'chi_sex', 
                                                         cat1=='Household income' ~'income6b', 
                                                         cat1=="Ethnicity" & cat1_varname=='hispanic' ~'race3', 
                                                         cat1=='Sexual orientation' ~'chi_sexorien_2',
                                                         cat1=='Regions' ~'chi_geo_region',
                                                         cat1=='Big cities' ~'bigcities',
                                                         cat1=='King County' ~'chi_geo_kc', 
                                                         TRUE ~cat1_varname))

olddata <- olddata %>% mutate(cat1_group  = case_when(cat1_group=='<$15,000' ~'<$15k', 
                                                      cat1_group=="$75,000+" ~"$75k+", 
                                                      cat1_group=="Hisp" ~"Hispanic", TRUE ~cat1_group))

olddata <- olddata %>% mutate(cat2  = case_when(cat2=="Overall" ~"King County",
                                                cat1=="King County" & is.na(cat2) ~"King County",
                                                cat2_varname=="age5" ~"Age",
                                                cat2=='Household Income' ~'Household income', TRUE ~cat2))

olddata <- olddata %>% mutate(cat2=case_when(is.na(cat2) & is.na(cat2_varname) & is.na(cat2_group) ~"King County", TRUE ~cat2))
olddata <- olddata %>% mutate(cat2_varname=case_when(is.na(cat2) & is.na(cat2_varname) & is.na(cat2_group) ~"chi_geo_kc", TRUE ~cat2_varname))
olddata <- olddata %>% mutate(cat2_group=case_when(is.na(cat2) & is.na(cat2_varname) & is.na(cat2_group) ~"King County", TRUE ~cat2_group))

olddata <- olddata %>% mutate(cat2_varname  = case_when(indicator_key=='x_crcrec' & cat2=='Age' ~'age_crc',
                                                        indicator_key=='mam2yrs'  & cat2=='Age' ~'age_mam', 
                                                        indicator_key=='pap3yrs'  & cat2=='Age' ~'age_pap',
                                                        cat2=='Age' ~'age5_v2',
                                                        cat2=='Gender' ~'chi_sex', 
                                                        cat2=='Household income' ~'income6b', 
                                                        cat2=="Ethnicity" & cat2_varname=='hispanic' ~'race3', 
                                                        cat2=='Sexual orientation' ~'chi_sexorien_2',
                                                        cat2=='Regions' ~'chi_geo_region',
                                                        cat2=='Big cities' ~'bigcities',
                                                        cat2=='King County' ~'chi_geo_kc',
                                                        cat2=='Overall' ~'chi_geo_kc',
                                                        TRUE ~cat2_varname))

olddata <- olddata %>% mutate(cat2_group  = case_when(cat2=='King County' ~'King County', 
                                                      cat2_group=="LBG" ~"LGB", 
                                                      cat2_group=='<$15,000' ~'<$15k', 
                                                      cat2_group=="$75,000+" ~"$75k+", 
                                                      cat2_group=="Hisp" ~"Hispanic", TRUE ~cat2_group))

olddata <- olddata %>% mutate(year = case_when(year=="2012, 2014" ~ "2012 & 2014", 
                                             year=="2013, 2015" ~ "2013 & 2015", 
                                             year=="2015, 2017" ~ "2015 & 2017", 
                                             year=="2016, 2018" ~ "2016 & 2018", 
                                             year=="2017, 2019" ~ "2017 & 2019", 
                                             year=="2018, 2020" ~ "2018 & 2020",
                                             year=="2019, 2021" ~ "2019 & 2021",
                                             year=="2016, 2018, 2020" ~ "2016, 2018 & 2020", 
                                             TRUE ~ year))

olddata$numerator <- as.integer(olddata$numerator)
olddata$denominator <- as.integer(olddata$denominator)
olddata$result <- as.numeric(olddata$result)

olddata <- olddata %>% mutate(drop1  = case_when(indicator_key=="mam2yrs" & cat1=="Gender" ~1,
                                                 indicator_key=="pap3yrs" & cat1=="Gender" ~2,
                                                 indicator_key=="ecignow1" | indicator_key=="fnotlast" ~3,
                                                 year=="2010-2011" | year=="2011-2012" | year=="2009" | 
                                                 year=="2012" | year=="2018" ~4, 
                                                 TRUE ~0))

olddata <- subset(olddata, drop1==0)
olddata <- subset(olddata, cat1_varname!="income6b")
olddata <- subset(olddata, cat2_varname!="income6b")
olddata <- subset(olddata, cat1_varname!="hracode")
olddata <- subset(olddata, cat2_varname!="hracode")
olddata <- subset(olddata, cat1_varname!="chi_geo_region")
olddata <- subset(olddata, cat2_varname!="chi_geo_region")

olddata <- subset(olddata, denominator>=50)

olddata1 <- subset(olddata, tab=="_kingcounty" | tab=="demgroups")
olddata1 <- olddata1[ , c("indicator_key", "tab", "year", "cat1", "cat1_varname", "cat1_group", 
                          "cat2", "cat2_varname", "cat2_group", "result", "numerator", "denominator")]

olddata1 <- olddata1 %>% rename("result_old"="result", "numerator_old" = "numerator", "denominator_old" = "denominator")

olddata2 <- subset(olddata, tab=="trends")
olddata2 <- olddata2[ , c("indicator_key", "year", "cat1", "cat1_varname", "cat1_group", "result", "numerator", "denominator")]
olddata2 <- olddata2 %>% rename("result_old"="result", "numerator_old" = "numerator", "denominator_old" = "denominator")

write.csv(olddata1, "olddata.csv", row.names = F)

#-----
brfss_all <- read.xlsx("brfss_all.xlsx")

newdata1 <- subset(brfss_all, tab=="_kingcounty" |  tab=="demgroups")
newdata1 <- subset(newdata1, cat1!="Cities/neighborhoods")

newdata1 <- newdata1[ , c("indicator_key", "tab", "year", "cat1", "cat1_varname", "cat1_group", 
                          "cat2", "cat2_varname", "cat2_group", "result", "numerator", "denominator")]
newdata1 <- newdata1 %>% rename("result_new"="result", "numerator_new" = "numerator", "denominator_new" = "denominator")

newdata1 <- subset(newdata1, indicator_key!="x_veglt1a")
newdata1 <- subset(newdata1, cat1_varname!="income6b")
newdata1 <- subset(newdata1, cat2_varname!="income6b")
newdata1 <- subset(newdata1, cat1_varname!="chi_geo_region")
newdata1 <- subset(newdata1, cat2_varname!="chi_geo_region")

newdata1 <- subset(newdata1, cat2_group!="Other cities")

newdata1 <- subset(newdata1, denominator_new>=50)

newdata2 <- subset(brfss_all, tab=="trends")
newdata2 <- newdata2 %>% mutate(drop2  = case_when(year=="2018-2020" & indicator_key=="asthnow" ~1,
                                                   year=="2018-2020" & indicator_key=="cvdheart" ~1,
                                                   year=="2018-2020" & indicator_key=="cvdstrok" ~1,
                                                   year=="2018-2020" & indicator_key=="flushot_v1" ~1,
                                                   year=="2018-2020" & indicator_key=="flushot_v2" ~1,
                                                   year=="2018-2020" & indicator_key=="fnotlast" ~1,
                                                   year=="2018-2020" & indicator_key=="genhlth2" ~1,
                                                   year=="2018-2020" & indicator_key=="mjpast30" ~1,
                                                   year=="2018-2020" & indicator_key=="overw1" ~1,
                                                   year=="2018-2020" & indicator_key=="persdoc" ~1,
                                                   year=="2018-2020" & indicator_key=="pneumvac" ~1,
                                                   year=="2018 & 2020" & indicator_key=="mam2yrs" ~1,
                                                   year=="2018 & 2020" & indicator_key=="pap3yrs" ~1,
                                                   indicator_key=="disabil" & year=="2013-2014" ~2,
                                                   indicator_key=="ecignow1" | indicator_key=="fnotlast" ~3,
                                                   TRUE ~0))
newdata2 <- subset(newdata2, drop2==0)
newdata2 <- newdata2 %>% rename("result_new"="result", "numerator_new" = "numerator", "denominator_new" = "denominator")
newdata2 <- newdata2[ , c("indicator_key", "year", "cat1", "cat1_varname", "cat1_group", "result_new", "numerator_new", "denominator_new")]

write.csv(newdata1, "newdata.csv", row.names = F)

#----------------
tab1(olddata1$cat2_varname, graph=F)
tab1(newdata1$cat2_varname, graph=F)

alldata1 <- merge(olddata1, newdata1, by = c("indicator_key","tab", 
                  "cat1", "cat1_varname", "cat1_group", "cat2", "cat2_varname", "cat2_group"), all=T)
alldata1 <- subset(alldata1, !is.na(result_old))
alldata1 <- subset(alldata1, !is.na(result_new))

alldata1$abs_diff <- 100*(alldata1$result_new - alldata1$result_old)
alldata1$rel_diff <- 100*(alldata1$result_new - alldata1$result_old)/alldata1$result_old
alldata1$rel_diff <- sprintf("%5.1f", round(alldata1$rel_diff, digits=1))

alldata1 <- alldata1[ , c("indicator_key", "tab", "cat1", "cat1_varname", "cat1_group", 
                          "cat2", "cat2_varname", "cat2_group",
                  "result_old", "numerator_old", "denominator_old", 
                  "result_new", "numerator_new", "denominator_new", "abs_diff", "rel_diff")]
write.xlsx(alldata1, "brfss_QA.xlsx", sheetName = "Sheet1", overwrite = T)

alldata2 <- merge(olddata2, newdata2, by = c("indicator_key", "year", "cat1", "cat1_varname", "cat1_group"), all=T)
alldata2 <- subset(alldata2, year!="2009-2011" & year!="2010-2012" & year!="2011-2013" & year!="2011 & 2013" & 
                     year!="2019-2021" & year!="2019 & 2021")

alldata2$absdiff <- (alldata2$result_new - alldata2$result_old) *100
alldata2 <- alldata2 %>% mutate(bigdiff  = case_when(abs(absdiff)>=3 & abs(absdiff)<10 ~"*", abs(absdiff)>=10 ~"^", TRUE ~""))

alldata2 <- alldata2 %>% mutate(bigdiff  = case_when(abs(absdiff)>=3 & abs(absdiff)<10 ~"*", abs(absdiff)>=10 ~"^", TRUE ~""))

write.xlsx(alldata2, "alldata2.xlsx", sheetName = "Sheet1", overwrite = T)
