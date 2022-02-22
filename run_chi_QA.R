#compare old data in SQL with new data 
rm(list=ls())
pacman::p_load(dplyr, foreign, odbc, DBI, dbplyr, epiDisplay, data.table, janitor, rads, naniar)
setwd("c:/R_learning/CHI_brfss") 
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
olddata <- subset(olddata_raw,indicator_key=="crcscrn2" | indicator_key=="denvst1" | 
                              indicator_key=="diab2" |    indicator_key=="firearm4" |
                              indicator_key=="fnotlast" | indicator_key=="menthlth" | 
                              indicator_key=="medcost" | indicator_key=="obese" | 
                              indicator_key=="smoker1")

olddata <- olddata %>% mutate(tab  = case_when(tab=='King County' ~'_kingcounty', TRUE ~tab))
olddata <- olddata %>% mutate(cat2  = case_when(cat2=='King County' ~'Overall', TRUE ~cat2))
olddata <- olddata %>% mutate(indicator_key  = case_when(indicator_key=='menthlth' ~'FMD',
                                                         indicator_key=="crcscrn2" ~'x_crcrec',
                                                         TRUE ~indicator_key))

olddata <- olddata %>% mutate(cat1  = case_when(cat1_varname=='race3' ~'race3',
                                                cat1_varname=='race4' ~'race4', TRUE ~cat1))
olddata <- olddata %>% mutate(cat2  = case_when(cat2=='Race/ethnicity' ~'Race', 
                                                cat2_varname=='race3' ~'race3',
                                                cat2_varname=='race4' ~'race4', TRUE ~cat2))
olddata <- olddata %>% mutate(cat2_group  = case_when(cat2_group=='LBG' ~'LGB', 
                                                      cat2_group=="King County" ~"Overall",
                                                      TRUE ~cat2_group))

olddata <- olddata %>% mutate(cat2  = case_when(tab=='_kingcounty' | tab=="demgroups" ~"Overall",
                                                TRUE ~cat2))
olddata <- olddata %>% mutate(cat2_group  = case_when(tab=='_kingcounty' | tab=='demgroups' ~"Overall",
                                                TRUE ~cat2_group))
olddata <- olddata[ , c("indicator_key", "tab", "year", "cat1", "cat1_group", "cat2", "cat2_group", "result", "denominator")]

olddata1 <- subset(olddata, tab!="trends")
olddata1 <- olddata1[ , c("indicator_key", "tab", "cat1", "cat1_group", "cat2", "cat2_group", "result", "denominator")]

olddata2 <- subset(olddata, tab=="trends")
olddata2 <- olddata2[ , c("indicator_key", "year", "cat1", "cat1_group", "cat2", "cat2_group", "result", "denominator")]

write.csv(olddata, "olddata.csv", row.names = F)

#-----
newdata1 <- read.csv("chi_brfs.csv")
newdata <- subset(newdata1,indicator_key=="x_crcrec" | indicator_key=="denvst1" | 
                    indicator_key=="diab2" |           indicator_key=="firearm4" |
                    indicator_key=="fnotlast" |        indicator_key=="FMD" | 
                    indicator_key=="medcost" |        indicator_key=="obese" | 
                    indicator_key=="smoker1")
newdata <- subset(newdata, tab!="_wastate")
newdata <- subset(newdata, cat1!="Bigcities")
newdata <- subset(newdata, cat2!="Bigcities")

newdata <- newdata %>% mutate(cat1  = case_when(cat1_varname=='race3' ~'race3',
                                                cat1_varname=='race4' ~'race4', TRUE ~cat1))
newdata <- newdata %>% mutate(cat2  = case_when(cat2_varname=='race3' ~'race3',
                                                cat2_varname=='race4' ~'race4', TRUE ~cat2))

newdata <- newdata %>% mutate(cat2  = case_when(tab=='_kingcounty' ~"Overall", TRUE ~cat2))
newdata <- newdata %>% mutate(cat2_group  = case_when(tab=='_kingcounty' ~"Overall", TRUE ~cat2_group))

newdata <- newdata[ , c("indicator_key", "tab", "year", "cat1", "cat1_group", "cat2", "cat2_group", "result", "denominator")]

newdata <- newdata %>% rename("result_new"="result", "denominator_new" = "denominator")
newdata <- newdata[ , c("indicator_key", "tab", "year", "cat1", "cat1_group", "cat2", "cat2_group", "result_new", "denominator_new")]

newdata1 <- subset(newdata, tab!="trends")
newdata1 <- newdata1[ , c("indicator_key", "tab", "cat1", "cat1_group", "cat2", "cat2_group", "result_new", "denominator_new")]

newdata2 <- subset(newdata, tab=="trends")
newdata2 <- newdata2[ , c("indicator_key", "year", "cat1", "cat1_group", "cat2", "cat2_group", "result_new", "denominator_new")]

write.csv(newdata, "newdata.csv", row.names = F)

alldata1 <- merge(olddata1, newdata1, by = c("indicator_key","tab", "cat1", "cat1_group", "cat2", "cat2_group"), all=T)
write.csv(alldata1, "alldata1.csv", row.names=F)

alldata2 <- merge(olddata2, newdata2, by = c("indicator_key", "year", "cat1", "cat1_group"), all=T)
write.csv(alldata2, "alldata2.csv", row.names=F)
