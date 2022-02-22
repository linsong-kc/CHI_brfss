#generate BRFSS data for most recent year
rm(list=ls())
pacman::p_load(dplyr, foreign, survey, srvyr, epiDisplay, data.table, janitor, rads, labelled, dtsurvey)
setwd("c:/R_learning/CHI_brfss")

#Read Stata file, can only translate Stata version 12 or earlier, other packages can read later versions
brfsraw <- read.dta(file="S:/WORK/surveys/brfs/prog_all/kc0020_finalz.dta", warn.missing.labels = FALSE)
brfsraw <- subset(brfsraw, year>=2019) 

#2016-2020: diab2, disab2, medcost, fmd, obese, smoker1; #2016, 2018, 2020: x_crcrec, denvst1, firearm4
#fnotlast (2018, 2019, 2020); #soda1 (2018, 2019) continuous, No trend data for soda1

names(brfsraw)<-gsub("^_", "x_", names(brfsraw)) #change variable names start with "_"
brfs <- brfsraw[ , c("year", "x_ststr", "kcwt_llcp",
                     "x_crcrec", "denvst1", "firearm4", "fnotlast", "diab2", "disab2",
                     "medcost", "menthlth", "obese", "smoker1", "soda1")]

#recode the following integer variables into character variables
brfs <- brfs %>% mutate_at(vars("x_crcrec", "disab2"), list(~recode(., '0'='No', '1'="Yes", .default ="NA")))
brfs <- brfs %>% mutate(fnotlast = case_when(fnotlast=='Often true' | fnotlast=='Sometimes true' ~'Yes',
        fnotlast=='Never true' ~ 'No', fnotlast=="Don't know/Not sure" | fnotlast=='Refused' ~'NA'))
brfs <- brfs %>% mutate(menthlth = case_when(menthlth<=13 ~'No', menthlth<=30 ~ 'Yes'))

brfs <- brfs %>% mutate(SSB = case_when(soda1==888 ~'No', 
                                          soda1>=201 & soda1<=206 ~'No',
                                          soda1>=301 & soda1<=330 ~'No',
                                          soda1>=101 & soda1<=199 ~'Yes',
                                          soda1>=207 & soda1<=220 ~'Yes',
                                          soda1>=330 & soda1<=380 ~'Yes'))

#recode demographic variable
brfs <- brfs %>% rename("FMD"="menthlth")
brfs$all <-  as.character("Total")

#--------------------------------
brfs <- brfs[complete.cases(brfs$kcwt_llcp), ]  #drop cases with missing weight
brfs[brfs == "NA"] <- NA
myvars <- c("diab2", "disab2","medcost", "FMD", "obese", "smoker1",
               "x_crcrec", "denvst1", "firearm4", "fnotlast")
brfs[, myvars] <- lapply(brfs[, myvars], as.factor)

options(survey.lonely.psu = "adjust")
brfskc <- dtsurvey(DT=brfs, psu=NULL, weight= "kcwt_llcp", strata="x_ststr")
brfskc$SSB <- as.factor(brfs$SSB)

result1 <- calc(ph.data = brfskc,
                  what = myvars,
                  year == "2020",
                  by = "all",
                  metrics=c("mean"),
                  per=100,
                  time_var="year",
                  proportion=T)

result2 <- calc(ph.data = brfskc,
                what = "SSB",
                year == "2019",
                by = "all",
                metrics=c("mean"),
                per=100,
                time_var="year",
                proportion=T)

res_meta <- rbind(result1, result2)
res_meta <- subset(res_meta, select = c("year", "variable", "mean", "level")) 

res_meta <- subset(res_meta, level=="Yes")
res_meta <- res_meta[ , c("year", "variable", "mean")] 
res_meta <- res_meta %>% rename("indicator_key" = "variable","latest_year" = "year","latest_year_result"="mean")

res_meta$data_source <- "brfss"
res_meta$result_type <- "proportion"
res_meta$valence <- "negative"

res_meta$latest_year_kc_pop <- as.numeric(1799943)
res_meta <- res_meta %>% mutate(latest_year_kc_pop= case_when(latest_year=="2019" ~1769325, TRUE ~latest_year_kc_pop))
res_meta$latest_year_count <- res_meta$latest_year_kc_pop * res_meta$latest_year_result
#attr(res_meta$latest_year_count, "format") <- "%.0f"
res_meta$latest_year_count <- round(res_meta$latest_year_count)
res_meta$map_type <- "hra"
res_meta$unit <- "adults"
res_meta$valid_years <- as.character("2009-2020")
res_meta <- res_meta %>% mutate(valid_years = case_when(indicator_key=="x_crcrec" ~"2014 2015 2016 2018 2020", 
                                              indicator_key=="denvst1"  ~"2010 2011 2012 2014 2015 2016 2018 2020",
                                              indicator_key=="firearm4" ~"2012 2013 2015 2016 2018 2020",
                                              indicator_key=="fnotlast" ~"20102011 2012 2013 2018 2019 2020",
                                              indicator_key=="SSB" ~"2018 2019", TRUE ~valid_years))

res_meta$chi <- 1
res_meta$run_date <- "2022-0215"

res_meta <- res_meta[, c("data_source", "indicator_key", "result_type", "valence",
                         "latest_year", "latest_year_result", "latest_year_kc_pop", "latest_year_count",
                         "map_type", "unit", "valid_years", "chi", "run_date")]
write.csv(res_meta, "chi_brfs_meta.csv", row.names = F)

#DB connection
db51 <- odbc::dbConnect(odbc::odbc(),
                        Driver = "SQL Server",
                        Server = "KCITSQLUTPDBH51",
                        Database = "PHExtractStore")

dbWriteTable(db51, Id(schema = "APDE_WIP", table = "brfss_meta_new"), res_meta, overwrite = T)
DBI::dbDisconnect(db51)
