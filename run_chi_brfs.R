##generate BRFSS data for CHI with custom/crosstab data
rm(list=ls())
pacman::p_load(dplyr, foreign, survey, srvyr, epiDisplay, data.table, janitor, rads, labelled, dtsurvey)
setwd("c:/R_learning/CHI_brfss")

#Read Stata file, can only translate Stata version 12 or earlier, other packages can read later versions
brfsraw <- read.dta(file="S:/WORK/surveys/brfs/prog_all/kc0020_finalz.dta", warn.missing.labels = FALSE)
brfsraw <- subset(brfsraw, year>=2016)

names(brfsraw)<-gsub("^_", "x_", names(brfsraw)) #change variable names start with "_"
brfs <- brfsraw[ , c("year", "x_ststr", "kcwt_llcp", "hracode", "hra2code", "ccreg","income6",
                     "age", "age7", "sex", "sexorien", "mrace", "mracex", "hispanic", "veteran",
                     "denvst1", "firearm4", "fnotlast", "diab2", "disab2",
                     "medcost", "menthlth", "obese", "smoker1", "soda1")]

#recode the following integer variables into character variables
brfs <- brfs %>% mutate_at(vars("disab2"), list(~recode(., '0'='No', '1'="Yes", .default ="NA")))
brfs <- brfs %>% mutate(fnotlast = case_when(fnotlast=='Often true' | fnotlast=='Sometimes true' ~'Yes',
        fnotlast=='Never true' ~ 'No', fnotlast=="Don't know/Not sure" | fnotlast=='Refused' ~'NA'))
brfs <- brfs %>% mutate(menthlth = case_when(menthlth<=13 ~'No', menthlth<=30 ~ 'Yes'))
brfs$denvst1 <- as.character(brfs$denvst1)
brfs <- brfs %>% mutate(denvst1 = case_when(denvst1=='Yes' ~'No', denvst1=='No' ~ 'Yes', TRUE ~denvst1))

brfs <- brfs %>% mutate(soda1 = case_when(soda1==888 ~'No', 
                                        soda1>=201 & soda1<=206 ~'No',
                                        soda1>=301 & soda1<=330 ~'No',
                                        soda1>=101 & soda1<=199 ~'Yes',
                                        soda1>=207 & soda1<=220 ~'Yes',
                                        soda1>=330 & soda1<=380 ~'Yes'))

#recode demographic variable
brfs <- brfs %>% mutate(age7 = case_when(age7=="25-34" | age7=="35-44" ~as.character("25-44"), 
                                         age7=="45-54" | age7=="55-64" ~as.character("45-64"), 
                                         TRUE ~ as.character(age7)))

brfs <- brfs %>% mutate(sex = case_when(sex=="male" ~"Male", sex=="female" ~"Female", TRUE ~ 'NA'))
brfs <- brfs %>% mutate(sexorien = case_when(sexorien=="A. Heterosexual, that is, straight" ~"Heterosexual", 
                        sexorien=="B. Homosexual, that is gay or lesbian" | sexorien=="C. Bisexual" ~"LGB", TRUE ~ 'NA'))

brfs$mrace <- as.character(brfs$mrace)
brfs <- brfs %>% mutate(mrace = case_when(mrace=="Other" | mrace=="Unknown" ~"Other/Unknown", TRUE ~ mrace))
brfs <- brfs %>% mutate(mracex= case_when(mracex=="Other" | mracex=="Unknown" ~"Other/Unknown", TRUE ~ mracex))

brfs <- brfs %>% mutate(veteran=case_when(veteran=="Yes" ~"Veteran", veteran=="No" ~"Non-Veteran", TRUE ~ "NA"))

brfs <- brfs %>% mutate(hra2  = case_when(hra2code=='Auburn' ~'Auburn city',
                                          hra2code=='Seattle' ~'Seattle city',
                                          hra2code=='Bellevue' ~'Bellevue city',
                                          hra2code=='Federal Way' ~'Federal Way city',
                                          hra2code=='Kent' ~'Kent city',
                                          hra2code=='Kirkland' ~'Kirkland city',
                                          hra2code=='Renton' ~'Renton city',
                                          TRUE ~'Other cities'))

brfs <- brfs %>% rename("FMD"="menthlth", "SSB" = "soda1")
brfs <- brfs %>% rename("age5"="age7", "LGB"="sexorien", "race3"="mrace", "race4"="mracex", "bigcities"="hra2")
brfs$all <-  "Total"

#--------------------------------
brfs <- brfs[complete.cases(brfs$kcwt_llcp), ]  #drop cases with missing weight
brfs[brfs == "NA"] <- NA
myvars <- c("diab2", "disab2","medcost", "FMD", "obese", "smoker1", 
               "denvst1", "firearm4", "fnotlast", "SSB")
brfs[, myvars] <- lapply(brfs[, myvars], as.factor)

byvars <- c("all", "age5", "sex", "race3", "hispanic", "race4", "LGB", "income6", "veteran", 
            "ccreg", "bigcities", "hracode")
brfs[, byvars] <- lapply(brfs[, byvars], as.character)

options(survey.lonely.psu = "adjust")
brfskc <- dtsurvey(DT=brfs, psu=NULL, weight= "kcwt_llcp", strata="x_ststr")
mygrid <- data.frame(expand.grid(myvars = myvars, byvars = byvars))

result.function <- function(X){
  temp <- calc(ph.data = brfskc,
               what = paste0(mygrid[X, ]$myvars),
               year >=2016,
               metrics = c("mean", "rse", "numerator", "denominator"),
               per = 100,
               time_var = "year",
               proportion = T,
               by =paste0(mygrid[X, ]$byvars))  
  temp[, byvar := paste0(mygrid[X, ]$byvars)]
  setnames(temp, paste0(mygrid[X, ]$byvars), "byvar_level")
  setcolorder(temp, c("variable", "level", "byvar", "byvar_level"))
  return(temp)
}

result1 <- rbindlist(
  lapply(X = as.list(seq(1, nrow(mygrid))), result.function), 
  use.names = T, fill = T
)

result2 <- result1
result2 <- result2 %>% rename("cat1"="byvar", "cat1_group"="byvar_level")
result2 <- subset(result2, level=="Yes")
result2 <- subset(result2, cat1_group!='NA')
result2 <- subset(result2, cat1_group!='Other/Unknown')
result2 <- subset(result2, cat1_group!='non-Hisp')

result2 <- result2 %>% mutate(cat1 = case_when(cat1=="age5" ~ "Age",
                                               cat1=="sex" ~"Gender", 
                                               cat1=="LGB" ~"Sexual orientation",
                                               cat1=="income6" ~ "Household Income",
                                               cat1=="veteran" ~ "Military Service",
                                               cat1=="hispanic" ~"Race", 
                                               cat1=="ccreg" ~"Regions", 
                                               cat1=="bigcities" ~"Bigcities", 
                                               cat1=="hracode" ~"Cities/neighborhoods", 
                                               TRUE ~ cat1))

result2 <- result2 %>% mutate(year=case_when(
  variable=="denvst1" | variable=="firearm4" ~"2016, 2018 & 2020", 
  variable=="fnotlast" ~"2018, 2019 & 2020",
  variable=="SSB" ~"2018-2019", TRUE ~year))
result2 <- result2 %>% mutate(tab = case_when(cat1=="all" ~ "_kingcounty", TRUE ~ "demgroups"))
result2 <- result2 %>% mutate(cat1 = case_when(cat1=="all" ~ "King County", TRUE ~ cat1))
result2 <- result2 %>% mutate(cat1_group = case_when(cat1=="King County" ~ "King County", TRUE ~ cat1_group))
result2$cat2 <- "Overall"
result2$cat2_group <- "Overall"

#-----run crosstab by age5-----
age5by <- c("sex","LGB","race3","hispanic","race4","income6","veteran","ccreg")
res_byage = rbindlist(lapply(X = as.list(age5by),
                              FUN = function(X){
                                message(X)
                                tempDT <- rads::calc(ph.data = brfskc,
                                                     what = myvars,
                                                     year >=2016,
                                                     by = c("age5", X),
                                                     metrics=c("mean","rse","numerator","denominator"),
                                                     per=100,
                                                     win=5,
                                                     time_var="year",
                                                     proportion=T)
                                tempDT[, cat2 := X]
                                setnames(tempDT, X, "cat2_group")
                              }), use.names = T)
res_byage <- res_byage %>% rename("cat1_group"="age5")
res_byage$cat1 <- "Age"

#-----run crosstabs by sex-----
sexby <- c("age5","LGB","race3","hispanic","race4","income6","veteran","ccreg")
res_bysex = rbindlist(lapply(X = as.list(sexby),
                              FUN = function(X){
                                message(X)
                                tempDT <- rads::calc(ph.data = brfskc,
                                                     what = myvars,
                                                     year >=2016,
                                                     by = c("sex", X),
                                                     metrics=c("mean","rse","numerator","denominator"),
                                                     per=100,
                                                     win=5,
                                                     time_var="year",
                                                     proportion=T)
                                tempDT[, cat2 := X]
                                setnames(tempDT, X, "cat2_group")
                              }), use.names = T)
res_bysex <- res_bysex %>% rename("cat1_group"="sex")
res_bysex$cat1 <- "Gender"

#-----
raceby <- c("age5","sex","LGB","income6","veteran","ccreg")
res_byrace3 = rbindlist(lapply(X = as.list(raceby),
                             FUN = function(X){
                               message(X)
                               tempDT <- rads::calc(ph.data = brfskc,
                                                    what = myvars,
                                                    year >=2016,
                                                    by = c("race3", X),
                                                    metrics=c("mean","rse","numerator","denominator"),
                                                    per=100,
                                                    win=5,
                                                    time_var="year",
                                                    proportion=T)
                               tempDT[, cat2 := X]
                               setnames(tempDT, X, "cat2_group")
                             }), use.names = T)
res_byrace3 <- res_byrace3 %>% rename("cat1_group"="race3")
res_byrace3$cat1 <- "race3"

#-----
res_byrace4 = rbindlist(lapply(X = as.list(raceby),
                               FUN = function(X){
                                 message(X)
                                 tempDT <- rads::calc(ph.data = brfskc,
                                                      what = myvars,
                                                      year >=2016,
                                                      by = c("race4", X),
                                                      metrics=c("mean","rse","numerator","denominator"),
                                                      per=100,
                                                      win=5,
                                                      time_var="year",
                                                      proportion=T)
                                 tempDT[, cat2 := X]
                                 setnames(tempDT, X, "cat2_group")
                               }), use.names = T)
res_byrace4 <- res_byrace4 %>% rename("cat1_group"="race4")
res_byrace4$cat1 <- "race4"

#-----
res_byhisp = rbindlist(lapply(X = as.list(raceby),
                               FUN = function(X){
                                 message(X)
                                 tempDT <- rads::calc(ph.data = brfskc,
                                                      what = myvars,
                                                      year >=2016,
                                                      by = c("hispanic", X),
                                                      metrics=c("mean","rse","numerator","denominator"),
                                                      per=100,
                                                      win=5,
                                                      time_var="year",
                                                      proportion=T)
                                 tempDT[, cat2 := X]
                                 setnames(tempDT, X, "cat2_group")
                               }), use.names = T)
res_byhisp <- res_byhisp %>% rename("cat1_group"="hispanic")
res_byhisp$cat1 <- "Race"

#-----
lgbby <- c("age5","sex", "race3", "race4", "hispanic", "income6","veteran","ccreg")
res_bylgb = rbindlist(lapply(X = as.list(lgbby),
                              FUN = function(X){
                                message(X)
                                tempDT <- rads::calc(ph.data = brfskc,
                                                     what = myvars,
                                                     year >=2016,
                                                     by = c("LGB", X),
                                                     metrics=c("mean","rse","numerator","denominator"),
                                                     per=100,
                                                     win=5,
                                                     time_var="year",
                                                     proportion=T)
                                tempDT[, cat2 := X]
                                setnames(tempDT, X, "cat2_group")
                              }), use.names = T)
res_bylgb <- res_bylgb %>% rename("cat1_group"="LGB")
res_bylgb$cat1 <- "Sexual orientation"

#-----
incby <- c("age5","sex", "LGB", "race3", "race4", "hispanic", "veteran","ccreg")
res_byinc = rbindlist(lapply(X = as.list(incby),
                             FUN = function(X){
                               message(X)
                               tempDT <- rads::calc(ph.data = brfskc,
                                                    what = myvars,
                                                    year >=2016,
                                                    by = c("income6", X),
                                                    metrics=c("mean","rse","numerator","denominator"),
                                                    per=100,
                                                    win=5,
                                                    time_var="year",
                                                    proportion=T)
                               tempDT[, cat2 := X]
                               setnames(tempDT, X, "cat2_group")
                             }), use.names = T)
res_byinc <- res_byinc %>% rename("cat1_group"="income6")
res_byinc$cat1 <- "Household Income"

#-----
vetby <- c("age5","sex", "LGB", "race3", "race4", "hispanic", "income6","ccreg")
res_byvet = rbindlist(lapply(X = as.list(vetby),
                             FUN = function(X){
                               message(X)
                               tempDT <- rads::calc(ph.data = brfskc,
                                                    what = myvars,
                                                    year >=2016,
                                                    by = c("veteran", X),
                                                    metrics=c("mean","rse","numerator","denominator"),
                                                    per=100,
                                                    win=5,
                                                    time_var="year",
                                                    proportion=T)
                               tempDT[, cat2 := X]
                               setnames(tempDT, X, "cat2_group")
                             }), use.names = T)
res_byvet <- res_byvet %>% rename("cat1_group"="veteran")
res_byvet$cat1 <- "Military Service"

#-----
regby <- c("age5","sex", "LGB", "race3", "race4", "hispanic", "income6","veteran")
res_byreg = rbindlist(lapply(X = as.list(regby),
                             FUN = function(X){
                               message(X)
                               tempDT <- rads::calc(ph.data = brfskc,
                                                    what = myvars,
                                                    year >=2016,
                                                    by = c("ccreg", X),
                                                    metrics=c("mean","rse","numerator","denominator"),
                                                    per=100,
                                                    win=5,
                                                    time_var="year",
                                                    proportion=T)
                               tempDT[, cat2 := X]
                               setnames(tempDT, X, "cat2_group")
                             }), use.names = T)
res_byreg <- res_byreg %>% rename("cat1_group"="ccreg")
res_byreg$cat1 <- "Regions"

#-----

result_by <- rbind(res_byage, res_bysex, res_bylgb, res_byrace3, res_byrace4, res_byhisp, 
                   res_byinc, res_byvet, res_byreg)
result_by <- subset(result_by, cat1_group!='NA')
result_by <- subset(result_by, cat1_group!='Other/Unknown')
result_by <- subset(result_by, cat1_group!='non-Hisp')
result_by <- subset(result_by, cat2_group!='NA')
result_by <- subset(result_by, cat2_group!='Other/Unknown')
result_by <- subset(result_by, cat2_group!='non-Hisp')
result_by <- subset(result_by, level=="Yes")

result_by <- result_by %>% mutate(cat2 = case_when(cat2=="age5" ~ "Age",
                                 cat2=="sex" ~"Gender", 
                                 cat2=="LGB" ~"Sexual orientation",
                                 cat2=="income6" ~ "Household Income",
                                 cat2=="veteran" ~ "Military Service",
                                 cat2=="hispanic" ~"Race", 
                                 cat2=="ccreg" ~"Regions", 
                                 TRUE ~ cat2))
result_by$tab <-"crosstabs"

result_by <- result_by %>% mutate(year=case_when(variable=="diab2" | variable=="disab2" | variable=="medcost" | 
                            variable=="FMD" | variable=="obese" | variable=="smoker1" ~ "2016-2020",
                            variable=="denvst1" | variable=="firearm4" ~"2016, 2018 & 2020", 
                            variable=="fnotlast" ~"2018, 2019 & 2020",
                            variable=="SSB" ~"2018-2019"))

write.csv(result2, "result2.csv", row.names = F)
write.csv(result_by, "result_by.csv", row.names = F)

#----------------------------------------
resultx <- bind_rows(result2, result_by)

#-----King County average for significance comparison-----
res_kc <- subset(resultx, subset=resultx$tab=="_kingcounty", 
                 select= c("variable", "mean", "mean_lower", "mean_upper" ))
res_kc <- res_kc %>% rename("kc_result" = "mean", "kc_lower" = "mean_lower", "kc_upper"="mean_upper")

resultx <- merge(resultx, res_kc, by="variable")
resultx <- resultx %>% mutate(comparison_with_kc = case_when(mean_upper < kc_lower ~as.character("lower"), 
                                                               mean_lower > kc_upper ~as.character("higher"), 
                                                               TRUE ~as.character("not different")))
resultx <- resultx %>% mutate(significance = case_when(mean_upper < kc_lower ~as.character("*"), 
                                                         mean_lower > kc_upper ~as.character("*"), 
                                                         TRUE ~as.character("")))

resultx <- resultx %>% rename("indicator_key"="variable", "result"="mean", 
                              "se"="mean_se", "lower_bound"="mean_lower", "upper_bound"="mean_upper")

resultx <- resultx[, c("tab", "year", "indicator_key", "cat1","cat1_group", "cat2", "cat2_group",
                       "result", "lower_bound", "upper_bound", "se", "rse", "numerator", "denominator",
                       "comparison_with_kc", "significance")]
resultx <- resultx[order(tab, indicator_key, cat1, cat1_group, year), ]
write.csv(resultx, "resultx.csv", row.names = F)
