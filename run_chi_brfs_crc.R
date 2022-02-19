#generate BRFSS data for CHI/CRC screening with custom data
rm(list=ls())
pacman::p_load(dplyr, foreign, survey, srvyr, epiDisplay, data.table, janitor, rads, labelled, dtsurvey)
setwd("c:/R_learning/CHI") 

#Read Stata file, can only translate Stata version 12 or earlier, other packages can read later versions
brfsraw <- read.dta(file="S:/WORK/surveys/brfs/prog_all/kc0020_finalz.dta", warn.missing.labels = FALSE)
brfsraw <- subset(brfsraw, year>=2016)

names(brfsraw)<-gsub("^_", "x_", names(brfsraw)) #change variable names start with "_"
brfs <- brfsraw[ , c("year", "x_ststr", "kcwt_llcp", "hracode", "hra2code", "ccreg","income6",
                     "age", "sex", "sexorien", "mrace", "mracex", "hispanic", "veteran",
                     "x_crcrec")]

#recode the following integer variables into character variables
brfs <- brfs %>% mutate_at(vars("x_crcrec"), list(~recode(., '0'='Yes', '1'="No", .default ="NA")))
#recode demographic variable
brfs <- brfs %>% mutate(age2 = case_when(age>=50 & age<65  ~as.character("50-64"), 
                                         age>=65 & age<=75 ~as.character("65-75"), 
                                         TRUE ~ as.character("NA")))

brfs <- brfs %>% mutate(sex = case_when(sex=="male" ~"Male", sex=="female" ~"Female", TRUE ~ 'NA'))
brfs <- brfs %>% mutate(sexorien = case_when(sexorien=="A. Heterosexual, that is, straight" ~"Heterosexual", 
                        sexorien=="B. Homosexual, that is gay or lesbian" | sexorien=="C. Bisexual" ~"LGB", TRUE ~ 'NA'))

brfs$mrace <- as.character(brfs$mrace)
brfs$mracex <- as.character(brfs$mracex)
brfs <- brfs %>% mutate(mrace = case_when(mrace=="Other" | mrace=="Unknown" ~"Other/Unknown", TRUE ~ mrace))
brfs <- brfs %>% mutate(mracex= case_when(mracex=="Other" | mracex=="Unknown" ~"Other/Unknown", TRUE ~ mracex))

brfs <- brfs %>% mutate(veteran=case_when(veteran=="Yes" ~"Veteran", veteran=="No" ~"Non-veteran", TRUE ~ "NA"))

brfs <- brfs %>% mutate(hra2  = case_when(hra2code=='Auburn' ~'Auburn city',
                                          hra2code=='Seattle' ~'Seattle city',
                                          hra2code=='Bellevue' ~'Bellevue city',
                                          hra2code=='Federal Way' ~'Federal Way city',
                                          hra2code=='Kent' ~'Kent city',
                                          hra2code=='Kirkland' ~'Kirkland city',
                                          hra2code=='Renton' ~'Renton city',
                                          TRUE ~'Other cities'))

brfs <- brfs %>% rename("LGB"="sexorien", "race3"="mrace", "race4"="mracex", "bigcities"="hra2")
brfs$all <-  "Total"

#--------------------------------
brfs <- brfs[complete.cases(brfs$kcwt_llcp), ]  #drop cases with missing weight
brfs[brfs == "NA"] <- NA
brfs$x_crcrec <- as.factor(brfs$x_crcrec)
myvars <- c("x_crcrec")

byvars <- c("all", "age2", "sex", "race3", "hispanic", "race4", "LGB", "income6", "veteran", 
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

result2 <- result2 %>% mutate(cat1 = case_when(cat1=="age2" ~ "Age",
                                               cat1=="sex" ~"Gender", 
                                               cat1=="LGB" ~"Sexual orientation",
                                               cat1=="income6" ~ "Household Income",
                                               cat1=="veteran" ~ "Military Service",
                                               cat1=="hispanic" ~"Race", 
                                               cat1=="ccreg" ~"Regions", 
                                               cat1=="bigcities" ~"Bigcities", 
                                               cat1=="hracode" ~"Cities/neighborhoods", 
                                               TRUE ~ cat1))

result2 <- result2 %>% mutate(year=case_when(variable=="x_crcrec" ~"2016, 2018 & 2020"))
result2 <- result2 %>% mutate(tab = case_when(cat1=="all" ~ "_kingcounty", TRUE ~ "demgroups"))
result2 <- result2 %>% mutate(cat1 = case_when(cat1=="all" ~ "King County", TRUE ~ cat1))
result2 <- result2 %>% mutate(cat1_group = case_when(cat1=="King County" ~ "King County", TRUE ~ cat1_group))
result2$cat2 <- "Overall"
result2$cat2_group <- "Overall"

#-----run crosstab by age2-----
age2by <- c("sex","LGB","race3","hispanic","race4","income6","veteran","ccreg","bigcities","hracode")
res_byage = rbindlist(lapply(X = as.list(age2by),
                              FUN = function(X){
                                message(X)
                                tempDT <- rads::calc(ph.data = brfskc,
                                                     what = myvars,
                                                     year >=2016,
                                                     by = c("age2", X),
                                                     metrics=c("mean","rse","numerator","denominator"),
                                                     per=100,
                                                     win=5,
                                                     time_var="year",
                                                     proportion=T)
                                tempDT[, cat2 := X]
                                setnames(tempDT, X, "cat2_group")
                              }), use.names = T)
res_byage <- res_byage %>% rename("cat1_group"="age2")
res_byage$cat1 <- "Age"

#-----run crosstabs by sex-----
sexby <- c("age2","LGB","race3","hispanic","race4","income6","veteran","ccreg","bigcities","hracode")
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
raceby <- c("age2","sex","LGB","income6","veteran","ccreg","bigcities","hracode")
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
lgbby <- c("age2","sex", "race3", "race4", "hispanic", "income6","veteran","ccreg","bigcities","hracode")
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
incby <- c("age2","sex", "LGB", "race3", "race4", "hispanic", "veteran","ccreg","bigcities","hracode")
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
vetby <- c("age2","sex", "LGB", "race3", "race4", "hispanic", "income6","ccreg","bigcities","hracode")
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
regby <- c("age2","sex", "LGB", "race3", "race4", "hispanic", "income6","veteran")
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
res_bybig = rbindlist(lapply(X = as.list(regby),
                             FUN = function(X){
                               message(X)
                               tempDT <- rads::calc(ph.data = brfskc,
                                                    what = myvars,
                                                    year >=2016,
                                                    by = c("bigcities", X),
                                                    metrics=c("mean","rse","numerator","denominator"),
                                                    per=100,
                                                    win=5,
                                                    time_var="year",
                                                    proportion=T)
                               tempDT[, cat2 := X]
                               setnames(tempDT, X, "cat2_group")
                             }), use.names = T)
res_bybig <- res_bybig %>% rename("cat1_group"="bigcities")
res_bybig$cat1 <- "Bigcities"

#-----
res_byhra = rbindlist(lapply(X = as.list(regby),
                             FUN = function(X){
                               message(X)
                               tempDT <- rads::calc(ph.data = brfskc,
                                                    what = myvars,
                                                    year >=2016,
                                                    by = c("hracode", X),
                                                    metrics=c("mean","rse","numerator","denominator"),
                                                    per=100,
                                                    win=5,
                                                    time_var="year",
                                                    proportion=T)
                               tempDT[, cat2 := X]
                               setnames(tempDT, X, "cat2_group")
                             }), use.names = T)
res_byhra <- res_byhra %>% rename("cat1_group"="hracode")
res_byhra$cat1 <- "Cities/neighborhoods"

result_by <- rbind(res_byage, res_bysex, res_bylgb, res_byrace3, res_byrace4, res_byhisp, res_byinc,
                   res_byvet, res_byreg, res_bybig, res_byhra)
result_by <- subset(result_by, cat1_group!='NA')
result_by <- subset(result_by, cat1_group!='Other/Unknown')
result_by <- subset(result_by, cat1_group!='non-Hisp')
result_by <- subset(result_by, cat2_group!='NA')
result_by <- subset(result_by, cat2_group!='Other/Unknown')
result_by <- subset(result_by, cat2_group!='non-Hisp')
result_by <- subset(result_by, level=="Yes")

result_by <- result_by %>% mutate(cat2 = case_when(cat2=="age2" ~ "Age",
                                 cat2=="sex" ~"Gender", 
                                 cat2=="LGB" ~"Sexual orientation",
                                 cat2=="income6" ~ "Household Income",
                                 cat2=="veteran" ~ "Military Service",
                                 cat2=="hispanic" ~"Race", 
                                 cat2=="ccreg" ~"Regions", 
                                 cat2=="bigcities" ~"Bigcities", 
                                 cat2=="hracode" ~"Cities/neighborhoods", 
                                 TRUE ~ cat2))
result_by$tab <-"crosstabs"

result_by <- result_by %>% mutate(year=case_when(variable=="x_crcrec" ~"2016, 2018 & 2020"))

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
write.csv(resultx, "result_crc.csv", row.names = F)
