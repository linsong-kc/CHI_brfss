##generate BRFSS data for CHI with custom/crosstab data, most recent year = 2021
#remotes::install_github("PHSKC-APDE/rads", auth_token = NULL)
#King County
#demgroups (line 82): kingco, age5v2, income6, region, hra20, sex, race3, race3 (hisp), race4, sexorien, veteran, bigcities 
#crosstabs(line 212): kingco, age5v2, income6, region, hra20, sex, race3, race3 (hisp), race4, veteran 
#trends (line 1055):    kingco, region, race3, race3, race4, sexorien
#WA State (line 1110)
rm(list=ls())
pacman::p_load(dplyr, foreign, survey, srvyr, epiDisplay, data.table, janitor, rads, labelled, dtsurvey, openxlsx, DBI)
setwd("C:/pers_linsong/chi_brfss")
options(max.print= 99999)

kc1221 <- readRDS("//dphcifs/APDE-CDIP/BRFSS/prog_all/kc1221final.rds")
d1 <- as.data.table(kc1221)
colnames(d1)[grepl("mj", colnames(d1))]

d2 <- d1[, .(year, x_ststr, finalwt1, hra20_name, bigcities, region, income6,  
             age, age7, sex, sexorien, trnsgndr, race3, race4, hispanic, veteran3, 
             asthnow, bphigh, cholchk5, x_crcrec, cvdheart, cvdstrk3, x_denvst3, diab2,  
             disab2, ecignow1, firearm4, flushot7, fnotlast, genhlth2, mam2yrs, medcost1,
             fmd, mjnow, obese, x_bmi5cat, x_veglt1a, pap3yrs, persdoc3, x_pneumo3, smoker1, SSB)]

#-----recode demographic variable
d2 <- setnames(d2, c("x_denvst3", "cvdstrk3", "disab2", "medcost1", "mjnow", "persdoc3", "x_pneumo3", "SSB",
                     "hispanic", "income6", "trnsgndr"),
                   c("denvst1", "cvdstrok", "disabil", "medcost", "mjpast30", "persdoc", "pneumvac", "ssb",
                     "hisp", "income6b", "trans"))

d2[, age7:=as.integer(age7)] [, race3:=as.character(race3)] [, race4:=as.character(race4)] [, income6b:=as.character(income6b)] 
d2[age7==1, age5_v2 :="18-24"] [age7 %in% c(2,3), age5_v2 :="25-44"] [age7 %in% c(4, 5), age5_v2 :="45-64"] [age7==6, age5_v2 :="65-74"] [age7==7, age5_v2 :="75+"]
d2[sex==1, chi_sex :="Male"] [sex==2, chi_sex :="Female"]
d2[sexorien==1, chi_sexorien_2 :="Heterosexual"] [sexorien %in% c(2, 3), chi_sexorien_2 :="LGB"][sexorien==4, chi_sexorien_2:=NA_character_]
d2[trans==0, trnsgndr :="Cisgender"] [trans==1, trnsgndr :="Transgender"] [is.na(trans), trnsgndr :=NA]

d2[veteran3==0, veteran :="Non-Veteran"] [veteran3==1, veteran :="Veteran"]
d2[hisp==0, hispanic :="Non-Hispanic"] [hisp==1, hispanic :="Hispanic"]
d2 <- d2 %>% mutate(race4 = case_when(race4=="Hisp" ~"Hispanic", race4=="Multiracial" ~"Multiple", TRUE ~race4))

d2 <- d2 %>% mutate(race3 = case_when(race3=="Multiracial" ~"Multiple", TRUE ~race3))
d2 <- d2 %>% mutate(bigcities = case_when(grepl(" city", bigcities) ~bigcities, TRUE ~"x_other"))

#this is no flushot=1, valence in the negative
d2$flushot_v1 <- as.integer(d2$flushot_v1)
d2$flushot_v2 <- as.integer(d2$flushot_v2)
d2[age<65, flushot_v1 := flushot7] [age>=65, flushot_v1 := NA]
d2[age>=65, flushot_v2 := flushot7] [age<65,  flushot_v2 := NA]
d2[age<21 | age>65, pap3yrs := NA] [age<50 | age>74, mam2yrs := NA] [age<50 | age>75, x_crcrec := NA]
d2[age>=21 & age<45, age_pap :="21-44"] [age>=45 & age<=65, age_pap :="45-65"] [age<21 | age>65, age_pap :=NA] 
d2[age>=50 & age<65, age_mam :="50-64"] [age>=65 & age<75, age_mam :="65-74"] [age<50 | age>=75, age_mam :=NA] 
d2[age>=50 & age<65, age_crc :="50-64"] [age>=65 & age<=75, age_crc :="65-75"] [age<50 | age>75, age_crc :=NA] 
d2[ecignow1==2,  ecignow1 := 1] [ecignow1==3, ecignow1 := 0]

#convert the following varaibles into negative valence
d2[persdoc==1,  persdoc := 2] [persdoc==0, persdoc := 1] [persdoc==2, persdoc :=0]
d2[cholchk5==1,  cholchk5 := 2] [cholchk5==0, cholchk5 := 1] [cholchk5==2, cholchk5 :=0]
d2[pneumvac==1,  pneumvac := 2] [pneumvac==0, pneumvac := 1] [pneumvac==2, pneumvac :=0]
d2[x_bmi5cat<=2 | x_bmi5cat==4,  overw1 := 0] [x_bmi5cat==3, overw1 := 1] [is.na(x_bmi5cat), overw1 :=NA]

d2[denvst1==1,  denvst1 := 2] [denvst1==0, denvst1 := 1] [denvst1==2, denvst1 :=0]
d2[mam2yrs==1,  mam2yrs := 2] [mam2yrs==0, mam2yrs := 1] [mam2yrs==2, mam2yrs :=0]
d2[pap3yrs==1,  pap3yrs := 2] [pap3yrs==0, pap3yrs := 1] [pap3yrs==2, pap3yrs :=0]
d2[x_crcrec==1, x_crcrec := 2] [x_crcrec==0, x_crcrec := 1] [x_crcrec==2, x_crcrec :=0]

d2$all <- "Total"
d2 <- d2[, !c("age7", "sexorien", "hisp", "veteran3", "trans"), with=F]

# ----- Check indicator variables:
mytable <- function(x, y) {
#  prop.table(table(x, y), margin=1)
  table(x, y)
  
}

# apply the mytable function to each combination of columns using lapply
lapply(d2[, c("asthnow", "cvdheart", "cvdstrok", "diab2", "disabil", "flushot_v1", "flushot_v2",
              "genhlth2", "medcost", "fmd", "obese", "overw1", "persdoc", "pneumvac", "smoker1",
              "bphigh", "cholchk5", "ecignow1", "fnotlast", "mjpast30", "x_veglt1a",
              "denvst1", "firearm4", "x_crcrec", "mam2yrs", "pap3yrs")], 
       function(x) mytable(d2$year, x))

#--------------------------------
dt1 <- d2[complete.cases(d2$finalwt1), ]  #drop cases with missing weight
dt2 <- subset(dt1, chi_sex=="Female")

#indicator lists ("within 2017-2021" variables, "2016, 2018, 2020" variables, and variables for females
myvar1 <- c("asthnow", "cvdheart", "cvdstrok", "diab2", "disabil", "flushot_v1", "flushot_v2",
             "genhlth2", "medcost", "fmd", "obese", "overw1", "persdoc", "pneumvac", "smoker1",
             "bphigh", "cholchk5", "ecignow1", "fnotlast", "mjpast30", "x_veglt1a", "ssb")
myvar2 <- c("denvst1", "firearm4")
myvar3 <- c("x_crcrec")
myvar4 <- c("mam2yrs")
myvar5 <- c("pap3yrs")
myvar6 <- c("asthnow", "cvdheart", "cvdstrok", "diab2", "disabil", "flushot_v1", "flushot_v2",
            "genhlth2", "medcost", "fmd", "obese", "overw1", "persdoc", "pneumvac", "smoker1",
            "bphigh", "cholchk5", "ecignow1", "fnotlast", "mjpast30", "x_veglt1a",
            "denvst1", "firearm4", "x_crcrec", "mam2yrs", "pap3yrs")

byvar1 <- c("all", "age5_v2", "chi_sex", "chi_sexorien_2", "trnsgndr", "race3", "hispanic", "race4", "income6b", "veteran", "hra20_name", "bigcities", "region")
byvar3 <- c("all", "age_crc", "chi_sex", "chi_sexorien_2", "race3", "hispanic", "race4", "income6b", "veteran", "hra20_name", "bigcities", "region")
byvar4 <- c("all", "age_mam", "chi_sexorien_2", "race3", "hispanic", "race4", "income6b", "veteran", "hra20_name", "bigcities", "region")
byvar5 <- c("all", "age_pap", "chi_sexorien_2", "race3", "hispanic", "race4", "income6b", "veteran", "hra20_name", "bigcities", "region")
byvar6 <- c("all", "chi_sexorien_2", "race3", "hispanic", "race4", "region")

mygrid1 <- data.frame(expand.grid(myvars = myvar1, byvars = byvar1))
mygrid2 <- data.frame(expand.grid(myvars = myvar2, byvars = byvar1))
mygrid3 <- data.frame(expand.grid(myvars = myvar3, byvars = byvar3))
mygrid4 <- data.frame(expand.grid(myvars = myvar4, byvars = byvar4))
mygrid5 <- data.frame(expand.grid(myvars = myvar5, byvars = byvar5))
mygrid6 <- data.frame(expand.grid(myvars = myvar6, byvars = byvar6))

options(survey.lonely.psu = "adjust")
brfs1 <- dtsurvey(DT=dt1, psu=NULL, weight= "finalwt1", strata="x_ststr")
brfs2 <- dtsurvey(DT=dt2, psu=NULL, weight= "finalwt1", strata="x_ststr")

#testing RADS calc results
brfs3 <- svydesign(id=~1, weights=~finalwt1, strata = ~x_ststr, survey.longly.psu = "adjust", data=dt1)
prop.table(svytable(~bphigh+age5_v2, brfs3), margin=2)

##-----run by demographics
#-----run 2017-2021 data
result1.function <- function(X){
  temp <- calc(ph.data = brfs1,
               what = paste0(mygrid1[X, ]$myvars),
               year >=2017,
               metrics = c("mean", "rse", "numerator", "denominator"),
               per = 100, time_var = "year", proportion = T,
               by =paste0(mygrid1[X, ]$byvars))  
   temp[, byvar := paste0(mygrid1[X, ]$byvars)]
   setnames(temp,  paste0(mygrid1[X, ]$byvars), "byvar_level")
   setcolorder(temp, c("variable", "level", "byvar", "byvar_level"))
   return(temp)
}

result1a <- rbindlist(
  lapply(X = as.list(seq(1, nrow(mygrid1))), result1.function), 
  use.names = T, fill = T
)

#-----run 2016, 2018, 2020 data
result2.function <- function(X){
  temp <- calc(ph.data = brfs1,
               what = paste0(mygrid2[X, ]$myvars),
               year==2016 | year==2018 | year==2020,
               metrics = c("mean", "rse", "numerator", "denominator"),
               per = 100, time_var = "year", proportion = T,
               by=paste0(mygrid2[X, ]$byvars))  
  temp[, byvar := paste0(mygrid2[X, ]$byvars)]
  setnames(temp,  paste0(mygrid2[X, ]$byvars), "byvar_level")
  setcolorder(temp, c("variable", "level", "byvar", "byvar_level"))
  return(temp)
}

result2a <- rbindlist(
  lapply(X = as.list(seq(1, nrow(mygrid2))), result2.function), 
  use.names = T, fill = T
)

#-----run CRC screening data
result3.function <- function(X){
  temp <- calc(ph.data = brfs1,
               what = paste0(mygrid3[X, ]$myvars),
               year==2016 | year==2018 | year==2020,
               metrics = c("mean", "rse", "numerator", "denominator"),
               per = 100, time_var = "year", proportion = T,
               by=paste0(mygrid3[X, ]$byvars))  
  temp[, byvar := paste0(mygrid3[X, ]$byvars)]
  setnames(temp,  paste0(mygrid3[X, ]$byvars), "byvar_level")
  setcolorder(temp, c("variable", "level", "byvar", "byvar_level"))
  return(temp)
}

result3a <- rbindlist(
  lapply(X = as.list(seq(1, nrow(mygrid3))), result3.function), 
  use.names = T, fill = T
)

#-----run mam2yrs
result4.function <- function(X){
  temp <- calc(ph.data = brfs2,
               what = paste0(mygrid4[X, ]$myvars),
               year==2016 | year==2018 | year==2020,
               metrics = c("mean", "rse", "numerator", "denominator"),
               per = 100, time_var = "year", proportion = T,
               by=paste0(mygrid4[X, ]$byvars))  
  temp[, byvar := paste0(mygrid4[X, ]$byvars)]
  setnames(temp,  paste0(mygrid4[X, ]$byvars), "byvar_level")
  setcolorder(temp, c("variable", "level", "byvar", "byvar_level"))
  return(temp)
}

result4a <- rbindlist(
  lapply(X = as.list(seq(1, nrow(mygrid4))), result4.function), 
  use.names = T, fill = T
)

#-----run pap3yrs
result5.function <- function(X){
  temp <- calc(ph.data = brfs2,
               what = paste0(mygrid5[X, ]$myvars),
               year==2016 | year==2018 | year==2020,
               metrics = c("mean", "rse", "numerator", "denominator"),
               per = 100, time_var = "year", proportion = T,
               by=paste0(mygrid5[X, ]$byvars))  
  temp[, byvar := paste0(mygrid5[X, ]$byvars)]
  setnames(temp,  paste0(mygrid5[X, ]$byvars), "byvar_level")
  setcolorder(temp, c("variable", "level", "byvar", "byvar_level"))
  return(temp)
}

result5a <- rbindlist(
  lapply(X = as.list(seq(1, nrow(mygrid5))), result5.function), 
  use.names = T, fill = T
)

res1 <- bind_rows(result1a, result2a, result3a, result4a, result5a)
res2 <- res1[!(res1$variable == "x_crcrec" & res1$byvar == "age5_v2")]
res2 <- res2 %>% rename("cat1_group"="byvar_level")
res2 <- res2 %>% dplyr::select(-c("level"))

res2 <- res2[!(cat1_group %in% c("Non-Hispanic", "NA", "Other/unknown", "Other", "x_other", NA))]
res2 <- res2 %>% mutate(cat1 = case_when(byvar=="all" ~"King County",
                                         byvar %in% c("age5_v2", "age_crc", "age_mam", "age_pap") ~ "Age",
                                         byvar=="chi_sex" ~"Gender", 
                                         byvar=="chi_sexorien_2" ~"Sexual orientation",
                                         byvar=="trnsgndr" ~"Transgender",
                                         byvar=="income6b" ~ "Household income",
                                         byvar=="veteran" ~ "Military Service",
                                         byvar=="hispanic" ~"Ethnicity",
                                         byvar %in% c("race3", "race4") ~"Race",
                                         byvar=="region" ~"Regions", 
                                         byvar=="bigcities" ~"Big cities", 
                                         byvar=="hra20_name" ~"Cities/neighborhoods", TRUE ~ ""))

res2 <- res2 %>% mutate(cat1_varname = case_when(byvar=="all" ~"chi_geo_kc",
                                               byvar=="hispanic" ~"race3",
                                               byvar=="trnsgndr" ~"Transgender",
                                               byvar=="region" ~"chi_geo_region", TRUE ~ byvar))

res2 <- res2 %>% mutate(cat1_group = case_when(byvar=="all" ~"King County", TRUE ~ cat1_group))
                                               
res2 <- res2 %>% mutate(tab = case_when(byvar=="all" ~ "_kingcounty", TRUE ~ "demgroups"))

res2$cat2 <- "King County"
res2$cat2_varname <- "chi_geo_kc"
res2$cat2_group <- "King County"

res2 <- res2[, c("variable", "tab", "year", "cat1", "cat1_varname", "cat1_group", "cat2", "cat2_varname", "cat2_group",
                 "mean", "mean_se", "mean_lower", "mean_upper", "rse", "numerator", "denominator")]
res2 <- res2[order(res2$variable, res2$cat1, res2$cat1_group), ]
write.xlsx(res2, "brfss_demo.xlsx", sheetName = "Sheet1", overwrite = T)

##-----run crosstab1-----
#------by variable lists
by_age5 <- c("chi_sex", "chi_sexorien_2", "race3", "hispanic", "race4", "income6b", "veteran", "region")
by_sex <-  c("age5_v2", "chi_sexorien_2", "race3", "hispanic", "race4", "income6b", "veteran", "region")
by_lgb <- c("age5_v2", "chi_sex", "race3", "hispanic", "race4", "income6b", "veteran", "region")
by_tns <- c("age5_v2", "chi_sex", "race3", "hispanic", "race4", "income6b", "veteran", "region")
by_race <- c("age5_v2", "chi_sex", "chi_sexorien_2", "income6b", "veteran", "region")
by_inc <- c("age5_v2", "chi_sex", "chi_sexorien_2", "race3", "hispanic", "race4", "veteran", "region")
by_vet <- c("age5_v2", "chi_sex", "chi_sexorien_2", "race3", "hispanic", "race4", "income6b", "region")
by_reg <- c("age5_v2", "chi_sex", "chi_sexorien_2", "chi_sexorien_2", "race3", "hispanic", "race4", "income6b", "veteran")

#------indicators in myvar1
#-----1. by age
res_byage = rbindlist(lapply(X = as.list(by_age5),
                              FUN = function(X){
                                message(X)
                                tempDT <- rads::calc(ph.data = brfs1,
                                                     what = myvar1,
                                                     year >=2017,
                                                     by = c("age5_v2", X),
                                                     metrics=c("mean","rse","numerator","denominator"),
                                                     per=100, win=5, time_var="year", proportion=T)
                                tempDT[, cat2 := X]
                                setnames(tempDT, X, "cat2_group")
                              }), use.names = T)
res_byage <- res_byage %>% rename("cat1_group"="age5_v2")
res_byage[, ':=' (cat1 = 'Age', cat1_varname ='age5_v2')]

#-----2. by sex
res_bysex = rbindlist(lapply(X = as.list(by_sex),
                              FUN = function(X){
                                message(X)
                                tempDT <- rads::calc(ph.data = brfs1,
                                                     what = myvar1,
                                                     year >=2017,
                                                     by = c("chi_sex", X),
                                                     metrics=c("mean","rse","numerator","denominator"),
                                                     per=100, win=5, time_var="year", proportion=T)
                                tempDT[, cat2 := X]
                                setnames(tempDT, X, "cat2_group")
                              }), use.names = T)
res_bysex <- res_bysex %>% rename("cat1_group"="chi_sex")
res_bysex[, ':=' (cat1 = 'Gender', cat1_varname ='chi_sex')]

#-----3. by sexual orientation
res_bylgb = rbindlist(lapply(X = as.list(by_lgb),
                             FUN = function(X){
                               message(X)
                               tempDT <- rads::calc(ph.data = brfs1,
                                                    what = myvar1,
                                                    year >=2017,
                                                    by = c("chi_sexorien_2", X),
                                                    metrics=c("mean","rse","numerator","denominator"),
                                                    per=100, win=5, time_var="year", proportion=T)
                               tempDT[, cat2 := X]
                               setnames(tempDT, X, "cat2_group")
                             }), use.names = T)
res_bylgb <- res_bylgb %>% rename("cat1_group"="chi_sexorien_2")
res_bylgb[, ':=' (cat1 = 'Sexual orientation', cat1_varname ='chi_sexorien_2')]

#-----3b. by trnsgndr
res_bytns = rbindlist(lapply(X = as.list(by_tns),
                             FUN = function(X){
                               message(X)
                               tempDT <- rads::calc(ph.data = brfs1,
                                                    what = myvar1,
                                                    year >=2017,
                                                    by = c("trnsgndr", X),
                                                    metrics=c("mean","rse","numerator","denominator"),
                                                    per=100, win=5, time_var="year", proportion=T)
                               tempDT[, cat2 := X]
                               setnames(tempDT, X, "cat2_group")
                             }), use.names = T)
res_bytns <- res_bytns %>% rename("cat1_group"="trnsgndr")
res_bytns[, ':=' (cat1 = 'Transgender', cat1_varname ='transgender')]

#-----4. by race3, excluded chi_sexorien_2 because of small sample size
res_byrace3 = rbindlist(lapply(X = as.list(by_race),
                             FUN = function(X){
                               message(X)
                               tempDT <- rads::calc(ph.data = brfs1,
                                                    what = myvar1,
                                                    year >=2017,
                                                    by = c("race3", X),
                                                    metrics=c("mean","rse","numerator","denominator"),
                                                    per=100, win=5, time_var="year", proportion=T)
                               tempDT[, cat2 := X]
                               setnames(tempDT, X, "cat2_group")
                             }), use.names = T)
res_byrace3 <- res_byrace3 %>% rename("cat1_group"="race3")
res_byrace3[, ':=' (cat1 = 'Race', cat1_varname ='race3')]

#-----5. by race4
res_byrace4 = rbindlist(lapply(X = as.list(by_race),
                               FUN = function(X){
                                 message(X)
                                 tempDT <- rads::calc(ph.data = brfs1,
                                                      what = myvar1,
                                                      year >=2017,
                                                      by = c("race4", X),
                                                      metrics=c("mean","rse","numerator","denominator"),
                                                      per=100, win=5, time_var="year", proportion=T)
                                 tempDT[, cat2 := X]
                                 setnames(tempDT, X, "cat2_group")
                               }), use.names = T)
res_byrace4 <- res_byrace4 %>% rename("cat1_group"="race4")
res_byrace4[, ':=' (cat1 = 'Race', cat1_varname ='race4')]

#-----6. by hispanic
res_byhisp = rbindlist(lapply(X = as.list(by_race),
                               FUN = function(X){
                                 message(X)
                                 tempDT <- rads::calc(ph.data = brfs1,
                                                      what = myvar1,
                                                      year >=2017,
                                                      by = c("hispanic", X),
                                                      metrics=c("mean","rse","numerator","denominator"),
                                                      per=100, win=5, time_var="year", proportion=T)
                                 tempDT[, cat2 := X]
                                 setnames(tempDT, X, "cat2_group")
                               }), use.names = T)
res_byhisp <- res_byhisp %>% rename("cat1_group"="hispanic")
res_byhisp[, ':=' (cat1 = 'Ethnicity', cat1_varname ='hispanic')]

#-----7. by income6b, excluded chi_sexorien_2
res_byinc = rbindlist(lapply(X = as.list(by_inc),
                             FUN = function(X){
                               message(X)
                               tempDT <- rads::calc(ph.data = brfs1,
                                                    what = myvar1,
                                                    year >=2017,
                                                    by = c("income6b", X),
                                                    metrics=c("mean","rse","numerator","denominator"),
                                                    per=100, win=5, time_var="year", proportion=T)
                               tempDT[, cat2 := X]
                               setnames(tempDT, X, "cat2_group")
                             }), use.names = T)
res_byinc <- res_byinc %>% rename("cat1_group"="income6b")
res_byinc[, ':=' (cat1 = 'Household income', cat1_varname ='income6b')]

#-----8. by veteran status, excluded chi_sexorien_2
res_byvet = rbindlist(lapply(X = as.list(by_vet),
                             FUN = function(X){
                               message(X)
                               tempDT <- rads::calc(ph.data = brfs1,
                                                    what = myvar1,
                                                    year >=2017,
                                                    by = c("veteran", X),
                                                    metrics=c("mean","rse","numerator","denominator"),
                                                    per=100, win=5, time_var="year", proportion=T)
                               tempDT[, cat2 := X]
                               setnames(tempDT, X, "cat2_group")
                             }), use.names = T)
res_byvet <- res_byvet %>% rename("cat1_group"="veteran")
res_byvet[, ':=' (cat1 = 'Military Service', cat1_varname ='veteran')]

#-----9. by region
res_byreg = rbindlist(lapply(X = as.list(by_reg),
                             FUN = function(X){
                               message(X)
                               tempDT <- rads::calc(ph.data = brfs1,
                                                    what = myvar1,
                                                    year >=2017,
                                                    by = c("region", X),
                                                    metrics=c("mean","rse","numerator","denominator"),
                                                    per=100, win=5, time_var="year", proportion=T)
                               tempDT[, cat2 := X]
                               setnames(tempDT, X, "cat2_group")
                             }), use.names = T)
res_byreg <- res_byreg %>% rename("cat1_group"="region")
res_byreg[, ':=' (cat1 = 'Regions', cat1_varname ='chi_geo_region')]

#-----combined all crosstab results: hra20 and bigcities are not run for crosstabs, LGB crosstab sample size are too small
cross1 <- rbind(res_byage, res_bysex, res_bylgb, res_bytns, res_byrace3, res_byrace4, res_byhisp, res_byinc, res_byvet, res_byreg)
cross1 <- cross1[!(cross1$variable == "flushot_v1" & cross1$cat1_group %in% c("65-74", "75+"))]
cross1 <- cross1[!(cross1$variable == "flushot_v1" & cross1$cat2_group %in% c("65-74", "75+"))]
write.xlsx(cross1, "cross1.xlsx", sheetName = "Sheet1", overwrite = T)


##-----run crosstab2-------
rm(list=ls(pattern="^res_by"))
#------indicators in brfs1, mygrid2 (myvar2 and brfs1)
#-----1. by age
res_byage = rbindlist(lapply(X = as.list(by_age5),
                             FUN = function(X){
                               message(X)
                               tempDT <- rads::calc(ph.data = brfs1,
                                                    what = myvar2,
                                                    year == 2016 | year==2018 | year==2020,
                                                    by = c("age5_v2", X),
                                                    metrics=c("mean","rse","numerator","denominator"),
                                                    per=100, win=5, time_var="year", proportion=T)
                               tempDT[, cat2 := X]
                               setnames(tempDT, X, "cat2_group")
                             }), use.names = T)
res_byage <- res_byage %>% rename("cat1_group"="age5_v2")
res_byage[, ':=' (cat1 = 'Age', cat1_varname ='age5_v2')]

#-----2. by sex
res_bysex = rbindlist(lapply(X = as.list(by_sex),
                             FUN = function(X){
                               message(X)
                               tempDT <- rads::calc(ph.data = brfs1,
                                                    what = myvar2,
                                                    year == 2016 | year==2018 | year==2020,
                                                    by = c("chi_sex", X),
                                                    metrics=c("mean","rse","numerator","denominator"),
                                                    per=100, win=5, time_var="year", proportion=T)
                               tempDT[, cat2 := X]
                               setnames(tempDT, X, "cat2_group")
                             }), use.names = T)
res_bysex <- res_bysex %>% rename("cat1_group"="chi_sex")
res_bysex[, ':=' (cat1 = 'Gender', cat1_varname ='chi_sex')]

#-----3. by sexual orientation
res_bylgb = rbindlist(lapply(X = as.list(by_lgb),
                             FUN = function(X){
                               message(X)
                               tempDT <- rads::calc(ph.data = brfs1,
                                                    what = myvar2,
                                                    year == 2016 | year==2018 | year==2020,
                                                    by = c("chi_sexorien_2", X),
                                                    metrics=c("mean","rse","numerator","denominator"),
                                                    per=100, win=5, time_var="year", proportion=T)
                               tempDT[, cat2 := X]
                               setnames(tempDT, X, "cat2_group")
                             }), use.names = T)
res_bylgb <- res_bylgb %>% rename("cat1_group"="chi_sexorien_2")
res_bylgb[, ':=' (cat1 = 'Sexual orientation', cat1_varname ='chi_sexorien_2')]

#-----3b. by trnsgndr
res_bytns = rbindlist(lapply(X = as.list(by_tns),
                             FUN = function(X){
                               message(X)
                               tempDT <- rads::calc(ph.data = brfs1,
                                                    what = myvar2,
                                                    year == 2016 | year==2018 | year==2020,
                                                    by = c("trnsgndr", X),
                                                    metrics=c("mean","rse","numerator","denominator"),
                                                    per=100, win=5, time_var="year", proportion=T)
                               tempDT[, cat2 := X]
                               setnames(tempDT, X, "cat2_group")
                             }), use.names = T)
res_bytns <- res_bytns %>% rename("cat1_group"="trnsgndr")
res_bytns[, ':=' (cat1 = 'Transgender', cat1_varname ='transgender')]

#-----4. by race3, excluded chi_sexorien_2 because of small sample size
res_byrace3 = rbindlist(lapply(X = as.list(by_race),
                               FUN = function(X){
                                 message(X)
                                 tempDT <- rads::calc(ph.data = brfs1,
                                                      what = myvar2,
                                                      year == 2016 | year==2018 | year==2020,
                                                      by = c("race3", X),
                                                      metrics=c("mean","rse","numerator","denominator"),
                                                      per=100, win=5, time_var="year", proportion=T)
                                 tempDT[, cat2 := X]
                                 setnames(tempDT, X, "cat2_group")
                               }), use.names = T)
res_byrace3 <- res_byrace3 %>% rename("cat1_group"="race3")
res_byrace3[, ':=' (cat1 = 'Race', cat1_varname ='race3')]

#-----5. by race4
res_byrace4 = rbindlist(lapply(X = as.list(by_race),
                               FUN = function(X){
                                 message(X)
                                 tempDT <- rads::calc(ph.data = brfs1,
                                                      what = myvar2,
                                                      year == 2016 | year==2018 | year==2020,
                                                      by = c("race4", X),
                                                      metrics=c("mean","rse","numerator","denominator"),
                                                      per=100, win=5, time_var="year", proportion=T)
                                 tempDT[, cat2 := X]
                                 setnames(tempDT, X, "cat2_group")
                               }), use.names = T)
res_byrace4 <- res_byrace4 %>% rename("cat1_group"="race4")
res_byrace4[, ':=' (cat1 = 'Race', cat1_varname ='race4')]

#-----6. by hispanic
res_byhisp = rbindlist(lapply(X = as.list(by_race),
                              FUN = function(X){
                                message(X)
                                tempDT <- rads::calc(ph.data = brfs1,
                                                     what = myvar2,
                                                     year == 2016 | year==2018 | year==2020,
                                                     by = c("hispanic", X),
                                                     metrics=c("mean","rse","numerator","denominator"),
                                                     per=100, win=5, time_var="year", proportion=T)
                                tempDT[, cat2 := X]
                                setnames(tempDT, X, "cat2_group")
                              }), use.names = T)
res_byhisp <- res_byhisp %>% rename("cat1_group"="hispanic")
res_byhisp[, ':=' (cat1 = 'Ethnicity', cat1_varname ='hispanic')]

#-----7. by income6b, excluded chi_sexorien_2
res_byinc = rbindlist(lapply(X = as.list(by_inc),
                             FUN = function(X){
                               message(X)
                               tempDT <- rads::calc(ph.data = brfs1,
                                                    what = myvar2,
                                                    year == 2016 | year==2018 | year==2020,
                                                    by = c("income6b", X),
                                                    metrics=c("mean","rse","numerator","denominator"),
                                                    per=100, win=5, time_var="year", proportion=T)
                               tempDT[, cat2 := X]
                               setnames(tempDT, X, "cat2_group")
                             }), use.names = T)
res_byinc <- res_byinc %>% rename("cat1_group"="income6b")
res_byinc[, ':=' (cat1 = 'Household income', cat1_varname ='income6b')]

#-----8. by veteran status, excluded chi_sexorien_2
res_byvet = rbindlist(lapply(X = as.list(by_vet),
                             FUN = function(X){
                               message(X)
                               tempDT <- rads::calc(ph.data = brfs1,
                                                    what = myvar2,
                                                    year == 2016 | year==2018 | year==2020,
                                                    by = c("veteran", X),
                                                    metrics=c("mean","rse","numerator","denominator"),
                                                    per=100, win=5, time_var="year", proportion=T)
                               tempDT[, cat2 := X]
                               setnames(tempDT, X, "cat2_group")
                             }), use.names = T)
res_byvet <- res_byvet %>% rename("cat1_group"="veteran")
res_byvet[, ':=' (cat1 = 'Military Service', cat1_varname ='veteran')]

#-----9. by region
res_byreg = rbindlist(lapply(X = as.list(by_reg),
                             FUN = function(X){
                               message(X)
                               tempDT <- rads::calc(ph.data = brfs1,
                                                    what = myvar2,
                                                    year == 2016 | year==2018 | year==2020,
                                                    by = c("region", X),
                                                    metrics=c("mean","rse","numerator","denominator"),
                                                    per=100, win=5, time_var="year", proportion=T)
                               tempDT[, cat2 := X]
                               setnames(tempDT, X, "cat2_group")
                             }), use.names = T)
res_byreg <- res_byreg %>% rename("cat1_group"="region")
res_byreg[, ':=' (cat1 = 'Regions', cat1_varname ='chi_geo_region')]

cross2 <- rbind(res_byage, res_bysex, res_bylgb, res_bytns, res_byrace3, res_byrace4, res_byhisp, res_byinc, res_byvet, res_byreg)
cross2$year <- as.character("2016, 2018, 2020")
write.xlsx(cross2, "cross2.xlsx", sheetName = "Sheet1", overwrite = T)


##-----run crosstab for CRC screening-------
rm(list=ls(pattern="^res_by"))
rm(list=ls(pattern="^by_"))

by_age <- c("chi_sex", "chi_sexorien_2", "race3", "hispanic", "race4", "income6b", "veteran", "region")
by_sex <- c("age_crc", "chi_sexorien_2", "race3", "hispanic", "race4", "income6b", "veteran", "region")
by_lgb <- c("age_crc", "chi_sex", "race3", "hispanic", "race4", "income6b", "veteran", "region")
by_tns <- c("age_crc", "chi_sex", "race3", "hispanic", "race4", "income6b", "veteran", "region")
by_race <-c("age_crc", "chi_sex", "chi_sexorien_2", "income6b", "veteran", "region")
by_inc <- c("age_crc", "chi_sex", "chi_sexorien_2", "race3", "hispanic", "race4", "veteran", "region")
by_vet <- c("age_crc", "chi_sex", "chi_sexorien_2", "race3", "hispanic", "race4", "income6b", "region")
by_reg <- c("age_crc", "chi_sex", "chi_sexorien_2", "race3", "hispanic", "race4", "income6b", "veteran")

#------indicators in brfs1, mygrid3 (myvar3, and byvar3 and brfs1)
#-----1. by age
res_byage = rbindlist(lapply(X = as.list(by_age),
                             FUN = function(X){
                               message(X)
                               tempDT <- rads::calc(ph.data = brfs1,
                                                    what = myvar3,
                                                    year == 2016 | year==2018 | year==2020,
                                                    by = c("age_crc", X),
                                                    metrics=c("mean","rse","numerator","denominator"),
                                                    per=100, win=5, time_var="year", proportion=T)
                               tempDT[, cat2 := X]
                               setnames(tempDT, X, "cat2_group")
                             }), use.names = T)
res_byage <- res_byage %>% rename("cat1_group"="age_crc")
res_byage[, ':=' (cat1 = 'Age', cat1_varname ='age_crc')]

#-----2. by sex
res_bysex = rbindlist(lapply(X = as.list(by_sex),
                             FUN = function(X){
                               message(X)
                               tempDT <- rads::calc(ph.data = brfs1,
                                                    what = myvar3,
                                                    year == 2016 | year==2018 | year==2020,
                                                    by = c("chi_sex", X),
                                                    metrics=c("mean","rse","numerator","denominator"),
                                                    per=100, win=5, time_var="year", proportion=T)
                               tempDT[, cat2 := X]
                               setnames(tempDT, X, "cat2_group")
                             }), use.names = T)
res_bysex <- res_bysex %>% rename("cat1_group"="chi_sex")
res_bysex[, ':=' (cat1 = 'Gender', cat1_varname ='chi_sex')]


#-----3. by sexual orientation
res_bylgb = rbindlist(lapply(X = as.list(by_lgb),
                             FUN = function(X){
                               message(X)
                               tempDT <- rads::calc(ph.data = brfs1,
                                                    what = myvar3,
                                                    year == 2016 | year==2018 | year==2020,
                                                    by = c("chi_sexorien_2", X),
                                                    metrics=c("mean","rse","numerator","denominator"),
                                                    per=100, win=5, time_var="year", proportion=T)
                               tempDT[, cat2 := X]
                               setnames(tempDT, X, "cat2_group")
                             }), use.names = T)
res_bylgb <- res_bylgb %>% rename("cat1_group"="chi_sexorien_2")
res_bylgb[, ':=' (cat1 = 'Sexual orientation', cat1_varname ='chi_sexorien_2')]

#-----3b. by trnsgndr
res_bytns = rbindlist(lapply(X = as.list(by_tns),
                             FUN = function(X){
                               message(X)
                               tempDT <- rads::calc(ph.data = brfs1,
                                                    what = myvar3,
                                                    year == 2016 | year==2018 | year==2020,
                                                    by = c("trnsgndr", X),
                                                    metrics=c("mean","rse","numerator","denominator"),
                                                    per=100, win=5, time_var="year", proportion=T)
                               tempDT[, cat2 := X]
                               setnames(tempDT, X, "cat2_group")
                             }), use.names = T)
res_bytns <- res_bytns %>% rename("cat1_group"="trnsgndr")
res_bytns[, ':=' (cat1 = 'Transgender', cat1_varname ='transgender')]

#-----4. by race3, excluded chi_sexorien_2 because of small sample size
res_byrace3 = rbindlist(lapply(X = as.list(by_race),
                               FUN = function(X){
                                 message(X)
                                 tempDT <- rads::calc(ph.data = brfs1,
                                                      what = myvar3,
                                                      year == 2016 | year==2018 | year==2020,
                                                      by = c("race3", X),
                                                      metrics=c("mean","rse","numerator","denominator"),
                                                      per=100, win=5, time_var="year", proportion=T)
                                 tempDT[, cat2 := X]
                                 setnames(tempDT, X, "cat2_group")
                               }), use.names = T)
res_byrace3 <- res_byrace3 %>% rename("cat1_group"="race3")
res_byrace3[, ':=' (cat1 = 'Race', cat1_varname ='race3')]

#-----5. by race4
res_byrace4 = rbindlist(lapply(X = as.list(by_race),
                               FUN = function(X){
                                 message(X)
                                 tempDT <- rads::calc(ph.data = brfs1,
                                                      what = myvar3,
                                                      year == 2016 | year==2018 | year==2020,
                                                      by = c("race4", X),
                                                      metrics=c("mean","rse","numerator","denominator"),
                                                      per=100, win=5, time_var="year", proportion=T)
                                 tempDT[, cat2 := X]
                                 setnames(tempDT, X, "cat2_group")
                               }), use.names = T)
res_byrace4 <- res_byrace4 %>% rename("cat1_group"="race4")
res_byrace4[, ':=' (cat1 = 'Race', cat1_varname ='race4')]

#-----6. by hispanic
res_byhisp = rbindlist(lapply(X = as.list(by_race),
                              FUN = function(X){
                                message(X)
                                tempDT <- rads::calc(ph.data = brfs1,
                                                     what = myvar3,
                                                     year == 2016 | year==2018 | year==2020,
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
res_byhisp[, ':=' (cat1 = 'Ethnicity', cat1_varname ='hispanic')]

#-----7. by income6b, excluded chi_sexorien_2
res_byinc = rbindlist(lapply(X = as.list(by_inc),
                             FUN = function(X){
                               message(X)
                               tempDT <- rads::calc(ph.data = brfs1,
                                                    what = myvar3,
                                                    year == 2016 | year==2018 | year==2020,
                                                    by = c("income6b", X),
                                                    metrics=c("mean","rse","numerator","denominator"),
                                                    per=100, win=5, time_var="year", proportion=T)
                               tempDT[, cat2 := X]
                               setnames(tempDT, X, "cat2_group")
                             }), use.names = T)
res_byinc <- res_byinc %>% rename("cat1_group"="income6b")
res_byinc[, ':=' (cat1 = 'Household income', cat1_varname ='income6b')]

#-----8. by veteran status, excluded chi_sexorien_2
res_byvet = rbindlist(lapply(X = as.list(by_vet),
                             FUN = function(X){
                               message(X)
                               tempDT <- rads::calc(ph.data = brfs1,
                                                    what = myvar3,
                                                    year == 2016 | year==2018 | year==2020,
                                                    by = c("veteran", X),
                                                    metrics=c("mean","rse","numerator","denominator"),
                                                    per=100, win=5, time_var="year", proportion=T)
                               tempDT[, cat2 := X]
                               setnames(tempDT, X, "cat2_group")
                             }), use.names = T)
res_byvet <- res_byvet %>% rename("cat1_group"="veteran")
res_byvet[, ':=' (cat1 = 'Military Service', cat1_varname ='veteran')]

#-----9. by region
res_byreg = rbindlist(lapply(X = as.list(by_reg),
                             FUN = function(X){
                               message(X)
                               tempDT <- rads::calc(ph.data = brfs1,
                                                    what = myvar3,
                                                    year == 2016 | year==2018 | year==2020,
                                                    by = c("region", X),
                                                    metrics=c("mean","rse","numerator","denominator"),
                                                    per=100, win=5, time_var="year", proportion=T)
                               tempDT[, cat2 := X]
                               setnames(tempDT, X, "cat2_group")
                             }), use.names = T)
res_byreg <- res_byreg %>% rename("cat1_group"="region")
res_byreg[, ':=' (cat1 = 'Regions', cat1_varname ='chi_geo_region')]

cross3 <- rbind(res_byage, res_bysex, res_bylgb, res_bytns, res_byrace3, res_byrace4, res_byhisp, res_byinc, res_byvet, res_byreg)
cross3$year <- as.character("2016, 2018, 2020")
write.xlsx(cross3, "cross3.xlsx", sheetName = "Sheet1", overwrite = T)


##-----run crosstab for mammogram screening: mam2yrs-------
rm(list=ls(pattern="^res_by"))
rm(list=ls(pattern="^by_"))

by_age <- c("chi_sex", "chi_sexorien_2", "race3", "hispanic", "race4", "income6b", "veteran", "region")
by_lgb <- c("age_mam", "race3", "hispanic", "race4", "income6b", "veteran", "region")
by_tns <- c("age_mam", "race3", "hispanic", "race4", "income6b", "veteran", "region")
by_race <-c("age_mam", "chi_sexorien_2", "income6b", "veteran", "region")
by_inc <- c("age_mam", "chi_sexorien_2", "race3", "hispanic", "race4", "veteran", "region")
by_vet <- c("age_mam", "chi_sexorien_2", "race3", "hispanic", "race4", "income6b", "region")
by_reg <- c("age_mam", "chi_sexorien_2", "race3", "hispanic", "race4", "income6b", "veteran")

#------indicators in brfs2, myvar4, and byvar4
#-----1. by age
res_byage = rbindlist(lapply(X = as.list(by_age),
                             FUN = function(X){
                               message(X)
                               tempDT <- rads::calc(ph.data = brfs2,
                                                    what = myvar4,
                                                    year == 2016 | year==2018 | year==2020,
                                                    by = c("age_mam", X),
                                                    metrics=c("mean","rse","numerator","denominator"),
                                                    per=100, win=5, time_var="year", proportion=T)
                               tempDT[, cat2 := X]
                               setnames(tempDT, X, "cat2_group")
                             }), use.names = T)
res_byage <- res_byage %>% rename("cat1_group"="age_mam")
res_byage[, ':=' (cat1 = 'Age', cat1_varname ='age_mam')]

#-----3. by sexual orientation
res_bylgb = rbindlist(lapply(X = as.list(by_lgb),
                             FUN = function(X){
                               message(X)
                               tempDT <- rads::calc(ph.data = brfs2,
                                                    what = myvar4,
                                                    year == 2016 | year==2018 | year==2020,
                                                    by = c("chi_sexorien_2", X),
                                                    metrics=c("mean","rse","numerator","denominator"),
                                                    per=100, win=5, time_var="year", proportion=T)
                               tempDT[, cat2 := X]
                               setnames(tempDT, X, "cat2_group")
                             }), use.names = T)
res_bylgb <- res_bylgb %>% rename("cat1_group"="chi_sexorien_2")
res_bylgb[, ':=' (cat1 = 'Sexual orientation', cat1_varname ='chi_sexorien_2')]

#-----3b. by trnsgndr
res_bytns = rbindlist(lapply(X = as.list(by_tns),
                             FUN = function(X){
                               message(X)
                               tempDT <- rads::calc(ph.data = brfs2,
                                                    what = myvar4,
                                                    year == 2016 | year==2018 | year==2020,
                                                    by = c("trnsgndr", X),
                                                    metrics=c("mean","rse","numerator","denominator"),
                                                    per=100, win=5, time_var="year", proportion=T)
                               tempDT[, cat2 := X]
                               setnames(tempDT, X, "cat2_group")
                             }), use.names = T)
res_bytns <- res_bytns %>% rename("cat1_group"="trnsgndr")
res_bytns[, ':=' (cat1 = 'Transgender', cat1_varname ='transgender')]


#-----4. by race3, excluded chi_sexorien_2 because of small sample size
res_byrace3 = rbindlist(lapply(X = as.list(by_race),
                               FUN = function(X){
                                 message(X)
                                 tempDT <- rads::calc(ph.data = brfs2,
                                                      what = myvar4,
                                                      year == 2016 | year==2018 | year==2020,
                                                      by = c("race3", X),
                                                      metrics=c("mean","rse","numerator","denominator"),
                                                      per=100, win=5, time_var="year", proportion=T)
                                 tempDT[, cat2 := X]
                                 setnames(tempDT, X, "cat2_group")
                               }), use.names = T)
res_byrace3 <- res_byrace3 %>% rename("cat1_group"="race3")
res_byrace3[, ':=' (cat1 = 'Race', cat1_varname ='race3')]

#-----5. by race4
res_byrace4 = rbindlist(lapply(X = as.list(by_race),
                               FUN = function(X){
                                 message(X)
                                 tempDT <- rads::calc(ph.data = brfs2,
                                                      what = myvar4,
                                                      year == 2016 | year==2018 | year==2020,
                                                      by = c("race4", X),
                                                      metrics=c("mean","rse","numerator","denominator"),
                                                      per=100, win=5, time_var="year", proportion=T)
                                 tempDT[, cat2 := X]
                                 setnames(tempDT, X, "cat2_group")
                               }), use.names = T)
res_byrace4 <- res_byrace4 %>% rename("cat1_group"="race4")
res_byrace4[, ':=' (cat1 = 'Race', cat1_varname ='race4')]

#-----6. by hispanic
res_byhisp = rbindlist(lapply(X = as.list(by_race),
                              FUN = function(X){
                                message(X)
                                tempDT <- rads::calc(ph.data = brfs2,
                                                     what = myvar4,
                                                     year == 2016 | year==2018 | year==2020,
                                                     by = c("hispanic", X),
                                                     metrics=c("mean","rse","numerator","denominator"),
                                                     per=100, win=5, time_var="year", proportion=T)
                                tempDT[, cat2 := X]
                                setnames(tempDT, X, "cat2_group")
                              }), use.names = T)
res_byhisp <- res_byhisp %>% rename("cat1_group"="hispanic")
res_byhisp[, ':=' (cat1 = 'Ethnicity', cat1_varname ='hispanic')]

#-----7. by income6b, excluded chi_sexorien_2
res_byinc = rbindlist(lapply(X = as.list(by_inc),
                             FUN = function(X){
                               message(X)
                               tempDT <- rads::calc(ph.data = brfs2,
                                                    what = myvar4,
                                                    year == 2016 | year==2018 | year==2020,
                                                    by = c("income6b", X),
                                                    metrics=c("mean","rse","numerator","denominator"),
                                                    per=100, win=5, time_var="year", proportion=T)
                               tempDT[, cat2 := X]
                               setnames(tempDT, X, "cat2_group")
                             }), use.names = T)
res_byinc <- res_byinc %>% rename("cat1_group"="income6b")
res_byinc[, ':=' (cat1 = 'Household income', cat1_varname ='income6b')]

#-----8. by veteran status, excluded chi_sexorien_2
res_byvet = rbindlist(lapply(X = as.list(by_vet),
                             FUN = function(X){
                               message(X)
                               tempDT <- rads::calc(ph.data = brfs2,
                                                    what = myvar4,
                                                    year == 2016 | year==2018 | year==2020,
                                                    by = c("veteran", X),
                                                    metrics=c("mean","rse","numerator","denominator"),
                                                    per=100, win=5, time_var="year", proportion=T)
                               tempDT[, cat2 := X]
                               setnames(tempDT, X, "cat2_group")
                             }), use.names = T)
res_byvet <- res_byvet %>% rename("cat1_group"="veteran")
res_byvet[, ':=' (cat1 = 'Military Service', cat1_varname ='veteran')]

#-----9. by region
res_byreg = rbindlist(lapply(X = as.list(by_reg),
                             FUN = function(X){
                               message(X)
                               tempDT <- rads::calc(ph.data = brfs2,
                                                    what = myvar4,
                                                    year == 2016 | year==2018 | year==2020,
                                                    by = c("region", X),
                                                    metrics=c("mean","rse","numerator","denominator"),
                                                    per=100, win=5, time_var="year", proportion=T)
                               tempDT[, cat2 := X]
                               setnames(tempDT, X, "cat2_group")
                             }), use.names = T)
res_byreg <- res_byreg %>% rename("cat1_group"="region")
res_byreg[, ':=' (cat1 = 'Regions', cat1_varname ='chi_geo_region')]

cross4 <- rbind(res_byage, res_bylgb, res_bytns, res_byrace3, res_byrace4, res_byhisp, res_byinc, res_byvet, res_byreg)
cross4$year <- as.character("2016, 2018, 2020")
write.xlsx(cross4, "cross4.xlsx", sheetName = "Sheet1", overwrite = T)


##-----run crosstab for cervical cancer screening: pap3yrs-------
rm(list=ls(pattern="^res_by"))
rm(list=ls(pattern="^by_"))

by_age <- c( "chi_sexorien_2", "race3", "hispanic", "race4", "income6b", "veteran", "region")
by_lgb <- c("age_pap", "race3", "hispanic", "race4", "income6b", "veteran", "region")
by_tns <- c("age_pap", "race3", "hispanic", "race4", "income6b", "veteran", "region")
by_race <-c("age_pap", "chi_sexorien_2", "income6b", "veteran", "region")
by_inc <- c("age_pap", "chi_sexorien_2", "race3", "hispanic", "race4", "veteran", "region")
by_vet <- c("age_pap", "chi_sexorien_2", "race3", "hispanic", "race4", "income6b", "region")
by_reg <- c("age_pap", "chi_sexorien_2", "race3", "hispanic", "race4", "income6b", "veteran")

#------indicators in brfs2, myvar5, and byvar5
#-----1. by age
res_byage = rbindlist(lapply(X = as.list(by_age),
                             FUN = function(X){
                               message(X)
                               tempDT <- rads::calc(ph.data = brfs2,
                                                    what = myvar5,
                                                    year == 2016 | year==2018 | year==2020,
                                                    by = c("age_pap", X),
                                                    metrics=c("mean","rse","numerator","denominator"),
                                                    per=100, win=5, time_var="year", proportion=T)
                               tempDT[, cat2 := X]
                               setnames(tempDT, X, "cat2_group")
                             }), use.names = T)
res_byage <- res_byage %>% rename("cat1_group"="age_pap")
res_byage[, ':=' (cat1 = 'Age', cat1_varname ='age_pap')]

#-----3. by sexual orientation
res_bylgb = rbindlist(lapply(X = as.list(by_lgb),
                             FUN = function(X){
                               message(X)
                               tempDT <- rads::calc(ph.data = brfs2,
                                                    what = myvar5,
                                                    year == 2016 | year==2018 | year==2020,
                                                    by = c("chi_sexorien_2", X),
                                                    metrics=c("mean","rse","numerator","denominator"),
                                                    per=100, win=5, time_var="year", proportion=T)
                               tempDT[, cat2 := X]
                               setnames(tempDT, X, "cat2_group")
                             }), use.names = T)
res_bylgb <- res_bylgb %>% rename("cat1_group"="chi_sexorien_2")
res_bylgb[, ':=' (cat1 = 'Sexual orientation', cat1_varname ='chi_sexorien_2')]

#-----3b. by trnsgndr
res_bytns = rbindlist(lapply(X = as.list(by_tns),
                             FUN = function(X){
                               message(X)
                               tempDT <- rads::calc(ph.data = brfs2,
                                                    what = myvar5,
                                                    year == 2016 | year==2018 | year==2020,
                                                    by = c("trnsgndr", X),
                                                    metrics=c("mean","rse","numerator","denominator"),
                                                    per=100, win=5, time_var="year", proportion=T)
                               tempDT[, cat2 := X]
                               setnames(tempDT, X, "cat2_group")
                             }), use.names = T)
res_bytns <- res_bytns %>% rename("cat1_group"="trnsgndr")
res_bytns[, ':=' (cat1 = 'Transgender', cat1_varname ='transgender')]

#-----4. by race3, excluded chi_sexorien_2 because of small sample size
res_byrace3 = rbindlist(lapply(X = as.list(by_race),
                               FUN = function(X){
                                 message(X)
                                 tempDT <- rads::calc(ph.data = brfs2,
                                                      what = myvar5,
                                                      year == 2016 | year==2018 | year==2020,
                                                      by = c("race3", X),
                                                      metrics=c("mean","rse","numerator","denominator"),
                                                      per=100, win=5, time_var="year", proportion=T)
                                 tempDT[, cat2 := X]
                                 setnames(tempDT, X, "cat2_group")
                               }), use.names = T)
res_byrace3 <- res_byrace3 %>% rename("cat1_group"="race3")
res_byrace3[, ':=' (cat1 = 'Race', cat1_varname ='race3')]

#-----5. by race4
res_byrace4 = rbindlist(lapply(X = as.list(by_race),
                               FUN = function(X){
                                 message(X)
                                 tempDT <- rads::calc(ph.data = brfs2,
                                                      what = myvar5,
                                                      year == 2016 | year==2018 | year==2020,
                                                      by = c("race4", X),
                                                      metrics=c("mean","rse","numerator","denominator"),
                                                      per=100, win=5, time_var="year", proportion=T)
                                 tempDT[, cat2 := X]
                                 setnames(tempDT, X, "cat2_group")
                               }), use.names = T)
res_byrace4 <- res_byrace4 %>% rename("cat1_group"="race4")
res_byrace4[, ':=' (cat1 = 'Race', cat1_varname ='race4')]

#-----6. by hispanic
res_byhisp = rbindlist(lapply(X = as.list(by_race),
                              FUN = function(X){
                                message(X)
                                tempDT <- rads::calc(ph.data = brfs2,
                                                     what = myvar5,
                                                     year == 2016 | year==2018 | year==2020,
                                                     by = c("hispanic", X),
                                                     metrics=c("mean","rse","numerator","denominator"),
                                                     per=100, win=5, time_var="year", proportion=T)
                                tempDT[, cat2 := X]
                                setnames(tempDT, X, "cat2_group")
                              }), use.names = T)
res_byhisp <- res_byhisp %>% rename("cat1_group"="hispanic")
res_byhisp[, ':=' (cat1 = 'Ethnicity', cat1_varname ='hispanic')]

#-----7. by income6b, excluded chi_sexorien_2
res_byinc = rbindlist(lapply(X = as.list(by_inc),
                             FUN = function(X){
                               message(X)
                               tempDT <- rads::calc(ph.data = brfs2,
                                                    what = myvar5,
                                                    year == 2016 | year==2018 | year==2020,
                                                    by = c("income6b", X),
                                                    metrics=c("mean","rse","numerator","denominator"),
                                                    per=100, win=5, time_var="year", proportion=T)
                               tempDT[, cat2 := X]
                               setnames(tempDT, X, "cat2_group")
                             }), use.names = T)
res_byinc <- res_byinc %>% rename("cat1_group"="income6b")
res_byinc[, ':=' (cat1 = 'Household income', cat1_varname ='income6b')]

#-----8. by veteran status, excluded chi_sexorien_2
res_byvet = rbindlist(lapply(X = as.list(by_vet),
                             FUN = function(X){
                               message(X)
                               tempDT <- rads::calc(ph.data = brfs2,
                                                    what = myvar5,
                                                    year == 2016 | year==2018 | year==2020,
                                                    by = c("veteran", X),
                                                    metrics=c("mean","rse","numerator","denominator"),
                                                    per=100, win=5, time_var="year", proportion=T)
                               tempDT[, cat2 := X]
                               setnames(tempDT, X, "cat2_group")
                             }), use.names = T)
res_byvet <- res_byvet %>% rename("cat1_group"="veteran")
res_byvet[, ':=' (cat1 = 'Military Service', cat1_varname ='veteran')]

#-----9. by region
res_byreg = rbindlist(lapply(X = as.list(by_reg),
                             FUN = function(X){
                               message(X)
                               tempDT <- rads::calc(ph.data = brfs2,
                                                    what = myvar5,
                                                    year == 2016 | year==2018 | year==2020,
                                                    by = c("region", X),
                                                    metrics=c("mean","rse","numerator","denominator"),
                                                    per=100, win=5, time_var="year", proportion=T)
                               tempDT[, cat2 := X]
                               setnames(tempDT, X, "cat2_group")
                             }), use.names = T)
res_byreg <- res_byreg %>% rename("cat1_group"="region")
res_byreg[, ':=' (cat1 = 'Regions', cat1_varname ='chi_geo_region')]

#-----combined all crosstab results: hra20 and bigcities are not run for crosstabs, LGB crosstab sample size are too small
cross5 <- rbind(res_byage, res_bylgb, res_bytns, res_byrace3, res_byrace4, res_byhisp, res_byinc, res_byvet, res_byreg)
cross5$year <- as.character("2016, 2018, 2020")
write.xlsx(cross5, "cross5.xlsx", sheetName = "Sheet1", overwrite = T)

cross1 <- read.xlsx("cross1.xlsx")
cross2 <- read.xlsx("cross2.xlsx")
cross3 <- read.xlsx("cross3.xlsx")
cross4 <- read.xlsx("cross4.xlsx")
cross5 <- read.xlsx("cross5.xlsx")


#-----merging crosstab results
crossx <- bind_rows(cross1, cross2, cross3, cross4, cross5)
crossx <- subset(crossx, !(cat1_group %in% c("Non-Hispanic", "NA", "Other/unknown", "Other", "x_other", NA)))
crossx <- subset(crossx, !(cat2_group %in% c("Non-Hispanic", "NA", "Other/unknown", "Other", "x_other", NA)))

crossx <- crossx %>% mutate(cat1_varname = case_when(cat1_varname=="hispanic" ~"race3",
                                                 cat1_varname=="trnsgndr" ~"Transgender",
                                                 TRUE ~ cat1_varname))

crossx <- crossx %>% mutate(cat1_varname = case_when(cat1=="Ethnicity" & cat1_varname=="hispanic" ~ "race3", 
                                                     TRUE ~cat1_varname))

crossx$cat2_varname <- crossx$cat2
crossx <- crossx %>% mutate(cat2 = case_when(cat2=="age5_v2" ~ "Age",
                                             cat2=="age_crc" ~ "Age",
                                             cat2=="age_mam" ~ "Age",
                                             cat2=="age_pap" ~ "Age",
                                             cat2=="chi_sex" ~"Gender", 
                                             cat2=="chi_sexorien_2" ~"Sexual orientation",
                                             cat2=="race3" ~"Race",
                                             cat2=="hispanic" ~"Ethnicity", 
                                             cat2=="race4" ~"Race",
                                             cat2=="income6b" ~ "Household income",
                                             cat2=="veteran" ~ "Military Service",
                                             cat2=="region" ~"Regions",
                                             TRUE ~ NA_character_))

crossx <- crossx %>% mutate(cat2_varname = case_when(cat2=="Ethnicity" & cat2_varname=="hispanic" ~ "race3", 
                                                     cat2=="Regions" ~"chi_geo_region",
                                                     TRUE ~cat2_varname))

crossx$tab <-"crosstabs"
crossx <- crossx[, c("variable", "tab", "year", "cat1", "cat1_varname", "cat1_group", "cat2", "cat2_varname", "cat2_group",
                      "mean", "mean_se", "mean_lower", "mean_upper", "rse", "numerator", "denominator")]
crossx <- crossx[order(crossx$variable, crossx$cat1, crossx$cat2), ]
write.xlsx(crossx, "crossx.xlsx", sheetName = "Sheet1", overwrite = T)
#----------------------------------------

#-----merging demographic and crosstab results
res2 <- read.xlsx("brfss_demo.xlsx", sheet = 1)
crossx <- read.xlsx("crossx.xlsx", sheet = 1)

resultx <- bind_rows(res2, crossx)
resultx <- resultx %>% mutate(year = case_when(
  variable %in% c("asthnow", "cvdheart", "cvdstrok", "diab2", "disabil", "flushot_v1", "flushot_v2", 
                  "genhlth2",  "medcost", "fmd", "obese", "overw1", "persdoc", "pneumvac", "smoker1") ~"2017-2021",
  variable %in% c("bphigh", "cholchk5", "x_veglt1a") ~"2017, 2019, & 2021", 
  variable=="ecignow1" ~"2017, 2020, & 2021",
  variable=="fnotlast" ~"2018-2021",
  variable=="mjpast30" ~ "2017-2019, & 2021",
  variable=="ssb" ~"2018-2019", 
  variable %in% c("denvst1", "firearm4", "x_crcrec", "mam2yrs", "pap3yrs") ~"2017, 2019, & 2021", 
))
tab1(resultx$year, graph=F)



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

resultx <- resultx %>% rename("result"="mean", 
                              "se"="mean_se", "lower_bound"="mean_lower", "upper_bound"="mean_upper")

resultx <- resultx[, c("tab", "year", "variable", "cat1", "cat1_varname", "cat1_group", "cat2", "cat2_varname", "cat2_group",
                       "result", "lower_bound", "upper_bound", "se", "rse", "numerator", "denominator",
                       "comparison_with_kc", "significance")]
resultx <- resultx[order(resultx$tab, resultx$variable, resultx$cat1, resultx$cat1_group, resultx$year), ]
write.xlsx(resultx, "resultx.xlsx", sheetName = "Sheet1", overwrite = T)


#-----Trend analysis-----
result6.function <- function(X){
  temp <- calc(ph.data = brfs1,
               what = paste0(mygrid6[X, ]$myvars),
               year >=2012,
               metrics = c("mean", "rse", "numerator", "denominator"),
               per = 100,
               win = 3,
               time_var = "year",
               proportion = T,
               by =paste0(mygrid6[X, ]$byvars))  
   temp[, byvar := paste0(mygrid6[X, ]$byvars)]
   setnames(temp,  paste0(mygrid6[X, ]$byvars), "byvar_level")
   setcolorder(temp, c("variable", "level", "byvar", "byvar_level"))
  return(temp)
}

result6a <- rbindlist(
  lapply(X = as.list(seq(1, nrow(mygrid6))), result6.function), 
  use.names = T, fill = T
)

trend1 <- result6a
trend1 <- trend1 %>% mutate(cat1 = case_when(byvar=="all" ~"King County",
                                         byvar=="chi_sexorien_2" ~"Sexual orientation",
                                         byvar=="hispanic" ~"Ethnicity",
                                         byvar=="race3" ~"Race",
                                         byvar=="race4" ~"Race",
                                         byvar=="region" ~"Regions", TRUE ~ ""))

trend1 <- trend1 %>% mutate(cat1_varname = case_when(byvar=="all" ~"chi_geo_kc",
                                                 byvar=="hispanic" ~"race3",
                                                 byvar=="region" ~"chi_geo_region", TRUE ~ byvar))


trend1 <- trend1 %>% rename("cat1_group"="byvar_level")
trend1 <- trend1[!(cat1_group %in% c("Non-Hispanic", "NA", "Other/unknown", "Other", NA))]

trend1 <- trend1[!(year %in% c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "character(0)")),]
trend1 <- trend1 %>% mutate(year = case_when(year=="2012, 2014" ~ "2012 & 2014", 
                                             year=="2013, 2015" ~ "2013 & 2015", 
                                             year=="2015, 2017" ~ "2015 & 2017", 
                                             year=="2016, 2018" ~ "2016 & 2018", 
                                             year=="2017, 2019" ~ "2017 & 2019", 
                                             year=="2018, 2020" ~ "2018 & 2020",
                                             year=="2019, 2021" ~ "2019 & 2021",
                                             TRUE ~ year))

trend1 <- trend1 %>% mutate(cat1_group = case_when(cat1_group=="Total" ~ "King County", TRUE ~cat1_group))
trend1$tab <- "trends"

#-----King County average for significance comparison-----
tnd_kc <- subset(trend1, cat1=="King County", select= c("variable", "year",  "mean", "mean_lower", "mean_upper"))
tnd_kc <- tnd_kc %>% rename("kc_result" = "mean", "kc_lower" = "mean_lower", "kc_upper"="mean_upper")

trend2 <- merge(x=trend1, y=tnd_kc, by=c("variable", "year"))
trend2 <- trend2 %>% mutate(comparison_with_kc = case_when(mean_upper < kc_lower ~as.character("lower"), 
                                                             mean_lower > kc_upper ~as.character("higher"), 
                                                             TRUE ~as.character("not different")))

trend2 <- trend2 %>% mutate(significance = case_when(mean_upper < kc_lower ~as.character("*"), 
                                                       mean_lower > kc_upper ~as.character("*"), 
                                                       TRUE ~as.character("")))

trend2 <- trend2 %>% rename("result"="mean", "se"="mean_se", "lower_bound"="mean_lower", "upper_bound"="mean_upper")
trend2 <- trend2[, c("tab", "year", "variable", "cat1", "cat1_varname", "cat1_group", "result", "lower_bound", "upper_bound",
                     "se", "rse", "numerator", "denominator", "comparison_with_kc", "significance")]
trend2 <- trend2[order(variable, cat1, cat1_varname, cat1_group, year), ]
write.xlsx(trend2, "resultt.xlsx", sheetName = "Sheet1", overwrite = T)


#-----WA State data-----
wa0021 <- readRDS("//dphcifs/APDE-CDIP/BRFSS/WA/wa0021.rds")
wa1 <- subset(wa0021, year>=2016)
wa1 <- wa1 %>% rename("denvst1"="x_denvst1")



wa1 <- wa1[, .(year, x_ststr, x_llcpwt, income6,  
             age, age7, sex, sexorien, race3, race4, hispanic, veteran3, 
             asthnow, bphigh, cholchk5, x_crcrec, cvdheart, cvdstrk3, denvst1, diab2,  
             disab2, ecignow1, firearm4, flushot_v1, flushot_v2, fnotlast, genhlth2, mam2yrs, medcost1,
             fmd, mjnow, obese, x_bmi5cat, x_veglt1a, pap3yrs, persdoc3, x_pneumo3, smoker1)]

wa1 <- setnames(wa1, c("cvdstrk3", "disab2", "medcost1", "mjnow", "persdoc3", "x_pneumo3"),
               c("cvdstrok", "disabil", "medcost", "mjpast30", "persdoc", "pneumvac"))

wa1[x_bmi5cat<=2 | x_bmi5cat==4,  overw1 := 0] [x_bmi5cat==3, overw1 := 1] [is.na(x_bmi5cat), overw1 :=NA]

wa1$all <- as.character("all")
setDT(wa1)
wa1[sex==1, chi_sex :="Male"] [sex==2, chi_sex :="Female"] [sex>=7, chi_sex :=NA]
wa2 <- subset(wa1, chi_sex=="Female")

#indicator lists ("within 2017-2021" variables, "2016, 2018, 2020" variables, and variables for females, no SSB for WA
myvar1 <- c("asthnow", "cvdheart", "cvdstrok", "diab2", "disabil", "flushot_v1", "flushot_v2",
            "genhlth2", "medcost", "fmd", "obese", "overw1", "persdoc", "pneumvac", "smoker1",
            "bphigh", "cholchk5", "ecignow1", "fnotlast", "mjpast30", "x_veglt1a")
myvar2 <- c("denvst1", "firearm4")
myvar3 <- c("x_crcrec")
myvar4 <- c("mam2yrs")
myvar5 <- c("pap3yrs")

byvar1 <- c("all")

mygrid1 <- data.frame(expand.grid(myvars = myvar1, byvars = byvar1))
mygrid2 <- data.frame(expand.grid(myvars = myvar2, byvars = byvar1))
mygrid3 <- data.frame(expand.grid(myvars = myvar3, byvars = byvar1))
mygrid4 <- data.frame(expand.grid(myvars = myvar4, byvars = byvar1))
mygrid5 <- data.frame(expand.grid(myvars = myvar5, byvars = byvar1))

options(survey.lonely.psu = "adjust")
wabrfs1 <- dtsurvey(DT=wa1, psu=NULL, weight= "x_llcpwt", strata="x_ststr")
wabrfs2 <- dtsurvey(DT=wa2, psu=NULL, weight= "x_llcpwt", strata="x_ststr")

#testing RADS calc results
#wabrfs3 <- svydesign(id=~1, weights=~x_llcpwt, strata = ~x_ststr, survey.longly.psu = "adjust", data=wa1)
# prop.table(svytable(~bphigh, wabrfs3), margin=2)

##-----run WA State total
#-----run 2017-2021 data
result1.function <- function(X){
  temp <- calc(ph.data = wabrfs1,
               what = paste0(mygrid1[X, ]$myvars),
               year >=2017,
               metrics = c("mean", "rse", "numerator", "denominator"),
               per = 100, time_var = "year", proportion = T,
               by =paste0(mygrid1[X, ]$byvars))  
  temp[, byvar := paste0(mygrid1[X, ]$byvars)]
  setnames(temp,  paste0(mygrid1[X, ]$byvars), "byvar_level")
  setcolorder(temp, c("variable", "level", "byvar", "byvar_level"))
  return(temp)
}

result1a <- rbindlist(
  lapply(X = as.list(seq(1, nrow(mygrid1))), result1.function), 
  use.names = T, fill = T
)

#-----run 2016, 2018, 2020 data
result2.function <- function(X){
  temp <- calc(ph.data = wabrfs1,
               what = paste0(mygrid2[X, ]$myvars),
               year==2016 | year==2018 | year==2020,
               metrics = c("mean", "rse", "numerator", "denominator"),
               per = 100, time_var = "year", proportion = T,
               by=paste0(mygrid2[X, ]$byvars))  
  temp[, byvar := paste0(mygrid2[X, ]$byvars)]
  setnames(temp,  paste0(mygrid2[X, ]$byvars), "byvar_level")
  setcolorder(temp, c("variable", "level", "byvar", "byvar_level"))
  return(temp)
}

result2a <- rbindlist(
  lapply(X = as.list(seq(1, nrow(mygrid2))), result2.function), 
  use.names = T, fill = T
)

#-----run CRC screening data
result3.function <- function(X){
  temp <- calc(ph.data = wabrfs1,
               what = paste0(mygrid3[X, ]$myvars),
               year==2016 | year==2018 | year==2020,
               metrics = c("mean", "rse", "numerator", "denominator"),
               per = 100, time_var = "year", proportion = T,
               by=paste0(mygrid3[X, ]$byvars))  
  temp[, byvar := paste0(mygrid3[X, ]$byvars)]
  setnames(temp,  paste0(mygrid3[X, ]$byvars), "byvar_level")
  setcolorder(temp, c("variable", "level", "byvar", "byvar_level"))
  return(temp)
}

result3a <- rbindlist(
  lapply(X = as.list(seq(1, nrow(mygrid3))), result3.function), 
  use.names = T, fill = T
)

#-----run mam2yrs
result4.function <- function(X){
  temp <- calc(ph.data = wabrfs2,
               what = paste0(mygrid4[X, ]$myvars),
               year==2016 | year==2018 | year==2020,
               metrics = c("mean", "rse", "numerator", "denominator"),
               per = 100, time_var = "year", proportion = T,
               by=paste0(mygrid4[X, ]$byvars))  
  temp[, byvar := paste0(mygrid4[X, ]$byvars)]
  setnames(temp,  paste0(mygrid4[X, ]$byvars), "byvar_level")
  setcolorder(temp, c("variable", "level", "byvar", "byvar_level"))
  return(temp)
}

result4a <- rbindlist(
  lapply(X = as.list(seq(1, nrow(mygrid4))), result4.function), 
  use.names = T, fill = T
)

#-----run pap3yrs
result5.function <- function(X){
  temp <- calc(ph.data = wabrfs2,
               what = paste0(mygrid5[X, ]$myvars),
               year==2016 | year==2018 | year==2020,
               metrics = c("mean", "rse", "numerator", "denominator"),
               per = 100, time_var = "year", proportion = T,
               by=paste0(mygrid5[X, ]$byvars))  
  temp[, byvar := paste0(mygrid5[X, ]$byvars)]
  setnames(temp,  paste0(mygrid5[X, ]$byvars), "byvar_level")
  setcolorder(temp, c("variable", "level", "byvar", "byvar_level"))
  return(temp)
}

result5a <- rbindlist(
  lapply(X = as.list(seq(1, nrow(mygrid5))), result5.function), 
  use.names = T, fill = T
)

wares1 <- bind_rows(result1a, result2a, result3a, result4a, result5a)
wares1 <- wares1 %>% rename("result"="mean", "se"="mean_se", "lower_bound"="mean_lower", "upper_bound"="mean_upper")
wares2 <- wares1 %>% rename("cat1_group"="byvar_level")
wares2 <- wares2 %>% dplyr::select(-c("level"))
wares2$cat1 <- "Washington State"
wares2$cat1_group <- "Washington State"
wares2$cat1_varname <- "chi_geo_wa"
wares2$tab <- "_wastate"
wares2$cat2 <- "Washington State"
wares2$cat2_group <- "Washington State"
wares2$cat2_varname <- "chi_geo_wa"

wares2 <- wares2[, c("variable", "tab", "year", "cat1", "cat1_varname", "cat1_group", "cat2", "cat2_varname", "cat2_group",
                 "result", "se", "lower_bound", "upper_bound", "rse", "numerator", "denominator")]
wares2 <- wares2[order(wares2$variable, wares2$cat1, wares2$cat1_group), ]
write.xlsx(wares2, "brfss_wa.xlsx", sheetName = "Sheet1", overwrite = T)

#-----merging resultx, trend2, and wares2
resultx <- read.xlsx("resultx.xlsx")
trend2 <- read.xlsx("resultt.xlsx")
wares2 <- read.xlsx("brfss_wa.xlsx")

brfsall <- bind_rows(resultx, trend2, wares2)
res_all <- brfsall %>% rename("indicator_key"="variable")

res_all <- res_all %>% mutate(caution = case_when(rse >=30 ~as.character("!"), TRUE ~as.character("")))
res_all[which(res_all$denominator < 50), c("result", "lower_bound", "upper_bound")] <- NA

res_all <- res_all %>% mutate(comparison_with_kc = case_when(tab=='_kingcounty' ~"NA", TRUE ~comparison_with_kc))
res_all <- res_all %>% mutate(significance = case_when(tab=='_kingcounty' ~"NA", TRUE ~significance))
res_all <- res_all %>% mutate(suppression= case_when(denominator < 50 ~as.character("^"), TRUE ~as.character("")))

res_all$data_source <- "brfss"
res_all <- res_all %>% mutate(chi= case_when(cat1 == "Bigcities" ~0, TRUE ~1))
res_all$time_trends <- ""
res_all$source_date <- as.Date("2022-09-15")
res_all$run_date <- as.Date("2023-02-15")

res_all <- res_all %>% mutate(year = case_when(year=="2016, 2018, 2020" ~ "2016, 2018 & 2020", 
                                             year=="2017-2019, & 2021"  ~ "2017-2019 & 2021", 
                                             year=="2017-2019, 2021"    ~ "2017-2019 & 2021",
                                             year=="2017, 2019, 2021"   ~ "2017, 2019 & 2021", 
                                             year=="2017, 2020, & 2021" ~ "2017, 2020 & 2021",
                                             TRUE ~ year))



res_all <- res_all %>% mutate_at(vars(result, lower_bound, upper_bound, se), list(~ round(., 3)))
res_all$rse <- as.numeric(res_all$rse)
res_all$rse <- sprintf("%5.1f", round(res_all$rse, digits=1))
res_all <- res_all %>% mutate_at(vars(numerator, denominator, chi), list(~ round(., 0)))
numvars <- c("result", "lower_bound", "upper_bound", "se", "rse", "numerator", "denominator", "chi")
res_all[, numvars] <- lapply(res_all[, numvars], as.numeric)

res_all <- res_all[ , c("indicator_key", "tab", "year", "cat1", "cat1_varname", "cat1_group", 
                        "cat2", "cat2_varname", "cat2_group", "result", "lower_bound", "upper_bound",     
                        "se", "rse", "numerator", "denominator", "comparison_with_kc",
                        "significance", "caution",
                        "suppression", "data_source", "chi", "time_trends", "source_date", "run_date")]

write.xlsx(res_all, "brfss_all.xlsx", sheetName = "Sheet1", overwrite = T)

#-----DB connection, send updated data to temp server 51 
db51 <- odbc::dbConnect(odbc::odbc(),
                        Driver = "SQL Server",
                        Server = "KCITSQLUTPDBH51",
                        Database = "PHExtractStore")

dbWriteTable(db51, Id(schema = "APDE_WIP", table = "brfss_result2023"), res_all, overwrite = T)
DBI::dbDisconnect(db51)