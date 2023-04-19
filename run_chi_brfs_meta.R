##generate BRFSS meta data for CHI
rm(list=ls())
pacman::p_load(dplyr, foreign, survey, srvyr, epiDisplay, data.table, janitor, rads, labelled, dtsurvey, openxlsx, DBI)
setwd("C:/pers_linsong/chi_brfss")

kc1221 <- readRDS("//dphcifs/APDE-CDIP/BRFSS/prog_all/kc1221final.rds")
d1 <- as.data.table(kc1221)
d2 <- d1[, .(year, x_ststr, finalwt1, hra20_name, bigcities, region, income6,  
             age, age7, sex, sexorien, trnsgndr, race3, race4, hispanic, veteran3, 
             asthnow, bphigh, cholchk5, x_crcrec, cvdheart, cvdstrk3, x_denvst3, diab2,  
             disab2, ecignow1, firearm4, flushot7, fnotlast, genhlth2, mam2yrs, medcost1,
             fmd, mjnow, obese, x_bmi5cat, x_veglt1a, pap3yrs, persdoc3, x_pneumo3, smoker1, SSB)]

#-----recode demographic variable
d2 <- setnames(d2, c("x_denvst3", "cvdstrk3", "disab2", "medcost1", "mjnow", "persdoc3", "x_pneumo3", "SSB", "hispanic", "income6"), 
               c("denvst1", "cvdstrok", "disabil", "medcost", "mjpast30", "persdoc", "pneumvac", "ssb", "hisp", "income6b"))

d2[, age7:=as.integer(age7)] [, race3:=as.character(race3)] [, race4:=as.character(race4)] [, income6b:=as.character(income6b)] 
d2[age7==1, age5_v2 :="18-24"] [age7 %in% c(2,3), age5_v2 :="25-44"] [age7 %in% c(4, 5), age5_v2 :="45-64"] [age7==6, age5_v2 :="65-74"] [age7==7, age5_v2 :="75+"]
d2[sex==1, chi_sex :="Male"] [sex==2, chi_sex :="Female"]
d2[sexorien==1, chi_sexorien_2 :="Heterosexual"] [sexorien %in% c(2, 3), chi_sexorien_2 :="LGB"][sexorien==4, chi_sexorien_2:=NA_character_]
d2[veteran3==0, veteran :="Non-Veteran"] [veteran3==1, veteran :="Veteran"]
d2[hisp==0, hispanic :="Non-Hispanic"] [hisp==1, hispanic :="Hispanic"]
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
d2 <- d2[, !c("age7", "sexorien", "hisp", "veteran3"), with=F]

#--------------------------------
dt1 <- d2[complete.cases(d2$finalwt1), ]  #drop cases with missing weight
dt2 <- subset(dt1, chi_sex=="Female")

#indicator lists ("within 2017-2021" variables, "2016, 2018, 2020" variables, and variables for females
myvar1 <- c("asthnow", "cvdheart", "cvdstrok", "diab2", "disabil", "flushot_v1", "flushot_v2",
            "genhlth2", "medcost", "fmd", "obese", "overw1", "persdoc", "pneumvac", "smoker1",
            "bphigh", "cholchk5", "ecignow1", "fnotlast", "mjpast30", "x_veglt1a")
myvar2 <- c("denvst1", "firearm4")
myvar3 <- c("x_crcrec")
myvar4 <- c("mam2yrs")
myvar5 <- c("pap3yrs")
myvar6 <- c("ssb")

byvar1 <- c("all")

mygrid1 <- data.frame(expand.grid(myvars = myvar1, byvars = byvar1))
mygrid2 <- data.frame(expand.grid(myvars = myvar2, byvars = byvar1))
mygrid3 <- data.frame(expand.grid(myvars = myvar3, byvars = byvar1))
mygrid4 <- data.frame(expand.grid(myvars = myvar4, byvars = byvar1))
mygrid5 <- data.frame(expand.grid(myvars = myvar5, byvars = byvar1))
mygrid6 <- data.frame(expand.grid(myvars = myvar6, byvars = byvar1))

options(survey.lonely.psu = "adjust")
brfs1 <- dtsurvey(DT=dt1, psu=NULL, weight= "finalwt1", strata="x_ststr")
brfs2 <- dtsurvey(DT=dt2, psu=NULL, weight= "finalwt1", strata="x_ststr")

#-----run 2021 data
result1.function <- function(X){
  temp <- calc(ph.data = brfs1,
               what = paste0(mygrid1[X, ]$myvars),
               year ==2021,
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

#-----run 2020 data
result2.function <- function(X){
  temp <- calc(ph.data = brfs1,
               what = paste0(mygrid2[X, ]$myvars),
               year==2020,
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
               year==2020,
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
               year==2020,
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
               year==2020,
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

#-----run ssb
result6.function <- function(X){
  temp <- calc(ph.data = brfs1,
               what = paste0(mygrid6[X, ]$myvars),
               year==2019,
               metrics = c("mean", "rse", "numerator", "denominator"),
               per = 100, time_var = "year", proportion = T,
               by=paste0(mygrid6[X, ]$byvars))  
  temp[, byvar := paste0(mygrid6[X, ]$byvars)]
  setnames(temp,  paste0(mygrid6[X, ]$byvars), "byvar_level")
  setcolorder(temp, c("variable", "level", "byvar", "byvar_level"))
  return(temp)
}

result6a <- rbindlist(
  lapply(X = as.list(seq(1, nrow(mygrid6))), result6.function), 
  use.names = T, fill = T
)

res1 <- bind_rows(result1a, result2a, result3a, result4a, result5a, result6a)
res_meta <- subset(res1, select = c("year", "variable", "mean")) 
res_meta <- res_meta %>% rename("indicator_key" = "variable","latest_year" = "year","latest_year_result"="mean")
res_meta$data_source <- "brfss"
res_meta$result_type <- "proportion"
res_meta$valence <- "negative"

kcpop2021 <- get_population(years=2021,ages=c(18:99), geo_type=c("kc"),round=T) 
kcpop2020 <- get_population(years=2020,ages=c(18:99), geo_type=c("kc"),round=T) 
kcpop2019 <- get_population(years=2019,ages=c(18:99), geo_type=c("kc"),round=T) 

res_meta$latest_year_kc_pop <- as.numeric(kcpop2021$pop)
res_meta <- res_meta %>% mutate(latest_year_kc_pop= case_when(latest_year=="2020" ~kcpop2020$pop, 
                                                              latest_year=="2019" ~kcpop2019$pop, 
                                                              TRUE ~latest_year_kc_pop))

res_meta$latest_year_count <- round(res_meta$latest_year_kc_pop * res_meta$latest_year_result)
res_meta$map_type <- "hra"
res_meta$unit <- "adults"
res_meta$valid_years <- as.character("2012-2021")

res_meta <- res_meta %>% mutate(valid_years = case_when(indicator_key=="x_crcrec" ~"2014 2015 2016 2018 2020", 
                                              indicator_key=="denvst1"  ~"2012 2014 2015 2016 2018 2020",
                                              indicator_key=="firearm4" ~"2012 2013 2015 2016 2018 2020",
                                              indicator_key=="fnotlast" ~"2012 2013 2018 2019 2020 2021",
                                              indicator_key=="ecignow1" ~"2016 2017 2020 2021",
                                              indicator_key=="mjpast30"    ~"2012-2019 2021",
                                              indicator_key=="ssb" ~"2018 2019", TRUE ~valid_years))

res_meta$chi <- 1
res_meta$run_date <- as.character("2023-02-15")

res_meta$latest_year_result <- sprintf("%.3f", round(res_meta$latest_year_result, digits=3))
res_meta$latest_year  <- as.integer(res_meta$latest_year)
res_meta$latest_year_result  <- as.character(res_meta$latest_year_result)
res_meta$chi  <- as.character(res_meta$chi)

res_meta$latest_year_kc_pop <- as.character(gsub(",", "", res_meta$latest_year_kc_pop))
res_meta$latest_year_count  <- as.character(gsub(",", "", res_meta$latest_year_count))
res_meta$chi  <- as.integer(res_meta$chi)

res_meta <- res_meta[, c("data_source", "indicator_key", "result_type", "valence",
                         "latest_year", "latest_year_result", "latest_year_kc_pop", "latest_year_count",
                         "map_type", "unit", "valid_years", "chi", "run_date")]
write.csv(res_meta, "chi_brfs_meta.csv", row.names = F)
write.xlsx(res_meta, "brfss_meta.xlsx", sheetName = "Sheet1", overwrite = T)

#DB connection, send updated data to temp server 51 
db51 <- odbc::dbConnect(odbc::odbc(),
                        Driver = "SQL Server",
                        Server = "KCITSQLUTPDBH51",
                        Database = "PHExtractStore")

dbWriteTable(db51, Id(schema = "APDE_WIP", table = "brfss_meta2023"), res_meta, overwrite = T)
DBI::dbDisconnect(db51)

#------------------------
# pull prior data from temp sql server 51 and change to oldmeta----
#DB connection
db51 <- odbc::dbConnect(odbc::odbc(),
                        Driver = "SQL Server",
                        Server = "KCITSQLUTPDBH51",
                        Database = "PHExtractStore")
oldmeta <- setDT(DBI::dbGetQuery(conn = db51, statement = 
                                   "SELECT * FROM [PHExtractStore].[APDE_WIP].[brfss_metadata]"))

dbWriteTable(db51, Id(schema = "APDE_WIP", table = "brfss_metadata_oldz"), oldmeta, overwrite = T)
DBI::dbDisconnect(db51)
