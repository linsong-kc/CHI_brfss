#generate BRFSS trend data
rm(list=ls())
pacman::p_load(dplyr, foreign, survey, srvyr, epiDisplay, data.table, janitor, rads, labelled, dtsurvey)
setwd("c:/R_learning/CHI") 

#Read Stata file, can only translate Stata version 12 or earlier, other packages can read later versions
brfsraw <- read.dta(file="S:/WORK/surveys/brfs/prog_all/kc0020_finalz.dta", warn.missing.labels = FALSE)
brfsraw <- subset(brfsraw, year>=2009) #for most recent 10 3-year rolling averages

names(brfsraw)<-gsub("^_", "x_", names(brfsraw)) #change variable names start with "_"
brfs <- brfsraw[ , c("year", "x_ststr", "kcwt_llcp", "ccreg", "sexorien", "mrace", "mracex", "hispanic", 
                     "x_crcrec", "denvst1", "firearm4", "fnotlast", "diab2", "disab2",
                     "medcost", "menthlth", "obese", "smoker1")]

#recode the following integer variables into character variables
brfs <- brfs %>% mutate_at(vars("x_crcrec", "disab2"), list(~recode(., '0'='No', '1'="Yes", .default ="NA")))
brfs <- brfs %>% mutate(fnotlast = case_when(fnotlast=='Often true' | fnotlast=='Sometimes true' ~'Yes',
        fnotlast=='Never true' ~ 'No', fnotlast=="Don't know/Not sure" | fnotlast=='Refused' ~'NA'))
brfs <- brfs %>% mutate(menthlth = case_when(menthlth<=13 ~'No', menthlth<=30 ~ 'Yes'))
brfs$denvst1 <- as.character(brfs$denvst1)
brfs <- brfs %>% mutate(denvst1 = case_when(denvst1=='Yes' ~'No', denvst1=='No' ~ 'Yes', TRUE ~denvst1))
brfs$x_crcrec <- as.character(brfs$x_crcrec)
brfs <- brfs %>% mutate(x_crcrec = case_when(x_crcrec=='Yes' ~'No', x_crcrec=='No' ~ 'Yes', TRUE ~x_crcrec))

#recode demographic variable
brfs <- brfs %>% mutate(sexorien = case_when(sexorien=="A. Heterosexual, that is, straight" ~"Heterosexual", 
                                          sexorien=="B. Homosexual, that is gay or lesbian" | 
                                            sexorien=="C. Bisexual" ~"LGB", TRUE ~ 'NA'))
brfs$mrace <- as.character(brfs$mrace)
brfs$mracex <- as.character(brfs$mracex)
brfs <- brfs %>% mutate(mrace = case_when(mrace=="Other" | mrace=="Unknown" ~"Other/Unknown", TRUE ~ mrace))
brfs <- brfs %>% mutate(mracex = case_when(mracex=="Other" | mracex=="Unknown" ~"Other/Unknown", TRUE ~ mracex))

brfs <- brfs %>% mutate(veteran = case_when(veteran=="Yes" ~ "Veteran",
                                            veteran=="No" ~ "Non-Veteran", TRUE ~ "NA"))

brfs <- brfs %>% rename("FMD"="menthlth", "LGB"="sexorien", "race3"="mrace", "race4"="mracex")
brfs$all <-  "Total"

#--------------------------------
brfs <- brfs[complete.cases(brfs$kcwt_llcp), ]  #drop cases with missing weight
brfs[brfs == "NA"] <- NA
myvars <- c("diab2", "disab2","medcost", "FMD", "obese", "smoker1",
               "x_crcrec", "denvst1", "firearm4", "fnotlast")
brfs[, myvars] <- lapply(brfs[, myvars], as.factor)

byvars <- c("all", "race3", "hispanic", "race4", "LGB", "ccreg")
brfs[, byvars] <- lapply(brfs[, byvars], as.character)

options(survey.lonely.psu = "adjust")
brfskc <- dtsurvey(DT=brfs, psu=NULL, weight= "kcwt_llcp", strata="x_ststr")
mygrid <- data.frame(expand.grid(myvars = myvars, byvars = byvars))

result.function <- function(X){
  temp <- calc(ph.data = brfskc,
               what = paste0(mygrid[X, ]$myvars),
               year >=2009,
               metrics = c("mean", "rse", "numerator", "denominator"),
               per = 100,
               win = 3,
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
result2 <- result2[!(year=="2013" & variable=="disab2"),]
result2 <- result2[!(year=="2013-2014" & variable=="disab2"),]

result2 <- result2 %>% mutate(cat1 = case_when(cat1=="all" ~ "King County",
                              cat1=="LGB" ~"Sexual orientation",
                              cat1=="hispanic" ~"Race", cat1=="ccreg" ~"Regions", TRUE ~ cat1))
result2 <- result2 %>% mutate(cat1_group = case_when(cat1=="King County" ~ "King County", TRUE ~ cat1_group))

result3 <- result2[, .SD[1], by = .(variable, cat1, cat1_group, year)]
result3$tab <- "trends"

#-----King County average for significance comparison-----
res_kc <- subset(result3, subset=result3$cat1=="King County", 
                 select= c("variable", "year",  "mean", "mean_lower", "mean_upper" ))

res_kc <- res_kc %>% rename("kc_result" = "mean", "kc_lower" = "mean_lower", "kc_upper"="mean_upper")

result4 <- merge(x=result3, y=res_kc, by=c("variable", "year"))

result4 <- result4 %>% mutate(comparison_with_kc = case_when(mean_upper < kc_lower ~as.character("lower"), 
                                                             mean_lower > kc_upper ~as.character("higher"), 
                                                             TRUE ~as.character("not different")))

result4 <- result4 %>% mutate(significance = case_when(mean_upper < kc_lower ~as.character("*"), 
                                                       mean_lower > kc_upper ~as.character("*"), 
                                                       TRUE ~as.character("")))

result4 <- result4 %>% rename("indicator_key"="variable", "result"="mean", 
                              "se"="mean_se", "lower_bound"="mean_lower", "upper_bound"="mean_upper")

result4 <- result4[, c("tab", "year", "indicator_key", "cat1","cat1_group",
                       "result", "lower_bound", "upper_bound", "se", "rse", "numerator", "denominator",
                       "comparison_with_kc", "significance")]

result4 <- result4[order(indicator_key, cat1, cat1_group, year), ]
write.csv(result4, "resultt.csv", row.names = F)
