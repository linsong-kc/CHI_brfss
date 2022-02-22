#generate BRFSS data for City Health Profile, no custom data as in CHI
rm(list=ls())
pacman::p_load(dplyr, foreign, survey, srvyr, epiDisplay, data.table, rads, labelled, dtsurvey)
setwd("c:/R_learning/CHI_brfss") 

#Read Stata file, can only translate Stata version 12 or earlier, other packages can read later versions
brfs1wa <- read.dta(file="s:/work/surveys/brfs/data/final/wavar0020_v12.dta", warn.missing.labels = FALSE)
brfs2wa <- subset(brfs1wa, year>=2016)
brfs2wa <- brfs2wa %>% dplyr::select(order(colnames(brfs2wa)))
names(brfs2wa)<-gsub("^_", "x_", names(brfs2wa)) #change variable names start with "_"
brfs2wa$age <- as.numeric(brfs2wa$age)
colnames(brfs2wa)[grepl("disa",colnames(brfs2wa))]
brfs2wa <- brfs2wa %>% rename("county"="ctycode", "mrace"="x_mrace", "mracex"="race2", "denvst1"="x_denvst2")
brfs3wa <- brfs2wa[ , c("year", "x_ststr", "x_llcpwt", "county",
                     "x_crcrec", "denvst1", "firearm4", "fnotlast", "diab2", "disab2",
                     "medcost", "menthlth", "obese", "smoker1")]

#recode the following integer variables into character variables
brfs3wa <- brfs3wa %>% mutate_at(vars("fnotlast","disab2","denvst1","firearm4","diab2","medcost","obese","smoker1"), 
                                 list(~recode(., '0'='No', '1'="Yes", .default ="NA")))
brfs3wa <- brfs3wa %>% mutate(x_crcrec = case_when(x_crcrec=='Yes' ~'Yes',
                                             x_crcrec=='No' ~'No', TRUE ~"NA"))
brfs3wa <- brfs3wa %>% mutate(FMD = case_when(menthlth<=13 ~'No', menthlth<=30 ~ 'Yes'))
brfs$denvst1 <- as.character(brfs$denvst1)
brfs <- brfs %>% mutate(denvst1 = case_when(denvst1=='Yes' ~'No', denvst1=='No' ~ 'Yes', TRUE ~denvst1))
brfs$x_crcrec <- as.character(brfs$x_crcrec)
brfs <- brfs %>% mutate(x_crcrec = case_when(x_crcrec=='Yes' ~'No', x_crcrec=='No' ~ 'Yes', TRUE ~x_crcrec))
brfs3wa$all <-  "WA"

brfs3wa <- brfs3wa[complete.cases(brfs3wa$x_llcpwt), ]  #drop cases with missing weight
brfs3wa[brfs3wa == "NA"] <- NA
myvars <- c("diab2", "disab2","medcost", "FMD", "obese", "smoker1", 
            "x_crcrec", "denvst1", "firearm4", "fnotlast")
brfs3wa[, myvars] <- lapply(brfs3wa[, myvars], as.factor)

#-----set survey design weight, myvars are 2016-2020 variables for all adults-----
brfs3wa <- brfs3wa[complete.cases(brfs3wa$x_llcpwt), ]     #drop cases with missing weight
brfs3wa$x_llcpwt <- as.numeric(brfs3wa$x_llcpwt)
options(survey.lonely.psu = "adjust")
brfswa <- dtsurvey(DT=brfs3wa, psu=NULL, weight= "x_llcpwt", strata="x_ststr")

result_wa <- calc(ph.data = brfswa,
                what = myvars,
                year >=2016,
                by = "all",
                metrics=c("mean","rse","numerator","denominator"),
                per=100,
                time_var="year",
                proportion=T)

res_wa <- subset(result_wa, level=="Yes")
res_wa$tab <- "_wastate"
res_wa$cat1 <- "Washington State"
res_wa$cat1_group <- "Washington State"
res_wa$cat1_group_alias <- "Washington State"
res_wa$cat1_varname <- "wastate"

res_wa <- res_wa %>% mutate(year=case_when(
                            variable=="x_crcrec" | variable=="denvst1" | variable=="firearm4" ~"2016, 2018 & 2020", 
                            variable=="fnotlast" ~"2018, 2019 & 2020",
                            variable=="SSB" ~"2018-2019", TRUE ~year))

res_wa <- res_wa %>% rename("indicator_key"="variable", "result"="mean", 
                              "se"="mean_se", "lower_bound"="mean_lower", "upper_bound"="mean_upper")
res_wa <- res_wa[, c("tab", "year", "indicator_key", "cat1","cat1_group", "cat1_group_alias", "cat1_varname",
                        "result", "lower_bound", "upper_bound", "se", "rse", "numerator", "denominator")]
write.csv(res_wa, "resultwa.csv", row.names = F)
