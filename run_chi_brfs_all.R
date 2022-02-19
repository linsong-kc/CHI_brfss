#combine processed BRFSS datasets(demgroups and crosstabs, trends, wa)
rm(list=ls())
pacman::p_load(dplyr, foreign, survey, srvyr, epiDisplay, data.table, janitor, rads, labelled, dtsurvey, DBI)
setwd("c:/R_learning/CHI") 

chi_brfs1 <- read.csv("resultx.csv")
chi_brfs2 <- read.csv("resultt.csv")
chi_brfs3 <- read.csv("resultwa.csv")
chi_brfs4 <- read.csv("result_crc.csv")
res_all <-bind_rows(chi_brfs1, chi_brfs4, chi_brfs2, chi_brfs3)

res_all <- res_all %>% mutate(caution = case_when(rse >=30 ~as.character("*"), TRUE ~as.character("NA")))
res_all[c("result", "lower_bound", "upper_bound")][which(res_all$denominator<50), ] <-NA
res_all <- res_all %>% mutate(comparison_with_kc = case_when(tab=='_kingcounty' ~"NA", TRUE ~comparison_with_kc))
res_all <- res_all %>% mutate(significance = case_when(tab=='_kingcounty' ~"NA", TRUE ~significance))
res_all <- res_all %>% mutate(suppression= case_when(denominator < 50 ~as.character("^"), TRUE ~as.character("NA")))

res_all$cat1_group_alias <- res_all$cat1_group
res_all <- res_all %>% mutate(cat1_group_alias
            = case_when(cat1_group == "Beacon/Gtown/S.Park" ~"Beacon Hill/Georgetown/South Park", 
            cat1_group == "Bellevue-NE" ~"Bellevue-Northeast",
            cat1_group == "Black Diamond/Enumclaw/SE County" ~"Black Diamond/Enumclaw/Southeast County",
            cat1_group == "Capitol Hill/E.lake" ~"Capitol Hill/Eastlake",
            cat1_group == "Fed Way-Central/Military Rd" ~"Federal Way-Central/Military Rd",
            cat1_group == "Fed Way-Dash Pt" ~"Federal Way-Dash Point/Woodmont",
            cat1_group == "Kenmore/LFP" ~"Kenmore/Lake Forest Park",
            cat1_group == "Kent-SE" ~"Kent-Southeast",
            cat1_group == "Mercer Isle/Pt Cities" ~"Mercer Island/Point Cities",
            cat1_group == "NE Seattle" ~"Northeast Seattle",
            cat1_group == "NW Seattle" ~"Northwest Seattle",
            cat1_group == "QA/Magnolia" ~"Queen Anne/Magnolia",
            cat1_group == "SE Seattle" ~"Southeast Seattle",
            TRUE ~cat1_group))  

res_all <- res_all %>% mutate(cat1_varname = case_when(cat1 == "King Counyt" ~"kingco", 
                                  cat1 == "Sexual orientation" ~"sexorien",
                                  cat1 == "race3" ~"race3",
                                  cat1 == "race4" ~"race4",
                                  cat1 == "Race"  ~"race3",
                                  cat1 == "Regions" ~"region",
                                  cat1 == "Age" ~"age5",
                                  cat1 == "Cities/neighborhoods" ~"hracode",
                                  cat1 == "Gender" ~"sex",
                                  cat1 == "Household Income" ~"income6",
                                  cat1 == "Military Service" ~"veteram",
                                  cat1 == "Big cities" ~"hra2code", TRUE ~cat1))   

res_all <- res_all %>% mutate(cat1=recode(cat1, "Race"="Hispanic", "race3"="Race", "race4"="Race")) 

res_all$cat2_group_alias <- res_all$cat2_group
res_all <- res_all %>% mutate(cat2_group_alias
                      = case_when(cat2_group == "Beacon/Gtown/S.Park" ~"Beacon Hill/Georgetown/South Park", 
                        cat2_group == "Bellevue-NE" ~"Bellevue-Northeast",
                        cat2_group == "Black Diamond/Enumclaw/SE County" ~"Black Diamond/Enumclaw/Southeast County",
                        cat2_group == "Capitol Hill/E.lake" ~"Capitol Hill/Eastlake",
                        cat2_group == "Fed Way-Central/Military Rd" ~"Federal Way-Central/Military Rd",
                        cat2_group == "Fed Way-Dash Pt" ~"Federal Way-Dash Point/Woodmont",
                        cat2_group == "Kenmore/LFP" ~"Kenmore/Lake Forest Park",
                        cat2_group == "Kent-SE" ~"Kent-Southeast",
                        cat2_group == "Mercer Isle/Pt Cities" ~"Mercer Island/Point Cities",
                        cat2_group == "NE Seattle" ~"Northeast Seattle",
                        cat2_group == "NW Seattle" ~"Northwest Seattle",
                        cat2_group == "QA/Magnolia" ~"Queen Anne/Magnolia",
                        cat2_group == "SE Seattle" ~"Southeast Seattle",
                        TRUE ~cat2_group))

res_all <- res_all %>% mutate(cat2_varname
                                = case_when(cat2 == "King Counyt" ~"kingco", 
                                            cat2 == "Sexual orientation" ~"sexorien",
                                            cat2 == "race3" ~"race3",
                                            cat2 == "race4" ~"race4",
                                            cat2 == "Race"  ~"race3",
                                            cat2 == "Regions" ~"region",
                                            cat2 == "Age" ~"age5",
                                            cat2 == "Cities/neighborhoods" ~"hracode",
                                            cat2 == "Gender" ~"sex",
                                            cat2 == "Household Income" ~"income6",
                                            cat2 == "Military Service" ~"veteram",
                                            cat2 == "Big cities" ~"hra2code",
                                            TRUE ~cat2))   

res_all <- res_all %>% mutate(cat2= case_when(cat2=="Race" ~"Hispanic",    
                                              cat2 == "race3" ~"Race", 
                                              cat2 == "race4" ~"Race", TRUE ~cat2)) 
res_all$data_source <- "brfss"
res_all$chi <- 1
res_all$time_trends <- ""
res_all$source_date <- "2021-0915"
res_all$run_date <- "2022-0215"

res_all <- res_all[, c("data_source", "indicator_key", "tab", "year", 
                         "cat1", "cat1_group", "cat1_group_alias", "cat1_varname",
                         "cat2", "cat2_group", "cat2_group_alias", "cat2_varname",
                         "result", "lower_bound", "upper_bound", "se", "rse",
                         "comparison_with_kc", "time_trends", "significance", 
                         "caution", "suppression", "numerator", "denominator",
                         "chi", "source_date", "run_date")]
res_all <-  res_all[order(res_all$indicator_key, res_all$tab, res_all$cat1), ]
write.csv(res_all, "chi_brfs.csv", row.names = F)

#DB connection
db51 <- odbc::dbConnect(odbc::odbc(),
                        Driver = "SQL Server",
                        Server = "KCITSQLUTPDBH51",
                        Database = "PHExtractStore")

dbWriteTable(db51, Id(schema = "APDE_WIP", table = "brfss_results_new"), res_all, overwrite = T)
DBI::dbDisconnect(db51)

