#Revised data compilation using 2024 needs assessment

#load libraries
library(tidyverse)
library(plyr)

# Create sample and governance variables ----------------------------------------------------

###Load data and initial manipulations
CWSgov <- read.csv(here::here("Data_raw/ACTIVE VERSION_ CWS institution types Sept 28 2023.csv"), header=T,
                   na.strings=c("","NA"))
CWSgov$Primacy_FINAL <- as.factor(CWSgov$Primacy_FINAL)
CWSgov <- CWSgov %>% filter(Primacy_FINAL != "EPA Region 9") #get rid of EPA regulated Tribal water systems
CWSgov <- CWSgov %>% filter(Final_inst_update != "Tribal Government") #Get rid of three remaining systems that are Tribal but state regulated
CWSgov$Primacy_FINAL <- droplevels(CWSgov$Primacy_FINAL)
CWSgov <- CWSgov[,c(1:6)] #get rid of columns I don't need
CWSgov$PWSID <- as.factor(CWSgov$PWSID)
CWSgov$Counties_Served <- as.factor(CWSgov$Counties_Served)
CWSgov$Final_inst_update <- as.factor(CWSgov$Final_inst_update)
CWSgov$Inst_Subtype  <- as.factor(CWSgov$Inst_Subtype)

### create new variables for ancillary/non ancillary, public/private and customer enfranchisement

## To focus on residential systems where residents are served and could have a role in governance/decision-making, identify which systems are "residential" this is mostly synonymous to non-ancillary except with this variable maintaining Mobile Home Parks which are generally considered to be ancillary and ye tin other ways these systems are more similar to non-ancillary systems in that they serve a stable residential base. Moreover, California has seen significant organizing among Mobile Home Park communities in recent years around drinking water issues. 
CWSgov$Residential <- ifelse(CWSgov$Final_inst_update == "Community Services District" |
                               CWSgov$Final_inst_update == "County Service Area" | 
                               CWSgov$Final_inst_update == "Municipal Water District" | 
                               CWSgov$Final_inst_update == "City" | 
                               CWSgov$Final_inst_update == "County Sanitation District"| 
                               CWSgov$Final_inst_update == "County Water District"| 
                               CWSgov$Final_inst_update == "County Waterworks District" |
                               CWSgov$Final_inst_update == "Irrigation District"| 
                               CWSgov$Final_inst_update == "Maintenance District"|
                               CWSgov$Final_inst_update == "Municipal Utility District"|
                               CWSgov$Final_inst_update == "Public Utility District"| 
                               CWSgov$Final_inst_update == "Resort Improvement District"|
                               CWSgov$Final_inst_update == "Resource Conservation District"|
                               CWSgov$Final_inst_update == "Sanitary District"| 
                               CWSgov$Final_inst_update == "Water Conservation District"|
                               CWSgov$Final_inst_update == "Special Act District" |
                               CWSgov$Final_inst_update == "Mutual Benefit" | 
                               CWSgov$Final_inst_update == "Joint Powers Authority/Agreement" |
                               CWSgov$Final_inst_update == "Investor Owned Utility" | 
                               CWSgov$Final_inst_update == "California Water District" |
                               CWSgov$Final_inst_update == "Mobile Home Park","Yes", NA)

CWSgov$Residential <- ifelse(CWSgov$Final_inst_update == "County" | 
                               CWSgov$Final_inst_update == "Federal" | 
                               CWSgov$Final_inst_update == "Private - Ancillary" | 
                               CWSgov$Final_inst_update == "School District" | 
                               CWSgov$Final_inst_update == "State", "No", CWSgov$Residential)

#make factor
CWSgov$Residential <- as.factor(CWSgov$Residential)

## create public/private ownership variable
CWSgov$public <- ifelse(CWSgov$Final_inst_update == "Community Services District" |
                          CWSgov$Final_inst_update == "County Service Area" |
                          CWSgov$Final_inst_update == "Municipal Water District" |
                          CWSgov$Final_inst_update == "City" | 
                          CWSgov$Final_inst_update == "County Sanitation District"| 
                          CWSgov$Final_inst_update == "County Water District"|
                          CWSgov$Final_inst_update == "County Waterworks District" |
                          CWSgov$Final_inst_update == "Irrigation District"| 
                          CWSgov$Final_inst_update == "Maintenance District"| 
                          CWSgov$Final_inst_update == "Municipal Utility District"| 
                          CWSgov$Final_inst_update == "Public Utility District"| 
                          CWSgov$Final_inst_update == "Resort Improvement District"|
                          CWSgov$Final_inst_update == "Resource Conservation District"| 
                          CWSgov$Final_inst_update == "Sanitary District"| 
                          CWSgov$Final_inst_update == "Water Conservation District"| 
                          CWSgov$Final_inst_update == "Special Act District" | 
                          CWSgov$Final_inst_update == "Joint Powers Authority/Agreement" |
                          CWSgov$Final_inst_update == "California Water District" |
                          CWSgov$Final_inst_update == "County" | 
                          CWSgov$Final_inst_update == "Federal" | 
                          CWSgov$Final_inst_update == "School District" | 
                          CWSgov$Final_inst_update == "State" | 
                          CWSgov$Final_inst_update == "Tribal", "Yes", "No")

#make factor
CWSgov$public <- as.factor(CWSgov$public)

#create enfranchisement variable
CWSgov$enfranchisement<- ifelse(CWSgov$Final_inst_update == "Investor Owned Utility" | 
                                  CWSgov$Final_inst_update == "Mobile Home Park", "None", NA)

CWSgov$enfranchisement <- ifelse(CWSgov$Final_inst_update == "Mutual Benefit",
                                 "Limited", CWSgov$enfranchisement)

CWSgov$enfranchisement <- ifelse(CWSgov$Final_inst_update == "California Water District" |
                                   CWSgov$Final_inst_update == "Joint Powers Authority/Agreement" |
                                   CWSgov$Final_inst_update == "Special Act District" | 
                                   CWSgov$Final_inst_update == "Irrigation District", 
                                 "Variable", CWSgov$enfranchisement)

CWSgov$enfranchisement <- ifelse(CWSgov$Final_inst_update == "Community Services District" |
                                   CWSgov$Final_inst_update == "County Service Area" |
                                   CWSgov$Final_inst_update == "Municipal Water District" |
                                   CWSgov$Final_inst_update == "City" | 
                                   CWSgov$Final_inst_update == "County Sanitation District"|
                                   CWSgov$Final_inst_update == "County Water District"|
                                   CWSgov$Final_inst_update == "County Waterworks District" |
                                   CWSgov$Final_inst_update == "Maintenance District"| 
                                   CWSgov$Final_inst_update == "Municipal Utility District"|
                                   CWSgov$Final_inst_update == "Public Utility District"|
                                   CWSgov$Final_inst_update == "Resort Improvement District"|
                                   CWSgov$Final_inst_update == "Resource Conservation District"|
                                   CWSgov$Final_inst_update == "Sanitary District"|
                                   CWSgov$Final_inst_update == "Water Conservation District", 
                                 "Full", CWSgov$enfranchisement)

#make factor
CWSgov$enfranchisement <- as.factor(CWSgov$enfranchisement)

Variable_districts <- read.csv(here::here("Data_raw/Variablevotingdistricts_JMcoded.csv"))
Variable_districts <- Variable_districts[,c(1,7)] 
Variable_districts$PWSID <- as.factor(Variable_districts$PWSID)
Variable_districts$Vote_structure <- as.factor(Variable_districts$Vote_structure)
CWSgov <- left_join(CWSgov, Variable_districts)
CWSgov <- CWSgov %>% dplyr::rename("Variablecoded" = "Vote_structure")

CWSgov$enfranchisement <- as.character(CWSgov$enfranchisement)
CWSgov$Variablecoded <- as.character(CWSgov$Variablecoded)
CWSgov <- CWSgov %>% mutate(enfranchisement_final = case_when(enfranchisement == "Variable" ~ Variablecoded,
                                                              .default = enfranchisement))
CWSgov$enfranchisement_final <- as.factor(CWSgov$enfranchisement_final)

#Remove any systems that have changed institutional type since 2018
Changes <- read.csv(here::here("Data_raw/Inst_changed_2018_2023.csv"),
                    header=T,
                    na.strings=c("","NA"))
Changes_small <- Changes[,c(1,4)] # reduce to only what is needed

CWSgov <- left_join(CWSgov, Changes_small)
CWSgov$Changed_enfranchisement. <- as.factor(CWSgov$Changed_enfranchisement.)
CWSgov <- filter(CWSgov, Changed_enfranchisement. == "No" | is.na(Changed_enfranchisement.)) # removes 20 systems with institutional changes that correspond to enfranchisement changes since 2018

# Add in outcome data and control variables -----------------------------------------------------

## Arrearage data
Arrearage <- read.csv(here::here("Data_raw/public-arrearage-program-data-2022-01-19.csv")) #data comes from SWRCB (https://www.waterboards.ca.gov/arrearage_payment_program/, original website is now accessible as an archive at the bottom of the page). This 1-19-22 version has remained the most updated on their website for many years before being replaced by the extended arrearage program data. The board confirmed that the deadline was December 2021 btu they took late applications through Jan 2022 but all funds had to be distributed by Jan 31 2022 so this should be the final.  There is data available about who applied for the extended program but its not in csv/excel format and I believe was many of the same systems as they extended the timeframe for which arrearages could be reimbursed. 
Arrearage <- dplyr::rename(Arrearage, PWSID = Water.System.ID....PWSID.)
Arrearage <- Arrearage[!duplicated(Arrearage[,c("PWSID")]),] #get rid of duplicated row
Arrearage$PWSID <- as.factor(Arrearage$PWSID)
Arrearage <- Arrearage %>% dplyr::select(PWSID, Intend.to.apply., Application.complete.)
Arrearage$Intend.to.apply. <- as.factor(Arrearage$Intend.to.apply.)
Arrearage$Application.complete. <- as.factor(Arrearage$Application.complete.)

Arrearage <- distinct(Arrearage, .keep_all = )

## SAFER
SAFER2024 <- read.csv(here::here("Data_raw/SaferRiskAssessmentTable2024.csv"))#SAFER data comes from https://data.ca.gov/dataset/safer-failing-and-at-risk-drinking-water-systems. Dictionary saved in data_raw folder as well. Is from 2024 report. 
SAFER2024 <- dplyr::rename(SAFER2024, PWSID = WATER_SYSTEM_NUMBER)
SAFER2024 <- SAFER2024 %>% dplyr::select(PWSID, SERVICE_CONNECTIONS, POPULATION, MHI,
                                         CALENVIRO_SCREEN_SCORE, FINAL_SAFER_STATUS,
                                         HOUSEHOLD_SOCIOECONOMIC_BURDEN_RAW_SCORE,
                                         FUNDING_RECEIVED_SINCE_2017, PRIMARY_MCL_VIOLATION,
                                         SECONDARY_MCL_VIOLATION,E_COLI_VIOLATION, 
                                         TREATMENT_TECHNIQUE_VIOLATION, MONITORING_AND_REPORTING_VIOLATION,
                                         HISTORY_OF_E_COLI_PRESENCE_RISK_LEVEL,
                                         TREATMENT_TECHNIUQE_VIOLATIONS_RISK_LEVEL,
                                         PERCENTAGE_OF_SOURCES_EXCEEDING_AN_MCL_RISK_LEVEL,
                                         CONSTITUENTS_OF_EMERGING_CONCERN_RISK_LEVEL,
                                         NUMBER_OF_WATER_SOURCES_RISK_LEVEL,
                                         ABESENCE_OF_INTERTIES_RISK_LEVEL,
                                         BOTTLED_WATER_OR_HAULED_WATER_RELIANCE_RISK_LEVEL,
                                         SOURCE_CAPACITY_VIOLATION_RISK_LEVEL,
                                         PERCENT_OF_MEDIAN_HOUSEHOLD_INCOME_MHI_RISK_LEVEL, 
                                         EXTREME_WATER_BILL_RISK_LEVEL,
                                         OPERATOR_CERTIFICATION_VIOLATIONS_RISK_LEVEL,
                                         MONITORING_AND_REPORTING_VIOLATIONS_RISK_LEVEL,
                                         DAYS_CASH_ON_HAND_RISK_LEVEL,
                                         OPERATING_RATIO_RISK_LEVEL)

#Change Not assessed to NAs
library(naniar)
na_string <- "Not Assessed"
SAFER2024 <- SAFER2024 %>% replace_with_na_all(condition = ~.x %in% na_string)

#Change data types
SAFER2024[,c(9:27)] <- lapply(SAFER2024[,c(9:27)], factor)
SAFER2024$PWSID <- as.factor(SAFER2024$PWSID)
SAFER2024$MHI <- as.numeric(SAFER2024$MHI)
SAFER2024$FINAL_SAFER_STATUS <- as.factor(SAFER2024$FINAL_SAFER_STATUS)
SAFER2024$CALENVIRO_SCREEN_SCORE <- as.numeric(SAFER2024$CALENVIRO_SCREEN_SCORE)
SAFER2024$HOUSEHOLD_SOCIOECONOMIC_BURDEN_RAW_SCORE <- as.numeric(SAFER2024$HOUSEHOLD_SOCIOECONOMIC_BURDEN_RAW_SCORE)

## join all together and final type corrections
Data <- left_join(CWSgov, SAFER2024)
Data <- left_join(Data, Arrearage)

#add water source and whether purchased from CA SDWIS download July 23 2024
SDWIS <- read.csv(here::here("Data_raw/CA_SDWIS_alldownload_July232024.csv"))
SDWIS <- dplyr::rename(SDWIS, PWSID = Water.System.No.)
SDWIS$PWSID <- as.factor(SDWIS$PWSID)
SDWIS$Type <- as.factor(SDWIS$Type)
SDWIS$Primary.Source.Water.Type <- as.factor(SDWIS$Primary.Source.Water.Type)
SDWIS <- SDWIS %>% dplyr::select(PWSID, Type, Primary.Source.Water.Type)
SDWIS$Source <- as.factor(ifelse(SDWIS$Primary.Source.Water.Type == "GU" |
                                   SDWIS$Primary.Source.Water.Type == "GUP" | 
                                   SDWIS$Primary.Source.Water.Type == "GW" | 
                                   SDWIS$Primary.Source.Water.Type == "GWP", "GW", "SW"))
SDWIS$Purchased <- as.factor(ifelse(SDWIS$Primary.Source.Water.Type == "GUP" |
                                      SDWIS$Primary.Source.Water.Type == "SWP" | 
                                      SDWIS$Primary.Source.Water.Type == "GWP", "Purchased", "Self-produced"))

Data <- left_join(Data, SDWIS)

Data$LN_POP <- log((Data$POPULATION+1)) #added one to population first to avoid issue of logging 0. 


# Data clean up and formatting --------------------------------------------


#adjust the indicators that have three levels so there is only two (no or minimal risk versus high/medium risk)
Data <- Data %>%
  mutate(CONSTITUENTS_OF_EMERGING_CONCERN_RISK_LEVEL_BI = as.factor(ifelse(CONSTITUENTS_OF_EMERGING_CONCERN_RISK_LEVEL ==
                                                                             "HIGH", "HIGH", "NONE")),
         NUMBER_OF_WATER_SOURCES_RISK_LEVEL_BI = as.factor(ifelse(NUMBER_OF_WATER_SOURCES_RISK_LEVEL == "NONE", "NONE", 
                                                                  "HIGH")),
         BOTTLED_WATER_OR_HAULED_WATER_RELIANCE_RISK_LEVEL = recode(BOTTLED_WATER_OR_HAULED_WATER_RELIANCE_RISK_LEVEL,
                                                                    "VERY HIGH" = "HIGH"),
         PERCENT_OF_MEDIAN_HOUSEHOLD_INCOME_MHI_RISK_LEVEL_BI = as.factor(ifelse
                                                                          (PERCENT_OF_MEDIAN_HOUSEHOLD_INCOME_MHI_RISK_LEVEL 
                                                                            == "NONE", "NONE", "HIGH")))

na_string2 <- "MISSING"
Data <- Data %>% replace_with_na_at(.vars= c("EXTREME_WATER_BILL_RISK_LEVEL", "DAYS_CASH_ON_HAND_RISK_LEVEL",
                                             "OPERATING_RATIO_RISK_LEVEL"), condition = ~.x %in% na_string2)

Data$EXTREME_WATER_BILL_RISK_LEVEL <- droplevels(Data$EXTREME_WATER_BILL_RISK_LEVEL)
Data$DAYS_CASH_ON_HAND_RISK_LEVEL <- droplevels(Data$DAYS_CASH_ON_HAND_RISK_LEVEL)
Data$OPERATING_RATIO_RISK_LEVEL <- droplevels(Data$OPERATING_RATIO_RISK_LEVEL)

Data <- Data %>% 
  mutate(EXTREME_WATER_BILL_RISK_LEVEL_BI = as.factor(ifelse(EXTREME_WATER_BILL_RISK_LEVEL == "NONE", "NONE", "HIGH")))

Data$FUNDING_any <- as.factor(ifelse(Data$FUNDING_RECEIVED_SINCE_2017 > 0, "Yes", "No"))

Data <- Data %>%
  mutate(FUNDING_any_failingoratriskonly = as.factor(case_when(FINAL_SAFER_STATUS == "At-Risk" | 
                                                                 FINAL_SAFER_STATUS == "Failing" ~ FUNDING_any,
                                                               .default = NA)))

#Adjust the non risk assessment affordability indicators so the mirror the others in directionality
Data$hasnotreceivedfunding <- Data$FUNDING_any_failingoratriskonly
Data$hasnotreceivedfunding <- revalue(Data$hasnotreceivedfunding , c("Yes" = "No", "No" = "Yes"))
Data$hasnotreceivedfunding <- relevel(Data$hasnotreceivedfunding, ref = "No")

Data$didnotapplycovid <- Data$Application.complete.
Data$didnotapplycovid <- relevel(Data$didnotapplycovid, ref = "Yes")
Data$didnotapplycovid  <- revalue(Data$didnotapplycovid , c("Yes" = "No", "No" = "Yes"))

#Create composite variables for each category. 

#version one is a count of how many of the indicators they are high on 

WQ <- Data %>% 
  dplyr::select(PWSID, HISTORY_OF_E_COLI_PRESENCE_RISK_LEVEL, TREATMENT_TECHNIUQE_VIOLATIONS_RISK_LEVEL,
                PERCENTAGE_OF_SOURCES_EXCEEDING_AN_MCL_RISK_LEVEL) %>% 
  mutate(HISTORY_OF_E_COLI_PRESENCE_RISK_LEVEL = case_when(HISTORY_OF_E_COLI_PRESENCE_RISK_LEVEL == "HIGH" ~ 1,
                                                           HISTORY_OF_E_COLI_PRESENCE_RISK_LEVEL == "NONE" ~ 0),
         TREATMENT_TECHNIUQE_VIOLATIONS_RISK_LEVEL = case_when(TREATMENT_TECHNIUQE_VIOLATIONS_RISK_LEVEL == "HIGH" ~ 1,
                                                               TREATMENT_TECHNIUQE_VIOLATIONS_RISK_LEVEL == "NONE" ~ 0),
         PERCENTAGE_OF_SOURCES_EXCEEDING_AN_MCL_RISK_LEVEL = case_when(PERCENTAGE_OF_SOURCES_EXCEEDING_AN_MCL_RISK_LEVEL ==
                                                                         "HIGH" ~ 1,
        PERCENTAGE_OF_SOURCES_EXCEEDING_AN_MCL_RISK_LEVEL == "NONE" ~ 0))

WQ <- WQ %>% mutate(WQ_combined_count = rowSums(WQ[,2:4], na.rm = TRUE))


Accessibility <- Data %>% 
  dplyr::select(PWSID, NUMBER_OF_WATER_SOURCES_RISK_LEVEL_BI, ABESENCE_OF_INTERTIES_RISK_LEVEL, 
                BOTTLED_WATER_OR_HAULED_WATER_RELIANCE_RISK_LEVEL) %>%
  mutate(NUMBER_OF_WATER_SOURCES_RISK_LEVEL_BI = case_when(NUMBER_OF_WATER_SOURCES_RISK_LEVEL_BI == "HIGH" ~ 1,
                                                           NUMBER_OF_WATER_SOURCES_RISK_LEVEL_BI == "NONE" ~ 0),
        ABESENCE_OF_INTERTIES_RISK_LEVEL = case_when(ABESENCE_OF_INTERTIES_RISK_LEVEL == "HIGH" ~ 1,
                                                      ABESENCE_OF_INTERTIES_RISK_LEVEL == "NONE" ~ 0),
        BOTTLED_WATER_OR_HAULED_WATER_RELIANCE_RISK_LEVEL = case_when(BOTTLED_WATER_OR_HAULED_WATER_RELIANCE_RISK_LEVEL ==
                                                                        "HIGH" ~ 1,
        BOTTLED_WATER_OR_HAULED_WATER_RELIANCE_RISK_LEVEL == "NONE" ~ 0))

Accessibility <- Accessibility %>% mutate(Accessibility_combined_count = rowSums(Accessibility[,2:4], na.rm = TRUE))

Affordability <- Data %>% 
  dplyr::select(PWSID, PERCENT_OF_MEDIAN_HOUSEHOLD_INCOME_MHI_RISK_LEVEL_BI, EXTREME_WATER_BILL_RISK_LEVEL_BI, 
                                        Application.complete.) %>%
  mutate(PERCENT_OF_MEDIAN_HOUSEHOLD_INCOME_MHI_RISK_LEVEL_BI = case_when(PERCENT_OF_MEDIAN_HOUSEHOLD_INCOME_MHI_RISK_LEVEL_BI
                                                                          == "HIGH" ~ 1,
        PERCENT_OF_MEDIAN_HOUSEHOLD_INCOME_MHI_RISK_LEVEL_BI == "NONE" ~ 0),
        EXTREME_WATER_BILL_RISK_LEVEL_BI = case_when(EXTREME_WATER_BILL_RISK_LEVEL_BI == "HIGH" ~ 1,
                                                      EXTREME_WATER_BILL_RISK_LEVEL_BI == "NONE" ~ 0),
        Application.complete. = case_when(Application.complete. == "No" ~ 1,
                                           Application.complete. == "Yes" ~ 0))

Affordability <- Affordability %>% mutate(Affordability_combined_count = rowSums(Affordability[,2:4], na.rm = TRUE))

TMF <- Data %>% dplyr::select(PWSID, OPERATOR_CERTIFICATION_VIOLATIONS_RISK_LEVEL,
                              MONITORING_AND_REPORTING_VIOLATIONS_RISK_LEVEL, DAYS_CASH_ON_HAND_RISK_LEVEL) %>%
  mutate(OPERATOR_CERTIFICATION_VIOLATIONS_RISK_LEVEL = case_when(OPERATOR_CERTIFICATION_VIOLATIONS_RISK_LEVEL == "HIGH" ~ 1,
                                                                  OPERATOR_CERTIFICATION_VIOLATIONS_RISK_LEVEL == "NONE" ~ 0),
        MONITORING_AND_REPORTING_VIOLATIONS_RISK_LEVEL = case_when(MONITORING_AND_REPORTING_VIOLATIONS_RISK_LEVEL == 
                                                                     "HIGH" ~ 1,
        MONITORING_AND_REPORTING_VIOLATIONS_RISK_LEVEL == "NONE" ~ 0),
        DAYS_CASH_ON_HAND_RISK_LEVEL = case_when(DAYS_CASH_ON_HAND_RISK_LEVEL == "HIGH" ~ 1,
                                                  DAYS_CASH_ON_HAND_RISK_LEVEL == "NONE" ~ 0))

TMF <- TMF %>% mutate(TMF_combined_count = rowSums(TMF[,2:4], na.rm = TRUE))

#version two is a binary - are any of the indicators high risk?

WQ$WQ_combined_any <- ifelse(WQ$WQ_combined_count >= 1, "1", "0")
WQ$WQ_combined_any <- as.factor(WQ$WQ_combined_any)

Accessibility$Accessibility_combined_any <- ifelse(Accessibility$Accessibility_combined_count >= 1, "1", "0")
Accessibility$Accessibility_combined_any <- as.factor(Accessibility$Accessibility_combined_any)

Affordability$Affordability_combined_any <- ifelse(Affordability$Affordability_combined_count >= 1, "1", "0")
Affordability$Affordability_combined_any <- as.factor(Affordability$Affordability_combined_any)

TMF$TMF_combined_any <- ifelse(TMF$TMF_combined_count >= 1, "1", "0")
TMF$TMF_combined_any  <- as.factor(TMF$TMF_combined_any)

#make first version four level factors
TMF$TMF_combined_count <- as.factor(TMF$TMF_combined_count)
Affordability$Affordability_combined_count <- as.factor(Affordability$Affordability_combined_count)
Accessibility$Accessibility_combined_count <- as.factor(Accessibility$Accessibility_combined_count)
WQ$WQ_combined_count <- as.factor(WQ$WQ_combined_count)
levels(WQ$WQ_combined_count) <- c(levels(WQ$WQ_combined_count), "3")

#add combined category scores into main dataset
Data <- left_join(Data, TMF[,c(1,5,6)], by = "PWSID")
Data <- left_join(Data, Affordability[,c(1,5,6)], by = "PWSID")
Data <- left_join(Data, Accessibility[,c(1,5,6)], by = "PWSID")
Data <- left_join(Data, WQ[,c(1,5,6)], by = "PWSID")

#write csv

write.csv(Data, file = here::here("Data_processed/Compiled_data_August2024.csv"))
