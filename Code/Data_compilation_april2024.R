##Data compilation, cleaning and processing

#load libraries
library(tidyverse)

###Load data and initial manipulations

CWSgov <- read.csv(here::here("Data_raw/ACTIVE VERSION_ CWS institution types Sept 28 2023.csv"), header=T, na.strings=c("","NA"))
CWSgov$Primacy_FINAL <- as.factor(CWSgov$Primacy_FINAL)
CWSgov <- CWSgov %>% filter(Primacy_FINAL != "EPA Region 9") #for purposes of this project get rid of EPA regulated Tribal water systems
CWSgov <- CWSgov %>% filter(Final_inst_update != "Tribal Government") #Get rid of three remaining systems that are Tribal but state regulated for whatever reason
CWSgov$Primacy_FINAL <- droplevels(CWSgov$Primacy_FINAL)
CWSgov <- CWSgov[,c(1:6)] #get rid of columns I don't need
CWSgov$PWSID <- as.factor(CWSgov$PWSID)
CWSgov$Counties_Served <- as.factor(CWSgov$Counties_Served)
CWSgov$Final_inst_update <- as.factor(CWSgov$Final_inst_update)
CWSgov$Inst_Subtype  <- as.factor(CWSgov$Inst_Subtype)

### create new variables for ancillary/non ancillary, public/private and customer enfranchisement
 
## create ancillary variable
CWSgov$ancillary <- ifelse(CWSgov$Final_inst_update == "Community Services District" | CWSgov$Final_inst_update == "County Service Area" | CWSgov$Final_inst_update == "Municipal Water District" | CWSgov$Final_inst_update == "City" | CWSgov$Final_inst_update == "County Sanitation District"| CWSgov$Final_inst_update == "County Water District"| CWSgov$Final_inst_update == "County Waterworks District" | CWSgov$Final_inst_update == "Irrigation District"| CWSgov$Final_inst_update == "Maintenance District"| CWSgov$Final_inst_update == "Municipal Utility District"| CWSgov$Final_inst_update == "Public Utility District"| CWSgov$Final_inst_update == "Resort Improvement District"| CWSgov$Final_inst_update == "Resource Conservation District"| CWSgov$Final_inst_update == "Sanitary District"| CWSgov$Final_inst_update == "Water Conservation District"| CWSgov$Final_inst_update == "Special Act District" | CWSgov$Final_inst_update == "Mutual Benefit" | CWSgov$Final_inst_update == "Joint Powers Authority/Agreement" | CWSgov$Final_inst_update == "Investor Owned Utility" | CWSgov$Final_inst_update == "California Water District" | CWSgov$Final_inst_update == "Mobile Home Park","No", NA)

CWSgov$ancillary <- ifelse(CWSgov$Final_inst_update == "County" | CWSgov$Final_inst_update == "Federal" | CWSgov$Final_inst_update == "Private - Ancillary" | CWSgov$Final_inst_update == "School District" | CWSgov$Final_inst_update == "State", "Yes", CWSgov$ancillary)
CWSgov$ancillary <- as.factor(CWSgov$ancillary)
#NOTES: Mobile Home Parks can be considered ancillary but we aren't using them as such here because water provision is so central to their primary aspect and because they are so common (probably need to refine this rational for paper). This variable leaves to institution type as NA: Private unknown which is okay since they will drop out of our analysis anyways with all the ancillary systems. 

## create public/private variable
CWSgov$public <- ifelse(CWSgov$Final_inst_update == "Community Services District" | CWSgov$Final_inst_update == "County Service Area" | CWSgov$Final_inst_update == "Municipal Water District" | CWSgov$Final_inst_update == "City" | CWSgov$Final_inst_update == "County Sanitation District"| CWSgov$Final_inst_update == "County Water District"| CWSgov$Final_inst_update == "County Waterworks District" | CWSgov$Final_inst_update == "Irrigation District"| CWSgov$Final_inst_update == "Maintenance District"| CWSgov$Final_inst_update == "Municipal Utility District"| CWSgov$Final_inst_update == "Public Utility District"| CWSgov$Final_inst_update == "Resort Improvement District"| CWSgov$Final_inst_update == "Resource Conservation District"| CWSgov$Final_inst_update == "Sanitary District"| CWSgov$Final_inst_update == "Water Conservation District"| CWSgov$Final_inst_update == "Special Act District" | CWSgov$Final_inst_update == "Joint Powers Authority/Agreement" | CWSgov$Final_inst_update == "California Water District" | CWSgov$Final_inst_update == "County" | CWSgov$Final_inst_update == "Federal" | CWSgov$Final_inst_update == "School District" | CWSgov$Final_inst_update == "State" | CWSgov$Final_inst_update == "Tribal", "Yes", "No")
CWSgov$public <- as.factor(CWSgov$public)

#remove ancillary systems for this analysis
CWSgov <- filter(CWSgov, ancillary== "No")

#create enfranchisement variable
CWSgov$enfranchisement<- ifelse(CWSgov$Final_inst_update == "Investor Owned Utility" | CWSgov$Final_inst_update == "Mobile Home Park", "None", NA)

CWSgov$enfranchisement <- ifelse(CWSgov$Final_inst_update == "Mutual Benefit" ,"Limited", CWSgov$enfranchisement)

CWSgov$enfranchisement <- ifelse(CWSgov$Final_inst_update == "California Water District" | CWSgov$Final_inst_update == "Joint Powers Authority/Agreement" | CWSgov$Final_inst_update == "Special Act District" | CWSgov$Final_inst_update == "Irrigation District", "Variable", CWSgov$enfranchisement)

CWSgov$enfranchisement <- ifelse(CWSgov$Final_inst_update == "Community Services District" | CWSgov$Final_inst_update == "County Service Area" | CWSgov$Final_inst_update == "Municipal Water District" | CWSgov$Final_inst_update == "City" | CWSgov$Final_inst_update == "County Sanitation District"| CWSgov$Final_inst_update == "County Water District"| CWSgov$Final_inst_update == "County Waterworks District" | CWSgov$Final_inst_update == "Maintenance District"| CWSgov$Final_inst_update == "Municipal Utility District"| CWSgov$Final_inst_update == "Public Utility District"| CWSgov$Final_inst_update == "Resort Improvement District"| CWSgov$Final_inst_update == "Resource Conservation District"| CWSgov$Final_inst_update == "Sanitary District"| CWSgov$Final_inst_update == "Water Conservation District", "Full", CWSgov$enfranchisement)

CWSgov$enfranchisement <- as.factor(CWSgov$enfranchisement)

Variable_districts <- read.csv(here::here("Data_raw/Variablevotingdistricts_JMcoded.csv"))
Variable_districts <- Variable_districts[,c(1,7)] 
Variable_districts$PWSID <- as.factor(Variable_districts$PWSID)
Variable_districts$Vote_structure <- as.factor(Variable_districts$Vote_structure)
CWSgov <- left_join(CWSgov, Variable_districts)
CWSgov <- CWSgov %>% dplyr::rename("Variablecoded" = "Vote_structure")

CWSgov$enfranchisement <- as.character(CWSgov$enfranchisement)
CWSgov$Variablecoded <- as.character(CWSgov$Variablecoded)
CWSgov <- CWSgov %>% mutate(enfranchisement_final = case_when(enfranchisement == "Variable" ~ Variablecoded, .default = enfranchisement))
CWSgov$enfranchisement_final <- as.factor(CWSgov$enfranchisement_final)

#NOTES: leaving dependent districts (e.g. CSAs) as full enfranchisement for now since they are popularly elected 



### add in outcome data

## Arrearage data
Arrearage <- read.csv(here::here("Data_raw/public-arrearage-program-data-2022-01-19.csv")) #data comes from SWRCB (https://www.waterboards.ca.gov/arrearage_payment_program/, original website is now accessible as an archive at the bottom of the page). This 1-19-22 version has remained the most updated on their website for many years before being replaced by the extended arrearage program data, there are two different deadline dates listed for the original program, Dec 2021 and April 2022, I am waiting for the baord to confirm which is correct and if this is the complete list. There is data available about who applied for the extended program but its not in csv/excel format so not using for now). 
Arrearage <- dplyr::rename(Arrearage, PWSID = Water.System.ID....PWSID.)
Arrearage <- Arrearage[!duplicated(Arrearage[,c("PWSID")]),] #get rid of duplicated row
Arrearage$PWSID <- as.factor(Arrearage$PWSID)
Arrearage <- Arrearage %>% dplyr::select(PWSID, Intend.to.apply., Application.complete.)
Arrearage$Intend.to.apply. <- as.factor(Arrearage$Intend.to.apply.)
Arrearage$Application.complete. <- as.factor(Arrearage$Application.complete.)

Arrearage <- distinct(Arrearage, .keep_all = )

## SAFER
SAFER2023 <- read.csv(here::here("Data_raw/Drinking_Water_Risk_Assessment.csv"))#SAFER data comes from https://data.ca.gov/dataset/safer-failing-and-at-risk-drinking-water-systems. Dictionary saved in data folder. Is from 2023 report. 
SAFER2023 <- dplyr::rename(SAFER2023, PWSID = WATER_SYSTEM_NUMBER)
SAFER2023 <- SAFER2023 %>% dplyr::select(PWSID, SERVICE_CONNECTIONS, POPULATION, MHI,
                                         CALENVIRO_SCREEN_SCORE, FINAL_SAFER_STATUS, HOUSEHOLD_SOCIOECONOMIC_BURDEN_RAW_SCORE,
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

library(naniar)
na_string <- "Not Assessed"
SAFER2023 <- SAFER2023 %>% replace_with_na_all(condition = ~.x %in% na_string)
SAFER2023[,c(9:27)] <- lapply(SAFER2023[,c(9:27)], factor)
SAFER2023$PWSID <- as.factor(SAFER2023$PWSID)
SAFER2023$MHI <- as.numeric(SAFER2023$MHI)
SAFER2023$FINAL_SAFER_STATUS <- as.factor(SAFER2023$FINAL_SAFER_STATUS)

SAFER2023$CALENVIRO_SCREEN_SCORE <- as.numeric(SAFER2023$CALENVIRO_SCREEN_SCORE)
SAFER2023$HOUSEHOLD_SOCIOECONOMIC_BURDEN_RAW_SCORE <- as.numeric(SAFER2023$HOUSEHOLD_SOCIOECONOMIC_BURDEN_RAW_SCORE)

## join all together and final type corrections
Data <- left_join(CWSgov, SAFER2023)
Data <- left_join(Data, Arrearage)

#add water source and whether purchased from CA SDWIS download July 5
SDWIS <- read.csv(here::here("Data_raw/CA_SDWIS_alldownload_July52023.csv"))
SDWIS <- dplyr::rename(SDWIS, PWSID = Water.System.No.)
SDWIS$PWSID <- as.factor(SDWIS$PWSID)
SDWIS$Type <- as.factor(SDWIS$Type)
SDWIS$Primary.Source.Water.Type <- as.factor(SDWIS$Primary.Source.Water.Type)
SDWIS <- SDWIS %>% dplyr::select(PWSID, Type, Primary.Source.Water.Type)
SDWIS$Source <- as.factor(ifelse(SDWIS$Primary.Source.Water.Type == "GU" | SDWIS$Primary.Source.Water.Type == "GUP" | SDWIS$Primary.Source.Water.Type == "GW" | SDWIS$Primary.Source.Water.Type == "GWP", "GW", "SW"))
SDWIS$Purchased <- as.factor(ifelse(SDWIS$Primary.Source.Water.Type == "GUP" | SDWIS$Primary.Source.Water.Type == "SWP" | SDWIS$Primary.Source.Water.Type == "GWP", "Purchased", "Self-produced"))

Data <- left_join(Data, SDWIS)
Data$LN_POP <- log((Data$POPULATION+1)) #added one to population first to avoid issue of loging 0. Need to figure out if this is the best way....

#adjust the indicators that have three levels so there is only one (no or minimal risk versus high/medium risk)
Data$CONSTITUENTS_OF_EMERGING_CONCERN_RISK_LEVEL_BI <- as.factor(ifelse(Data$CONSTITUENTS_OF_EMERGING_CONCERN_RISK_LEVEL == "NONE", "NONE", "HIGH"))
Data$NUMBER_OF_WATER_SOURCES_RISK_LEVEL_BI <- as.factor(ifelse(Data$CONSTITUENTS_OF_EMERGING_CONCERN_RISK_LEVEL == "NONE", "NONE", "HIGH"))
Data$BOTTLED_WATER_OR_HAULED_WATER_RELIANCE_RISK_LEVEL <- recode(Data$BOTTLED_WATER_OR_HAULED_WATER_RELIANCE_RISK_LEVEL, "VERY HIGH" = "HIGH")
Data$PERCENT_OF_MEDIAN_HOUSEHOLD_INCOME_MHI_RISK_LEVEL_BI <- as.factor(ifelse(Data$PERCENT_OF_MEDIAN_HOUSEHOLD_INCOME_MHI_RISK_LEVEL == "NONE", "NONE", "HIGH"))
na_string2 <- "MISSING"
Data <- Data %>% replace_with_na_at(.vars= c("EXTREME_WATER_BILL_RISK_LEVEL", "DAYS_CASH_ON_HAND_RISK_LEVEL", "OPERATING_RATIO_RISK_LEVEL"),
  condition = ~.x %in% na_string2)
Data$EXTREME_WATER_BILL_RISK_LEVEL <- droplevels(Data$EXTREME_WATER_BILL_RISK_LEVEL)
Data$DAYS_CASH_ON_HAND_RISK_LEVEL <- droplevels(Data$DAYS_CASH_ON_HAND_RISK_LEVEL)
Data$OPERATING_RATIO_RISK_LEVEL <- droplevels(Data$OPERATING_RATIO_RISK_LEVEL)
Data$EXTREME_WATER_BILL_RISK_LEVEL_BI <- as.factor(ifelse(Data$EXTREME_WATER_BILL_RISK_LEVEL == "NONE", "NONE", "HIGH"))

Data$FUNDING_any <- as.factor(ifelse(Data$FUNDING_RECEIVED_SINCE_2017 > 0, "Yes", "No"))

Data$FUNDING_any_failingoratriskonly <- NA
Data$FUNDING_any_failingoratriskonly <- as.factor(ifelse(Data$FINAL_SAFER_STATUS == "At-Risk" | Data$FINAL_SAFER_STATUS == "Failing", Data$FUNDING_any, Data$FUNDING_any_failingoratriskonly))
library(plyr)
Data$FUNDING_any_failingoratriskonly <- revalue(Data$FUNDING_any_failingoratriskonly, c("1" = "No", "2" = "Yes"))

#Adjust the non risk assessment affordability indicators so the mirror the others in directional
Data$didnotapplycovid <- Data$Application.complete.
Data$didnotapplycovid <- relevel(Data$didnotapplycovid, ref = "Yes")
Data$hasnotreceivedfunding <- Data$FUNDING_any_failingoratriskonly
Data$hasnotreceivedfunding <- relevel(Data$hasnotreceivedfunding, ref = "Yes")

#Create composite variables for each category. 

#version one is a count of how many of the indicators they are high on 

WQ <- Data %>% dplyr::select(PWSID, HISTORY_OF_E_COLI_PRESENCE_RISK_LEVEL, TREATMENT_TECHNIUQE_VIOLATIONS_RISK_LEVEL,
                              PERCENTAGE_OF_SOURCES_EXCEEDING_AN_MCL_RISK_LEVEL, CONSTITUENTS_OF_EMERGING_CONCERN_RISK_LEVEL_BI) %>% 
  mutate(HISTORY_OF_E_COLI_PRESENCE_RISK_LEVEL = case_when(HISTORY_OF_E_COLI_PRESENCE_RISK_LEVEL == "HIGH" ~ 1,
                                                           HISTORY_OF_E_COLI_PRESENCE_RISK_LEVEL == "NONE" ~ 0)) %>%
  mutate(TREATMENT_TECHNIUQE_VIOLATIONS_RISK_LEVEL = case_when(TREATMENT_TECHNIUQE_VIOLATIONS_RISK_LEVEL == "HIGH" ~ 1,
                                                               TREATMENT_TECHNIUQE_VIOLATIONS_RISK_LEVEL == "NONE" ~ 0)) %>%
  mutate(PERCENTAGE_OF_SOURCES_EXCEEDING_AN_MCL_RISK_LEVEL = case_when(PERCENTAGE_OF_SOURCES_EXCEEDING_AN_MCL_RISK_LEVEL == "HIGH" ~ 1,
                                                                        PERCENTAGE_OF_SOURCES_EXCEEDING_AN_MCL_RISK_LEVEL == "NONE" ~ 0)) %>% 
  mutate(CONSTITUENTS_OF_EMERGING_CONCERN_RISK_LEVEL_BI = case_when(CONSTITUENTS_OF_EMERGING_CONCERN_RISK_LEVEL_BI == "HIGH" ~ 1,
                                                                    CONSTITUENTS_OF_EMERGING_CONCERN_RISK_LEVEL_BI == "NONE" ~ 0))

WQ <- WQ %>% mutate(WQ_combined_count = rowSums(WQ[,2:5], na.rm = TRUE))
  

Accessibility <- Data %>% dplyr::select(PWSID, NUMBER_OF_WATER_SOURCES_RISK_LEVEL_BI, ABESENCE_OF_INTERTIES_RISK_LEVEL,   
                                       BOTTLED_WATER_OR_HAULED_WATER_RELIANCE_RISK_LEVEL, SOURCE_CAPACITY_VIOLATION_RISK_LEVEL) %>%
  mutate(NUMBER_OF_WATER_SOURCES_RISK_LEVEL_BI = case_when(NUMBER_OF_WATER_SOURCES_RISK_LEVEL_BI == "HIGH" ~ 1,
                                                           NUMBER_OF_WATER_SOURCES_RISK_LEVEL_BI == "NONE" ~ 0)) %>%
  mutate(ABESENCE_OF_INTERTIES_RISK_LEVEL = case_when(ABESENCE_OF_INTERTIES_RISK_LEVEL == "HIGH" ~ 1,
                                                      ABESENCE_OF_INTERTIES_RISK_LEVEL == "NONE" ~ 0)) %>%
  mutate(BOTTLED_WATER_OR_HAULED_WATER_RELIANCE_RISK_LEVEL = case_when(BOTTLED_WATER_OR_HAULED_WATER_RELIANCE_RISK_LEVEL == "HIGH" ~ 1,
                                                                       BOTTLED_WATER_OR_HAULED_WATER_RELIANCE_RISK_LEVEL == "NONE" ~ 0)) %>%
  mutate(SOURCE_CAPACITY_VIOLATION_RISK_LEVEL = case_when(SOURCE_CAPACITY_VIOLATION_RISK_LEVEL == "HIGH" ~ 1,
                                                          SOURCE_CAPACITY_VIOLATION_RISK_LEVEL == "NONE" ~ 0))

Accessibility <- Accessibility %>% mutate(Accessibility_combined_count = rowSums(Accessibility[,2:5], na.rm = TRUE))


Affordability <- Data %>% dplyr::select(PWSID, PERCENT_OF_MEDIAN_HOUSEHOLD_INCOME_MHI_RISK_LEVEL_BI, EXTREME_WATER_BILL_RISK_LEVEL_BI, 
                                       Application.complete., FUNDING_any_failingoratriskonly) %>%
  mutate(PERCENT_OF_MEDIAN_HOUSEHOLD_INCOME_MHI_RISK_LEVEL_BI = case_when(PERCENT_OF_MEDIAN_HOUSEHOLD_INCOME_MHI_RISK_LEVEL_BI == "HIGH" ~ 1,
                                                                          PERCENT_OF_MEDIAN_HOUSEHOLD_INCOME_MHI_RISK_LEVEL_BI == "NONE" ~ 0)) %>%
  mutate(EXTREME_WATER_BILL_RISK_LEVEL_BI = case_when(EXTREME_WATER_BILL_RISK_LEVEL_BI == "HIGH" ~ 1,
                                                      EXTREME_WATER_BILL_RISK_LEVEL_BI == "NONE" ~ 0)) %>%
  mutate(Application.complete. = case_when(Application.complete. == "No" ~ 1,
                                           Application.complete. == "Yes" ~ 0)) %>%
  mutate(FUNDING_any_failingoratriskonly = case_when(FUNDING_any_failingoratriskonly == "No" ~ 1,
                                                     FUNDING_any_failingoratriskonly == "Yes" ~ 0))

Affordability <- Affordability %>% mutate(Affordability_combined_count = rowSums(Affordability[,2:5], na.rm = TRUE))


TMF <- Data %>% dplyr::select(PWSID, OPERATOR_CERTIFICATION_VIOLATIONS_RISK_LEVEL, MONITORING_AND_REPORTING_VIOLATIONS_RISK_LEVEL, DAYS_CASH_ON_HAND_RISK_LEVEL,
                              OPERATING_RATIO_RISK_LEVEL) %>%
  mutate(OPERATOR_CERTIFICATION_VIOLATIONS_RISK_LEVEL = case_when(OPERATOR_CERTIFICATION_VIOLATIONS_RISK_LEVEL == "HIGH" ~ 1,
                                                                  OPERATOR_CERTIFICATION_VIOLATIONS_RISK_LEVEL == "NONE" ~ 0)) %>%
  mutate(MONITORING_AND_REPORTING_VIOLATIONS_RISK_LEVEL = case_when(MONITORING_AND_REPORTING_VIOLATIONS_RISK_LEVEL == "HIGH" ~ 1,
                                                                    MONITORING_AND_REPORTING_VIOLATIONS_RISK_LEVEL == "NONE" ~ 0)) %>%
  mutate(DAYS_CASH_ON_HAND_RISK_LEVEL = case_when(DAYS_CASH_ON_HAND_RISK_LEVEL == "HIGH" ~ 1,
                                                  DAYS_CASH_ON_HAND_RISK_LEVEL == "NONE" ~ 0)) %>%
  mutate(OPERATING_RATIO_RISK_LEVEL = case_when(OPERATING_RATIO_RISK_LEVEL == "HIGH" ~ 1,
                                                OPERATING_RATIO_RISK_LEVEL == "NONE" ~ 0))

TMF <- TMF %>% mutate(TMF_combined_count = rowSums(TMF[,2:5], na.rm = TRUE))


#version two is a binary - are any of the indicators high risk?

WQ$WQ_combined_any <- ifelse(WQ$WQ_combined_count >= 1, "1", "0")
WQ$WQ_combined_any <- as.factor(WQ$WQ_combined_any)

Accessibility$Accessibility_combined_any <- ifelse(Accessibility$Accessibility_combined_count >= 1, "1", "0")
Accessibility$Accessibility_combined_any <- as.factor(Accessibility$Accessibility_combined_any)

Affordability$Affordability_combined_any <- ifelse(Affordability$Affordability_combined_count >= 1, "1", "0")
Affordability$Affordability_combined_any <- as.factor(Affordability$Affordability_combined_any)

TMF$TMF_combined_any <- ifelse(TMF$TMF_combined_count >= 1, "1", "0")

#make first version four level factors
TMF$TMF_combined_count <- as.factor(TMF$TMF_combined_count)
Affordability$Affordability_combined_count <- as.factor(Affordability$Affordability_combined_count)
Accessibility$Accessibility_combined_count <- as.factor(Accessibility$Accessibility_combined_count)
WQ$WQ_combined_count <- as.factor(WQ$WQ_combined_count)
levels(WQ$WQ_combined_count) <- c(levels(WQ$WQ_combined_count), "4")


#add combined category scores into main dataset
Data <- left_join(Data, TMF[,c(1,6,7)], by = "PWSID")
Data <- left_join(Data, Affordability[,c(1,6,7)], by = "PWSID")
Data <- left_join(Data, Accessibility[,c(1,6,7)], by = "PWSID")
Data <- left_join(Data, WQ[,c(1,6,7)], by = "PWSID")

#write csv

write.csv(Data, file = here::here("Data_processed/Compiled_data_April2024.csv"))
