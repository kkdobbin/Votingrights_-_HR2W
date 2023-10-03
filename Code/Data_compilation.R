##Data compilation, cleaning and processing

#load libraries
library(tidyverse)

###Load data and initial manipulations

CWSgov <- read.csv(here::here("Data_raw/ACTIVE VERSION_ CWS institution types Sept 28 2023.csv"), header=T, na.strings=c("","NA"))
CWSgov$Primacy_FINAL <- as.factor(CWSgov$Primacy_FINAL)
CWSgov <- CWSgov %>% filter(Primacy_FINAL != "EPA Region 9") #for purposes of this project get rid of EPA regulated Tribal water systems
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
#NOTES: Mobile Home Parks can be considered ancillary but we aren't using them as such here because water provision is so central to their primary aspect and because they are so common (probably need to refine this rational for paper). This variable leaves to institution types as NA: Private unknown and Tribal which is okay since they will drop out of our analysis anyways. 

## create public/private variable
CWSgov$public <- ifelse(CWSgov$Final_inst_update == "Community Services District" | CWSgov$Final_inst_update == "County Service Area" | CWSgov$Final_inst_update == "Municipal Water District" | CWSgov$Final_inst_update == "City" | CWSgov$Final_inst_update == "County Sanitation District"| CWSgov$Final_inst_update == "County Water District"| CWSgov$Final_inst_update == "County Waterworks District" | CWSgov$Final_inst_update == "Irrigation District"| CWSgov$Final_inst_update == "Maintenance District"| CWSgov$Final_inst_update == "Municipal Utility District"| CWSgov$Final_inst_update == "Public Utility District"| CWSgov$Final_inst_update == "Resort Improvement District"| CWSgov$Final_inst_update == "Resource Conservation District"| CWSgov$Final_inst_update == "Sanitary District"| CWSgov$Final_inst_update == "Water Conservation District"| CWSgov$Final_inst_update == "Special Act District" | CWSgov$Final_inst_update == "Mutual Benefit" | CWSgov$Final_inst_update == "Joint Powers Authority/Agreement" | CWSgov$Final_inst_update == "California Water District" | CWSgov$Final_inst_update == "County" | CWSgov$Final_inst_update == "Federal" | CWSgov$Final_inst_update == "School District" | CWSgov$Final_inst_update == "State" | CWSgov$Final_inst_update == "Tribal", "Yes", "No")
CWSgov$public <- as.factor(CWSgov$public)

#remove ancillary systems for this analysis
CWSgov <- filter(CWSgov, ancillary== "No")

#create enfranchisement variable
CWSgov$enfranchisement<- ifelse(CWSgov$Final_inst_update == "Investor Owned Utility" | CWSgov$Final_inst_update == "Mobile Home Park", "None", NA)

CWSgov$enfranchisement <- ifelse(CWSgov$Final_inst_update == "Mutual Benefit" ,"Limited", CWSgov$enfranchisement)

CWSgov$enfranchisement <- ifelse(CWSgov$Final_inst_update == "California Water District" | CWSgov$Final_inst_update == "Joint Powers Authority/Agreement" | CWSgov$Final_inst_update == "Special Act District" | CWSgov$Final_inst_update == "Irrigation District", "Variable", CWSgov$enfranchisement)

CWSgov$enfranchisement <- ifelse(CWSgov$Final_inst_update == "Community Services District" | CWSgov$Final_inst_update == "County Service Area" | CWSgov$Final_inst_update == "Municipal Water District" | CWSgov$Final_inst_update == "City" | CWSgov$Final_inst_update == "County Sanitation District"| CWSgov$Final_inst_update == "County Water District"| CWSgov$Final_inst_update == "County Waterworks District" | CWSgov$Final_inst_update == "Irrigation District"| CWSgov$Final_inst_update == "Maintenance District"| CWSgov$Final_inst_update == "Municipal Utility District"| CWSgov$Final_inst_update == "Public Utility District"| CWSgov$Final_inst_update == "Resort Improvement District"| CWSgov$Final_inst_update == "Resource Conservation District"| CWSgov$Final_inst_update == "Sanitary District"| CWSgov$Final_inst_update == "Water Conservation District", "Full", CWSgov$enfranchisement)

CWSgov$enfranchisement <- as.factor(CWSgov$enfranchisement)

Variable_districts <- read.csv(here::here("Data_raw/Variablevotingdistricts_JMcoded.csv"))
Variable_districts <- Variable_districts[,c(1,7)] 
Variable_districts$PWSID <- as.factor(Variable_districts$PWSID)
Variable_districts$Vote_structure <- as.factor(Variable_districts$Vote_structure)
CWSgov <- left_join(CWSgov, Variable_districts)
CWSgov <- CWSgov %>% rename("Variablecoded" = "Vote_structure")

CWSgov$enfranchisement <- as.character(CWSgov$enfranchisement)
CWSgov$Variablecoded <- as.character(CWSgov$Variablecoded)
CWSgov <- CWSgov %>% mutate(enfranchisement_final = case_when(enfranchisement == "Variable" ~ Variablecoded, .default = enfranchisement))
CWSgov$enfranchisement_final <- as.factor(CWSgov$enfranchisement_final)
CWSgov <- CWSgov %>% filter(enfranchisement_final != "Appointed by member agencies") %>% filter(enfranchisement_final != "Unknown") # remove cases where enfranchisement is unknown and cases where board members are appointed by member agencies. Could change this later
CWSgov$enfranchisement_final <- droplevels(CWSgov$enfranchisement_final)

#NOTES: leaving dependent districts (e.g. CSAs) as full enfranchisement for now since they are popularly elected but this is a big thing to consider/discuss further



### add in outcome data

## Arrearage data
Arrearage <- read.csv(here::here("Data_raw/public-arrearage-program-data-2022-01-19.csv")) #data comes from SWRCB (https://www.waterboards.ca.gov/arrearage_payment_program/). This 1-19-22 version has remained the most updated on their website, I think applications were due December 2021 so that makes sense?
Arrearage <- rename(Arrearage, PWSID = Water.System.ID....PWSID.)
Arrearage$PWSID <- as.factor(Arrearage$PWSID)
Arrearage <- Arrearage %>% select(PWSID, Intend.to.apply., Application.complete.)
Arrearage$Intend.to.apply. <- as.factor(Arrearage$Intend.to.apply.)
Arrearage$Application.complete. <- as.factor(Arrearage$Application.complete.)

## SAFER
SAFER2023 <- read.csv(here::here("Data_raw/Drinking_Water_Risk_Assessment.csv"))#SAFER data comes from https://data.ca.gov/dataset/safer-failing-and-at-risk-drinking-water-systems. Dictionary saved in data folder
SAFER2023 <- rename(SAFER2023, PWSID = WATER_SYSTEM_NUMBER)
SAFER2023 <- SAFER2023 %>% select(PWSID, SERVICE_CONNECTIONS, POPULATION, MHI, CALENVIRO_SCREEN_SCORE, FINAL_SAFER_STATUS, PRIMARY_MCL_VIOLATION, SECONDARY_MCL_VIOLATION,E_COLI_VIOLATION, TREATMENT_TECHNIQUE_VIOLATION, MONITORING_AND_REPORTING_VIOLATION, WEIGHTED_WATER_QUALITY_SCORE, WEIGHTED_ACCESSIBILITY_SCORE, WEIGHTED_AFFORDABILITY_SCORE, WEIGHTED_TMF_CAPACITY_SCORE, FUNDING_RECEIVED_SINCE_2017, TOTAL_WEIGHTED_RISK_SCORE_BEFORE_DIVIDING_BY_CATEGORY_COUNT, CURRENT_FAILING)
SAFER2023$WEIGHTED_ACCESSIBILITY_SCORE <- as.numeric(SAFER2023$WEIGHTED_ACCESSIBILITY_SCORE)
SAFER2023$WEIGHTED_TMF_CAPACITY_SCORE <- as.numeric(SAFER2023$WEIGHTED_TMF_CAPACITY_SCORE)
SAFER2023$WEIGHTED_AFFORDABILITY_SCORE <- as.numeric(SAFER2023$WEIGHTED_AFFORDABILITY_SCORE)
SAFER2023$WEIGHTED_WATER_QUALITY_SCORE <- as.numeric(SAFER2023$WEIGHTED_WATER_QUALITY_SCORE)
SAFER2023$FINAL_SAFER_STATUS <- as.factor(SAFER2023$FINAL_SAFER_STATUS)
SAFER2023$PWSID <- as.factor(SAFER2023$PWSID)
SAFER2023$MHI <- as.numeric(SAFER2023$MHI)
SAFER2023$CALENVIRO_SCREEN_SCORE <- as.numeric(SAFER2023$CALENVIRO_SCREEN_SCORE)
SAFER2023$TOTAL_WEIGHTED_RISK_SCORE_BEFORE_DIVIDING_BY_CATEGORY_COUNT[SAFER2023$TOTAL_WEIGHTED_RISK_SCORE_BEFORE_DIVIDING_BY_CATEGORY_COUNT == "Not Assessed"] <- NA
SAFER2023$TOTAL_WEIGHTED_RISK_SCORE_BEFORE_DIVIDING_BY_CATEGORY_COUNT <- as.numeric(SAFER2023$TOTAL_WEIGHTED_RISK_SCORE_BEFORE_DIVIDING_BY_CATEGORY_COUNT)
SAFER2023$CURRENT_FAILING <- as.factor(SAFER2023$CURRENT_FAILING)

## join all together and final type corrections
Data <- left_join(CWSgov, SAFER2023)
Data <- left_join(Data, Arrearage)

Data$FundingYorN <- ifelse(Data$FUNDING_RECEIVED_SINCE_2017 > 0, "Yes", "No")
Data$FundingYorN <- as.factor(Data$FundingYorN)
Data <- rename(Data, Total_risk = TOTAL_WEIGHTED_RISK_SCORE_BEFORE_DIVIDING_BY_CATEGORY_COUNT)

#add water source and whether purchased from CA SDWIS download July 5
SDWIS <- read.csv(here::here("Data_raw/CA_SDWIS_alldownload_July52023.csv"))
SDWIS <- rename(SDWIS, PWSID = Water.System.No.)
SDWIS$PWSID <- as.factor(SDWIS$PWSID)
SDWIS$Type <- as.factor(SDWIS$Type)
SDWIS$Primary.Source.Water.Type <- as.factor(SDWIS$Primary.Source.Water.Type)
SDWIS <- SDWIS %>% select(PWSID, Type, Primary.Source.Water.Type)
SDWIS$Source <- as.factor(ifelse(SDWIS$Primary.Source.Water.Type == "GU" | SDWIS$Primary.Source.Water.Type == "GUP" | SDWIS$Primary.Source.Water.Type == "GW" | SDWIS$Primary.Source.Water.Type == "GWP", "GW", "SW"))
SDWIS$Purchased <- as.factor(ifelse(SDWIS$Primary.Source.Water.Type == "GUP" | SDWIS$Primary.Source.Water.Type == "SWP" | SDWIS$Primary.Source.Water.Type == "GWP", "Purchased", "Self-produced"))

Data <- left_join(Data, SDWIS)
Data$LN_POP <- log((Data$POPULATION+1)) #added one to population first to avoid issue of loging 0. Need to figure out if this is the best way....


write.csv(Data, file = here::here("Data_processed/Compiled_data.csv"))

