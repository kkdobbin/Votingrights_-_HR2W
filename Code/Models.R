#FINAL DATA COMPILATION (from rmarkdown)
library(tidyverse)
Data <- read.csv(here::here("Data_processed/Compiled_data_April2024.csv"))
Data <- Data[,-1]
CWSgov <- read.csv(here::here("Data_raw/ACTIVE VERSION_ CWS institution types Sept 28 2023.csv"), header=T, na.strings=c("","NA")) #just for comparison of full body of systems in state
CWSgov$Primacy_FINAL <- as.factor(CWSgov$Primacy_FINAL)
Data$enfranchisement_final <- as.factor(Data$enfranchisement_final)
Data$Final_inst_update <- as.factor(Data$Final_inst_update)
#correct types
Data[,c(1:11,16,19:58)] <- lapply(Data[,c(1:11,16,19:58)], factor)
levels(Data$WQ_combined_count) <- c(levels(Data$WQ_combined_count), "4")

Data <- Data %>% filter(enfranchisement_final != "Appointed by member agencies") %>% filter(enfranchisement_final != "Unknown") # remove cases where enfranchisement is unknown and cases where board members are appointed by member agencies. Could change this later
Data$enfranchisement_final <- droplevels(Data$enfranchisement_final)

Data$FAILING <- ifelse(Data$FINAL_SAFER_STATUS == "Failing", "Failing", "Not failing")
Data$FAILING <- as.factor(Data$FAILING)

#population from safer
#water source and whether purchased from CA SDWIS download July 5
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

Data$Source <- as.factor(Data$Source)
Data$public <- as.factor(Data$public)

#re-order factor levels for enfrahcisement to be full, limited, none
Data$enfranchisement_final <- factor(Data$enfranchisement_final, levels = c("Full", "Limited", "None"))

#Make self produced water the reference (compared to purchased)
Data$Purchased <- relevel(Data$Purchased, ref = "Self-produced")

#MODELS

#Failing
Data$FAILING <- relevel(Data$FAILING, ref = "Not failing")
Data$enfranchisement_final <- relevel(Data$enfranchisement_final, ref = "Full")
Failing <- glm(FAILING ~ enfranchisement_final + LN_POP + Source + Purchased, data = Data, family= binomial)
summary(Failing)

#Water quality
Data$WQ_combined_any <- as.factor(Data$WQ_combined_any) #not needed as saying binomial interprets it this way anyway but fo rsummary stats
WQ1 <- glm(WQ_combined_any ~ enfranchisement_final + LN_POP + Source + Purchased, data = Data, family= binomial)
summary(WQ1)

#library(MASS)
#WQ2 <- polr(WQ_combined_count ~ enfranchisement_final + LN_POP + Source + Purchased, data = Data, method = "logistic" , Hess = TRUE)
#summary(WQ2)


##WQ sub components
Data$HISTORY_OF_E_COLI_PRESENCE_RISK_LEVEL <- relevel(Data$HISTORY_OF_E_COLI_PRESENCE_RISK_LEVEL, ref = "NONE")
ecoli <- glm(HISTORY_OF_E_COLI_PRESENCE_RISK_LEVEL ~ enfranchisement_final + LN_POP + Source + Purchased, data = Data, family= binomial)
summary(ecoli)
exp(coef(ecoli))


Data$TREATMENT_TECHNIUQE_VIOLATIONS_RISK_LEVEL <- relevel(Data$TREATMENT_TECHNIUQE_VIOLATIONS_RISK_LEVEL, ref = "NONE")
TT <- glm(TREATMENT_TECHNIUQE_VIOLATIONS_RISK_LEVEL ~ enfranchisement_final + LN_POP + Source + Purchased, data = Data, family= binomial)
summary(TT)

Data$PERCENTAGE_OF_SOURCES_EXCEEDING_AN_MCL_RISK_LEVEL <- relevel(Data$PERCENTAGE_OF_SOURCES_EXCEEDING_AN_MCL_RISK_LEVEL, ref = "NONE")
MCL <- glm(PERCENTAGE_OF_SOURCES_EXCEEDING_AN_MCL_RISK_LEVEL ~ enfranchisement_final + LN_POP + Source + Purchased, data = Data, family= binomial)
summary(MCL)

Data$CONSTITUENTS_OF_EMERGING_CONCERN_RISK_LEVEL_BI <- relevel(Data$CONSTITUENTS_OF_EMERGING_CONCERN_RISK_LEVEL_BI, ref = "NONE")
CEC <- glm(CONSTITUENTS_OF_EMERGING_CONCERN_RISK_LEVEL_BI ~ enfranchisement_final + LN_POP + Source + Purchased, data = Data, family= binomial)
summary(CEC)

#Accessibility
Accessibility1 <- glm(Accessibility_combined_any ~ enfranchisement_final + LN_POP + Source + Purchased, data = Data, family= binomial)
summary(Accessibility1)

#Accessibility2 <- polr(Accessibility_combined_count ~ enfranchisement_final + LN_POP + Source + Purchased, data = Data, method = "logistic" , Hess = TRUE)
#summary(Accessibility2)
#tidy(Accessibility2, conf.int = TRUE)


##Accessibility sub components
Data$NUMBER_OF_WATER_SOURCES_RISK_LEVEL_BI <- relevel(Data$NUMBER_OF_WATER_SOURCES_RISK_LEVEL_BI, ref = "NONE")
sources <- glm(NUMBER_OF_WATER_SOURCES_RISK_LEVEL_BI ~ enfranchisement_final + LN_POP + Source + Purchased, data = Data, family= binomial)
summary(sources)

Data$ABESENCE_OF_INTERTIES_RISK_LEVEL <- relevel(Data$ABESENCE_OF_INTERTIES_RISK_LEVEL, ref = "NONE")
Interties <- glm(ABESENCE_OF_INTERTIES_RISK_LEVEL ~ enfranchisement_final + LN_POP + Source + Purchased, data = Data, family= binomial)
summary(Interties)

Data$BOTTLED_WATER_OR_HAULED_WATER_RELIANCE_RISK_LEVEL <- relevel(Data$BOTTLED_WATER_OR_HAULED_WATER_RELIANCE_RISK_LEVEL, ref = "NONE")
Bottled <- glm(BOTTLED_WATER_OR_HAULED_WATER_RELIANCE_RISK_LEVEL ~ enfranchisement_final + LN_POP + Source + Purchased, data = Data, family= binomial)
summary(Bottled)

Data$SOURCE_CAPACITY_VIOLATION_RISK_LEVEL <- relevel(Data$SOURCE_CAPACITY_VIOLATION_RISK_LEVEL, ref = "NONE")
sourcecapacity <- glm(SOURCE_CAPACITY_VIOLATION_RISK_LEVEL ~ enfranchisement_final + LN_POP + Source + Purchased, data = Data, family= binomial)
summary(sourcecapacity)

#Affordability 
Affordability1 <- glm(Affordability_combined_any ~ enfranchisement_final + LN_POP + Source + Purchased, data = Data, family= binomial)
summary(Affordability1)

#Affordability2 <- polr(Affordability_combined_count ~ enfranchisement_final + LN_POP + Source + Purchased, data = Data, method = "logistic" , Hess = TRUE)
#summary(Affordability2)
#tidy(Affordability2, conf.int = TRUE)


## Affordability sub components
Data$PERCENT_OF_MEDIAN_HOUSEHOLD_INCOME_MHI_RISK_LEVEL_BI <- relevel(Data$PERCENT_OF_MEDIAN_HOUSEHOLD_INCOME_MHI_RISK_LEVEL_BI, ref = "NONE")
MHI <- glm(PERCENT_OF_MEDIAN_HOUSEHOLD_INCOME_MHI_RISK_LEVEL_BI ~ enfranchisement_final + LN_POP + Source + Purchased, data = Data, family= binomial)
summary(MHI)

Data$EXTREME_WATER_BILL_RISK_LEVEL_BI <- relevel(Data$EXTREME_WATER_BILL_RISK_LEVEL_BI, ref = "NONE")
extreme <- glm(EXTREME_WATER_BILL_RISK_LEVEL_BI ~ enfranchisement_final + LN_POP + Source + Purchased, data = Data, family= binomial)
summary(extreme)

covid <- glm(didnotapplycovid ~ enfranchisement_final + LN_POP + Source + Purchased, data = Data, family= binomial)
summary(covid)

Data$FUNDING_any_failingoratriskonly <- relevel(Data$FUNDING_any_failingoratriskonly, ref = "No")
funding <- glm(hasnotreceivedfunding ~ enfranchisement_final + LN_POP + Source + Purchased, data = Data, family= binomial)
summary(funding)

# TMF
TMF1 <- glm(TMF_combined_any ~ enfranchisement_final + LN_POP + Source + Purchased, data = Data, family= binomial)
summary(TMF1)

#TMF2 <- polr(TMF_combined_count ~ enfranchisement_final + LN_POP + Source + Purchased, data = Data, method = "logistic" , Hess = TRUE)
#summary(TMF2)
#tidy(TMF2, conf.int = TRUE)

#Plot9 <- plot_model(TMF2 , type = "pred", terms = "enfranchisement_final", axis.title = c("Enfranchisement", "TMF_combined_count"), title = " ") + theme_sjplot(base_size = 10); Plot9

## TMF sub components
Data$OPERATOR_CERTIFICATION_VIOLATIONS_RISK_LEVEL <- relevel(Data$OPERATOR_CERTIFICATION_VIOLATIONS_RISK_LEVEL, ref = "NONE")
Opcert_violations <- glm(OPERATOR_CERTIFICATION_VIOLATIONS_RISK_LEVEL ~ enfranchisement_final + LN_POP + Source + Purchased, data = Data, family= binomial)
summary(Opcert_violations)

Data$MONITORING_AND_REPORTING_VIOLATIONS_RISK_LEVEL <- relevel(Data$MONITORING_AND_REPORTING_VIOLATIONS_RISK_LEVEL, ref = "NONE")
mrviolations <- glm(MONITORING_AND_REPORTING_VIOLATIONS_RISK_LEVEL ~ enfranchisement_final + LN_POP + Source + Purchased, data = Data, family= binomial)
summary(mrviolations)

Data$DAYS_CASH_ON_HAND_RISK_LEVEL <- relevel(Data$DAYS_CASH_ON_HAND_RISK_LEVEL, ref = "NONE")
cash <- glm(DAYS_CASH_ON_HAND_RISK_LEVEL ~ enfranchisement_final + LN_POP + Source + Purchased, data = Data, family= binomial)
summary(cash)

Data$OPERATING_RATIO_RISK_LEVEL <- relevel(Data$OPERATING_RATIO_RISK_LEVEL, ref = "NONE")
operating <- glm(OPERATING_RATIO_RISK_LEVEL ~ enfranchisement_final + LN_POP + Source + Purchased, data = Data, family= binomial)
summary(operating)

#Who is served MOVE THIS TO SES ANALYSIS
Calenviroscore <- lm(CALENVIRO_SCREEN_SCORE ~ enfranchisement_final + LN_POP + Source + Purchased, Data)
summary(Calenviroscore)

ES<- aov(CALENVIRO_SCREEN_SCORE ~ enfranchisement_final, data = Data)
summary(ES)
TukeyHSD(ES)

socioeconomicburden <- lm(HOUSEHOLD_SOCIOECONOMIC_BURDEN_RAW_SCORE ~ enfranchisement_final + LN_POP + Source + Purchased, Data)
summary(socioeconomicburden)

SES<- aov(HOUSEHOLD_SOCIOECONOMIC_BURDEN_RAW_SCORE ~ enfranchisement_final, data = Data) #measures the percent of households in a census tract that are both low income (making less than 80% of the Housing and Urban Development (HUD) Area Median Family Income) and severely burdened by housing. costs (paying greater than 50% of their income to housing costs). Higher is more burden. 
summary(SES)
TukeyHSD(SES)

plot(predictorEffect("enfranchisement_final", Calenviroscore))

#Use zelig to fix CI issue son predicted probabilites?
library(zelig)
set.seed(1990)
x.out_ecoli <- setx(ecoli, enfranchisement_final = "None", LN_POP = 6.526, Source = "GW", Purchased = "Self-produced")
s.out_LR_no <- sim(FinalModel, x=x.out_LR_no, num=100000)
summary(s.out_LR_no) 
