#attempting october analysis including ancillary systems 

#load libraries
library(tidyverse)
library(vtable)
library(sjPlot)

###Load data
CWSgov <- read.csv(here::here("Data_processed/Compiled_data.csv"))
CWSgov <- CWSgov[,-1]
CWSgov$enfranchisement_final <- as.factor(CWSgov$enfranchisement_final)
CWSgov$Final_inst_update <- as.factor(CWSgov$Final_inst_update)

CWSgov$voting <- ifelse(CWSgov$Final_inst_update == "Community Services District" | CWSgov$Final_inst_update == "County Service Area" | CWSgov$Final_inst_update == "Municipal Water District" | CWSgov$Final_inst_update == "City" | CWSgov$Final_inst_update == "County Sanitation District"| CWSgov$Final_inst_update == "County Water District"| CWSgov$Final_inst_update == "County Waterworks District" | CWSgov$Final_inst_update == "Irrigation District"| CWSgov$Final_inst_update == "Maintenance District"| CWSgov$Final_inst_update == "Municipal Utility District"| CWSgov$Final_inst_update == "Public Utility District"| CWSgov$Final_inst_update == "Resort Improvement District"| CWSgov$Final_inst_update == "Resource Conservation District"| CWSgov$Final_inst_update == "Sanitary District"| CWSgov$Final_inst_update == "Water Conservation District", "Full", NA)

CWSgov$voting <- ifelse(CWSgov$Final_inst_update == "Mutual benefit" | CWSgov$Final_inst_update == "Mutual Benefit" | CWSgov$Final_inst_update == "California Water District" | CWSgov$Final_inst_update == "Stock Corp" , "Limited", CWSgov$voting)

CWSgov$voting <- ifelse(CWSgov$Final_inst_update == "Mobile home park" | CWSgov$Final_inst_update == "Mobile Home Park" | CWSgov$Final_inst_update == "Investor Owned Utility" | CWSgov$Final_inst_update == "LLC" | CWSgov$Final_inst_update == "Private ancillary" | CWSgov$Final_inst_update == "Private - Ancillary", "None", CWSgov$voting)

CWSgov$voting <- as.factor(CWSgov$voting)
summary(CWSgov$voting)

Data <- CWSgov
Data$enfranchisement_final <- Data$voting
Data$CURRENT_FAILING <- as.factor(Data$CURRENT_FAILING)
Data$Source <- as.factor(Data$Source)
Data$enfranchisement_final <- as.factor(Data$enfranchisement_final)
Data$Application.complete. <- as.factor(Data$Application.complete.)
Data$PRIMARY_MCL_VIOLATION <- as.factor(Data$PRIMARY_MCL_VIOLATION)


Data$CURRENT_FAILING <- relevel(Data$CURRENT_FAILING, ref = "Not Failing")
Data$enfranchisement_final <- relevel(Data$enfranchisement_final, ref = "None")
Failing <- glm(CURRENT_FAILING ~ enfranchisement_final + LN_POP + Source, data = Data, family= binomial)
summary(Failing)

Data$PRIMARY_MCL_VIOLATION <- relevel(Data$PRIMARY_MCL_VIOLATION, ref = "NO")
MCL <-  glm(PRIMARY_MCL_VIOLATION ~ enfranchisement_final + LN_POP + Source, data = Data, family= binomial)
summary(MCL)

Plot1 <- plot_model(Failing, type = "pred", terms = "enfranchisement_final", axis.title = c("Enfranchisement", "Failing system status"), title = " ") + theme_sjplot(base_size = 10); Plot1

Risk <- lm(Total_risk ~ enfranchisement_final + LN_POP + Source, Data)
summary(Risk)

Plot2 <- plot_model(Risk, type = "pred", terms = "enfranchisement_final", axis.title = c("Enfranchisement", "Predicted total risk score"), title = " ") + theme_sjplot(base_size = 10); Plot2

Affordability <- lm(WEIGHTED_AFFORDABILITY_SCORE ~ enfranchisement_final + LN_POP + Source, Data)
summary(Affordability)

Data$Application.complete. <- relevel(Data$Application.complete., ref = "No")
COVID <- glm(Application.complete. ~ enfranchisement_final + LN_POP + Source, data = Data, family= binomial)
summary(COVID)

Plot3 <- plot_model(COVID, type = "pred", terms = "enfranchisement_final", axis.title = c("Enfranchisement", "Applied for covid arrearage relief"), title = " ") + theme_sjplot(base_size = 10); Plot3
