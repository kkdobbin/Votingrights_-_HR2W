#Comparing institutional type data 2018 to 2023

#Identify systems (if any) that have changed institutional type since 2018
library(tidyverse)
Twentythree <- read.csv(here::here("Data_raw/ACTIVE VERSION_ CWS institution types Sept 28 2023.csv"),
                                  header=T,
                                  na.strings=c("","NA"))

Eighteen <- read.csv(here::here("Data_raw/Dobbin_Fencl2021_Californiawaterinstitutionsdata.csv"),
                     header=T,
                     na.strings=c("","NA"))
Eighteen$PWSID <- Eighteen$PSWID

Compare <- left_join(Twentythree, Eighteen)
Compare <- Compare[, c(1,2,3,5,6,7,16)]

#Compared 2018 and 2023 data by hand and created list of changes, ignoring differences that were noted as mistakes in the 2018 coding and improvements in methods for 2023 coding as detailed in dataset (new ways of distinguising mutual benefits systems for example). Then investigated each of these cases to 1) make sure it represented a real change and not a mistake and 2) determine if enfranchisement was altered with the change. Then added to this list systems under the control of an administrator  prior to July 2024 per the SWRCB list as of July 24, 2024 and any systems I know of under receivership. Receivorship is not tracked anywhere so there may be more of these systems but suspect not many as it is less used now than administrators
Changes <- read.csv(here::here("Data_raw/Inst_changed_2018_2023.csv"),
                                   header=T,
                                   na.strings=c("","NA"))

#reduce to only what is needed
Changes_small <- Changes[,c(1,4)]


#Load list of systems in receivorship/with administrators to remove
