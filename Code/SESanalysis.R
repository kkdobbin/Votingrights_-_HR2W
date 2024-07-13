#SES analysis of system enfranchisement types

#Load libraries
library(tidyverse)
library(ggmap)
library(censusapi)
library(tidycensus)
library(sf)
library(sp)
#census_api_key("84059d7faf015832a99ef159379684476b2ec4a7", overwrite = TRUE, install = TRUE)
#readRenviron("~/.Renviron")
#Sys.getenv("CENSUS_API_KEY")

Demographics_blockg <- get_acs(geography = "block group", variables = c("B03002_001E", "B03002_003E", "B03002_004E", "B03002_005E", "B03002_006E", "B03002_007E", "B03002_012E", "B19013_001E", "B25003_001E", "B25003_002E", "B25003_003E", "B11001_001E"), state = "CA", year =2020, output = "wide", survey = "acs5", geometry = TRUE)

#rename variables
Demographics_blockg <- Demographics_blockg %>% dplyr::rename(Race.estimate.total = B03002_001E, Race.white.alone = B03002_003E, Race.black.alone = B03002_004E, Race.native.alone = B03002_005E, Race.asian.alone = B03002_006E, Race.PI.alone = B03002_007E, Race.hispanicorlatino = B03002_012E, Median.hh.income = B19013_001E, Tenure.estimate.total = B25003_001E, Tenure.owner = B25003_002E, Tenure.renter = B25003_003E, Households.total = B11001_001E)

#Make median household income into a count by multiplying median income by total households (see https://crd230.github.io/lab3.html)
Demographics_blockg$Income.aggregatecount <- Demographics_blockg$Median.hh.income*Demographics_blockg$Households.total

#load in boundary polygons for CWS
PWS_boundary <- st_read("Data_raw/PWS/")
str(PWS_boundary)
#plot(st_geometry(PWS_boundary)) #takes a while but works!

PWS_boundary$SABL_PWSID <- as.factor(PWS_boundary$SABL_PWSID)
PWS_boundary$WATER_SYST <- as.factor(PWS_boundary$WATER_SYST)
PWS_boundary <- PWS_boundary %>% filter(BOUNDARY_T != "Jurisdictional") #remove jurisidictional boundaires to get rid of duplicates

Data_small <- Data[,c(1,11)]
Data_geo <- left_join(PWS_boundary, Data_small, by = c("SABL_PWSID" = "PWSID"))
summary(Data_geo$enfranchisement_final) #Most are in the boundary dataset
Data_geo <- Data_geo[,c(2,4,5,15,19,33:36)]

#areal interpolation (https://crd230.github.io/lab3.html)
#make data set of just boundaries to play with
Boundaries <- Data_geo %>% dplyr::select(SABL_PWSID, geometry)
Boundaries <- Boundaries %>% unique() #all unique

library(areal)
Boundaries <- st_as_sf(Boundaries) #make sf object

#make to be the same crs
Boundaries <- st_transform(Boundaries, crs = 3857)
st_crs(Boundaries) 
Boundaries <- st_make_valid(Boundaries)
Demographics_blockg <- st_transform(Demographics_blockg, crs = 3857)
st_crs(Demographics_blockg) 

#now try areal interpolation
Interpolation <- aw_interpolate(Boundaries, tid = SABL_PWSID, source = Demographics_blockg, sid = GEOID, weight = "total", output = "tibble", extensive = c("Race.estimate.total", "Race.white.alone", "Race.black.alone", "Race.native.alone", "Race.asian.alone", "Race.PI.alone", "Race.hispanicorlatino", "Tenure.estimate.total", "Tenure.owner", "Tenure.renter", "Households.total", "Income.aggregatecount"))

#trouble shooting aw_interpolate command
#ar_validate(source = Demographics_blockg, target = Recieving_boundaries, varList = c("Race.estimate.total", "Race.white.alone", "Race.black.alone", "Race.native.alone", "Race.asian.alone", "Race.PI.alone", "Race.hispanicorlatino", "Tenure.estimate.total", "Tenure.owner", "Tenure.renter", "Households.total", "Income.aggregatecount"), verbose = TRUE)

#now add into cases, first remove geometry columns
Data_small <- left_join(Data_small, Interpolation, by = c("PWSID" = "SABL_PWSID"))

#make into percents/bock group MHI
Data_small$MHI <- ((Data_small$Income.aggregatecount)/(Data_small$Households.total))
summary(Data_small$MHI)

Data_small$Percent.hispanicorlatino <- ((Data_small$Race.hispanicorlatino)/(Data_small$Race.estimate.total))*100
summary(Data_small$Percent.hispanicorlatino)

Data_small$Percent.white <- ((Data_small$Race.white.alone)/(Data_small$Race.estimate.total))*100
summary(Data_small$Percent.white)

Data_small$Percent.black <- ((Data_small$Race.black.alone)/(Data_small$Race.estimate.total))*100
summary(Data_small$Percent.black)

Data_small$Percent.asian <- ((Data_small$Race.asian.alone)/(Data_small$Race.estimate.total))*100
summary(Data_small$Percent.asian)

Data_small$Percent.renter<- ((Data_small$Tenure.renter)/(Data_small$Tenure.estimate.total))*100
summary(Data_small$Percent.renter)

#Add in socioeconomic burden and cal enviroscreen score
Add <- Data %>% dplyr::select(PWSID, CALENVIRO_SCREEN_SCORE, HOUSEHOLD_SOCIOECONOMIC_BURDEN_RAW_SCORE)
Data_small <- left_join(Data_small, Add)

#ANOVA and Tukey HSD
MHI<- aov(MHI ~ enfranchisement_final, data = Data_small)
summary(MHI)
TukeyMHI <- TukeyHSD(MHI)
TukeyMHI_table <- as.data.frame(TukeyMHI$enfranchisement_final)

WHITE <- aov(Percent.white ~ enfranchisement_final, data = Data_small)
summary(WHITE)
TukeyWHITE <- TukeyHSD(WHITE)
TukeyWHITE_table <- as.data.frame(TukeyWHITE$enfranchisement_final)

LATINO <- aov(Percent.hispanicorlatino ~ enfranchisement_final, data = Data_small)
summary(LATINO)
TukeyLATINO <- TukeyHSD(LATINO)
TukeyLATINO_table <- as.data.frame(TukeyWHITE$enfranchisement_final)

BLACK <- aov(Percent.black ~ enfranchisement_final, data = Data_small)
summary(BLACK)
TukeyBLACK <- TukeyHSD(BLACK)
TukeyBLACK_table <- as.data.frame(TukeyBLACK$enfranchisement_final)

ASIAN <- aov(Percent.asian ~ enfranchisement_final, data = Data_small)
summary(ASIAN)
TukeyASIAN <- TukeyHSD(ASIAN)
TukeyASIAN_table <- as.data.frame(TukeyASIAN$enfranchisement_final)

RENTER <- aov(Percent.renter ~ enfranchisement_final, data = Data_small)
summary(RENTER)
TukeyRENTER <- TukeyHSD(RENTER)
TukeyRENTER_table <- as.data.frame(TukeyRENTER$enfranchisement_final)

CES <- aov(CALENVIRO_SCREEN_SCORE ~ enfranchisement_final, data = Data_small)
summary(CES)
TukeyCES <- TukeyHSD(CES)
TukeyCES_table <- as.data.frame(TukeyCES$enfranchisement_final)

#Make bloxplot

#MHI
Bplot_MHI <- ggplot(Data_small, aes(x=enfranchisement_final, y = MHI, color = enfranchisement_final)) +
  geom_boxplot(aes(color = enfranchisement_final)) + labs(x= " ", y = "MHI") + scale_color_brewer(palette = "Dark2")

#CalEnviro
Bplot_CES <- ggplot(Data_small, aes(x=enfranchisement_final, y = CALENVIRO_SCREEN_SCORE, group=enfranchisement_final, color = enfranchisement_final)) + geom_boxplot(aes(color = enfranchisement_final)) + labs(x= " ", y = "CalEnviroScreen score", fill = "Enfranchisement type") + scale_color_brewer(palette = "Dark2")

#household ses burden
Bplot_Burden <- ggplot(Data_small, aes(x=enfranchisement_final, y = HOUSEHOLD_SOCIOECONOMIC_BURDEN_RAW_SCORE, group=enfranchisement_final, fill=enfranchisement_final)) + geom_boxplot() + labs(x= " ", y = "Socioeconomic burden score") #sad

#LATINO
Bplot_Latino <- ggplot(Data_small, aes(x=enfranchisement_final, y = Percent.hispanicorlatino, group=enfranchisement_final, fill=enfranchisement_final)) + geom_boxplot() + labs(x= " ", y = "% population Hispanic or Latinx")

#BLACK
Bplot_Black <- ggplot(Data_small, aes(x=enfranchisement_final, y = Percent.black, group=enfranchisement_final, fill=enfranchisement_final)) + geom_boxplot() + labs(x= " ", y = "% population African American")

#ASIAN
Bplot_Asian <- ggplot(Data_small, aes(x=enfranchisement_final, y = Percent.asian, group=enfranchisement_final, fill=enfranchisement_final)) + geom_boxplot() + labs(x= " ", y = "% population Asian")

#WHITE
Bplot_White <- ggplot(Data_small, aes(x=enfranchisement_final, y = Percent.white, color = enfranchisement_final)) + geom_boxplot(aes(color = enfranchisement_final)) + labs(x= " ", y = "% population non-Hispanic white")  + scale_color_brewer(palette = "Dark2")

#RENTER
Bplot_Renter <- ggplot(Data_small, aes(x=enfranchisement_final, y = Percent.renter, color = enfranchisement_final)) + geom_boxplot(aes(color = enfranchisement_final)) + labs(x= " ", y = "% households renter occupied") + scale_color_brewer(palette = "Dark2"); Bplot_Renter 

#combine plots
library(ggpubr)
Boxplotpanel <- ggarrange(Bplot_CES, Bplot_MHI, Bplot_White, Bplot_Renter, ncol=2, nrow=2, common.legend = TRUE, legend="none" )

ggsave("Fig3_tmp.jpeg", Boxplotpanel, path = "Figures/", height = 6, width = 7.487, units = "in", dpi = 720)

#make a summary tables of descriptive statistics

Data_small_names <- Data_small
Data_small_names <- dplyr::rename(Data_small_names, c(enfranchisement = enfranchisement_final, CES = CALENVIRO_SCREEN_SCORE, white = Percent.white, renter= Percent.renter))

Test <- Data_small_names %>% dplyr::select(enfranchisement, CES, MHI, white, renter) %>%              
        group_by(enfranchisement) %>%
        summarise_all(funs(min = min, 
                     mean = mean, 
                     median = median,
                     max = max,
                     sd = sd), na.rm = TRUE)

Test <- Test %>% gather(stat, val) %>%
        separate(stat, into = c("var", "stat"), sep = "_") %>%
        spread(stat, val) %>%
        dplyr::select(var, min, mean, median, max, sd) 

Dem.rec <- Dem.rec %>% mutate(across(is.numeric, round, digits=2))

#make anoter summary tables of descriptive statistics for racial and ethnic breakdowns

Data_small_names2 <- Data_small
Data_small_names2 <- dplyr::rename(Data_small_names2, c(enfranchisement = enfranchisement_final, white = Percent.white, latino = Percent.hispanicorlatino, black = Percent.black, asian = Percent.asian))

Test2 <- Data_small_names2 %>% dplyr::select(enfranchisement, white, latino, black, asian) %>%              
  group_by(enfranchisement) %>%
  summarise_all(funs(min = min, 
                     mean = mean, 
                     median = median,
                     max = max,
                     sd = sd), na.rm = TRUE)

Test2 <- Test2 %>% gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  dplyr::select(var, min, mean, median, max, sd) 

Dem.rec2 <- Dem.rec2 %>% mutate(across(is.numeric, round, digits=2))
