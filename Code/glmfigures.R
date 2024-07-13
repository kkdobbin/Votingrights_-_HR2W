#Predicted probabilities


## Try effects package to visually display predicted probabilities instead of odds ratios

library(effects)
plot(allEffects(WQ1))

#composite score plots and arrangement ussing effects package

WQplot <- plot(predictorEffect("enfranchisement_final", WQ1),
     main = "Water quality",
     grid=TRUE,
     rescale.axis = FALSE,
     axes=list(y=list(lim=c(0,1),
                      lab=" ")))


Accessibilityplot <- plot(predictorEffect("enfranchisement_final", Accessibility1),
                          main = "Accessibility",
                          grid=TRUE,
                          rescale.axis = FALSE,
                          axes=list(y=list(lim=c(0,1),
                                           lab=" ")))

Affordabilityplot <- plot(predictorEffect("enfranchisement_final", Affordability1),
                          main = "Affordability",
                          grid=TRUE,
                          rescale.axis = FALSE,
                          axes=list(y=list(lim=c(0,1),
                                           lab=" ")))

TMFplot <- plot(predictorEffect("enfranchisement_final", TMF1),
                          main = "TMF capacity",
                          grid=TRUE,
                          rescale.axis = FALSE,
                          axes=list(y=list(lim=c(0,1),
                                           lab=" ")))

library(gridExtra) 
grid.arrange(WQplot, Accessibilityplot, Affordabilityplot, TMFplot, ncol=2)

#Try moving this into GGPlot to have more control of the formatting

#pull out probabilities in confidence intervals
Effect_WQ <- as.data.frame(Effect(focal.predictors = "enfranchisement_final", mod = WQ1))
Effect_Accessibility <- as.data.frame(Effect(focal.predictors = "enfranchisement_final", mod = Accessibility1))
Effect_Affordability <- as.data.frame(Effect(focal.predictors = "enfranchisement_final", mod = Affordability1))
Effect_TMF <- as.data.frame(Effect(focal.predictors = "enfranchisement_final", mod = TMF1))

#add key and combine data sets
Effect_WQ$Key <- "Water quality risk"
Effect_Accessibility$Key <- "Accessibility risk"
Effect_Affordability$Key <- "Affordability risk"
Effect_TMF$Key <- "TMF capacity risk"
Probabilities <- bind_rows(Effect_Accessibility, Effect_Affordability, Effect_TMF, Effect_WQ)

#make figure faceting with key
plotCombined <- Probabilities %>%
  ggplot(aes(x = enfranchisement_final, y = fit)) +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = enfranchisement_final), width = 0.2, show.legend = FALSE) +
  geom_point(aes(color = enfranchisement_final), show.legend = FALSE) +
  xlab(" ") +
  ylab(" ") +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~Key, scales = "free", nrow = 2, ncol = 2) +
  cowplot::theme_half_open(); plotCombined

ggsave("Fig2_tmp.jpeg", plotCombined, path = "Figures/", height = 6, width = 7.487, units = "in", dpi = 720)

#water quality subindicators figure
Effect_WQ_ecoli <- as.data.frame(Effect(focal.predictors = "enfranchisement_final", mod = ecoli))
#self fix CI issue FOR NOW
Effect_WQ_ecoli[1, 4] <-  -0.3476059
Effect_WQ_ecoli[1, 5] <-  0.3502342
Effect_WQ_ecoli[2, 4] <-  -0.2524489
Effect_WQ_ecoli[2, 5] <-  0.254357
Effect_WQ_ecoli[3, 4] <-  -0.5090467
Effect_WQ_ecoli[3, 5] <-  0.5128981


Effect_WQ_TT <- as.data.frame(Effect(focal.predictors = "enfranchisement_final", mod = TT))
Effect_WQ_MCL <- as.data.frame(Effect(focal.predictors = "enfranchisement_final", mod = MCL))
Effect_WQ_CEC <- as.data.frame(Effect(focal.predictors = "enfranchisement_final", mod = CEC))


Effect_WQ_ecoli$Key <- "E. coli"
Effect_WQ_TT$Key <- "Treatment Technique violations"
Effect_WQ_MCL$Key <- "MCL exceedences"
Effect_WQ_CEC$Key <- "Constituents of emerging concern"
Probabilities_WQ <- bind_rows(Effect_WQ_ecoli, Effect_WQ_CEC, Effect_WQ_MCL, Effect_WQ_TT)
Probabilities_WQ$Category <- "Water quality"

plotCombined_WQ <- Probabilities_WQ %>%
  ggplot(aes(x = enfranchisement_final, y = fit)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "grey") +
  geom_point() +
  xlab(" ") +
  ylab(" ") +
  facet_wrap(~Key, scales = "free", nrow = 2, ncol = 2) +
  cowplot::theme_half_open(); plotCombined_WQ


#affordability subindicators figure
Effect_AF_MHI <- as.data.frame(Effect(focal.predictors = "enfranchisement_final", mod = MHI))
Effect_AF_extreme <- as.data.frame(Effect(focal.predictors = "enfranchisement_final", mod = extreme))
Effect_AF_covid <- as.data.frame(Effect(focal.predictors = "enfranchisement_final", mod = covid))
Effect_AF_funding <- as.data.frame(Effect(focal.predictors = "enfranchisement_final", mod = funding))


Effect_AF_MHI$Key <- "Bill ≥ 1.5% of MHI"
Effect_AF_extreme$Key <- "Bill ≥ 150% of statewide average"
Effect_AF_covid$Key <- "Did not apply for arrearage relief"
Effect_AF_funding$Key <- "No funding since 2017"
Probabilities_AF <- bind_rows(Effect_AF_MHI, Effect_AF_extreme, Effect_AF_covid, Effect_AF_funding)
Probabilities_AF$Category <- "Affordability"

plotCombined_AF <- Probabilities_AF %>%
  ggplot(aes(x = enfranchisement_final, y = fit)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "grey") +
  geom_point() +
  xlab(" ") +
  ylab(" ") +
  facet_wrap(~Key, scale = "free", nrow = 2, ncol = 2) +
  cowplot::theme_half_open(); plotCombined_AF

#accessibility sub indicators figure
Effect_AC_sources <- as.data.frame(Effect(focal.predictors = "enfranchisement_final", mod = sources))
Effect_AC_interties <- as.data.frame(Effect(focal.predictors = "enfranchisement_final", mod = Interties))
Effect_AC_bottled <- as.data.frame(Effect(focal.predictors = "enfranchisement_final", mod = Bottled))
Effect_AC_sourcecapacity <- as.data.frame(Effect(focal.predictors = "enfranchisement_final", mod = sourcecapacity))


Effect_AC_sources$Key <- "Single source"
Effect_AC_interties$Key <- "Lack of interties"
Effect_AC_bottled$Key <- "Bottled water"
Effect_AC_sourcecapacity$Key <- "Source capacity violations"
Probabilities_AC <- bind_rows(Effect_AC_sources, Effect_AC_interties, Effect_AC_bottled, Effect_AC_sourcecapacity)
Probabilities_AC$Category <- "Accessibility"

plotCombined_AC <- Probabilities_AC %>%
  ggplot(aes(x = enfranchisement_final, y = fit)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "grey") +
  geom_point() +
  xlab(" ") +
  ylab(" ") +
  facet_wrap(~Key, scales = "free", nrow = 2, ncol = 2) +
  cowplot::theme_half_open(); plotCombined_AC


#TMF subindicators figure
Effect_TMF_opcertviolations <- as.data.frame(Effect(focal.predictors = "enfranchisement_final", mod = Opcert_violations))
#Just for nwo calculate by hand. NOTE DONE CALCULATING HERE> 
Effect_TMF_opcertviolations[1, 4] <-  -0.1571286
Effect_TMF_opcertviolations[1, 5] <-  0.1583093
Effect_TMF_opcertviolations[2, 4] <-  -0.2155767
Effect_TMF_opcertviolations[2, 5] <-  0.2171968
Effect_TMF_opcertviolations[3, 4] <-  -0.1239938
Effect_TMF_opcertviolations[3, 5] <-  0.1249254

Effect_TMF_mrviolations <- as.data.frame(Effect(focal.predictors = "enfranchisement_final", mod = mrviolations))
Effect_TMF_cash <- as.data.frame(Effect(focal.predictors = "enfranchisement_final", mod = cash))
Effect_TMF_operating <- as.data.frame(Effect(focal.predictors = "enfranchisement_final", mod = operating))

Effect_TMF_opcertviolations$Key <- "Operator certification violations"
Effect_TMF_mrviolations$Key <- "M&R violations"
Effect_TMF_cash$Key <- "<30 days cash on hand"
Effect_TMF_operating$Key <- "Operating ratio <1"
Probabilities_TMF <- bind_rows(Effect_TMF_opcertviolations, Effect_TMF_mrviolations, Effect_TMF_cash, Effect_TMF_operating)
Probabilities_TMF$Category <- "TMF capacity"

plotCombined_TMF <- Probabilities_TMF %>%
  ggplot(aes(x = enfranchisement_final, y = fit)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "grey") +
  geom_point() +
  xlab(" ") +
  ylab(" ") +
  facet_wrap(~Key, scales = "free", nrow = 2, ncol = 2) +
  cowplot::theme_half_open(); plotCombined_TMF

#Make one massive combined plot with all sixzteen?
library(RColorBrewer)

#Probabilities_MASSIVE <- bind_rows(Effect_WQ_ecoli, Effect_WQ_CEC, Effect_WQ_MCL, Effect_WQ_TT, Effect_AF_MHI, Effect_AF_extreme, Effect_AF_covid, Effect_AF_funding, Effect_AC_sources, Effect_AC_interties, Effect_AC_bottled, Effect_AC_sourcecapacity, Effect_TMF_opcertviolations, Effect_TMF_mrviolations, Effect_TMF_cash, Effect_TMF_operating)
Probabilities_MASSIVE <- bind_rows(Probabilities_WQ, Probabilities_AF, Probabilities_AC, Probabilities_TMF)
Probabilities_MASSIVE$Key <- as.factor(Probabilities_MASSIVE$Key)
Probabilities_MASSIVE$Key <- factor(Probabilities_MASSIVE$Key, levels=c("E. coli", "MCL exceedences", "Treatment Technique violations", "Constituents of emerging concern", "Bill ≥ 150% of statewide average", "Bill ≥ 1.5% of MHI", "Did not apply for arrearage relief", "No funding since 2017", "Single source", "Source capacity violations", "Lack of interties", "Bottled water", "Operator certification violations", "M&R violations", "<30 days cash on hand", "Operating ratio <1"))

plotCombined_MASSIVE <- Probabilities_MASSIVE %>%
  ggplot(aes(x = enfranchisement_final, y = fit)) +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = enfranchisement_final), width = 0.2, show.legend = FALSE) +
  geom_point(aes(color = enfranchisement_final), show.legend = FALSE) +
  xlab(" ") +
  ylab(" ") +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~Category+Key, scales = "free", nrow = 4, ncol = 4) +
  cowplot::theme_half_open(); plotCombined_MASSIVE
 

 
 

#odds ratio plot

OddsWQ <- exp(cbind("Odds ratio" = coef(WQ1), confint.default(WQ1, level = 0.95))); OddsWQ

OddsAffordability1 <- exp(cbind("Odds ratio" = coef(Affordability1), confint.default(Affordability1, level = 0.95))); OddsAffordability1


#Try to make odds ratio plot for first 16 models
Oddsecoli <-  as.data.frame(exp(cbind("Odds ratio" = coef(ecoli), confint.default(ecoli, level = 0.95))))
Oddsecoli <- Oddsecoli[2:3,];Oddsecoli
Oddsecoli$enfranchisement<- as.factor(c("Limited", "None"))
Oddsecoli$DV <- as.factor("E. coli")

OddsTT <-  as.data.frame(exp(cbind("Odds ratio" = coef(TT), confint.default(TT, level = 0.95))))
OddsTT <- OddsTT[2:3,];OddsTT
OddsTT$enfranchisement<- as.factor(c("Limited", "None"))
OddsTT$DV <- as.factor("Treatment Technique violations")

OddsMCL <-  as.data.frame(exp(cbind("Odds ratio" = coef(MCL), confint.default(MCL, level = 0.95))))
OddsMCL <- OddsMCL[2:3,];OddsMCL
OddsMCL$enfranchisement<- as.factor(c("Limited", "None"))
OddsMCL$DV <- as.factor("MCL exceedences")

OddsCEC <-  as.data.frame(exp(cbind("Odds ratio" = coef(CEC), confint.default(CEC, level = 0.95))))
OddsCEC <- OddsCEC[2:3,];OddsCEC
OddsCEC$enfranchisement<- as.factor(c("Limited", "None"))
OddsCEC$DV <- as.factor("Constituents of emerging concern")

OddsWQ <- rbind(Oddsecoli, OddsTT, OddsMCL, OddsCEC); OddsWQ 
OddsWQ <- dplyr::rename(OddsWQ, c(OR = 'Odds ratio', LL = '2.5 %', UL = '97.5 %'))

#https://rpubs.com/mbounthavong/forest_plots_r

Test <- ggplot(OddsWQ, aes(y = DV, x = OR, fill = enfranchisement)) +
  geom_point(shape = 18, size = 5) +  
  geom_errorbarh(aes(xmin = LL, xmax = UL), height = 0.25) +
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) 
Test

#OLD from another script

jpeg(filename = "Figures/oddsratioWQ.jpg", width = 7, height = 6, units = "in", res = 100)

Wq1coefs <- coef(summary(WQ1))
or5 <- Wq1coefs[,1]
or5b<-exp(Wq1coefs[,1])
ci5a<-exp(Wq1coefs[,1]-1.96*Wq1coefs[,2])
ci5b<-exp(Wq1coefs[,1]+1.96*Wq1coefs[,2])


place<-c(0,1,2,3,4,5,6,7,8,9,10,11,12)
place1<-c(1,2,3,4,5,6,7,8,9,10,11,12)


par(mar=c(5,12.75,4,3), family="serif", cex=.8)
plot.new()
plot.window(xlim=c(0,3), ylim=c(0, 5))

points(x=exp(or5[2:13]), y=place1, pch=19)

for(i in 2:13){
  lines(x=c(ci5a[i], ci5b[i]), y=c(place[i],place[i]), lwd=1)
}
axis(1, cex.axis=1.2)
axis(2, at=place1, las=2, cex.axis=1.6) #labels=c("AD GSA***","JPA GSA**", "MOU GSA**", "MHI*", "Incorporated**","Intersection %***", "Population**","Groundwater reliant","GSA eligible***", "% Latino","Number DACs*", "Facilitation")
abline(v=1, lty=2)
box()


dev.off()

#Make bargraph for enfranchisement variables

Bargraph <- ggplot(Data, aes(enfranchisement_final)) + 
  geom_bar(aes(fill = enfranchisement_final), show.legend = FALSE) + 
  scale_fill_brewer(palette = "Dark2") +
  xlab(" ") +
  ylab(" "); Bargraph
