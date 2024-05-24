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
Effect_WQ$Key <- "Water quality"
Effect_Accessibility$Key <- "Accessibility"
Effect_Affordability$Key <- "Affordability"
Effect_TMF$Key <- "TMF capacity"
Probabilities <- bind_rows(Effect_Accessibility, Effect_Affordability, Effect_TMF, Effect_WQ)

#make figure faceting with key
plotCombined <- Probabilities %>%
  ggplot(aes(x = enfranchisement_final, y = fit)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "grey") +
  geom_point() +
  ylim(0.15,.95) +
  xlab(" ") +
  ylab(" ") +
  facet_wrap(~Key, nrow = 2, ncol = 2) +
  cowplot::theme_half_open(); plotCombined

#water quality subindicators figure
Effect_WQ_ecoli <- as.data.frame(Effect(focal.predictors = "enfranchisement_final", mod = ecoli))
Effect_WQ_TT <- as.data.frame(Effect(focal.predictors = "enfranchisement_final", mod = TT))
Effect_WQ_MCL <- as.data.frame(Effect(focal.predictors = "enfranchisement_final", mod = MCL))
Effect_WQ_CEC <- as.data.frame(Effect(focal.predictors = "enfranchisement_final", mod = CEC))


Effect_WQ_ecoli$Key <- "History of E. coli presence"
Effect_WQ_TT$Key <- "Treatment Technique violations"
Effect_WQ_MCL$Key <- "Sources exceeding an MCL"
Effect_WQ_CEC$Key <- "Constituents of emerging concern"
Probabilities_WQ <- bind_rows(Effect_WQ_ecoli, Effect_WQ_CEC, Effect_WQ_MCL, Effect_WQ_TT)

plotCombined_WQ <- Probabilities_WQ %>%
  ggplot(aes(x = enfranchisement_final, y = fit)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "grey") +
  geom_point() +
  ylim(0,0.20) +
  xlab(" ") +
  ylab(" ") +
  facet_wrap(~Key, nrow = 2, ncol = 2) +
  cowplot::theme_half_open(); plotCombined_WQ

#affordability subindicators figure
Effect_AF_MHI <- as.data.frame(Effect(focal.predictors = "enfranchisement_final", mod = MHI))
Effect_AF_extreme <- as.data.frame(Effect(focal.predictors = "enfranchisement_final", mod = extreme))
Effect_AF_covid <- as.data.frame(Effect(focal.predictors = "enfranchisement_final", mod = covid))
Effect_AF_funding <- as.data.frame(Effect(focal.predictors = "enfranchisement_final", mod = funding))


Effect_AF_MHI$Key <- ">150% of MHI"
Effect_AF_extreme$Key <- ">150% of statewide average"
Effect_AF_covid$Key <- "Did not apply for arrearage relief"
Effect_AF_funding$Key <- "Has not received state funding"
Probabilities_AF <- bind_rows(Effect_AF_MHI, Effect_AF_extreme, Effect_AF_covid, Effect_AF_funding)

plotCombined_AF <- Probabilities_AF %>%
  ggplot(aes(x = enfranchisement_final, y = fit)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "grey") +
  geom_point() +
  ylim(0,1) +
  xlab(" ") +
  ylab(" ") +
  facet_wrap(~Key, nrow = 2, ncol = 2) +
  cowplot::theme_half_open(); plotCombined_AF

#accessibility sub indicators figure
Effect_AC_sources <- as.data.frame(Effect(focal.predictors = "enfranchisement_final", mod = sources))
Effect_AC_interties <- as.data.frame(Effect(focal.predictors = "enfranchisement_final", mod = Interties))
Effect_AC_bottled <- as.data.frame(Effect(focal.predictors = "enfranchisement_final", mod = Bottled))
Effect_AC_sourcecapacity <- as.data.frame(Effect(focal.predictors = "enfranchisement_final", mod = sourcecapacity))


Effect_AC_sources$Key <- "Single source reliance"
Effect_AC_interties$Key <- "Lack of interties"
Effect_AC_bottled$Key <- "Reliance on bottled water"
Effect_AC_sourcecapacity$Key <- "Source capacity violations"
Probabilities_AC <- bind_rows(Effect_AC_sources, Effect_AC_interties, Effect_AC_bottled, Effect_AC_sourcecapacity)

plotCombined_AC <- Probabilities_AC %>%
  ggplot(aes(x = enfranchisement_final, y = fit)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "grey") +
  geom_point() +
  ylim(0,1) +
  xlab(" ") +
  ylab(" ") +
  facet_wrap(~Key, nrow = 2, ncol = 2) +
  cowplot::theme_half_open(); plotCombined_AC

#TMF subindicators figure
Effect_TMF_opcertviolations <- as.data.frame(Effect(focal.predictors = "enfranchisement_final", mod = Opcert_violations))
Effect_TMF_mrviolations <- as.data.frame(Effect(focal.predictors = "enfranchisement_final", mod = mrviolations))
Effect_TMF_cash <- as.data.frame(Effect(focal.predictors = "enfranchisement_final", mod = cash))
Effect_TMF_operating <- as.data.frame(Effect(focal.predictors = "enfranchisement_final", mod = operating))

Effect_TMF_opcertviolations$Key <- "Operator cert violations"
Effect_TMF_mrviolations$Key <- "M&R violations"
Effect_TMF_cash$Key <- "<30 days cash on hand"
Effect_TMF_operating$Key <- "<1 operating ratio"
Probabilities_TMF <- bind_rows(Effect_TMF_opcertviolations, Effect_TMF_mrviolations, Effect_TMF_cash, Effect_TMF_operating)

plotCombined_TMF <- Probabilities_TMF %>%
  ggplot(aes(x = enfranchisement_final, y = fit)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "grey") +
  geom_point() +
  ylim(0,0.5) +
  xlab(" ") +
  ylab(" ") +
  facet_wrap(~Key, nrow = 2, ncol = 2) +
  cowplot::theme_half_open(); plotCombined_TMF

#Try with linear regressions
Effect_CalEnviroSore <- as.data.frame(Effect(focal.predictors = "enfranchisement_final", mod = Calenviroscore))

#odds ratio plot

OddsWQ <- exp(cbind("Odds ratio" = coef(WQ1), confint.default(WQ1, level = 0.95))); OddsWQ

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