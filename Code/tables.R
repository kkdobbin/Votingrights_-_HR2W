#Coefficient and OR tables

#load libraries
library(stargazer)

Composites <- list(Affordability1, WQ1, Accessibility1, TMF1)
Table_composites <- stargazer(Composites, type = "text")

Quality <- list(ecoli, TT, MCL, CEC)
Table_WQ <- stargazer(Quality, type = "text")

Access <- list(sources, Interties, Bottled, sourcecapacity)
Table_AC <- stargazer(Access, type = "text")

Afford <- list(MHI, extreme, covid, funding)
Table_AF <- stargazer(Afford, type = "text")

TechMF <- list(Opcert_violations, mrviolations, cash, operating)
Table_TMF <- stargazer(TechMF, type = "text")

#example table with just one model
covidOR <- covid
covidOR$coefficients <- exp(covidOR$coefficients)
p.values <- list(summary(covid)$coefficients[,4])
p.values2 <- list(c(1,1,1,1,1,1))

Test <- capture.output(stargazer(covid, covidOR, type = 'text', 
                                column.labels = c("coefficients(se)", "Odds ratio"),
                                model.numbers = FALSE, 
                                dep.var.labels = "Arrearage releif", ci=c(F,T), 
                                star.cutoffs = c(0.05, 0.01, 0.001),
                                p = c(p.values, p.values2),
                                omit.stat = c("ll", "aic"),
                                covariate.labels = c("Limited enfranchisement", "No enfranchisement",
                                            "Population (log)", "Surface water",
                                            "Purchased water"),
                                omit = "Constant",
                                apply.ci = function(x) { 0 }))

cat(paste(gsub("\\(0.000, 0.000\\)", "", Test), collapse = "\n"), "\n")

#Try for Composites
WQ1OR <- WQ1
WQ1OR$coefficients <- exp(WQ1OR$coefficients)
p.valuesWQ1 <- list(summary(WQ1)$coefficients[,4])

Affordability1OR <- Affordability1
Affordability1OR$coefficients <- exp(Affordability1OR$coefficients)
pvaluesAffordability1 <- list(summary(Affordability1)$coefficients[,4])

Accessibility1OR <- Accessibility1
Accessibility1OR$coefficients <- exp(Accessibility1OR$coefficients)
pvaluesAccesibility1 <- list(summary(Accessibility1)$coefficients[,4])

TMF1OR <- TMF1
TMF1OR$coefficients <- exp(TMF1OR$coefficients)
pvaluesTMF1 <- list(summary(TMF1)$coefficients[,4])

p.values2 <- list(c(1,1,1,1,1,1))

compositemodellist <- list(WQ1, WQ1OR, Affordability1, Affordability1OR,
                           Accessibility1, Accessibility1OR, TMF1, TMF1OR)

Compositestable <- capture.output(stargazer(compositemodellist,
                                 type = 'html', 
                                 column.labels = c("coefficients(se)", "Odds ratio(95%CI)", 
                                                   "coefficients(se)", "Odds ratio(95%CI)", 
                                                   "coefficients(se)", "Odds ratio(95%CI)", 
                                                   "coefficients(se)", "Odds ratio(95%CI)"),
                                 model.numbers = FALSE, 
                                 dep.var.labels = c("Water quality", "Affordability", 
                                                    "Accessibility", "TMF"), 
                                 ci=c(F,T, F, T, F, T, F, T), 
                                 star.cutoffs = c(0.05, 0.01, 0.001),
                                 p = c(p.valuesWQ1, p.values2, pvaluesAffordability1, p.values2,
                                       pvaluesAccesibility1, p.values2, pvaluesTMF1, p.values2),
                                 omit.stat = c("ll", "aic"),
                                 covariate.labels = c("Limited enfranchisement", "No enfranchisement",
                                                      "Population (log)", "Surface water",
                                                      "Purchased water"),
                                 omit = "Constant"))
                                #apply.ci = function(x) { 0 }))

cat(paste(gsub("\\(0.000, 0.000\\)", "", Compositestable), collapse = "\n"), "\n")

#moved to Rmarkdown stargazertables.rmd so i can render in HTML and take screenshots


