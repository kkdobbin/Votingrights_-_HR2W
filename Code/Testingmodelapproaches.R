#test

Data$EXTREME_WATER_BILL_RAW_SCORE_TEST <- Data$EXTREME_WATER_BILL_RAW_SCORE
Data$EXTREME_WATER_BILL_RAW_SCORE_TEST <- as.ordered(Data$EXTREME_WATER_BILL_RAW_SCORE_TEST)

#as numeric, 0s .5s and 1s
Extreme_waterbill <- lm(EXTREME_WATER_BILL_RAW_SCORE ~ enfranchisement_final + LN_POP + Source + Purchased, Data_noMHP)
summary(Extreme_waterbill)

Plot12 <- plot_model(Extreme_waterbill, type = "pred", terms = "enfranchisement_final", axis.title = c("Enfranchisement", "Predicted extreme water bill raw score"), title = " ") + theme_sjplot(base_size = 10); Plot12

#as factor
Extreme_waterbill_TEST <- glm(EXTREME_WATER_BILL_RAW_SCORE ~ enfranchisement_final + LN_POP + Source + Purchased, data = Data_noMHP, family = binomial(link = "logit"))
summary(Extreme_waterbill_TEST)

#not sure about link, https://stats.stackexchange.com/questions/225211/which-glm-family-to-use-for-ordinal-dv says polr better, https://pubmed.ncbi.nlm.nih.gov/9447413/

library(MASS)
Test <- polr(EXTREME_WATER_BILL_RAW_SCORE_TEST ~ enfranchisement_final + LN_POP + Source + Purchased, data = Data, method = "logistic" , Hess = TRUE)
summary(Test)

#visualization
#https://library.virginia.edu/data/articles/visualizing-the-effects-of-logistic-regression
#https://library.virginia.edu/data/articles/visualizing-the-effects-of-proportional-odds-logistic-regression
# https://bookdown.org/josiesmith/labbook/logistic-regression.html
