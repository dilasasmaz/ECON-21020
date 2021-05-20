# Problem Set 5  ===============================================================
# Author: Dila Sasmaz


# Preliminaries ================================================================
library(sandwich) 
library(lmtest)
library(AER)
# Data =========================================================================
# Import data from csv file
income_determinants <- read.csv(file = "~/Documents/GitHub/ECON---21020/Pset #5/cps04.csv", header = TRUE, sep = ";")

#Summary Statistics ============================================================

head(income_determinants, 10)
fit_lm <- lm(AHE ~ Female + College + HS, data = income_determinants)
summary(fit_lm)
vcov(fit_lm)
c <- 0
r <- c(0, 0, 1, -1)
linearHypothesis(fit_lm, r, c, vcov = vcovHC(fit_lm, type = "HC1"),test = "Chisq")
sqrt(linearHypothesis(fit_lm, r, c, vcov = vcovHC(fit_lm, type = "HC1"),test = "Chisq")$Chisq[2])

income_determinants$total_school <- income_determinants$College + income_determinants$HS
head(income_determinants, 10)
fit_lm2 <- lm(AHE ~ Female + College + total_school, data = income_determinants)
summary(fit_lm2)

linearHypothesis(fit_lm2, c("College=0"), vcov = vcovHC(fit_lm2, type = "HC1"),test = "Chisq")
sqrt(linearHypothesis(fit_lm2, c("College=0"),vcov = vcovHC(fit_lm2, type = "HC1"), test = "Chisq")$Chisq[2])
                 