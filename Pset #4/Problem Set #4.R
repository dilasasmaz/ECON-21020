# Problem Set 4  ===============================================================
# Author: Dila Sasmaz

# This problem set uses Birthweight smoking dataset to do regression analysis.

# Preliminaries ================================================================
install.packages(c("sandwich", "lmtest"))
library(sandwich) 
library(lmtest)

# Data =========================================================================
# Import data from csv file
birthweight_smoking <- read.csv(file = "~/Documents/GitHub/ECON---21020/Pset #4/birthweight_smoking.csv", header = TRUE, sep = ";")

#Summary Statistics ============================================================

head(birthweight_smoking, 10)

# Part a =======================================================================

fit_lm <- lm(birthweight ~ smoker, data = birthweight_smoking)
summary(fit_lm)
coeftest(fit_lm, vcov = vcovHC(fit_lm, type = "HC1"))

# Part b =======================================================================

fit_lm2 <- lm(birthweight ~ smoker + alcohol + nprevist, data = birthweight_smoking)
summary(fit_lm2)
coeftest(fit_lm2, vcov = vcovHC(fit_lm, type = "HC1"))

# Part c =======================================================================
x1_hat <- lm(smoker ~  alcohol + nprevist, data = birthweight_smoking)$residuals

y_hat <- lm(birthweight ~  alcohol + nprevist, data = birthweight_smoking)$residuals

fit_lm3 <- lm(y_hat ~ x1_hat)
summary(fit_lm3)

coeftest(fit_lm3, vcov = vcovHC(fit_lm3, type = "HC1"))
