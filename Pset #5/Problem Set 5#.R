# Problem Set 5  ===============================================================
# Author: Dila Sasmaz


# Preliminaries ================================================================
library(sandwich) 
library(lmtest)
library(AER)

# Problem 4 ====================================================================
# Data =========================================================================
# Import data from csv file
income_determinants <- read.csv(file = "~/Documents/GitHub/ECON---21020/Pset #5/cps04.csv",
                                header = TRUE, sep = ";")

# Summary Statistics ===========================================================
head(income_determinants, 10)

# Problem 4 - Part a(ii) =======================================================
# Run the initial regression
fit_lm <- lm(AHE ~ Female + College + HS, data = income_determinants)
summary(fit_lm)

# Problem 4 - Part a(iii) ======================================================
# Construct a hypothesis test, H0 : β2 = β3 versus H1 : β2 > β3

c <- 0
r <- c(0, 0, 1, -1)
linearHypothesis(fit_lm, r, c, vcov = vcovHC(fit_lm, type = "HC1"),
                 test = "Chisq")
sqrt(linearHypothesis(fit_lm, r, c, vcov = vcovHC(fit_lm, type = "HC1"),
                      test = "Chisq")$Chisq[2])

# Problem 4 - Part b(ii) =======================================================
# Add a new column total_school 
income_determinants$total_school <- 
  income_determinants$College + income_determinants$HS

# Run a second regression with the new data frame 

fit_lm2 <- lm(AHE ~ Female + College + total_school, data = income_determinants)
summary(fit_lm2)
# Construct a hypothesis test, H0 : γ2 = 0 versus H1: γ2 > 0
c <- 0
r <- c(0, 0, 1, 0)
linearHypothesis(fit_lm2, r, c, vcov = vcovHC(fit_lm2, type = "HC1"),
                 test = "Chisq")
sqrt(linearHypothesis(fit_lm2, r, c, vcov = vcovHC(fit_lm2, type = "HC1"),
                      test = "Chisq")$Chisq[2])
# Problem 4 - Part c ===========================================================
# Add two new interaction columns fem_col and fem_hs
income_determinants$fem_col <- 
  income_determinants$College * income_determinants$Female
income_determinants$fem_hs <- 
  income_determinants$HS * income_determinants$Female

# Run a third regression with the new data frame 
fit_lm3 <- lm(AHE ~ Female + College + total_school + fem_col + fem_hs, 
              data = income_determinants)
summary(fit_lm3)

# Construct a hypothesis test, H0 : H0 :β3 = 0 and β5 = 0 versus 
# H1 : β3 != 0 and β5 != 0

c_2 <- c(0, 0)
R <- rbind(c(0, 0, 0, 0, 1, 0),
           c(0, 0, 0, 0, 0, 1))
linearHypothesis(fit_lm3, R, c_2, vcov = vcovHC(fit_lm3, type = "HC1"),
                 test = "Chisq")
sqrt(linearHypothesis(fit_lm3, R, c_2, vcov = vcovHC(fit_lm3, type = "HC1"),
                     test = "Chisq")$Chisq[2])
# Problem 5 ====================================================================
# Data =========================================================================
# Import data from csv file
fertility_labor <- read.csv(file = "~/Documents/GitHub/ECON---21020/Pset #5/fertility.csv",
                                header = TRUE, sep = ";")
head(fertility_labor, 10)
# Problem 5 - Part a  ==========================================================
# Run the regression
ls_fit<- lm(weeksm1 ~ morekids, data = fertility_labor)
summary(ls_fit)
coeftest(ls_fit, vcov = vcovHC(ls_fit , type = "HC1"))
# Problem 5 - Part c  ==========================================================
# Run the regression
ls_fit2<- lm(morekids ~ samesex, data = fertility_labor)
summary(ls_fit2)
coeftest(ls_fit2, vcov = vcovHC(ls_fit2 , type = "HC1"))

# Problem 5 - Part e  ==========================================================
# Run TSLS regression
tsls_fit <- ivreg(weeksm1 ~ morekids | samesex , data = fertility_labor)
coeftest(tsls_fit, vcov = vcovHC(tsls_fit, type = "HC1"))

# Problem 5 - Part f  ==========================================================
ls_fit3<- lm(weeksm1 ~ morekids + agem1 + black + hispan + othrace , 
             data = fertility_labor)
summary(ls_fit3)
coeftest(ls_fit3, vcov = vcovHC(ls_fit2 , type = "HC1"))

tsls_fit2 <- ivreg(weeksm1 ~ morekids + agem1 + black + hispan + othrace | 
                     samesex + agem1 + black + hispan + othrace , 
                   data = fertility_labor)
coeftest(tsls_fit2, vcov = vcovHC(tsls_fit, type = "HC1"))

