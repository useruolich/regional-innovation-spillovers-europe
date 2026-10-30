#### ---- Pooled OLS Regression ---- ####
library(tidyverse)
library(car)
oecd_panel_data <- readRDS("data/processed_panel_data_final.rds")

# Based on literature found, the following variables work as causal variables or enablers, that can explain patent activity in a region:

# R&D expenditure in business (causal)
# R&D expenditure in public (causal)
# Digital Skills (enabler, human capital)
# Collaboration of innovative SMEs (enabler, indirect)
# Employment share in knowledge intensive services (indirect and weak link, structural indicator)


# Running Regression - with R&D expenditure only
pooled_lm <- 
  lm(sqrt.PCT.patent.applications ~ 
       sqrt.R.D.expenditure.in.the.public.sector +
       sqrt.R.D.expenditure.in.the.business.sector,
     data = oecd_panel_data
  )

summary(pooled_lm) 
# all variables are significant (5% lvl) and behaving as expected.

# Running Regression - with expenditure and human capital info
pooled_lm_exp_hc <- 
  lm(sqrt.PCT.patent.applications ~ 
       sqrt.R.D.expenditure.in.the.public.sector +
       sqrt.R.D.expenditure.in.the.business.sector +
       `Digital Skills` +
       `Employment in knowledge-intensive activities`,
     data = oecd_panel_data
  )

summary(pooled_lm_exp_hc) 

# Coefficients are behaving as expected
# All, except employment in KI activities, are singificant (at 5% lvl  of sig.)
# Adjusted R2 score has slighty increased


# Regression model with all relevant explanatory variables included
pooled_lm_final <- 
  lm(sqrt.PCT.patent.applications ~ 
       sqrt.R.D.expenditure.in.the.business.sector +
       sqrt.R.D.expenditure.in.the.public.sector +
       `Digital Skills` +
       `Employment in knowledge-intensive activities` +
       `Innovative SMEs collaborating with others`,
     data = oecd_panel_data
  )

summary(pooled_lm_final) 
# Employment in KIS is very broad category and only the R&D services part of it generates patents.
# All coefficients are behaving as expected
# adj R2 score improved very very slightly
# coeff for emp in KI acts and inn. SME collab are insignificant
# Model itself is significant from the F-statistic and corresponding p-value.

#### ---- Pooled OLS Regression: with "year" dummies ---- ####
pooled_lm_year_dummies <- 
  lm(sqrt.PCT.patent.applications ~ 
       sqrt.R.D.expenditure.in.the.business.sector +
       sqrt.R.D.expenditure.in.the.public.sector +
       `Digital Skills` +
       `Employment in knowledge-intensive activities` +
       `Innovative SMEs collaborating with others` +
       factor(year),
     data = oecd_panel_data
  )

summary(pooled_lm_year_dummies) 
# Adj R2 improved negligibly
# coeff behave as expected
# coeff for 2020 and 2021 dummies are significant (base: 2017)
# coeff for innovative SMEs collaborating became significant (compared to case without dummies)
# coeff for emp in KI acts is still insignificant
# From F-stat, model itself is significant

#### ---- Regression Diagnostics ---- ####

# Residual plot analysis
plot(pooled_lm_year_dummies)
# Findings:
# Mean of the residuals appears to be close to 0.
# The variance of residuals does not appear constant, especially in the middle and right tail.
# Influential points are points with high leverage and large residual value.
# Points 207 and 208 are the two influential points here (having a disproportionate effect on regression model).
# In q-q residuals plot most of the points lie on 45° line i.e. approximately normal, with deviation in right tail (again indicating influential points in this region).
# From the scale location plot and qq plot, points 751, 752, 753 appear to have very high deviation from mean (however, there are additional points with similar deviation). 

# VIF for Multicollinearity
library(car)
vif(pooled_lm_year_dummies) # acceptable since GVIF adjusted for dof < 2

## Core Assumptions:
#1. The error must not be correlated with x, meaning E(u|x) = E(u); there must be independence of error dist. with X (satisfied!)
#2. The conditional variance of errors with x must be constant and independent of x (not satisfied as variance is not constant!).
#3. No perfect collinearity in regressors (satisfied!)

# 1. Testing for heteroskedasticity
library(lmtest)
bptest(pooled_lm_year_dummies) # Breusch pagan test | H0: Homoscedasticity
## Finding: there is heteroskedasticity, meaning that p-values and confidence intervals are not reliable!

# 2. Hetroskedasticity Robust Testing
library(sandwich)
library(lmtest)
coeftest(pooled_lm_year_dummies, vcov = vcovHC(pooled_lm_year_dummies, type = "HC1")) # tested with robust standard errors
## Finding: 
# results are same as with the scenario with non-robust std errors.
# non-significant coefficients for emp knowledge intensive acts and years 2018 and 2019.

# Identifying the relationship between explanatory variables and the dependent variable
## Scatterplot with LOESS curve: R&D Expenditure (Public)
plot(oecd_panel_data$sqrt.R.D.expenditure.in.the.public.sector, oecd_panel_data$sqrt.PCT.patent.applications, main = "Relationship Assessment For Functional Form",
     xlab = "R&D Expenditure (Public)^1/2", ylab = "PCT Patent Applications^1/2", pch = 19, col = "darkgrey")
lines(lowess(oecd_panel_data$sqrt.R.D.expenditure.in.the.public.sector, oecd_panel_data$sqrt.PCT.patent.applications), col = "blue", lwd = 2)
abline(lm(oecd_panel_data$sqrt.PCT.patent.applications ~ oecd_panel_data$sqrt.R.D.expenditure.in.the.public.sector), col = "red", lwd = 2)
legend("topleft", legend = c("Regression line", "LOESS curve"),
       col = c("red", "blue"), lwd = 2, bty = "n")

## Scatterplot with LOESS curve: R&D Expenditure (Business)
plot(oecd_panel_data$sqrt.R.D.expenditure.in.the.business.sector, oecd_panel_data$sqrt.PCT.patent.applications, main = "Relationship Assessment For Functional Form",
     xlab = "R&D Expenditure (Business)^1/2", ylab = "PCT Patent Applications^1/2", pch = 19, col = "darkgrey")
lines(lowess(oecd_panel_data$sqrt.R.D.expenditure.in.the.business.sector, oecd_panel_data$sqrt.PCT.patent.applications), col = "blue", lwd = 2)
abline(lm(oecd_panel_data$sqrt.PCT.patent.applications ~ oecd_panel_data$sqrt.R.D.expenditure.in.the.business.sector), col = "red", lwd = 2)
legend("topleft", legend = c("Regression line", "LOESS curve"),
       col = c("red", "blue"), lwd = 2, bty = "n")

## Scatterplot with LOESS curve: Digital Skills
plot(oecd_panel_data$`Digital Skills`, oecd_panel_data$sqrt.PCT.patent.applications, main = "Relationship Assessment For Functional Form",
     xlab = "Digital Skills", ylab = "PCT Patent Applications^1/2", pch = 19, col = "darkgrey")
lines(lowess(oecd_panel_data$`Digital Skills`, oecd_panel_data$sqrt.PCT.patent.applications), col = "blue", lwd = 2)
abline(lm(oecd_panel_data$sqrt.PCT.patent.applications ~ oecd_panel_data$`Digital Skills`), col = "red", lwd = 2)
legend("topleft", legend = c("Regression line", "LOESS curve"),
       col = c("red", "blue"), lwd = 2, bty = "n")

## Scatterplot with LOESS curve: Collaboration of Innovative SMEs
plot(oecd_panel_data$`Innovative SMEs collaborating with others`, oecd_panel_data$sqrt.PCT.patent.applications, main = "Relationship Assessment For Functional Form",
     xlab = "Innovative SMEs collaborating with others", ylab = "PCT Patent Applications^1/2", pch = 19, col = "darkgrey")
lines(lowess(oecd_panel_data$`Innovative SMEs collaborating with others`, oecd_panel_data$sqrt.PCT.patent.applications), col = "blue", lwd = 2)
abline(lm(oecd_panel_data$sqrt.PCT.patent.applications ~ oecd_panel_data$`Innovative SMEs collaborating with others`), col = "red", lwd = 2)
legend("topleft", legend = c("Regression line", "LOESS curve"),
       col = c("red", "blue"), lwd = 2, bty = "n")

## Scatterplot with LOESS curve: Employment in KI acts
plot(oecd_panel_data$`Employment in knowledge-intensive activities`, oecd_panel_data$sqrt.PCT.patent.applications, main = "Relationship Assessment For Functional Form",
     xlab = "Employment in Knowledge-Intensive Activities", ylab = "PCT Patent Applications^1/2", pch = 19, col = "darkgrey")
lines(lowess(oecd_panel_data$`Employment in knowledge-intensive activities`, oecd_panel_data$sqrt.PCT.patent.applications), col = "blue", lwd = 2)
abline(lm(oecd_panel_data$sqrt.PCT.patent.applications ~ oecd_panel_data$`Employment in knowledge-intensive activities`), col = "red", lwd = 2)
legend("topleft", legend = c("Regression line", "LOESS curve"),
       col = c("red", "blue"), lwd = 2, bty = "n")

## Finding: All explanatory variables roughly have a linear relationship with the dependent variable.

## -- Robust Regression: Downweighting the influential points -- ##

# creating weights
weights <- rep(1, nrow(oecd_panel_data))
ip_idx <- c(207, 208, 751, 752, 753) # influential points + points that significantly deviate from mean. 
weights[ip_idx] <- 0.1   # new weight (default is "1")

# Regression with customised weights
pooled_lm_year_dummies_weighted <- 
  lm(sqrt.PCT.patent.applications ~ 
       sqrt.R.D.expenditure.in.the.business.sector +
       sqrt.R.D.expenditure.in.the.public.sector +
       `Digital Skills` +
       `Employment in knowledge-intensive activities` +
       `Innovative SMEs collaborating with others` +
       factor(year), 
     data = oecd_panel_data,
     weights = weights)

summary(pooled_lm_year_dummies_weighted)
# Findings:
# R2 score has improved
# coefficients changed slightly for all explanatory variables!

# Residual plot
plot(pooled_lm_year_dummies_weighted)
# Findings:
# qqplot appears to have improved slightly, however residual variance is still not constant

# Hetroskedasticity Robust Testing
coeftest(pooled_lm_year_dummies_weighted, vcov = vcovHC(pooled_lm_year_dummies_weighted, type = "HC1")) # tested with robust standard errors
# Findings:
# statistical significance of most coefficients remains same, except categorical factor for 2020.
