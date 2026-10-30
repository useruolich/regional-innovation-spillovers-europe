#### ---- Panel Regression ---- ####

library(tidyverse)
library(plm)
oecd_panel_data <- readRDS("data/processed_panel_data_final.rds")

# Filtering the dataset to only use relevant explanatory variables
oecd_panel_data <- oecd_panel_data[,c("sqrt.PCT.patent.applications", 
                                      "sqrt.R.D.expenditure.in.the.business.sector",
                                      "sqrt.R.D.expenditure.in.the.public.sector",
                                      "Digital Skills",
                                      "Innovative SMEs collaborating with others",
                                      "Employment in knowledge-intensive activities",
                                      "year",
                                      "region",
                                      "status")]

# Declare the panel structure
pdata <- pdata.frame(oecd_panel_data, index = c("region", "year"))

## Performing first-differencing

# Run first-difference model (Oneway: only time invariant hetrogeneity is differenced out)
model_fd <- plm(
  sqrt.PCT.patent.applications ~ 
    sqrt.R.D.expenditure.in.the.public.sector +
    sqrt.R.D.expenditure.in.the.business.sector +
    Innovative.SMEs.collaborating.with.others +
    Employment.in.knowledge.intensive.activities +
    Digital.Skills,  
  data = pdata,
  model = "fd"
)

summary(model_fd) # Model is statistically insignificant!

# Run "within" estimator model - FE (oneway)
model_within_one <- plm(
  sqrt.PCT.patent.applications ~ 
    sqrt.R.D.expenditure.in.the.public.sector +
    sqrt.R.D.expenditure.in.the.business.sector +
    Innovative.SMEs.collaborating.with.others +
    Employment.in.knowledge.intensive.activities +
    Digital.Skills,  
  data = pdata,
  model = "within",
  effect = "individual"
)

summary(model_within_one)

# Run "within" estimator model - FE (twoways)
model_within_two <- plm(
  sqrt.PCT.patent.applications ~ 
    sqrt.R.D.expenditure.in.the.public.sector +
    sqrt.R.D.expenditure.in.the.business.sector +
    Innovative.SMEs.collaborating.with.others +
    Employment.in.knowledge.intensive.activities +
    Digital.Skills,  
  data = pdata,
  model = "within",
  effect = "twoways"
)

summary(model_within_two)

# Run "between" estimator model - RE (oneway)
model_between_one <- plm(
  sqrt.PCT.patent.applications ~ 
    sqrt.R.D.expenditure.in.the.public.sector +
    sqrt.R.D.expenditure.in.the.business.sector +
    Innovative.SMEs.collaborating.with.others +
    Employment.in.knowledge.intensive.activities +
    Digital.Skills,  
  data = pdata,
  model = "between",
  effect = "individual"
  )

summary(model_between_one)