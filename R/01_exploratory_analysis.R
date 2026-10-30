## ---- SCATTERPLOT ANALYSIS ---- ##

library(tidyverse)
library(GGally)

# Adding Dataframe - in panel data format
library(readr)
oecd <- read.csv("data/OECD_Final_Variables.csv")

# Converting Tibble into a vector for each column in the df
oecd[1]

oecd[1] <- oecd[[1]]
oecd[2] <- oecd[[2]]
oecd[3] <- oecd[[3]]
oecd[4] <- oecd[[4]]
oecd[5] <- oecd[[5]]
oecd[6] <- oecd[[6]]
oecd[7] <- oecd[[7]]
oecd[8] <- oecd[[8]]
oecd[9] <- oecd[[9]]
oecd[10] <- oecd[[10]]

# Length of rows - Actual Length is 1200
nrow(oecd)

# Selecting rows with actual data
oecd <- oecd[1:1200,]

# Looking for NANs - 5 NANs in the original dataset
sum(is.na(oecd))

# Which variable has NaNs?
sum(is.na(oecd[,1]))
sum(is.na(oecd[,2]))
sum(is.na(oecd[,3]))
sum(is.na(oecd[,4]))
sum(is.na(oecd[,5]))
sum(is.na(oecd[,6]))
sum(is.na(oecd[,7])) 
sum(is.na(oecd[,8])) 
sum(is.na(oecd[,9])) # has NaN Values
sum(is.na(oecd[,10])) 

# Missing values are removed
oecd <- na.omit(oecd) # Rows with missing values are removed

# Shape of the dataset
nrow(oecd) # 1195 rows
ncol(oecd) # 10 variables
length(unique(oecd$Region)) # 239 cross-sectional units

# renaming region and year columns in new_var
oecd$region <- oecd$Region
oecd$year <- oecd$Year

# Dropping columns that are not required
oecd <- oecd[, -c(2,3)]

# Column Names
colnames(oecd)

# Scatterplot Matrix - no transformation (with jittering)
jittered_oecd_panel <- as.data.frame(lapply(oecd[, 3:8], jitter))

## Plotting
pairs(jittered_oecd_panel, 
      main = "Scatterplot Matrix - OECD Data, No Transformation (Jittered)",
      pch = 21,
)

#### ---- OUTLIER ANALYSIS ---- ####

boxplot(oecd[, 3], main= "Boxplot - Digital Skills") # no outliers, distribution seems approx symmetric
boxplot(oecd[, 7], main= "Boxplot - Employment in knowledge intensive activities") # no outliers, symmetric
boxplot(oecd[, 4], main= "Boxplot - Collaboration of Innovative SMEs with others") # no outliers, distribution seems approx symmetric
boxplot(oecd[, 5], main= "Boxplot - R&D Expenditure in Public Sector") # Contains extreme outlier
boxplot(oecd[, 6], main= "Boxplot - R&D Expenditure in Business Sector") # Long Right tail, not symmetric
boxplot(oecd[, 8], main= "Boxplot - PCT Patent Applications") # long right tail, not symmetric

# Looking at outlier in R&D expenditure public sector
max(oecd[, 5]) # max value is 9.69 which is too high

# Regions with this high value - Croatia
unique(oecd[oecd$`R&D expenditure in the public sector`>9,1])

# Croatia also has very high value for business expenditure
oecd[(oecd$Country == "HR - Croatia")&(oecd$`R&D expenditure in the business sector`>6),c(1,6)]
    ## Croatia appears to have a lot of inflated values, hence it should be removed as well

# Removing Croatia
oecd <- oecd[!(oecd$Country == "HR - Croatia"),]

# Bxplot of R&D Expenditure Public, outlier removed
boxplot(oecd[, 5], main= "Boxplot - R&D Expenditure in Public Sector (Outlier Removed)") # Long right tail

# Note: Other extreme values cannot be seen as outliers since they provide genuine information.
    ## Most variables appear to be right-skewed.
    
## Log transformations can be used, however it is yielding infinite values!
sum(is.infinite(log(oecd$`PCT patent applications`)))

# Hence, square root transformation can be more helpful.

#### ---- VARIABLE TRANSFORMATIONS ---- ####

## Visualising SQRT Transformation
boxplot(sqrt(oecd[, 3]), main= "Boxplot - Digital Skills^1/2") # Not ideal, generating outlier!
boxplot(sqrt(oecd[, 4]), main= "Boxplot - Collaboration of Innovative SMEs with others^1/2") # Not ideal, generating outlier!
boxplot(sqrt(oecd[, 8]), main= "Boxplot - Patent Applications^1/2") # Distribution is much better, though some outliers still exist!
boxplot(sqrt(oecd[, 7]), main= "Boxplot - Employment in K.I. activities^1/2") # Not ideal, generating outlier!
boxplot(sqrt(oecd[, 5]), main= "Boxplot - R&D Expenditure in Public Sector^1/2") # Distribution is much better, though some outliers still exist!
boxplot(sqrt(oecd[, 6]), main= "Boxplot - R&D Expenditure in Business Sector^1/2") # Distribution is much better, though some outliers still exist!

## Selectively applying SQRT Transformation, saved as new variables
oecd$sqrt.PCT.patent.applications <- sqrt(oecd[, 8])
oecd$sqrt.R.D.expenditure.in.the.public.sector <- sqrt(oecd[, 5])
oecd$sqrt.R.D.expenditure.in.the.business.sector <- sqrt(oecd[, 6]) 

# Column Names
colnames(oecd)

# Checking the classes
class(oecd$`Digital Skills`)
class(oecd$`Innovative SMEs collaborating with others`)
class(oecd$`Employment in knowledge-intensive activities`)
class(oecd$sqrt.PCT.patent.applications) # saved as df
class(oecd$sqrt.R.D.expenditure.in.the.public.sector) # saved as df
class(oecd$sqrt.R.D.expenditure.in.the.business.sector) # saved as df

# converting df columns to vectors
oecd[11] <- oecd[[11]]
oecd[12] <- oecd[[12]]
oecd[13] <- oecd[[13]]

## Visualising relationships after transformations
jittered_oecd_panel_trns <- as.data.frame(lapply(oecd[, c("Digital Skills",
                                                          "Innovative SMEs collaborating with others",
                                                          "Employment in knowledge-intensive activities",
                                                          "sqrt.PCT.patent.applications",
                                                          "sqrt.R.D.expenditure.in.the.public.sector",
                                                          "sqrt.R.D.expenditure.in.the.business.sector")], 
                                                 jitter))

## Plotting
pairs(jittered_oecd_panel_trns, 
      main = "Scatterplot Matrix - OECD Data, With Transformation (Jittered)",
      pch = 21,
)

#### ---- CORRELATION ANALYSIS ---- ####

## Dividing df, by year
oecd_2017 <- oecd[oecd$year == 2017, ]
oecd_2018 <- oecd[oecd$year == 2018, ]
oecd_2019 <- oecd[oecd$year == 2019, ]
oecd_2020 <- oecd[oecd$year == 2020, ]
oecd_2021 <- oecd[oecd$year == 2021, ]


## Plotting - Scatter (Yearwise)
ggpairs(oecd_2017[, c(11,3,4,7,12,13)], title = 'PairPlot with Correlation: OECD Data for Selected Variable (2017)')
ggpairs(oecd_2018[, c(11,3,4,7,12,13)], title = 'PairPlot with Correlation: OECD Data for Selected Variable (2018)')
ggpairs(oecd_2019[, c(11,3,4,7,12,13)], title = 'PairPlot with Correlation: OECD Data for Selected Variable (2019)')
ggpairs(oecd_2020[, c(11,3,4,7,12,13)], title = 'PairPlot with Correlation: OECD Data for Selected Variable (2020)')
ggpairs(oecd_2021[, c(11,3,4,7,12,13)], title = 'PairPlot with Correlation: OECD Data for Selected Variable (2021)')

###### Observation 1: The correlation coefficient seem to remain more or less the same, with passing year
###### Observation 2: This suggests not so significant time variant effect across all regions (panel data)
###### Observation 3: The correlation between explanatory variables does not appear very high (less chances of multicollinearity)


#### ---- EXPORTING THE DATASET ---- ####

# exporting oecd dataset 
saveRDS(oecd, file = "data/processed_panel_data_final.rds")

##--------------------------------------------- END ----------------------------------- ##