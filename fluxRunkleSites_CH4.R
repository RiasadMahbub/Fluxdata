#Script: fluxRunkleSites_CH4.R
#Date: April 6, 2021
#Run: Office PC
#We use ReddyProc 
#This script contains the CH4 flux calculations following u* star threshold and moving test 
#following Gu et al., 2005; Papale et al., 2006 and  Nemitz et al. (2018)

setwd("C:/Users/rbmahbub/Documents/RProjects/FluxdataR")



#Loading the libraries
library(dplyr)
library(rafalib)
library(REddyProc)
library(tidyverse)
library(lubridate)
library(tidyr)
library(naniar)
library(plantecophys)
df <- read.csv("AMF_US-HRA_BASE_HH_2-5_dsep.csv")

#Datetime
df<-df %>% 
  mutate(DateTime = make_datetime(Year, Month, Day, Hour, Minute))

# recode 	-9999 to missing for variable v1
# select rows where v1 is 	-9999 and recode column v1
df$FCH4[df$FCH4==	-9999] <- NA 

# list rows of data that have missing values
df[!complete.cases(df),]

# create new dataset without missing data
df <- na.omit(df) 

View(df)

plot(df$DateTime, df$FCH4)
df$sPlotFingerprintY('FCH4', Year = 2016)

EProc <- sEddyProc$new(
  'USA-HRA', df, c('FCH4','SW_IN','TA_1_1_1', 'RH', 'USTAR'))
EProc$sPlotFingerprintY('FCH4', Year = 2016)

View(EProc$sDATA)

####SECOND STEP######
#The second step, is the estimation of the distribution of uStar thresholds, to identify periods of low friction velocity (uStar), where NEE is biased low. Discarding periods with low uStar is one of the largest sources of uncertainty in aggregated fluxes. Hence, several quantiles of the distribution of the uncertain uStar threshold are estimated by a bootstrap.
#The friction velocity, uStar, needs to be in column named "Ustar" of the input dataset.
EProc$sEstimateUstarScenarios(
  nSample = 100L, probs = c(0.05, 0.5, 0.95))

EProc$sGetEstimatedUstarThresholdDistribution()

EProc$sGetUstarScenarios()

View(EProc$sDATA)



