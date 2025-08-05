
#source: https://www.neonscience.org/resources/learning-hub/tutorials/eddy-data-intro
setwd("C:/Users/rbmahbub/Documents/RProjects/FluxdataR")
df <- read.csv("AMF_US-HRA_BASE_HH_2-5_dsep.csv")

## Site: US-HRA
## Version: 2-5
## AMF_US-HRA_BASE_HH_2-
# Install the packages
install.packages("tidyverse")
install.packages("lubridate")
devtools::install_github("tidyverse/lubridate")
install.packages("naniar")
install.packages("plantecophys")

#Loading the libraries
library(dplyr)
library(rafalib)
library(REddyProc)
library(tidyverse)
library(lubridate)
library(tidyr)
library(naniar)
library(plantecophys)
getwd()

#
head(df)
col(df)

View(df)


##VPD (kPa)
## TA (degree celsius)
esat <- function(TA){  
  esatval <- 0.6108* (exp(17.27 * TA/(237.3 + TA)))
  return(esatval)
}

RHtoVPD <- function(RH, TA){
  esatval <- esat(TA)
  e <- (RH/100) * (esatval)/1000
  VPD <- (esatval - e)
  return(VPD)
}

df<-df %>% 
  mutate(VPD = RHtoVPD(df$RH_1_1_1, df$Tair))

view(df)
plot(df$DateTime, df$VPD)


# recode 	-9999 to missing for variable v1
# select rows where v1 is 	-9999 and recode column v1
df$NEE[df$NEE==	-9999] <- NA 
df$VPD[df$VPD== -9999]<- NA
df$Tair[df$Tair== -9999]<- NA
df$Ustar[df$Ustar== -9999]<- NA
df$Rg[df$Rg== -9999]<- NA
view(df)

df<- df %>% drop_na(NEE)
df<- df %>% drop_na(VPD)
df<- df %>% drop_na(Tair)
df<- df %>% drop_na(Ustar)
df<- df %>% drop_na(Rg)

View(df)
# list rows of data that have missing values
df[!complete.cases(df),]


# create new dataset without missing data
df <- na.omit(df) 
View(df)


#plot
plot(df$NEE)

examplePath <- getExamplePath('AMF_US-HRA_BASE_HH_2-5.csv', TRUE)
EddyData.F <- fLoadTXTIntoDataframe(examplePath)


dates
col_dates <-ymd(df[,2])
print(df[,2])



df<-df %>% 
  mutate(DateTime = make_datetime(Year, Month, Day, Hour, Minute))


view(df)
EddyData <- filterLongRuns(df, "NEE")
df <- filterLongRuns(df, "NEE")
view(EddyData)


##VPD (kPa)
## TA (degree celsius)
esat <- function(TA){  
  esatval <- 0.6108* (exp(17.27 * TA/(237.3 + TA)))
  return(esatval)
}

RHtoVPD <- function(RH, TA){
  esatval <- esat(TA)
  e <- (RH/100) * (esatval)/1000
  VPD <- (esatval - e)
  return(VPD)
}

df<-df %>% 
  mutate(VPD = RHtoVPD(df$RH_1_1_1, df$Tair))

view(df)
plot(df$DateTime, df$VPD)

EProc <- sEddyProc$new(
  'USA-HRA', df, c('NEE','Rg','Tair','VPD', 'Ustar'))

EProc$sPlotFingerprintY('NEE', Year = 2016)
view(EProc$`.->sDATA`)

plot(df$DateTime, df$NEE)
df_nullemitted<- df %>% drop_na(NEE)
view(df_nullemitted)
plot(df_nullemitted$DateTime, df_nullemitted$CO2_1_1_1)

df_nullemitted <- df_nullemitted %>% replace_with_na(replace = list(CO2_1_1_1 = -9999.0000))
df_nullemitted<- df %>% drop_na(FC_1_1_1)
plot(df_nullemitted$DateTime, df_nullemitted$CO2_1_1_1)
min(df_nullemitted$CO2_1_1_1)

df_nullemitted <- df_nullemitted %>% replace_with_na(replace = list(CO2_1_1_1 = -9999))
df_nullemitted<- df_nullemitted %>% drop_na(FC_1_1_1)
plot(df_nullemitted$DateTime, df_nullemitted$CO2_1_1_1)

df_NA<- df %>% replace_with_na(replace = list(CO2_1_1_1 = -9999.0000))
df_NA<- df_NA %>% replace_with_na(replace = list(CO2_1_1_1 = -9999.0000))


EProc <- sEddyProc$new(
  'USA-HRA', df_NA, c('FC_1_1_1','G','TA_1_1_1', 'USTAR'))
par(mar=c(0.1,0.1,0.1,0.1))
EProc$sPlotFingerprintY('FC_1_1_1', Year = 2016)


#A fingerprint-plot of the source half-hourly shows already several gaps. A fingerprint-plot is a color-coded image of the half-hourly fluxes by daytime on the x and and day of the year on the y axis.
EProc$sPlotFingerprintY('NEE', Year = 2016)
EProc$sPlotDiurnalCycle('NEE')
warning()

####SECOND STEP######
#The second step, is the estimation of the distribution of uStar thresholds, to identify periods of low friction velocity (uStar), where NEE is biased low. Discarding periods with low uStar is one of the largest sources of uncertainty in aggregated fluxes. Hence, several quantiles of the distribution of the uncertain uStar threshold are estimated by a bootstrap.
#The friction velocity, uStar, needs to be in column named "Ustar" of the input dataset.
EProc$sEstimateUstarScenarios(
  nSample = 100L, probs = c(0.05, 0.5, 0.95))

  
view(EProc$sDATA)

EProc$sGetEstimatedUstarThresholdDistribution()

EProc$sGetUstarScenarios()

View(EProc$sDATA)

####THIRD STEP######
EProc$sMDSGapFillUStarScens('NEE')

grep("NEE_.*_f$",names(EProc$sExportResults()), value = TRUE, Legend.b = TRUE)
grep("NEE_.*_fsd$",names(EProc$sExportResults()), value = TRUE)


EProc$sPlotFingerprintY('NEE_U50_f', Year = 1998)


EProc$sSetLocationInfo(LatDeg = 51.0, LongDeg = 13.6, TimeZoneHour = 1)  
EProc$sMDSGapFill('Tair', FillAll = FALSE,  minNWarnRunLength = NA)     
EProc$sMDSGapFill('VPD', FillAll = FALSE,  minNWarnRunLength = NA) 


EProc$sMRFluxPartitionUStarScens()

grep("GPP.*_f$|Reco",names(EProc$sExportResults()), value = TRUE)

EProc$sPlotFingerprintY('GPP_U50_f', Year = 1998)

FilledEddyData <- EProc$sExportResults()
uStarSuffixes <- colnames(EProc$sGetUstarScenarios())[-1]
#suffix <- uStarSuffixes[2]
GPPAggCO2 <- sapply( uStarSuffixes, function(suffix) {
  GPPHalfHour <- FilledEddyData[[paste0("GPP_",suffix,"_f")]]
  mean(GPPHalfHour, na.rm = TRUE)
})
molarMass <- 12.011
GPPAgg <- GPPAggCO2 * 1e-6 * molarMass * 3600*24*365.25
print(GPPAgg)


(max(GPPAgg) - min(GPPAgg)) / median(GPPAgg) 

EProc$sEstimateUstarScenarios( 
  nSample = 200, probs = seq(0.025,0.975,length.out = 39) )


FilledEddyData <- EProc$sExportResults()
CombinedData <- cbind(EddyData, FilledEddyData)
fWriteDataframeToFile(CombinedData, 'DE-Tha-Results.txt', Dir = tempdir())
write.csv(CombinedData, file = "DE-Tha-Results.csv")
getwd()

EProc$sMRFluxPartitionUStarScens()

grep("GPP.*_f$|Reco",names(EProc$sExportResults()), value = TRUE)

EProc$sPlotFingerprintY('GPP_U50_f', Year = 1998)

