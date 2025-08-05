#REddyProc typical workflow
#source: https://cran.r-project.org/web/packages/REddyProc/REddyProc.pdf
#github source: https://github.com/bgctw/REddyProc/blob/master/vignettes/useCase.md
install.packages("REddyProc")



####FIRST STEP######
#Importing the half-hourly data
#The workflow starts with importing the half-hourly data. The example, reads a text file with data of the year 1998 from the Tharandt site and converts the separate decimal columns year, day, and hour to a POSIX timestamp column. Next, it initializes the sEddyProc class.
#+++ load libraries used in this vignette
library(REddyProc)
library(dplyr)
library(rafalib)

#+++ Load data with 1 header and 1 unit row from (tab-delimited) text file
fileName <- getExamplePath('Example_DETha98.txt', isTryDownload = TRUE)
EddyData <- if (length(fileName)) fLoadTXTIntoDataframe(fileName) else
  # or use example dataset in RData format provided with REddyProc
  Example_DETha98
View(EddyData)
#+++ Replace long runs of equal NEE values by NA
EddyData <- filterLongRuns(EddyData, "NEE")

#+++ Add time stamp in POSIX time format

EddyDataWithPosix <- fConvertTimeToPosix(
  EddyData, 'YDH',Year = 'Year',Day = 'DoY', Hour = 'Hour') %>% 
  filterLongRuns("NEE")
#+++ Initalize R5 reference class sEddyProc for post-processing of eddy data
#+++ with the variables needed for post-processing later

EProc <- sEddyProc$new(
  'DE-Tha', EddyDataWithPosix, c('NEE','Rg','Tair','VPD', 'Ustar'))
View(sEddyProc)
#A fingerprint-plot of the source half-hourly shows already several gaps. A fingerprint-plot is a color-coded image of the half-hourly fluxes by daytime on the x and and day of the year on the y axis.
EProc$sPlotFingerprintY('NEE', Year = 1998)
EProc$sPlotDiurnalCycle('NEE')



####SECOND STEP######
#The second step, is the estimation of the distribution of uStar thresholds, to identify periods of low friction velocity (uStar), where NEE is biased low. Discarding periods with low uStar is one of the largest sources of uncertainty in aggregated fluxes. Hence, several quantiles of the distribution of the uncertain uStar threshold are estimated by a bootstrap.
#The friction velocity, uStar, needs to be in column named "Ustar" of the input dataset.
EProc$sEstimateUstarScenarios(
  nSample = 100L, probs = c(0.05, 0.5, 0.95))

EProc$sGetEstimatedUstarThresholdDistribution()

EProc$sGetUstarScenarios()

View(EProc)

#The output reports annually aggregated uStar estimates of 0.42 for the original data and 0.38, 0.45, 0.62 for lower, median, and upper quantile of the estimated distribution. The threshold can vary between periods of different surface roughness, e.g. before and after harvest. Therefore, there are estimates for different time periods, called seasons. These season-estimates are by default aggregated to entire years.
#The subsequent post processing steps will be repeated using the four u* threshold scenarios (non-resampled and tree quantiles of the bootstrapped distribution). They require to specify a u*-threshold for each season and a suffix to distinguish the outputs related to different thresholds. By default the annually aggregated estimates are used for each season within the year.


####THIRD STEP######
EProc$sMDSGapFillUStarScens('NEE')

grep("NEE_.*_f$",names(EProc$sExportResults()), value = TRUE)
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

