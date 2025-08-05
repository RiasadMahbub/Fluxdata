# ==============================
# LOAD REQUIRED LIBRARIES
# ==============================
library(REddyProc)
library(bigleaf)
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)

# Description of Key Variables in the Dataset for reddyproc
# Year: The year of the measurement.
# DoY: Day of the Year (usually ranging from 1 to 365 or 366, depending on the year).
# Hour: The hour of the measurement (typically in 24-hour format).
# NEE: Net Ecosystem Exchange (the flux measurement of carbon dioxide in micromoles of CO2 per square meter per second).
# Rg: Global radiation or incoming solar radiation (in W/m²).
# Tair: Air temperature (in °C).
# RH_1_1_1: Relative humidity (in %).
# Ustar: Friction velocity (a measure of turbulence in the atmosphere, typically in m/s).

# ==============================
# SITE INFORMATION
# ==============================
LatDeg <- 34.585  # Way3
LonDeg <- -91.751
TimeZoneHour <- -6

# ==============================
# DATA LOADING
# ==============================
# Define data directories
way3_dir <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/OutputLocalProcessedData_AFguidedSubmitted/ForLab/Way3"
way4_dir <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/OutputLocalProcessedData_AFguidedSubmitted/ForLab/Way4"

# List and read files
way3_data <- lapply(list.files(way3_dir, full.names = TRUE), read_csv)
way4_data <- lapply(list.files(way4_dir, full.names = TRUE), read_csv)

# Replace -9999 with NaN
replace_missing <- function(df) {
  df[df == -9999] <- NaN
  return(df)
}
way3_data <- lapply(way3_data, replace_missing)
way4_data <- lapply(way4_data, replace_missing)

# ==============================
# TIMESTAMP CONVERSION
# ==============================
convert_timestamp <- function(df) {
  df$TIMESTAMP_START <- as.POSIXct(as.character(df$TIMESTAMP_START), format = "%Y%m%d%H%M")
  df$Year <- year(df$TIMESTAMP_START)
  df$DoY <- yday(df$TIMESTAMP_START)
  df$Hour <- hour(df$TIMESTAMP_START)
  return(df)
}

way3_data <- lapply(way3_data, convert_timestamp)
way4_data <- lapply(way4_data, convert_timestamp)

# ==============================
# PLANTING AND HARVESTING DATES
# ==============================
# Planting and Harvesting Dates by Year
dates_by_year <- list(
  "2015" = list(way3 = c("2015-04-08", "2015-08-19"), way4 = c("2015-04-07", "2015-08-19")),
  "2016" = list(way3 = c("2016-04-23", "2016-09-13"), way4 = c("2016-04-23", "2016-09-13")),
  "2017" = list(way3 = c("2017-04-10", "2017-08-27"), way4 = c("2017-04-09", "2017-08-27")),
  "2018" = list(way3 = c("2018-04-30", "2018-09-15"), way4 = c("2018-04-30", "2018-08-31")),
  "2019" = list(way3 = c("2019-05-13", "2019-09-12"), way4 = c("2019-05-13", "2019-09-12")),
  "2020" = list(way3 = c("2020-04-02", "2020-08-19"), way4 = c("2020-04-02", "2020-08-18")),
  "2021" = list(way3 = c("2021-05-01", "2021-09-28"), way4 = c("2021-05-01", "2021-09-27")),
  "2022" = list(way3 = c("2022-04-29", "2022-09-05"), way4 = c("2022-04-28", "2022-09-05")),
  "2023" = list(way3 = c("2023-04-14", "2023-08-26"), way4 = c("2023-04-12", "2023-08-23")),
  "2024" = list(way3 = c("2024-04-03", "2024-08-16"), way4 = c("2024-04-03", "2024-08-16"))
)

# ==============================
# CONVERT TO DOY
# ==============================
calculate_doy <- function(date_str) yday(as.Date(date_str))
# Generate DOY list
doy_by_year <- lapply(dates_by_year, function(year_data) {
  list(
    way3 = sapply(year_data$way3, calculate_doy),
    way4 = sapply(year_data$way4, calculate_doy)
  )
})

# Should now print years like "2018", "2019", etc.
# List of start (PD) and end (HD) dates for each year
pd_dates_way3 <- list(way32018PD, way32019PD, way32020PD, way32021PD, way32022PD, way32023PD, way32024PD)
hd_dates_way3 <- list(way32018HD, way32019HD, way32020HD, way32021HD, way32022HD, way32023HD, way32024HD)

pd_dates_way4 <- list(way42018PD, way42019PD, way42020PD, way42021PD, way42022PD, way42023PD, way42024PD)
hd_dates_way4 <- list(way42018HD, way42019HD, way42020HD, way42021HD, way42022HD, way42023HD, way42024HD)

# Function to filter data
filter_by_dates <- function(data_list, pd_dates, hd_dates) {
  lapply(seq_along(data_list), function(i) {
    subset(data_list[[i]], as.Date(TIMESTAMP_START) >= pd_dates[[i]] & as.Date(TIMESTAMP_START) <= hd_dates[[i]])
  })
}
# Apply filtering
way3_data <- filter_by_dates(way3_data, pd_dates_way3, hd_dates_way3)
way4_data <- filter_by_dates(way4_data, pd_dates_way4, hd_dates_way4)


# ==============================
# 6. DATA FORMATTING FOR REDDYPROC
# ==============================
# Create new list for way3_data_reddyproc by selecting specific columns
select_and_rename_columns <- function(df) {
  # Select the columns of interest
  df <- df %>%
    select(Year, DoY, FC_1_1_1, SW_IN, TA_1_1_1, RH_1_1_1, USTAR, VPD, TIMESTAMP_START, WD)  # Selecting NEE, Rg, Tair, rH, Ustar
  # Rename the specific columns
  df <- df %>%
    rename(
      NEE = FC_1_1_1,           # Rename FC_1_1_1 to NEE
      Rg = SW_IN,         # Rename SW_IN to Rg
      Tair = TA_1_1_1,          # Rename TA_1_1_1 to Tair
      rH = RH_1_1_1,            # Rename RH to rH
      Ustar = USTAR,       # Rename USTAR to Ustar
      VPD = VPD,
      DateTime= TIMESTAMP_START,
      WD = WD
    )
  
  return(df)
}

# Apply the function to the way3_data list
way3_data_reddyproc <- lapply(way3_data, select_and_rename_columns)
# Apply the function to the way4_data list
way4_data_reddyproc <- lapply(way4_data, select_and_rename_columns)


# ==============================
# 7. CREATE EQUIDISTANT TIMESTAMPS
# ==============================
# Function to process each dataset and fill missing half-hourly timestamps with NaN
process_data <- function(data) {
  # Convert DateTime to POSIXct if not already
  data$DateTime <- as.POSIXct(data$DateTime, format = "%Y-%m-%d %H:%M:%S")
  # Create a sequence of half-hourly timestamps from the min to the max of the DateTime column
  timestamp_seq <- seq(from = min(data$DateTime), to = max(data$DateTime), by = "30 min")
  # Create a new data frame with the full half-hourly sequence
  full_data <- data.frame(DateTime = timestamp_seq)
  # Merge the full timestamp sequence with the existing data, keeping all timestamps
  full_data <- merge(full_data, data, by = "DateTime", all.x = TRUE)
  # Replace NA values with NaN
  full_data[is.na(full_data)] <- NaN
  return(full_data)
}

# Apply the function to all elements in way4_data_reddyproc from [[1]] to [[7]]
way4_data_reddyproc <- lapply(way4_data_reddyproc, process_data)
# Apply the function to all elements in way3_data_reddyproc from [[1]] to [[7]]
way3_data_reddyproc <- lapply(way3_data_reddyproc, process_data)

nonnegative_removal_vpd_rg <- function(df) {
  df$Rg[df$Rg < 0] <- NaN
  df$VPD[df$VPD < 0] <- NaN
  return(df)
}

# Apply the function to all elements in way4_data_reddyproc from [[1]] to [[7]]
way4_data_reddyproc <- lapply(way4_data_reddyproc[1:7], nonnegative_removal_vpd_rg)
# Apply the function to all elements in way3_data_reddyproc from [[1]] to [[7]]
way3_data_reddyproc <- lapply(way3_data_reddyproc[1:7], nonnegative_removal_vpd_rg)


# ==============================
# 8. DATA VISUALIZATION
# ==============================
# Enhanced plotting function with WD coloring (no rounding, no fixed breaks)
plot_and_save_multiple_vars <- function(data_list, label, years, y_vars, base_output_dir) {
  for (y_var in y_vars) {
    output_dir <- file.path(base_output_dir, y_var)
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    
    for (i in seq_along(data_list)) {
      df <- data_list[[i]]
      
      if (!is.null(df) && y_var %in% names(df) && "WD" %in% names(df)) {
        year <- years[i]
        
        # Get exact min and max of WD without rounding
        wd_vals <- na.omit(df$WD)
        wd_min <- min(wd_vals)
        wd_max <- max(wd_vals)
        
        # Create plot with dynamic scaling (WD-colored)
        p <- ggplot(df, aes(x = DateTime, y = .data[[y_var]], color = WD)) +
          geom_point(alpha = 0.6, size = 1.3) +
          scale_color_gradientn(
            colors = hcl.colors(12, "Spectral"),
            name = "Wind Direction (°)",
            limits = c(wd_min, wd_max)
          ) +
          labs(
            title = paste(label, y_var, "Year:", year),
            x = "DateTime", 
            y = y_var
          ) +
          theme_minimal() +
          theme(
            legend.position = "bottom",
            legend.key.width = unit(2, "cm")
          )
        
        filename <- file.path(output_dir, paste0(label, "_", y_var, "_", year, ".jpeg"))
        ggsave(filename, plot = p, width = 10, height = 6, dpi = 300)
      }
    }
  }
}


# ============================================
# EXECUTION FOR WAY3 AND WAY4 (updated call)
# ============================================
plot_and_save_multiple_vars(way3_data_reddyproc, "Way3", way3_years, y_variables, base_fig_dir)
plot_and_save_multiple_vars(way4_data_reddyproc, "Way4", way4_years, y_variables, base_fig_dir)

# ============================================
# 10. DATA QUALITY CHECK: OUTLIER INSPECTION
# ============================================
# Get Way3 2019 data (assuming your list way3_data is in the same order as way3_files and 2019 is the 5th file)
way3_2019 <- way3_data[[which(grepl("2019", way3_files))]]

# Filter for August 1 to 15, 2019
way3_2019_aug1to15 <- way3_2019 %>%
  filter(TIMESTAMP_START >= as.POSIXct("2019-08-01") &
           TIMESTAMP_START < as.POSIXct("2019-08-16"))

# Print TIMESTAMP_START and NEE column
View(way3_2019_aug1to15 %>% select(TIMESTAMP_START, FC))

# Make a histogram of FC for August 1–15, 2019
hist(way3_2019_aug1to15$FC_1_1_1,
     main = "Histogram of NEE (Way3, Aug 1–15, 2019)",
     xlab = "FC_1_1_1",
     col = "skyblue",
     border = "white")



# ==============================
# Combine all years into one dataframe
# ==============================







# Function: create_sEddyProc_objects
# Task 1a: Create the REddyProc class named `EProc`
# with columns c('NEE','Rg','Tair','VPD', 'Ustar')
# at Location LatDeg = 34.585, LongDeg = -91.751,
# and timezone six hours behind GMT (TimeZoneHour = -6)
create_sEddyProc_objects <- function(data_list, site_name) {
  LatDeg <- 34.585
  LongDeg <- -91.751
  TimeZoneHour <- -6
  
  results <- lapply(seq_along(data_list[1:7]), function(i) {
    EProc <- sEddyProc$new(paste0(site_name, "-Data", 2018 + i - 1), data_list[[i]], 
                           c('NEE', 'Rg', 'Tair', 'VPD', 'Ustar'))
    EProc$sSetLocationInfo(LatDeg = LatDeg, LongDeg = LongDeg, TimeZoneHour = TimeZoneHour)
    return(EProc)
  })
  return(results)
}

# Apply function to Way3 and Way4 datasets
EProcWay3 <- create_sEddyProc_objects(way3_data_reddyproc, "Way3")
EProcWay4 <- create_sEddyProc_objects(way4_data_reddyproc, "Way4")

# Function to create season starts for each year
create_seasonStarts <- function(PD_DOY, HD_DOY) {
  return(as.data.frame(do.call(rbind, list(
    c(PD_DOY, HD_DOY)
  ))))
}
# Create seasonStarts for Way3 (2018-2024)
seasonStartsWay3 <- list(
  create_seasonStarts(way32018PD_DOY, way32018HD_DOY),
  create_seasonStarts(way32019PD_DOY, way32019HD_DOY),
  create_seasonStarts(way32020PD_DOY, way32020HD_DOY),
  create_seasonStarts(way32021PD_DOY, way32021HD_DOY),
  create_seasonStarts(way32022PD_DOY, way32022HD_DOY),
  create_seasonStarts(way32023PD_DOY, way32023HD_DOY),
  create_seasonStarts(way32024PD_DOY, way32024HD_DOY)
)

# Create seasonStarts for Way4 (2018-2024)
seasonStartsWay4 <- list(
  create_seasonStarts(way42018PD_DOY, way42018HD_DOY),
  create_seasonStarts(way42019PD_DOY, way42019HD_DOY),
  create_seasonStarts(way42020PD_DOY, way42020HD_DOY),
  create_seasonStarts(way42021PD_DOY, way42021HD_DOY),
  create_seasonStarts(way42022PD_DOY, way42022HD_DOY),
  create_seasonStarts(way42023PD_DOY, way42023HD_DOY),
  create_seasonStarts(way42024PD_DOY, way42024HD_DOY)
)

# Create a list to store seasonFactor variables for Way3
seasonFactorListway3 <- list()
# Create a list to store seasonFactor variables for Way4
seasonFactorListway4 <- list()
# Loop through the years 2018-2024 (7 years)
for (i in 1:7) {
  # For Way3
  seasonFactorListway3[[paste0("seasonFactorWay3_", 2017 + i)]] <- usCreateSeasonFactorYdayYear(
    way3_data_reddyproc[[i]]$DateTime - 15 * 60, 
    starts = seasonStartsWay3[[i]]
  )
  
  # For Way4
  seasonFactorListway4[[paste0("seasonFactorWay4_", 2017 + i)]] <- usCreateSeasonFactorYdayYear(
    way4_data_reddyproc[[i]]$DateTime - 15 * 60, 
    starts = seasonStartsWay4[[i]]
  )
}

# Loop through the 7 elements of EProcWay3 and apply sEstimateUstarScenarios
for (i in 1:7) {
  EProcWay3[[i]]$sEstimateUstarScenarios(
    seasonFactor = seasonFactorListway3[[i]], 
    nSample = 30L, 
    probs = c(0.1, 0.9)
  )
}

# Loop through the 7 elements of EProcWay4 and apply sEstimateUstarScenarios
for (i in 1:7) {
  EProcWay4[[i]]$sEstimateUstarScenarios(
    seasonFactor = seasonFactorListway4[[i]], 
    nSample = 30L, 
    probs = c(0.1, 0.9)
  )
}

EProcWay4[[6]]$sEstimateUstarScenarios(
  seasonFactor = seasonFactorListway4[[6]], 
  nSample = 30L, 
  probs = c(0.1, 0.9)
)
EProcWay4[[6]]
###################################################
################one field ############################
###################################################
# Create seasonStarts data frame for 2024
EProcWay32024 <- sEddyProc$new('Way3-Data2024', way3_data_reddyproc[[7]], c('NEE', 'Rg', 'Tair', 'VPD', 'Ustar'))
EProcWay32024$sSetLocationInfo(LatDeg = 51.1, LongDeg = 10.9, TimeZoneHour = 1)  # Set location and timezone

seasonStartsway32024 <- as.data.frame(do.call(rbind, list(
  c(way32024PD_DOY, way32024HD_DOY)
)))

seasonFactorway32024 <- usCreateSeasonFactorYdayYear(
  way3_data_reddyproc[[7]]$DateTime - 15*60, starts = seasonStartsway32024)
EProcWay32024$sEstimateUstarScenarios(seasonFactor = seasonFactorway32024, nSample = 30L, probs = c(0.1, 0.9))
EProcWay32024$sMDSGapFillUStarScens("NEE", FillAll = TRUE)

EProcWay32024$sPlotFingerprintY('NEE_uStar_orig', Year = 2024)
EProcWay32024$sPlotFingerprint('NEE_U90_f', Dir = "plotsHandsOn")

EProcWay32024$sMDSGapFill('Rg', FillAll = FALSE)
EProcWay32024$sMDSGapFill('Tair', FillAll = FALSE)
EProcWay32024$sMDSGapFill('VPD', FillAll = FALSE)

EProcWay32024$sPlotFingerprintY('Rg_f', Year = 2024)
EProcWay32024$sPlotFingerprintY('Tair_f', Year = 2024)
EProcWay32024$sPlotFingerprintY('VPD_f', Year = 2024)

EProcWay32024$sMRFluxPartitionUStarScens()
dsResultsWay32024 <- EProcWay32024$sExportResults()
View(dsResults)
EProcWay32024$sGLFluxPartitionUStarScens()

# Bonus Task: Repeat with fixed Temperature Sensitivity to 80 $\pm$40 K
EProcWay32024$sGLFluxPartitionUStarScens(
  controlGLPart = partGLControl(
    fixedTempSens = data.frame(E0 = 80, sdE0 = 40, RRef = NA_real_))
  , isWarnReplaceColumns = FALSE
)

EProcWay32024$sPlotFingerprintY("GPP_DT_uStar", Year = 2024)
EProcWay32024$sPlotFingerprintY("Reco_DT_uStar", Year = 2024)

dsResultsWay32024 <- EProcWay32024$sExportResults()
EProcWay32024$sGLFluxPartitionUStarScens()
colnames(dsResultsWay32024)





##########################################
#########################################
#########################################



