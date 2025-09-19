# Fluxdata: Ecosystem Flux Data Processing and Analysis

This repository contains a collection of R scripts for processing, gap-filling, and partitioning Net Ecosystem Exchange (NEE) data from eddy covariance flux towers, specifically for the Runkle AmeriFlux sites for the years 2015-2017.

## 📁 Repository Overview

The scripts in this project are primarily focused on handling and analyzing eddy covariance data using the `REddyProc` package in R.

## 🚀 Features

*   **Data Aggregation:** Scripts to aggregate high-frequency flux data into daily and other temporal resolutions.
*   **Gap-Filling:** Implementation of the standard REddyProc routines for gap-filling meteorological and flux data (e.g., `GapfillingUsingReddyProc.R`).
*   **NEE Partitioning:** Separation of Net Ecosystem Exchange (NEE) into its constituent fluxes: Gross Primary Production (GPP) and Ecosystem Respiration (RECO).
*   **Data Visualization:** Basic plotting scripts for environmental drivers like Temperature, Vapor Pressure Deficit (VPD), and Solar Radiation (Rg).
*   **Multi-Gas Analysis:** Includes scripts for handling both CO2 (`fluxRunkleSites.R`) and CH4 (`fluxRunkleSites_CH4.R`) flux data.

## 📊 Scripts Description

| Script Name | Purpose |
| :--- | :--- |
| `ECdataaggregate201520162017w3w4VPMGPP.R` | Aggregates EC data for 2015-2017 and calculates VPM GPP. |
| `GapfillingUsingReddyProc.R` | Performs gap-filling on meteorological and flux variables using REddyProc. |
| `NEEpartitioning.R` | Partitions NEE into GPP and RECO components. |
| `PartitioningUsingReddyProc.R` | Uses the REddyProc package specifically for partitioning. |
| `PlotTemperatureVPDRg.R` | Generates plots for Temperature, VPD, and Solar Radiation. |
| `ReddyProc.R`, `ReddyProcBasic.R` | Core scripts for running REddyProc processing workflows. |
| `RunkleAmerifulx201520162017.R` | Main data processing script for the Runkle AmeriFlux sites (2015-2017). |
| `SingleFileReddyProc.R` | Likely a script to process a single data file through REddyProc. |
| `fluxRunkleSites.R` | Processes CO2 flux data for the Runkle sites. |
| `fluxRunkleSites_CH4.R` | Processes methane (CH4) flux data for the Runkle sites. |
| `way3dailyaggregate2015.R` | Aggregates data into daily timesteps for the year 2015. |
| `CheckScript.R` | Possibly a script for data quality control and sanity checks. |

## 🛠️ Installation & Setup

To use these scripts, you will need to have R and several key packages installed.

1.  **Install R and RStudio:** Download and install [R](https://cran.r-project.org/) and optionally [RStudio](https://www.rstudio.com/products/rstudio/download/).
2.  **Open the Project:** Open the `FluxdataR.Rproj` file in RStudio to activate the project environment.
3.  **Install Required R Packages:** Run the following commands in your R console to install the necessary dependencies.

```r
# Install necessary packages
install.packages("REddyProc")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("readr")

# Load the packages
library(REddyProc)
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
