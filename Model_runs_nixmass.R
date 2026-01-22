# Date: 20.07.2025
# Title: EU Fieldcourse run DeltaSnow and HS2SWE Model
setwd("~/Desktop/EU Field/Datenauswertung")

#load packages

library(devtools)
library(tidyverse)
library(RColorBrewer)
library(fields) 
library(dplyr)
library(lubridate)

# Install devtools if needed
if (!require("devtools")) install.packages("devtools")

# Load functions from a directory (even if not a full R package)
devtools::load_all("/Users/jakobwerkgarner/code/Master_Delta/nixmass/R/")

# Read the file
file_path <- "data/model_input/hef30_daily_hs_input.csv"

# Read data with proper header handling
hs_data <- read.csv(
  file_path,
  header = TRUE,         # We'll add column names
  na.strings = c("NA", "NAN", ""),
  stringsAsFactors = FALSE
)

# Convert TIMESTAMP to proper datetime format
hs_data$date <- as.POSIXct(hs_data$date, format = "%Y-%m-%d")


# Verify the result
hs_data
hs_data[1,2] = 0.  # set first value zero to be able to run the model 

hs_data$date <- as.Date(hs_data$date) #make sure we have a date format


################################################################################


swe_deltasnow <- swe.delta.snow(hs_data, layers = FALSE)
swe_hs2swe <- hs2swe(hs_data)
plot(seq_along(hs_data$date), swe_deltasnow, type = "l", ylab = "SWE (mm) / hs (cm)", xlab = "day")
lines(seq_along(hs_data$date), swe_hs2swe$SWE, type = "l", col = "red")
lines(seq_along(hs_data$date), hs_data$hs * 100, type = "l", lty = 2, col = "grey30")
legend("topleft", legend = c("deltaSNOW", "HS2SWE", "HS"),
       col = c("black", "red", "grey30"), lty = c(1, 1, 2))



#altitude does not matter as long as larger then 2000m

#Selected Region 4 since Wallis resempbles Hinter Ötztal the best in terms of inner alpine dry valley

jo09 <- swe.jo09(hs_data, alt = 3000, region.jo09 = 4)

pi16 <- swe.pi16(hs_data)

st10 <- swe.st10(hs_data, snowclass.st10 = "alpine")


output_data <- data.frame(
  date = hs_data$date,
  hs = hs_data$hs,
  swe_deltasnow = swe_deltasnow,
  swe_hs2swe = swe_hs2swe$SWE,
  swe_pi16 = pi16,
  swe_st10 = st10,
  swe_jo09 = jo09
  
)

path_to_save <- "/Users/jakobwerkgarner/Desktop/EU Field/Datenauswertung/data/output"

# write CSV without row names
write.csv(
  output_data,
  file = file.path(path_to_save, "nixmass_model_output.csv"),
  row.names = FALSE
)
