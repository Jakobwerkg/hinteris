# Author: Jakob Werkgarner
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
file_path <- "data/HEF30_lf.dat"

# Read data with proper header handling
data <- read.csv(
     file_path,
     skip = 4,               # Skip metadata rows
     header = FALSE,         # We'll add column names
     col.names = unlist(read.csv(file_path, skip = 1, nrows = 1, header = FALSE)),
     na.strings = c("NA", "NAN", ""),
     stringsAsFactors = FALSE
)

# Convert TIMESTAMP to proper datetime format
data$TIMESTAMP <- as.POSIXct(data$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")

# Create daily mean snow depth data
hs_data <- data %>%
     mutate(date = as.Date(TIMESTAMP)) %>%  # Extract date part
     group_by(date) %>%                     # Group by date
     summarize(
          hs = mean(Snow_Depth, na.rm = TRUE)  # Calculate daily mean
     ) %>%
     ungroup() %>%
     arrange(date)                          # Sort chronologically

# Verify the result
hs_data
hs_data[1,2] = 0.  # set first value zero to be able to run the model

################################################################################


data(hs_data, package = "nixmass")

swe_deltasnow <- swe.delta.snow(hs_data, layers = FALSE)
swe_hs2swe <- hs2swe(hs_data)
plot(seq_along(hs_data$date), swe_deltasnow, type = "l", ylab = "SWE (mm) / hs (cm)", xlab = "day")
lines(seq_along(hs_data$date), swe_hs2swe$SWE, type = "l", col = "red")
lines(seq_along(hs_data$date), hs_data$hs * 100, type = "l", lty = 2, col = "grey30")
legend("topleft", legend = c("deltaSNOW", "HS2SWE", "HS"),
col = c("black", "red", "grey30"), lty = c(1, 1, 2))

# also run other model

gu19 <- swe.gu19(hs_data,'italy')
jo09 <- swe.jo09(hs_data)
pi16 <- swe.pi16(hs_data)
st10 <- swe.st10(hs_data)


output_data <- data.frame(
     date = hs_data$date,
     hs = hs_data$hs,
     swe_deltasnow = swe_deltasnow,
     swe_hs2swe = swe_hs2swe$SWE,
     swe_gu19 = gu19,
     swe_pi16 = pi16,
     swe_st10 = st10
     
)


     
