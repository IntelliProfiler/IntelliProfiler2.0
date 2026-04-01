###########################################
# Script name : 04_IP_HourToZT_Converter_IID.R
# Description : Convert hourly inter-individual distance (IID) data into Zeitgeber Time (ZT) format
# Authors     : Shohei Ochi, Masashi Azuma
# Version history:
#   v1.1 - 2025-01-20 - Shohei Ochi, Masashi Azuma
#   v1.2 - 2026-03-31 - Shohei Ochi, Masashi Azuma
###########################################
# --- Required libraries ---
library(readxl)
library(dplyr)
library(lubridate)
library(writexl)

# --- Interactive file selection ---
cat("▶ Please select the file (Hourly_IID.xlsx)\n")
file_path <- file.choose()
cat("▶ Selected file:", file_path, "\n")

# --- Load data ---
data <- read_excel(file_path)

# --- Convert time column to POSIXct ---
data <- data %>%
  mutate(Hour = as.POSIXct(Hour, tz = "UTC"))

# --- Define ZT0 reference time (08:00 of the first recording day) ---
zt0 <- floor_date(min(data$Hour), "day") + hours(8)

# --- Add ZT labels ---
data <- data %>%
  mutate(
    ZT_Hour  = as.numeric(difftime(Hour, zt0, units = "hours")),
    ZT_Day   = floor(ZT_Hour / 24) + 1,  # Start counting from Day1
    ZT_Label = paste0("Day", ZT_Day, " ZT", ZT_Hour %% 24)
  )

output <- data %>%
  select(ID_1, ID_2, ZT_Label, Avg_Inter_Individual_Distance)

# --- Set output directory (same as input file location) ---
output_dir <- dirname(file_path)

# --- Output file name ---
output_file <- file.path(output_dir, "ZT_converted_Hourly_IID.xlsx")

# --- Save result ---
write_xlsx(output, path = output_file)

cat("✅ Saved:", output_file, "\n")
