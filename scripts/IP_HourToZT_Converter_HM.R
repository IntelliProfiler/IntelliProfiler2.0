# --- Required libraries ---
library(readxl)
library(dplyr)
library(lubridate)
library(writexl)

# --- Interactive file selection ---
cat("▶ Please select the file (Hourly_Movement.xlsx)\n")
file_path <- file.choose()
cat("▶ Selected file:", file_path, "\n")

# --- Load data ---
data <- read_excel(file_path)

# --- Convert time column to POSIXct ---
data <- data %>%
  mutate(Hour = as.POSIXct(Hour, tz = "UTC"))

# --- Define ZT0 reference time ---
zt0 <- as.POSIXct("2025-05-24 08:00:00", tz = "UTC")  # Change if necessary

# --- Add ZT labels ---
data <- data %>%
  mutate(
    ZT_Hour = as.numeric(difftime(Hour, zt0, units = "hours")),
    ZT_Day = floor(ZT_Hour / 24) + 1,  # Start counting from Day1
    ZT_Label = paste0("Day", ZT_Day, " ZT", ZT_Hour %% 24)
  )

# --- Extract and reorder necessary columns ---
output <- data %>%
  select(ID, ZT_Label, Total_Distance)

# --- Set output directory (same as input file location) ---
output_dir <- dirname(file_path)

# --- Output file name ---
output_file <- file.path(output_dir, "ZT_converted_HM.xlsx")

# --- Save result ---
write_xlsx(output, path = output_file)

cat("✅ Saved:", output_file, "\n")
