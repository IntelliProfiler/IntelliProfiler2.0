###########################################
# Script name : 01_IP_RawLog_to_PositionID.R
# Description : Convert raw IntelliProfiler log data into time-resolved positional coordinates (X, Y)
# Authors     : Shohei Ochi, Masashi Azuma
# Version history:
#   v1.1 - 2025-01-20 - Shohei Ochi, Masashi Azuma
#   v1.2 - 2026-03-31 - Shohei Ochi, Masashi Azuma
###########################################
# install.packages(c("dplyr","tidyr","lubridate","doParallel","openxlsx","readr","purrr","foreach"))

# --- Required libraries ---
library(dplyr)
library(tidyr)
library(lubridate)
library(doParallel)
library(openxlsx)
library(readr)
library(purrr)
library(foreach)
library(parallel)

# --- A. Data import function ---
import_IP1_data <- function(filePaths, boards) {
  data <- list()
  for (i in seq_along(filePaths)) {
    file <- filePaths[i]
    board <- boards[i]
    
    lines <- readLines(file)
    lines <- lines[lines != ""]
    
    df <- data.frame(raw = lines, stringsAsFactors = FALSE)
    df <- df %>%
      separate(raw, c("Date", "PositionID"), sep = "\\] ", extra = "merge", fill = "right") %>%
      mutate(Date = gsub("\\[", "", Date)) %>%
      mutate(PositionID = gsub("^\\d+,", "", PositionID)) %>%  
      separate(PositionID, c("Position", "ID"), sep = ":", extra = "merge", fill = "right") %>%  # split by `:`
      mutate(
        Position = as.integer(Position),   
        Board = board,                     
        ID = gsub(",", "_", ID),           
        Date = as.POSIXct(Date, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"),
        Date = floor_date(Date, unit = "second")
      )
    data[[i]] <- df
  }
  bind_rows(data)
}

# --- B. Load data (interactive file selection) ---
cat("▶ Please select the TeraTerm log file for Board1.\n")
f1 <- file.choose()

cat("▶ Please select the TeraTerm log file for Board2.\n")
f2 <- file.choose()

cat("▶ Please select the TeraTerm log file for Board3.\n")
f3 <- file.choose()

cat("▶ Please select the TeraTerm log file for Board4.\n")
f4 <- file.choose()

filePaths <- c(f1, f2, f3, f4)

boards <- c(1, 2, 3, 4)

df <- import_IP1_data(filePaths, boards)

df <- df %>%
  separate_rows(ID, sep = "_") %>%
  arrange(Date, ID)

df <- df %>%
  arrange(Date, ID) %>%
  group_by(Date, ID) %>%
  slice(1) %>%
  ungroup()

df <- df %>%
  group_split(ID) %>%
  map_dfr(function(dfi) {
    id <- unique(dfi$ID)
    full_time <- seq(min(dfi$Date), max(dfi$Date), by = "1 sec")
    dfi %>%
      complete(Date = full_time, ID = id) %>%
      fill(Position, Board, .direction = "downup")
  })

# --- C. Coordinate transformation ---
# Board1: X = 1-6,  Y = 1-4
# Board2: X = 7-12, Y = 1-4
# Board3: X = 1-6,  Y = 5-8
# Board4: X = 7-12, Y = 5-8

df <- df %>%
  mutate(
    X = case_when(
      Board == 1 ~ ((Position - 1) %% 6) + 1,
      Board == 2 ~ 7 + ((24 - Position) %% 6),
      Board == 3 ~ ((Position - 1) %% 6) + 1,
      Board == 4 ~ 7 + ((24 - Position) %% 6)
    ),
    Y = case_when(
      Board == 1 ~ 4 - floor((Position - 1) / 6),
      Board == 2 ~ 4 - floor((24 - Position) / 6),
      Board == 3 ~ 8 - floor((Position - 1) / 6),
      Board == 4 ~ 8 - floor((24 - Position) / 6)
    )
  )

# --- D. Reorder columns for export ---
df <- df %>%
  select(Date, ID, Board, Position, X, Y, everything())

df <- df %>%
  mutate(Date = format(Date, "%Y/%m/%d %H:%M:%S", tz = "UTC"))

# --- E. Export per-ID Excel files ---
base_dir <- dirname(filePaths[1])
output_dir <- file.path(base_dir, "processed_data_xlsx")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

unique_ids <- unique(df$ID)
for (id in unique_ids) {
  id_data <- df %>% filter(ID == id)
  openxlsx::write.xlsx(id_data, file = file.path(output_dir, paste0(id, ".xlsx")))
}

# --- F. Export the full dataset (single file) ---
full_output_file <- file.path(base_dir, "processed_data.xlsx")
openxlsx::write.xlsx(df, full_output_file)

message("✅ Completed: Exported per-ID and full Excel files efficiently.")

# --- G. Combine per-ID data into a single workbook (one sheet per ID) ---
output_combined_file <- file.path(base_dir, "processed_data_combined.xlsx")
wb <- createWorkbook()

id_list <- split(df, df$ID)

for (id_name in names(id_list)) {
  df_id <- id_list[[id_name]]
  
  addWorksheet(wb, id_name)
  writeData(wb, id_name, df_id)
}

saveWorkbook(wb, output_combined_file, overwrite = TRUE)
message("✅ Combined all per-ID data into a single workbook: ", output_combined_file)