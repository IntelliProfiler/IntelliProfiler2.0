install.packages("dplyr")
install.packages("tidyr")
install.packages("lubridate")
install.packages("doParallel")
install.packages("openxlsx")
install.packages("readr")

# --- Required libraries ---
library(dplyr)
library(tidyr)
library(lubridate)
library(doParallel)
library(openxlsx)
library(readr)
library(purrr)
library(foreach)

# --- A. Data import function ---
import_IP1_data <- function(filePaths, positions) {
  data <- list()
  for (i in seq_along(filePaths)) {
    file <- filePaths[i]
    pos_start <- positions[i]
    
    lines <- readLines(file)
    lines <- lines[lines != ""]
    
    df <- data.frame(raw = lines, stringsAsFactors = FALSE)
    df <- df %>%
      separate(raw, c("Date", "PositionID"), sep = "\\] ") %>%
      mutate(Date = gsub("\\[", "", Date)) %>%
      mutate(PositionID = gsub("^\\d+,", "", PositionID)) %>%  # remove the leading `1,`
      separate(PositionID, c("Position", "ID"), sep = ":", extra = "merge", fill = "right") %>%  # split by `:`
      mutate(
        Position = as.integer(Position) + pos_start - 1,  # convert Position to numeric
        ID = gsub(",", "_", ID),  # replace `,` with `_` (double check)
        Date = as.POSIXct(Date, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"),
        Date = floor_date(Date, unit = "second")
      )
    data[[i]] <- df
  }
  bind_rows(data)
}

# --- B. Load data (interactive file selection) ---
cat("▶ Please select the TeraTerm log file for Board 1.\n")
f1 <- file.choose()

cat("▶ Please select the TeraTerm log file for Board 2.\n")
f2 <- file.choose()

cat("▶ Please select the TeraTerm log file for Board 3.\n")
f3 <- file.choose()

cat("▶ Please select the TeraTerm log file for Board 4.\n")
f4 <- file.choose()

filePaths <- c(f1, f2, f3, f4)
positions <- c(1, 25, 49, 73)

df <- import_IP1_data(filePaths, positions)

# Split IDs
df <- df %>%
  separate_rows(ID, sep = "_") %>%
  arrange(Date, ID)

df <- df %>%
  arrange(Date, ID) %>%
  group_by(Date, ID) %>%
  slice(1) %>%
  ungroup()

# ---- 🧠 Complete each ID separately and recombine (prevents data explosion) ---
df <- df %>%
  group_split(ID) %>%
  map_dfr(function(dfi) {
    id <- unique(dfi$ID)
    full_time <- seq(min(dfi$Date), max(dfi$Date), by = "1 sec")
    dfi %>%
      complete(Date = full_time, ID = id) %>%
      fill(Position, .direction = "downup")
  })

# --- C. Coordinate transformation ---
df <- df %>%
  mutate(
    X = case_when(
      between(Position, 1, 24)   ~ ((Position - 1) %% 6) + 1,
      between(Position, 25, 48)  ~ 7 + ((48 - Position) %% 6),
      between(Position, 49, 72)  ~ ((Position - 49) %% 6) + 1,
      between(Position, 73, 96)  ~ 7 + ((96 - Position) %% 6)
    ),
    Y = case_when(
      between(Position, 1, 24)   ~ 9 - (floor((Position - 1) / 6) + 5),
      between(Position, 25, 48)  ~ 9 - (floor((48 - Position) / 6) + 5),
      between(Position, 49, 72)  ~ 9 - (floor((Position - 49) / 6) + 1),
      between(Position, 73, 96)  ~ 9 - (floor((96 - Position) / 6) + 1)
    )
  )

# --- D. Export per-ID Excel files using parallel processing ---
output_dir <- "processed_data_xlsx"
dir.create(output_dir, showWarnings = FALSE)

n_cores <- max(1, detectCores() - 1)
cl <- makeCluster(n_cores)
registerDoParallel(cl)

unique_ids <- unique(df$ID)
foreach(id = unique_ids, .packages = c("dplyr", "openxlsx")) %dopar% {
  id_data <- df %>% filter(ID == id)
  openxlsx::write.xlsx(id_data, file = file.path(output_dir, paste0(id, ".xlsx")))
}

stopCluster(cl)

# --- E. Export the full dataset (single file) ---
openxlsx::write.xlsx(df, "processed_data.xlsx")

message("✅ Completed: Exported per-ID and full Excel files efficiently.")

# --- F. Combine per-ID Excel files into a single workbook (one sheet per ID) ---
library(openxlsx)

output_combined_file <- "processed_data_combined.xlsx"
wb <- createWorkbook()

# Load files from the folder and add each as a sheet
id_files <- list.files(output_dir, pattern = "\\.xlsx$", full.names = TRUE)

for (file_path in id_files) {
  sheet_name <- tools::file_path_sans_ext(basename(file_path))
  
  df_id <- read.xlsx(file_path, detectDates = FALSE)
  
  # Excel serial date (numeric) -> POSIXct (datetime)
  df_id$Date <- as.POSIXct("1899-12-30", tz = "Asia/Tokyo") + as.numeric(df_id$Date) * 86400
  
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, df_id)
}

# Save
saveWorkbook(wb, output_combined_file, overwrite = TRUE)
message("✅ Combined all per-ID files into a single workbook: ", output_combined_file)