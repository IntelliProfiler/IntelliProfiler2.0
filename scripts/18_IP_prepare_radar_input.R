###########################################
# Script name : 18_IP_prepare_radar_input.R
# Description : Integrate TD, IID, and CCR datasets, perform normalization (male reference and min-max scaling), and generate structured input tables for radar chart visualization
# Authors     : Shohei Ochi, Masashi Azuma
# Version history:
#   v1.1 - 2025-01-20 - Shohei Ochi, Masashi Azuma
#   v1.2 - 2026-03-31 - Shohei Ochi, Masashi Azuma
###########################################
suppressPackageStartupMessages({
  library(readxl)
  library(writexl)
  library(dplyr)
  library(tidyr)
  library(stringr)
})

###########################################
# Helper functions
###########################################

# Robust column selector
pick_col <- function(df, candidates) {
  hit <- intersect(names(df), candidates)
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

# Extract Day and ZT from "ZT_Label"
parse_zt_label <- function(df, zt_col = "ZT_Label") {
  df %>%
    mutate(
      ZT_Label = as.character(.data[[zt_col]]),
      Day = str_extract(ZT_Label, "Day\\d+"),
      ZT  = as.integer(str_extract(ZT_Label, "(?<=ZT)\\d+"))
    )
}

# Assign Light/Dark phase based on ZT
# Default: ZT0–11 = Light, ZT12–23 = Dark
assign_ld <- function(df) {
  df %>%
    mutate(
      LD = case_when(
        !is.na(ZT) & ZT >= 0  & ZT <= 11 ~ "Light",
        !is.na(ZT) & ZT >= 12 & ZT <= 23 ~ "Dark",
        TRUE ~ NA_character_
      )
    )
}

# Min-max scaling
# If max == min, return 0 for all values in the group
safe_minmax <- function(x) {
  x <- as.numeric(x)
  rng <- range(x, na.rm = TRUE)
  if (any(is.infinite(rng)) || is.na(rng[1]) || is.na(rng[2])) {
    return(rep(NA_real_, length(x)))
  }
  if ((rng[2] - rng[1]) == 0) {
    return(rep(0, length(x)))
  }
  (x - rng[1]) / (rng[2] - rng[1])
}

###########################################
# Interactive file selection
###########################################

cat("▶ Please select the Male file (ZT_converted_Hourly_TD.xlsx)\n")
file_male_td <- file.choose()

cat("▶ Please select the Female file (ZT_converted_Hourly_TD.xlsx)\n")
file_female_td <- file.choose()

cat("▶ Please select the Male file (ZT_converted_Hourly_IID.xlsx)\n")
file_male_iid <- file.choose()

cat("▶ Please select the Female file (ZT_converted_Hourly_IID.xlsx)\n")
file_female_iid <- file.choose()

cat("▶ Please select the Male file (ZT_converted_Hourly_CCR.xlsx)\n")
file_male_ccr <- file.choose()

cat("▶ Please select the Female file (ZT_converted_Hourly_CCR.xlsx)\n")
file_female_ccr <- file.choose()

###########################################
# Load TD data
###########################################

read_td_data <- function(file, group_name) {
  df <- read_excel(file)
  
  id_col <- pick_col(df, c("ID", "Id", "id"))
  zt_col <- pick_col(df, c("ZT_Label", "ZT label", "ZT"))
  
  if (is.na(id_col) || is.na(zt_col)) {
    stop("Required columns for TD were not found.")
  }
  
  if ("ZT_converted_Hourly_TD" %in% names(df)) {
    val_col <- "ZT_converted_Hourly_TD"
  } else if ("Total_Distance" %in% names(df)) {
    val_col <- "Total_Distance"
  } else {
    stop("TD column not found.")
  }
  
  df %>%
    transmute(
      Sex = group_name,
      ID = as.character(.data[[id_col]]),
      ZT_Label = as.character(.data[[zt_col]]),
      TD = as.numeric(.data[[val_col]])
    ) %>%
    parse_zt_label("ZT_Label") %>%
    assign_ld()
}

###########################################
# Load IID data (pair -> individual expansion)
###########################################

read_iid_data <- function(file, group_name) {
  df <- read_excel(file)
  
  id1 <- pick_col(df, c("ID_1"))
  id2 <- pick_col(df, c("ID_2"))
  zt  <- pick_col(df, c("ZT_Label"))
  val <- pick_col(df, c("Avg_Inter_Individual_Distance"))
  
  if (any(is.na(c(id1, id2, zt, val)))) {
    stop("Required columns for IID were not found.")
  }
  
  base <- df %>%
    transmute(
      Sex = group_name,
      ID_1 = as.character(.data[[id1]]),
      ID_2 = as.character(.data[[id2]]),
      ZT_Label = as.character(.data[[zt]]),
      IID = as.numeric(.data[[val]])
    ) %>%
    parse_zt_label("ZT_Label") %>%
    assign_ld()
  
  bind_rows(
    base %>% transmute(Sex, ID = ID_1, Day, LD, IID),
    base %>% transmute(Sex, ID = ID_2, Day, LD, IID)
  )
}

###########################################
# Load CCR data (pair -> individual expansion)
###########################################

read_ccr_data <- function(file, group_name) {
  df <- read_excel(file)
  
  id1 <- pick_col(df, c("ID_1"))
  id2 <- pick_col(df, c("ID_2"))
  zt  <- pick_col(df, c("ZT_Label"))
  val <- pick_col(df, c("CCR"))
  
  if (any(is.na(c(id1, id2, zt, val)))) {
    stop("Required columns for CCR were not found.")
  }
  
  base <- df %>%
    transmute(
      Sex = group_name,
      ID_1 = as.character(.data[[id1]]),
      ID_2 = as.character(.data[[id2]]),
      ZT_Label = as.character(.data[[zt]]),
      CCR = as.numeric(.data[[val]])
    ) %>%
    parse_zt_label("ZT_Label") %>%
    assign_ld()
  
  bind_rows(
    base %>% transmute(Sex, ID = ID_1, Day, LD, CCR),
    base %>% transmute(Sex, ID = ID_2, Day, LD, CCR)
  )
}

###########################################
# Read all datasets
###########################################

td_all <- bind_rows(
  read_td_data(file_male_td, "Male"),
  read_td_data(file_female_td, "Female")
)

iid_all <- bind_rows(
  read_iid_data(file_male_iid, "Male"),
  read_iid_data(file_female_iid, "Female")
)

ccr_all <- bind_rows(
  read_ccr_data(file_male_ccr, "Male"),
  read_ccr_data(file_female_ccr, "Female")
)

###########################################
# Aggregate to radar input format
###########################################

td_summary <- td_all %>%
  group_by(Sex, ID, Day, LD) %>%
  summarise(`Travel dist` = sum(TD, na.rm = TRUE), .groups = "drop")

iid_summary <- iid_all %>%
  group_by(Sex, ID, Day, LD) %>%
  summarise(`Inter-indiv dist` = mean(IID, na.rm = TRUE), .groups = "drop")

ccr_summary <- ccr_all %>%
  group_by(Sex, ID, Day, LD) %>%
  summarise(CCR = mean(CCR, na.rm = TRUE), .groups = "drop")

###########################################
# Merge datasets
###########################################

radar_input_raw <- td_summary %>%
  full_join(iid_summary, by = c("Sex", "ID", "Day", "LD")) %>%
  full_join(ccr_summary, by = c("Sex", "ID", "Day", "LD")) %>%
  arrange(Sex, ID, Day, LD)

###########################################
# 1. Male Day1-4 average = 1.0 normalization
###########################################

male_ref <- radar_input_raw %>%
  filter(Sex == "Male") %>%
  group_by(LD) %>%
  summarise(
    ref_td  = mean(`Travel dist`, na.rm = TRUE),
    ref_iid = mean(`Inter-indiv dist`, na.rm = TRUE),
    ref_ccr = mean(CCR, na.rm = TRUE),
    .groups = "drop"
  )

radar_input_maleref <- radar_input_raw %>%
  left_join(male_ref, by = "LD") %>%
  mutate(
    `Travel dist`      = `Travel dist` / ref_td,
    `Inter-indiv dist` = `Inter-indiv dist` / ref_iid,
    CCR                = CCR / ref_ccr
  ) %>%
  select(Sex, ID, Day, LD, `Travel dist`, `Inter-indiv dist`, CCR) %>%
  arrange(Sex, ID, Day, LD)

###########################################
# 2. Day-wise min-max scaling across all individuals
#    followed by Male Day1-4 average = 1.0 normalization
###########################################

radar_input_dayminmax <- radar_input_raw %>%
  group_by(Day, LD) %>%
  mutate(
    `Travel dist`      = safe_minmax(`Travel dist`),
    `Inter-indiv dist` = safe_minmax(`Inter-indiv dist`),
    CCR                = safe_minmax(CCR)
  ) %>%
  ungroup() %>%
  arrange(Sex, ID, Day, LD)

male_ref_dayminmax <- radar_input_dayminmax %>%
  filter(Sex == "Male") %>%
  group_by(LD) %>%
  summarise(
    ref_td  = mean(`Travel dist`, na.rm = TRUE),
    ref_iid = mean(`Inter-indiv dist`, na.rm = TRUE),
    ref_ccr = mean(CCR, na.rm = TRUE),
    .groups = "drop"
  )

radar_input_minmax_maleref <- radar_input_dayminmax %>%
  left_join(male_ref_dayminmax, by = "LD") %>%
  mutate(
    `Travel dist`      = `Travel dist` / ref_td,
    `Inter-indiv dist` = `Inter-indiv dist` / ref_iid,
    CCR                = CCR / ref_ccr
  ) %>%
  select(Sex, ID, Day, LD, `Travel dist`, `Inter-indiv dist`, CCR) %>%
  arrange(Sex, ID, Day, LD)

###########################################
# Define output directory and file
###########################################

output_dir <- file.path(dirname(file_male_td), "Radar_Input_Data")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

out_file <- file.path(output_dir, "radar_input.xlsx")

###########################################
# Save output
###########################################

write_xlsx(
  list(
    Radar_Input_Raw = radar_input_raw,
    Radar_Input_MaleRef = radar_input_maleref,
    Radar_Input_MinMax_MaleRef = radar_input_minmax_maleref,
    Male_Reference_Raw = male_ref,
    Male_Reference_MinMax = male_ref_dayminmax,
    TD = td_all,
    IID = iid_all,
    CCR = ccr_all,
    TD_summary = td_summary,
    IID_summary = iid_summary,
    CCR_summary = ccr_summary
  ),
  out_file
)

cat("========================================\n")
cat("Processing completed successfully\n")
cat("Output directory:\n", output_dir, "\n")
cat("Output file:\n", out_file, "\n")
cat("Saved sheets:\n")
cat(" - Radar_Input_Raw\n")
cat(" - Radar_Input_MaleRef\n")
cat(" - Radar_Input_MinMax_MaleRef\n")
cat("========================================\n")