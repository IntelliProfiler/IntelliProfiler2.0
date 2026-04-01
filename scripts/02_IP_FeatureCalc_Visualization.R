###########################################
# Script name : 02_IP_FeatureCalc_Visualization.R
# Description : Calculate hourly behavioral metrics (TD, IID, CCR) from processed positional data and generate summary plots
# Authors     : Shohei Ochi, Masashi Azuma
# Version history:
#   v1.1 - 2025-01-20 - Shohei Ochi, Masashi Azuma
#   v1.2 - 2026-03-31 - Shohei Ochi, Masashi Azuma
###########################################
# ================================================
#  Generate Hourly TD, IID, and CCR
#  from processed IntelliProfiler data
# ------------------------------------------------
#  Assumption:
#    A single Excel file named "processed_data_combined.xlsx"
#    contains one sheet per individual animal ID.
#    Each sheet includes at least the columns:
#        Date | ID | X | Y
#    (Board and Position may also be present, but are not required for this analysis.)
#    Date is recorded in UTC using "%Y/%m/%d %H:%M:%S".
#
#  Outputs:
#    ・analysis_output/Hourly_TD.xlsx
#    ・analysis_output/Hourly_IID.xlsx
#    ・analysis_output/Hourly_CCR.xlsx
#    ・analysis_output/Hourly_TD.png (+ .pdf)
#    ・analysis_output/Hourly_IID.png (+ .pdf)
#    ・analysis_output/Hourly_CCR.png (+ .pdf)
#    ・analysis_output/ID_mapping.txt
# =================================================

# install.packages(c("tidyverse", "lubridate", "readxl", "writexl", "RColorBrewer"))

library(tidyverse)
library(lubridate)
library(readxl)
library(writexl)

# --- Interactive input ---
cat("▶ Select processed_data_combined.xlsx\n")
processed_file <- file.choose()

base_dir <- dirname(processed_file)
out_dir <- file.path(base_dir, "analysis_output")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# --- Load processed data ---
sheets <- excel_sheets(processed_file)
all_data <- map_dfr(sheets, ~ {
  read_excel(processed_file, sheet = .x)
}) %>%
  mutate(Date = as.POSIXct(Date, format = "%Y/%m/%d %H:%M:%S", tz = "UTC")) %>%
  select(Date, ID, X, Y) %>%
  arrange(ID, Date)

# --- Short ID mapping ---
original_ids <- unique(all_data$ID)
short_ids <- paste0("ID", seq_along(original_ids))
id_dict <- setNames(short_ids, original_ids)
all_data$ID <- id_dict[all_data$ID]

write.table(tibble(Short_ID = short_ids, Original_ID = original_ids),
            file.path(out_dir, "ID_mapping.txt"),
            sep = "\t", row.names = FALSE, quote = FALSE)

# --- 2. Hourly travel distance calculation ---
hourly_td <- all_data %>%
  group_by(ID) %>%
  arrange(Date) %>%
  mutate(Dist = sqrt((X - lag(X, default = first(X)))^2 +
                       (Y - lag(Y, default = first(Y)))^2) * 5,
         Hour = floor_date(Date, "hour")) %>%
  group_by(ID, Hour) %>%
  summarise(Total_Distance = sum(Dist, na.rm = TRUE), .groups = "drop")

write_xlsx(hourly_td, file.path(out_dir, "Hourly_TD.xlsx"))

# --- 3. Hourly inter-individual distance calculation ---
ids <- unique(all_data$ID)
pairwise_list <- list()

for (i in 1:(length(ids) - 1)) {
  for (j in (i + 1):length(ids)) {
    id1 <- ids[i]
    id2 <- ids[j]
    
    df1 <- filter(all_data, ID == id1) %>% select(Date, X, Y)
    df2 <- filter(all_data, ID == id2) %>% select(Date, X, Y)
    
    merged <- inner_join(df1, df2, by = "Date", suffix = c("_1", "_2")) %>%
      mutate(
        Inter_Individual_Distance = sqrt((X_1 - X_2)^2 + (Y_1 - Y_2)^2) * 5,
        Hour = floor_date(Date, "hour")
      ) %>%
      group_by(Hour) %>%
      summarise(
        Avg_Inter_Individual_Distance = mean(Inter_Individual_Distance, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(ID_1 = id1, ID_2 = id2)
    
    pairwise_list[[paste(id1, id2, sep = "_vs_")]] <- merged
  }
}

hourly_iid <- bind_rows(pairwise_list)
write_xlsx(hourly_iid, file.path(out_dir, "Hourly_IID.xlsx"))

# --- 4. Close Contact Ratio (CCR) calculation ---
pairwise_ccr_list <- list()

for (i in 1:(length(ids) - 1)) {
  for (j in (i + 1):length(ids)) {
    id1 <- ids[i]
    id2 <- ids[j]
    
    df1 <- filter(all_data, ID == id1) %>% select(Date, X, Y)
    df2 <- filter(all_data, ID == id2) %>% select(Date, X, Y)
    
    merged <- inner_join(df1, df2, by = "Date", suffix = c("_1", "_2")) %>%
      mutate(
        Inter_Individual_Distance = sqrt((X_1 - X_2)^2 + (Y_1 - Y_2)^2) * 5,
        Hour = floor_date(Date, "hour"),
        Close_Contact = ifelse(Inter_Individual_Distance < 10, 1, 0)
      ) %>%
      group_by(Hour) %>%
      summarise(
        CCR = 100 * sum(Close_Contact, na.rm = TRUE) / n(),
        .groups = "drop"
      ) %>%
      mutate(ID_1 = id1, ID_2 = id2)
    
    pairwise_ccr_list[[paste(id1, id2, sep = "_vs_")]] <- merged
  }
}

hourly_ccr <- bind_rows(pairwise_ccr_list)
write_xlsx(hourly_ccr, file.path(out_dir, "Hourly_CCR.xlsx"))

# --- 5. Figure generation (PNG + PDF) ---
palette_id <- setNames(RColorBrewer::brewer.pal(max(3, length(ids)), "Set2"), ids)

save_plot <- function(name, plot, w = 7, h = 4) {
  ggsave(file.path(out_dir, paste0(name, ".png")), plot, width = w, height = h, dpi = 300)
  ggsave(file.path(out_dir, paste0(name, ".pdf")), plot, width = w, height = h)
}

## 5.1 Travel distance
p_move <- ggplot(hourly_td, aes(Hour, Total_Distance, color = ID)) +
  geom_line() + theme_minimal() +
  labs(title = "Hourly Travel Distance", y = "Distance (cm)")
save_plot("Hourly_TD", p_move)

## 5.2 Inter-individual distance
p_iid <- ggplot(hourly_iid, aes(Hour, Avg_Inter_Individual_Distance,
                                color = interaction(ID_1, ID_2))) +
  geom_line(alpha = 0.7) + theme_minimal() +
  labs(title = "Hourly Inter-Individual Distance", y = "Distance (cm)")
save_plot("Hourly_IID", p_iid, 12, 8)

## 5.3 CCR
p_ccr <- ggplot(hourly_ccr, aes(Hour, CCR, color = interaction(ID_1, ID_2))) +
  geom_line(alpha = 0.7) + theme_minimal() +
  labs(title = "Hourly Close Contact Ratio", y = "CCR (%)")
save_plot("Hourly_CCR", p_ccr, 12, 8)

# --- 6. Console output ---------------------------------------
cat("\nAnalysis complete: Excel files and figures have been saved to:\n", out_dir, "\n")
