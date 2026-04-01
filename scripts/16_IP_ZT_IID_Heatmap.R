###########################################
# Script name : 16_IP_ZT_IID_Heatmap.R
# Description : Generate 1-hour and 12-hour heatmaps of inter-individual distance (IID) normalized across pairs and groups in Zeitgeber Time (ZT)
# Authors     : Shohei Ochi, Masashi Azuma
# Version history:
#   v1.1 - 2025-01-20 - Shohei Ochi, Masashi Azuma
#   v1.2 - 2026-03-31 - Shohei Ochi, Masashi Azuma
###########################################
# --- Required packages ---
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(writexl)
library(scales)

###########################################
# Select files
###########################################

cat("▶ Please select the Male file (ZT_converted_Hourly_IID.xlsx)\n")
file_male <- file.choose()

cat("▶ Please select the Female file (ZT_converted_Hourly_IID.xlsx)\n")
file_female <- file.choose()

###########################################
# Load data
###########################################

read_iid_data <- function(file, group_name) {
  df <- read_excel(file)
  
  # Accept either canonical or legacy column name
  if ("Avg_Inter_Individual_Distance" %in% names(df)) {
    df <- df %>%
      mutate(IID_Value = as.numeric(Avg_Inter_Individual_Distance))
  } else if ("Avg_InterIndividual_Distance" %in% names(df)) {
    df <- df %>%
      mutate(IID_Value = as.numeric(Avg_InterIndividual_Distance))
  } else if ("Avg_Social_Distance" %in% names(df)) {
    df <- df %>%
      mutate(IID_Value = as.numeric(Avg_Social_Distance))
  } else {
    stop("Column 'Avg_Inter_Individual_Distance', 'Avg_InterIndividual_Distance', or 'Avg_Social_Distance' was not found.")
  }
  
  df %>%
    mutate(
      Group = group_name,
      Pair = paste(ID_1, ID_2, sep = "-"),
      ZT = as.integer(sub(".*ZT", "", ZT_Label)),
      Day = as.integer(sub("Day(\\d+).*", "\\1", ZT_Label))
    ) %>%
    arrange(Day, ZT)
}

df_male   <- read_iid_data(file_male, "Male")
df_female <- read_iid_data(file_female, "Female")

df_all <- bind_rows(df_male, df_female)

###########################################
# Output directory
###########################################

out_dir <- file.path(dirname(file_male), "IID_Heatmap_Output")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

###########################################
# Common settings
###########################################

pair_levels_global <- rev(sort(unique(df_all$Pair)))

zt_levels_global <- df_all %>%
  distinct(Day, ZT, ZT_Label) %>%
  arrange(Day, ZT) %>%
  pull(ZT_Label)

period_levels_global <- c(
  "Day1 Light", "Day1 Dark",
  "Day2 Light", "Day2 Dark",
  "Day3 Light", "Day3 Dark",
  "Day4 Light", "Day4 Dark"
)

###########################################
# 1hr data
# Normalize by common max across all pairs
# in Male + Female
###########################################

hourly_all <- df_all %>%
  mutate(
    ZT_Label = factor(ZT_Label, levels = zt_levels_global),
    Pair = factor(Pair, levels = pair_levels_global)
  ) %>%
  arrange(Group, Pair, Day, ZT)

hourly_global_max <- max(hourly_all$IID_Value, na.rm = TRUE)

hourly_all <- hourly_all %>%
  mutate(
    Ratio = ifelse(!is.na(IID_Value) & hourly_global_max > 0,
                   IID_Value / hourly_global_max * 100,
                   NA_real_)
  )

hourly_male   <- hourly_all %>% filter(Group == "Male")
hourly_female <- hourly_all %>% filter(Group == "Female")

###########################################
# 12hr data
# Light = ZT0-11 mean
# Dark  = ZT12-23 mean
# Normalize by common max across all pairs
# in Male + Female
###########################################

lightdark_all <- df_all %>%
  mutate(
    LightDark = ifelse(ZT < 12, "Light", "Dark"),
    Period_Label = paste0("Day", Day, " ", LightDark)
  ) %>%
  group_by(Group, Pair, Day, LightDark, Period_Label) %>%
  summarise(
    Mean_IID = mean(IID_Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Period_Label = factor(Period_Label, levels = period_levels_global),
    Pair = factor(Pair, levels = pair_levels_global)
  ) %>%
  arrange(Group, Pair, Period_Label)

lightdark_global_max <- max(lightdark_all$Mean_IID, na.rm = TRUE)

lightdark_all <- lightdark_all %>%
  mutate(
    Ratio = ifelse(!is.na(Mean_IID) & lightdark_global_max > 0,
                   Mean_IID / lightdark_global_max * 100,
                   NA_real_)
  )

lightdark_male   <- lightdark_all %>% filter(Group == "Male")
lightdark_female <- lightdark_all %>% filter(Group == "Female")

###########################################
# Save calculation process as CSV/XLSX
###########################################

write.csv(
  hourly_all,
  file.path(out_dir, "IID_Heatmap_1hr_All_Calc.csv"),
  row.names = FALSE
)

write.csv(
  lightdark_all,
  file.path(out_dir, "IID_Heatmap_12hr_All_Calc.csv"),
  row.names = FALSE
)

write_xlsx(
  list(
    "1h_All" = hourly_all,
    "1h_Male" = hourly_male,
    "1h_Female" = hourly_female,
    "12h_All" = lightdark_all,
    "12h_Male" = lightdark_male,
    "12h_Female" = lightdark_female
  ),
  file.path(out_dir, "IID_Heatmap_Calculation_Summary.xlsx")
)

###########################################
# Plot functions
###########################################

save_png_pdf <- function(plot_obj, base_name, out_dir, w = 14, h = 6.5, dpi = 300) {
  ggsave(
    file.path(out_dir, paste0(base_name, ".png")),
    plot_obj,
    width = w, height = h, dpi = dpi,
    bg = "white"
  )
  ggsave(
    file.path(out_dir, paste0(base_name, ".pdf")),
    plot_obj,
    width = w, height = h,
    device = cairo_pdf,
    bg = "white"
  )
}

plot_hourly_heatmap <- function(df, group_label, out_dir) {
  zt_levels <- levels(df$ZT_Label)
  
  p <- ggplot(df, aes(x = ZT_Label, y = Pair, fill = Ratio)) +
    geom_tile(color = "black", linewidth = 0.1, na.rm = TRUE) +
    scale_fill_gradientn(
      colors = c("#0000FF", "#ADD8E6", "white"),
      values = scales::rescale(c(0, 100)),
      limits = c(0, 100),
      oob = scales::squish,
      breaks = c(0, 25, 50, 75, 100),
      labels = c("0", "25", "50", "75", "100"),
      name = "Ratio (%)"
    ) +
    labs(
      title = "Relative inter-indiv dist",
      x = "Day and ZT",
      y = "Mouse pair ID"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      axis.text.x  = element_text(angle = 45, hjust = 1, size = 14),
      axis.text.y  = element_text(size = 14),
      axis.title.x = element_text(size = 16, face = "bold"),
      axis.title.y = element_text(size = 16, face = "bold"),
      plot.title   = element_text(size = 18, face = "bold"),
      legend.title = element_text(size = 14),
      legend.text  = element_text(size = 12)
    ) +
    scale_x_discrete(breaks = zt_levels[seq(1, length(zt_levels), by = 3)])
  
  save_png_pdf(
    p,
    paste0("IID_Heatmap_1hr_", group_label, "8"),
    out_dir
  )
}

plot_lightdark_heatmap <- function(df, group_label, out_dir) {
  p <- ggplot(df, aes(x = Period_Label, y = Pair, fill = Ratio)) +
    geom_tile(color = "black", linewidth = 0.1, na.rm = TRUE) +
    scale_fill_gradientn(
      colors = c("#0000FF", "#ADD8E6", "white"),
      values = scales::rescale(c(0, 100)),
      limits = c(0, 100),
      oob = scales::squish,
      breaks = c(0, 25, 50, 75, 100),
      labels = c("0", "25", "50", "75", "100"),
      name = "Ratio (%)"
    ) +
    labs(
      title = "Relative inter-indiv dist",
      x = "",
      y = "Mouse pair ID"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      axis.text.x  = element_text(angle = 45, hjust = 1, size = 14),
      axis.text.y  = element_text(size = 14),
      axis.title.x = element_text(size = 16, face = "bold"),
      axis.title.y = element_text(size = 16, face = "bold"),
      plot.title   = element_text(size = 18, face = "bold"),
      legend.title = element_text(size = 14),
      legend.text  = element_text(size = 12)
    )
  
  save_png_pdf(
    p,
    paste0("IID_Heatmap_12hr_", group_label, "8"),
    out_dir
  )
}

###########################################
# Run
###########################################

plot_hourly_heatmap(hourly_male,   "Male",   out_dir)
plot_hourly_heatmap(hourly_female, "Female", out_dir)

plot_lightdark_heatmap(lightdark_male,   "Male",   out_dir)
plot_lightdark_heatmap(lightdark_female, "Female", out_dir)

cat("✅ All IID heatmaps and calculation tables have been exported to -> ", out_dir, "\n")