###########################################
# Script name : 15_IP_ZT_TD_Heatmap.R
# Description : Generate 1-hour and 12-hour heatmaps of travel distance (TD) normalized across individuals and groups in Zeitgeber Time (ZT)
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

cat("▶ Please select the Male file (ZT_converted_Hourly_TD.xlsx)\n")
file_male <- file.choose()

cat("▶ Please select the Female file (ZT_converted_Hourly_TD.xlsx)\n")
file_female <- file.choose()

###########################################
# Load data
###########################################

read_td_data <- function(file, group_name) {
  df <- read_excel(file)
  
  if ("ZT_converted_Hourly_TD" %in% names(df)) {
    df <- df %>% mutate(Hourly_TD = as.numeric(ZT_converted_Hourly_TD))
  } else if ("Total_Distance" %in% names(df)) {
    df <- df %>% mutate(Hourly_TD = as.numeric(Total_Distance))
  } else {
    stop("Column 'ZT_converted_Hourly_TD' or 'Total_Distance' was not found.")
  }
  
  df %>%
    mutate(
      Group = group_name,
      ZT = as.integer(sub(".*ZT", "", ZT_Label)),
      Day = as.integer(sub("Day(\\d+).*", "\\1", ZT_Label))
    ) %>%
    arrange(Day, ZT)
}

df_male   <- read_td_data(file_male, "Male")
df_female <- read_td_data(file_female, "Female")

df_all <- bind_rows(df_male, df_female)

###########################################
# Output directory
###########################################

out_dir <- file.path(dirname(file_male), "TD_Heatmap_Output")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

###########################################
# Common settings
###########################################

# Global ID order shared across Male/Female
id_levels_global <- rev(sort(unique(df_all$ID)))

# Global 1hr ZT order shared across Male/Female
zt_levels_global <- df_all %>%
  distinct(Day, ZT, ZT_Label) %>%
  arrange(Day, ZT) %>%
  pull(ZT_Label)

# Global 12hr order shared across Male/Female
period_levels_global <- c(
  "Day1 Light", "Day1 Dark",
  "Day2 Light", "Day2 Dark",
  "Day3 Light", "Day3 Dark",
  "Day4 Light", "Day4 Dark"
)

###########################################
# 1hr data
# Normalize by common max across all IDs
# in Male + Female (16 IDs x 96 points)
###########################################

hourly_all <- df_all %>%
  mutate(
    ZT_Label = factor(ZT_Label, levels = zt_levels_global),
    ID = factor(ID, levels = id_levels_global)
  ) %>%
  arrange(Group, ID, Day, ZT)

hourly_global_max <- max(hourly_all$Hourly_TD, na.rm = TRUE)

hourly_all <- hourly_all %>%
  mutate(
    Ratio = ifelse(!is.na(Hourly_TD) & hourly_global_max > 0,
                   Hourly_TD / hourly_global_max * 100,
                   NA_real_)
  )

hourly_male   <- hourly_all %>% filter(Group == "Male")
hourly_female <- hourly_all %>% filter(Group == "Female")

###########################################
# 12hr data
# Light = ZT0-11 cumulative sum
# Dark  = ZT12-23 cumulative sum
# Normalize by common max across all IDs
# in Male + Female (16 IDs x 8 points)
###########################################

lightdark_all <- df_all %>%
  mutate(
    LightDark = ifelse(ZT < 12, "Light", "Dark"),
    Period_Label = paste0("Day", Day, " ", LightDark)
  ) %>%
  group_by(Group, ID, Day, LightDark, Period_Label) %>%
  summarise(
    Sum_TD = sum(Hourly_TD, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Period_Label = factor(Period_Label, levels = period_levels_global),
    ID = factor(ID, levels = id_levels_global)
  ) %>%
  arrange(Group, ID, Period_Label)

lightdark_global_max <- max(lightdark_all$Sum_TD, na.rm = TRUE)

lightdark_all <- lightdark_all %>%
  mutate(
    Ratio = ifelse(!is.na(Sum_TD) & lightdark_global_max > 0,
                   Sum_TD / lightdark_global_max * 100,
                   NA_real_)
  )

lightdark_male   <- lightdark_all %>% filter(Group == "Male")
lightdark_female <- lightdark_all %>% filter(Group == "Female")

###########################################
# Save calculation process as CSV/XLSX
###########################################

write.csv(
  hourly_all,
  file.path(out_dir, "TD_Heatmap_1hr_All_Calc.csv"),
  row.names = FALSE
)

write.csv(
  lightdark_all,
  file.path(out_dir, "TD_Heatmap_12hr_All_Calc.csv"),
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
  file.path(out_dir, "TD_Heatmap_Calculation_Summary.xlsx")
)

###########################################
# Plot functions
###########################################

save_png_pdf <- function(plot_obj, base_name, out_dir, w = 14, h = 4.2, dpi = 300) {
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
  
  p <- ggplot(df, aes(x = ZT_Label, y = ID, fill = Ratio)) +
    geom_tile(color = "black", linewidth = 0.1, na.rm = TRUE) +
    scale_fill_gradientn(
      colors = c("white", "red"),
      values = scales::rescale(c(0, 100)),
      limits = c(0, 100),
      oob = scales::squish,
      breaks = c(0, 25, 50, 75, 100),
      labels = c("0", "25", "50", "75", "100"),
      name = "Ratio (%)"
    ) +
    labs(
      title = "Relative travel distance",
      x = "Day and ZT",
      y = "Mouse ID"
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
    paste0("TD_Heatmap_1hr_", group_label, "8"),
    out_dir
  )
}

plot_lightdark_heatmap <- function(df, group_label, out_dir) {
  p <- ggplot(df, aes(x = Period_Label, y = ID, fill = Ratio)) +
    geom_tile(color = "black", linewidth = 0.1, na.rm = TRUE) +
    scale_fill_gradientn(
      colors = c("white", "red"),
      values = scales::rescale(c(0, 100)),
      limits = c(0, 100),
      oob = scales::squish,
      breaks = c(0, 25, 50, 75, 100),
      labels = c("0", "25", "50", "75", "100"),
      name = "Ratio (%)"
    ) +
    labs(
      title = "Relative travel distance",
      x = "",
      y = "Mouse ID"
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
    paste0("TD_Heatmap_12hr_", group_label, "8"),
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

cat("✅ All heatmaps and calculation tables have been exported to -> ", out_dir, "\n")