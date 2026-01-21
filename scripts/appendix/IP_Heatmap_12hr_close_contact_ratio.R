# --- Required packages ---
library(readxl)
library(dplyr)
library(ggplot2)
library(writexl)
library(scales)   

# --- Select input files (Male -> Female) ---
cat("▶ Please select the Male file\n")
file_male <- file.choose()

cat("▶ Please select the Female file\n")
file_female <- file.choose()

# --- Read data and extract basic variables ---
# ID: mouse pair identifier (ID_1 + ID_2)
# Day and ZT are parsed numerically from ZT_Label
df_male <- read_excel(file_male) %>%
  mutate(
    Group = "Male",
    ID = paste(ID_1, ID_2, sep = "_"),
    Day = as.numeric(gsub("Day(\\d+) ZT.*", "\\1", ZT_Label)),
    ZT  = as.numeric(gsub("Day\\d+ ZT(\\d+)", "\\1", ZT_Label))
  )

df_female <- read_excel(file_female) %>%
  mutate(
    Group = "Female",
    ID = paste(ID_1, ID_2, sep = "_"),
    Day = as.numeric(gsub("Day(\\d+) ZT.*", "\\1", ZT_Label)),
    ZT  = as.numeric(gsub("Day\\d+ ZT(\\d+)", "\\1", ZT_Label))
  )

# --- Function to assign Light / Dark phase ---
# Light: ZT < 12, Dark: ZT >= 12
assign_light_dark <- function(day, zt) {
  phase <- ifelse(zt < 12, "Light", "Dark")
  paste0("Day", day, " ", phase)
}

df_male <- df_male %>%
  mutate(LD_Label = assign_light_dark(Day, ZT))

df_female <- df_female %>%
  mutate(LD_Label = assign_light_dark(Day, ZT))

# --- Aggregate data (mean CCR within each 12-hour Light/Dark phase) ---
df_male_summary <- df_male %>%
  group_by(ID, LD_Label) %>%
  summarise(Ratio = mean(Close_Contact_Ratio, na.rm = TRUE)) %>%
  ungroup()

df_female_summary <- df_female %>%
  group_by(ID, LD_Label) %>%
  summarise(Ratio = mean(Close_Contact_Ratio, na.rm = TRUE)) %>%
  ungroup()

# --- Normalize Light/Dark label order (Day1 -> Day4, Light -> Dark) ---
zt_order <- c(
  "Day1 Light", "Day1 Dark",
  "Day2 Light", "Day2 Dark",
  "Day3 Light", "Day3 Dark",
  "Day4 Light", "Day4 Dark"
)

process_df <- function(df) {
  df %>%
    mutate(
      ZT_Label = factor(LD_Label, levels = zt_order),
      ID = factor(ID, levels = rev(unique(ID)))
    ) %>%
    arrange(ID, ZT_Label)
}

df_male_summary   <- process_df(df_male_summary)
df_female_summary <- process_df(df_female_summary)

# --- Create output folder ---
out_dir <- file.path(dirname(file_male), "Heatmap_Output")
if (!dir.exists(out_dir)) dir.create(out_dir)

# --- Shared plotting function (one group at a time) ---
plot_heatmap <- function(df, group_label, out_dir) {
  
  zt_levels <- levels(df$ZT_Label)
  
  p <- ggplot(df, aes(x = ZT_Label, y = ID, fill = Ratio)) +
    geom_tile(color = "black", size = 0.1) +  # Draw tiles with black borders
    scale_fill_gradientn(
      colors = c("white", "#ADD8E6", "#0000FF"),  # white -> light blue -> blue
      values = scales::rescale(c(0, 100)),
      limits = c(0, 100),
      breaks = c(0, 25, 50, 75, 100),
      labels = c("0", "25", "50", "75", "100"),
      name = "Ratio (%)"
    ) +
    labs(
      title = "Close contact ratio",
      x = "",
      y = "Mouse pair ID"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
      axis.text.y = element_text(size = 14),
      axis.title.x = element_text(size = 16, face = "bold"),
      axis.title.y = element_text(size = 16, face = "bold"),
      plot.title  = element_text(size = 18, face = "bold"),
      legend.title = element_text(size = 14),
      legend.text  = element_text(size = 12),
      legend.margin = margin(t = 10, b = 10, unit = "pt")
    ) +
    scale_x_discrete(breaks = zt_levels)
  
  # --- Save outputs (PNG) ---
  out_file_png <- file.path(
    out_dir,
    paste0("CloseContactRatio12hr_", group_label, "_blue_to_white.png")
  )
  ggsave(out_file_png, p, width = 14, height = 6.5, dpi = 300)
  cat("✅ PNG saved:", out_file_png, "\n")
  
  # --- Save outputs (PDF) ---
  out_file_pdf <- file.path(
    out_dir,
    paste0("CloseContactRatio12hr_", group_label, "_blue_to_white.pdf")
  )
  ggsave(out_file_pdf, p, width = 14, height = 6.5, dpi = 300)
  cat("✅ PDF saved:", out_file_pdf, "\n")
  
  # --- Save outputs (Excel) ---
  out_file_excel <- file.path(
    out_dir,
    paste0("CloseContactRatio12hr_", group_label, ".xlsx")
  )
  write_xlsx(df, out_file_excel)
  cat("✅ Excel saved:", out_file_excel, "\n")
}

# --- Run ---
plot_heatmap(df_male_summary, "Male8", out_dir)
plot_heatmap(df_female_summary, "Female8", out_dir)

cat("✅ All 12-hour close contact ratio heatmaps have been exported ->", out_dir, "\n")
