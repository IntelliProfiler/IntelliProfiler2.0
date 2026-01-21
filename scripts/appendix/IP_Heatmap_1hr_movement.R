# --- Required packages ---
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tools)
library(writexl)
library(scales)   # Needed because we use values = scales::rescale

# --- Select files (Male -> Female) ---
cat("▶ Please select the Male file\n")
file_male <- file.choose()

cat("▶ Please select the Female file\n")
file_female <- file.choose()

# --- Read data ---
df_male   <- read_excel(file_male) %>%
  mutate(Group = "Male")
df_female <- read_excel(file_female) %>%
  mutate(Group = "Female")

# --- Normalize ZT_Label (sort by numeric Day and ZT order) ---
process_df <- function(df) {
  df %>%
    mutate(
      Day = as.numeric(gsub("Day(\\d+) ZT.*", "\\1", ZT_Label)),
      ZT  = as.numeric(gsub("Day\\d+ ZT(\\d+)", "\\1", ZT_Label))
    ) %>%
    arrange(Day, ZT) %>%
    mutate(
      ZT_Label = factor(ZT_Label, levels = unique(ZT_Label)),
      ID = factor(ID, levels = rev(unique(ID)))  # Put ID1 at the top
    )
}

df_male   <- process_df(df_male)
df_female <- process_df(df_female)

# --- Create output folder ---
out_dir <- file.path(dirname(file_male), "Heatmap_Output")
if (!dir.exists(out_dir)) dir.create(out_dir)

# --- Common function (for one group) ---
plot_heatmap <- function(df, group_label, out_dir) {
  
  zt_levels <- levels(df$ZT_Label)
  
  p <- ggplot(df, aes(x = ZT_Label, y = ID, fill = Ratio)) +
    geom_tile(color = "black", size = 0.1) +  # Tile border: black
    scale_fill_gradientn(
      colors = c("white", "red"),  # 0 = white -> 100 = red
      values = scales::rescale(c(0, 100)),
      limits = c(0, 100),
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
  
  # --- Save (PNG) ---
  out_file <- file.path(out_dir, paste0("Traveldistanceratio_", group_label, "_white_to_red.png"))
  ggsave(out_file, p, width = 14, height = 4.2, dpi = 300)
  cat("✅ Saved:", out_file, "\n")
  
  # --- Save (PDF: default device) ---
  out_file_pdf <- file.path(out_dir, paste0("Traveldistanceratio_", group_label, "_white_to_red.pdf"))
  ggsave(out_file_pdf, p, width = 14, height = 4.2)  # no device specified
  cat("✅ PDF saved:", out_file_pdf, "\n")
  
  # --- Save (Excel) ---
  out_file_excel <- file.path(out_dir, paste0("Traveldistanceratio_", group_label, ".xlsx"))
  write_xlsx(df, out_file_excel)
  cat("✅ Excel saved:", out_file_excel, "\n")
}

# --- Run ---
plot_heatmap(df_male, "Male8", out_dir)
plot_heatmap(df_female, "Female8", out_dir)

cat("✅ All heatmaps exported ->", out_dir, "\n")
