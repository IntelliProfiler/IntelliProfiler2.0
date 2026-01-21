# --- Required packages ---
library(readxl)
library(dplyr)
library(ggplot2)
library(writexl)
library(scales)   # Required because values = scales::rescale(...) is used

# --- Select input files (Male -> Female) ---
cat("▶ Please select the Male file\n")
file_male <- file.choose()

cat("▶ Please select the Female file\n")
file_female <- file.choose()

# --- Read data ---
df_male <- read_excel(file_male) %>%
  mutate(Group = "Male")

df_female <- read_excel(file_female) %>%
  mutate(Group = "Female")

# --- Add Ratio column (CCR is already expressed as a percentage) ---
# Close_Contact_Ratio is directly used as the heatmap value
# Day and ZT are extracted numerically from ZT_Label for proper ordering
df_male <- df_male %>%
  mutate(
    ID = paste(ID_1, ID_2, sep = "_"),        # Create mouse-pair ID (e.g., ID1_ID2)
    Ratio = Close_Contact_Ratio,              # Close contact ratio (%)
    Day = as.numeric(gsub("Day(\\d+) ZT.*", "\\1", ZT_Label)),
    ZT  = as.numeric(gsub("Day\\d+ ZT(\\d+)", "\\1", ZT_Label))
  )

df_female <- df_female %>%
  mutate(
    ID = paste(ID_1, ID_2, sep = "_"),
    Ratio = Close_Contact_Ratio,
    Day = as.numeric(gsub("Day(\\d+) ZT.*", "\\1", ZT_Label)),
    ZT  = as.numeric(gsub("Day\\d+ ZT(\\d+)", "\\1", ZT_Label))
  )

# --- Normalize ZT_Label ordering ---
# Rows are ordered by numeric Day and ZT values,
# and factor levels are fixed accordingly for plotting
process_df <- function(df) {
  df %>%
    arrange(Day, ZT) %>%
    mutate(
      ZT_Label = factor(ZT_Label, levels = unique(ZT_Label)),
      ID = factor(ID, levels = rev(unique(ID)))  # Reverse order to align pairs vertically
    )
}

df_male   <- process_df(df_male)
df_female <- process_df(df_female)

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
      x = "Day and ZT",
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
    scale_x_discrete(
      breaks = zt_levels[seq(1, length(zt_levels), by = 3)]  # Show every third label to reduce clutter
    )
  
  # --- Save outputs (PNG) ---
  out_file_png <- file.path(
    out_dir,
    paste0("Closecontactratio_", group_label, "_blue_to_white.png")
  )
  ggsave(out_file_png, p, width = 14, height = 6.5, dpi = 300)
  cat("✅ PNG saved:", out_file_png, "\n")
  
  # --- Save outputs (PDF: default device) ---
  out_file_pdf <- file.path(
    out_dir,
    paste0("Closecontactratio_", group_label, "_blue_to_white.pdf")
  )
  ggsave(out_file_pdf, p, width = 14, height = 6.5)
  cat("✅ PDF saved:", out_file_pdf, "\n")
  
  # --- Save outputs (Excel) ---
  out_file_excel <- file.path(
    out_dir,
    paste0("Closecontactratio_", group_label, ".xlsx")
  )
  write_xlsx(df, out_file_excel)
  cat("✅ Excel saved:", out_file_excel, "\n")
}

# --- Run ---
plot_heatmap(df_male, "Male8", out_dir)
plot_heatmap(df_female, "Female8", out_dir)

cat("✅ All close contact ratio heatmaps have been exported ->", out_dir, "\n")
