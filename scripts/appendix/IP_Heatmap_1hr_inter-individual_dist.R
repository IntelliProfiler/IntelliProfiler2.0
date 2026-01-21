# --- Required packages ---
library(readxl)
library(dplyr)
library(ggplot2)
library(writexl)
library(scales)   # Needed because values = scales::rescale(...) is used

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

# --- Compute Ratio (normalize by the global maximum across Male + Female) ---
# NOTE: inter-individual distance column is assumed to be
# 'Avg_InterIndividual_Distance'
max_value <- max(
  c(df_male$Avg_InterIndividual_Distance,
    df_female$Avg_InterIndividual_Distance),
  na.rm = TRUE
)

df_male <- df_male %>%
  mutate(
    ID = paste(ID_1, ID_2, sep = "_"),   # Create mouse-pair ID (e.g., ID1_ID2)
    Ratio = Avg_InterIndividual_Distance / max_value * 100
  )

df_female <- df_female %>%
  mutate(
    ID = paste(ID_1, ID_2, sep = "_"),
    Ratio = Avg_InterIndividual_Distance / max_value * 100
  )

# --- Normalize ZT_Label ordering (sort by numeric Day and ZT) ---
process_df <- function(df) {
  df %>%
    mutate(
      Day = as.numeric(gsub("Day(\\d+) ZT.*", "\\1", ZT_Label)),
      ZT  = as.numeric(gsub("Day\\d+ ZT(\\d+)", "\\1", ZT_Label))
    ) %>%
    arrange(Day, ZT) %>%
    mutate(
      ZT_Label = factor(ZT_Label, levels = unique(ZT_Label)),
      ID = factor(ID, levels = rev(unique(ID)))  # Put ID1_ID2 at the top
    )
}

df_male <- process_df(df_male)
df_female <- process_df(df_female)

# --- Create output folder ---
out_dir <- file.path(dirname(file_male), "Heatmap_Output_revised")
if (!dir.exists(out_dir)) dir.create(out_dir)

# --- Shared plotting function (one group at a time) ---
plot_heatmap <- function(df, group_label, out_dir) {
  
  zt_levels <- levels(df$ZT_Label)
  
  p <- ggplot(df, aes(x = ZT_Label, y = ID, fill = Ratio)) +
    geom_tile(color = "black", size = 0.1) +
    scale_fill_gradientn(
      colors = c("#0000FF", "#ADD8E6", "white"),
      values = scales::rescale(c(0, 100)),
      limits = c(0, 100),
      breaks = c(0, 25, 50, 75, 100),
      labels = c("0", "25", "50", "75", "100"),
      name = "Ratio (%)"
    ) +
    labs(
      title = "Relative inter-individual distance (ID)",
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
    scale_x_discrete(breaks = zt_levels[seq(1, length(zt_levels), by = 3)])
  
  # --- Save outputs ---
  ggsave(
    file.path(out_dir, paste0("Inter-indiv_dist_ratio_", group_label, "_blue_to_white.png")),
    p, width = 14, height = 6.5, dpi = 300
  )
  
  ggsave(
    file.path(out_dir, paste0("Inter-indiv_dist_ratio_", group_label, "_blue_to_white.pdf")),
    p, width = 14, height = 6.5
  )
  
  write_xlsx(
    df,
    file.path(out_dir, paste0("Inter-indiv_dist_ratio_", group_label, ".xlsx"))
  )
}

# --- Run ---
plot_heatmap(df_male, "Male8", out_dir)
plot_heatmap(df_female, "Female8", out_dir)

cat("✅ All inter-individual distance (ID) heatmaps exported ->", out_dir, "\n")

