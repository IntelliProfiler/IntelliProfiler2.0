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

# --- Read data and rename distance variable to ID ---
df_male <- read_excel(file_male) %>%
  rename(Avg_InterIndividual_Distance = Avg_Social_Distance) %>%
  mutate(Group = "Male")

df_female <- read_excel(file_female) %>%
  rename(Avg_InterIndividual_Distance = Avg_Social_Distance) %>%
  mutate(Group = "Female")

# --- Compute the global maximum of inter-individual distance (ID) ---
max_value <- max(
  c(df_male$Avg_InterIndividual_Distance,
    df_female$Avg_InterIndividual_Distance),
  na.rm = TRUE
)
cat("✅ Global maximum (inter-individual distance, ID) =", max_value, "\n")

# --- Compute Ratio (normalize to the global maximum; 0–100%) ---
df_male <- df_male %>%
  mutate(
    Ratio = Avg_InterIndividual_Distance / max_value * 100,
    ID = factor(ID, levels = rev(unique(ID)))  # Put ID1_ID2 at the top
  )

df_female <- df_female %>%
  mutate(
    Ratio = Avg_InterIndividual_Distance / max_value * 100,
    ID = factor(ID, levels = rev(unique(ID)))
  )

# --- Normalize ZT_Label ordering (Light -> Dark, Day1 -> Day4) ---
zt_order <- c(
  "Day1 Light", "Day1 Dark",
  "Day2 Light", "Day2 Dark",
  "Day3 Light", "Day3 Dark",
  "Day4 Light", "Day4 Dark"
)

process_df <- function(df) {
  df %>%
    mutate(ZT_Label = factor(ZT_Label, levels = zt_order)) %>%
    arrange(ID, ZT_Label)
}

df_male <- process_df(df_male)
df_female <- process_df(df_female)

# --- Create output folder ---
out_dir <- file.path(dirname(file_male), "Heatmap_Output")
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
      x = "",
      y = "Mouse pair ID"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
      axis.text.y = element_text(size = 14),
      plot.title  = element_text(size = 18, face = "bold"),
      legend.title = element_text(size = 14),
      legend.text  = element_text(size = 12)
    ) +
    scale_x_discrete(breaks = zt_levels)
  
  # --- Save outputs ---
  ggsave(
    file.path(out_dir, paste0("ID_Ratio12hr_", group_label, "_blue_to_white.png")),
    p, width = 14, height = 6.5, dpi = 300
  )
  
  ggsave(
    file.path(out_dir, paste0("ID_Ratio12hr_", group_label, "_blue_to_white.pdf")),
    p, width = 14, height = 6.5, dpi = 300
  )
  
  write_xlsx(
    df,
    file.path(out_dir, paste0("ID_Ratio12hr_", group_label, ".xlsx"))
  )
}

# --- Run ---
plot_heatmap(df_male, "Male8", out_dir)
plot_heatmap(df_female, "Female8", out_dir)

cat("✅ All inter-individual distance (ID) 12hr heatmaps exported ->", out_dir, "\n")
