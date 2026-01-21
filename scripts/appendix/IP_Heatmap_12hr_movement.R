# --- Required packages ---
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tools)
library(writexl)   # Added: for saving Excel files

# --- Interactive file selection (Male → Female) ---
cat("▶ Please select the Male file\n")
file_male <- file.choose()

cat("▶ Please select the Female file\n")
file_female <- file.choose()

# --- Load data ---
df_male   <- read_excel(file_male) %>% mutate(Group = "Male")
df_female <- read_excel(file_female) %>% mutate(Group = "Female")

# --- ZT_Label order (Light → Dark) ---
zt_order <- c(
  "Day1 Light", "Day1 Dark",
  "Day2 Light", "Day2 Dark",
  "Day3 Light", "Day3 Dark",
  "Day4 Light", "Day4 Dark"
)

# --- Preprocessing ---
process_df <- function(df) {
  df %>%
    mutate(
      ZT_Label = factor(ZT_Label, levels = zt_order),
      ID = factor(ID, levels = rev(unique(ID)))  # Put ID1 at the top
    )
}

df_male   <- process_df(df_male)
df_female <- process_df(df_female)

# --- Create output folder ---
out_dir <- file.path(dirname(file_male), "Heatmap_LightDark_Output")
if (!dir.exists(out_dir)) dir.create(out_dir)

# --- Heatmap plotting function (expects a "Ratio" column) ---
plot_heatmap <- function(df, group_label, out_dir) {
  
  p <- ggplot(df, aes(x = ZT_Label, y = ID, fill = Ratio)) +
    geom_tile(color = "black", size = 0.1) +
    scale_fill_gradientn(
      colors = c("white", "red"),
      values = scales::rescale(c(0, 100)),
      limits = c(0, 100),
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
      axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
      axis.text.y = element_text(size = 14),
      axis.title.x = element_text(size = 16, face = "bold"),
      axis.title.y = element_text(size = 16, face = "bold"),
      plot.title = element_text(size = 18, face = "bold"),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12)
    )
  
  # --- Save PNG ---
  out_file_png <- file.path(out_dir, paste0("TravelDistanceratio_LightDark_", group_label, ".png"))
  ggsave(out_file_png, p, width = 14, height = 4.2, dpi = 300)
  cat("✅ PNG saved:", out_file_png, "\n")
  
  # --- Save PDF (default) ---
  out_file_pdf <- file.path(out_dir, paste0("TravelDistanceratio_LightDark_", group_label, ".pdf"))
  ggsave(out_file_pdf, p, width = 14, height = 4.2)  # no explicit device
  cat("✅ PDF saved:", out_file_pdf, "\n")
  
  # --- Save Excel ---
  out_file_excel <- file.path(out_dir, paste0("TravelDistanceRatio_LightDark_", group_label, ".xlsx"))
  write_xlsx(df %>% arrange(ID, ZT_Label), out_file_excel)
  cat("✅ Excel saved:", out_file_excel, "\n")
}

# --- Run ---
plot_heatmap(df_male, "Male8", out_dir)
plot_heatmap(df_female, "Female8", out_dir)

cat("✅ All heatmaps have been exported to -> ", out_dir, "\n")
