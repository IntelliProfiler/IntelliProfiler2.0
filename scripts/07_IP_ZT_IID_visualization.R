###########################################
# Script name : 07_IP_ZT_IID_visualization.R
# Description : Visualize pairwise inter-individual distance (IID) across Zeitgeber Time (ZT) with hourly and light/dark summaries
# Authors     : Shohei Ochi, Masashi Azuma
# Version history:
#   v1.1 - 2025-01-20 - Shohei Ochi, Masashi Azuma
#   v1.2 - 2026-03-31 - Shohei Ochi, Masashi Azuma
###########################################
# --- Required packages ---
library(readxl)
library(dplyr)
library(ggplot2)

# --- Interactive file selection ---
cat("▶ Please select the file (ZT_converted_Hourly_IID.xlsx)\n")
file_path <- file.choose()
cat("▶ Selected file:", file_path, "\n")

# --- Load data ---
data <- read_excel(file_path)

# --- Extract numeric ZT and Day ---
data <- data %>%
  mutate(
    Pair = paste(ID_1, ID_2, sep = "-"),
    ZT   = as.integer(sub(".*ZT", "", ZT_Label)),
    Day  = as.integer(sub("Day(\\d+).*", "\\1", ZT_Label))
  )

pair_levels <- sort(unique(data$Pair))

# --- Color generation function for each pair ---
generate_color_by_index <- function(index) {
  h <- (index * 47) %% 360
  c <- 60 + (index * 23) %% 40
  l <- 55 + (index * 31) %% 30
  hcl(h = h, c = c, l = l)
}

# --- Assign unique colors for each pair ---
palette_pair <- setNames(
  sapply(seq_along(pair_levels), generate_color_by_index),
  pair_levels
)

# --- Output directory ---
file_base_dir <- dirname(file_path)
out_dir <- file.path(file_base_dir, "IID")

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
  cat("▶ Created folder:", out_dir, "\n")
} else {
  cat("▶ Using existing folder:", out_dir, "\n")
}

# --- Helper function to save both PNG and PDF ---
save_png_pdf <- function(plot_obj, base_name, out_dir, w = 10, h = 6, dpi = 300) {
  ggsave(file.path(out_dir, paste0(base_name, ".png")), plot_obj, width = w, height = h, dpi = dpi)
  ggsave(file.path(out_dir, paste0(base_name, ".pdf")), plot_obj, width = w, height = h, device = cairo_pdf)
}

# --- Plot and save for each pair ---
unique_pairs <- unique(data$Pair)
df_all_light_dark <- data.frame()

for (pair in unique_pairs) {
  df_plot <- filter(data, Pair == pair) %>%
    arrange(Day, ZT) %>%
    mutate(Index = row_number())
  
  # Group the dark phase (ZT12–ZT23) for background shading
  grey_blocks <- df_plot %>%
    filter(ZT >= 12 & ZT <= 23) %>%
    group_by(Day) %>%
    summarise(
      xmin = min(Index),
      xmax = max(Index) + 1,
      .groups = "drop"
    )
  
  # Define axis labels every 3 hours
  label_interval <- 3
  label_indices <- df_plot$Index[df_plot$ZT %% label_interval == 0]
  label_texts   <- df_plot$ZT_Label[df_plot$ZT %% label_interval == 0]
  
  # Plot hourly inter-individual distance
  p <- ggplot(df_plot, aes(x = Index, y = Avg_Inter_Individual_Distance)) +
    geom_rect(
      data = grey_blocks,
      aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
      fill = "grey80", alpha = 0.5, inherit.aes = FALSE
    ) +
    geom_line(color = palette_pair[[pair]]) +
    geom_point(color = palette_pair[[pair]]) +
    scale_x_continuous(breaks = label_indices, labels = label_texts) +
    scale_y_continuous(limits = c(0, 40)) +
    labs(
      title = paste("Inter-Individual Distance -", pair),
      x = "Day and ZT",
      y = "Average inter-individual distance (cm)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )
  
  save_png_pdf(p, paste0("IID_ZT_", pair), out_dir)
  
  # --- Calculate Light/Dark mean per day ---
  df_light_dark <- df_plot %>%
    mutate(
      LightDark = ifelse(ZT < 12, "Light", "Dark"),
      Period_Label = paste0("Day", Day, " ", LightDark)
    ) %>%
    group_by(Day, LightDark, Period_Label) %>%
    summarise(
      Avg_Inter_Individual_Distance_Mean = mean(Avg_Inter_Individual_Distance, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(LightDark_Order = ifelse(grepl("Light", Period_Label), 1, 2)) %>%
    arrange(Day, LightDark_Order) %>%
    mutate(Index = row_number())
  
  # Background color for Light/Dark plot
  bg_blocks <- df_light_dark %>%
    mutate(LightDarkColor = ifelse(LightDark == "Light", "white", "grey80"))
  
  # Plot Light/Dark mean
  p_ld <- ggplot(df_light_dark, aes(x = Index, y = Avg_Inter_Individual_Distance_Mean)) +
    geom_rect(
      data = bg_blocks,
      aes(xmin = Index - 0.5, xmax = Index + 0.5, ymin = -Inf, ymax = Inf, fill = LightDarkColor),
      alpha = 0.5, inherit.aes = FALSE
    ) +
    scale_fill_identity() +
    geom_line(color = palette_pair[[pair]]) +
    geom_point(color = palette_pair[[pair]]) +
    scale_x_continuous(breaks = df_light_dark$Index, labels = df_light_dark$Period_Label) +
    scale_y_continuous(limits = c(0, 40)) +
    labs(
      title = paste("Inter-Individual Distance -", pair),
      x = "Day and phase",
      y = "Average inter-individual distance (cm)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )
  
  save_png_pdf(p_ld, paste0("IID_LightDarkMean_", pair), out_dir)
  
  df_light_dark$Pair <- pair
  df_all_light_dark <- rbind(df_all_light_dark, df_light_dark)
}

# --- Generate combined summary plot (all pairs, hourly) ---
time_index_all <- data %>%
  distinct(Day, ZT, ZT_Label) %>%
  arrange(Day, ZT) %>%
  mutate(Index = row_number())

data_all_hr <- data %>%
  left_join(time_index_all, by = c("Day", "ZT", "ZT_Label"))

grey_blocks_hr <- time_index_all %>%
  filter(ZT >= 12 & ZT <= 23) %>%
  group_by(Day) %>%
  summarise(
    xmin = min(Index),
    xmax = max(Index) + 1,
    .groups = "drop"
  )

zt_labels_all <- time_index_all %>%
  filter(ZT %% 3 == 0)

p_all_hr <- ggplot(
  data_all_hr,
  aes(x = Index, y = Avg_Inter_Individual_Distance, color = factor(Pair), group = Pair)
) +
  geom_rect(
    data = grey_blocks_hr,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    fill = "grey80", alpha = 0.5, inherit.aes = FALSE
  ) +
  geom_line() +
  geom_point() +
  scale_x_continuous(
    breaks = zt_labels_all$Index,
    labels = zt_labels_all$ZT_Label
  ) +
  scale_y_continuous(limits = c(0, 40)) +
  scale_color_manual(values = palette_pair) +
  labs(
    title = "Inter-Individual Distance - All Pairs",
    x = "Day and ZT",
    y = "Average inter-individual distance (cm)",
    color = "Pair"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

save_png_pdf(p_all_hr, "IID_ZT_AllPairs", out_dir)

# --- Generate combined Light/Dark summary plot (all pairs) ---
bg_blocks_all <- df_all_light_dark %>%
  distinct(Index, Period_Label) %>%
  mutate(LightDarkColor = ifelse(grepl("Light", Period_Label), "white", "grey80"))

p_all_ld <- ggplot(
  df_all_light_dark,
  aes(x = Index, y = Avg_Inter_Individual_Distance_Mean, color = factor(Pair), group = Pair)
) +
  geom_rect(
    data = bg_blocks_all,
    aes(xmin = Index - 0.5, xmax = Index + 0.5, ymin = -Inf, ymax = Inf, fill = LightDarkColor),
    alpha = 0.5, inherit.aes = FALSE
  ) +
  scale_fill_identity() +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = bg_blocks_all$Index, labels = bg_blocks_all$Period_Label) +
  scale_y_continuous(limits = c(0, 40)) +
  scale_color_manual(values = palette_pair) +
  labs(
    title = "Inter-Individual Distance - All Pairs",
    x = "Day and phase",
    y = "Average inter-individual distance (cm)",
    color = "Pair"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

save_png_pdf(p_all_ld, "IID_LightDarkMean_AllPairs", out_dir)