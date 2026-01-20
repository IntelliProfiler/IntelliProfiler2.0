# --- Required packages ---
library(readxl)
library(dplyr)
library(ggplot2)

# --- Interactive file selection ---
cat("▶ Please select the file (ZT_converted_IV.xlsx)\n")
file_path <- file.choose()
cat("▶ Selected file:", file_path, "\n")

# --- Load data ---
data <- read_excel(file_path)

# --- Extract numeric ZT and Day, and add Index ---
data <- data %>%
  mutate(
    Pair = paste(ID_1, ID_2, sep = "-"),
    ZT = as.numeric(sub(".*ZT", "", ZT_Label)),
    Day = as.numeric(sub("Day(\\d+).*", "\\1", ZT_Label)),
    Index = row_number()
  )

pair_levels <- sort(unique(data$Pair))

# --- Color generation function: vary hue + chroma + luminance to increase diversity ---
generate_color_by_index <- function(index) {
  h <- (index * 47) %% 360
  c <- 60 + (index * 23) %% 40
  l <- 55 + (index * 31) %% 30
  hcl(h = h, c = c, l = l)
}

# --- Assign a unique color to each pair (stable ordering) ---
palette_pair <- setNames(
  sapply(seq_along(pair_levels), generate_color_by_index),
  pair_levels
)

# --- Output directory (parent of input file) ---
file_base_dir <- dirname(file_path)

# --- Create output folder (including intermediate directories) ---
out_dir <- file.path(file_base_dir, "Visualization", "LinePlots", "Inter_Individual_Distance_Each_Pair")
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
  cat("▶ Created folder:", out_dir, "\n")
} else {
  cat("▶ Using existing folder:", out_dir, "\n")
}

# --- Helper function to save both PNG and PDF ---
save_png_pdf <- function(plot_obj, base_name, out_dir, w = 10, h = 4, dpi = 300) {
  ggsave(file.path(out_dir, paste0(base_name, ".png")), plot_obj, width = w, height = h, dpi = dpi)
  ggsave(file.path(out_dir, paste0(base_name, ".pdf")), plot_obj, width = w, height = h)
}

# --- Plot and save for each pair ---
unique_pairs <- unique(data$Pair)

# Initialize before loop
df_all_light_dark <- data.frame()

for (pair in unique_pairs) {
  df_plot <- filter(data, Pair == pair)
  
  # Shade dark phase (ZT12–ZT23) for each day
  grey_blocks <- df_plot %>%
    filter(ZT >= 12 & ZT <= 23) %>%
    group_by(Day) %>%
    summarise(
      xmin = min(Index),
      xmax = max(Index) + 1,
      .groups = "drop"
    )
  
  # Prepare x-axis labels every 3 hours
  label_interval <- 3
  label_indices <- df_plot$Index[df_plot$ZT %% label_interval == 0]
  label_texts   <- df_plot$ZT_Label[df_plot$ZT %% label_interval == 0]
  
  # Plot hourly inter-individual distance
  p <- ggplot(df_plot, aes(x = Index, y = Avg_Inter_Individual_Distance)) +
    geom_rect(data = grey_blocks,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              fill = "grey80", alpha = 0.5, inherit.aes = FALSE) +
    geom_line(color = palette_pair[[pair]]) +
    geom_point(color = palette_pair[[pair]]) +
    scale_x_continuous(breaks = label_indices, labels = label_texts) +
    scale_y_continuous(limits = c(0, 40)) +
    labs(title = paste("Inter-Individual Distance -", pair),
         x = "Zeitgeber Time (ZT)", y = "Average Inter-Individual Distance (cm)") +
    theme_minimal(base_size = 13) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold")
    )
  
  save_png_pdf(p, paste0("IV_ZT_", pair), out_dir)
  message("✅ Saved (Inter-Individual Distance): ", file.path(out_dir, paste0("IV_ZT_", pair, ".png / .pdf")))
  
  # --- Compute mean values for each Light/Dark period ---
  df_light_dark <- df_plot %>%
    mutate(
      LightDark = ifelse(ZT < 12, "Light", "Dark"),
      Period_Label = paste0("Day", Day, " ", LightDark)
    ) %>%
    group_by(Day, LightDark, Period_Label) %>%
    summarise(Avg_Inter_Individual_Distance_Mean = mean(Avg_Inter_Individual_Distance), .groups = "drop") %>%
    mutate(LightDark_Order = ifelse(grepl("Light", Period_Label), 1, 2)) %>%
    arrange(Day, LightDark_Order) %>%
    mutate(Index = row_number())
  
  # Background blocks for Light/Dark mean plot
  bg_blocks <- df_light_dark %>%
    mutate(LightDarkColor = ifelse(grepl("Light", Period_Label), "white", "grey80"))
  
  # Plot Light/Dark mean
  p_ld <- ggplot(df_light_dark, aes(x = Index, y = Avg_Inter_Individual_Distance_Mean)) +
    geom_rect(data = bg_blocks,
              aes(xmin = Index - 0.5, xmax = Index + 0.5, ymin = -Inf, ymax = Inf, fill = LightDarkColor),
              alpha = 0.5, inherit.aes = FALSE) +
    scale_fill_identity() +
    geom_line(color = palette_pair[[pair]]) +
    geom_point(color = palette_pair[[pair]]) +
    scale_x_continuous(breaks = df_light_dark$Index, labels = df_light_dark$Period_Label) +
    scale_y_continuous(limits = c(0, 40)) +
    labs(title = paste("Inter-Individual Distance -", pair),
         x = "", y = "Average Inter-Individual Distance (cm)") +
    theme_minimal(base_size = 13) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold"),
      legend.position = "none"
    )
  
  save_png_pdf(p_ld, paste0("IV_LightDarkMean_", pair), out_dir)
  message("✅ Saved (Light/Dark Mean): ", file.path(out_dir, paste0("IV_LightDarkMean_", pair, ".png / .pdf")))
  
  df_light_dark$Pair <- pair
  df_all_light_dark <- rbind(df_all_light_dark, df_light_dark)
}

# --- Convert ZT_Label to factor to fix ordering ---
ZT_Label_levels <- data %>%
  arrange(Day, ZT) %>%
  pull(ZT_Label) %>%
  unique()

data$ZT_Label_Factor <- factor(data$ZT_Label, levels = ZT_Label_levels)

# --- Create background data for Light/Dark (All Pairs; ZT_Label_Factor axis) ---
bg_blocks_hr_factor <- data %>%
  mutate(LightDark = ifelse(ZT < 12, "Light", "Dark")) %>%
  group_by(ZT_Label_Factor, LightDark) %>%
  summarise(.groups = "drop") %>%
  mutate(LightDarkColor = ifelse(LightDark == "Light", "white", "grey80"))

# --- Combined ZT plot for all pairs ---
zt_labels_all <- data %>%
  filter(ZT %% 3 == 0) %>%
  pull(ZT_Label_Factor) %>%
  unique()

p_all_hr <- ggplot(data, aes(x = ZT_Label_Factor, y = Avg_Inter_Individual_Distance, color = factor(Pair), group = Pair)) +
  geom_rect(data = bg_blocks_hr_factor,
            aes(xmin = as.numeric(ZT_Label_Factor) - 0.5,
                xmax = as.numeric(ZT_Label_Factor) + 0.5,
                ymin = -Inf, ymax = Inf,
                fill = LightDarkColor),
            alpha = 0.5, inherit.aes = FALSE) +
  scale_fill_identity() +
  geom_line() +
  geom_point() +
  scale_x_discrete(breaks = zt_labels_all) +
  scale_y_continuous(limits = c(0, 40)) +
  scale_color_manual(values = palette_pair) +
  labs(title = "Inter-Individual Distance - All Pairs",
       x = "Zeitgeber Time (ZT)", y = "Average Inter-Individual Distance (cm)", color = "Pair") +
  theme_minimal(base_size = 13) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

save_png_pdf(p_all_hr, "IV_ZT_AllPairs", out_dir)
message("✅ Saved (All Pairs ZT): ", file.path(out_dir, "IV_ZT_AllPairs.png / .pdf"))

# --- Create background data for Light/Dark (All Pairs) ---
bg_blocks_all <- df_all_light_dark %>%
  distinct(Index, Period_Label) %>%
  mutate(LightDarkColor = ifelse(grepl("Light", Period_Label), "white", "grey80"))

# --- Combined Light/Dark plot for all pairs ---
p_all_ld <- ggplot(df_all_light_dark, aes(x = Index, y = Avg_Inter_Individual_Distance_Mean, color = factor(Pair), group = Pair)) +
  geom_rect(data = bg_blocks_all,
            aes(xmin = Index - 0.5, xmax = Index + 0.5, ymin = -Inf, ymax = Inf, fill = LightDarkColor),
            alpha = 0.5, inherit.aes = FALSE) +
  scale_fill_identity() +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = bg_blocks_all$Index, labels = bg_blocks_all$Period_Label) +
  scale_y_continuous(limits = c(0, 40)) +
  scale_color_manual(values = palette_pair) +
  labs(title = "Inter-Individual Distance - All Pairs",
       x = "", y = "Average Inter-Individual Distance (cm)", color = "Pair") +
  theme_minimal(base_size = 13) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

save_png_pdf(p_all_ld, "IV_LightDarkMean_AllPairs", out_dir)
message("✅ Saved (All Pairs Light/Dark): ", file.path(out_dir, "IV_LightDarkMean_AllPairs.png / .pdf"))
