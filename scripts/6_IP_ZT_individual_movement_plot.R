# --- Required packages ---
library(readxl)
library(dplyr)
library(ggplot2)

# --- Interactive file selection ---
cat("▶ Please select the file (ZT_converted_HM.xlsx)\n")
file_path <- file.choose()
cat("▶ Selected file:", file_path, "\n")

# --- Load data ---
data <- read_excel(file_path)

# --- Extract numeric ZT and Day, and assign time index ---
data <- data %>%
  mutate(
    ZT = as.numeric(sub(".*ZT", "", ZT_Label)),
    Day = as.numeric(sub("Day(\\d+).*", "\\1", ZT_Label)),
    Index = row_number()
  )

# --- Color generation function for each ID ---
generate_color_by_index <- function(index) {
  h <- (index * 47) %% 360
  c <- 60 + (index * 23) %% 40
  l <- 55 + (index * 31) %% 30
  hcl(h = h, c = c, l = l)
}

# --- Assign unique colors for each animal ID ---
id_levels <- sort(unique(data$ID))
palette_id <- setNames(
  sapply(seq_along(id_levels), generate_color_by_index),
  id_levels
)

# --- Output directory (same parent as input file) ---
# Recommended: use a dataset-specific folder name to avoid mixing outputs
file_base_dir <- dirname(file_path)
out_dir <- file.path(file_base_dir, "Visualization", "LinePlots", "HM_Movement")

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
  cat("▶ Created folder:", out_dir, "\n")
} else {
  cat("▶ Using existing folder:", out_dir, "\n")
}

# --- Helper function to save both PNG and PDF ---
save_png_pdf <- function(plot_obj, base_name, out_dir, w = 10, h = 4, dpi = 300) {
  ggsave(file.path(out_dir, paste0(base_name, ".png")), plot_obj, width = w, height = h, dpi = dpi)
  ggsave(file.path(out_dir, paste0(base_name, ".pdf")), plot_obj, width = w, height = h, device = cairo_pdf)
}

# --- Plot and export per animal ---
unique_ids <- unique(data$ID)
df_all_light_dark <- data.frame()

for (id in unique_ids) {
  df_plot <- filter(data, ID == id)
  
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
  
  # Plot hourly travel distance
  p <- ggplot(df_plot, aes(x = Index, y = Total_Distance)) +
    geom_rect(data = grey_blocks,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              fill = "grey80", alpha = 0.5, inherit.aes = FALSE) +
    geom_line(color = palette_id[[id]]) +
    geom_point(color = palette_id[[id]]) +
    scale_x_continuous(breaks = label_indices, labels = label_texts) +
    scale_y_continuous(limits = c(0, 15000)) +
    labs(title = paste("Travel Distance -", id),
         x = "Zeitgeber Time (ZT)", y = "Travel distance (cm)") +
    theme_minimal(base_size = 13)
  
  # Save PNG + PDF (prefix with HM_Movement to avoid confusion with other datasets)
  save_png_pdf(p, paste0("HM_Movement_ZT_", id), out_dir)
  
  # --- Calculate cumulative light/dark activity per day ---
  df_light_dark <- df_plot %>%
    mutate(
      LightDark = ifelse(ZT < 12, "Light", "Dark"),
      Period_Label = paste0("Day", Day, " ", LightDark)
    ) %>%
    group_by(Day, LightDark, Period_Label) %>%
    summarise(Total_Distance_Sum = sum(Total_Distance), .groups = "drop") %>%
    mutate(LightDark_Order = ifelse(grepl("Light", Period_Label), 1, 2)) %>%
    arrange(Day, LightDark_Order) %>%
    mutate(Index = row_number())
  
  # Create background color for Light/Dark plot
  bg_blocks <- df_light_dark %>%
    mutate(LightDarkColor = ifelse(LightDark == "Light", "white", "grey80"))
  
  # Plot daily cumulative light/dark activity
  p_ld <- ggplot(df_light_dark, aes(x = Index, y = Total_Distance_Sum)) +
    geom_rect(data = bg_blocks,
              aes(xmin = Index - 0.5, xmax = Index + 0.5, ymin = -Inf, ymax = Inf, fill = LightDarkColor),
              alpha = 0.5, inherit.aes = FALSE) +
    scale_fill_identity() +
    geom_line(color = palette_id[[id]]) +
    geom_point(color = palette_id[[id]]) +
    scale_x_continuous(breaks = df_light_dark$Index, labels = df_light_dark$Period_Label) +
    scale_y_continuous(limits = c(0, 65000)) +
    labs(title = paste("Cumulative Travel Distance -", id),
         x = "", y = "Travel distance (cm)") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "none")
  
  save_png_pdf(p_ld, paste0("HM_Movement_LightDarkSum_", id), out_dir)
  
  df_light_dark$ID <- id
  df_all_light_dark <- rbind(df_all_light_dark, df_light_dark)
}

# --- Generate combined summary plot (all animals, hourly) ---
ZT_levels <- data %>% arrange(Day, ZT) %>% pull(ZT_Label) %>% unique()
data$ZT_Label_Factor <- factor(data$ZT_Label, levels = ZT_levels)

bg_blocks_hr <- data %>%
  mutate(LightDark = ifelse(ZT < 12, "Light", "Dark")) %>%
  group_by(ZT_Label_Factor, LightDark) %>%
  summarise(.groups = "drop") %>%
  mutate(LightDarkColor = ifelse(LightDark == "Light", "white", "grey80"))

zt_labels_all <- data %>% filter(ZT %% 3 == 0) %>% pull(ZT_Label_Factor) %>% unique()

p_all_hr <- ggplot(data, aes(x = ZT_Label_Factor, y = Total_Distance, color = factor(ID), group = ID)) +
  geom_rect(data = bg_blocks_hr,
            aes(xmin = as.numeric(ZT_Label_Factor) - 0.5,
                xmax = as.numeric(ZT_Label_Factor) + 0.5,
                ymin = -Inf, ymax = Inf,
                fill = LightDarkColor),
            alpha = 0.5, inherit.aes = FALSE) +
  scale_fill_identity() +
  geom_line() +
  geom_point() +
  scale_x_discrete(breaks = zt_labels_all) +
  scale_y_continuous(limits = c(0, 15000)) +
  scale_color_manual(values = palette_id) +
  labs(title = "Travel Distance - All Animals",
       x = "Zeitgeber Time (ZT)", y = "Travel distance (cm)", color = "ID") +
  theme_minimal(base_size = 13)

save_png_pdf(p_all_hr, "HM_Movement_ZT_All", out_dir)

# --- Generate combined cumulative Light/Dark summary plot ---
bg_blocks_all <- df_all_light_dark %>%
  distinct(Index, Period_Label) %>%
  mutate(LightDarkColor = ifelse(grepl("Light", Period_Label), "white", "grey80"))

p_all_ld <- ggplot(df_all_light_dark, aes(x = Index, y = Total_Distance_Sum, color = factor(ID), group = ID)) +
  geom_rect(data = bg_blocks_all,
            aes(xmin = Index - 0.5, xmax = Index + 0.5, ymin = -Inf, ymax = Inf, fill = LightDarkColor),
            alpha = 0.5, inherit.aes = FALSE) +
  scale_fill_identity() +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = bg_blocks_all$Index, labels = bg_blocks_all$Period_Label) +
  scale_y_continuous(limits = c(0, 65000)) +
  scale_color_manual(values = palette_id) +
  labs(title = "Cumulative Travel Distance - All Animals",
       x = "", y = "Travel distance (cm)", color = "ID") +
  theme_minimal(base_size = 13)

save_png_pdf(p_all_ld, "HM_Movement_LightDarkSum_All", out_dir)
