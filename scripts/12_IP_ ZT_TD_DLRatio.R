###########################################
# Script name : 12_IP_ZT_TD_DLRatio.R
# Description : Calculate dark/light ratio (%) based on travel distance (TD) across Zeitgeber Time (ZT) and perform group comparison using linear mixed models
# Authors     : Shohei Ochi, Masashi Azuma
# Version history:
#   v1.1 - 2025-01-20 - Shohei Ochi, Masashi Azuma
#   v1.2 - 2026-03-31 - Shohei Ochi, Masashi Azuma
###########################################
# Required packages
library(readxl)
library(stringr)
library(dplyr)
library(ggplot2)
library(emmeans)
library(tidyr)
library(purrr)
library(pbkrtest)
library(lme4)
library(lmerTest)

###########################################
# Data loading and data frame preparation #
###########################################

# Select files for Hourly TD
message("▶ Please select Male file")
file1 <- file.choose()
message("▶ Please select Female file")
file2 <- file.choose()

group1_name <- "Male"
group2_name <- "Female"

# Output directory
out_dir <- file.path(dirname(file1), "TD_DLR_Group_Comparison_Results")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
message("Results are stored in: ", out_dir)

# Load data and assign group labels
read_group_data <- function(file, group_name) {
  read_excel(file) %>%
    mutate(Group = group_name)
}

data1 <- read_group_data(file1, group1_name)
data2 <- read_group_data(file2, group2_name)
data_all <- bind_rows(data1, data2)

# Fix group order: Male → Female
data_all <- data_all %>%
  mutate(Group = factor(Group, levels = c("Male", "Female")))

# ------------------------------------------------------------
# Expected columns:
#   ID
#   ZT_Label   (e.g., "Day1 ZT0", "Day2 ZT13")
#   Total_Distance   or   ZT_converted_Hourly_TD
# ------------------------------------------------------------

# Use ZT_converted_Hourly_TD if present; otherwise use Total_Distance
if ("ZT_converted_Hourly_TD" %in% names(data_all)) {
  data_all <- data_all %>%
    mutate(Hourly_TD = as.numeric(ZT_converted_Hourly_TD))
} else if ("Total_Distance" %in% names(data_all)) {
  data_all <- data_all %>%
    mutate(Hourly_TD = as.numeric(Total_Distance))
} else {
  stop("Column 'ZT_converted_Hourly_TD' or 'Total_Distance' was not found.")
}

# Extract ZT and Day from ZT_Label
data_all <- data_all %>%
  mutate(
    ZT = as.numeric(sub(".*ZT", "", ZT_Label)),
    Day_num = as.numeric(sub("Day(\\d+).*", "\\1", ZT_Label)),
    Day = factor(paste0("Day", Day_num), levels = c("Day1", "Day2", "Day3", "Day4"))
  )

# Add Light/Dark information
data_all <- data_all %>%
  mutate(
    LightDark = ifelse(ZT < 12, "Light", "Dark")
  )

##########################################################
# Calculate Dark/Light ratio (%) for each individual/day #
##########################################################

# Sum within each ID x Day x LightDark
sum_df <- data_all %>%
  group_by(Group, ID, Day, Day_num, LightDark) %>%
  summarise(
    Sum_TD = sum(Hourly_TD, na.rm = TRUE),
    .groups = "drop"
  )

# Convert to wide format and calculate ratio
ratio_df <- sum_df %>%
  pivot_wider(
    names_from = LightDark,
    values_from = Sum_TD
  ) %>%
  mutate(
    `Dark/Light (%)` = ifelse(!is.na(Light) & Light != 0,
                              Dark / Light * 100,
                              NA_real_)
  ) %>%
  arrange(Group, ID, Day_num)

# Save calculated table: CSV only
write.csv(
  ratio_df,
  file.path(out_dir, "TD_DLR_Calculated.csv"),
  row.names = FALSE
)

########################################
# Summary statistics for plotting      #
########################################

summary_ratio <- ratio_df %>%
  group_by(Group, Day, Day_num) %>%
  summarise(
    Avg_Ratio = mean(`Dark/Light (%)`, na.rm = TRUE),
    SD = sd(`Dark/Light (%)`, na.rm = TRUE),
    .groups = "drop"
  )

##############################
# Statistical analyses (LMM) #
##############################

options(contrasts = c("contr.sum", "contr.poly"))

fit_ratio <- lmer(
  `Dark/Light (%)` ~ Group * Day + (1 | ID),
  data = ratio_df
)

aov_res_ratio <- anova(fit_ratio, type = 3)

aov_res_ratio$Effect <- rownames(aov_res_ratio)
colnames(aov_res_ratio)[which(colnames(aov_res_ratio) == "Pr(>F)")] <- "p_value"

aov_res_ratio <- aov_res_ratio %>%
  select(Effect, everything()) %>%
  mutate(sig_label = case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01  ~ "**",
    p_value < 0.05  ~ "*",
    TRUE ~ ""
  ))

write.csv(
  aov_res_ratio,
  file.path(out_dir, "TD_DLR_ANOVA_TypeIII.csv"),
  row.names = FALSE
)

###########################################################
# Compare groups for each Day: emmeans + BH correction    #
###########################################################

emm_ratio <- emmeans(fit_ratio, pairwise ~ Group | Day)
contrast_raw_ratio <- summary(emm_ratio$contrasts, adjust = "none")

contrast_df_ratio <- as.data.frame(contrast_raw_ratio)
contrast_df_ratio$p_BH <- p.adjust(contrast_df_ratio$p.value, method = "BH")

contrast_df_ratio <- contrast_df_ratio %>%
  mutate(
    Day_num = as.integer(str_extract(as.character(Day), "\\d+")),
    sig_label = case_when(
      p_BH < 0.001 ~ "***",
      p_BH < 0.01  ~ "**",
      p_BH < 0.05  ~ "*",
      TRUE ~ ""
    )
  ) %>%
  arrange(Day_num)

result_table_ratio <- contrast_df_ratio %>%
  rename(diff = estimate, p_raw = p.value)

write.csv(
  result_table_ratio,
  file.path(out_dir, "TD_DLR_Result_BH.csv"),
  row.names = FALSE
)

############################
# Color palette by ID      #
############################

generate_color_by_index <- function(index) {
  h <- (index * 47) %% 360
  c <- 60 + (index * 23) %% 40
  l <- 55 + (index * 31) %% 30
  hcl(h = h, c = c, l = l)
}

id_levels <- sort(unique(as.character(ratio_df$ID)))
palette_id <- setNames(
  sapply(seq_along(id_levels), generate_color_by_index),
  id_levels
)

# Group comparison colors
color_values <- c("Male" = "#377EB8", "Female" = "#E41A1C")
group_levels <- c("Male", "Female")

####################
# Figure plotting  #
####################

summary_ratio <- summary_ratio %>%
  mutate(Group = factor(Group, levels = group_levels))

day_max_values <- summary_ratio %>%
  group_by(Day) %>%
  summarise(max_y = max(Avg_Ratio, na.rm = TRUE), .groups = "drop")

sig_stars_ratio <- result_table_ratio %>%
  filter(p_BH < 0.05) %>%
  mutate(
    winner = ifelse(diff > 0, "Male", "Female"),
    label_color = color_values[winner]
  ) %>%
  left_join(day_max_values, by = "Day") %>%
  mutate(y_pos = pmin(max_y + 40, 440))

p_ratio <- ggplot(
  summary_ratio,
  aes(x = Day, y = Avg_Ratio, color = Group, group = Group)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_ribbon(
    aes(
      ymin = Avg_Ratio - SD,
      ymax = Avg_Ratio + SD,
      fill = Group
    ),
    alpha = 0.2,
    color = NA
  ) +
  scale_color_manual(
    values = color_values,
    breaks = group_levels,
    limits = group_levels
  ) +
  scale_fill_manual(
    values = color_values,
    breaks = group_levels,
    limits = group_levels
  ) +
  scale_y_continuous(limits = c(0, 450)) +
  geom_text(
    angle = 90,
    data = sig_stars_ratio,
    aes(x = Day, y = y_pos, label = sig_label),
    color = sig_stars_ratio$label_color,
    show.legend = FALSE,
    inherit.aes = FALSE,
    family = "Courier",
    size = 5,
    fontface = "bold"
  ) +
  labs(
    title = "Travel distance - group comparison",
    x = "",
    y = "Dark/Light (%)",
    color = "Group"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold")
  )

# Save group comparison
ggsave(
  file.path(out_dir, "TD_DLR_GroupComparison.png"),
  p_ratio,
  width = 10,
  height = 4,
  dpi = 300,
  bg = "white"
)

ggsave(
  file.path(out_dir, "TD_DLR_GroupComparison.pdf"),
  p_ratio,
  width = 10,
  height = 4,
  device = cairo_pdf,
  bg = "white"
)

##############################################
# Per-ID plots for Male and Female separately #
##############################################

save_png_pdf <- function(plot_obj, base_name, out_dir, w = 10, h = 4, dpi = 300) {
  ggsave(
    file.path(out_dir, paste0(base_name, ".png")),
    plot_obj,
    width = w,
    height = h,
    dpi = dpi,
    bg = "white"
  )
  ggsave(
    file.path(out_dir, paste0(base_name, ".pdf")),
    plot_obj,
    width = w,
    height = h,
    device = cairo_pdf,
    bg = "white"
  )
}

out_dir_male <- file.path(out_dir, "Male")
out_dir_female <- file.path(out_dir, "Female")
if (!dir.exists(out_dir_male)) dir.create(out_dir_male, recursive = TRUE)
if (!dir.exists(out_dir_female)) dir.create(out_dir_female, recursive = TRUE)

for (grp in c("Male", "Female")) {
  
  df_grp <- ratio_df %>%
    filter(Group == grp) %>%
    mutate(Day = factor(Day, levels = c("Day1", "Day2", "Day3", "Day4")))
  
  ids_grp <- unique(df_grp$ID)
  out_subdir <- ifelse(grp == "Male", out_dir_male, out_dir_female)
  
  for (id_now in ids_grp) {
    df_plot <- df_grp %>% filter(ID == id_now)
    
    p_id <- ggplot(df_plot, aes(x = Day, y = `Dark/Light (%)`, group = 1)) +
      geom_line(color = palette_id[[as.character(id_now)]], linewidth = 1.2) +
      geom_point(color = palette_id[[as.character(id_now)]], size = 3) +
      scale_y_continuous(limits = c(0, 450)) +
      labs(
        title = paste("Travel distance -", id_now),
        x = "",
        y = "Dark/Light (%)"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor = element_line(color = "grey90", linewidth = 0.3),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(face = "bold", size = 14),
        axis.title.y = element_text(face = "bold", size = 14),
        plot.title = element_text(face = "bold", size = 16),
        legend.position = "none"
      )
    
    save_png_pdf(
      p_id,
      paste0("TD_DLR_", grp, "_", id_now),
      out_subdir
    )
  }
}

message("Saved to: ", out_dir)