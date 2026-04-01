###########################################
# Script name : 14_IP_ZT_CCR_DLRatio.R
# Description : Calculate dark/light ratio (%) based on close contact ratio (CCR) across Zeitgeber Time (ZT) and perform group comparison using linear mixed models
# Authors     : Shohei Ochi, Masashi Azuma
# Version history:
#   v1.1 - 2025-01-20 - Shohei Ochi, Masashi Azuma
#   v1.2 - 2026-03-31 - Shohei Ochi, Masashi Azuma
###########################################
# --- Required packages ---
library(readxl)
library(dplyr)
library(ggplot2)
library(emmeans)
library(tidyr)
library(stringr)
library(pbkrtest)
library(lme4)
library(lmerTest)

###########################################
# Data loading and data frame preparation #
###########################################

# --- Select input files ---
cat("▶ Please select the Male data file\n")
file_male <- file.choose()
cat("▶ Please select the Female data file\n")
file_female <- file.choose()

group1_name <- "Male"
group2_name <- "Female"

# --- Create output directory ---
out_dir_base <- file.path(dirname(file_male), "CCR_DLR_Group_Comparison_Results")
if (!dir.exists(out_dir_base)) dir.create(out_dir_base, recursive = TRUE)

# --- Load data ---
read_group_data <- function(file, group_name) {
  read_excel(file) %>%
    mutate(
      CCR = as.numeric(CCR),
      ID = paste(ID_1, ID_2, sep = "-"),
      ZT = as.integer(sub(".*ZT", "", ZT_Label)),
      Day_num = as.integer(sub("Day(\\d+).*", "\\1", ZT_Label)),
      Day = factor(paste0("Day", Day_num), levels = c("Day1", "Day2", "Day3", "Day4")),
      Group = group_name
    )
}

df_male <- read_group_data(file_male, "Male")
df_female <- read_group_data(file_female, "Female")

df_all <- bind_rows(df_male, df_female) %>%
  mutate(
    Group = factor(Group, levels = c("Male", "Female"))
  )

df_ratio <- df_all %>%
  mutate(
    LightDark = ifelse(ZT < 12, "Light", "Dark")
  ) %>%
  group_by(Group, ID, Day, Day_num, LightDark) %>%
  summarise(
    Mean_CCR = mean(CCR, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = LightDark,
    values_from = Mean_CCR
  ) %>%
  mutate(
    `Dark/Light (%)` = ifelse(!is.na(Light) & Light != 0,
                              Dark / Light * 100,
                              NA_real_)
  ) %>%
  arrange(Group, ID, Day_num)

df_model <- df_ratio %>%
  filter(!is.na(`Dark/Light (%)`))

###########################################
# Color palette by ID
###########################################

pair_levels <- sort(unique(df_model$ID))

generate_color_by_index <- function(index) {
  h <- (index * 47) %% 360
  c <- 60 + (index * 23) %% 40
  l <- 55 + (index * 31) %% 30
  hcl(h = h, c = c, l = l)
}

palette_pair <- setNames(
  sapply(seq_along(pair_levels), generate_color_by_index),
  pair_levels
)

safe_name <- function(x) str_replace_all(x, "[^A-Za-z0-9._-]", "_")

###########################################
# Shared Y-axis range
###########################################

y_max_each  <- 100
y_max_group <- 100

###########################################
# Save plots for each ID
###########################################

save_png_pdf <- function(plot_obj, base_name, out_dir, w = 10, h = 4, dpi = 300) {
  ggsave(
    file.path(out_dir, paste0(base_name, ".png")),
    plot = plot_obj,
    width = w, height = h, dpi = dpi,
    bg = "white"
  )
  ggsave(
    file.path(out_dir, paste0(base_name, ".pdf")),
    plot = plot_obj,
    width = w, height = h,
    device = cairo_pdf,
    bg = "white"
  )
}

for (group_name in c("Male", "Female")) {
  
  df_group <- df_model %>% filter(Group == group_name)
  out_dir <- file.path(out_dir_base, group_name)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  pair_levels_group <- sort(unique(df_group$ID))
  
  for (pair_id in pair_levels_group) {
    df_plot <- df_group %>% filter(ID == pair_id)
    
    p <- ggplot(df_plot, aes(x = Day, y = `Dark/Light (%)`, group = 1)) +
      geom_line(color = palette_pair[[pair_id]], linewidth = 1.2) +
      geom_point(color = palette_pair[[pair_id]], size = 3) +
      scale_y_continuous() +
      coord_cartesian(ylim = c(0, y_max_each)) +
      labs(
        title = paste("Close contact ratio -", pair_id),
        x = "",
        y = "Dark/Light (%)"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor = element_line(color = "grey90", linewidth = 0.3),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(face = "bold", size = 14),
        axis.title.y = element_text(face = "bold", size = 14),
        plot.title = element_text(face = "bold", size = 16),
        legend.position = "none"
      )
    
    fn <- safe_name(pair_id)
    
    save_png_pdf(
      p,
      paste0("CCR_DLR_", group_name, "_", fn),
      out_dir
    )
    
    message("Saved: ", pair_id, " - ", group_name)
  }
  
  write.csv(
    df_group,
    file.path(out_dir, "CCR_DLR_Each_ID.csv"),
    row.names = FALSE
  )
}

###########################################
# Plot all IDs within each group
###########################################

for (group_name in c("Male", "Female")) {
  
  df_g <- df_model %>% filter(Group == group_name)
  out_dir_g <- file.path(out_dir_base, group_name)
  
  p_all <- ggplot(df_g, aes(x = Day, y = `Dark/Light (%)`, color = ID, group = ID)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    scale_color_manual(values = palette_pair) +
    scale_y_continuous() +
    coord_cartesian(ylim = c(0, y_max_group)) +
    labs(
      title = paste0("Close contact ratio - All IDs (", group_name, ")"),
      x = "",
      y = "Dark/Light (%)",
      color = "ID"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "grey80", linewidth = 0.5),
      panel.grid.minor = element_line(color = "grey90", linewidth = 0.3),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      axis.title.x = element_text(face = "bold", size = 14),
      axis.title.y = element_text(face = "bold", size = 14),
      plot.title = element_text(face = "bold", size = 16),
      legend.position = "right"
    )
  
  save_png_pdf(
    p_all,
    "CCR_DLR_AllIDs",
    out_dir_g
  )
}

###########################################
# Statistical analyses (LMM + Type III)
###########################################

options(contrasts = c("contr.sum", "contr.poly"))

fit_ratio <- lmer(
  `Dark/Light (%)` ~ Group * Day + (1 | ID),
  data = df_model
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
  file.path(out_dir_base, "CCR_DLR_ANOVA_TypeIII.csv"),
  row.names = FALSE
)

###########################################################
# Compare groups for each Day: emmeans + BH correction
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
  file.path(out_dir_base, "CCR_DLR_Result_BH.csv"),
  row.names = FALSE
)

###########################################
# Summary statistics for group plot
###########################################

summary_df <- df_model %>%
  group_by(Group, Day, Day_num) %>%
  summarise(
    Avg = mean(`Dark/Light (%)`, na.rm = TRUE),
    SD = sd(`Dark/Light (%)`, na.rm = TRUE),
    .groups = "drop"
  )

color_values <- c("Male" = "#377EB8", "Female" = "#E41A1C")
group_levels <- c("Male", "Female")

summary_df <- summary_df %>%
  mutate(Group = factor(Group, levels = group_levels))

day_max_values <- summary_df %>%
  group_by(Day) %>%
  summarise(max_y = max(Avg, na.rm = TRUE), .groups = "drop")

sig_stars <- result_table_ratio %>%
  filter(p_BH < 0.05) %>%
  mutate(
    winner = ifelse(diff > 0, "Male", "Female"),
    label_color = color_values[winner]
  ) %>%
  left_join(day_max_values, by = "Day") %>%
  mutate(y_pos = pmin(max_y + 10, 95))

###########################################
# Group comparison plot
###########################################

p_group <- ggplot(summary_df, aes(x = Day, y = Avg, color = Group, group = Group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_ribbon(
    aes(ymin = Avg - SD, ymax = Avg + SD, fill = Group),
    alpha = 0.2, color = NA
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
  scale_y_continuous(limits = c(0, y_max_group)) +
  geom_text(
    angle = 90,
    data = sig_stars,
    aes(x = Day, y = y_pos, label = sig_label),
    color = sig_stars$label_color,
    show.legend = FALSE,
    inherit.aes = FALSE,
    family = "Courier",
    size = 5,
    fontface = "bold"
  ) +
  labs(
    title = "Close contact ratio - group comparison",
    x = "",
    y = "Dark/Light (%)",
    color = "Group"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor = element_line(color = "grey90", linewidth = 0.3),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "right"
  )

save_png_pdf(
  p_group,
  "CCR_DLR_GroupComparison",
  out_dir_base
)

message("✅ All done: ", out_dir_base)