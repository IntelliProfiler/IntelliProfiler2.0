###########################################
# Script name : 09_IP_ZT_TD_plot.R
# Description : Perform group comparison of hourly travel distance (TD) across Zeitgeber Time (ZT) using linear mixed models and visualize results
# Authors     : Shohei Ochi, Masashi Azuma
# Version history:
#   v1.1 - 2025-01-20 - Shohei Ochi, Masashi Azuma
#   v2.1 - 2026-03-29 - Hitoshi Inada
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

# Select files for Hourly Travel Distance
message("▶ Please select Male file")
file1 <- file.choose()
message("▶ Please select Female file")
file2 <- file.choose()
group1_name <- "Male"
group2_name <- "Female"

# Output directory (same as Male file location)
out_dir <- file.path(dirname(file1), "TD_Group_Comparison_Results")
if (!dir.exists(out_dir))
  dir.create(out_dir, recursive = TRUE)
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

# Extract ZT, Day, and create display label
data_all <- data_all %>%
  mutate(
    ZT = as.numeric(sub(".*ZT", "", ZT_Label)),
    Day = as.numeric(sub("Day(\\d+).*", "\\1", ZT_Label)),
    ZT_Display = paste0("Day", Day, " ZT", ZT),
    ZT_Display = factor(ZT_Display, levels = unique(ZT_Display))
  )

# Calculate group-wise summary statistics
summary_data <- data_all %>%
  group_by(Group, ZT_Display, ZT, Day) %>%
  summarise(
    Avg_Distance = mean(Total_Distance),
    SD = sd(Total_Distance),
    .groups = "drop"
  )

# Add Light/Dark information and create Period_Label
data_all_sum12h <- data_all %>%
  mutate(
    LightDark = ifelse(ZT < 12, "Light", "Dark"),
    Period_Label = paste0("D", Day, "-", LightDark)
  )

# Convert Period_Label to factor in Light → Dark order
lightdark_order <- c("Light", "Dark")
period_levels_ordered <- data_all_sum12h %>%
  distinct(Period_Label, LightDark, Day) %>%
  mutate(LightDark_order = match(LightDark, lightdark_order)) %>%
  arrange(Day, LightDark_order) %>%
  pull(Period_Label)

# Convert Period_Label to factor + create Index
data_all_sum12h <- data_all_sum12h %>%
  mutate(
    Period_Label = factor(Period_Label, levels = period_levels_ordered),
    Index = as.numeric(Period_Label)
  )

#############################
# Statistical analyses (1h) #
#############################

# Analysis by Linear Mixed Model
fit_1h <- lmer(
  Total_Distance ~ Group * ZT_Label + (1 | ID),
  data = data_all
)

aov_res_1h <- anova(fit_1h, type = 3)

aov_res_1h$Effect <- rownames(aov_res_1h)
colnames(aov_res_1h)[which(colnames(aov_res_1h) == "Pr(>F)")] <- "p_value"

aov_res_1h <- aov_res_1h %>%
  select(Effect, everything()) %>%
  mutate(sig_label = case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01 ~ "**",
    p_value < 0.05 ~ "*",
    TRUE ~ ""
  ))

# Save anova result
write.csv(aov_res_1h, 
          file.path(out_dir, paste0("TD_ANOVA_TypeIII_1h_", Sys.Date(), ".csv")), 
          row.names = FALSE)


# Compare groups for each ZT_Label
emm_1h <- emmeans(fit_1h, pairwise ~ Group | ZT_Label)
contrast_raw_1h <- summary(emm_1h$contrasts, adjust = "none")

# p.adjust for 96 time points
contrast_df_1h <- as.data.frame(contrast_raw_1h)
contrast_df_1h$p_BH <- p.adjust(contrast_df_1h$p.value, method = "BH")

contrast_df_1h <- contrast_df_1h %>%
  mutate(
    day  = as.integer(str_extract(ZT_Label, "(?<=Day)\\d")),
    zt   = as.integer(str_extract(ZT_Label, "(?<=ZT)\\d+")),
    time_point = (day - 1) * 24 + zt,
    sig_label = case_when(
      p_BH < 0.001 ~ "***",
      p_BH < 0.01  ~ "**",
      p_BH < 0.05  ~ "*",
      TRUE          ~ ""
      ),
    ZT_Display = ZT_Label
  )

contrast_df_1h_sorted <- contrast_df_1h %>% arrange(time_point)

result_table_1h <- contrast_df_1h_sorted %>%
  rename(diff = estimate, p_raw = p.value)

# Save results
write.csv(result_table_1h,
          file.path(out_dir, paste0(
            "TD_Result_1h_BH_", Sys.Date(), ".csv"
          )),
          row.names = FALSE)

########################
# Figure Plotting (1h) #
########################

# Define dark phase blocks (ZT12–ZT23)
grey_blocks <- summary_data %>%
  filter(ZT >= 12 & ZT <= 23) %>%
  group_by(Day) %>%
  summarise(
    xmin = min(as.numeric(factor(
      ZT_Display, levels = levels(data_all$ZT_Display)
    ))),
    xmax = max(as.numeric(factor(
      ZT_Display, levels = levels(data_all$ZT_Display)
    ))) + 1,
    .groups = "drop"
  )

# Define labels every 3 hours
display_labels <- summary_data %>%
  filter(ZT %% 3 == 0) %>%
  pull(ZT_Display) %>%
  unique()

# Color settings
color_values <- setNames(c("#377EB8", "#E41A1C"), c(group1_name, group2_name))

# Maximum Avg_Distance for each ZT
zt_max_values <- summary_data %>%
  mutate(ZT_Display = as.character(ZT_Display)) %>%
  group_by(ZT_Display) %>%
  summarise(max_y = max(Avg_Distance), .groups = "drop") 

# Generate colored significance labels and positions
sig_stars <- result_table_1h %>%
  filter(p_BH < 0.05) %>%
  mutate(winner = ifelse(diff > 0, "Male", "Female"),
         label_color = color_values[winner]) %>%
  left_join(zt_max_values, by = "ZT_Display") %>%
  mutate(y_pos = max_y + 2000)

# Create plot
# Fully fix legend order
group_levels <- c("Male", "Female")

summary_data <- summary_data %>%
  mutate(Group = factor(Group, levels = group_levels))

p_group <- ggplot(summary_data,
                  aes(
                    x = ZT_Display,
                    y = Avg_Distance,
                    color = Group,
                    group = Group
                  )) +
  geom_rect(
    data = grey_blocks,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "grey80",
    alpha = 0.5,
    inherit.aes = FALSE
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  geom_ribbon(
    aes(
      ymin = Avg_Distance - SD,
      ymax = Avg_Distance + SD,
      fill = Group
    ),
    alpha = 0.2,
    color = NA
  ) +
  scale_x_discrete(breaks = display_labels) +
  scale_color_manual(values = color_values,
                     breaks = group_levels,
                     limits = group_levels) +
  scale_fill_manual(values = color_values,
                    breaks = group_levels,
                    limits = group_levels) +
  scale_y_continuous(limits = c(0, 15000)) +
  geom_text(angle = 90,
    data = sig_stars,
    aes(x = ZT_Display, y = y_pos, label = sig_label),
    color = sig_stars$label_color,
    show.legend = FALSE,
    inherit.aes = FALSE,
    family = "Courier",
    size = 5,
    fontface = "bold"
  ) +
  labs(
    title = "Travel distance - group comparison",
    x = "Day and ZT",
    y = "Travel distance (cm)",
    color = "Group"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ),
    legend.position = "right",
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

# PNG (transparent)
ggsave(
  file.path(
    out_dir,
    paste0("TD_GroupComparison_1h_", Sys.Date(), ".png")
  ),
  p_group,
  width = 10,
  height = 4,
  dpi = 300,
  bg = "transparent"
)

# PDF (transparent)
ggsave(
  file.path(
    out_dir,
    paste0("TD_GroupComparison_1h_", Sys.Date(), ".pdf")
  ),
  p_group,
  width = 10,
  height = 4,
  bg = "transparent"
)

message("Saved to: ", out_dir)

# 12-hour cumulative distance for each individual
individual_sum12h <- data_all_sum12h %>%
  group_by(Group, Period_Label, Index, ID) %>%
  summarise(Sum_Distance = sum(Total_Distance), .groups = "drop")

# Summary data for plotting
summary_data_12h <- individual_sum12h %>%
  group_by(Group, Period_Label, Index) %>%
  summarise(
    Avg_Distance = mean(Sum_Distance),
    SD = sd(Sum_Distance),
    .groups = "drop"
  )

# Background data for shading
bg_blocks_12h <- data_all_sum12h %>%
  distinct(Index, Period_Label, LightDark) %>%
  mutate(LightDarkColor = ifelse(LightDark == "Light", "white", "grey80"))

# Define dark phase blocks only (LightDark == "Dark")
grey_blocks_12h <- data_all_sum12h %>%
  filter(LightDark == "Dark") %>%
  distinct(Index, Period_Label) %>%
  mutate(xmin = Index - 0.5, xmax = Index + 0.5)


##############################
# Statistical analyses (12h) #  
##############################

# Analysis by Linear Mixed Model
fit_12h <- lmer(
  Sum_Distance ~ Group * Period_Label + (1 | ID),
  data = individual_sum12h
)

aov_res_12h <- anova(fit_12h, type = 3)

aov_res_12h$Effect <- rownames(aov_res_12h)
colnames(aov_res_12h)[which(colnames(aov_res_12h) == "Pr(>F)")] <- "p_value"

aov_res_12h <- aov_res_12h %>%
  select(Effect, everything()) %>%
  mutate(sig_label = case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01 ~ "**",
    p_value < 0.05 ~ "*",
    TRUE ~ ""
  ))

# Save anova result
write.csv(aov_res_12h, 
          file.path(out_dir, paste0("TD_ANOVA_TypeIII_12h_", Sys.Date(), ".csv")), 
          row.names = FALSE)

# Compare groups for each Period_Label
emm_12h <- emmeans(fit_12h, pairwise ~ Group | Period_Label)
contrast_raw_12h <- summary(emm_12h$contrasts, adjust = "none")

# p.adjust for 8 time points
contrast_df_12h <- as.data.frame(contrast_raw_12h)
contrast_df_12h$p_BH <- p.adjust(contrast_df_12h$p.value, method = "BH")

contrast_df_12h <- contrast_df_12h %>%
  mutate(
    sig_label = case_when(
      p_BH < 0.001 ~ "***",
      p_BH < 0.01  ~ "**",
      p_BH < 0.05  ~ "*",
      TRUE          ~ ""
    ),
  )

result_table_12h <- contrast_df_12h %>%
  rename(diff = estimate, p_raw = p.value)

# Save results (12h)
write.csv(result_table_12h,
          file.path(out_dir, paste0(
            "TD_Result_12h_BH_", Sys.Date(), ".csv"
          )),
          row.names = FALSE)

#########################
# Figure Plotting (12h) #
#########################

# Significance label positions
period_max_values <- summary_data_12h %>%
  group_by(Period_Label) %>%
  summarise(max_y = max(Avg_Distance), .groups = "drop")

# Generate colored significance labels and positions
sig_stars_12h <- result_table_12h %>%
  filter(p_BH < 0.05) %>%
  mutate(
    winner = ifelse(diff > 0, "Male", "Female"),
    label_color = color_values[winner]
  ) %>%
  left_join(period_max_values, by = "Period_Label") %>%
  left_join(bg_blocks_12h, by = "Period_Label") %>%
  mutate(y_pos = max_y + 12000)

# Function to convert X-axis labels
label_fix <- function(x) {
  x %>%
    gsub("^D(\\d+)", "Day\\1", .) %>%
    gsub("-", " ", .)
}

# 12-hour cumulative plot
group_levels <- c("Male", "Female")

summary_data_12h <- summary_data_12h %>%
  mutate(Group = factor(Group, levels = group_levels))

p_sum12h <- ggplot(summary_data_12h,
                   aes(
                     x = Index,
                     y = Avg_Distance,
                     color = Group,
                     group = Group
                   )) +
  geom_rect(
    data = grey_blocks_12h,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "grey80",
    alpha = 0.5,
    inherit.aes = FALSE
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  geom_ribbon(
    aes(
      ymin = Avg_Distance - SD,
      ymax = Avg_Distance + SD,
      fill = Group
    ),
    alpha = 0.2,
    color = NA
  ) +
  scale_color_manual(values = color_values,
                     breaks = group_levels,
                     limits = group_levels) +
  scale_fill_manual(values = color_values,
                    breaks = group_levels,
                    limits = group_levels) +
  scale_x_continuous(breaks = bg_blocks_12h$Index,
                     labels = label_fix(bg_blocks_12h$Period_Label)) +
  scale_y_continuous(limits = c(0, 65000)) +
  geom_text(angle = 90,
    data = sig_stars_12h,
    aes(x = Index, y = y_pos, label = sig_label),
    color = sig_stars_12h$label_color,
    show.legend = FALSE,
    inherit.aes = FALSE,
    family = "Courier",
    size = 5,
    fontface = "bold"
  ) +
  labs(
    title = "Travel distance - group comparison",
    x = "",
    y = "Travel distance (cm)",
    color = "Group"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ),
    legend.position = "right",
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

# Save outputs
# PNG (transparent)
ggsave(
  file.path(out_dir, paste0(
    "TD_GroupComparison_12h_", Sys.Date(), ".png"
  )),
  p_sum12h,
  width = 10,
  height = 4,
  dpi = 300,
  bg = "transparent"
)

# PDF (transparent)
ggsave(
  file.path(out_dir, paste0(
    "TD_GroupComparison_12h_", Sys.Date(), ".pdf"
  )),
  p_sum12h,
  width = 10,
  height = 4,
  bg = "transparent"
)

message("Saved to: ", out_dir)