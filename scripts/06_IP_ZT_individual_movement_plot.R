# --- Required packages ---
library(readxl)
library(stringr)
library(dplyr)
library(ggplot2)
library(emmeans)
library(tidyr)
library(purrr)   # <- added

# --- Select files interactively ---
cat("▶ Please select Male file\n")
file1 <- file.choose()
cat("▶ Please select Female file\n")
file2 <- file.choose()
group1_name <- "Male"
group2_name <- "Female"

# --- Output directory (same as Male file location) ---
out_dir <- file.path(dirname(file1), "Group_Comparison_Results")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# --- Load data and assign group labels ---
read_group_data <- function(file, group_name) {
  read_excel(file) %>%
    mutate(Group = group_name)
}

data1 <- read_group_data(file1, group1_name)
data2 <- read_group_data(file2, group2_name)
data_all <- bind_rows(data1, data2)

# --- Fix group order: Male → Female ---
data_all <- data_all %>%
  mutate(Group = factor(Group, levels = c("Male", "Female")))

# --- Extract ZT, Day, and create display label ---
data_all <- data_all %>%
  mutate(
    ZT = as.numeric(sub(".*ZT", "", ZT_Label)),
    Day = as.numeric(sub("Day(\\d+).*", "\\1", ZT_Label)),
    ZT_Display = paste0("Day", Day, " ZT", ZT),
    ZT_Display = factor(ZT_Display, levels = unique(ZT_Display))
  )

# --- Calculate group-wise summary statistics ---
summary_data <- data_all %>%
  group_by(Group, ZT_Display, ZT, Day) %>%
  summarise(Avg_Distance = mean(Total_Distance), SD = sd(Total_Distance), .groups = "drop")

# === Insert from here ===
# --- Add Light/Dark information and create Period_Label ---
data_all_sum12h <- data_all %>%
  mutate(
    LightDark = ifelse(ZT < 12, "Light", "Dark"),
    Period_Label = paste0("D", Day, "-", LightDark)
  )

# --- Convert Period_Label to factor in Light → Dark order ---
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

# === Insert up to here ===

# --- ANOVA & TukeyHSD for each ZT ---
tukey_hourly_list <- list()
anova_hourly_list <- list()
zt_levels <- levels(data_all$ZT_Display)

for (zt in zt_levels) {
  dat <- filter(data_all, ZT_Display == zt)
  if (n_distinct(dat$Group) == 2) {
    model <- tryCatch(aov(Total_Distance ~ Group, data = dat), error = function(e) NULL)
    if (!is.null(model)) {
      pval <- summary(model)[[1]][["Pr(>F)"]][1]
      anova_hourly_list[[zt]] <- data.frame(ZT_Display = zt, p = pval)
      em <- emmeans(model, "Group")
      tuk <- pairs(em, adjust = "tukey") %>% as.data.frame()
      tuk$ZT_Display <- zt
      tukey_hourly_list[[zt]] <- tuk
    }
  }
}

anova_hourly <- bind_rows(anova_hourly_list) %>%
  mutate(
    p_adj_BH = p.adjust(p, method = "BH"),
    sig_label = case_when(
      p_adj_BH < 0.001 ~ "***",
      p_adj_BH < 0.01  ~ "**",
      p_adj_BH < 0.05  ~ "*",
      TRUE ~ ""
    )
  )

tukey_hourly_df <- bind_rows(tukey_hourly_list) %>%
  mutate(
    p_adj_BH = p.adjust(p.value, method = "BH"),
    sig_label = case_when(
      p_adj_BH < 0.001 ~ "***",
      p_adj_BH < 0.01  ~ "**",
      p_adj_BH < 0.05  ~ "*",
      TRUE ~ ""
    )
  )

# --- Define dark phase blocks (ZT12–ZT23) ---
grey_blocks <- summary_data %>%
  filter(ZT >= 12 & ZT <= 23) %>%
  group_by(Day) %>%
  summarise(
    xmin = min(as.numeric(factor(ZT_Display, levels = levels(data_all$ZT_Display)))),
    xmax = max(as.numeric(factor(ZT_Display, levels = levels(data_all$ZT_Display)))) + 1,
    .groups = "drop"
  )

# --- Define labels every 3 hours ---
display_labels <- summary_data %>%
  filter(ZT %% 3 == 0) %>%
  pull(ZT_Display) %>%
  unique()

# --- Y-axis upper values (for aligning significance labels) ---
global_max <- max(data_all$Total_Distance, na.rm = TRUE)
y_max <- global_max * 1.1
y_limit <- global_max * 1.2

# --- Color settings ---
color_values <- setNames(c("#377EB8", "#E41A1C"), c(group1_name, group2_name))

# --- Maximum Avg_Distance for each ZT ---
zt_max_values <- summary_data %>%
  group_by(ZT_Display) %>%
  summarise(max_y = max(Avg_Distance), .groups = "drop")

# --- Generate colored significance labels ---
sig_stars <- tukey_hourly_df %>%
  filter(p_adj_BH < 0.05) %>%
  mutate(
    winner = ifelse(estimate > 0,
                    str_extract(contrast, ".*(?= - )"),
                    str_extract(contrast, "(?<= - ).*")),
    label_color = color_values[winner]
  ) %>%
  left_join(zt_max_values, by = "ZT_Display") %>%
  mutate(y_pos = max_y + 2000)

# --- Create plot ---
# ---- Fully fix legend order ----
group_levels <- c("Male", "Female")

summary_data <- summary_data %>%
  mutate(Group = factor(Group, levels = group_levels))

p_group <- ggplot(
  summary_data,
  aes(x = ZT_Display, y = Avg_Distance, color = Group, group = Group)
) +
  geom_rect(
    data = grey_blocks,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    fill = "grey80", alpha = 0.5, inherit.aes = FALSE
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  geom_ribbon(
    aes(ymin = Avg_Distance - SD, ymax = Avg_Distance + SD, fill = Group),
    alpha = 0.2, color = NA
  ) +
  scale_x_discrete(breaks = display_labels) +
  scale_color_manual(values = color_values, breaks = group_levels, limits = group_levels) +
  scale_fill_manual(values  = color_values, breaks = group_levels, limits = group_levels) +
  scale_y_continuous(limits = c(0, 15000)) +
  geom_text(
    data = sig_stars,
    aes(x = ZT_Display, y = y_pos, label = sig_label),
    color = sig_stars$label_color,
    show.legend = FALSE, inherit.aes = FALSE,
    size = 8, fontface = "bold"
  ) +
  labs(
    title = "Travel distance - group comparison",
    x = "Day and ZT", y = "Travel distance (cm)", color = "Group"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "right",
    plot.background  = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

# PNG (transparent)
ggsave(file.path(out_dir, paste0("HourlyMovement_GroupComparison_", Sys.Date(), ".png")),
       p_group, width = 10, height = 4, dpi = 300, bg = "transparent")

# PDF (transparent)
ggsave(file.path(out_dir, paste0("HourlyMovement_GroupComparison_", Sys.Date(), ".pdf")),
       p_group, width = 10, height = 4, bg = "transparent")

write.csv(anova_hourly, file.path(out_dir, paste0("ANOVA_Hourly_byZT_", Sys.Date(), ".csv")), row.names = FALSE)
write.csv(tukey_hourly_df, file.path(out_dir, paste0("Tukey_Hourly_byZT_", Sys.Date(), ".csv")), row.names = FALSE)

message("Saved to: ", out_dir)

# --- Global model (1 hr) ---
model_global_1hr <- aov(Total_Distance ~ Group * ZT_Display, data = data_all)

# Convert ANOVA table to data frame
anova_global_1hr_df <- as.data.frame(summary(model_global_1hr)[[1]])
anova_global_1hr_df$Effect <- rownames(anova_global_1hr_df)
colnames(anova_global_1hr_df)[which(colnames(anova_global_1hr_df) == "Pr(>F)")] <- "p_value"

# --- Add adjusted p-values and significance labels ---
anova_global_1hr_df <- anova_global_1hr_df %>%
  select(Effect, everything()) %>%
  mutate(
    p_adj_BH = p.adjust(p_value, method = "BH"),
    sig_label = case_when(
      p_adj_BH < 0.001 ~ "***",
      p_adj_BH < 0.01  ~ "**",
      p_adj_BH < 0.05  ~ "*",
      TRUE ~ ""
    )
  )

# Save results
write.csv(anova_global_1hr_df, file.path(out_dir, paste0("ANOVA_Global_1hr_", Sys.Date(), ".csv")), row.names = FALSE)

# --- Convert Period_Label to factor in Light → Dark order ---
lightdark_order <- c("Light", "Dark")
period_levels_ordered <- data_all_sum12h %>%
  distinct(Period_Label, LightDark, Day) %>%
  mutate(LightDark_order = match(LightDark, lightdark_order)) %>%
  arrange(Day, LightDark_order) %>%
  pull(Period_Label)

data_all_sum12h <- data_all_sum12h %>%
  mutate(Period_Label = factor(Period_Label, levels = period_levels_ordered),
         Index = as.numeric(Period_Label))

# === Replace from here ===

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

# === Replace up to here ===

# --- Background data for shading ---
bg_blocks_12h <- data_all_sum12h %>%
  distinct(Index, Period_Label, LightDark) %>%
  mutate(LightDarkColor = ifelse(LightDark == "Light", "white", "grey80"))

# --- Define dark phase blocks only (LightDark == "Dark") ---
grey_blocks_12h <- data_all_sum12h %>%
  filter(LightDark == "Dark") %>%
  distinct(Index, Period_Label) %>%
  mutate(xmin = Index - 0.5,
         xmax = Index + 0.5)

# --- ANOVA + Tukey test for each Period_Label ---
tukey_12h_list <- list()
anova_12h_list <- list()

period_levels <- levels(data_all_sum12h$Period_Label)

for (period in period_levels) {
  dat <- filter(individual_sum12h, Period_Label == period)
  if (n_distinct(dat$Group) == 2) {
    model <- tryCatch(aov(Sum_Distance ~ Group, data = dat), error = function(e) NULL)
    if (!is.null(model)) {
      pval <- summary(model)[[1]][["Pr(>F)"]][1]
      anova_12h_list[[period]] <- data.frame(Period_Label = period, p = pval)
      em <- emmeans(model, "Group")
      tuk <- pairs(em, adjust = "tukey") %>% as.data.frame()
      tuk$Period_Label <- period
      tukey_12h_list[[period]] <- tuk
    }
  }
}

anova_12h <- anova_12h_list %>%
  purrr::compact() %>%
  bind_rows() %>%
  mutate(
    p_adj_BH = p.adjust(p, method = "BH"),
    sig_label = case_when(
      p_adj_BH < 0.001 ~ "***",
      p_adj_BH < 0.01  ~ "**",
      p_adj_BH < 0.05  ~ "*",
      TRUE ~ ""
    )
  )

tukey_12h_df <- bind_rows(tukey_12h_list) %>%
  mutate(
    p_adj_BH = p.adjust(p.value, method = "BH"),
    sig_label = case_when(
      p_adj_BH < 0.001 ~ "***",
      p_adj_BH < 0.01  ~ "**",
      p_adj_BH < 0.05  ~ "*",
      TRUE ~ ""
    )
  )

# --- Significance label positions ---
period_max_values <- summary_data_12h %>%
  group_by(Period_Label) %>%
  summarise(max_y = max(Avg_Distance), .groups = "drop")

sig_stars_12h <- tukey_12h_df %>%
  filter(p_adj_BH < 0.05) %>%
  mutate(
    winner = ifelse(estimate > 0,
                    str_extract(contrast, ".*(?= - )"),
                    str_extract(contrast, "(?<= - ).*")),
    label_color = color_values[winner]
  ) %>%
  left_join(period_max_values, by = "Period_Label") %>%
  left_join(bg_blocks_12h, by = "Period_Label") %>%
  mutate(y_pos = max_y + 12000)

# --- Function to convert X-axis labels ---
label_fix <- function(x) {
  x %>%
    gsub("^D(\\d+)", "Day\\1", .) %>%
    gsub("-", " ", .)
}

# --- 12-hour cumulative plot ---
group_levels <- c("Male", "Female")

summary_data_12h <- summary_data_12h %>%
  mutate(Group = factor(Group, levels = group_levels))

p_sum12h <- ggplot(
  summary_data_12h,
  aes(x = Index, y = Avg_Distance, color = Group, group = Group)
) +
  geom_rect(
    data = grey_blocks_12h,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    fill = "grey80", alpha = 0.5, inherit.aes = FALSE
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  geom_ribbon(
    aes(ymin = Avg_Distance - SD, ymax = Avg_Distance + SD, fill = Group),
    alpha = 0.2, color = NA
  ) +
  scale_color_manual(values = color_values, breaks = group_levels, limits = group_levels) +
  scale_fill_manual(values  = color_values, breaks = group_levels, limits = group_levels) +
  scale_x_continuous(
    breaks = bg_blocks_12h$Index,
    labels = label_fix(bg_blocks_12h$Period_Label)
  ) +
  scale_y_continuous(limits = c(0, 65000)) +
  geom_text(
    data = sig_stars_12h,
    aes(x = Index, y = y_pos, label = sig_label),
    color = sig_stars_12h$label_color,
    show.legend = FALSE, inherit.aes = FALSE,
    size = 8, fontface = "bold"
  ) +
  labs(
    title = "Travel distance - group comparison",
    x = "", y = "Travel distance (cm)", color = "Group"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "right",
    plot.background  = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

# --- Save outputs ---
# PNG (transparent)
ggsave(file.path(out_dir, paste0("Sum12h_GroupComparison_", Sys.Date(), ".png")),
       p_sum12h, width = 10, height = 4, dpi = 300, bg = "transparent")

# PDF (transparent)
ggsave(file.path(out_dir, paste0("Sum12h_GroupComparison_", Sys.Date(), ".pdf")),
       p_sum12h, width = 10, height = 4, bg = "transparent")

# --- Save 12-hour ANOVA / Tukey results ---
write.csv(anova_12h, file.path(out_dir, paste0("ANOVA_Sum12h_byPeriod_", Sys.Date(), ".csv")), row.names = FALSE)
write.csv(tukey_12h_df, file.path(out_dir, paste0("Tukey_Sum12h_byPeriod_", Sys.Date(), ".csv")), row.names = FALSE)

# --- Global model (12 hr) ---
model_global_12h <- aov(Sum_Distance ~ Group * Period_Label, data = individual_sum12h)

# Convert ANOVA table to data frame
anova_global_12h_df <- as.data.frame(summary(model_global_12h)[[1]])
anova_global_12h_df$Effect <- rownames(anova_global_12h_df)
colnames(anova_global_12h_df)[which(colnames(anova_global_12h_df) == "Pr(>F)")] <- "p_value"

# --- Add adjusted p-values and significance labels ---
anova_global_12h_df <- anova_global_12h_df %>%
  select(Effect, everything()) %>%
  mutate(
    p_adj_BH = p.adjust(p_value, method = "BH"),
    sig_label = case_when(
      p_adj_BH < 0.001 ~ "***",
      p_adj_BH < 0.01  ~ "**",
      p_adj_BH < 0.05  ~ "*",
      TRUE ~ ""
    )
  )

# Save results
write.csv(anova_global_12h_df, file.path(out_dir, paste0("ANOVA_Global_12hr_", Sys.Date(), ".csv")), row.names = FALSE)