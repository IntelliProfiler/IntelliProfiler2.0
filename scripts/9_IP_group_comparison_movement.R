# --- Required packages ---
library(readxl)
library(stringr)
library(dplyr)
library(ggplot2)
library(emmeans)
library(tidyr)

# --- Interactive file selection ---
cat("▶ Please select the Male file\n")
file1 <- file.choose()
cat("▶ Please select the Female file\n")
file2 <- file.choose()
group1_name <- "Male"
group2_name <- "Female"

# --- Base output folder (same location as the Male file) ---
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

# ★★★ Fix Group order: Male → Female ★★★
data_all <- data_all %>%
  mutate(Group = factor(Group, levels = c("Male", "Female")))

# --- Extract numeric ZT and Day, and create a display label for ZT ---
data_all <- data_all %>%
  mutate(
    ZT = as.numeric(sub(".*ZT", "", ZT_Label)),
    Day = as.numeric(sub("Day(\\d+).*", "\\1", ZT_Label)),
    ZT_Display = paste0("Day", Day, " ZT", ZT),
    ZT_Display = factor(ZT_Display, levels = unique(ZT_Display))
  )

# --- Compute group-wise summary statistics ---
summary_data <- data_all %>%
  group_by(Group, ZT_Display, ZT, Day) %>%
  summarise(Avg_Distance = mean(Total_Distance), SD = sd(Total_Distance), .groups = "drop")

# --- ANOVA & Tukey HSD (for each ZT) ---
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
  mutate(sig_label = case_when(
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    TRUE ~ ""
  ))

tukey_hourly_df <- bind_rows(tukey_hourly_list) %>%
  mutate(sig_label = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    TRUE ~ ""
  ))

# --- Define night-phase blocks (ZT12–ZT23) ---
grey_blocks <- summary_data %>%
  filter(ZT >= 12 & ZT <= 23) %>%
  group_by(Day) %>%
  summarise(
    xmin = min(as.numeric(factor(ZT_Display, levels = levels(data_all$ZT_Display)))),
    xmax = max(as.numeric(factor(ZT_Display, levels = levels(data_all$ZT_Display)))) + 1,
    .groups = "drop"
  )

# --- Set x-axis labels shown every 3 hours ---
display_labels <- summary_data %>%
  filter(ZT %% 3 == 0) %>%
  pull(ZT_Display) %>%
  unique()

# --- Global y-axis max (to align asterisk positions) ---
global_max <- max(data_all$Total_Distance, na.rm = TRUE)
y_max <- global_max * 1.1
y_limit <- global_max * 1.2

# --- Color settings ---
color_values <- setNames(c("#377EB8", "#E41A1C"), c(group1_name, group2_name))

# --- Max Avg_Distance for each ZT_Display --- ← ★ Added here!
zt_max_values <- summary_data %>%
  group_by(ZT_Display) %>%
  summarise(max_y = max(Avg_Distance), .groups = "drop")

# --- Build colored asterisk annotation data ---
sig_stars <- anova_hourly %>%
  filter(sig_label != "") %>%
  left_join(tukey_hourly_df, by = "ZT_Display") %>%
  filter(p.value < 0.05) %>%
  mutate(
    winner = ifelse(estimate > 0,
                    str_extract(contrast, ".*(?= - )"),
                    str_extract(contrast, "(?<= - ).*")),
    label_color = color_values[winner],
    sig_label = sig_label.x    # ★★★ Use sig_label from anova_hourly ★★★
  ) %>%
  left_join(zt_max_values, by = "ZT_Display") %>%
  mutate(y_pos = max_y + 1200)


# --- Create plot ---
p_group <- ggplot(summary_data, aes(x = ZT_Display, y = Avg_Distance, color = Group, group = Group)) +
  geom_rect(data = grey_blocks,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "grey80", alpha = 0.5, inherit.aes = FALSE) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  geom_ribbon(aes(ymin = Avg_Distance - SD, ymax = Avg_Distance + SD, fill = Group), alpha = 0.2, color = NA) +
  scale_x_discrete(breaks = display_labels) +
  scale_color_manual(values = color_values) +
  scale_fill_manual(values = color_values) +
  scale_y_continuous(limits = c(0, 10000)) +    # ← ★ Add here ★
  geom_text(data = sig_stars,
            aes(x = ZT_Display, y = y_pos, label = sig_label),   # ← use y_pos
            color = sig_stars$label_color,
            show.legend = FALSE, inherit.aes = FALSE, size = 5,
            fontface = "bold") +   # ← ★★★ this + is required ★★★
  
  labs(title = "Travel distance - group comparison",
       x = "Day and ZT", y = "Travel distance (cm)", color = "Group") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "right",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    axis.title.y = element_text(face = "bold")   # ★ just add this!
  )

# PNG (transparent background)
ggsave(file.path(out_dir, paste0("HourlyMovement_GroupComparison_", Sys.Date(), ".png")),
       p_group, width = 10, height = 4, dpi = 300, bg = "transparent")

# PDF (transparent background)
ggsave(file.path(out_dir, paste0("HourlyMovement_GroupComparison_", Sys.Date(), ".pdf")),
       p_group, width = 10, height = 4, bg = "transparent")

write.csv(anova_hourly, file.path(out_dir, paste0("ANOVA_Hourly_byZT_", Sys.Date(), ".csv")), row.names = FALSE)
write.csv(tukey_hourly_df, file.path(out_dir, paste0("Tukey_Hourly_byZT_", Sys.Date(), ".csv")), row.names = FALSE)

message("Save completed: ", out_dir)

# --- Global model (1hr) ---
model_global_1hr <- aov(Total_Distance ~ Group * ZT_Display, data = data_all)

# Convert ANOVA table to a data frame
anova_global_1hr_df <- as.data.frame(summary(model_global_1hr)[[1]])
anova_global_1hr_df$Effect <- rownames(anova_global_1hr_df)
colnames(anova_global_1hr_df)[which(colnames(anova_global_1hr_df) == "Pr(>F)")] <- "p_value"

# ★★★ Added here ★★★
anova_global_1hr_df <- anova_global_1hr_df %>%
  select(Effect, everything()) %>%
  mutate(sig_label = case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01  ~ "**",
    p_value < 0.05  ~ "*",
    TRUE ~ ""
  ))

# Save
write.csv(anova_global_1hr_df, file.path(out_dir, paste0("ANOVA_Global_1hr_", Sys.Date(), ".csv")), row.names = FALSE)

# --- Convert Period_Label to a factor ordered Light → Dark ---
lightdark_order <- c("Light", "Dark")
period_levels_ordered <- data_all_sum12h %>%
  distinct(Period_Label, LightDark, Day) %>%
  mutate(LightDark_order = match(LightDark, lightdark_order)) %>%
  arrange(Day, LightDark_order) %>%
  pull(Period_Label)

data_all_sum12h <- data_all_sum12h %>%
  mutate(Period_Label = factor(Period_Label, levels = period_levels_ordered),
         Index = as.numeric(Period_Label))

summary_data_12h <- data_all_sum12h %>%
  group_by(Group, Period_Label, Index) %>%
  summarise(Avg_Sum = mean(Sum_Distance), SD = sd(Sum_Distance), .groups = "drop")

# --- Background data for grey shading ---
bg_blocks_12h <- data_all_sum12h %>%
  distinct(Index, Period_Label, LightDark) %>%
  mutate(LightDarkColor = ifelse(LightDark == "Light", "white", "grey80"))

# --- Define night-phase blocks (only LightDark == "Dark") --- ← ★ Added here
grey_blocks_12h <- data_all_sum12h %>%
  filter(LightDark == "Dark") %>%
  distinct(Index, Period_Label) %>%
  mutate(xmin = Index - 0.5,
         xmax = Index + 0.5)

# --- ANOVA + Tukey (for each Period_Label) ---
tukey_12h_list <- list()
anova_12h_list <- list()

period_levels <- levels(data_all_sum12h$Period_Label)

for (period in period_levels) {
  dat <- filter(data_all_sum12h, Period_Label == period)
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

anova_12h <- bind_rows(anova_12h_list) %>%
  mutate(sig_label = case_when(
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    TRUE ~ ""
  ))

tukey_12h_df <- bind_rows(tukey_12h_list) %>%
  mutate(sig_label = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    TRUE ~ ""
  ))

# --- Asterisk positions ---
period_max_values <- summary_data_12h %>%
  group_by(Period_Label) %>%
  summarise(max_y = max(Avg_Sum), .groups = "drop")

sig_stars_12h <- anova_12h %>%
  filter(sig_label != "") %>%
  left_join(tukey_12h_df, by = "Period_Label") %>%
  filter(p.value < 0.05) %>%
  mutate(
    winner = ifelse(estimate > 0,
                    str_extract(contrast, ".*(?= - )"),
                    str_extract(contrast, "(?<= - ).*")),
    label_color = color_values[winner],
    sig_label = sig_label.x   
  ) %>%
  left_join(period_max_values, by = "Period_Label") %>%
  left_join(bg_blocks_12h, by = "Period_Label") %>%
  mutate(y_pos = max_y + 8000)

# --- Helper function to convert x-axis labels --- ← ★★★ Add this block here ★★★
label_fix <- function(x) {
  x %>%
    gsub("^D(\\d+)", "Day\\1", .) %>%  
    gsub("-", " ", .)                
}

# --- 12h cumulative plot ---
p_sum12h <- ggplot(summary_data_12h, aes(x = Index, y = Avg_Sum, color = Group, group = Group)) +
  
  # Background shading (Light/Dark)
  geom_rect(data = grey_blocks_12h,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "grey80", alpha = 0.5, inherit.aes = FALSE) +
  
  # Main lines and points
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  
  # Ribbon (match 1hr styling: group-colored ribbon)
  geom_ribbon(aes(ymin = Avg_Sum - SD, ymax = Avg_Sum + SD, fill = Group), alpha = 0.2, color = NA) +
  
  # Color settings
  scale_color_manual(values = color_values) +
  scale_fill_manual(values = color_values) +   # ← required
  
  # Axes
  scale_y_continuous(limits = c(0, 60000)) +
  scale_x_continuous(
    breaks = bg_blocks_12h$Index,
    labels = label_fix(bg_blocks_12h$Period_Label)
  ) +
  
  # Asterisks
  geom_text(data = sig_stars_12h,
            aes(x = Index, y = y_pos, label = sig_label),
            color = sig_stars_12h$label_color,
            show.legend = FALSE, inherit.aes = FALSE, size = 5,
            fontface = "bold") +
  
  # Labels and theme
  labs(title = "Travel distance - group comparison",
       x = "Day and ZT", y = "Travel distance (cm)", color = "Group") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "right",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    axis.title.y = element_text(face = "bold")   # ★ just add this!
  )

# --- X-axis label conversion ---
label_fix <- function(x) {
  x %>% 
    gsub("^D", "Day ", .) %>%  
    gsub("-", " ", .)
}

# --- Save outputs ---
# PNG (transparent background)
ggsave(file.path(out_dir, paste0("Sum12h_GroupComparison_", Sys.Date(), ".png")),
       p_sum12h, width = 10, height = 4, dpi = 300, bg = "transparent")

# PDF (transparent background)
ggsave(file.path(out_dir, paste0("Sum12h_GroupComparison_", Sys.Date(), ".pdf")),
       p_sum12h, width = 10, height = 4, bg = "transparent")

# --- Save 12h ANOVA / Tukey results
write.csv(anova_12h, file.path(out_dir, paste0("ANOVA_Sum12h_byPeriod_", Sys.Date(), ".csv")), row.names = FALSE)
write.csv(tukey_12h_df, file.path(out_dir, paste0("Tukey_Sum12h_byPeriod_", Sys.Date(), ".csv")), row.names = FALSE)

# --- Global model (12hr) ---
model_global_12h <- aov(Sum_Distance ~ Group * Period_Label, data = data_all_sum12h)

# Convert ANOVA table to a data frame
anova_global_12h_df <- as.data.frame(summary(model_global_12h)[[1]])
anova_global_12h_df$Effect <- rownames(anova_global_12h_df)
colnames(anova_global_12h_df)[which(colnames(anova_global_12h_df) == "Pr(>F)")] <- "p_value"


# ★★★ Added here ★★★
anova_global_12h_df <- anova_global_12h_df %>%
  select(Effect, everything()) %>%
  mutate(sig_label = case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01  ~ "**",
    p_value < 0.05  ~ "*",
    TRUE ~ ""
  ))

# Save
write.csv(anova_global_12h_df, file.path(out_dir, paste0("ANOVA_Global_12hr_", Sys.Date(), ".csv")), row.names = FALSE)
