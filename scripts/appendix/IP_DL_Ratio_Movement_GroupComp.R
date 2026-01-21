# --- Required packages ---
library(readxl)
library(dplyr)
library(ggplot2)
library(writexl)
library(emmeans)
library(tidyr)
library(stringr)

# --- Select input files ---
cat("▶ Please select the Male data file\n")
file_male <- file.choose()
cat("▶ Please select the Female data file\n")
file_female <- file.choose()

# --- Create output directory ---
out_dir_base <- file.path(dirname(file_male), "Group_Comparison_DarkLightRatio")
if (!dir.exists(out_dir_base)) {
  dir.create(out_dir_base, recursive = TRUE)
  cat("▶ Output directory created:", out_dir_base, "\n")
} else {
  cat("▶ Using existing output directory:", out_dir_base, "\n")
}

# --- Load data ---
read_group_data <- function(file, group_name) {
  read_excel(file) %>%
    mutate(
      Day = factor(Day, levels = c("Day1", "Day2", "Day3", "Day4")),
      ID = as.character(ID),
      Group = group_name
    )
}

df_male <- read_group_data(file_male, "Male")
df_female <- read_group_data(file_female, "Female")

df_all <- bind_rows(df_male, df_female)

# --- Fix the order of the Group factor levels ---
df_all <- df_all %>%
  mutate(Group = factor(Group, levels = c("Male", "Female")))

# --- Create an ID color palette using only ID levels excluding "All" ---
id_levels_no_all <- sort(unique(df_all$ID[df_all$ID != "All"]))

generate_color_by_index <- function(index) {
  h <- (index * 47) %% 360
  c <- 60 + (index * 23) %% 40
  l <- 55 + (index * 31) %% 30
  hcl(h = h, c = c, l = l)
}

palette_id <- setNames(
  sapply(seq_along(id_levels_no_all), generate_color_by_index),
  as.character(id_levels_no_all)
)

# --- Shared upper limit for the Y-axis ---
y_max <- max(df_all$`Dark/Light (%)`, na.rm = TRUE) * 1.1

# --- Generate and save plots for each individual ID ---
for (group_name in c("Male", "Female")) {
  
  df_group <- df_all %>% filter(Group == group_name)
  out_dir <- file.path(out_dir_base, group_name)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  df_group_no_all <- df_group %>% filter(ID != "All")
  
  for (id in id_levels_no_all) {
    id_chr <- as.character(id)
    df_plot <- filter(df_group, ID == id_chr)
    
    p <- ggplot(df_plot, aes(x = Day, y = `Dark/Light (%)`, group = 1)) +
      geom_line(color = palette_id[[id_chr]], size = 1.2) +
      geom_point(color = palette_id[[id_chr]], size = 3) +
      scale_y_continuous(limits = c(0, y_max)) +
      labs(title = paste0("Dark/Light ratio - ", group_name, " - ", id_chr),
           x = "", y = "Dark/Light (%)") +
      theme_minimal(base_size = 13) +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "grey80", size = 0.5),
        panel.grid.minor = element_line(color = "grey90", size = 0.3),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(face = "bold", size = 14),
        axis.title.y = element_text(face = "bold", size = 14),
        plot.title = element_text(face = "bold", size = 16),
        legend.position = "none"
      )
    
    ggsave(file.path(out_dir, paste0("DarkLightRatio_", id_chr, ".png")),
           plot = p, width = 10, height = 4, dpi = 300)
    ggsave(file.path(out_dir, paste0("DarkLightRatio_", id_chr, ".pdf")),
           plot = p, width = 10, height = 4)
    message("Saved: ", id_chr, " - ", group_name)
  }
  
  # --- Combined plot for all IDs ---
  p_all <- ggplot(df_group_no_all, aes(x = Day, y = `Dark/Light (%)`, color = ID, group = ID)) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    scale_y_continuous(limits = c(0, y_max)) +
    scale_color_manual(values = palette_id) +
    labs(title = paste0("Dark/Light ratio - all IDs - ", group_name),
         x = "", y = "Dark/Light (%)", color = "ID") +
    theme_minimal(base_size = 13) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "grey80", size = 0.5),
      panel.grid.minor = element_line(color = "grey90", size = 0.3),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      axis.title.x = element_text(face = "bold", size = 14),
      axis.title.y = element_text(face = "bold", size = 14),
      plot.title = element_text(face = "bold", size = 16),
      legend.position = "right"
    )
  
  ggsave(file.path(out_dir, "DarkLightRatio_All.png"),
         plot = p_all, width = 10, height = 4, dpi = 300)
  
  ggsave(file.path(out_dir, "DarkLightRatio_All.pdf"),
         plot = p_all, width = 10, height = 4)
  write_xlsx(df_group, file.path(out_dir, "DarkLightRatio_Each_ID.xlsx"))
}

# --- Group comparison (ANOVA and Tukey's test) ---
anova_list <- list()
tukey_list <- list()

for (day in levels(df_all$Day)) {
  dat_day <- filter(df_all, Day == day, ID != "All")
  if (n_distinct(dat_day$Group) == 2) {
    model <- tryCatch(aov(`Dark/Light (%)` ~ Group, data = dat_day), error = function(e) NULL)
    if (!is.null(model)) {
      pval <- summary(model)[[1]][["Pr(>F)"]][1]
      anova_list[[day]] <- data.frame(Day = day, p = pval)
      
      em <- emmeans(model, "Group")
      tuk <- pairs(em, adjust = "tukey") %>% as.data.frame()
      tuk$Day <- day
      tukey_list[[day]] <- tuk
    }
  }
}

anova_df <- bind_rows(anova_list) %>%
  mutate(sig_label = case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE ~ ""
  ))

tukey_df <- bind_rows(tukey_list) %>%
  mutate(sig_label = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    TRUE ~ ""
  ))

# --- Mean and standard deviation ---
summary_df <- df_all %>%
  filter(ID != "All") %>%
  group_by(Group, Day) %>%
  summarise(Avg = mean(`Dark/Light (%)`), SD = sd(`Dark/Light (%)`), .groups = "drop")

# --- Create annotation data for significance stars (sig_stars) ---
sig_stars <- anova_df %>%
  filter(sig_label != "") %>%
  left_join(tukey_df, by = "Day") %>%
  filter(p.value < 0.05) %>%
  mutate(
    winner = ifelse(estimate > 0,
                    str_extract(contrast, ".*(?= - )"),
                    str_extract(contrast, "(?<= - ).*")),
    label_color = c("Male" = "#377EB8", "Female" = "#E41A1C")[winner],
    sig_label = sig_label.x
  ) %>%
  left_join(summary_df %>% group_by(Day) %>% summarise(max_y = max(Avg) + 100), by = "Day") %>%
  mutate(y_pos = max_y + 50)

# --- Y-position for significance stars ---
y_stars <- max(summary_df$Avg) + 100

# --- Define colors for significance stars ---
star_colors <- summary_df %>%
  pivot_wider(names_from = Group, values_from = Avg) %>%
  mutate(
    color = ifelse(Female > Male, "#E41A1C", "#377EB8")
  ) %>%
  select(Day, color)

# --- Create the two-group comparison plot (p_group) ---
p_group <- ggplot(summary_df, aes(x = Day, y = Avg, color = Group, group = Group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = Avg - SD, ymax = Avg + SD, fill = Group), alpha = 0.2, color = NA) +
  scale_color_manual(values = c("Male" = "#377EB8", "Female" = "#E41A1C")) +
  scale_fill_manual(values = c("Male" = "#377EB8", "Female" = "#E41A1C")) +
  scale_y_continuous(limits = c(0, y_max * 1.2)) +
  labs(title = "Travel distance",
       x = "", y = "Dark/Light (%)", color = "Group") +
  theme_minimal(base_size = 13) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey80", size = 0.5),
    panel.grid.minor = element_line(color = "grey90", size = 0.3),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "right"
  )

# --- Add significance stars to p_group ---
p_group <- p_group +
  geom_text(data = sig_stars,
            aes(x = Day, y = y_pos, label = sig_label),
            color = sig_stars$label_color,
            show.legend = FALSE, inherit.aes = FALSE, size = 8,
            fontface = "bold")

# --- Save (PNG) ---
ggsave(file.path(out_dir_base, "DarkLightRatio_GroupComparison.png"),
       plot = p_group, width = 10, height = 4, dpi = 300)

# --- Save (PDF; standard) ---
ggsave(file.path(out_dir_base, "DarkLightRatio_GroupComparison.pdf"),
       plot = p_group, width = 10, height = 4)   # dpi is not necessary for PDF (typically ignored)

# --- Save statistical results ---
write.csv(anova_df, file.path(out_dir_base, "ANOVA_DarkLightRatio.csv"), row.names = FALSE)
write.csv(tukey_df, file.path(out_dir_base, "Tukey_DarkLightRatio.csv"), row.names = FALSE)

message("✅ All done: ", out_dir_base)
