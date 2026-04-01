###########################################
# Script name : 20_IP_CCR_IID_correlation_plot.R
# Description : Analyze and visualize the correlation between close contact ratio (CCR) and inter-individual distance (IID) across groups and light/dark phases with linear regression and confidence intervals
# Authors     : Shohei Ochi, Masashi Azuma
# Version history:
#   v1.1 - 2026-03-15 - Masashi Azuma
#   v1.1 - 2026-03-31 - Shohei Ochi, Masashi Azuma
###########################################
# ============================================================
# CCR-IID correlation plots for Male / Female and Light / Dark / All
#
# Input (interactive):
#   - Male CCR file   : ZT_converted_Hourly_CCR.xlsx
#   - Female CCR file : ZT_converted_Hourly_CCR.xlsx
#   - Male IID file   : ZT_converted_Hourly_IID.xlsx
#   - Female IID file : ZT_converted_Hourly_IID.xlsx
#
# Output:
#   1) Combined multi-page PDFs
#      - combined_pdf/CCR_IID_corr_1hr_combined_legend.pdf
#      - combined_pdf/CCR_IID_corr_1hr_combined_nolegend.pdf
#      - combined_pdf/CCR_IID_corr_12hr_combined_legend.pdf
#      - combined_pdf/CCR_IID_corr_12hr_combined_nolegend.pdf
#
#   2) Single-plot PDFs (one plot per PDF)
#      - single_pdf/1hr/CCR_IID_corr_Male_L_1hr.pdf
#      - single_pdf/1hr/CCR_IID_corr_Male_D_1hr.pdf
#      - single_pdf/1hr/CCR_IID_corr_Male_A_1hr_legend.pdf
#      - single_pdf/1hr/CCR_IID_corr_Male_A_1hr_nolegend.pdf
#      - single_pdf/1hr/CCR_IID_corr_Female_L_1hr.pdf
#      - single_pdf/1hr/CCR_IID_corr_Female_D_1hr.pdf
#      - single_pdf/1hr/CCR_IID_corr_Female_A_1hr_legend.pdf
#      - single_pdf/1hr/CCR_IID_corr_Female_A_1hr_nolegend.pdf
#      - single_pdf/12hr/CCR_IID_corr_Male_L_12hr.pdf
#      - single_pdf/12hr/CCR_IID_corr_Male_D_12hr.pdf
#      - single_pdf/12hr/CCR_IID_corr_Male_A_12hr_legend.pdf
#      - single_pdf/12hr/CCR_IID_corr_Male_A_12hr_nolegend.pdf
#      - single_pdf/12hr/CCR_IID_corr_Female_L_12hr.pdf
#      - single_pdf/12hr/CCR_IID_corr_Female_D_12hr.pdf
#      - single_pdf/12hr/CCR_IID_corr_Female_A_12hr_legend.pdf
#      - single_pdf/12hr/CCR_IID_corr_Female_A_12hr_nolegend.pdf
#
# Notes:
#   - IID and CCR files are merged by ID_1, ID_2, and ZT_Label.
#   - Supported IID column names:
#       * Avg_Inter_Individual_Distance
#   - Supported CCR column names:
#       * CCR
#       * Close_Contact_Ratio
#   - The ribbon from geom_smooth(method = "lm", se = TRUE)
#     represents the 95% confidence interval of the fitted regression line.
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(ggplot2)
})

# =========================
# SETTINGS
# =========================

cat("▶ Please select the Male file (ZT_converted_Hourly_CCR.xlsx)\n")
file_male <- file.choose()

cat("▶ Please select the Female file (ZT_converted_Hourly_CCR.xlsx)\n")
file_female <- file.choose()

cat("▶ Please select the Male file (ZT_converted_Hourly_IID.xlsx)\n")
file_male_iid <- file.choose()

cat("▶ Please select the Female file (ZT_converted_Hourly_IID.xlsx)\n")
file_female_iid <- file.choose()

output_dir <- file.path(dirname(file_male), "CCR_IID_correlation_results")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

combined_pdf_dir <- file.path(output_dir, "combined_pdf")
single_pdf_dir   <- file.path(output_dir, "single_pdf")

dir.create(combined_pdf_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(single_pdf_dir, recursive = TRUE, showWarnings = FALSE)

output_pdf_1h  <- file.path(combined_pdf_dir, "CCR_IID_corr_1hr_combined_legend.pdf")
output_pdf_1h_nolegend <- file.path(combined_pdf_dir, "CCR_IID_corr_1hr_combined_nolegend.pdf")

output_pdf_12h <- file.path(combined_pdf_dir, "CCR_IID_corr_12hr_combined_legend.pdf")
output_pdf_12h_nolegend <- file.path(combined_pdf_dir, "CCR_IID_corr_12hr_combined_nolegend.pdf")

make_12h_average_pdf <- TRUE
point_size <- 3.4
base_size <- 16
axis_margin <- 0.05

title_size <- 18
axis_title_size <- 17
axis_text_size <- 15
legend_title_size <- 15
legend_text_size <- 14
annotation_size <- 5

# =========================
# HELPER FUNCTIONS
# =========================

resolve_iid_col <- function(df) {
  if (!"Avg_Inter_Individual_Distance" %in% names(df)) {
    stop("IID column was not found: Avg_Inter_Individual_Distance")
  }
  "Avg_Inter_Individual_Distance"
}

resolve_ccr_col <- function(df) {
  candidates <- c("CCR", "Close_Contact_Ratio")
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) {
    stop(
      "CCR column was not found. Expected one of: ",
      paste(candidates, collapse = ", ")
    )
  }
  hit[1]
}

load_and_prepare <- function(iid_file_path, ccr_file_path, sex_label) {
  iid_df <- read_excel(iid_file_path)
  ccr_df <- read_excel(ccr_file_path)
  
  iid_col <- resolve_iid_col(iid_df)
  ccr_col <- resolve_ccr_col(ccr_df)
  
  required_cols <- c("ID_1", "ID_2", "ZT_Label")
  
  if (!all(required_cols %in% names(iid_df))) {
    stop("IID file is missing one or more required columns: ID_1, ID_2, ZT_Label")
  }
  if (!all(required_cols %in% names(ccr_df))) {
    stop("CCR file is missing one or more required columns: ID_1, ID_2, ZT_Label")
  }
  
  iid_df2 <- iid_df %>%
    transmute(
      ID_1 = as.character(ID_1),
      ID_2 = as.character(ID_2),
      ZT_Label = as.character(ZT_Label),
      IID = as.numeric(.data[[iid_col]])
    )
  
  ccr_df2 <- ccr_df %>%
    transmute(
      ID_1 = as.character(ID_1),
      ID_2 = as.character(ID_2),
      ZT_Label = as.character(ZT_Label),
      CCR = as.numeric(.data[[ccr_col]])
    )
  
  joined_df <- iid_df2 %>%
    inner_join(ccr_df2, by = c("ID_1", "ID_2", "ZT_Label"))
  
  message("Loaded: ", sex_label)
  message("  IID rows   : ", nrow(iid_df2))
  message("  CCR rows   : ", nrow(ccr_df2))
  message("  Joined rows: ", nrow(joined_df))
  
  joined_df %>%
    mutate(
      Sex = sex_label,
      Day = str_extract(ZT_Label, "Day\\d+"),
      ZT  = as.integer(str_extract(ZT_Label, "(?<=ZT)\\d+")),
      Phase = case_when(
        !is.na(ZT) & ZT >= 0  & ZT < 12 ~ "Light",
        !is.na(ZT) & ZT >= 12 & ZT < 24 ~ "Dark",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(IID), !is.na(CCR), !is.na(Phase)) %>%
    select(Sex, ID_1, ID_2, ZT_Label, Day, ZT, Phase, IID, CCR)
}

format_p_value <- function(p) {
  if (is.na(p)) {
    return("p = NA")
  }
  if (p < 0.001) {
    return("p < 0.001")
  }
  paste0("p = ", sprintf("%.3f", p))
}

make_annotation_text <- function(df) {
  if (nrow(df) < 3) {
    return(paste0("n = ", nrow(df), "\nPearson r = NA\np = NA"))
  }
  
  test <- suppressWarnings(cor.test(df$CCR, df$IID, method = "pearson"))
  r_val <- unname(test$estimate)
  p_val <- test$p.value
  
  paste0(
    "n = ", nrow(df),
    "\nPearson r = ", sprintf("%.3f", r_val),
    "\n", format_p_value(p_val)
  )
}

get_axis_limits <- function(df, margin = 0.05) {
  x_max <- max(df$CCR, na.rm = TRUE)
  y_max <- max(df$IID, na.rm = TRUE)
  
  if (!is.finite(x_max)) x_max <- 1
  if (!is.finite(y_max)) y_max <- 1
  
  list(
    x_lim = c(0, x_max * (1 + margin)),
    y_lim = c(0, y_max * (1 + margin))
  )
}

sanitize_label <- function(x) {
  x %>%
    gsub("[[:space:]]+", "_", .) %>%
    gsub("[^A-Za-z0-9_\\-]", "", .)
}

make_plot <- function(df,
                      sex_label,
                      phase_label,
                      averaging_label = "1 h",
                      x_lim,
                      y_lim,
                      show_legend = TRUE) {
  annotation_text <- make_annotation_text(df)
  title_text <- paste0(sex_label, " - ", phase_label, " phase (", averaging_label, ")")
  
  if (phase_label == "All") {
    p <- ggplot(df, aes(x = CCR, y = IID)) +
      geom_point(
        aes(fill = factor(Phase, levels = c("Light", "Dark"))),
        shape = 21,
        color = "black",
        size = point_size,
        stroke = 0.4,
        alpha = 0.9
      ) +
      geom_smooth(
        method = "lm",
        se = TRUE,
        color = "black",
        linewidth = 0.9
      ) +
      scale_fill_manual(
        values = c("Light" = "white", "Dark" = "grey60"),
        breaks = c("Light", "Dark"),
        limits = c("Light", "Dark")
      ) +
      labs(fill = "Phase")
  } else {
    fill_color <- ifelse(phase_label == "Light", "white", "grey60")
    
    p <- ggplot(df, aes(x = CCR, y = IID)) +
      geom_point(
        shape = 21,
        fill = fill_color,
        color = "black",
        size = point_size,
        stroke = 0.4,
        alpha = 0.9
      ) +
      geom_smooth(
        method = "lm",
        se = TRUE,
        color = "black",
        linewidth = 0.9
      )
  }
  
  p +
    labs(
      title = title_text,
      x = "Average CCR (%)",
      y = "Average inter-individual distance (cm)"
    ) +
    annotate(
      "text",
      x = Inf, y = Inf,
      label = annotation_text,
      hjust = 1.05, vjust = 1.2,
      size = annotation_size
    ) +
    scale_x_continuous(
      limits = x_lim,
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = y_lim,
      expand = c(0, 0)
    ) +
    theme_bw(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = title_size),
      axis.title.x = element_text(size = axis_title_size, face = "bold"),
      axis.title.y = element_text(size = axis_title_size, face = "bold"),
      axis.text.x = element_text(size = axis_text_size),
      axis.text.y = element_text(size = axis_text_size),
      legend.title = element_text(size = legend_title_size, face = "bold"),
      legend.text = element_text(size = legend_text_size),
      legend.position = if (show_legend && phase_label == "All") "right" else "none",
      panel.grid.minor = element_blank()
    )
}

save_single_plot_pdf <- function(plot_obj, out_file, width = 7.8, height = 6.2) {
  pdf(out_file, width = width, height = height, onefile = TRUE)
  print(plot_obj)
  dev.off()
}

save_phase_plots <- function(df,
                             output_pdf,
                             output_pdf_nolegend,
                             single_pdf_subdir,
                             averaging_label = "1 h",
                             file_label = "1hr",
                             margin = 0.05) {
  sexes  <- c("Male", "Female")
  phases <- c("Light", "Dark", "All")
  
  dir.create(single_pdf_subdir, recursive = TRUE, showWarnings = FALSE)
  
  axis_limits <- get_axis_limits(df, margin = margin)
  
  # Combined PDF with legend
  pdf(output_pdf, width = 7.8, height = 6.2, onefile = TRUE)
  for (sex_label in sexes) {
    sex_df <- df %>% filter(Sex == sex_label)
    
    for (phase_label in phases) {
      if (phase_label == "All") {
        plot_df <- sex_df
      } else {
        plot_df <- sex_df %>% filter(Phase == phase_label)
      }
      
      p <- make_plot(
        df = plot_df,
        sex_label = sex_label,
        phase_label = phase_label,
        averaging_label = averaging_label,
        x_lim = axis_limits$x_lim,
        y_lim = axis_limits$y_lim,
        show_legend = TRUE
      )
      print(p)
    }
  }
  dev.off()
  
  # Combined PDF without legend
  pdf(output_pdf_nolegend, width = 7.8, height = 6.2, onefile = TRUE)
  for (sex_label in sexes) {
    sex_df <- df %>% filter(Sex == sex_label)
    
    for (phase_label in phases) {
      if (phase_label == "All") {
        plot_df <- sex_df
      } else {
        plot_df <- sex_df %>% filter(Phase == phase_label)
      }
      
      p <- make_plot(
        df = plot_df,
        sex_label = sex_label,
        phase_label = phase_label,
        averaging_label = averaging_label,
        x_lim = axis_limits$x_lim,
        y_lim = axis_limits$y_lim,
        show_legend = FALSE
      )
      print(p)
    }
  }
  dev.off()
  
  # Single PDFs
  for (sex_label in sexes) {
    sex_df <- df %>% filter(Sex == sex_label)
    
    for (phase_label in phases) {
      if (phase_label == "All") {
        plot_df <- sex_df
      } else {
        plot_df <- sex_df %>% filter(Phase == phase_label)
      }
      
      phase_short <- c("Light" = "L", "Dark" = "D", "All" = "A")[phase_label]
      
      if (phase_label == "All") {
        single_file_name_with_legend <- paste0(
          "CCR_IID_corr_",
          sanitize_label(sex_label), "_",
          phase_short, "_",
          sanitize_label(file_label),
          "_legend.pdf"
        )
        
        single_file_name_no_legend <- paste0(
          "CCR_IID_corr_",
          sanitize_label(sex_label), "_",
          phase_short, "_",
          sanitize_label(file_label),
          "_nolegend.pdf"
        )
        
        save_single_plot_pdf(
          make_plot(
            df = plot_df,
            sex_label = sex_label,
            phase_label = phase_label,
            averaging_label = averaging_label,
            x_lim = axis_limits$x_lim,
            y_lim = axis_limits$y_lim,
            show_legend = TRUE
          ),
          file.path(single_pdf_subdir, single_file_name_with_legend)
        )
        
        save_single_plot_pdf(
          make_plot(
            df = plot_df,
            sex_label = sex_label,
            phase_label = phase_label,
            averaging_label = averaging_label,
            x_lim = axis_limits$x_lim,
            y_lim = axis_limits$y_lim,
            show_legend = FALSE
          ),
          file.path(single_pdf_subdir, single_file_name_no_legend)
        )
        
      } else {
        single_file_name <- paste0(
          "CCR_IID_corr_",
          sanitize_label(sex_label), "_",
          phase_short, "_",
          sanitize_label(file_label),
          ".pdf"
        )
        
        save_single_plot_pdf(
          make_plot(
            df = plot_df,
            sex_label = sex_label,
            phase_label = phase_label,
            averaging_label = averaging_label,
            x_lim = axis_limits$x_lim,
            y_lim = axis_limits$y_lim,
            show_legend = FALSE
          ),
          file.path(single_pdf_subdir, single_file_name)
        )
      }
    }
  }
}

make_12h_average <- function(df) {
  df %>%
    group_by(Sex, ID_1, ID_2, Day, Phase) %>%
    summarise(
      IID = mean(IID, na.rm = TRUE),
      CCR = mean(CCR, na.rm = TRUE),
      .groups = "drop"
    )
}

# =========================
# MAIN
# =========================

male_df <- load_and_prepare(
  iid_file_path = file_male_iid,
  ccr_file_path = file_male,
  sex_label = "Male"
)

female_df <- load_and_prepare(
  iid_file_path = file_female_iid,
  ccr_file_path = file_female,
  sex_label = "Female"
)

all_df <- bind_rows(male_df, female_df)

# 1 h plots
save_phase_plots(
  df = all_df,
  output_pdf = output_pdf_1h,
  output_pdf_nolegend = output_pdf_1h_nolegend,
  single_pdf_subdir = file.path(single_pdf_dir, "1hr"),
  averaging_label = "1 h",
  file_label = "1hr",
  margin = axis_margin
)

# 12 h plots
if (make_12h_average_pdf) {
  avg12_df <- make_12h_average(all_df)
  
  save_phase_plots(
    df = avg12_df,
    output_pdf = output_pdf_12h,
    output_pdf_nolegend = output_pdf_12h_nolegend,
    single_pdf_subdir = file.path(single_pdf_dir, "12hr"),
    averaging_label = "12 h",
    file_label = "12hr",
    margin = axis_margin
  )
}

message("Saved combined PDF (legend): ", output_pdf_1h)
message("Saved combined PDF (no legend): ", output_pdf_1h_nolegend)
message("Saved single-plot PDFs in: ", file.path(single_pdf_dir, "1hr"))
message("  Light/Dark: no legend only")
message("  All: legend and no legend")

if (make_12h_average_pdf) {
  message("Saved combined PDF (legend): ", output_pdf_12h)
  message("Saved combined PDF (no legend): ", output_pdf_12h_nolegend)
  message("Saved single-plot PDFs in: ", file.path(single_pdf_dir, "12hr"))
  message("  Light/Dark: no legend only")
  message("  All: legend and no legend")
}