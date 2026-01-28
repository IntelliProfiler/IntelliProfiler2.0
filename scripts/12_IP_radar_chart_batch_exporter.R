# ============================================================
# Radar chart batch exporter (interactive Excel import)
#  - 01 Day-average (raw, rmax=2.0) Male vs Female + stars
#  - 02 Day1..DayN (raw, rmax=2.0) Male vs Female + stars
#  - 03 Day-average (within-sex Z -> min-shift, rmax=4.0) M vs F
#  - 04 Day1..DayN (within-sex Z -> min-shift, rmax=4.0) M vs F
#
# Output: transparent background PDF (vector)
#
# Requirements:
#   install.packages(c("readxl","dplyr","tidyr","stringr"))
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
})

# ====== Global canvas settings (ALL figures identical) ======
PDF_W <- 9.5
PDF_H <- 9.5
CANVAS_MULT <- 1.75   # Often, 1.75–1.90 prevents label clipping

# ---------- helpers ----------
p_to_stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("***")
  if (p < 0.01)  return("**")
  if (p < 0.05)  return("*")
  ""
}

within_sex_z <- function(x) {
  x <- as.numeric(x)
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}

# ---------- NEW: draw labels + bigger bold stars ----------
draw_axis_labels_with_big_stars <- function(angles, rmax, rticks,
                                            base_labels, stars_map, var_keys,
                                            cex_label = 1.5, cex_star = 1.9,
                                            font_label = 1, font_star = 2) {
  stopifnot(length(base_labels) == length(var_keys))
  N <- length(base_labels)
  
  label_r <- rmax * 1.12
  data_r  <- max(rticks)
  
  for (i in seq_len(N)) {
    a <- angles[i]
    x <- label_r*cos(a); y <- label_r*sin(a)
    
    adjx <- ifelse(cos(a) > 0.2, 0, ifelse(cos(a) < -0.2, 1, 0.5))
    adjy <- ifelse(sin(a) > 0.2, 0, ifelse(sin(a) < -0.2, 1, 0.5))
    
    # label
    text(x, y, labels = base_labels[i],
         cex = cex_label, adj = c(adjx, adjy), font = font_label)
    
    # stars
    s <- stars_map[[ var_keys[i] ]]
    if (!is.null(s) && s != "") {
      dx <- 0.10 * data_r
      dy <- 0.10 * data_r
      text(x + dx, y + dy, labels = s,
           cex = cex_star, adj = c(adjx, adjy), font = font_star)
    }
  }
}

# Robust column finder (accepts multiple candidate names)
pick_col <- function(df, candidates) {
  hit <- intersect(names(df), candidates)
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

# Day ordering helper: "Day1","Day2"... or numeric -> sort properly
order_days <- function(days_chr) {
  # try to extract number from strings like "Day1"
  num <- suppressWarnings(as.numeric(str_extract(days_chr, "\\d+")))
  if (all(is.na(num))) return(sort(days_chr))
  days_chr[order(num, days_chr)]
}

# ---------- core radar plot ----------
radar_plot <- function(values_m, values_f,
                       out_file,
                       rmax,
                       rticks,
                       stars_map,
                       axis_labels,   
                       var_keys,      
                       font_size = 18,
                       main = NULL) {
  
  col_m <- "#377EB8"
  col_f <- "#E41A1C"
  
  CEX_TICK   <- 1.5
  CEX_AXIS   <- 1.5
  CEX_LEGEND <- 1.5
  FONT_TICK  <- 2
  FONT_AXIS  <- 1
  FONT_LEG   <- 2
  
  # ★remove dependency on labels_pretty
  N <- length(axis_labels)
  angles <- seq(pi/2, pi/2 - 2*pi, length.out = N + 1)
  angles <- angles[-(N + 1)]
  
  values_m <- c(values_m, values_m[1])
  values_f <- c(values_f, values_f[1])
  ang_w <- c(angles, angles[1])
  
  pdf(out_file, width = PDF_W, height = PDF_H, bg = "transparent")
  op <- par(mar = c(3.5,3.5,3.5,3.5), xpd = NA)
  
  plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "",
       xlim = c(-rmax*CANVAS_MULT, rmax*CANVAS_MULT),
       ylim = c(-rmax*CANVAS_MULT, rmax*CANVAS_MULT),
       main = main)
  
  for (rt in rticks) {
    t <- seq(0, 2*pi, length.out = 400)
    lines(rt*cos(t), rt*sin(t), col = "grey70", lwd = 2)
  }
  for (a in angles) {
    lines(c(0, rmax*cos(a)), c(0, rmax*sin(a)), col = "grey70", lwd = 2)
  }
  
  lab_angle <- pi/10
  for (rt in rticks) {
    text(rt*cos(lab_angle), rt*sin(lab_angle),
         labels = sprintf("%.1f", rt),
         cex = CEX_TICK, font = FONT_TICK)
  }
  
  xy_m <- cbind(values_m*cos(ang_w), values_m*sin(ang_w))
  xy_f <- cbind(values_f*cos(ang_w), values_f*sin(ang_w))
  polygon(xy_m, border = col_m, lwd = 4, col = adjustcolor(col_m, 0.25))
  polygon(xy_f, border = col_f, lwd = 4, col = adjustcolor(col_f, 0.25))
  
  # ★pass explicitly
  draw_axis_labels_with_big_stars(
    angles = angles,
    rmax = rmax,
    rticks = rticks,
    base_labels = axis_labels,
    stars_map = stars_map,
    var_keys = var_keys,
    cex_label = CEX_AXIS,
    cex_star  = CEX_AXIS * 1.25,
    font_label = FONT_AXIS,
    font_star  = 2
  )
  
  legend("topright", inset = c(-0.02, -0.02),
         legend = c("Male","Female"),
         col = c(col_m, col_f), lwd = 4, bty = "n",
         cex = CEX_LEGEND, text.font = FONT_LEG)
  
  par(op)
  dev.off()
}

# ---------- main ----------
cat("▶ Please select the Excel file\n")
infile <- file.choose()
df <- readxl::read_excel(infile)

# --- column normalization (adjust here if your headers differ) ---
col_sex  <- pick_col(df, c("Sex","sex"))
col_id   <- pick_col(df, c("ID","Id","id","MouseID","Mouse_ID"))
col_day  <- pick_col(df, c("Day","day"))
col_ld   <- pick_col(df, c("LD","ld","LightDark","Light_Dark","Phase"))
col_trav <- pick_col(df, c("Travel dist","Travel_dist","Travel_Distance","TravelDistance"))
col_iid  <- pick_col(df, c("Inter-indiv dist","Inter_indiv_dist","InterIndiv_Distance","InterIndivDistance","Inter-indiv distance"))
col_ccr  <- pick_col(df, c("CCR","Avg_CCR","Avg_CCR_Light","Avg_CCR_Dark"))

needed <- c(col_sex,col_id,col_day,col_ld,col_trav,col_iid,col_ccr)
if (any(is.na(needed))) {
  stop("Required column name(s) were not found. Please check your column headers.\n",
       "Missing column(s): ",
       paste(c("Sex","ID","Day","LD","Travel dist","Inter-indiv dist","CCR")[is.na(needed)],
             collapse=", "))
}

df2 <- df %>%
  transmute(
    Sex = as.character(.data[[col_sex]]),
    ID  = as.character(.data[[col_id]]),
    Day = as.character(.data[[col_day]]),
    LD  = as.character(.data[[col_ld]]),
    Travel_Distance = as.numeric(.data[[col_trav]]),
    InterIndiv_Distance = as.numeric(.data[[col_iid]]),
    CCR = as.numeric(.data[[col_ccr]])
  ) %>%
  mutate(
    Sex = ifelse(str_detect(Sex, regex("^m", ignore_case=TRUE)), "Male",
                 ifelse(str_detect(Sex, regex("^f", ignore_case=TRUE)), "Female", Sex)),
    LD  = ifelse(str_detect(LD, regex("light", ignore_case=TRUE)), "Light",
                 ifelse(str_detect(LD, regex("dark", ignore_case=TRUE)), "Dark", LD))
  )

# --- wide per (Sex,ID,Day): create 6 vars (Light/Dark x 3 metrics) ---
wide <- df2 %>%
  pivot_wider(
    id_cols = c(Sex,ID,Day),
    names_from = LD,
    values_from = c(Travel_Distance,InterIndiv_Distance,CCR),
    values_fn = mean,
    names_sep = "_"
  )

vars_order <- c(
  "Travel_Distance_Light",
  "InterIndiv_Distance_Light",
  "CCR_Light",
  "Travel_Distance_Dark",
  "InterIndiv_Distance_Dark",
  "CCR_Dark"
)

labels_pretty <- c(
  "Light Travel dist",
  "Light Inter-indiv dist",
  "Light CCR",
  "Dark Travel dist",
  "Dark Inter-indiv dist",
  "Dark CCR"
)

# ============================================================
# Plain labels (no stars)  ★use this name consistently★
# ============================================================
lab_plain_expr <- lapply(labels_pretty, function(s) parse(text = sprintf('"%s"', s))[[1]])

# --- output folder ---
out_dir <- file.path(dirname(infile),
                     paste0(tools::file_path_sans_ext(basename(infile)),
                            "_Radar_Output_PDF"))
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
cat("✅ Output folder: ", out_dir, "\n")

# ============================================================
# Color function (must be defined BEFORE PANEL 00/00b)
# ============================================================
generate_color_by_index <- function(index) {
  h <- (index * 47) %% 360
  c <- 60 + (index * 23) %% 40
  l <- 55 + (index * 31) %% 30
  hcl(h = h, c = c, l = l)
}

# --- significance (Welch t-test on ID-level values) -> stars per axis ---
sig_stars <- function(dat_wide) {
  out <- setNames(rep("", length(vars_order)), vars_order)
  for (v in vars_order) {
    m <- dat_wide %>% filter(Sex=="Male")   %>% pull(.data[[v]]) %>% na.omit()
    f <- dat_wide %>% filter(Sex=="Female") %>% pull(.data[[v]]) %>% na.omit()
    if (length(m) >= 2 && length(f) >= 2) {
      p <- tryCatch(t.test(m, f, var.equal = FALSE)$p.value, error = function(e) NA_real_)
      out[v] <- p_to_stars(p)
    }
  }
  out
}

empty_stars <- setNames(rep("", length(vars_order)), vars_order)

radar_plot_multi <- function(series_list, axis_labels_expr,
                             out_file,
                             rmax,
                             rticks,
                             font_size = 18,  # ←keep (not used here)
                             main = NULL) {
  
  # ===== Match PNG appearance: fixed scaling for text =====
  CEX_TICK   <- 1.5
  CEX_AXIS   <- 1.5
  CEX_LEGEND <- 1.5
  FONT_TICK  <- 2
  FONT_AXIS  <- 1   # set 2 for bold
  FONT_LEG   <- 2
  
  N <- length(labels_pretty)
  angles <- seq(pi/2, pi/2 - 2*pi, length.out = N + 1)
  angles <- angles[-(N + 1)]
  ang_w <- c(angles, angles[1])
  
  pdf(out_file, width = PDF_W, height = PDF_H, bg = "transparent")
  op <- par(mar = c(2,2,2,2), xpd = NA)
  
  plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "",
       xlim = c(-rmax*CANVAS_MULT, rmax*CANVAS_MULT),
       ylim = c(-rmax*CANVAS_MULT, rmax*CANVAS_MULT),
       main = main)
  
  for (rt in rticks) {
    t <- seq(0, 2*pi, length.out = 400)
    lines(rt*cos(t), rt*sin(t), col = "grey70", lwd = 2)
  }
  for (a in angles) {
    lines(c(0, rmax*cos(a)), c(0, rmax*sin(a)), col = "grey70", lwd = 2)
  }
  
  lab_angle <- pi/10
  for (rt in rticks) {
    text(rt*cos(lab_angle), rt*sin(lab_angle),
         labels = sprintf("%.1f", rt),
         cex = CEX_TICK, font = FONT_TICK)
  }
  
  # draw each series
  for (nm in names(series_list)) {
    v <- series_list[[nm]]$values
    col <- series_list[[nm]]$col
    
    v <- as.numeric(v)
    v <- c(v, v[1])
    xy <- cbind(v*cos(ang_w), v*sin(ang_w))
    
    polygon(xy, border = col, lwd = 3, col = adjustcolor(col, 0.18))
  }
  
  data_r <- max(rticks)      # raw: 2.0
  label_r <- data_r * 1.12   # place labels just outside data radius
  for (i in seq_len(N)) {
    a <- angles[i]
    x <- label_r*cos(a)
    y <- label_r*sin(a)
    
    adjx <- ifelse(cos(a) > 0.2, 0, ifelse(cos(a) < -0.2, 1, 0.5))
    adjy <- ifelse(sin(a) > 0.2, 0, ifelse(sin(a) < -0.2, 1, 0.5))
    
    text(x, y, labels = axis_labels_expr[[i]],
         cex = CEX_AXIS, adj = c(adjx, adjy), font = FONT_AXIS)
  }
  
  legend("topright", inset = c(-0.02, -0.02),
         legend = names(series_list),
         col = vapply(series_list, \(x) x$col, character(1)),
         lwd = 3, bty = "n",
         cex = CEX_LEGEND, text.font = FONT_LEG)
  
  par(op); dev.off()
}

# ============================================================
# PANEL 00: Each ID (Day-average raw)
#  - Split output folders by sex (Male / Female)
#  - One PDF per ID (Day-average, raw)
# ============================================================

out_dir_id <- file.path(out_dir, "00_Individual_ID_DayAvg_raw")
dir.create(out_dir_id, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir_id, "Male"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir_id, "Female"), showWarnings = FALSE, recursive = TRUE)

id_dayavg <- wide %>%
  group_by(Sex, ID) %>%
  summarise(across(all_of(vars_order), ~mean(.x, na.rm = TRUE)), .groups = "drop")

id_levels <- sort(unique(id_dayavg$ID))
palette_id <- setNames(
  sapply(seq_along(id_levels), generate_color_by_index),
  id_levels
)

for (sx in c("Male", "Female")) {
  dat_sx <- id_dayavg %>% filter(Sex == sx)
  
  for (id in dat_sx$ID) {
    v <- dat_sx %>% filter(ID == id) %>% select(all_of(vars_order)) %>% unlist(use.names = FALSE)
    
    series_list <- list()
    series_list[[id]] <- list(values = v, col = palette_id[[id]])
    
    radar_plot_multi(
      series_list = series_list,
      axis_labels_expr = lab_plain_expr,
      out_file = file.path(out_dir_id, sx, paste0("ID_", id, "_DayAvg_raw_rmax2.pdf")),
      rmax = 2.0,
      rticks = c(0.5, 1.0, 1.5, 2.0),
      font_size = 18,
      main = NULL
    )
  }
}

# ============================================================
# PANEL 00b: Each ID per Day (raw)
#  - Split folders by day (Day1..Day4, etc.)
#  - Subfolders by sex (Male/Female)
#  - One PDF per ID (raw, rmax=2.0)
# ============================================================

out_dir_id_day <- file.path(out_dir, "00b_Individual_ID_ByDay_raw_rmax2")
dir.create(out_dir_id_day, showWarnings = FALSE, recursive = TRUE)

# --- days list ---
days_all  <- wide %>% distinct(Day) %>% pull(Day) %>% order_days()
days_1to4 <- days_all[days_all %in% c("Day1","Day2","Day3","Day4")]

for (d in days_1to4) {
  
  day_dir <- file.path(out_dir_id_day, d)
  dir.create(day_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(day_dir, "Male"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(day_dir, "Female"), showWarnings = FALSE, recursive = TRUE)
  
  dat_d <- wide %>% filter(Day == d)
  
  for (sx in c("Male", "Female")) {
    dat_sx <- dat_d %>% filter(Sex == sx)
    ids_here <- sort(unique(dat_sx$ID))
    
    for (id in ids_here) {
      v <- dat_sx %>%
        filter(ID == id) %>%
        select(all_of(vars_order)) %>%
        unlist(use.names = FALSE)
      
      if (all(is.na(v))) next
      
      col_id <- palette_id[[id]]
      if (is.null(col_id) || is.na(col_id)) col_id <- "grey30"
      
      series_list <- list()
      series_list[[id]] <- list(values = v, col = col_id)
      
      radar_plot_multi(
        series_list = series_list,
        axis_labels_expr = lab_plain_expr,
        out_file = file.path(day_dir, sx, paste0("ID_", id, "_", d, "_raw_rmax2.pdf")),
        rmax = 2.0,
        rticks = c(0.5, 1.0, 1.5, 2.0),
        font_size = 18,
        main = NULL
      )
    }
  }
}

# ============================================================
# PANEL 00c: Each ID (Day-average raw, rmax = 2.0 like PANEL 01)
# ============================================================

out_dir_id_rmax2 <- file.path(out_dir, "00c_Individual_ID_DayAvg_raw_rmax2")
dir.create(out_dir_id_rmax2, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir_id_rmax2, "Male"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir_id_rmax2, "Female"), showWarnings = FALSE, recursive = TRUE)

# ============================================================
# PANEL 00c: Each ID (Day-average raw, rmax=2.0)
# ============================================================

for (sx in c("Male", "Female")) {
  dat_sx <- id_dayavg %>% filter(Sex == sx)
  
  for (id in dat_sx$ID) {
    v <- dat_sx %>%
      filter(ID == id) %>%
      select(all_of(vars_order)) %>%
      unlist(use.names = FALSE)
    
    series_list <- list()
    series_list[[id]] <- list(values = v, col = palette_id[[id]])
    
    radar_plot_multi(
      series_list = series_list,
      axis_labels_expr = lab_plain_expr,
      out_file = file.path(
        out_dir_id_rmax2,
        sx,
        paste0("ID_", id, "_DayAvg_raw_rmax2.pdf")
      ),
      rmax = 2.0,
      rticks = c(0.5, 1.0, 1.5, 2.0),
      font_size = 18,
      main = NULL
    )
  }
}

# ---------- PANEL 01: Day-average raw ----------
dayavg_by_id <- wide %>%
  group_by(Sex,ID) %>%
  summarise(across(all_of(vars_order), ~mean(.x, na.rm=TRUE)), .groups="drop")

stars_01 <- sig_stars(dayavg_by_id)

dayavg_sex <- dayavg_by_id %>%
  group_by(Sex) %>%
  summarise(across(all_of(vars_order), mean, na.rm=TRUE), .groups="drop")

vals_m <- as.numeric(dayavg_sex %>% filter(Sex=="Male")   %>% select(all_of(vars_order)))
vals_f <- as.numeric(dayavg_sex %>% filter(Sex=="Female") %>% select(all_of(vars_order)))

radar_plot(vals_m, vals_f,
           out_file = file.path(out_dir, "01_DayAvg_raw_rmax2_Male_vs_Female.pdf"),
           rmax = 2.0, rticks = c(0.5,1.0,1.5,2.0),
           stars_map = stars_01,
           axis_labels = labels_pretty,  
           var_keys    = vars_order,     
           font_size = 18, main = NULL)

# ============================================================
# PANEL 01b: Day-average raw (Male only / Female only)
# ============================================================

out_dir_single <- file.path(out_dir, "01b_DayAvg_raw_SingleSex")
dir.create(out_dir_single, showWarnings = FALSE, recursive = TRUE)

# --- Male only ---
series_male <- list(
  Male = list(
    values = vals_m,
    col = "#377EB8"
  )
)

radar_plot_multi(
  series_list = series_male,
  axis_labels_expr = lab_plain_expr,
  out_file = file.path(out_dir_single, "01b_DayAvg_raw_rmax2_Male_only.pdf"),
  rmax = 2.0,
  rticks = c(0.5, 1.0, 1.5, 2.0),
  font_size = 18,
  main = NULL
)

# --- Female only ---
series_female <- list(
  Female = list(
    values = vals_f,
    col = "#E41A1C"
  )
)

radar_plot_multi(
  series_list = series_female,
  axis_labels_expr = lab_plain_expr,
  out_file = file.path(out_dir_single, "01b_DayAvg_raw_rmax2_Female_only.pdf"),
  rmax = 2.0,
  rticks = c(0.5, 1.0, 1.5, 2.0),
  font_size = 18,
  main = NULL
)

# ---------- PANEL 02: Day1..Day4 raw ----------
out_dir_02_single <- file.path(out_dir, "02b_Day_byDay_raw_SingleSex")
dir.create(out_dir_02_single, showWarnings = FALSE, recursive = TRUE)

for (d in days_1to4) {
  wd <- wide %>% filter(Day == d)
  
  by_id <- wd %>%
    group_by(Sex,ID) %>%
    summarise(across(all_of(vars_order), mean, na.rm=TRUE), .groups="drop")
  
  stars_d <- sig_stars(by_id)
  
  mean_sex <- by_id %>%
    group_by(Sex) %>%
    summarise(across(all_of(vars_order), mean, na.rm=TRUE), .groups="drop")
  
  if (!all(c("Male","Female") %in% mean_sex$Sex)) next
  
  vm <- as.numeric(mean_sex %>% filter(Sex=="Male")   %>% select(all_of(vars_order)))
  vf <- as.numeric(mean_sex %>% filter(Sex=="Female") %>% select(all_of(vars_order)))
  
  # (02) Male vs Female
  radar_plot(vm, vf,
             out_file = file.path(out_dir, paste0("02_", d, "_raw_rmax2_Male_vs_Female.pdf")),
             rmax = 2.0, rticks = c(0.5,1.0,1.5,2.0),
             stars_map = stars_d,
             axis_labels = labels_pretty,
             var_keys    = vars_order,
             font_size = 18, main = NULL)
  
  # (02b) SingleSex: Male only
  series_m <- list(Male = list(values = vm, col = "#377EB8"))
  radar_plot_multi(
    series_list = series_m,
    axis_labels_expr = lab_plain_expr,
    out_file = file.path(out_dir_02_single,
                         paste0("02b_", d, "_raw_rmax2_Male_only.pdf")),
    rmax = 2.0,
    rticks = c(0.5,1.0,1.5,2.0),
    font_size = 18
  )
  
  # (02b) SingleSex: Female only
  series_f <- list(Female = list(values = vf, col = "#E41A1C"))
  radar_plot_multi(
    series_list = series_f,
    axis_labels_expr = lab_plain_expr,
    out_file = file.path(out_dir_02_single,
                         paste0("02b_", d, "_raw_rmax2_Female_only.pdf")),
    rmax = 2.0,
    rticks = c(0.5,1.0,1.5,2.0),
    font_size = 18
  )
}

# ---------- PANEL 03: within-sex Z (Day-average) -> min-shift ----------
zm <- within_sex_z(vals_m)
zf <- within_sex_z(vals_f)

min_all <- min(c(zm, zf), na.rm=TRUE)
zm_s <- zm - min_all
zf_s <- zf - min_all

# (If PANEL 03/04 have no stars, keep labels_pretty as expressions)
radar_plot(zm_s, zf_s,
           out_file = file.path(out_dir, "03_Zwithin_minshift_rmax4_DayAvg_Male_vs_Female.pdf"),
           rmax = 4.0, rticks = c(1,2,3,4),
           stars_map = empty_stars,
           axis_labels = labels_pretty,  
           var_keys    = vars_order,     
           font_size = 18, main = NULL)

# ============================================================
# PANEL 03b: within-sex Z (Day-average) SingleSex
# ============================================================

out_dir_03_single <- file.path(out_dir, "03b_Zwithin_DayAvg_SingleSex")
dir.create(out_dir_03_single, showWarnings = FALSE, recursive = TRUE)

# Male only
series_m <- list(Male = list(values = zm_s, col = "#377EB8"))
radar_plot_multi(
  series_list = series_m,
  axis_labels_expr = lab_plain_expr,
  out_file = file.path(out_dir_03_single,
                       "03b_Zwithin_DayAvg_rmax4_Male_only.pdf"),
  rmax = 4.0,
  rticks = c(1,2,3,4),
  font_size = 18
)

# Female only
series_f <- list(Female = list(values = zf_s, col = "#E41A1C"))
radar_plot_multi(
  series_list = series_f,
  axis_labels_expr = lab_plain_expr,
  out_file = file.path(out_dir_03_single,
                       "03b_Zwithin_DayAvg_rmax4_Female_only.pdf"),
  rmax = 4.0,
  rticks = c(1,2,3,4),
  font_size = 18
)

# ---------- PANEL 04: within-sex Z (Day1..Day4) -> min-shift ----------
out_dir_04_single <- file.path(out_dir, "04b_Zwithin_byDay_SingleSex")
dir.create(out_dir_04_single, showWarnings = FALSE, recursive = TRUE)

for (d in days_1to4) {
  wd <- wide %>% filter(Day == d)
  
  by_id <- wd %>%
    group_by(Sex,ID) %>%
    summarise(across(all_of(vars_order), mean, na.rm=TRUE), .groups="drop")
  
  mean_sex <- by_id %>%
    group_by(Sex) %>%
    summarise(across(all_of(vars_order), mean, na.rm=TRUE), .groups="drop")
  
  if (!all(c("Male","Female") %in% mean_sex$Sex)) next
  
  vm <- as.numeric(mean_sex %>% filter(Sex=="Male")   %>% select(all_of(vars_order)))
  vf <- as.numeric(mean_sex %>% filter(Sex=="Female") %>% select(all_of(vars_order)))
  
  zm <- within_sex_z(vm)
  zf <- within_sex_z(vf)
  
  min_all <- min(c(zm, zf), na.rm=TRUE)
  zm_s <- zm - min_all
  zf_s <- zf - min_all
  
  # (04) Male vs Female
  radar_plot(zm_s, zf_s,
             out_file = file.path(out_dir, paste0("04_Zwithin_minshift_rmax4_", d, "_Male_vs_Female.pdf")),
             rmax = 4.0, rticks = c(1,2,3,4),
             stars_map = empty_stars,
             axis_labels = labels_pretty,
             var_keys    = vars_order,
             font_size = 18, main = NULL)
  
  # (04b) SingleSex: Male only
  series_m <- list(Male = list(values = zm_s, col = "#377EB8"))
  radar_plot_multi(
    series_list = series_m,
    axis_labels_expr = lab_plain_expr,
    out_file = file.path(out_dir_04_single,
                         paste0("04b_Zwithin_", d, "_rmax4_Male_only.pdf")),
    rmax = 4.0,
    rticks = c(1,2,3,4),
    font_size = 18
  )
  
  # (04b) SingleSex: Female only
  series_f <- list(Female = list(values = zf_s, col = "#E41A1C"))
  radar_plot_multi(
    series_list = series_f,
    axis_labels_expr = lab_plain_expr,
    out_file = file.path(out_dir_04_single,
                         paste0("04b_Zwithin_", d, "_rmax4_Female_only.pdf")),
    rmax = 4.0,
    rticks = c(1,2,3,4),
    font_size = 18
  )
}

cat("Completed.\n",
    "Output folder: ", out_dir, "\n", sep="")
