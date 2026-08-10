# ============================================================
# 0. Setup
# ============================================================

library(readxl)
library(openxlsx)
library(dplyr)
library(ggplot2)
library(patchwork)
library(lme4)
library(emmeans)
library(car)
library(flextable)
library(officer)
library(cluster)
library(factoextra)
library(dendextend)
library(MASS)
library(phia)
library(fmsb)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(ggspatial)
library(cowplot)
library(ggplotify)
library(vegan)
library(permute)
library(ggord)

set.seed(123)

FILE_ALL <- "Raw data - 0701.xlsx"
d <- as.data.frame(read_excel(FILE_ALL, na = "NA"))
names(d) <- trimws(names(d))
names(d)[names(d) == "LSM"] <- "LMA"

TNR10    <- fp_text(font.family = "Times New Roman", font.size = 10)
TNR10_B  <- fp_text(font.family = "Times New Roman", font.size = 10, bold = TRUE)
TNR10_I  <- fp_text(font.family = "Times New Roman", font.size = 10, italic = TRUE)
TNR10_BI <- fp_text(font.family = "Times New Roman", font.size = 10, bold = TRUE, italic = TRUE)
TNR9     <- fp_text(font.family = "Times New Roman", font.size = 9)
TNR9_B   <- fp_text(font.family = "Times New Roman", font.size = 9, bold = TRUE)
TNR9_I   <- fp_text(font.family = "Times New Roman", font.size = 9, italic = TRUE)

# ============================================================
# 1. Optional raw-data cleaning
# ============================================================

RUN_DATA_CLEANING <- FALSE

if (RUN_DATA_CLEANING) {
  FILE_3REP <- "1. Raw data - 3 Replicates - 0611.xlsx"
  FILE_1REP <- "2. Raw data - 1 Replicates - 0611.xlsx"
  CLEANED_OUTPUT <- "3. Family mean combined data - 0611.xlsx"

  d3 <- read_excel(FILE_3REP, na = "NA")
  d1 <- read_excel(FILE_1REP, na = "NA")

  id_cols <- c("Group", "Range", "Latitude", "Longitude", "Collection_site", "Family")
  trait_cols <- c(
    "Height", "Aboveground_biomass", "Belowground_biomass", "Root_shoot",
    "Total_biomass", "Shoot_density", "Seed_set", "LSM", "Leaf_toughness",
    "Laelia_consumption", "Locust_consumption"
  )

  fam_means <- aggregate(d3[trait_cols], by = list(Family = d3$Family), FUN = mean, na.rm = TRUE)
  fam_ids <- unique(d3[id_cols])
  d3_family <- merge(fam_ids, fam_means, by = "Family")
  combined <- merge(d3_family, d1, by = id_cols)

  stopifnot(nrow(combined) == length(unique(d3$Family)))
  stopifnot(nrow(combined) == nrow(d1))

  numeric_columns <- vapply(combined, is.numeric, logical(1))
  for (j in which(numeric_columns)) {
    combined[[j]] <- round(combined[[j]], 4)
    combined[[j]][is.nan(combined[[j]])] <- NA
  }

  combined <- combined[match(unique(d3$Family), combined$Family), ]
  rownames(combined) <- NULL

  wb <- createWorkbook()
  addWorksheet(wb, "Sheet1")
  writeData(wb, "Sheet1", combined)
  addStyle(
    wb, "Sheet1", style = createStyle(numFmt = "0.0000"),
    rows = 2:(nrow(combined) + 1), cols = which(numeric_columns), gridExpand = TRUE
  )
  setColWidths(wb, "Sheet1", cols = 1:ncol(combined), widths = "auto")
  saveWorkbook(wb, CLEANED_OUTPUT, overwrite = TRUE)
}

# ============================================================
# 2. Figure 2: collection-site map
# ============================================================

site_labels <- c(
  MC = "Morehead City (MC)",
  SA = "Sapelo Island 1 (SA)",
  SB = "Sapelo Island 2 (SB)",
  TB = "Tampa Bay (TB)",
  TS = "Tangshan (TS)",
  DY = "Dongying (DY)",
  LYG = "Lianyungang (LYG)",
  YC = "Yancheng (YC)",
  SH = "Shanghai (SH)",
  CX = "Cixi (CX)",
  TZ = "Taizhou (TZ)",
  WZ = "Wenzhou (WZ)",
  FZ = "Fuzhou (FZ)",
  QZ = "Quanzhou (QZ)",
  ZZ = "Zhangzhou (ZZ)",
  BH = "Beihai (BH)",
  LZ = "Leizhou (LZ)"
)

sites <- unique(d[, c("Collection_site", "Longitude", "Latitude", "Group")])
names(sites) <- c("site", "lon", "lat", "group")
sites$label <- site_labels[sites$site]
sites$range <- ifelse(grepl("Native", sites$group), "Native", "Introduced")

world <- ne_countries(scale = "medium", returnclass = "sf")

crs_us <- st_crs("+proj=aea +lat_1=25 +lat_2=47 +lat_0=0 +lon_0=-83 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
crs_china <- st_crs("+proj=aea +lat_1=25 +lat_2=47 +lat_0=0 +lon_0=117.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

bbox_us <- st_as_sfc(st_bbox(c(xmin = -96, xmax = -70, ymin = 18, ymax = 42.5), crs = 4326))
bbox_china <- st_as_sfc(st_bbox(c(xmin = 104.5, xmax = 130.5, ymin = 18, ymax = 42.5), crs = 4326))
grat_us <- st_graticule(bbox_us, lon = seq(-95, -75, 5), lat = seq(20, 40, 5))
grat_china <- st_graticule(bbox_china, lon = seq(105, 130, 5), lat = seq(20, 40, 5))

map_theme <- theme_bw(base_size = 11) +
  theme(
    panel.grid = element_line(colour = "grey72", linewidth = 0.3),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.border = element_rect(colour = "black", linewidth = 0.7, fill = NA),
    axis.title = element_blank(),
    axis.text = element_text(colour = "black", size = 14),
    axis.ticks = element_line(colour = "black", linewidth = 0.5),
    plot.margin = margin(4, 5, 4, 5)
  )

map_us <- ggplot() +
  geom_sf(data = world, fill = "grey85", colour = "grey55", linewidth = 0.25) +
  geom_sf(data = grat_us, colour = "grey72", linewidth = 0.3) +
  geom_point(
    data = sites[sites$range == "Native", ],
    aes(lon, lat), shape = 21, colour = "black", fill = "grey50", size = 4.4, stroke = 0.6
  ) +
  geom_text_repel(
    data = sites[sites$range == "Native", ],
    aes(lon, lat, label = label),
    size = 4.8, hjust = 1, nudge_x = -1.2, direction = "y",
    segment.colour = NA, box.padding = 0.14, point.padding = 0.3,
    max.overlaps = Inf, seed = 1
  ) +
  scale_x_continuous(
    breaks = seq(-95, -75, 5),
    labels = c("95° W", "90° W", "85° W", "80° W", "75° W"),
    position = "top"
  ) +
  scale_y_continuous(
    breaks = seq(20, 40, 5),
    labels = c("20° N", "25° N", "30° N", "35° N", "40° N"),
    position = "left"
  ) +
  annotation_north_arrow(
    location = "tl", which_north = "grid",
    height = grid::unit(1.5, "cm"), width = grid::unit(1.2, "cm"),
    pad_x = grid::unit(0.15, "cm"), pad_y = grid::unit(0.45, "cm"),
    style = north_arrow_fancy_orienteering(text_size = 11, line_width = 1, fill = c("white", "grey25"))
  ) +
  annotation_scale(
    location = "br", width_hint = 0.33, text_cex = 1.0,
    height = grid::unit(0.30, "cm"),
    pad_x = grid::unit(0.25, "cm"), pad_y = grid::unit(0.25, "cm")
  ) +
  annotate("text", x = -70.7, y = 40.8, label = "(a)", hjust = 1, vjust = 1, size = 6.5) +
  coord_sf(crs = crs_us, default_crs = st_crs(4326), xlim = c(-96, -70), ylim = c(18, 42.5), expand = FALSE) +
  map_theme

map_china <- ggplot() +
  geom_sf(data = world, fill = "grey85", colour = "grey55", linewidth = 0.25) +
  geom_sf(data = grat_china, colour = "grey72", linewidth = 0.3) +
  geom_point(
    data = sites[sites$range == "Introduced", ],
    aes(lon, lat), shape = 21, colour = "black", fill = "grey50", size = 4.4, stroke = 0.6
  ) +
  geom_text_repel(
    data = sites[sites$range == "Introduced", ],
    aes(lon, lat, label = label),
    size = 4.8, hjust = 1, nudge_x = -1.0, direction = "y",
    segment.colour = NA, box.padding = 0.14, point.padding = 0.3,
    max.overlaps = Inf, seed = 1
  ) +
  scale_x_continuous(
    breaks = seq(105, 130, 5),
    labels = c("105° E", "110° E", "115° E", "120° E", "125° E", "130° E"),
    position = "top"
  ) +
  scale_y_continuous(
    breaks = seq(20, 40, 5),
    labels = c("20° N", "25° N", "30° N", "35° N", "40° N"),
    position = "right"
  ) +
  annotation_north_arrow(
    location = "tl", which_north = "grid",
    height = grid::unit(1.5, "cm"), width = grid::unit(1.2, "cm"),
    pad_x = grid::unit(0.15, "cm"), pad_y = grid::unit(0.45, "cm"),
    style = north_arrow_fancy_orienteering(text_size = 11, line_width = 1, fill = c("white", "grey25"))
  ) +
  annotation_scale(
    location = "br", width_hint = 0.33, text_cex = 1.0,
    height = grid::unit(0.30, "cm"),
    pad_x = grid::unit(0.25, "cm"), pad_y = grid::unit(0.25, "cm")
  ) +
  annotate("text", x = 129.8, y = 40.8, label = "(b)", hjust = 1, vjust = 1, size = 6.5) +
  coord_sf(crs = crs_china, default_crs = st_crs(4326), xlim = c(104.5, 130.5), ylim = c(18, 42.5), expand = FALSE) +
  map_theme

figure_2 <- (map_us | map_china) + plot_layout(widths = c(1, 1))
ggsave("Figure_2.pdf", figure_2, width = 12, height = 6.4)

# ============================================================
# 3. LMM, Figure 3, Table S2 and Table S6
# ============================================================

trait_info <- data.frame(
  trait = c(
    "Height", "Aboveground_biomass", "Total_biomass", "Root_shoot",
    "Shoot_density", "Seed_set", "Carbon", "Nitrogen", "C_N", "LMA",
    "Leaf_toughness", "Alkaloids", "Flavonoids", "Tannins", "Phenolics",
    "Lignins", "Laelia_consumption", "Locust_consumption"
  ),
  model_trait = c(
    "log_Height", "log_Aboveground_biomass", "log_Total_biomass", "log_Root_shoot",
    "Shoot_density", "Seed_set", "log_Carbon", "log_Nitrogen", "log_C_N", "log_LMA",
    "log_Leaf_toughness", "log_Alkaloids", "log_Flavonoids", "log_Tannins", "log_Phenolics",
    "log_Lignins", "log_Laelia_consumption", "log_Locust_consumption"
  ),
  label = c(
    "Height (cm)", "Aboveground biomass (g)", "Total biomass (g)", "Root : shoot",
    "Shoot density (#shoots/pot)", "Seed set (#seeds/pot)", "Carbon (%)", "Nitrogen (%)", "C : N",
    "LMA (mg/cm²)", "Toughness (N)", "Alkaloids (mg/g)", "Flavonoids (mg/g)", "Tannins (mg/g)",
    "Phenolics (mg/g)", "Lignins (mg/g)", "Laelia consumption (g)", "Locusta consumption (g)"
  ),
  table_label = c(
    "Height", "Aboveground biomass", "Total biomass", "Root: shoot", "Shoot density", "Seed set",
    "Carbon", "Nitrogen", "C:N", "LMA", "Leaf toughness", "Alkaloids", "Flavonoids", "Tannins",
    "Phenolics", "Lignins", "Laelia consumption", "Locusta consumption"
  ),
  stringsAsFactors = FALSE
)

lmm_data <- d
for (v in trait_info$trait) lmm_data[[v]] <- suppressWarnings(as.numeric(lmm_data[[v]]))
lmm_data$Latitude <- suppressWarnings(as.numeric(lmm_data$Latitude))
for (v in trait_info$trait[!trait_info$trait %in% c("Shoot_density", "Seed_set")]) lmm_data[[paste0("log_", v)]] <- log(lmm_data[[v]])

lmm_data$Group <- factor(
  lmm_data$Group,
  levels = c("Native_main", "Native_Tampa", "Introduced_low", "Introduced_high")
)

group_labels <- c(
  Native_main = "Nat_main",
  Native_Tampa = "Nat_Tampa",
  Introduced_low = "Intro_low",
  Introduced_high = "Intro_high"
)

group_fills <- c(
  Native_main = "#7FB069",
  Native_Tampa = "#E8A66E",
  Introduced_low = "#7FA9C8",
  Introduced_high = "#D88D9A"
)

group_lines <- c(
  Native_main = "#3F6B33",
  Native_Tampa = "#A85F23",
  Introduced_low = "#3F6B85",
  Introduced_high = "#9E4555"
)

S6 <- data.frame()

for (i in seq_len(nrow(trait_info))) {
  dat_i <- lmm_data[!is.na(lmm_data[[trait_info$model_trait[i]]]), ]

  m <- lmer(
    as.formula(paste0("`", trait_info$model_trait[i], "` ~ Group + (1|Collection_site)")),
    data = dat_i,
    REML = TRUE,
    control = lmerControl(optimizer = "bobyqa")
  )

  emm <- emmeans(m, ~ Group, lmer.df = "asymptotic")

  ct <- as.data.frame(
    contrast(
      emm,
      method = list(
        Native_vs_Intro = c(3 / 4, 1 / 4, -8 / 13, -5 / 13),
        Native_within = c(1, -1, 0, 0),
        Intro_within = c(0, 0, 1, -1)
      ),
      adjust = "none"
    )
  )

  vc <- as.data.frame(VarCorr(m))
  sd_value <- vc$sdcor[vc$grp != "Residual"][1]

  tmp <- data.frame(
    comparison = c("Native–introduced", "Native_main–Native_Tampa", "Intro_low–Intro_high"),
    contrast = c("Native_vs_Intro", "Native_within", "Intro_within"),
    trait = trait_info$trait[i],
    dependent_variable = trait_info$table_label[i],
    chi = ct$z.ratio^2,
    p = pchisq(ct$z.ratio^2, df = 1, lower.tail = FALSE),
    sd = sd_value,
    singular = isSingular(m),
    stringsAsFactors = FALSE
  )

  S6 <- rbind(S6, tmp)
}

S6$comparison <- factor(
  S6$comparison,
  levels = c("Native–introduced", "Native_main–Native_Tampa", "Intro_low–Intro_high")
)
S6 <- S6[order(S6$comparison, match(S6$trait, trait_info$trait)), ]
S6$comparison <- as.character(S6$comparison)
rownames(S6) <- NULL

tabS6 <- data.frame(
  comparison = S6$comparison,
  dependent_variable = S6$dependent_variable,
  chi = ifelse(is.na(S6$chi), "NA", sprintf("%.3f", S6$chi)),
  p = ifelse(is.na(S6$p), "NA", ifelse(S6$p < 0.001, "<0.001", sprintf("%.3f", S6$p))),
  sd = ifelse(is.na(S6$sd), "-", sprintf("%.3f", S6$sd)),
  stringsAsFactors = FALSE
)

sig_rows_S6 <- which(!is.na(S6$p) & S6$p < 0.05)

ftS6 <- flextable(tabS6)
ftS6 <- set_header_labels(
  ftS6,
  comparison = "Compared groups",
  dependent_variable = "Dependent variable",
  chi = "χ²",
  p = "p",
  sd = "s.d."
)
ftS6 <- add_header_row(
  ftS6, top = TRUE,
  values = c("Compared groups", "Dependent variable", "Range (fixed effect)", "Random effect"),
  colwidths = c(1, 1, 2, 1)
)
ftS6 <- merge_v(ftS6, j = c("comparison", "dependent_variable"), part = "header")
ftS6 <- merge_v(ftS6, j = "comparison", part = "body")
ftS6 <- valign(ftS6, j = "comparison", valign = "top", part = "body")
ftS6 <- align(ftS6, j = c("chi", "p", "sd"), align = "center", part = "all")
ftS6 <- border_remove(ftS6)
ftS6 <- border_outer(ftS6, border = fp_border(color = "black", width = 0.75), part = "all")
ftS6 <- border_inner_h(ftS6, border = fp_border(color = "black", width = 0.75), part = "all")
ftS6 <- border_inner_v(ftS6, border = fp_border(color = "black", width = 0.75), part = "all")
ftS6 <- font(ftS6, fontname = "Times New Roman", part = "all")
ftS6 <- fontsize(ftS6, size = 10, part = "all")
ftS6 <- bold(ftS6, part = "header")
ftS6 <- bold(ftS6, i = sig_rows_S6, j = c("dependent_variable", "chi", "p"), part = "body")
ftS6 <- padding(ftS6, padding.top = 1, padding.bottom = 1, padding.left = 2, padding.right = 2, part = "all")
ftS6 <- fix_border_issues(ftS6)
ftS6 <- italic(ftS6, i = 2, j = c("p", "sd"), part = "header")
ftS6 <- width(ftS6, j = "comparison", width = 1.5)
ftS6 <- width(ftS6, j = "dependent_variable", width = 1.7)
ftS6 <- width(ftS6, j = c("chi", "p"), width = 1.0)
ftS6 <- width(ftS6, j = "sd", width = 1.1)

caption_S6 <- fpar(
  ftext("Table S6 ", TNR10_B),
  ftext(
    paste0(
      "Linear mixed-model outputs comparing dependent variables between native and introduced ranges, ",
      "between Native_main and Native_Tampa, and between Intro_low and Intro_high. ",
      "For each trait, a single model was fitted as trait ~ Group + (1|Collection_site), ",
      "and planned contrasts were extracted from estimated marginal means using asymptotic Wald chi-square tests (1 df). ",
      "The native–introduced contrast compared site-count-weighted means of the native and introduced groups. ",
      "All variables except shoot density and seed set were natural-log transformed prior to model fitting. ",
      "Rows with s.d. = 0.000 indicate singular fits and should be interpreted with caution. ",
      "Significant results ("
    ),
    TNR10
  ),
  ftext("p", TNR10_I),
  ftext(" < 0.05) are shown in bold.", TNR10)
)

doc_S6 <- read_docx()
doc_S6 <- body_add_fpar(doc_S6, caption_S6)
doc_S6 <- body_add_par(doc_S6, "")
doc_S6 <- body_add_flextable(doc_S6, ftS6)
print(doc_S6, target = "Table_S6_LMM.docx")

S2 <- data.frame()

for (i in seq_len(nrow(trait_info))) {
  dat_i <- lmm_data[grepl("Introduced", lmm_data$Group) & !is.na(lmm_data[[trait_info$model_trait[i]]]) & !is.na(lmm_data$Latitude), ]

  m_linear <- lm(as.formula(paste0("`", trait_info$model_trait[i], "` ~ Latitude")), data = dat_i)
  m_quadratic <- lm(as.formula(paste0("`", trait_info$model_trait[i], "` ~ poly(Latitude, 2, raw = TRUE)")), data = dat_i)

  s_linear <- summary(m_linear)
  s_quadratic <- summary(m_quadratic)
  f_linear <- s_linear$fstatistic
  f_quadratic <- s_quadratic$fstatistic

  S2 <- rbind(
    S2,
    data.frame(
      Variable = trait_info$table_label[i],
      Linear_R2 = s_linear$r.squared,
      Linear_adjR2 = s_linear$adj.r.squared,
      Linear_p = pf(f_linear[1], f_linear[2], f_linear[3], lower.tail = FALSE),
      Quadratic_R2 = s_quadratic$r.squared,
      Quadratic_adjR2 = s_quadratic$adj.r.squared,
      Quadratic_p = pf(f_quadratic[1], f_quadratic[2], f_quadratic[3], lower.tail = FALSE),
      stringsAsFactors = FALSE
    )
  )
}

tabS2 <- data.frame(
  Variable = S2$Variable,
  Linear_R2 = ifelse(is.na(S2$Linear_R2), "NA", sprintf("%.3f", S2$Linear_R2)),
  Linear_p = ifelse(is.na(S2$Linear_p), "NA", ifelse(S2$Linear_p < 0.001, "<0.001", sprintf("%.3f", S2$Linear_p))),
  Quadratic_R2 = ifelse(is.na(S2$Quadratic_R2), "NA", sprintf("%.3f", S2$Quadratic_R2)),
  Quadratic_p = ifelse(is.na(S2$Quadratic_p), "NA", ifelse(S2$Quadratic_p < 0.001, "<0.001", sprintf("%.3f", S2$Quadratic_p))),
  stringsAsFactors = FALSE
)

linear_sig <- which(!is.na(S2$Linear_p) & S2$Linear_p < 0.05)
quadratic_sig <- which(!is.na(S2$Quadratic_p) & S2$Quadratic_p < 0.05)

ftS2 <- flextable(tabS2)
ftS2 <- set_header_labels(
  ftS2,
  Variable = "Variable",
  Linear_R2 = "R²",
  Linear_p = "p",
  Quadratic_R2 = "R²",
  Quadratic_p = "p"
)
ftS2 <- add_header_row(
  ftS2, top = TRUE,
  values = c("Variable", "Linear regression", "Quadratic regression"),
  colwidths = c(1, 2, 2)
)
ftS2 <- merge_v(ftS2, j = "Variable", part = "header")
ftS2 <- align(ftS2, j = c("Linear_R2", "Linear_p", "Quadratic_R2", "Quadratic_p"), align = "center", part = "all")
ftS2 <- border_remove(ftS2)
ftS2 <- border_outer(ftS2, border = fp_border(color = "black", width = 0.75), part = "all")
ftS2 <- border_inner_h(ftS2, border = fp_border(color = "black", width = 0.75), part = "all")
ftS2 <- border_inner_v(ftS2, border = fp_border(color = "black", width = 0.75), part = "all")
ftS2 <- font(ftS2, fontname = "Times New Roman", part = "all")
ftS2 <- fontsize(ftS2, size = 10, part = "all")
ftS2 <- bold(ftS2, part = "header")
ftS2 <- padding(ftS2, padding.top = 1, padding.bottom = 1, padding.left = 2, padding.right = 2, part = "all")
ftS2 <- fix_border_issues(ftS2)
ftS2 <- italic(ftS2, i = 2, j = c("Linear_R2", "Linear_p", "Quadratic_R2", "Quadratic_p"), part = "header")
ftS2 <- bold(ftS2, i = linear_sig, j = "Linear_p", part = "body")
ftS2 <- bold(ftS2, i = quadratic_sig, j = "Quadratic_p", part = "body")
ftS2 <- width(ftS2, j = "Variable", width = 1.9)
ftS2 <- width(ftS2, j = c("Linear_R2", "Linear_p", "Quadratic_R2", "Quadratic_p"), width = 1.0)

caption_S2 <- fpar(
  ftext("Table S2. ", TNR10_B),
  ftext("R", TNR10_I),
  ftext("²", TNR10_I),
  ftext(" and ", TNR10),
  ftext("p", TNR10_I),
  ftext(" values of linear and quadratic regressions testing latitudinal variation in plant traits and consumption by generalists for ", TNR10),
  ftext("Spartina alterniflora", TNR10_I),
  ftext(". All variables except shoot density and seed set were natural-log transformed prior to regression. ", TNR10),
  ftext("R", TNR10_I),
  ftext("²", TNR10_I),
  ftext(" = goodness of fit. Significant results (", TNR10),
  ftext("p", TNR10_I),
  ftext(" < 0.05) are shown in bold.", TNR10)
)

doc_S2 <- read_docx()
doc_S2 <- body_add_fpar(doc_S2, caption_S2)
doc_S2 <- body_add_par(doc_S2, "")
doc_S2 <- body_add_flextable(doc_S2, ftS2)
print(doc_S2, target = "Table_S2_latitude_regression.docx")

figure_theme <- theme_classic(base_size = 11) %+replace%
  theme(
    text = element_text(family = "sans", colour = "black"),
    axis.line = element_line(colour = "black", linewidth = 0.35),
    axis.ticks = element_line(colour = "black", linewidth = 0.35),
    axis.ticks.length = grid::unit(2, "pt"),
    axis.text = element_text(size = 10, colour = "black"),
    axis.title = element_text(size = 11, colour = "black"),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    plot.margin = margin(4, 4, 4, 4, "pt"),
    legend.position = "none",
    strip.background = element_blank(),
    plot.tag = element_text(face = "bold", size = 13, hjust = 0, vjust = 1),
    plot.tag.position = c(0.02, 0.98)
  )

figure_3_panels <- vector("list", nrow(trait_info))

for (i in seq_len(nrow(trait_info))) {
  trait <- trait_info$trait[i]
  model_trait <- trait_info$model_trait[i]
  dat_i <- lmm_data[!is.na(lmm_data[[trait]]), ]
  dat_intro <- dat_i[grepl("Introduced", dat_i$Group), ]

  p_lookup <- S6$p[S6$trait == trait]
  names(p_lookup) <- S6$contrast[S6$trait == trait]

  y_max <- max(dat_i[[trait]], na.rm = TRUE)
  y_min <- min(dat_i[[trait]], na.rm = TRUE)
  y_span <- y_max - y_min
  y_limits <- c(y_min - y_span * 0.05, y_max + y_span * 0.30)

  brackets <- data.frame(
    name = c("Native_within", "Intro_within", "Native_vs_Intro"),
    xmin = c(1, 3, 1.5),
    xmax = c(2, 4, 3.5),
    y = c(y_max + y_span * 0.06, y_max + y_span * 0.06, y_max + y_span * 0.20),
    stringsAsFactors = FALSE
  )
  brackets$p <- as.numeric(p_lookup[brackets$name])
  brackets <- brackets[!is.na(brackets$p), ]
  brackets$label <- ifelse(brackets$p < 0.001, "***", ifelse(brackets$p < 0.01, "**", ifelse(brackets$p < 0.05, "*", "ns")))

  p_violin <- ggplot(lmm_data, aes(x = Group, y = .data[[trait]])) +
    geom_violin(aes(fill = Group, colour = Group), trim = TRUE, linewidth = 0.35, scale = "width", width = 0.82, alpha = 0.55, na.rm = TRUE) +
    geom_jitter(aes(colour = Group), width = 0.08, size = 0.55, alpha = 0.35, stroke = 0, na.rm = TRUE) +
    geom_boxplot(width = 0.12, fill = "white", outlier.shape = NA, colour = "gray15", linewidth = 0.3, na.rm = TRUE) +
    annotate("segment", x = brackets$xmin, xend = brackets$xmax, y = brackets$y, yend = brackets$y, colour = "gray25", linewidth = 0.3) +
    annotate("segment", x = brackets$xmin, xend = brackets$xmin, y = brackets$y, yend = brackets$y - y_span * 0.018, colour = "gray25", linewidth = 0.3) +
    annotate("segment", x = brackets$xmax, xend = brackets$xmax, y = brackets$y, yend = brackets$y - y_span * 0.018, colour = "gray25", linewidth = 0.3) +
    annotate("text", x = (brackets$xmin + brackets$xmax) / 2, y = brackets$y + y_span * 0.012, label = brackets$label, size = 3.8, colour = "gray25", vjust = 0) +
    scale_fill_manual(values = group_fills, drop = FALSE) +
    scale_colour_manual(values = group_lines, drop = FALSE) +
    scale_x_discrete(labels = group_labels, drop = FALSE) +
    scale_y_continuous(limits = y_limits, expand = c(0, 0)) +
    labs(x = NULL, y = trait_info$label[i], tag = letters[i]) +
    figure_theme +
    theme(aspect.ratio = 1)

  linear_model <- lm(as.formula(sprintf("`%s` ~ Latitude", model_trait)), data = dat_intro)
  quadratic_model <- lm(as.formula(sprintf("`%s` ~ poly(Latitude, 2, raw = TRUE)", model_trait)), data = dat_intro)
  linear_summary <- summary(linear_model)
  quadratic_summary <- summary(quadratic_model)
  linear_f <- linear_summary$fstatistic
  quadratic_f <- quadratic_summary$fstatistic
  linear_p <- pf(linear_f[1], linear_f[2], linear_f[3], lower.tail = FALSE)
  quadratic_p <- pf(quadratic_f[1], quadratic_f[2], quadratic_f[3], lower.tail = FALSE)

  selected_model <- NULL
  if (linear_p < 0.05 && quadratic_p < 0.05) {
    selected_model <- if (quadratic_summary$adj.r.squared > linear_summary$adj.r.squared) quadratic_model else linear_model
  } else if (quadratic_p < 0.05) {
    selected_model <- quadratic_model
  } else if (linear_p < 0.05) {
    selected_model <- linear_model
  }

  p_scatter <- ggplot(dat_intro, aes(x = Latitude, y = .data[[trait]]))

  if (!is.null(selected_model)) {
    latitude_grid <- data.frame(Latitude = seq(min(dat_intro$Latitude, na.rm = TRUE), max(dat_intro$Latitude, na.rm = TRUE), length.out = 100))
    prediction <- as.data.frame(predict(selected_model, newdata = latitude_grid, interval = "confidence"))
    if (model_trait != trait) prediction <- as.data.frame(lapply(prediction, exp))
    fit_line <- cbind(latitude_grid, prediction)
    p_scatter <- p_scatter +
      geom_ribbon(data = fit_line, aes(x = Latitude, y = fit, ymin = lwr, ymax = upr), fill = "#2C2C2C", alpha = 0.13, inherit.aes = FALSE) +
      geom_line(data = fit_line, aes(x = Latitude, y = fit), colour = "#2C2C2C", linewidth = 0.55, inherit.aes = FALSE)
  }

  display_p <- pf(quadratic_f[1], quadratic_f[2], quadratic_f[3], lower.tail = FALSE)
  label_r2 <- sprintf("%.3f", quadratic_summary$r.squared)
  label_p <- ifelse(display_p < 0.001, "< 0.001", paste0("= ", sprintf("%.3f", display_p)))

  p_scatter <- p_scatter +
    geom_point(aes(fill = Group, colour = Group), shape = 21, size = 2.2, alpha = 0.85, stroke = 0.3) +
    annotate("text", x = -Inf, y = Inf, hjust = -0.08, vjust = 1.7, label = paste0("R² = ", label_r2, ", p ", label_p), size = 3.6, colour = "gray15") +
    scale_fill_manual(values = group_fills) +
    scale_colour_manual(values = group_lines) +
    scale_y_continuous(limits = y_limits, expand = c(0, 0)) +
    labs(x = "Latitude (°N)", y = NULL) +
    figure_theme +
    theme(aspect.ratio = 1, plot.tag = element_blank(), axis.text.y = element_blank())

  if (i <= nrow(trait_info) - 3) {
    p_violin <- p_violin + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())
    p_scatter <- p_scatter + labs(x = NULL) + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())
  } else {
    p_violin <- p_violin + theme(axis.text.x = element_text(size = 10, angle = 35, hjust = 1, vjust = 1))
  }

  figure_3_panels[[i]] <- p_violin + p_scatter + plot_layout(widths = c(1, 1))
}

figure_3 <- wrap_plots(figure_3_panels, ncol = 3, nrow = 6) & theme(plot.background = element_rect(fill = "white", colour = NA))
ggsave("Figure_3.pdf", figure_3, width = 12, height = 13.5)

# ============================================================
# 4. Trait screening, Table S3 and Table S4
# ============================================================

growth_traits <- c("Height", "Aboveground_biomass", "Total_biomass", "Root_shoot", "Shoot_density", "Seed_set")
growth_labels <- c("Height", "Aboveground biomass", "Total biomass", "Root: shoot", "Shoot density", "Seed set")
defence_traits <- c("Carbon", "Nitrogen", "C_N", "LMA", "Leaf_toughness", "Alkaloids", "Flavonoids", "Tannins", "Phenolics", "Lignins")
defence_labels <- c("Carbon", "Nitrogen", "C: N", "LMA", "Toughness", "Alkaloids", "Flavonoids", "Tannins", "Phenolics", "Lignins")
defence_screen_traits <- c("Carbon", "C_N", "LMA", "Leaf_toughness", "Alkaloids", "Flavonoids", "Tannins", "Phenolics", "Lignins")
defence_screen_labels <- c("Carbon", "C:N", "LMA", "Toughness", "Alkaloids", "Flavonoids", "Tannins", "Phenolics", "Lignins")

screen_data <- d
for (v in unique(c(growth_traits, defence_traits, "Laelia_consumption", "Locust_consumption"))) screen_data[[v]] <- suppressWarnings(as.numeric(screen_data[[v]]))

M_growth <- as.matrix(screen_data[, growth_traits])
r_growth <- cor(M_growth, method = "spearman", use = "pairwise.complete.obs")
p_growth <- matrix(NA_real_, length(growth_traits), length(growth_traits))
for (i in seq_len(length(growth_traits) - 1)) {
  for (j in (i + 1):length(growth_traits)) {
    p_growth[i, j] <- suppressWarnings(cor.test(M_growth[, i], M_growth[, j], method = "spearman")$p.value)
    p_growth[j, i] <- p_growth[i, j]
  }
}

display_growth <- matrix("", length(growth_traits), length(growth_traits))
for (i in seq_along(growth_traits)) {
  display_growth[i, i] <- "1.000"
  for (j in seq_along(growth_traits)) if (j > i) display_growth[i, j] <- sprintf("%.3f", r_growth[i, j])
}

keys_growth <- paste0("c", seq_along(growth_traits))
tab_growth <- data.frame(Trait = growth_labels, as.data.frame(display_growth), check.names = FALSE)
colnames(tab_growth) <- c("Trait", keys_growth)

ft_growth <- flextable(tab_growth)
ft_growth <- set_header_labels(ft_growth, values = setNames(as.list(c("Trait", growth_labels)), c("Trait", keys_growth)))
ft_growth <- align(ft_growth, j = keys_growth, align = "center", part = "all")
ft_growth <- border_remove(ft_growth)
ft_growth <- border_outer(ft_growth, border = fp_border(color = "black", width = 0.75), part = "all")
ft_growth <- border_inner_h(ft_growth, border = fp_border(color = "black", width = 0.75), part = "all")
ft_growth <- border_inner_v(ft_growth, border = fp_border(color = "black", width = 0.75), part = "all")
ft_growth <- font(ft_growth, fontname = "Times New Roman", part = "all")
ft_growth <- fontsize(ft_growth, size = 8, part = "all")
ft_growth <- bold(ft_growth, part = "header")
ft_growth <- padding(ft_growth, padding.top = 1, padding.bottom = 1, padding.left = 2, padding.right = 2, part = "all")
ft_growth <- fix_border_issues(ft_growth)
ft_growth <- valign(ft_growth, valign = "bottom", part = "header")
ft_growth <- align(ft_growth, j = "Trait", align = "left", part = "all")
ft_growth <- width(ft_growth, j = "Trait", width = 1.25)
ft_growth <- width(ft_growth, j = keys_growth, width = 0.72)
ft_growth <- set_table_properties(ft_growth, layout = "fixed")

for (i in seq_along(growth_traits)) {
  for (j in seq_along(growth_traits)) {
    if (j > i && !is.na(p_growth[i, j]) && p_growth[i, j] < 0.05) ft_growth <- bold(ft_growth, i = i, j = keys_growth[j], part = "body")
  }
}

M_defence <- as.matrix(screen_data[, defence_traits])
r_defence <- cor(M_defence, method = "spearman", use = "pairwise.complete.obs")
p_defence <- matrix(NA_real_, length(defence_traits), length(defence_traits))
for (i in seq_len(length(defence_traits) - 1)) {
  for (j in (i + 1):length(defence_traits)) {
    p_defence[i, j] <- suppressWarnings(cor.test(M_defence[, i], M_defence[, j], method = "spearman")$p.value)
    p_defence[j, i] <- p_defence[i, j]
  }
}

display_defence <- matrix("", length(defence_traits), length(defence_traits))
for (i in seq_along(defence_traits)) {
  display_defence[i, i] <- "1.000"
  for (j in seq_along(defence_traits)) if (j > i) display_defence[i, j] <- sprintf("%.3f", r_defence[i, j])
}

keys_defence <- paste0("c", seq_along(defence_traits))
tab_defence <- data.frame(Trait = defence_labels, as.data.frame(display_defence), check.names = FALSE)
colnames(tab_defence) <- c("Trait", keys_defence)

ft_defence <- flextable(tab_defence)
ft_defence <- set_header_labels(ft_defence, values = setNames(as.list(c("Trait", defence_labels)), c("Trait", keys_defence)))
ft_defence <- align(ft_defence, j = keys_defence, align = "center", part = "all")
ft_defence <- border_remove(ft_defence)
ft_defence <- border_outer(ft_defence, border = fp_border(color = "black", width = 0.75), part = "all")
ft_defence <- border_inner_h(ft_defence, border = fp_border(color = "black", width = 0.75), part = "all")
ft_defence <- border_inner_v(ft_defence, border = fp_border(color = "black", width = 0.75), part = "all")
ft_defence <- font(ft_defence, fontname = "Times New Roman", part = "all")
ft_defence <- fontsize(ft_defence, size = 8, part = "all")
ft_defence <- bold(ft_defence, part = "header")
ft_defence <- padding(ft_defence, padding.top = 1, padding.bottom = 1, padding.left = 2, padding.right = 2, part = "all")
ft_defence <- fix_border_issues(ft_defence)
ft_defence <- valign(ft_defence, valign = "bottom", part = "header")
ft_defence <- align(ft_defence, j = "Trait", align = "left", part = "all")
ft_defence <- width(ft_defence, j = "Trait", width = 1.25)
ft_defence <- width(ft_defence, j = keys_defence, width = 0.55)
ft_defence <- set_table_properties(ft_defence, layout = "fixed")

for (i in seq_along(defence_traits)) {
  for (j in seq_along(defence_traits)) {
    if (j > i && !is.na(p_defence[i, j]) && p_defence[i, j] < 0.05) ft_defence <- bold(ft_defence, i = i, j = keys_defence[j], part = "body")
  }
}

caption_S3 <- fpar(
  ftext("Table S3. ", TNR10_B),
  ftext("Spearman's rank correlation coefficient (", TNR10),
  ftext("r", TNR10_I),
  ftext(") among plant traits. Significant results (", TNR10),
  ftext("p", TNR10_I),
  ftext(" < 0.05) are shown in bold.", TNR10)
)

section_portrait <- prop_section(
  page_size = page_size(orient = "portrait", width = 8.5, height = 11),
  page_margins = page_mar(top = 0.6, bottom = 0.6, left = 0.6, right = 0.6)
)

doc_S3 <- read_docx()
doc_S3 <- body_add_fpar(doc_S3, caption_S3)
doc_S3 <- body_add_par(doc_S3, "")
doc_S3 <- body_add_flextable(doc_S3, ft_growth)
doc_S3 <- body_add_par(doc_S3, "")
doc_S3 <- body_add_flextable(doc_S3, ft_defence)
doc_S3 <- body_set_default_section(doc_S3, section_portrait)
print(doc_S3, target = "Table_S3_correlations.docx")

S4 <- data.frame()

for (i in seq_along(defence_screen_traits)) {
  x <- defence_screen_traits[i]

  dat_laelia <- screen_data[!is.na(screen_data$Laelia_consumption) & !is.na(screen_data[[x]]), ]
  dat_locusta <- screen_data[!is.na(screen_data$Locust_consumption) & !is.na(screen_data[[x]]), ]

  m_laelia <- lm(as.formula(paste0("log(Laelia_consumption) ~ `", x, "`")), data = dat_laelia)
  m_locusta <- lm(as.formula(paste0("log(Locust_consumption) ~ `", x, "`")), data = dat_locusta)

  s_laelia <- summary(m_laelia)
  s_locusta <- summary(m_locusta)

  S4 <- rbind(
    S4,
    data.frame(
      Variable = defence_screen_labels[i],
      Laelia_beta = coef(s_laelia)[2, 1],
      Laelia_R2 = s_laelia$r.squared,
      Laelia_p = coef(s_laelia)[2, 4],
      Locusta_beta = coef(s_locusta)[2, 1],
      Locusta_R2 = s_locusta$r.squared,
      Locusta_p = coef(s_locusta)[2, 4],
      stringsAsFactors = FALSE
    )
  )
}

tabS4 <- data.frame(
  Variable = S4$Variable,
  Laelia_beta = sprintf("%.3f", S4$Laelia_beta),
  Laelia_R2 = sprintf("%.3f", S4$Laelia_R2),
  Laelia_p = ifelse(S4$Laelia_p < 0.001, "<0.001", sprintf("%.3f", S4$Laelia_p)),
  Locusta_beta = sprintf("%.3f", S4$Locusta_beta),
  Locusta_R2 = sprintf("%.3f", S4$Locusta_R2),
  Locusta_p = ifelse(S4$Locusta_p < 0.001, "<0.001", sprintf("%.3f", S4$Locusta_p)),
  stringsAsFactors = FALSE
)

laelia_sig <- which(!is.na(S4$Laelia_p) & S4$Laelia_p < 0.05)
locusta_sig <- which(!is.na(S4$Locusta_p) & S4$Locusta_p < 0.05)

ftS4 <- flextable(tabS4)
ftS4 <- set_header_labels(
  ftS4,
  Variable = "Independent variable",
  Laelia_beta = "β", Laelia_R2 = "R²", Laelia_p = "p",
  Locusta_beta = "β", Locusta_R2 = "R²", Locusta_p = "p"
)
ftS4 <- add_header_row(
  ftS4, top = TRUE,
  values = c("Independent variable", "Laelia coenosa consumption", "Locusta migratoria consumption"),
  colwidths = c(1, 3, 3)
)
ftS4 <- merge_v(ftS4, j = "Variable", part = "header")
ftS4 <- align(ftS4, j = c("Laelia_beta", "Laelia_R2", "Laelia_p", "Locusta_beta", "Locusta_R2", "Locusta_p"), align = "center", part = "all")
ftS4 <- border_remove(ftS4)
ftS4 <- border_outer(ftS4, border = fp_border(color = "black", width = 0.75), part = "all")
ftS4 <- border_inner_h(ftS4, border = fp_border(color = "black", width = 0.75), part = "all")
ftS4 <- border_inner_v(ftS4, border = fp_border(color = "black", width = 0.75), part = "all")
ftS4 <- font(ftS4, fontname = "Times New Roman", part = "all")
ftS4 <- fontsize(ftS4, size = 10, part = "all")
ftS4 <- bold(ftS4, part = "header")
ftS4 <- padding(ftS4, padding.top = 1, padding.bottom = 1, padding.left = 2, padding.right = 2, part = "all")
ftS4 <- fix_border_issues(ftS4)
ftS4 <- bold(ftS4, i = laelia_sig, j = "Laelia_p", part = "body")
ftS4 <- bold(ftS4, i = locusta_sig, j = "Locusta_p", part = "body")
ftS4 <- width(ftS4, j = "Variable", width = 1.5)
ftS4 <- width(ftS4, j = c("Laelia_beta", "Laelia_R2", "Laelia_p", "Locusta_beta", "Locusta_R2", "Locusta_p"), width = 0.75)
ftS4 <- set_table_properties(ftS4, layout = "fixed")

caption_S4 <- fpar(
  ftext("Table S4 ", TNR10_B),
  ftext("Linear regressions testing effects of plant nutritional and putative defensive traits on leaf consumption by ", TNR10),
  ftext("Laelia coenosa", TNR10_I),
  ftext(" and ", TNR10),
  ftext("Locusta migratoria", TNR10_I),
  ftext(". Consumption was natural-log transformed; independent variables were retained on their original scales. ", TNR10),
  ftext("β", TNR10_I),
  ftext(" = slope estimator; ", TNR10),
  ftext("R", TNR10_I),
  ftext("²", TNR10_I),
  ftext(" = goodness of fit. Significant results (", TNR10),
  ftext("p", TNR10_I),
  ftext(" < 0.05) are shown in bold.", TNR10)
)

doc_S4 <- read_docx()
doc_S4 <- body_add_fpar(doc_S4, caption_S4)
doc_S4 <- body_add_par(doc_S4, "")
doc_S4 <- body_add_flextable(doc_S4, ftS4)
print(doc_S4, target = "Table_S4_defence_screening.docx")

# ============================================================
# 5. Syndrome analysis, Figure 4, Figure S3 and Table S7
# ============================================================

syndrome_traits <- c("Height", "Root_shoot", "Total_biomass", "Shoot_density", "Seed_set", "LMA", "Carbon", "C_N", "Flavonoids")
syndrome_labels <- c("Height", "Root:shoot", "Total biomass", "Shoot density", "Seed set", "LMA", "Carbon", "C:N", "Flavonoids")
cluster_colours <- c("#1F77B4", "#2CA02C", "#D62728", "#FF7F0E")
range_colours <- c(Native_main = "#7FB069", Native_Tampa = "#E8A66E", Introduced_low = "#7FA9C8", Introduced_high = "#D88D9A")
range_labels <- c("Nat_main", "Nat_Tampa", "Intro_low", "Intro_high")

syndrome_data <- d[order(d$Collection_site, d$Family), ]
for (v in syndrome_traits) syndrome_data[[v]] <- suppressWarnings(as.numeric(syndrome_data[[v]]))
syndrome_data$Collection_site[syndrome_data$Collection_site %in% c("SA", "SB")] <- "SA"

trait9 <- syndrome_data[, syndrome_traits]
meta <- syndrome_data[, c("Group", "Range", "Collection_site", "Family", "Latitude", "Longitude")]
ok <- complete.cases(trait9)
trait9 <- trait9[ok, ]
meta <- meta[ok, ]
trait9_z <- as.data.frame(scale(trait9))
rownames(trait9_z) <- meta$Family

hc <- hclust(dist(trait9_z, method = "euclidean"), method = "ward.D2")

figure_S3 <- fviz_nbclust(trait9_z, kmeans, method = "wss") + geom_vline(xintercept = 4, linetype = 2)
ggsave("Figure_S3_elbow.pdf", figure_S3, width = 6, height = 4)

cluster_raw <- cutree(hc, k = 4)
cluster_final <- unname(c("1" = 2, "2" = 4, "3" = 1, "4" = 3)[as.character(cluster_raw)])
range4 <- factor(meta$Group, levels = c("Native_main", "Native_Tampa", "Introduced_low", "Introduced_high"))

dat_syndrome <- cbind(trait9_z, meta, range4 = range4, cluster = factor(cluster_final, levels = 1:4))

dend <- as.dendrogram(hc)
dend_order <- order.dendrogram(dend)
branch_order <- unique(cluster_final[dend_order])
dend <- color_branches(dend, k = 4, col = cluster_colours[branch_order])
dend <- set(dend, "branches_lwd", 2.2)
dend <- set(dend, "labels_cex", 1.70)
range_bar_colours <- range_colours[as.character(range4)]

draw_dendrogram <- function() {
  par(mar = c(11, 4, 2, 2), xpd = NA, cex.axis = 2.00, cex.lab = 2.00, family = "sans")
  plot(dend, main = "", ylab = "Height", leaflab = "perpendicular")
  colored_bars(colors = range_bar_colours[dend_order], dend = dend, rowLabels = "Range", sort_by_labels_order = FALSE, cex.rowLabels = 2.00, y_shift = -3.2)
  legend("topright", legend = paste("Syndrome", 1:4), fill = cluster_colours, bty = "n", cex = 2.00, border = NA)
  legend("topleft", legend = range_labels, fill = range_colours, bty = "n", cex = 2.00, border = NA)
}

lda_fit <- lda(cluster ~ ., data = dat_syndrome[, c(syndrome_traits, "cluster")])
lda_trace <- round(100 * lda_fit$svd^2 / sum(lda_fit$svd^2), 2)
rownames(lda_fit$scaling) <- syndrome_labels

g_lda <- ggord(
  ord_in = lda_fit, grp_in = dat_syndrome$cluster,
  repel = TRUE, force = 10, txt = 6.5, labcol = "black",
  vec_ext = 6, veclsz = 0.4, parse = FALSE, poly = FALSE,
  ellipse = TRUE, alpha_el = 0.2, size = 1.8, alpha = 0.8, family = "sans"
) +
  scale_color_manual(values = cluster_colours, labels = paste("Syndrome", 1:4)) +
  scale_fill_manual(values = cluster_colours, guide = "none") +
  guides(fill = "none", shape = "none") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw() +
  theme(
    panel.grid = element_blank(), text = element_text(family = "sans"),
    panel.border = element_rect(linewidth = 1.1, fill = NA),
    axis.text = element_text(size = 23, colour = "black"),
    axis.title = element_text(size = 26), legend.text = element_text(size = 20),
    legend.position = "right", legend.title = element_blank(), aspect.ratio = 1
  ) +
  xlab(sprintf("LD1 (%.1f%%)", lda_trace[1])) +
  ylab(sprintf("LD2 (%.1f%%)", lda_trace[2]))

radar_means <- aggregate(dat_syndrome[, syndrome_traits], by = list(cluster = dat_syndrome$cluster), FUN = mean)
radar_max <- apply(radar_means[, syndrome_traits], 2, max)
radar_min <- apply(radar_means[, syndrome_traits], 2, min)

draw_radar <- function(i) {
  radar_data <- as.data.frame(rbind(radar_max, radar_min, as.numeric(radar_means[i, syndrome_traits])))
  colnames(radar_data) <- syndrome_labels
  old_par <- par(mar = c(1, 1, 2.6, 1), bg = NA, family = "sans", cex.main = 2.50)
  radarchart(
    radar_data, axistype = 0, seg = 4,
    pcol = cluster_colours[i], pfcol = adjustcolor(cluster_colours[i], alpha.f = 0.85),
    plwd = 2, cglcol = "grey60", cglty = 1, cglwd = 0.8, vlcex = 1.80,
    title = sprintf("Syndrome %d", i)
  )
  on.exit(par(old_par), add = TRUE)
}

dominant_syndrome <- dat_syndrome |>
  count(Collection_site, cluster, name = "n") |>
  group_by(Collection_site) |>
  slice_max(n, n = 1, with_ties = FALSE) |>
  ungroup() |>
  rename(dominant = cluster)

site_coordinates <- aggregate(cbind(Longitude = as.numeric(Longitude), Latitude = as.numeric(Latitude)) ~ Collection_site, data = meta, FUN = mean)
map_syndrome <- merge(dominant_syndrome, site_coordinates, by = "Collection_site")
syndrome_points <- st_as_sf(map_syndrome, coords = c("Longitude", "Latitude"), crs = 4326)

g_map <- ggplot() +
  geom_sf(data = world, fill = "grey94", colour = "grey70", linewidth = 0.2) +
  geom_sf(data = syndrome_points, aes(fill = dominant), shape = 21, colour = "white", size = 9, stroke = 0.6) +
  scale_fill_manual(values = cluster_colours, name = "Syndrome", labels = paste("Syndrome", 1:4)) +
  coord_sf(crs = 3857, default_crs = 4326, xlim = c(-100, 135), ylim = c(8, 55), expand = FALSE) +
  theme_bw() +
  theme(
    panel.grid = element_line(colour = "grey90", linewidth = 0.2),
    text = element_text(family = "sans", size = 26),
    axis.text = element_text(size = 23, colour = "black"),
    axis.title = element_blank(), legend.position = "right"
  ) +
  annotation_north_arrow(
    location = "tl", which_north = "grid",
    height = grid::unit(3.0, "cm"), width = grid::unit(2.4, "cm"),
    pad_x = grid::unit(0.2, "cm"), pad_y = grid::unit(0.5, "cm"),
    style = north_arrow_fancy_orienteering(text_size = 18, line_width = 1.5, fill = c("white", "grey25"))
  ) +
  annotation_scale(
    location = "br", width_hint = 0.11, text_cex = 1.85,
    height = grid::unit(0.50, "cm"),
    pad_x = grid::unit(0.3, "cm"), pad_y = grid::unit(0.3, "cm")
  )

radar_centres <- data.frame(lon = c(-54, -6, 41, 89), lat = c(35, 35, 35, 35))
radar_coordinates <- st_coordinates(st_transform(st_as_sf(radar_centres, coords = c("lon", "lat"), crs = 4326), 3857))
radar_grobs <- lapply(1:4, function(i) as.grob(function() draw_radar(i)))

g_map_combined <- g_map + theme(legend.position = "none", plot.margin = margin(5.5, 5.5, 5.5, 33, unit = "pt"))
for (i in 1:4) {
  g_map_combined <- g_map_combined +
    annotation_custom(
      radar_grobs[[i]],
      xmin = radar_coordinates[i, 1] - 2.3e6,
      xmax = radar_coordinates[i, 1] + 2.3e6,
      ymin = radar_coordinates[i, 2] - 2.3e6,
      ymax = radar_coordinates[i, 2] + 2.3e6
    )
}

figure_4 <- ggdraw() +
  draw_plot(as.ggplot(draw_dendrogram), x = 0.010, y = 0.474, width = 0.738, height = 0.519) +
  draw_plot(g_lda, x = 0.757, y = 0.524, width = 0.233, height = 0.436) +
  draw_plot(g_map_combined, x = 0.010, y = 0.012, width = 0.980, height = 0.453) +
  draw_plot_label(c("(a)", "(b)", "(c)"), x = c(0.748, 0.990, 0.990), y = c(0.992, 0.958, 0.465), hjust = 1, vjust = 1, fontface = "bold", size = 22)

ggsave("Figure_4_combined.pdf", figure_4, width = 35.9, height = 35.9 / 1.87)

n_traits <- length(syndrome_traits)
ld1 <- lda_fit$scaling[, 1]
emm_matrix <- matrix("", 4, n_traits, dimnames = list(paste0("Cluster ", 1:4), syndrome_traits))
F_values <- numeric(n_traits)
F_pvalues <- numeric(n_traits)
names(F_values) <- names(F_pvalues) <- syndrome_traits

first_model <- lm(dat_syndrome[[syndrome_traits[1]]] ~ cluster, data = dat_syndrome)
first_pair <- as.data.frame(testInteractions(first_model, pairwise = "cluster"))
first_pair <- first_pair[!grepl("Residual", rownames(first_pair)), , drop = FALSE]
pair_labels <- rownames(first_pair)
pair_F <- matrix(NA_real_, nrow(first_pair), n_traits, dimnames = list(pair_labels, syndrome_traits))
pair_p <- pair_F

for (v in syndrome_traits) {
  m <- lm(dat_syndrome[[v]] ~ cluster, data = dat_syndrome)
  em <- as.data.frame(emmeans(m, ~ cluster))
  emm_matrix[, v] <- paste0(sprintf("%.3f", em$emmean), " ± ", sprintf("%.3f", em$SE))

  a <- Anova(m, type = 2)
  F_values[v] <- a$`F value`[1]
  F_pvalues[v] <- a$`Pr(>F)`[1]

  pp <- as.data.frame(testInteractions(m, pairwise = "cluster"))
  pp <- pp[!grepl("Residual", rownames(pp)), , drop = FALSE]
  pair_F[, v] <- pp$F
  pair_p[, v] <- pp$`Pr(>F)`
}

F_stars <- ifelse(F_pvalues < 0.001, "***", ifelse(F_pvalues < 0.01, "**", ifelse(F_pvalues < 0.05, "*", "")))
F_row <- paste0(sprintf("%.3f", F_values), F_stars)
pair_display_labels <- gsub("([0-9]+)", "Cluster \\1", pair_labels)
pair_F_text <- matrix(sprintf("%.3f", pair_F), nrow(pair_F), ncol(pair_F), dimnames = dimnames(pair_F))

row_labels <- c(
  "(a) LD1 scaling, estimated marginal means ± SE, and F statistics",
  "LD1 scaling", rownames(emm_matrix), "F values",
  "(b) F statistics from pairwise comparisons between clusters",
  pair_display_labels
)

table_matrix <- matrix("", length(row_labels), n_traits, dimnames = list(NULL, syndrome_traits))
table_matrix[2, ] <- sprintf("%.3f", ld1)
table_matrix[3:6, ] <- emm_matrix
table_matrix[7, ] <- F_row
table_matrix[9:nrow(table_matrix), ] <- pair_F_text

table_S7 <- data.frame(Trait = row_labels, table_matrix, check.names = FALSE, stringsAsFactors = FALSE)
colnames(table_S7) <- c("Trait", syndrome_labels)

ftS7 <- flextable(table_S7) |>
  merge_h_range(i = c(1, 8), j1 = 1, j2 = ncol(table_S7)) |>
  italic(i = c(1, 8), part = "body") |>
  align(i = c(1, 8), align = "left", part = "body") |>
  align(j = 2:ncol(table_S7), align = "center", part = "all") |>
  align(align = "center", part = "header") |>
  bold(part = "header") |>
  font(fontname = "Times New Roman", part = "all") |>
  fontsize(size = 8, part = "all") |>
  padding(padding = 2, part = "all") |>
  autofit()

for (i in seq_len(nrow(pair_F))) {
  sig_cols <- which(pair_p[i, ] < 0.05)
  if (length(sig_cols) > 0) ftS7 <- bold(ftS7, i = 8 + i, j = 1 + sig_cols, part = "body")
}

caption_S7 <- fpar(
  ftext("Table S7 ", TNR9_B),
  ftext("Contributions of individual traits to clusters and differences in traits between clusters across ", TNR9),
  ftext("Spartina alterniflora", TNR9_I),
  ftext(
    paste0(
      " families. (a) Values show the estimated marginal mean ± SE of each standardised trait for each cluster, ",
      "the coefficient of each trait on LD1, and F statistics from type II ANOVA. ",
      "(b) Values show F statistics from pairwise comparisons between clusters; significant results ("
    ),
    TNR9
  ),
  ftext("p", TNR9_I),
  ftext(" < 0.05) are shown in bold.", TNR9)
)

doc_S7 <- read_docx()
doc_S7 <- body_add_fpar(doc_S7, caption_S7)
doc_S7 <- body_add_par(doc_S7, "")
doc_S7 <- body_add_flextable(doc_S7, ftS7)
doc_S7 <- body_end_section_landscape(doc_S7)
print(doc_S7, target = "Table_S7.docx")

# ============================================================
# 6. Phenotypic selection analysis and Table 1
# ============================================================

selection_traits <- c("Height", "Root_shoot", "Shoot_density", "Carbon", "C_N", "LMA", "Flavonoids")
selection_labels <- c(Height = "Height", Root_shoot = "Root:shoot", Shoot_density = "Shoot density", Carbon = "Carbon", C_N = "C:N", LMA = "LMA", Flavonoids = "Flavonoids")
selection_data <- d
for (v in c(selection_traits, "Total_biomass", "Seed_set")) selection_data[[v]] <- suppressWarnings(as.numeric(selection_data[[v]]))
selection_data$Seed_set[grepl("Native", selection_data$Group) & is.na(selection_data$Seed_set)] <- 0

selection_results <- list()
fitness_columns <- c("Total_biomass", "Seed_set")

for (fitness in fitness_columns) {
  sub <- selection_data[complete.cases(selection_data[, c(fitness, selection_traits)]), ]
  relative_fitness <- sub[[fitness]] / mean(sub[[fitness]])
  Z <- as.data.frame(scale(sub[, selection_traits]))
  names(Z) <- selection_traits

  differential <- data.frame(Trait = selection_traits, adjR2 = NA_real_, S = NA_real_, S_SE = NA_real_, S_p = NA_real_)

  for (i in seq_along(selection_traits)) {
    m <- summary(lm(relative_fitness ~ Z[[selection_traits[i]]]))
    differential$adjR2[i] <- max(m$adj.r.squared, 0)
    differential$S[i] <- coef(m)[2, 1]
    differential$S_SE[i] <- coef(m)[2, 2]
    differential$S_p[i] <- coef(m)[2, 4]
  }

  differential$S_p <- p.adjust(differential$S_p, method = "holm")

  gradient_model <- summary(lm(relative_fitness ~ ., data = cbind(relative_fitness = relative_fitness, Z)))
  gradient <- data.frame(
    beta = coef(gradient_model)[selection_traits, 1],
    beta_SE = coef(gradient_model)[selection_traits, 2],
    beta_p = p.adjust(coef(gradient_model)[selection_traits, 4], method = "holm")
  )

  selection_results[[fitness]] <- list(
    n = nrow(sub),
    gradient_adjR2 = gradient_model$adj.r.squared,
    table = data.frame(
      Trait = unname(selection_labels[selection_traits]),
      adjR2 = differential$adjR2,
      S = differential$S,
      S_SE = differential$S_SE,
      S_p = differential$S_p,
      beta = gradient$beta,
      beta_SE = gradient$beta_SE,
      beta_p = gradient$beta_p,
      stringsAsFactors = FALSE
    )
  )
}

bio <- selection_results$Total_biomass
seed <- selection_results$Seed_set

bio_stars_S <- ifelse(bio$table$S_p < 0.001, "***", ifelse(bio$table$S_p < 0.01, "**", ifelse(bio$table$S_p < 0.05, "*", "ns")))
bio_stars_B <- ifelse(bio$table$beta_p < 0.001, "***", ifelse(bio$table$beta_p < 0.01, "**", ifelse(bio$table$beta_p < 0.05, "*", "ns")))
seed_stars_S <- ifelse(seed$table$S_p < 0.001, "***", ifelse(seed$table$S_p < 0.01, "**", ifelse(seed$table$S_p < 0.05, "*", "ns")))
seed_stars_B <- ifelse(seed$table$beta_p < 0.001, "***", ifelse(seed$table$beta_p < 0.01, "**", ifelse(seed$table$beta_p < 0.05, "*", "ns")))

bio_display <- data.frame(
  Trait = bio$table$Trait,
  Differential_R2 = sprintf("%.3f", bio$table$adjR2),
  S = sprintf("%.3f (%.3f)", bio$table$S, bio$table$S_SE),
  S_p = paste0(ifelse(bio$table$S_p < 0.001, "<0.001", sprintf("%.3f", bio$table$S_p)), bio_stars_S),
  Gradient_R2 = sprintf("%.3f", bio$gradient_adjR2),
  Beta = sprintf("%.3f (%.3f)", bio$table$beta, bio$table$beta_SE),
  Beta_p = paste0(ifelse(bio$table$beta_p < 0.001, "<0.001", sprintf("%.3f", bio$table$beta_p)), bio_stars_B),
  stringsAsFactors = FALSE
)

seed_display <- data.frame(
  Trait = seed$table$Trait,
  Differential_R2 = sprintf("%.3f", seed$table$adjR2),
  S = sprintf("%.3f (%.3f)", seed$table$S, seed$table$S_SE),
  S_p = paste0(ifelse(seed$table$S_p < 0.001, "<0.001", sprintf("%.3f", seed$table$S_p)), seed_stars_S),
  Gradient_R2 = sprintf("%.3f", seed$gradient_adjR2),
  Beta = sprintf("%.3f (%.3f)", seed$table$beta, seed$table$beta_SE),
  Beta_p = paste0(ifelse(seed$table$beta_p < 0.001, "<0.001", sprintf("%.3f", seed$table$beta_p)), seed_stars_B),
  stringsAsFactors = FALSE
)

blank_total <- data.frame(Trait = "Total biomass", Differential_R2 = "", S = "", S_p = "", Gradient_R2 = "", Beta = "", Beta_p = "")
blank_seed <- data.frame(Trait = "Seed set", Differential_R2 = "", S = "", S_p = "", Gradient_R2 = "", Beta = "", Beta_p = "")
selection_table <- rbind(blank_total, bio_display, blank_seed, seed_display)

ft1 <- flextable(selection_table) |>
  set_header_labels(Trait = "Trait", Differential_R2 = "R²", S = "S (SE)", S_p = "p", Gradient_R2 = "R²", Beta = "β (SE)", Beta_p = "p") |>
  add_header_row(values = c("Trait", "Selection differentials", "Selection gradients"), colwidths = c(1, 3, 3), top = TRUE) |>
  merge_v(j = "Trait", part = "header") |>
  merge_at(i = 1, j = 1:7, part = "body") |>
  merge_at(i = 9, j = 1:7, part = "body") |>
  merge_at(i = 2:8, j = 5, part = "body") |>
  merge_at(i = 10:16, j = 5, part = "body") |>
  bold(i = c(1, 9), part = "body") |>
  align(align = "center", part = "header") |>
  align(j = 1, align = "left", part = "all") |>
  align(j = 2:7, align = "center", part = "body") |>
  font(fontname = "Times New Roman", part = "all") |>
  fontsize(size = 10, part = "all") |>
  border_remove() |>
  border_outer(part = "all", border = fp_border(color = "black", width = 1.2)) |>
  border_inner_h(part = "all", border = fp_border(color = "black", width = 0.5)) |>
  border_inner_v(part = "all", border = fp_border(color = "black", width = 0.5)) |>
  width(j = 1, width = 1.25) |>
  width(j = c(2, 5), width = 0.55) |>
  width(j = c(3, 6), width = 1.05) |>
  width(j = c(4, 7), width = 0.95) |>
  padding(padding.top = 1, padding.bottom = 1, part = "all")

ft1 <- bold(ft1, i = c(which(bio$table$S_p < 0.05) + 1, which(seed$table$S_p < 0.05) + 9), j = 4, part = "body")
ft1 <- bold(ft1, i = c(which(bio$table$beta_p < 0.05) + 1, which(seed$table$beta_p < 0.05) + 9), j = 7, part = "body")

caption_1 <- fpar(
  ftext("Table 1 Phenotypic selection analysis for traits of ", TNR10_B),
  ftext("Spartina alterniflora", TNR10_BI),
  ftext(" based on two measures of fitness: total biomass and seed set. ", TNR10_B),
  ftext("For each model, adjusted ", TNR10),
  ftext("R", TNR10_I),
  ftext("²", TNR10_I),
  ftext(", regression (", TNR10),
  ftext("S", TNR10_I),
  ftext(") or partial regression (", TNR10),
  ftext("β", TNR10_I),
  ftext(") coefficients for relative fitness on traits, standard error of the coefficient (SE), Holm-corrected ", TNR10),
  ftext("p", TNR10_I),
  ftext(", and significance levels are shown; significant ", TNR10),
  ftext("p", TNR10_I),
  ftext(" values (", TNR10),
  ftext("p", TNR10_I),
  ftext(" < 0.05) are in bold.", TNR10)
)

note_1 <- fpar(
  ftext(
    sprintf(
      paste0(
        "Note. Analyses pooled all families (n = %d for total biomass; n = %d for seed set). ",
        "Non-seeding native plants were scored as 0 for seed set. ",
        "Traits were standardised to a mean of 0 and a standard deviation of 1. ",
        "P values were Holm-corrected within each set of seven tests."
      ),
      bio$n, seed$n
    ),
    TNR10_I
  )
)

doc_1 <- read_docx()
doc_1 <- body_add_fpar(doc_1, caption_1)
doc_1 <- body_add_flextable(doc_1, ft1)
doc_1 <- body_add_fpar(doc_1, note_1)
print(doc_1, target = "Table_1_selection_analysis.docx")

# ============================================================
# 7. Climate analysis, Table 2, Table S5 and Figure S4
# ============================================================

climate_traits <- c("Height", "Root_shoot", "Total_biomass", "Shoot_density", "Seed_set", "Carbon", "C_N", "LMA", "Flavonoids")
climate_labels <- c(Height = "Height", Root_shoot = "Root:shoot", Total_biomass = "Total biomass", Shoot_density = "Shoot density", Seed_set = "Seed set", Carbon = "Carbon", C_N = "C:N", LMA = "LMA", Flavonoids = "Flavonoids")

climate_data <- d
for (v in c(climate_traits, "Temperature", "Precipitation")) climate_data[[v]] <- suppressWarnings(as.numeric(climate_data[[v]]))
climate_data$Range <- as.numeric(grepl("Native", climate_data$Range))
for (v in climate_traits[!climate_traits %in% c("Shoot_density", "Seed_set")]) climate_data[[v]] <- log(climate_data[[v]])

candidate_terms <- c(
  "Range", "Temperature", "I(Temperature^2)", "Precipitation", "I(Precipitation^2)",
  "Range:Temperature", "Range:I(Temperature^2)", "Range:Precipitation", "Range:I(Precipitation^2)"
)

candidate_sets <- list()
for (k in seq_along(candidate_terms)) {
  sets_k <- combn(candidate_terms, k, simplify = FALSE)
  for (s in sets_k) {
    valid <-
      (!("I(Temperature^2)" %in% s) || "Temperature" %in% s) &&
      (!("I(Precipitation^2)" %in% s) || "Precipitation" %in% s) &&
      (!("Range:Temperature" %in% s) || all(c("Range", "Temperature") %in% s)) &&
      (!("Range:I(Temperature^2)" %in% s) || all(c("Range", "I(Temperature^2)") %in% s)) &&
      (!("Range:Precipitation" %in% s) || all(c("Range", "Precipitation") %in% s)) &&
      (!("Range:I(Precipitation^2)" %in% s) || all(c("Range", "I(Precipitation^2)") %in% s))
    if (valid) candidate_sets[[length(candidate_sets) + 1]] <- s
  }
}

term_labels <- c(
  Range = "R",
  Temperature = "T",
  "I(Temperature^2)" = "T²",
  Precipitation = "P",
  "I(Precipitation^2)" = "P²",
  "Range:Temperature" = "R×T",
  "Range:I(Temperature^2)" = "R×T²",
  "Range:Precipitation" = "R×P",
  "Range:I(Precipitation^2)" = "R×P²"
)

climate_results <- list()

for (trait in climate_traits) {
  sub <- climate_data[!is.na(climate_data[[trait]]), ]
  model_table <- data.frame()

  for (term_set in candidate_sets) {
    m <- lm(as.formula(paste(trait, "~", paste(term_set, collapse = " + "))), data = sub)
    sm <- summary(m)
    f <- sm$fstatistic

    model_table <- rbind(
      model_table,
      data.frame(
        key = paste(term_set, collapse = "|"),
        Model = paste(term_labels[term_set], collapse = " + "),
        AIC = AIC(m),
        R2 = sm$r.squared,
        p = pf(f[1], f[2], f[3], lower.tail = FALSE),
        stringsAsFactors = FALSE
      )
    )
  }

  model_table <- model_table[order(model_table$AIC), ]
  best_terms <- strsplit(model_table$key[1], "\\|")[[1]]
  best_model <- lm(as.formula(paste(trait, "~", paste(best_terms, collapse = " + "))), data = sub)
  co <- summary(best_model)$coefficients
  ordered_terms <- candidate_terms[candidate_terms %in% best_terms]
  beta <- co[ordered_terms, 1]
  p_beta <- co[ordered_terms, 4]
  stars <- ifelse(p_beta < 0.001, "***", ifelse(p_beta < 0.01, "**", ifelse(p_beta < 0.05, "*", "")))
  signs <- ifelse(beta < 0, " − ", " + ")
  pieces <- paste0(signs, sprintf("%.3f", abs(beta)), term_labels[ordered_terms], stars)
  pieces[1] <- paste0(ifelse(beta[1] < 0, "−", ""), sprintf("%.3f", abs(beta[1])), term_labels[ordered_terms[1]], stars[1])
  equation <- paste0(pieces, collapse = "")

  climate_results[[trait]] <- list(table = model_table, best_model = best_model, best_terms = best_terms, equation = equation)
}

table_2 <- data.frame(
  Trait = unname(climate_labels[climate_traits]),
  Model = character(length(climate_traits)),
  R2 = character(length(climate_traits)),
  p = character(length(climate_traits)),
  stringsAsFactors = FALSE
)

for (i in seq_along(climate_traits)) {
  trait <- climate_traits[i]
  table_2$Model[i] <- climate_results[[trait]]$equation
  table_2$R2[i] <- sprintf("%.3f", climate_results[[trait]]$table$R2[1])
  table_2$p[i] <- ifelse(climate_results[[trait]]$table$p[1] < 0.001, "<0.001", sprintf("%.3f", climate_results[[trait]]$table$p[1]))
}

ft2 <- flextable(table_2) |>
  set_header_labels(Trait = "Trait", Model = "Model", R2 = "R²", p = "p") |>
  bold(part = "header") |>
  align(j = c("R2", "p"), align = "center", part = "all") |>
  align(j = c("Trait", "Model"), align = "left", part = "all") |>
  valign(valign = "top", part = "body") |>
  font(fontname = "Times New Roman", part = "all") |>
  fontsize(size = 10, part = "all") |>
  border_remove() |>
  border_outer(part = "all", border = fp_border(color = "black", width = 1.2)) |>
  border_inner_h(part = "all", border = fp_border(color = "black", width = 0.5)) |>
  border_inner_v(part = "all", border = fp_border(color = "black", width = 0.5)) |>
  width(j = "Trait", width = 1.05) |>
  width(j = "Model", width = 4.40) |>
  width(j = "R2", width = 0.55) |>
  width(j = "p", width = 0.75) |>
  padding(padding = 2, part = "all")

model_p <- numeric(length(climate_traits))
for (i in seq_along(climate_traits)) model_p[i] <- climate_results[[climate_traits[i]]]$table$p[1]
ft2 <- bold(ft2, i = which(model_p < 0.05), j = "p", part = "body")
caption_2 <- fpar(
  ftext("Table 2 The best regression models predicting variation in traits of ", TNR10_B),
  ftext("Spartina alterniflora", TNR10_BI),
  ftext(". ", TNR10_B),
  ftext(
    paste0(
      "R = range, T = temperature and P = precipitation; T² and P² denote quadratic terms and × denotes interactions. ",
      "Asterisks attached to coefficients indicate coefficient-level significance (*p < 0.05, **p < 0.01, ***p < 0.001). ",
      "R² is the goodness of fit, and the final p value is the significance of the whole model."
    ),
    TNR10
  )
)

doc_2 <- read_docx()
doc_2 <- body_add_fpar(doc_2, caption_2)
doc_2 <- body_add_flextable(doc_2, ft2)
print(doc_2, target = "Table_2_best_models.docx")

table_S5 <- data.frame()

for (trait in climate_traits) {
  top <- climate_results[[trait]]$table[1:5, ]
  table_S5 <- rbind(
    table_S5,
    data.frame(
      Trait = unname(climate_labels[trait]),
      Model = top$Model,
      AIC = sprintf("%.3f", top$AIC),
      R2 = sprintf("%.3f", top$R2),
      p = ifelse(top$p < 0.001, "<0.001", sprintf("%.3f", top$p)),
      best = c(TRUE, FALSE, FALSE, FALSE, FALSE),
      stringsAsFactors = FALSE
    )
  )
}

ftS5 <- flextable(table_S5, col_keys = c("Trait", "Model", "AIC", "R2", "p")) |>
  set_header_labels(Trait = "Trait", Model = "Model", AIC = "AIC", R2 = "R²", p = "p") |>
  merge_v(j = "Trait", part = "body") |>
  bold(part = "header") |>
  bold(i = which(table_S5$best), part = "body") |>
  align(j = c("AIC", "R2", "p"), align = "center", part = "all") |>
  align(j = c("Trait", "Model"), align = "left", part = "all") |>
  valign(j = "Trait", valign = "top", part = "body") |>
  font(fontname = "Times New Roman", part = "all") |>
  fontsize(size = 9, part = "all") |>
  border_remove() |>
  border_outer(part = "all", border = fp_border(color = "black", width = 1.2)) |>
  border_inner_h(part = "all", border = fp_border(color = "black", width = 0.5)) |>
  border_inner_v(part = "all", border = fp_border(color = "black", width = 0.5)) |>
  width(j = "Trait", width = 0.95) |>
  width(j = "Model", width = 4.55) |>
  width(j = "AIC", width = 0.75) |>
  width(j = "R2", width = 0.55) |>
  width(j = "p", width = 0.75) |>
  padding(padding = 1.5, part = "all")

caption_S5 <- fpar(
  ftext("Table S5 Multiple regression models predicting variation in traits of ", TNR10_B),
  ftext("Spartina alterniflora", TNR10_BI),
  ftext(". ", TNR10_B),
  ftext(
    paste0(
      "Candidate predictors were range, the linear and quadratic terms of temperature and precipitation, ",
      "and their interactions with range. R = range, T = temperature and P = precipitation; ",
      "superscript ² denotes a quadratic term and × denotes an interaction. ",
      "The five models with the lowest AIC values are shown for each trait, and the minimum-AIC model is in bold. "
    ),
    TNR10
  ),
  ftext("p", TNR10_I),
  ftext(" < 0.05 indicates a significant whole model. ", TNR10),
  ftext("R", TNR10_I),
  ftext("²", TNR10_I),
  ftext(" = goodness of fit.", TNR10)
)

doc_S5 <- read_docx()
doc_S5 <- body_add_fpar(doc_S5, caption_S5)
doc_S5 <- body_add_flextable(doc_S5, ftS5)
print(doc_S5, target = "Table_S5_climate_models.docx")

rda_variables <- c(climate_traits, "Range", "Temperature", "Precipitation", "Collection_site")
rda_data <- climate_data[complete.cases(climate_data[, rda_variables]), ]
rda_data$Site <- factor(rda_data$Collection_site)
rda_data$RangeF <- factor(ifelse(rda_data$Range == 1, "Native", "Introduced"), levels = c("Native", "Introduced"))
response_matrix <- rda_data[, climate_traits]

rda_model <- rda(response_matrix ~ RangeF + Temperature + Precipitation, data = rda_data, scale = TRUE)
permutation_control <- how(within = Within(type = "none"), plots = Plots(strata = rda_data$Site, type = "free"), nperm = 9999)
set.seed(1)
rda_overall <- anova(rda_model, permutations = permutation_control)
rda_marginal <- anova(rda_model, by = "margin", permutations = permutation_control)
rda_r2 <- RsquareAdj(rda_model)$r.squared

marginal_table <- as.data.frame(rda_marginal)
overall_table <- as.data.frame(rda_overall)
overall_F <- overall_table$F[1]
overall_p <- overall_table$`Pr(>F)`[1]
range_F <- marginal_table["RangeF", "F"]
range_p <- marginal_table["RangeF", "Pr(>F)"]
temperature_F <- marginal_table["Temperature", "F"]
temperature_p <- marginal_table["Temperature", "Pr(>F)"]
precipitation_F <- marginal_table["Precipitation", "F"]
precipitation_p <- marginal_table["Precipitation", "Pr(>F)"]

site_scores <- as.data.frame(scores(rda_model, display = "sites", scaling = 2, choices = 1:2))
trait_scores <- as.data.frame(scores(rda_model, display = "species", scaling = 2, choices = 1:2))
climate_scores <- as.data.frame(scores(rda_model, display = "bp", scaling = 2, choices = 1:2))
colnames(site_scores)[1:2] <- colnames(trait_scores)[1:2] <- colnames(climate_scores)[1:2] <- c("RDA1", "RDA2")
climate_scores <- climate_scores[rownames(climate_scores) %in% c("Temperature", "Precipitation"), ]
trait_scores$label <- unname(climate_labels[rownames(trait_scores)])
climate_scores$label <- rownames(climate_scores)

arrow_points <- rbind(trait_scores[, c("RDA1", "RDA2")], climate_scores[, c("RDA1", "RDA2")])
arrow_multiplier <- 0.85 * max(sqrt(site_scores$RDA1^2 + site_scores$RDA2^2)) / max(sqrt(arrow_points$RDA1^2 + arrow_points$RDA2^2))
trait_scores$X <- trait_scores$RDA1 * arrow_multiplier
trait_scores$Y <- trait_scores$RDA2 * arrow_multiplier
climate_scores$X <- climate_scores$RDA1 * arrow_multiplier
climate_scores$Y <- climate_scores$RDA2 * arrow_multiplier
site_scores$Range <- rda_data$RangeF

eigenvalues <- rda_model$CCA$eig
x_label <- sprintf("RDA 1 (%.2f%%)", 100 * eigenvalues[1] / sum(eigenvalues))
y_label <- sprintf("RDA 2 (%.2f%%)", 100 * eigenvalues[2] / sum(eigenvalues))
plot_limit <- max(abs(c(site_scores$RDA1, site_scores$RDA2, trait_scores$X, trait_scores$Y, climate_scores$X, climate_scores$Y))) * 1.12

rda_stats <- paste0(
  "Constrained R² = ", sprintf("%.3f", rda_r2), "\n",
  "Range: F = ", sprintf("%.3f", range_F), ", p ", ifelse(range_p < 0.001, "< 0.001", paste0("= ", sprintf("%.3f", range_p))), "\n",
  "Temperature: F = ", sprintf("%.3f", temperature_F), ", p ", ifelse(temperature_p < 0.001, "< 0.001", paste0("= ", sprintf("%.3f", temperature_p))), "\n",
  "Precipitation: F = ", sprintf("%.3f", precipitation_F), ", p ", ifelse(precipitation_p < 0.001, "< 0.001", paste0("= ", sprintf("%.3f", precipitation_p)))
)

range_palette <- c(Native = "#0072B2", Introduced = "#D55E00")

figure_S4 <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey85") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey85") +
  stat_ellipse(data = site_scores, aes(RDA1, RDA2, colour = Range, fill = Range), geom = "polygon", level = 0.95, alpha = 0.10, linetype = 2, linewidth = 0.7, show.legend = FALSE) +
  geom_point(data = site_scores, aes(RDA1, RDA2, fill = Range), shape = 21, size = 2.6, colour = "white", stroke = 0.4, alpha = 0.9) +
  geom_segment(data = climate_scores, aes(x = 0, y = 0, xend = X, yend = Y), arrow = arrow(length = grid::unit(0.2, "cm")), linewidth = 0.8, colour = "black") +
  geom_text_repel(data = climate_scores, aes(X, Y, label = label), colour = "black", fontface = "bold", size = 3.4, family = "sans", box.padding = 0.4, point.padding = 0.2, max.overlaps = Inf, min.segment.length = 0, segment.linetype = "dashed", segment.colour = "black", segment.size = 0.4) +
  geom_segment(data = trait_scores, aes(x = 0, y = 0, xend = X, yend = Y), arrow = arrow(length = grid::unit(0.2, "cm")), linewidth = 0.6, colour = "#E41A1C") +
  geom_text_repel(data = trait_scores, aes(X, Y, label = label), colour = "#E41A1C", fontface = "bold", size = 3.0, family = "sans", box.padding = 0.4, point.padding = 0.2, max.overlaps = Inf, min.segment.length = 0, segment.linetype = "dashed", segment.colour = "#E41A1C", segment.size = 0.4) +
  scale_fill_manual(values = range_palette, name = "Range") +
  scale_colour_manual(values = range_palette, guide = "none") +
  labs(x = x_label, y = y_label) +
  scale_x_continuous(labels = function(x) sprintf("%.1f", x)) +
  scale_y_continuous(labels = function(y) sprintf("%.1f", y)) +
  theme_bw(base_size = 13, base_family = "sans") +
  theme(
    panel.grid = element_blank(), legend.position = "right",
    legend.title = element_text(face = "bold", size = 12), legend.text = element_text(size = 11),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.5),
    axis.title = element_text(face = "bold", size = 13), axis.text = element_text(size = 11, colour = "black")
  ) +
  guides(fill = guide_legend(override.aes = list(shape = 21, size = 4, colour = "white"))) +
  annotate("label", x = -plot_limit, y = plot_limit, hjust = 0, vjust = 1, label = rda_stats, size = 3.0, family = "sans", lineheight = 0.95, label.size = 0, fill = adjustcolor("white", alpha.f = 0.7)) +
  coord_fixed(ratio = 1, xlim = c(-plot_limit, plot_limit), ylim = c(-plot_limit, plot_limit))

ggsave("Figure_S4_RDA.pdf", figure_S4, width = 7, height = 6)

cat("\nAll analyses completed.\n")
