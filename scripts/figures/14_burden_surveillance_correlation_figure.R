
library(tidyverse)
library(ggplot2)
library(plotly)
library(htmlwidgets)

# ==========================================================
# 2. HELPER FUNCTIONS
# ==========================================================

pick_col <- function(data, candidates,
                     default_val = NA_character_) {
  
  lower_names <- tolower(names(data))
  
  for (cand in tolower(candidates)) {
    
    idx <- match(cand, lower_names)
    
    if (!is.na(idx)) {
      
      vals <- trimws(as.character(data[[idx]]))
      
      vals <- ifelse(
        is.na(vals) | vals == "",
        default_val,
        vals
      )
      
      return(vals)
    }
  }
  
  rep(default_val, nrow(data))
}


format_country_display <- function(name_vec, iso_vec) {
  
  clean_name <- ifelse(
    !is.na(name_vec) & name_vec == toupper(name_vec),
    stringr::str_to_title(tolower(name_vec)),
    name_vec
  )
  
  clean_iso <- ifelse(
    !is.na(iso_vec) & iso_vec != "",
    toupper(iso_vec),
    NA_character_
  )
  
  case_when(
    !is.na(clean_name) & !is.na(clean_iso) ~
      paste0(clean_name, " (", clean_iso, ")"),
    !is.na(clean_name) ~ clean_name,
    !is.na(clean_iso) ~ clean_iso,
    TRUE ~ "Unknown"
  )
}


format_p_val <- function(p) {
  
  if (is.na(p)) return("P = NA")
  
  if (p < 0.001) return("P < 0.001")
  
  paste0(
    "P = ",
    formatC(p, format = "f", digits = 3)
  )
}


# ==========================================================
# 3. CUSTOM X-AXIS TRANSFORMATION
# ==========================================================

# 1–10       = 16% of axis
# 10–100     = 52% of axis
# 100–1000   = 32% of axis

source_axis_transform <- function(x) {
  
  z <- log10(pmax(as.numeric(x), 1))
  
  result <- numeric(length(z))
  
  idx1 <- z <= 1
  
  result[idx1] <- 0.16 * z[idx1]
  
  idx2 <- z > 1 & z <= 2
  
  result[idx2] <- 0.16 +
    0.52 * (z[idx2] - 1)
  
  idx3 <- z > 2
  
  result[idx3] <- 0.68 +
    0.32 * (z[idx3] - 2)
  
  result
}


# ==========================================================
# 4. PROJECT PATHS
# ==========================================================

project_dir <- "outputs"

csv_path <- file.path(
  project_dir,
  "figure_csvs",
  "burden_surveillance_correlation.csv"
)

out_dir <- file.path(
  project_dir,
  "figures",
  "dengue"
)


out_html <- file.path(
  out_dir,
  "figure_05_burden_surveillance_association.html"
)

out_png <- file.path(
  out_dir,
  "figure_05_burden_surveillance_association.png"
)


# ==========================================================
# 5. PANEL SETTINGS
# ==========================================================

panel_levels <- c(
  
  "Reported Cases vs Genomic Surveillance Representation",
  
  "GBD Dengue Incidence vs Genomic Surveillance Representation",
  
  "GBD Dengue Mortality vs Genomic Surveillance Representation",
  
  "GBD Dengue DALYs vs Genomic Surveillance Representation"
  
)


panel_colors <- c(
  
  "Reported Cases vs Genomic Surveillance Representation" =
    "#2563EB",
  
  "GBD Dengue Incidence vs Genomic Surveillance Representation" =
    "#EA580C",
  
  "GBD Dengue Mortality vs Genomic Surveillance Representation" =
    "#7C3AED",
  
  "GBD Dengue DALYs vs Genomic Surveillance Representation" =
    "#DC2626"
  
)


# ==========================================================
# 6. VISUAL SETTINGS
# ==========================================================

POINT_SIZE <- 0.9

POINT_ALPHA <- 0.22

# 50% thinner than the previous 0.65 setting

TREND_WIDTH <- 0.325

X_BREAKS <- c(
  1, 10, 30, 60, 100, 300, 1000
)


# ==========================================================
# 7. LOAD AND PREPARE DATA
# ==========================================================

raw_df <- read_csv(
  csv_path,
  show_col_types = FALSE
)


df <- raw_df %>%
  
  mutate(
    
    raw_metric = pick_col(
      .,
      c(
        "metric",
        "panel",
        "measure",
        "indicator",
        "burden_metric"
      )
    ),
    
    metric_clean = str_to_lower(raw_metric),
    
    panel_title = case_when(
      
      str_detect(
        metric_clean,
        "case|opendengue|reported"
      ) ~ panel_levels[1],
      
      str_detect(
        metric_clean,
        "incid"
      ) ~ panel_levels[2],
      
      str_detect(
        metric_clean,
        "death|mort"
      ) ~ panel_levels[3],
      
      str_detect(
        metric_clean,
        "daly"
      ) ~ panel_levels[4],
      
      TRUE ~ raw_metric
    ),
    
    n_sources = as.numeric(
      pick_col(
        .,
        c(
          "n_sources",
          "sources",
          "n_obs",
          "source_count"
        )
      )
    ),
    
    estimate = as.numeric(
      pick_col(
        .,
        c(
          "estimate",
          "burden_estimate",
          "value",
          "val",
          "cases"
        )
      )
    ),
    
    raw_country = pick_col(
      .,
      c(
        "country",
        "country_name",
        "location_name",
        "location",
        "entity",
        "countryterritory",
        "admin0"
      )
    ),
    
    raw_iso = pick_col(
      .,
      c(
        "iso3",
        "iso_code",
        "country_iso3",
        "iso",
        "code"
      )
    ),
    
    country_lbl = format_country_display(
      raw_country,
      raw_iso
    ),
    
    year_lbl = pick_col(
      .,
      c(
        "year",
        "collection_year",
        "period",
        "date_year"
      ),
      default_val = "Unknown"
    )
    
  ) %>%
  
  filter(
    
    panel_title %in% panel_levels,
    
    !is.na(n_sources),
    n_sources > 0,
    
    !is.na(estimate),
    estimate > 0
    
  ) %>%
  
  mutate(
    
    panel_title = factor(
      panel_title,
      levels = panel_levels
    ),
    
    x_display = source_axis_transform(n_sources)
    
  )


# ==========================================================
# 8. DYNAMIC X-AXIS BREAKS
# ==========================================================

max_sources <- max(df$n_sources, na.rm = TRUE)

if (max_sources > 1000) {
  X_BREAKS <- c(X_BREAKS, 3000)
}

if (max_sources > 3000) {
  X_BREAKS <- c(X_BREAKS, 10000)
}

if (max_sources > 10000) {
  X_BREAKS <- c(X_BREAKS, 30000)
}

X_BREAKS <- X_BREAKS[
  X_BREAKS <= max_sources * 1.05
]


# ==========================================================
# 9. CORRELATION STATISTICS
# ==========================================================

stats_df <- df %>%
  
  group_by(panel_title) %>%
  
  summarise(
    
    n_obs = n(),
    
    rho = suppressWarnings(
      cor(
        n_sources,
        estimate,
        method = "spearman",
        use = "complete.obs"
      )
    ),
    
    p_val = tryCatch(
      
      suppressWarnings(
        cor.test(
          n_sources,
          estimate,
          method = "spearman",
          exact = FALSE
        )$p.value
      ),
      
      error = function(e) NA_real_
      
    ),
    
    .groups = "drop"
    
  ) %>%
  
  mutate(
    
    p_text = vapply(
      p_val,
      format_p_val,
      character(1)
    ),
    
    stat_html = paste0(
      
      "<b>\u03c1 = ",
      formatC(rho, format = "f", digits = 2),
      "</b><br>",
      
      p_text,
      "<br>",
      
      "N = ",
      format(n_obs, big.mark = ",")
      
    ),
    
    stat_plain = paste0(
      
      "\u03c1 = ",
      formatC(rho, format = "f", digits = 2),
      "\n",
      
      p_text,
      "\n",
      
      "N = ",
      format(n_obs, big.mark = ",")
      
    )
    
  )


# ==========================================================
# 10. REGRESSION LINES
# ==========================================================

trend_df <- df %>%
  
  group_by(panel_title) %>%
  
  group_modify(~ {
    
    dat <- .x
    
    if (n_distinct(dat$n_sources) < 2) {
      return(tibble())
    }
    
    fit <- lm(
      log10(estimate) ~ log10(n_sources),
      data = dat
    )
    
    x_seq <- 10^seq(
      
      log10(min(dat$n_sources)),
      log10(max(dat$n_sources)),
      
      length.out = 200
      
    )
    
    predicted_log <- predict(
      fit,
      newdata = tibble(
        n_sources = x_seq
      )
    )
    
    tibble(
      
      n_sources = x_seq,
      
      x_display = source_axis_transform(x_seq),
      
      estimate_fit = 10^predicted_log
      
    )
    
  }) %>%
  
  ungroup()


# ==========================================================
# 11. COMMON X-AXIS SCALE
# ==========================================================

custom_x_scale <- function() {
  
  scale_x_continuous(
    
    breaks = source_axis_transform(X_BREAKS),
    
    labels = scales::comma(X_BREAKS),
    
    expand = expansion(
      mult = c(0.02, 0.05)
    )
    
  )
}


# ==========================================================
# 12. Y-AXIS FORMATTING
# ==========================================================

y_axis_labels <- scales::label_number(
  big.mark = ",",
  trim = TRUE
)


# ==========================================================
# 13. INTERACTIVE PANEL FUNCTION
# ==========================================================

make_panel <- function(p_title, col_hex) {
  
  sub_df <- df %>%
    filter(panel_title == p_title)
  
  sub_trend <- trend_df %>%
    filter(panel_title == p_title)
  
  
  g <- ggplot(
    
    sub_df,
    
    aes(
      x = x_display,
      y = estimate
    )
    
  ) +
    
    geom_point(
      
      aes(
        
        text = paste0(
          
          "<b>", p_title, "</b><br>",
          
          "Country: ", country_lbl, "<br>",
          
          "Year: ", year_lbl, "<br>",
          
          "Source Observations: ",
          format(n_sources, big.mark = ","), "<br>",
          
          "Burden Estimate: ",
          format(round(estimate, 2), big.mark = ",")
          
        )
        
      ),
      
      colour = col_hex,
      
      size = POINT_SIZE,
      
      alpha = POINT_ALPHA
      
    ) +
    
    geom_line(
      
      data = sub_trend,
      
      aes(
        x = x_display,
        y = estimate_fit
      ),
      
      inherit.aes = FALSE,
      
      colour = "#0F172A",
      
      linewidth = TREND_WIDTH
      
    ) +
    
    custom_x_scale() +
    
    scale_y_log10(
      labels = y_axis_labels
    ) +
    
    labs(
      x = NULL,
      y = NULL
    ) +
    
    theme_minimal(base_size = 11) +
    
    theme(
      
      panel.background = element_rect(
        fill = "#FFFFFF",
        colour = NA
      ),
      
      plot.background = element_rect(
        fill = "#FFFFFF",
        colour = NA
      ),
      
      panel.border = element_rect(
        colour = "#CBD5E1",
        fill = NA,
        linewidth = 0.6
      ),
      
      panel.grid.major = element_line(
        colour = "#E5E7EB",
        linewidth = 0.45
      ),
      
      panel.grid.minor = element_blank(),
      
      axis.title = element_blank(),
      
      axis.text = element_text(
        size = 9,
        colour = "#374151"
      ),
      
      plot.margin = margin(
        t = 3,
        r = 5,
        b = 3,
        l = 5
      )
      
    )
  
  
  ggplotly(
    g,
    tooltip = "text"
  )
  
}


# ==========================================================
# 14. CREATE FOUR INTERACTIVE PANELS
# ==========================================================

pA <- make_panel(
  panel_levels[1],
  panel_colors[1]
)

pB <- make_panel(
  panel_levels[2],
  panel_colors[2]
)

pC <- make_panel(
  panel_levels[3],
  panel_colors[3]
)

pD <- make_panel(
  panel_levels[4],
  panel_colors[4]
)


# ==========================================================
# 15. COMBINE PANELS
# ==========================================================

fig <- subplot(
  
  pA, pB,
  pC, pD,
  
  nrows = 2,
  
  shareX = FALSE,
  shareY = FALSE,
  
  titleX = FALSE,
  titleY = FALSE,
  
  margin = c(
    0.07,
    0.07,
    0.07,
    0.07
  )
  
)


# ==========================================================
# 16. FIGURE ANNOTATIONS
# ==========================================================

panel_annotations <- list(
  
  # Main title
  
  list(
    
    text = "<b>Dengue Burden vs. Genomic Surveillance Representation</b>",
    
    x = 0.01,
    y = 1.13,
    
    xref = "paper",
    yref = "paper",
    
    xanchor = "left",
    yanchor = "bottom",
    
    showarrow = FALSE,
    
    font = list(
      size = 19,
      color = "#111827"
    )
    
  ),
  
  # Subtitle
  
  list(
    
    text = paste0(
      "Ecological association across country-years ",
      "(Reported = OpenDengue; Modelled = IHME GBD)"
    ),
    
    x = 0.01,
    y = 1.075,
    
    xref = "paper",
    yref = "paper",
    
    xanchor = "left",
    yanchor = "bottom",
    
    showarrow = FALSE,
    
    font = list(
      size = 12,
      color = "#4B5563"
    )
    
  ),
  
  # Shared Y-axis title
  
  list(
    
    text = "<b>Burden Estimate (log10 scale)</b>",
    
    x = -0.085,
    y = 0.50,
    
    xref = "paper",
    yref = "paper",
    
    textangle = -90,
    
    xanchor = "center",
    yanchor = "middle",
    
    showarrow = FALSE,
    
    font = list(
      size = 11,
      color = "#1F2937"
    )
    
  ),
  
  # Shared X-axis title
  
  list(
    
    text = "<b>Source Observations (custom transformed scale)</b>",
    
    x = 0.50,
    y = -0.105,
    
    xref = "paper",
    yref = "paper",
    
    xanchor = "center",
    yanchor = "middle",
    
    showarrow = FALSE,
    
    font = list(
      size = 11,
      color = "#1F2937"
    )
    
  )
  
)


# ==========================================================
# 17. PANEL TITLES AND STATISTICS
# ==========================================================

axis_pairs <- list(
  
  list(
    xref = "x domain",
    yref = "y domain",
    title = panel_levels[1]
  ),
  
  list(
    xref = "x2 domain",
    yref = "y2 domain",
    title = panel_levels[2]
  ),
  
  list(
    xref = "x3 domain",
    yref = "y3 domain",
    title = panel_levels[3]
  ),
  
  list(
    xref = "x4 domain",
    yref = "y4 domain",
    title = panel_levels[4]
  )
  
)


for (ap in axis_pairs) {
  
  st_row <- stats_df %>%
    filter(panel_title == ap$title)
  
  
  panel_annotations[[length(panel_annotations) + 1]] <- list(
    
    text = paste0(
      "<b>",
      ap$title,
      "</b>"
    ),
    
    x = 0.01,
    y = 1.035,
    
    xref = ap$xref,
    yref = ap$yref,
    
    xanchor = "left",
    yanchor = "bottom",
    
    showarrow = FALSE,
    
    font = list(
      size = 11,
      color = "#111827"
    )
    
  )
  
  
  if (nrow(st_row) == 1) {
    
    panel_annotations[[length(panel_annotations) + 1]] <- list(
      
      text = st_row$stat_html,
      
      x = 0.025,
      y = 0.95,
      
      xref = ap$xref,
      yref = ap$yref,
      
      xanchor = "left",
      yanchor = "top",
      
      align = "left",
      
      showarrow = FALSE,
      
      font = list(
        size = 10,
        color = "#111827"
      ),
      
      bgcolor = "rgba(255, 255, 255, 0.94)",
      
      bordercolor = "#94A3B8",
      
      borderwidth = 1,
      
      borderpad = 4
      
    )
    
  }
  
}


# ==========================================================
# 18. FINAL INTERACTIVE LAYOUT
# ==========================================================

fig <- fig %>%
  
  layout(
    
    showlegend = FALSE,
    
    autosize = TRUE,
    
    # Compact overall height.
    # Both rows share this height.
    
    height = 750,
    
    margin = list(
      
      l = 105,
      r = 25,
      t = 105,
      b = 85
      
    ),
    
    annotations = panel_annotations
    
  )


fig <- config(
  
  fig,
  
  responsive = TRUE,
  
  displaylogo = FALSE
  
)


# ==========================================================
# 19. STATIC FOUR-PANEL FIGURE
# ==========================================================

p_static <- ggplot(
  
  df,
  
  aes(
    x = x_display,
    y = estimate
  )
  
) +
  
  geom_point(
    
    aes(
      colour = panel_title
    ),
    
    size = POINT_SIZE,
    
    alpha = POINT_ALPHA
    
  ) +
  
  geom_line(
    
    data = trend_df,
    
    aes(
      x = x_display,
      y = estimate_fit
    ),
    
    inherit.aes = FALSE,
    
    colour = "#0F172A",
    
    linewidth = TREND_WIDTH
    
  ) +
  
  geom_label(
    
    data = stats_df,
    
    aes(
      x = source_axis_transform(1.15),
      y = Inf,
      label = stat_plain
    ),
    
    inherit.aes = FALSE,
    
    hjust = 0,
    vjust = 1.2,
    
    size = 3.1,
    
    fill = "#FFFFFF",
    
    colour = "#111827",
    
    label.size = 0.35
    
  ) +
  
  facet_wrap(
    
    ~ panel_title,
    
    ncol = 2,
    
    scales = "free_y"
    
  ) +
  
  custom_x_scale() +
  
  scale_y_log10(
    labels = y_axis_labels
  ) +
  
  scale_colour_manual(
    values = panel_colors,
    guide = "none"
  ) +
  
  labs(
    
    title = "Dengue Burden vs. Genomic Surveillance Representation",
    
    subtitle = paste0(
      "Ecological association across country-years ",
      "(Reported = OpenDengue; Modelled = IHME GBD)"
    ),
    
    x = "Source Observations (custom transformed scale)",
    
    y = "Burden Estimate (log10 scale)"
    
  ) +
  
  theme_minimal(base_size = 11.5) +
  
  theme(
    
    panel.background = element_rect(
      fill = "#FFFFFF",
      colour = NA
    ),
    
    plot.background = element_rect(
      fill = "#FFFFFF",
      colour = NA
    ),
    
    panel.border = element_rect(
      colour = "#CBD5E1",
      fill = NA,
      linewidth = 0.6
    ),
    
    panel.grid.major = element_line(
      colour = "#E5E7EB",
      linewidth = 0.45
    ),
    
    panel.grid.minor = element_blank(),
    
    strip.text = element_text(
      face = "bold",
      size = 10.5,
      colour = "#111827",
      hjust = 0
    ),
    
    plot.title = element_text(
      size = 16,
      face = "bold",
      colour = "#111827",
      margin = margin(b = 5)
    ),
    
    plot.subtitle = element_text(
      size = 10.5,
      colour = "#4B5563",
      margin = margin(b = 14)
    ),
    
    axis.title = element_text(
      face = "bold",
      size = 10,
      colour = "#1F2937"
    ),
    
    plot.margin = margin(
      t = 15,
      r = 18,
      b = 15,
      l = 18
    )
    
  )


# ==========================================================
# 20. SAVE STATIC PNG
# ==========================================================

ggsave(
  
  filename = out_png,
  
  plot = p_static,
  
  width = 13,
  
  height = 8.5,
  
  dpi = 600,
  
  bg = "white"
  
)


# ==========================================================
# 21. SAVE INTERACTIVE HTML
# ==========================================================

saveWidget(
  
  widget = fig,
  
  file = out_html,
  
  selfcontained = TRUE,
  
  title = "Dengue Burden vs Genomic Surveillance"
  
)


# ==========================================================
# 22. OPEN OUTPUT
# ==========================================================

html_uri <- paste0(
  
  "file:///",
  
  gsub(
    "\\\\",
    "/",
    normalizePath(
      out_html,
      mustWork = FALSE
    )
  )
  
)


message(
  "Static PNG saved at: ",
  normalizePath(out_png, mustWork = FALSE)
)

message(
  "Interactive HTML saved at: ",
  normalizePath(out_html, mustWork = FALSE)
)

message(
  "Browser link: ",
  html_uri
)


chrome_paths <- c(
  
  "C:/Program Files/Google/Chrome/Application/chrome.exe",
  
  "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe"
  
)

chrome_exe <- chrome_paths[
  file.exists(chrome_paths)
][1]


if (!is.na(chrome_exe)) {
  
  system2(
    chrome_exe,
    args = shQuote(html_uri),
    wait = FALSE
  )
  
} else {
  
  browseURL(html_uri)
  
}
