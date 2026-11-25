# ============================================================
# FIGURE 02: GLOBAL PERSISTENCE OF DENGUE GENOMIC SURVEILLANCE
# Reproducible HTML + PNG export
#
# HTML: Plotly interactive choropleth
# PNG:  ggplot2 static choropleth
#
# PNG export does NOT require Chrome, webshot2 or Kaleido.
# ============================================================


# 1. PACKAGES -------------------------------------------------

required_packages <- c(
  "tidyverse",
  "plotly",
  "htmlwidgets",
  "sf",
  "rnaturalearth",
  "rnaturalearthdata"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {
  stop(
    "Missing packages: ",
    paste(missing_packages, collapse = ", "),
    ". Install them using install.packages()."
  )
}

library(tidyverse)
library(plotly)
library(htmlwidgets)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


# 2. PROJECT PATHS --------------------------------------------

project_dir <- "."

csv_path <- file.path(
  project_dir,
  "outputs",
  "figure_csvs",
  "surveillance_persistence.csv"
)

out_dir <- file.path(
  project_dir,
  "outputs",
  "figures",
  "dengue"
)

dir.create(
  out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

out_html <- normalizePath(
  file.path(
    out_dir,
    "figure_02_surveillance_persistence.html"
  ),
  mustWork = FALSE
)

out_png <- normalizePath(
  file.path(
    out_dir,
    "figure_02_surveillance_persistence.png"
  ),
  mustWork = FALSE
)

if (!file.exists(csv_path)) {
  stop("Input CSV not found: ", csv_path)
}


# 3. READ AND VALIDATE DATA -----------------------------------

df <- read_csv(
  csv_path,
  show_col_types = FALSE
)

required_columns <- c(
  "iso3",
  "country",
  "years_represented"
)

missing_columns <- setdiff(
  required_columns,
  names(df)
)

if (length(missing_columns) > 0) {
  stop(
    "The input CSV is missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

df <- df %>%
  mutate(
    iso3 = str_to_upper(str_trim(as.character(iso3))),
    persistence_years = as.numeric(years_represented)
  ) %>%
  filter(
    !is.na(iso3),
    iso3 != "",
    !is.na(persistence_years),
    is.finite(persistence_years)
  )

if (nrow(df) == 0) {
  stop("No valid observations remain after data cleaning.")
}

if (anyDuplicated(df$iso3)) {
  stop(
    "The input contains multiple rows per ISO3 code. ",
    "Resolve duplicates before creating the country-level map."
  )
}

if (any(df$persistence_years < 0)) {
  stop("Persistence years cannot be negative.")
}


# 4. SUMMARY STATISTICS ---------------------------------------

n_countries <- n_distinct(df$iso3)

max_persistence <- max(
  df$persistence_years,
  na.rm = TRUE
)

median_persistence <- median(
  df$persistence_years,
  na.rm = TRUE
)


# 5. TITLES AND ANNOTATIONS -----------------------------------

title_html <- paste0(
  "<b>Global Persistence of Dengue Genomic Surveillance</b>",
  "<br><span style='font-size:13px; color:#4A5568;'>",
  "Countries coloured by the number of years with at least one ",
  "source-level genomic observation",
  "</span>"
)

caption_text <- paste(
  "Global persistence of dengue genomic surveillance measured as",
  "the number of calendar years represented in the Source Inventory.",
  "Darker countries indicate longer-term surveillance activity and",
  "continuous genomic representation across multiple years.",
  "Counts represent years with at least one source-level genomic",
  "observation and are independent of the total number of sequences generated."
)

annotation_box_text <- paste0(
  "<b>Countries represented:</b> ",
  format(n_countries, big.mark = ","),
  "<br>",
  "<b>Maximum surveillance persistence:</b> ",
  max_persistence,
  " years<br>",
  "<b>Median surveillance persistence:</b> ",
  round(median_persistence, 1),
  " years"
)


# 6. INTERACTIVE PLOTLY MAP -----------------------------------

p <- plot_ly(
  data = df,
  type = "choropleth",
  locations = ~iso3,
  locationmode = "ISO-3",
  z = ~persistence_years,
  colors = "YlGnBu",
  text = ~paste0(
    "<b>", country, "</b> (", iso3, ")<br>",
    "Surveillance persistence: ",
    persistence_years,
    " years"
  ),
  hoverinfo = "text",
  marker = list(
    line = list(
      color = "#9E988E",
      width = 0.7
    )
  ),
  colorbar = list(
    title = "Surveillance<br>Years",
    thickness = 15,
    len = 0.68
  )
) %>%
  layout(
    title = list(
      text = title_html,
      font = list(size = 18),
      x = 0.02,
      xanchor = "left"
    ),
    margin = list(
      l = 10,
      r = 10,
      t = 75,
      b = 75
    ),
    geo = list(
      projection = list(type = "natural earth"),
      resolution = 50,
      showland = TRUE,
      landcolor = "#F7F5EE",
      showcountries = TRUE,
      countrycolor = "#9E988E",
      countrywidth = 0.8,
      showocean = TRUE,
      oceancolor = "#C6DDF0",
      showcoastlines = TRUE,
      coastlinecolor = "#7E9CB8",
      coastlinewidth = 0.8,
      showframe = FALSE
    ),
    annotations = list(
      list(
        text = annotation_box_text,
        x = 0.02,
        y = 0.14,
        xref = "paper",
        yref = "paper",
        xanchor = "left",
        yanchor = "bottom",
        align = "left",
        showarrow = FALSE,
        font = list(
          size = 11.5,
          color = "#1F2937"
        ),
        bgcolor = "rgba(255, 255, 255, 0.90)",
        bordercolor = "#9E988E",
        borderwidth = 1,
        borderpad = 6
      ),
      list(
        text = caption_text,
        x = 0.02,
        y = -0.14,
        xref = "paper",
        yref = "paper",
        xanchor = "left",
        yanchor = "top",
        align = "left",
        showarrow = FALSE,
        font = list(
          size = 10.5,
          color = "#4B5563"
        )
      )
    )
  )


# 7. SAVE INTERACTIVE HTML ------------------------------------

saveWidget(
  p,
  file = out_html,
  selfcontained = FALSE
)


# 8. PREPARE NATURAL EARTH DATA FOR STATIC PNG ----------------

world_sf <- ne_countries(
  scale = 50,
  returnclass = "sf"
) %>%
  filter(
    iso_a3 != "-99",
    iso_a3 != "ATA"
  ) %>%
  st_transform(
    crs = "+proj=natearth +datum=WGS84 +units=m +no_defs"
  )

# Join surveillance data to country boundaries.
world_map <- world_sf %>%
  left_join(
    df,
    by = c("iso_a3" = "iso3")
  )

matched_countries <- sum(
  !is.na(world_map$persistence_years)
)

message(
  "Countries in input data: ", n_countries,
  "\nCountries matched to map: ", matched_countries
)

if (matched_countries == 0) {
  stop(
    "No ISO3 codes matched the Natural Earth map. ",
    "Check the ISO3 codes in the input CSV."
  )
}


# 9. STATIC PNG MAP -------------------------------------------

p_static <- ggplot() +
  
  geom_sf(
    data = world_map,
    fill = "#F7F5EE",
    color = "#9E988E",
    linewidth = 0.18
  ) +
  
  geom_sf(
    data = world_map %>%
      filter(!is.na(persistence_years)),
    aes(fill = persistence_years),
    color = "#9E988E",
    linewidth = 0.18
  ) +
  
  scale_fill_distiller(
    palette = "YlGnBu",
    direction = 1,
    name = "Surveillance\nYears",
    na.value = "#F7F5EE"
  ) +
  
  labs(
    title = "Global Persistence of Dengue Genomic Surveillance",
    subtitle = paste(
      "Countries coloured by the number of years with at least",
      "one source-level genomic observation"
    ),
    caption = paste0(
      caption_text,
      "\n\nCountries represented: ",
      format(n_countries, big.mark = ","),
      " | Maximum persistence: ",
      max_persistence,
      " years | Median persistence: ",
      round(median_persistence, 1),
      " years"
    )
  ) +
  
  coord_sf(
    crs = st_crs(world_map),
    datum = NA,
    expand = FALSE
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 17,
      color = "#1F2937"
    ),
    plot.subtitle = element_text(
      size = 10.5,
      color = "#4A5568",
      margin = margin(b = 12)
    ),
    plot.caption = element_text(
      size = 8.5,
      color = "#4B5563",
      hjust = 0,
      margin = margin(t = 12)
    ),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = element_rect(
      fill = "#C6DDF0",
      color = NA
    ),
    plot.margin = margin(15, 15, 15, 15)
  )


# 10. SAVE PNG ------------------------------------------------

ggsave(
  filename = out_png,
  plot = p_static,
  width = 14,
  height = 8,
  units = "in",
  dpi = 300,
  bg = "white"
)


# 11. COMPLETION ----------------------------------------------

message("\nFigure 02 successfully generated.")
message("Interactive HTML: ", out_html)
message("Static PNG: ", out_png)

if (interactive() && file.exists(out_html)) {
  browseURL(out_html)
}
