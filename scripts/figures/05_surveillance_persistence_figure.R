library(tidyverse)
library(plotly)
library(htmlwidgets)

get_project_root <- function() {
  script_path <- NULL
  
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    ctx <- tryCatch(rstudioapi::getActiveDocumentContext(), error = function(e) NULL)
    if (!is.null(ctx) && nzchar(ctx$path)) {
      script_path <- ctx$path
    }
  }
  
  if (is.null(script_path)) {
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("^--file=", args, value = TRUE)
    if (length(file_arg) > 0) {
      script_path <- sub("^--file=", "", file_arg[1])
    }
  }
  
  if (!is.null(script_path) && nzchar(script_path)) {
    root_dir <- dirname(normalizePath(script_path, mustWork = FALSE))
    while (basename(root_dir) %in% c("scripts", "pipeline", "qc", "reporting", "supplementary", "figures")) {
      root_dir <- dirname(root_dir)
    }
    return(root_dir)
  }
  
  wd <- normalizePath(getwd(), mustWork = FALSE)
  while (basename(wd) %in% c("scripts", "pipeline", "qc", "reporting", "supplementary", "figures")) {
    wd <- dirname(wd)
  }
  wd
}

project_dir <- get_project_root()

csv_path <- file.path(
  project_dir,
  "outputs",
  "figure_csvs",
  "surveillance_persistence.csv"
)
out_dir <- file.path(project_dir, "outputs", "figures", "dengue")
out_html <- file.path(out_dir, "figure_02_surveillance_persistence.html")
out_png <- file.path(out_dir, "figure_02_surveillance_persistence.png")

df <- read_csv(csv_path, show_col_types = FALSE) %>%
  mutate(
    # Replace `years_represented` below if your CSV names this column differently
    persistence_years = as.numeric(years_represented)
  ) %>%
  filter(!is.na(persistence_years))

n_countries <- n_distinct(df$iso3)
max_persistence <- max(df$persistence_years, na.rm = TRUE)
median_persistence <- median(df$persistence_years, na.rm = TRUE)

title_html <- paste0(
  "<b>Global Persistence of Dengue Genomic Surveillance</b>",
  "<br><span style='font-size:13px; color:#4A5568;'>",
  "Countries coloured by the number of years with at least one source-level genomic observation",
  "</span>"
)

caption_text <- paste(
  "Global persistence of dengue genomic surveillance measured as the number of calendar years represented in the Source Inventory.",
  "Darker countries indicate longer-term surveillance activity and continuous genomic representation across multiple years.",
  "Counts represent years with at least one source-level genomic observation and are independent of the total number of sequences generated."
)

annotation_box_text <- paste0(
  "<b>Countries represented:</b> ", format(n_countries, big.mark = ","), "<br>",
  "<b>Maximum surveillance persistence:</b> ", max_persistence, " years<br>",
  "<b>Median surveillance persistence:</b> ", round(median_persistence, 1), " years"
)

p <- plot_ly(
  data = df,
  type = "choropleth",
  locations = ~iso3,
  locationmode = "ISO-3",
  z = ~persistence_years,
  colors = "YlGnBu",
  text = ~paste0(
    "<b>", country, "</b> (", iso3, ")<br>",
    "Surveillance persistence: ", persistence_years, " years"
  ),
  hoverinfo = "text",
  marker = list(
    line = list(color = "#9E988E", width = 0.7)
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
    margin = list(l = 10, r = 10, t = 75, b = 75),
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
        font = list(size = 11.5, color = "#1F2937"),
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
        font = list(size = 10.5, color = "#4B5563")
      )
    )
  )

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
saveWidget(p, file = out_html, selfcontained = TRUE)

if (requireNamespace("webshot2", quietly = TRUE)) {
  webshot2::webshot(out_html, file = out_png, vwidth = 1400, vheight = 800, delay = 1)
} else {
  plotly::save_image(p, file = out_png, width = 1400, height = 800, scale = 2)
}

message("Saved interactive map to: ", out_html)
message("Saved PNG map to: ", out_png)
browseURL(out_html)
