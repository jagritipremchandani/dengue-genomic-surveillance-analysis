library(tidyverse)
library(plotly)
library(htmlwidgets)

csv_path <- file.path(
  "outputs",
  "figure_csvs",
  "global_sequencing_representation_map.csv"
)

out_dir <- file.path("outputs", "figures", "dengue")
out_html <- file.path(out_dir, "figure_01_surveillance_distribution.html")
out_png <- file.path(out_dir, "figure_01_surveillance_distribution.png")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

df <- read_csv(csv_path, show_col_types = FALSE) %>%
  mutate(
    log_sources = log10(n_sources + 1),
    bubble_size = pmax(log_sources, 0.35)
  )

title_html <- paste0(
  "<b>Global Distribution of Dengue Genomic Surveillance Sources</b>",
  "<br><span style='font-size:13px; color:#4A5568;'>",
  "Source Inventory observations aggregated across all available years (n = 93,826)",
  "</span>"
)

caption_text <- paste(
  "Bubble size and colour represent the number of source-level dengue genomic observations recorded in the Source Inventory.",
  "Counts are aggregated across all available collection years and do not represent incidence, burden, or infection counts.",
  "Source inventories are constructed using the highest-available source identifier and retain distinctions among complete genomes,",
  "partial sequences and genome segments."
)

p <- plot_ly(
  data = df,
  type = "scattergeo",
  locations = ~iso3,
  locationmode = "ISO-3",
  color = ~log_sources,
  colors = "YlOrRd",
  size = ~bubble_size,
  sizes = c(5, 20),
  text = ~paste0(
    "<b>", country, "</b> (", iso3, ")<br>",
    "Source observations: ", format(n_sources, big.mark = ","), "<br>",
    "log10(Sources + 1): ", round(log_sources, 2)
  ),
  hoverinfo = "text",
  marker = list(
    opacity = 0.9,
    line = list(color = "#4A0E0E", width = 0.7)
  )
) %>%
  layout(
    title = list(
      text = title_html,
      font = list(size = 18),
      x = 0.02,
      xanchor = "left"
    ),
    margin = list(l = 10, r = 10, t = 75, b = 70),
    geo = list(
      projection = list(type = "natural earth"),
      resolution = 50,
      showland = TRUE,
      landcolor = "#F7F5EE",
      showcountries = TRUE,
      countrycolor = "#9E988E",
      countrywidth = 1,
      showocean = TRUE,
      oceancolor = "#C6DDF0",
      showcoastlines = TRUE,
      coastlinecolor = "#7E9CB8",
      coastlinewidth = 0.8,
      showframe = FALSE
    ),
    annotations = list(
      # Corner summary box (adjust x/y if it overlaps a region you want clear)
      list(
        text = paste0(
          "<b>Countries represented:</b> 146<br>",
          "<b>Source observations:</b> 93,826<br>",
          "<b>Collection years:</b> 1905–2025"
        ),
        x = 0.02,
        y = 0.14,
        xref = "paper",
        yref = "paper",
        xanchor = "left",
        yanchor = "bottom",
        align = "left",
        showarrow = FALSE,
        font = list(size = 11.5, color = "#1F2937"),
        bgcolor = "rgba(255, 255, 255, 0.88)",
        bordercolor = "#9E988E",
        borderwidth = 1,
        borderpad = 6
      ),
      # Bottom explanatory caption
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
  ) %>%
  colorbar(
    title = "Source Observations<br>(log10 scale)",
    thickness = 15,
    len = 0.68
  )

saveWidget(p, file = out_html, selfcontained = TRUE)

if (requireNamespace("webshot2", quietly = TRUE)) {
  webshot2::webshot(out_html, file = out_png, vwidth = 1400, vheight = 800, delay = 1)
} else {
  save_image(p, file = out_png, width = 1400, height = 800, scale = 2)
}

browseURL(out_html)
