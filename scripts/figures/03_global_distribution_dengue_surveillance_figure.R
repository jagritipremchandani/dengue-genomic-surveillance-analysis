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
  filter(!is.na(iso3), nzchar(trimws(iso3)), !is.na(n_sources)) %>%
  mutate(
    iso3 = toupper(trimws(iso3)),
    log_sources = log10(n_sources + 1),
    bubble_size = pmax(log_sources, 0.35)
  )

n_countries <- n_distinct(df$iso3)
total_obs <- sum(df$n_sources, na.rm = TRUE)
year_label <- if ("year" %in% names(df) && any(!is.na(df$year))) {
  paste0(min(df$year, na.rm = TRUE), "\u2013", max(df$year, na.rm = TRUE))
} else {
  "1905\u20132025"
}

title_html <- paste0(
  "<b>Global Distribution of Dengue Genomic Surveillance Sources</b>",
  "<br><span style='font-size:13px; color:#4A5568;'>",
  "Source Inventory observations aggregated across all available years (n = ",
  format(total_obs, big.mark = ","),
  ")</span>"
)

caption_html <- paste(
  "Bubble size and colour represent the number of source-level dengue genomic observations recorded in the Source Inventory.",
  "Counts are aggregated across all available collection years and do not represent incidence, burden, or infection counts.<br>",
  "Source inventories are constructed using the highest-available source identifier and retain distinctions among complete genomes,",
  "partial sequences and genome segments."
)

max_bubble <- max(df$bubble_size, na.rm = TRUE)
sizeref_val <- 2 * max_bubble / (12^2)

p <- plot_ly(
  data = df,
  type = "scattergeo",
  locations = ~iso3,
  locationmode = "ISO-3",
  text = ~paste0(
    "<b>", country, "</b> (", iso3, ")<br>",
    "Source observations: ", format(n_sources, big.mark = ","), "<br>",
    "log10(Sources + 1): ", round(log_sources, 2)
  ),
  hoverinfo = "text",
  marker = list(
    color = ~log_sources,
    colorscale = "YlOrRd",
    reversescale = FALSE,
    size = ~bubble_size,
    sizemode = "area",
    sizeref = sizeref_val,
    sizemin = 4.5,
    opacity = 0.88,
    line = list(color = "#4A0E0E", width = 0.7),
    colorbar = list(
      title = list(
        text = "Source Observations<br>(log10 scale)",
        font = list(size = 11.5)
      ),
      thickness = 14,
      len = 0.62,
      x = 0.99,
      xanchor = "left",
      y = 0.52,
      yanchor = "middle"
    )
  )
) %>%
  layout(
    width = 1400,
    height = 800,
    autosize = FALSE,
    title = list(
      text = title_html,
      font = list(size = 18, color = "#111827"),
      x = 0.02,
      y = 0.96,
      xanchor = "left",
      yanchor = "top"
    ),
    margin = list(l = 15, r = 110, t = 80, b = 95),
    geo = list(
      domain = list(x = c(0, 0.96), y = c(0.12, 0.92)),
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
      list(
        text = paste0(
          "<b>Countries represented:</b> ", format(n_countries, big.mark = ","), "<br>",
          "<b>Source observations:</b> ", format(total_obs, big.mark = ","), "<br>",
          "<b>Collection years:</b> ", year_label
        ),
        x = 0.02,
        y = 0.16,
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
        text = caption_html,
        x = 0.02,
        y = 0.01,
        xref = "paper",
        yref = "paper",
        xanchor = "left",
        yanchor = "bottom",
        align = "left",
        showarrow = FALSE,
        font = list(size = 10.5, color = "#4B5563")
      )
    )
  ) %>%
  config(displaylogo = FALSE)

saveWidget(p, file = out_html, selfcontained = FALSE)

find_chromium_browser <- function() {
  env_chrome <- Sys.getenv("CHROMOTE_CHROME", unset = "")
  if (nzchar(env_chrome) && file.exists(env_chrome)) {
    return(env_chrome)
  }
  
  candidates <- c(
    Sys.which(c("google-chrome", "chromium", "chromium-browser", "brave-browser", "microsoft-edge")),
    "C:/Program Files/Google/Chrome/Application/chrome.exe",
    "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe",
    "C:/Program Files/BraveSoftware/Brave-Browser/Application/brave.exe",
    "C:/Program Files/Microsoft/Edge/Application/msedge.exe",
    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
    "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser",
    "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge"
  )
  candidates <- unique(candidates[nzchar(candidates) & file.exists(candidates)])
  if (length(candidates) > 0) candidates[[1]] else ""
}

browser_path <- find_chromium_browser()

if (nzchar(browser_path) && requireNamespace("webshot2", quietly = TRUE)) {
  Sys.setenv(CHROMOTE_CHROME = browser_path)
  png_ok <- tryCatch({
    webshot2::webshot(
      out_html,
      file = out_png,
      vwidth = 1440,
      vheight = 840,
      delay = 1.5
    )
    TRUE
  }, error = function(e) {
    message("PNG export skipped (webshot2/chromote error): ", conditionMessage(e))
    FALSE
  })
} else {
  message("No Chromium browser or 'webshot2' found; saved HTML widget without failing PNG export.")
}
