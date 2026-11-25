
library(tidyverse)
library(ggplot2)
library(plotly)
library(htmlwidgets)

get_project_root <- function() {
  script_path <- NULL
  
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    ctx <- tryCatch(
      rstudioapi::getActiveDocumentContext(),
      error = function(e) NULL
    )
    
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
    script_dir <- dirname(normalizePath(
      script_path,
      mustWork = FALSE
    ))
    
    if (basename(script_dir) == "scripts") {
      return(dirname(script_dir))
    }
    
    return(script_dir)
  }
  
  wd <- normalizePath(getwd(), mustWork = FALSE)
  
  if (basename(wd) == "scripts") {
    return(dirname(wd))
  }
  
  wd
}

project_dir <- get_project_root()

csv_path <- file.path(
  project_dir,
  "outputs",
  "figure_csvs",
  "temporal_surveillance_growth.csv"
)

out_dir <- file.path(
  project_dir,
  "outputs",
  "figures",
  "dengue"
)


out_html <- file.path(
  out_dir,
  "figure_03A_surveillance_growth.html"
)

out_png <- file.path(
  out_dir,
  "figure_03A_surveillance_growth.png"
)

# ----------------------------------------------------------
# 1. LOAD AND PREPARE DATA
# ----------------------------------------------------------

df <- read_csv(
  csv_path,
  show_col_types = FALSE
) %>%
  mutate(
    count_tier = cut(
      n_sources,
      breaks = c(-Inf, 100, 1000, 3000, 5000, Inf),
      labels = c(
        "<= 100 (n = 34)",
        "101–1,000 (n = 12)",
        "1,001–3,000 (n = 10)",
        "3,001–5,000 (n = 5)",
        "> 5,000 (n = 5)"
      ),
      right = TRUE
    )
  ) %>%
  arrange(collection_year)

# ----------------------------------------------------------
# 2. COLOUR PALETTE
# ----------------------------------------------------------

tier_palette <- c(
  "<= 100 (n = 34)"       = "#7048E8",
  "101–1,000 (n = 12)"    = "#00A6A6",
  "1,001–3,000 (n = 10)"  = "#F2B705",
  "3,001–5,000 (n = 5)"   = "#F06424",
  "> 5,000 (n = 5)"       = "#D7264D"
)

# ----------------------------------------------------------
# 3. CREATE PLOT
# ----------------------------------------------------------

p <- ggplot(
  df,
  aes(
    x = collection_year,
    y = n_sources,
    
    text = paste0(
      "<b>Collection Year:</b> ", collection_year, "<br>",
      "<b>Source Observations:</b> ",
      format(n_sources, big.mark = ","), "<br>",
      "<b>Observation Tier:</b> ", count_tier
    )
  )
) +
  
  # Connecting line
  geom_line(
    aes(group = 1),
    linewidth = 0.85,
    colour = "#64748B",
    alpha = 0.75
  ) +
  
  # Points: size now increases with source observations
  geom_point(
    aes(
      fill = count_tier,
      size = n_sources
    ),
    shape = 21,
    colour = "#334155",
    stroke = 0.3,
    alpha = 0.96
  ) +
  
  # Colour categories
  scale_fill_manual(
    values = tier_palette,
    name = "Source Observations",
    drop = FALSE
  ) +
  
  # IMPORTANT: Variable dot sizes
  scale_size_continuous(
    range = c(1.7, 7.1),
    trans = "sqrt",
    guide = "none"
  ) +
  
  # X-axis
  scale_x_continuous(
    breaks = seq(1905, 2025, by = 15),
    expand = expansion(mult = c(0.04, 0.04))
  ) +
  
  # Y-axis
  scale_y_continuous(
    breaks = seq(0, 7000, by = 1000),
    expand = expansion(mult = c(0.02, 0.06))
  ) +
  
  # Titles and labels
  labs(
    title = "Temporal Growth of Dengue Genomic Surveillance",
    
    subtitle = paste(
      "Source-level genomic observations by collection year",
      "(73 years: low early counts, rising post-2020)"
    ),
    
    x = "Collection Year",
    y = "Number of Source Observations"
  ) +
  
  # Theme
  theme_minimal(base_size = 12) +
  
  theme(
    panel.background = element_rect(
      fill = "#FFFFFF",
      colour = NA
    ),
    
    plot.background = element_rect(
      fill = "#FFFFFF",
      colour = NA
    ),
    
    panel.grid.major = element_line(
      colour = "#E5E7EB",
      linewidth = 0.5
    ),
    
    panel.grid.minor = element_blank(),
    
    axis.line.x = element_line(
      colour = "#CBD5E1",
      linewidth = 0.6
    ),
    
    axis.ticks.x = element_line(
      colour = "#CBD5E1"
    ),
    
    plot.title = element_text(
      face = "bold",
      size = 15,
      colour = "#111827"
    ),
    
    plot.subtitle = element_text(
      size = 11.5,
      colour = "#4B5563"
    ),
    
    axis.title = element_text(
      face = "bold",
      size = 11,
      colour = "#1F2937"
    ),
    
    axis.text = element_text(
      colour = "#374151"
    ),
    
    legend.position = "right",
    
    legend.title = element_text(
      face = "bold",
      size = 11
    ),
    
    legend.text = element_text(
      size = 10.5,
      colour = "#374151"
    ),
    
    legend.key = element_rect(
      fill = "#FFFFFF",
      colour = NA
    ),
    
    plot.margin = margin(
      t = 12,
      r = 15,
      b = 10,
      l = 10
    )
  )

# ----------------------------------------------------------
# 4. SAVE STATIC FIGURE
# ----------------------------------------------------------

ggsave(
  filename = out_png,
  plot = p,
  width = 12,
  height = 6,
  dpi = 600,
  bg = "white"
)

# ----------------------------------------------------------
# 5. CREATE INTERACTIVE VERSION
# ----------------------------------------------------------

interactive_plot <- ggplotly(
  p,
  tooltip = "text"
) %>%
  layout(
    hoverlabel = list(
      bgcolor = "white",
      bordercolor = "#CBD5E1",
      font = list(
        color = "#1F2937",
        size = 12
      )
    )
  )

saveWidget(
  widget = interactive_plot,
  file = out_html,
  selfcontained = TRUE
)

# ----------------------------------------------------------
# 6. OPEN OUTPUT
# ----------------------------------------------------------

html_uri <- paste0(
  "file:///",
  gsub(
    "\\\\",
    "/",
    normalizePath(out_html, mustWork = FALSE)
  )
)

message(
  "Interactive HTML saved at: ",
  normalizePath(out_html, mustWork = FALSE)
)

chrome_paths <- c(
  "C:/Program Files/Google/Chrome/Application/chrome.exe",
  "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe"
)

chrome_exe <- chrome_paths[file.exists(chrome_paths)][1]

if (length(chrome_exe) > 0 && !is.na(chrome_exe)) {
  system2(
    chrome_exe,
    args = shQuote(html_uri),
    wait = FALSE
  )
} else {
  browseURL(html_uri)
}
