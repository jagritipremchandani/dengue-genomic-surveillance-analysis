
library(tidyverse)
library(ggplot2)
library(plotly)
library(htmlwidgets)
library(scales)

# ==========================================================
# 1. PROJECT ROOT
# ==========================================================
project_dir<-"outputs"


# ==========================================================
# 2. FILE PATHS
# ==========================================================

csv_path <- file.path(
  project_dir,
  "figure_csvs",
  "serotype_temporal_growth.csv"
)

out_dir <- file.path(
  project_dir,
  "figures",
  "dengue"
)


# ==========================================================
# 3. LOAD DATA
# ==========================================================

df <- read_csv(
  csv_path,
  show_col_types = FALSE
) %>%
  mutate(
    collection_year = as.numeric(collection_year),
    n_sources = as.numeric(n_sources),
    serotype = trimws(as.character(serotype))
  )


# Preserve unresolved DENV observations

unresolved_df <- df %>%
  filter(serotype == "DENV")

message(
  "Unresolved DENV observations: ",
  format(
    sum(unresolved_df$n_sources, na.rm = TRUE),
    big.mark = ","
  )
)


# ==========================================================
# 4. SEROTYPE LEVELS AND COLOURS
# ==========================================================

serotype_levels <- c(
  "DENV-1",
  "DENV-2",
  "DENV-3",
  "DENV-4"
)

serotype_palette <- c(
  "DENV-1" = "#2878B5",
  "DENV-2" = "#E87532",
  "DENV-3" = "#26965C",
  "DENV-4" = "#C94350"
)


# ==========================================================
# 5. FUNCTION TO GENERATE FIGURES
# ==========================================================

create_serotype_figure <- function(
    
  start_year,
  end_year,
  tick_interval,
  
  figure_title,
  figure_subtitle,
  
  output_stem,
  
  compress_early = FALSE,
  compression_factor = 0.25,
  
  fig_width = 14,
  fig_height = 7.5
  
) {
  
  # --------------------------------------------------------
  # 5A. X-AXIS TRANSFORMATION
  # --------------------------------------------------------
  
  transform_year <- function(year) {
    
    if (compress_early) {
      
      ifelse(
        year <= 1945,
        
        (year - 1905) * compression_factor,
        
        (1945 - 1905) * compression_factor +
          (year - 1945)
      )
      
    } else {
      
      year
      
    }
  }
  
  
  # --------------------------------------------------------
  # 5B. FILTER DATA
  # --------------------------------------------------------
  
  plot_df <- df %>%
    
    filter(
      serotype %in% serotype_levels,
      collection_year >= start_year,
      collection_year <= end_year
    ) %>%
    
    mutate(
      serotype = factor(
        serotype,
        levels = serotype_levels
      ),
      
      x_position = transform_year(collection_year)
    ) %>%
    
    arrange(
      serotype,
      collection_year
    )
  
  
  if (nrow(plot_df) == 0) {
    
    stop(
      paste(
        "No observations found for",
        start_year,
        "to",
        end_year
      )
    )
  }
  
  
  # --------------------------------------------------------
  # 5C. X-AXIS BREAKS
  # --------------------------------------------------------
  
  if (compress_early) {
    
    # Fewer labels in the compressed historical section.
    # Retain 10-year intervals in the expanded section.
    
    x_breaks <- c(
      1905,
      1925,
      1945,
      seq(1955, end_year, by = 10)
    )
    
  } else {
    
    x_breaks <- seq(
      start_year,
      end_year,
      by = tick_interval
    )
    
  }
  
  
  x_break_positions <- transform_year(x_breaks)
  
  
  # --------------------------------------------------------
  # 5D. CREATE GGPLOT
  # --------------------------------------------------------
  
  p <- ggplot(
    
    plot_df,
    
    aes(
      x = x_position,
      y = n_sources,
      
      colour = serotype,
      group = serotype,
      
      text = paste0(
        "<b>Serotype:</b> ", serotype, "<br>",
        "<b>Collection Year:</b> ", collection_year, "<br>",
        "<b>Source Observations:</b> ",
        format(n_sources, big.mark = ",")
      )
    )
    
  ) +
    
    # Connecting lines
    
    geom_line(
      linewidth = 0.9,
      na.rm = TRUE
    ) +
    
    # Observation markers
    
    geom_point(
      size = 2.2,
      shape = 16,
      na.rm = TRUE
    ) +
    
    # Colour palette
    
    scale_colour_manual(
      values = serotype_palette,
      breaks = serotype_levels,
      drop = FALSE,
      name = "Serotype"
    ) +
    
    # X-axis
    
    scale_x_continuous(
      
      breaks = x_break_positions,
      
      labels = x_breaks,
      
      limits = transform_year(
        c(start_year, end_year)
      ),
      
      expand = expansion(
        mult = c(0.025, 0.025)
      )
      
    ) +
    
    # Linear Y-axis
    
    scale_y_continuous(
      
      breaks = scales::breaks_pretty(n = 7),
      
      labels = scales::label_comma(),
      
      expand = expansion(
        mult = c(0, 0.06)
      )
      
    ) +
    
    # Titles and captions
    
    labs(
      
      title = figure_title,
      
      subtitle = figure_subtitle,
      
      x = "Collection Year",
      
      y = "Number of Source Observations",
      
      caption = if (compress_early) {
        
        paste(
          "Temporal distribution of source-level dengue genomic observations",
          "assigned to dengue serotypes 1–4.",
          "Counts represent Source Inventory observations.",
          "Note: Horizontal scale for 1905–1945 is compressed."
        )
        
      } else {
        
        paste(
          "Temporal distribution of source-level dengue genomic observations",
          "assigned to dengue serotypes 1–4.",
          "Counts represent Source Inventory observations."
        )
        
      }
      
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
      
      # Gridlines
      
      panel.grid.major.x = element_blank(),
      
      panel.grid.major.y = element_line(
        colour = "#E5E7EB",
        linewidth = 0.45
      ),
      
      panel.grid.minor = element_blank(),
      
      # Axis
      
      axis.line.x = element_line(
        colour = "#CBD5E1",
        linewidth = 0.6
      ),
      
      axis.ticks.x = element_line(
        colour = "#CBD5E1"
      ),
      
      axis.title = element_text(
        face = "bold",
        size = 11,
        colour = "#1F2937"
      ),
      
      axis.text = element_text(
        colour = "#374151",
        size = 10
      ),
      
      axis.text.x = element_text(
        margin = margin(t = 6)
      ),
      
      # Title
      
      plot.title = element_text(
        face = "bold",
        size = 15,
        colour = "#111827",
        margin = margin(b = 8)
      ),
      
      plot.subtitle = element_text(
        size = 10.5,
        colour = "#64748B",
        margin = margin(b = 12)
      ),
      
      # Caption
      
      plot.caption = element_text(
        size = 8.5,
        colour = "#64748B",
        hjust = 0,
        lineheight = 1.2,
        margin = margin(t = 14)
      ),
      
      # Legend
      
      legend.position = "right",
      
      legend.title = element_text(
        face = "bold",
        size = 10
      ),
      
      legend.text = element_text(
        size = 10
      ),
      
      legend.margin = margin(l = 14),
      
      # Plot margins
      
      plot.margin = margin(
        18, 24, 18, 18
      )
      
    )
  
  
  # --------------------------------------------------------
  # 5E. SAVE PNG
  # --------------------------------------------------------
  
  out_png <- file.path(
    out_dir,
    paste0(output_stem, ".png")
  )
  
  ggsave(
    
    filename = out_png,
    
    plot = p,
    
    width = fig_width,
    
    height = fig_height,
    
    dpi = 600,
    
    bg = "white"
    
  )
  
  
  # --------------------------------------------------------
  # 5F. CREATE INTERACTIVE PLOTLY FIGURE
  # --------------------------------------------------------
  
  interactive_plot <- ggplotly(
    p,
    tooltip = "text"
  ) %>%
    
    layout(
      
      hovermode = "closest",
      
      legend = list(
        
        title = list(
          text = "Serotype"
        ),
        
        orientation = "v",
        
        x = 1.03,
        
        xanchor = "left",
        
        y = 1,
        
        yanchor = "top"
        
      ),
      
      plot_bgcolor = "white",
      
      paper_bgcolor = "white",
      
      # X-axis
      
      xaxis = list(
        
        title = list(
          text = "Collection Year",
          standoff = 18
        ),
        
        range = transform_year(
          c(start_year - 1.5, end_year + 1.5)
        ),
        
        tickmode = "array",
        
        tickvals = x_break_positions,
        
        ticktext = as.character(x_breaks),
        
        showgrid = FALSE,
        
        showline = TRUE,
        
        linecolor = "#CBD5E1",
        
        ticks = "outside",
        
        tickcolor = "#CBD5E1",
        
        automargin = TRUE
        
      ),
      
      # Y-axis
      
      yaxis = list(
        
        title = list(
          text = "Number of Source Observations",
          standoff = 15
        ),
        
        showgrid = TRUE,
        
        gridcolor = "#E5E7EB",
        
        zeroline = FALSE,
        
        showline = FALSE,
        
        automargin = TRUE
        
      ),
      
      # Margins
      
      margin = list(
        
        l = 90,
        
        r = 160,
        
        t = 70,
        
        b = 95
        
      )
      
    ) %>%
    
    config(
      
      displayModeBar = TRUE,
      
      displaylogo = FALSE,
      
      responsive = TRUE
      
    )
  
  
  # --------------------------------------------------------
  # 5G. SAVE INTERACTIVE HTML
  # --------------------------------------------------------
  
  out_html <- file.path(
    out_dir,
    paste0(output_stem, ".html")
  )
  
  htmlwidgets::saveWidget(
    
    widget = interactive_plot,
    
    file = out_html,
    
    selfcontained = TRUE
    
  )
  
  
  # --------------------------------------------------------
  # 5H. PRINT OUTPUT LOCATIONS
  # --------------------------------------------------------
  
  message("\n----------------------------------")
  
  message(
    "Figure generated: ",
    start_year,
    "–",
    end_year
  )
  
  message(
    "PNG: ",
    normalizePath(out_png, mustWork = FALSE)
  )
  
  message(
    "HTML: ",
    normalizePath(out_html, mustWork = FALSE)
  )
  
  message("----------------------------------\n")
  
  
  invisible(
    
    list(
      
      plot = p,
      
      interactive = interactive_plot,
      
      png = out_png,
      
      html = out_html
      
    )
    
  )
  
}


# ==========================================================
# 6. FIGURE 03B: HISTORICAL PERIOD (1905–2005)
# ==========================================================

historical_figure <- create_serotype_figure(
  
  start_year = 1905,
  
  end_year = 2005,
  
  tick_interval = 10,
  
  figure_title = "Historical Distribution of Dengue Genomic Surveillance by Serotype",
  
  figure_subtitle = "Source-level genomic observations by collection year, 1905–2005",
  
  output_stem = "figure_03B_serotype_trends_historical",
  
  compress_early = TRUE,
  
  compression_factor = 0.25,
  
  fig_width = 14,
  
  fig_height = 7.5
  
)


# ==========================================================
# 7. FIGURE 03C: CONTEMPORARY PERIOD (2005–2025)
# ==========================================================

contemporary_figure <- create_serotype_figure(
  
  start_year = 2005,
  
  end_year = 2025,
  
  tick_interval = 5,
  
  figure_title = "Contemporary Expansion of Dengue Genomic Surveillance by Serotype",
  
  figure_subtitle = "Source-level genomic observations by collection year, 2005–2025",
  
  output_stem = "figure_03C_serotype_trends_contemporary",
  
  compress_early = FALSE,
  
  fig_width = 14,
  
  fig_height = 7.5
  
)


# ==========================================================
# 8. COMPLETION MESSAGE
# ==========================================================

message(
  "\nBoth historical and contemporary figures generated successfully."
)

message(
  "Output directory: ",
  normalizePath(out_dir, mustWork = FALSE)
)
