library(tidyverse)
library(ggplot2)
library(plotly)
library(htmlwidgets)


project_dir <- "outputs"

csv_path <- file.path(
  project_dir,
  "figure_csvs",
  "repository_representation.csv"
)

out_dir <- file.path(project_dir,"figures", "dengue")

out_html <- file.path(out_dir, "figure_06_repository_representation.html")
out_png  <- file.path(out_dir, "figure_06_repository_representation.png")

repo_levels <- c("NCBI_ONLY", "ENA_ONLY", "BOTH")

repo_colors <- c(
  "BOTH"      = "#1B7837",
  "ENA_ONLY"  = "#E67E22",
  "NCBI_ONLY" = "#2563EB"
)

caption_text <- paste(
  "Source-level observations were classified according to whether they were represented in both repositories (BOTH)",
  "or were unique to ENA (ENA_ONLY) or NCBI (NCBI_ONLY). Approximately two-thirds of source observations were represented",
  "in both repositories, while one-third were archive-specific. Representation was evaluated using the final source-key",
  "framework rather than accession-level records."
)

raw_df <- read_csv(csv_path, show_col_types = FALSE)

df <- raw_df %>%
  mutate(
    repo_cat = toupper(pick_col(., c("repository_representation", "repo_status", "representation", "category", "archive_status", "status"))),
    n_sources = as.numeric(pick_col(., c("n_sources", "source_units", "n_obs", "source_count", "count", "sources")))
  ) %>%
  filter(
    repo_cat %in% c("BOTH", "ENA_ONLY", "NCBI_ONLY"),
    !is.na(n_sources),
    n_sources >= 0
  ) %>%
  group_by(repo_cat) %>%
  summarise(n_sources = sum(n_sources, na.rm = TRUE), .groups = "drop") %>%
  complete(repo_cat = c("BOTH", "ENA_ONLY", "NCBI_ONLY"), fill = list(n_sources = 0)) %>%
  mutate(
    total_sources = sum(n_sources),
    pct = ifelse(total_sources > 0, 100 * n_sources / total_sources, 0),
    pct_label = paste0(formatC(pct, format = "f", digits = 1), "%"),
    repo_cat = factor(repo_cat, levels = repo_levels)
  )

max_x <- max(df$n_sources, na.rm = TRUE)
label_gap <- max_x * 0.025

df <- df %>%
  mutate(label_x = n_sources + label_gap)

p <- ggplot(
  df,
  aes(
    x = n_sources,
    y = repo_cat,
    fill = repo_cat,
    text = paste0(
      "<b>Repository Representation:</b> ", repo_cat, "<br>",
      "<b>Source Units:</b> ", format(round(n_sources), big.mark = ","), "<br>",
      "<b>Share:</b> ", pct_label
    )
  )
) +
  geom_col(
    width = 0.56,
    colour = "#1E293B",
    linewidth = 0.4,
    alpha = 0.95
  ) +
  geom_text(
    aes(x = label_x, label = pct_label),
    hjust = 0,
    size = 3.5,
    fontface = "bold",
    colour = "#111827"
  ) +
  scale_fill_manual(
    values = repo_colors,
    guide = "none"
  ) +
  scale_x_continuous(
    labels = scales::comma,
    limits = c(0, max_x * 1.22),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Repository Representation of Dengue Genomic Surveillance Sources",
    subtitle = "Distribution of source-level observations across ENA and NCBI archives",
    x = "Source Units (Number of Source-Level Observations)",
    y = "Repository Representation",
    caption = caption_text
  ) +
  theme_minimal(base_size = 11.5) +
  theme(
    panel.background = element_rect(fill = "#FFFFFF", colour = NA),
    plot.background = element_rect(fill = "#FFFFFF", colour = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "#E5E7EB", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    axis.line.y = element_line(colour = "#CBD5E1", linewidth = 0.6),
    axis.ticks.y = element_line(colour = "#CBD5E1"),
    plot.title = element_text(face = "bold", size = 14.5, colour = "#111827"),
    plot.subtitle = element_text(size = 11, colour = "#4B5563"),
    plot.caption = element_text(size = 9.5, colour = "#4B5563", hjust = 0, margin = margin(t = 12)),
    axis.title = element_text(face = "bold", size = 10.5, colour = "#1F2937"),
    axis.text.y = element_text(size = 10.5, colour = "#374151"),
    axis.text.x = element_text(size = 9.5, colour = "#374151"),
    plot.margin = margin(t = 15, r = 35, b = 20, l = 15)
  )

ggsave(
  filename = out_png,
  plot = p,
  width = 11.5,
  height = 5.8,
  dpi = 600
)

interactive_plot <- ggplotly(p, tooltip = "text") %>%
  layout(
    yaxis = list(tickfont = list(size = 9.5)),
    margin = list(l = 95, r = 55, t = 85, b = 115),
    annotations = list(
      list(
        text = caption_text,
        x = 0.0,
        y = -0.26,
        xref = "paper",
        yref = "paper",
        xanchor = "left",
        yanchor = "top",
        align = "left",
        showarrow = FALSE,
        font = list(size = 10, color = "#4B5563")
      )
    )
  )

saveWidget(
  widget = interactive_plot,
  file = out_html,
  selfcontained = TRUE
)

html_uri <- paste0("file:///", gsub("\\\\", "/", normalizePath(out_html, mustWork = FALSE)))
message("Interactive HTML saved at: ", normalizePath(out_html, mustWork = FALSE))
message("Browser link: ", html_uri)

chrome_paths <- c(
  "C:/Program Files/Google/Chrome/Application/chrome.exe",
  "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe"
)
chrome_exe <- chrome_paths[file.exists(chrome_paths)][1]

if (!is.na(chrome_exe)) {
  system2(chrome_exe, args = shQuote(html_uri), wait = FALSE)
} else {
  browseURL(html_uri)
}
