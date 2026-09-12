# --- Load packages ------------------------------------------------------------

library(janitor)

library(dplyr)
library(tidyr)

library(ggplot2)
library(showtext)
library(ggtext)

# --- Other setup --------------------------------------------------------------

data_week <- "2026-09-08"

# author details for figure caption
source("commons/Author_details.R")


# --- Get data -----------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(data_week)

cafe <- tuesdata$cafe
cappuccino_index <- tuesdata$cappuccino_index

# Replace non-breaking spaces in the data
cafe$country <- gsub("\u00a0", " ", cafe$country)
cappuccino_index$country <- gsub("\u00a0", " ", cappuccino_index$country)

# --- Data preprocesing --------------------------------------------------------

sum(is.na(cappuccino_index))

min(cappuccino_index$index)
max(cappuccino_index$index)

summary(cappuccino_index$n)

cappuccino_index$limited_data <- case_when(
  cappuccino_index$n < 6 ~ 1,
  cappuccino_index$n >= 6 ~ 0,
  !is.numeric(cappuccino_index$n) ~ NA
)

# max and min in each group
data_rich_min <- cappuccino_index[cappuccino_index$index == min(cappuccino_index[cappuccino_index$limited_data == 0, ]$index), ]
data_rich_max <- cappuccino_index[cappuccino_index$index == max(cappuccino_index[cappuccino_index$limited_data == 0, ]$index), ]

data_little_min <- cappuccino_index[cappuccino_index$index == min(cappuccino_index[cappuccino_index$limited_data == 1, ]$index), ]
data_little_max <- cappuccino_index[cappuccino_index$index == max(cappuccino_index[cappuccino_index$limited_data == 1, ]$index), ]

# median in each group
data_medians <- cappuccino_index |>
  group_by(limited_data) |>
  summarize(median_index = median(index))

# --- Plotting -----------------------------------------------------------------

font_add_google(name = "Plus Jakarta Sans", family = "plus_jakarta")
showtext_auto()


# Create standalone barcode plot
ggplot(cappuccino_index, aes(y = index)) +
  geom_segment(
    data = data_medians,
    aes(y = median_index, yend = median_index, x = -0.15, xend = 0.65),
    alpha = 0.85, color = "#C86D51", linewidth = 1
  ) +
  geom_segment(aes(yend = index, x = 0, xend = 0.5), alpha = 0.75, color = "#6F4E37") +
  coord_cartesian(expand = F, clip = "off") +
  scale_y_reverse(breaks = c(10, 50, 100, 200, 280), limits = c(0, 290)) +
  # median for each group
  # min and max for each group
  ggrepel::geom_text_repel(
    data = data_rich_min,
    aes(x = 0.65, y = index, label = country),
    min.segment.length = 0,
    nudge_x = 2,
    direction = "y",
    hjust = 1,
    segment.linetype = "dashed",
    colour = "#2B1E1A"
  ) +
  ggrepel::geom_text_repel(
    data = data_rich_max,
    aes(x = 0.65, y = index, label = country),
    min.segment.length = 0,
    nudge_x = 2,
    direction = "y",
    hjust = 1,
    segment.linetype = "dashed",
    colour = "#2B1E1A"
  ) +
  ggrepel::geom_text_repel(
    data = data_little_min,
    aes(x = 0.65, y = index, label = country),
    min.segment.length = 0,
    nudge_x = 2.5,
    direction = "y",
    hjust = 1,
    segment.linetype = "dashed",
    colour = "#2B1E1A"
  ) +
  ggrepel::geom_text_repel(
    data = data_little_max,
    aes(x = 0.65, y = index, label = country),
    min.segment.length = 0,
    nudge_x = 2.5,
    direction = "y",
    hjust = 1,
    segment.linetype = "dashed",
    colour = "#2B1E1A"
  ) +
  labs(title = "A cup of cappucino in 10 minutes or in hours") +
  facet_wrap(~limited_data, nrow = 1) +
  theme_void() +
  theme(
    plot.margin = margin(30, 10, 30, 10),
    panel.background = element_rect(fill = "#F5EFD6"),
    panel.spacing = unit(10, "pt"),
    plot.title.position = "plot",
    plot.title = element_text(family = "plus_jakarta"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.y = element_line(colour = "#2B1E1A"),
    axis.ticks.y = element_line(colour = "#2B1E1A"),
    axis.ticks.length.y = unit(5, "pt"),
    axis.text.y = element_text(colour = "#2B1E1A"),
    aspect.ratio = 3
  )


# --- Export figure ------------------------------------------------------------

ggsave(
  filename = fs::path("2026", data_week, paste0(gsub("-", "", data_week), "_plot.jpg")),
  plot = last_plot(), device = "jpg", width = 1200, height = 900, units = "px", dpi = 300
)
