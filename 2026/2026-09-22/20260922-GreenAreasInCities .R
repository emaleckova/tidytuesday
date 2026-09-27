# --- Load packages ------------------------------------------------------------

library(janitor)

library(dplyr)
library(stringr)

library(ggplot2)
library(showtext)
library(ggtext)

# --- Other setup --------------------------------------------------------------

data_week <- "2026-09-22"
week_nr <- 38

# author details for figure caption
source("commons/Author_details.R")


# --- Get data -----------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2026, week = 38)

urban <- tuesdata$urban
urban <- janitor::clean_names(urban)

# --- Data preprocesing --------------------------------------------------------

glimpse(urban)

unique(urban$sdg_region)
unique(urban$sdg_sub_region)

eur_capitals <- read.delim("data/2026-09-22/european-countries-and-capitals.csv",
                           sep = ",", header = T)

urban_eur_capitals <- urban |> 
  # Keep European capitals
  filter(grepl("Europe", sdg_sub_region)) |> 
  filter(grepl(paste(eur_capitals$capital, collapse = "|"), city_name)) |> 
  mutate(city_label = stringr::str_remove(city_name, pattern = "\\s*\\([^\\)]+\\)")) |> 
  arrange(city_name, year) |> 
  group_by(city_name) |> 
  # Calculate connections for sequence of arrows
  mutate(
    x_start = lag(year),
    y_start = lag(average_share_of_green_area_in_city_urban_area_pct),
    x_end   = year,
    y_end   = average_share_of_green_area_in_city_urban_area_pct,
    # Calculate difference and assign color rule
    diff = y_end - y_start,
    arrow_color = if_else(diff <= -5, "#09C810", if_else(diff >= 5, "#057009", "#8DA08D"))
  ) |> 
  # Drop NAs - first row of each group which has no start point
  filter(!is.na(x_start))

# --- Plotting -----------------------------------------------------------------

# New fonts
sysfonts::font_add_google(name = "Chango", family = "chango")
sysfonts::font_add_google(name = "Nunito", family = "nunito")
showtext_auto()

# Title & subtitle
plot_title <- c(
  "30 years of green urban area evolution in 33 European capitals"
)

plot_subtitle <- c("Green areas are green for most parts of the year and were detected based on satellite image analysis.<br>Decade-to-decade <span style='color: #09C810;'>**declines**</span> and <span style='color: #057009;'>**increases**</span> of five percentage points or larger are highlighted.<br>Data from 1990 to 2020 is included.")

# Caption text
plot_caption <- paste0(
  paste0("**TidyTuesday**", "  2026, week ", week_nr, "<br>"),
  c("**Data source:** UN-Habitat Urban Indicators Database<br>"),
  social_caption
)

ggplot(
  urban_eur_capitals,
  aes(x = year, y = shareaverage_share_of_green_area_in_city_urban_area_pct_frxn)
) +
  geom_segment(
    aes(
      x = x_start, y = y_start,
      xend = x_end, yend = y_end,
      color = arrow_color
    ),
    arrow = arrow(length = unit(3.5, "pt"), type = "closed"),
    linewidth = 0.8
  ) +
  # Directly use colours defined in the data frame
  scale_color_identity() +
  facet_wrap(~city_label, nrow = 4, scales = "free_x") +
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    caption = plot_caption
  ) +
  scale_x_continuous(
    breaks = c(1990, 2000, 2010, 2020),
    labels = function(x) paste0("'", sprintf("%02d", x %% 100)),
    expand = expansion(add = c(2, 0))
    ) +
  scale_y_continuous(labels = scales::label_number(suffix = "%")) +
  theme_void() +
  theme(
    text = element_text(family = "nunito"),
    panel.background = element_rect(fill = "grey95"),
    plot.background = element_rect(fill = "#8DA08D"),
    plot.margin = margin(0, 10, 10, 10),
    panel.spacing.x = unit(5, "pt"),
    plot.title = element_text(
      family = "chango",
      size = 14,
      colour = "#057009",
      margin = margin(10, 0, 5, 0)
      ),
    plot.subtitle = element_markdown(
      size = 10,
      lineheight = 1.25,
      margin = margin(5, 0, 10, 0)
      ),
    plot.caption = element_markdown(
      size = 8,
      lineheight = 1.25,
      hjust = 1
      ),
    strip.background = element_rect(fill = "grey95"),
    strip.text = element_text(
      size = 8, margin = margin(2, 0, 2, 0),
      face = "bold"
      ),
    panel.grid.major.y = element_line(colour = "grey80", linewidth = 0.25),
    axis.text.x = element_text(size = 8, margin = margin(5, 0, 0, 0)),
    axis.text.y = element_text(size = 8, margin = margin(0, 5, 0, 0)),
    aspect.ratio = 0.65
    )
