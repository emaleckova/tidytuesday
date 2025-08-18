# --- Load packages ------------------------------------------------------------

library(fs)

library(dplyr)
library(rnaturalearth)
library(sf)
library(ggplot2)
library(ggwordcloud)

library(ggtext)

data_week <- "2025-08-12"

# --- Get data -----------------------------------------------------------------
attr_data <- read.delim(fs::path("2025", "data", data_week, "attribution_studies.csv", header = T, sep = ","))


# --- Data preprocesing --------------------------------------------------------

plot_data <- attr_data |>
  group_by(event_type) |>
  summarize(n_occurences = n()) |>
  mutate(event_type = case_when(
    event_type == "Cold, snow & ice" ~ "Snow",
    event_type == "Rain & flooding" ~ "Floods",
    TRUE ~ event_type
  ))

# combination of Autumn and Tq Dark palettes
all_types <- sort(unique(plot_data$event_type))
print(all_types)

event_colours <- c(
  "#EAC862FF", "#00000000", "#FF9D1EFF", "#007ED3FF", "#800000FF", "#755028FF",
  "#0055AAFF", "#1B3A54FF", "#7FD2FFFF", "#00C19BFF", "#BA6E1DFF", "#C40003FF"
)

names(event_colours) <- all_types

plot_data <- left_join(plot_data, data.frame(event_colours, event_type = names(event_colours)))

# Event years are of type character and may be multiple years or time ranges
# Therefore, obtain individual years to detect the time of range for the entire data set
event_years <- na.omit(as.numeric(unlist(strsplit(attr_data$event_year, "-|, "))))

year_first <- min(event_years)
year_last <- max(event_years)

# --- Plot ---------------------------------------------------------------------

# define font
font_add_google(name = "Spectral", family = "spectral")

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_sf(data = world, fill = "grey95", colour = "grey95", alpha = 0.6) +
  coord_sf(expand = F) +
  geom_text_wordcloud(data = plot_data, aes(label = event_type, size = n_occurences, colour = event_type,
                                            angle = c(90, rep(0, 4), 90, rep(0, 5), 90)),
                      seed = 2, grid_size = 8, max_grid_size = 1280, shape = "circle", family = "spectral") +
  scale_colour_manual(values = event_colours) +
  scale_size_area(max_size = 25) +
  labs(
    title = paste("Extreme Weather Attribution Studies \n", year_first, "to", year_last),
    caption = paste(
      "**Data:** Carbon Brief <br> ",
      "**Graphic:** ", social_caption)) +
  theme_void() +
  theme(plot.margin = margin(30, 30, 30, 30),
        plot.title = element_text(hjust = 0.5, size = 18, margin = margin(0, 0, 15, 0)),
        plot.caption = element_textbox_simple(
          size = 10,
          margin = margin(10, 0, 0, 0),
          lineheight = 1.75),
        text = element_text(family = "spectral"))

ggsave(filename = fs::path("2025", data_week, paste0(gsub("-", "", data_week), "_plot.png")),
       plot = last_plot(), device = "png", width = 7, height = 5, units = "in", dpi = 300)

