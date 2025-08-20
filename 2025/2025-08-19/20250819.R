# --- Load packages ------------------------------------------------------------

library(fs)
library(janitor)

library(dplyr)
library(tidyr)

library(ggplot2)
library(ggsankey)
library(showtext)
library(ggtext)
library(patchwork)

# --- Other setup --------------------------------------------------------------

data_week <- "2025-08-19"

# author details for figure caption
source("commons/Author_details.R")


# --- Get data -----------------------------------------------------------------
munros_data <- read.delim(fs::path("data", data_week, "scottish_munros.csv"), header = T, sep = ",")
munros_data <- janitor::clean_names(munros_data)

# remove a "note row"
munros_data <- munros_data[!is.na(munros_data$name), ]


# --- Data preprocesing --------------------------------------------------------

# each year has a devoted column - get them
year_columns <- c(colnames(munros_data)[grepl("x\\d+", colnames(munros_data))])


# get number of munros and munro tops in each year
plot_munro_counts <- munros_data |> 
  pivot_longer(cols = year_columns, names_to = "year", values_to = "type") |> 
  filter(!is.na(type)) |> 
  group_by(year) |> 
  summarize(total_munros = length(unique(name)))


# prepare data for Sankey plot - flow per Munro (top)
plot_sankey <- munros_data |> 
  mutate(across(matches("x\\d+"), ~ replace(., is.na(.), "unassigned"))) |> 
  # for sankey
  make_long(all_of(year_columns))


# --- Plotting -----------------------------------------------------------------

font_add_google(name = "Maiden Orange", family = "maideno")

# fill/colour scale
col_type <- c("#3B7C70FF", "#CE9642FF", "#3B3A3EFF")
names(col_type) <- c("Munro Top", "Munro", "unassigned")


# number of Munros and Munro Tops in each year
p_counts <- ggplot(plot_munro_counts, aes(x = year, y = total_munros)) +
  geom_col(fill = "#898E9FFF", width = 0.15) + 
  annotate("segment", x = "x1891", y = 490, xend = "x2021", yend = 490, linetype = "dashed", linewidth = 0.2) +
  annotate(geom = "text", x = "x1997", y = min(plot_munro_counts$total_munros) + 70,
           label = paste(min(plot_munro_counts$total_munros), "Munros in 2021"),
           size = unit(4, "pt"), family = "maideno") +
  scale_x_discrete(expand = expansion(0.1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 600)) +
  theme_void() + 
  theme(plot.margin = margin(0, 10, 0, 10),
        axis.text = element_blank(),
        axis.title = element_blank(),
        text = element_text(family = "maideno"),
        aspect.ratio = 0.1)

# A Sankey plot - transitions year to year
p_sankey <- ggplot(plot_sankey, aes(x = x, 
                      next_x = next_x, 
                      node = node, 
                      next_node = next_node,
                      fill = factor(node))) +
  geom_sankey() +
  scale_fill_manual(values = col_type) +
  # this annotation is to replace legend
  annotate(geom = "text", x = "x1997", y = 322, label = "unassigned",
           colour = "white", size = unit(3, "pt")) +
  scale_x_discrete(labels = gsub("x", "", year_columns), expand = expansion(0.1)) +
  theme_void() +
  theme(plot.margin = margin(0, 10, 11, 10),
        axis.title.x = element_blank(),
        axis.text.x = element_text(vjust = -0.5),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_line(linewidth = 0.2),
        axis.ticks.length.x = unit(2, "pt"),
        legend.title = element_blank(),
        legend.title.position = "top",
        text = element_text(family = "maideno"),
        aspect.ratio = 0.25,
        legend.position = "none")

# Plot tile will include some background information about Munros and encoded colour legend
assembly_title <- glue::glue(
  "<span style='font-size:28px;'>Scottish Munros</span>
  <br>
  <br>
  <span style='font-size:20px;'>A <span style='color:{col_type[2]};'>Munro</span> <span>is a Scottish mountain with an elevation of over 3,000 feet,
  whereas a <span style='color:{col_type[1]};'>Munro Top</span><span> is a subsidiary summit of a Munro
  <br>that also exceeds 3,000 feet in height but is not considered a distinct mountain in its own right.
  <br>
  Since Munros lack a rigid set of criteria for inclusion, re-surveying can lead to changes in which peaks are included on the list.
  </span>
  <br>"
)


p_counts / p_sankey +
  plot_annotation(title = assembly_title,
                  caption = paste(
                    "**Data:** The Database of British and Irish Hills v18.2 <br> ",
                    "**Graphic:** ", social_caption),
                  theme = theme(
                    plot.title = element_markdown(hjust = 0.5, lineheight = 0.35, family = "maideno"),
                    plot.caption = element_textbox_simple(
                      size = 14,
                      margin = margin(5, 0, 0, 0),
                      lineheight = 0.5)))


# --- Export figure ------------------------------------------------------------

ggsave(filename = fs::path("2025", data_week, paste0(gsub("-", "", data_week), "_plot.jpg")),
       plot = last_plot(), device = "jpg", width = 1200, height = 900, units = "px", dpi = 300)

