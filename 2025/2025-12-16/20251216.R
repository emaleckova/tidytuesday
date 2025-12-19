library(tidytuesdayR)
library(dplyr)
library(ggplot2)

# Get data with tidytuesdayR
tuesdata <- tidytuesdayR::tt_load('2025-12-16')
roundabouts_clean <- tuesdata$roundabouts_clean

# Make year a factor and leave out all roundabouts with unknown completion date
roundabouts_completed <- roundabouts_clean %>% 
  filter(year_completed != 0)


roundabouts_cum <- roundabouts_completed %>% 
  group_by(country, year_completed) %>%
  summarize(n_rbouts = n()) %>% 
  arrange(year_completed) %>% 
  mutate(cum_rbouts = cumsum(n_rbouts))

ggplot(roundabouts_cum, aes(x = year_completed, y = cum_rbouts,
                            group = country)) +
  geom_col(aes(colour = country)) +
  facet_wrap(~country, scales = "free_y") +
  coord_cartesian(expand = F) +
  scale_y_continuous(breaks = seq(min(roundabouts_cum$year_completed),
                                  max(roundabouts_cum$year_completed), length.out = 5)) +
  theme_classic() +
  theme(strip.background = element_blank(),
        legend.position = "none")
