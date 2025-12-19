# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2025-12-16')
## OR
tuesdata <- tidytuesdayR::tt_load(2025, week = 50)

roundabouts_clean <- tuesdata$roundabouts_clean

# Option 2: Read directly from GitHub

roundabouts_clean <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-16/roundabouts_clean.csv')
