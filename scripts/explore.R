# Load packages.
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(waffle)

# Load data.
coffee_survey <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-14/coffee_survey.csv')

# Check missing.
sum(is.na(coffee_survey$dairy))

# Split up the dairy variable.
coffee_survey_long <- coffee_survey %>% 
  separate_rows(dairy, sep = ",") %>% 
  mutate(dairy = trimws(dairy),
         dairy = if_else(is.na(dairy), "None", dairy))

# Counts.
count(coffee_survey_long, dairy)

# Create dairy flag in the original data.
coffee_survey_long <- coffee_survey_long %>% 
  mutate(dairy_flag = recode(dairy,
    `Almond milk`             = "Alternative",
    `Coffee creamer`          = "Dairy",
    `Flavored coffee creamer` = "Dairy",
    `Half and half`           = "Dairy",
    `Oat milk`                = "Alternative",
    `Soy milk`                = "Alternative",
    `Skim milk`               = "Dairy",
    `Whole milk`              = "Dairy",
    `None`                    = "None (black)"
  ))

# Final clean. Drop missing political affiliation and randomly select one 
# of their milk preferences, for people that chose > 1. 
set.seed(1612)

political_long <- coffee_survey_long %>% 
  drop_na(political_affiliation) %>%
  group_by(submission_id) %>%
  sample_n(size = 1) %>% 
  ungroup()

# Plot.
ggplot(data = political_long) +
  geom_bar(mapping = aes(x = political_affiliation, fill = dairy_flag),
           position = "fill",
           colour = "black") +
  theme_bw() +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = NULL, fill = NULL,
       title = "Do republicans prefer 'real' milk in their coffee?",
       caption = "Data source: Great American Coffee Taste Test (2023)") +
  theme(legend.position = "bottom") 

# Save.
ggsave(filename = "visuals/political_milk_draft.png",
       height = 12, width = 16, unit = "cm", dpi = 300)

# Handling for more specific preferences.
# Final clean. Drop missing political affiliation and randomly select one 
# of their milk preferences, for people that chose > 1. 
waf_data <- coffee_survey_long %>%
  group_by(political_affiliation, dairy) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  drop_na(political_affiliation) %>% 
  filter(dairy != "None") %>% 
  group_by(political_affiliation) %>% 
  mutate(props = round(100*n/sum(n), 2)) %>% 
  ungroup()

# Make waffle (draft).
ggplot(data = waf_data,
         mapping = aes(fill = dairy, values = props)) +
  geom_waffle(flip = TRUE) +
  facet_wrap(~political_affiliation, nrow = 2, scales = "free") +
  scale_fill_brewer(palette = "Set3") +
  theme_bw()

  
  
  
  
  


