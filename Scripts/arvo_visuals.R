# AUTHOR: 
# PURPOSE: Visualizations for arvophilia case
# DATE:     2025-04-10
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------

library(glamr) #custom workflow package
library(tidyverse)
library(glitr) #custom data viz package
library(systemfonts)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(readxl)

# GLOBAL VARIABLES --------------------------------------------------------

# Set working directory
data_folder <- "Data"

clean_number <- function(x, digits = 0){
  dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                   x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                   x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                   TRUE ~ glue("{x}"))
}

# IMPORT ------------------------------------------------------------------
# read in tidy data from excel sheet
df <- data_folder %>% 
  return_latest("CHAI") %>% 
  read_excel(sheet = "Tidy data (for viz)")


# MUNGE -------------------------------------------------------------------

# #1: Cost Visual

#first, let's pull data for the cost per life saved viz
df_lives_saved <- df %>% 
  janitor::clean_names() %>% 
  select(group, contains("survivors"), lives_saved, cost_per_life_saved)

# pull the overall value of the cost per 1 additional life save across age groups (use in title in viz later)
total_cost_per_life <- df_lives_saved %>% 
  filter(group == "Total") %>% 
  mutate(cost_per_life_saved = dollar(cost_per_life_saved)) %>% 
  pull(cost_per_life_saved)

# 2: Mortality visual

df_lives_dumbbell <- df_lives_saved %>% 
  select(group, contains("survivors")) %>% 
  pivot_longer(cols = c(contains("survivors")), names_to = "treatment") %>% 
  mutate(treatment = str_remove(treatment, "_survivors")) %>% 
  filter(group != "Total") %>% 
  rename(survivors = value)

# Pivot wider to create segments
df_wide <- df_lives_dumbbell %>%
  tidyr::pivot_wider(names_from = treatment, values_from = survivors) %>% 
  mutate(delta = clairadol - hufstatin)
  

# VIZ # 1 --------------------------------------------------------------------

# set bar colors
chai_teal <- "#107996"
chai_blue <- "#003C56"

# bar chart to show cost per life saved by age group
df_lives_saved %>% 
  select(group, cost_per_life_saved) %>% 
  filter(group != "Total") %>% 
  mutate(fill_color = ifelse(group == "Children (<14)", chai_teal, chai_blue)) %>% 
  ggplot(aes(x = fct_reorder(group, cost_per_life_saved), y = cost_per_life_saved, fill = fill_color)) + #reorder bars to have children first
  geom_col(width = 0.5) +
  scale_fill_identity() +
  scale_y_continuous(limits = c(-30, 60), labels = scales::dollar) + #widen Y axis and format to $
  geom_text(aes(label = dollar(cost_per_life_saved, 1)),
            vjust = -0.5, family = "Source Sans 3", size = 4) +
  si_style_ygrid() + # custom function for ggplot styling (built in previous role)
  geom_hline(yintercept = 0, color = "black", size = 1.2) +
  labs(
    x = NULL, 
    y = "Cost per additional life saved by switching to Clairadol (USD)",
    title = "Clairadol is a highly cost-effective alternative to Huffstatin, at $1.34 per additional life saved",
    subtitle = "It even saves $23.39 per life saved for children <14, and remains cost-effective for adults >14 at $53.43",
    caption = glue("Source: Data supplied by CHAI | Date: {lubridate::today()}")
  ) +
  theme(
    theme(text = element_text(family = "Source Sans 3"))
  )

si_save("Images/01_cost_per_life_saved.png")



# VIZ # 2 --------------------------------------------------------------------

df_wide %>% 
  mutate(delta = clairadol - hufstatin) %>% 
  ggplot(aes(y = group)) +
  geom_segment(aes(x = hufstatin, xend = clairadol, yend = group),
               color = "grey70", size = 1) +
  geom_point(aes(x = hufstatin), color = trolley_grey_light, size = 4) +
  geom_point(aes(x = clairadol), color = chai_teal, size = 4) +
  geom_text(aes(x = hufstatin, label = clean_number(hufstatin)),
            hjust = 1.2, vjust = -0.5, family = "Roboto", size = 3.5) +
  geom_text(aes(x = clairadol, label = clean_number(clairadol)),
            hjust = -0.2, vjust = -0.5, family = "Roboto", size = 3.5) +
  geom_text(aes(x = (hufstatin + clairadol)/2, label = paste0("+", comma(delta))),
            vjust = 2, family = "Roboto", fontface = "bold", size = 3.5, color = chai_teal) +
  scale_x_continuous(label = scales::comma) +
  si_style_xgrid() +
  labs(
    x = "Number of Lives Saved",
    y = NULL,
    title = " <span style='color:#007d8a'><b>Clairadol</b></span> saves more lives than <span style='color:#6d6e71'><b>Huffstatin</b></span>, saving over 14,000 lives across all ages",
    subtitle = "Additional survivors seen in both children and adults",
    caption = glue("Source: Data supplied by CHAI | Date: {lubridate::today()}")
  ) +
  theme(
    plot.title = element_markdown()
  )


si_save("Images/02_lives_saved.png")

