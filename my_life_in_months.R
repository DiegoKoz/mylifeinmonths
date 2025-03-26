library(tidyverse)
library(lubridate)
library(forcats)
# remotes::install_github("hrbrmstr/waffle")
library(waffle)
library(hrbrthemes)
library(extrafont)
library(ggplot2)
library(prismatic)

#hrbrthemes::import_roboto_condensed()
#extrafont::ttf_import()
loadfonts(device = "pdf", quiet = FALSE)
# Prep data ----

birth_year <- 1992
birth_month <- 1
current_year <- year(today())
current_month <- month(today())

life_data <- expand_grid(
  month = month.name,
  year = birth_year:current_year
) %>%
  mutate(month = fct_relevel(month, month.name)) %>%
  arrange(year, month) %>%
  group_by(year) %>%
  mutate(month_number = row_number()) %>%
  ungroup() %>%
  filter(!(year == birth_year & month_number < birth_month)) # %>%
# filter(!(year == current_year & month_number > current_month)) # If you want to exclude after the current month - I didn't, because it looked weird!

# Add "eras" to be coloured
# "era" text can be used for annotation, and the fill colour will colour the waffle chart

eras <- tribble(
  ~year_month, ~era, ~fill_colour,
  "1992,1", "childhood", "#fbbcb8",
  "2005,3", "highschool", "#db9e9a",
  "2011,3", "undergrad", "#9acbf0",
  "2016,4", "data analyst", "#9fa7e3",
  "2017,3", "master", "#78baeb",
  "2019,3", "stagiaire", "#d2001e",
  "2019,8", "freelance\nR dev", "#beaef5",
  "2020,1", "PhD", "#eb0000",
  "2022,2", 'research_stay',"#B3A369",
  "2022,8", "PhD", "#eb0000",
  "2023,5", "PostDoc", "royalblue",

)

# Darken fill colour to be used for text annotations

eras[["text_colour"]] <- as.character(clr_darken(eras[["fill_colour"]], shift = 0.1))

life_data <- life_data %>%
  rowwise() %>%
  mutate(year_month = paste0(c(year, month_number), collapse = ",")) %>%
  ungroup() %>%
  left_join(eras, by = "year_month") %>%
  fill(era, fill_colour, text_colour) %>%
  mutate(fill_colour = fct_inorder(fill_colour))

# Split life data into list based on era for using labels/colours later on

life_data_list <- split(life_data, life_data$era)

# Make waffle chart! ----

# Base plot



background_colour <- "#ffffff"

life_in_months_base <- life_data %>%
  count(fill_colour) %>% ## the count of each era is the number of months in that era
  ggplot(aes(fill = fill_colour, values = n)) +
  geom_waffle(color = background_colour, n_rows = 12, size = 1, flip = FALSE,na.rm = TRUE) + ## make each row a year/12 months
  coord_equal() +
  scale_x_continuous(limits = c(-0.5, 37.5)) + # The max here will differ based on how old you are! I'm 29 (so there are 30 squares), so ~7.5 more for the additional annotation on the side
  scale_y_continuous(limits = c(-2.5, 14.5)) +
  scale_fill_identity() +
  labs(y = NULL, x = NULL) +
  theme_ipsum(grid = "") +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = background_colour, color = background_colour),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

# Initial annotations ----

annotation_base_size <- 10 # Use ~10 for exporting at dpi 300, and ~3 for working interactively
annotation_lineheight <- 1
initial_annotations_font_family <- "Comic Sans MS"
initial_annotations_colour <- "#666666"

initial_text <- function(x, y, label, size = annotation_base_size, colour = initial_annotations_colour, ...) {
  annotate("text", x = x, y = y, label = label, size = size, colour = colour, family = "Comic Sans MS", fontface = "italic", ...)
}

initial_segment <- function(x, xend, y, yend, colour = initial_annotations_colour) {
  geom_segment(aes(x = x, xend = xend, y = y, yend = yend), colour = colour)
}

 life_in_months_initial_annotations <-
  life_in_months_base +
  initial_text(x = 0, y = 6.5, label = "1 year", angle = 90) +
  initial_segment(x = 0, xend = 0, y = 1, yend = 5) +
  initial_segment(x = -0.25, xend = 0.25, y = 1, yend = 1) +
  initial_segment(x = 0, xend = 0, y = 8, yend = 12) +
  initial_segment(x = -0.25, xend = 0.25, y = 12, yend = 12) +
  initial_text(x = 1, y = 14.5, label = "1 square = 1 month", size = annotation_base_size * 0.8, lineheight = annotation_lineheight, hjust = 0.4) +
  geom_curve(aes(x = 0, xend = 1, y = 14, yend = 12), arrow = arrow(length = unit(0.0175, "npc")), colour = initial_annotations_colour) +
  initial_text(x = 0.5, y = 0, label = "age", size = annotation_base_size * 0.8, hjust = 0) +
  geom_segment(aes(x = 2, xend = 4, y = 0, yend = 0), arrow = arrow(length = unit(0.0175, "npc")), colour = initial_annotations_colour)
  #annotate("text", x = 31.25, y = 6.5, label = "my life\nchart", hjust = 0, family = "Arial", lineheight = 1, size = annotation_base_size)

# "Role" annotations ----

role_annotations_y <- -0.25
roles_size <- annotation_base_size

role_text <- function(x, y = role_annotations_y, label, size = roles_size, ...) {
  annotate("text", x = x, y = y, label = label, size = size, colour = unique(unique(life_data_list[[label]][["text_colour"]])), family = ".SF NS Rounded", ...)
}

role_text_under <- function(x, y, label, colour_era, size, ...) {
  annotate("text", x = x, y = y, label = label, colour = unique(life_data_list[[colour_era]][["text_colour"]]), size = size, family = ".SF NS Rounded", ...)
}

# For annotations: x values are the usually ~midpoint of your age (+1) during that era, give or take for some shifting around to fit labels

life_in_months_role_annotations <- life_in_months_initial_annotations +
  role_text(x = 8.5, label = "childhood") +
  role_text(x = 17, label = "highschool") +
  role_text(x = 19, y = role_annotations_y - 1, label = "undergrad") +
  role_text_under(x = 19, y = role_annotations_y - 1.5, label = "(economics-UBA)", colour_era = "undergrad", size = roles_size * 0.75)+
  geom_curve(aes(x = 21, xend = 22, y = -1.7, yend = 0.35), curvature = 0.4, arrow = arrow(length = unit(0.0175, "npc")), colour = unique(life_data_list[["undergrad"]][["text_colour"]]))+
  role_text(x = 24, y = role_annotations_y, label = "data analyst") +
  role_text_under(x = 24, y = role_annotations_y - .5, label = "(INDEC)", colour_era = "data analyst", size = roles_size * 0.75)+
  geom_curve(aes(x = 25.75, xend = 26, y = -.5, yend = 0.35), curvature = 0.25, arrow = arrow(length = unit(0.0175, "npc")), colour = unique(life_data_list[["data analyst"]][["text_colour"]]))+
  role_text(x = 27, y = role_annotations_y-1, label = "master") +
  role_text_under(x = 27, y = role_annotations_y - 1.5, label = "(data mining-UBA)", colour_era = "master", size = roles_size * 0.75)+
  geom_curve(aes(x = 27, xend = 27, y = -1, yend = 0.35), curvature = 0.25, arrow = arrow(length = unit(0.0175, "npc")), colour = unique(life_data_list[["master"]][["text_colour"]]))+
  role_text(x = 30, y = role_annotations_y, label = "PhD")+
  role_text_under(x = 30, y = role_annotations_y - .5, label = "(UL)", colour_era = "PhD", size = roles_size * 0.75)


# Location annotations ----

location_colour <- "#8c8c8c"
location_annotations_y <- 13

location_text <- function(x, y = location_annotations_y, label, size = annotation_base_size, ...) {
  annotate("text", x = x, y = y, label = label, size = size, colour = location_colour, family = "Arial", ...)
}

life_in_months_final <-
life_in_months_role_annotations +
  location_text(x = 13, y = location_annotations_y + 0.1, label = "born + raised in Buenos Aires") +
  geom_segment(aes(x = 1, xend = 8, y = 13, yend = 13), colour = location_colour) +
  geom_segment(aes(x = 18, xend = 27, y = 13, yend = 13), colour = location_colour) +
  geom_segment(aes(x = 1, xend = 1, y = 12.75, yend = 13.25), colour = location_colour) +
  geom_segment(aes(x = 27, xend = 27, y = 12.75, yend = 13.25), colour = location_colour) +
  location_text(x = 24, y = location_annotations_y + 1, label = "stage in Toulouse", hjust = 0.75) +
  geom_curve(aes(x = 25, xend = 27.7, y = location_annotations_y + 1, yend = 12.6), curvature = -0.1, arrow = arrow(length = unit(0.0175, "npc")), colour = location_colour) +
  location_text(x = 29, y = location_annotations_y+1.5, label = "moved to\nLuxembourg") +
  geom_curve(aes(x = 29, xend = 29, y = 13.5, yend = 12), curvature = 0, arrow = arrow(length = unit(0.0175, "npc")), colour = location_colour)+
  location_text(x = 35, y = location_annotations_y + 1, label = "Research stay at\nMontreal and Atlanta") +
  geom_curve(aes(x = 35, xend = 31.5, y = 13, yend = 12), curvature = -0.3, arrow = arrow(length = unit(0.0175, "npc")), colour = location_colour)+
  role_text_under(x = 37, y = 7.5, colour_era = "PostDoc",  size = roles_size, label = "Currently doing \nmy PostDoc at\nUdeM-Montreal")


# Save final plot ----
ggsave("life_in_months.png", plot = life_in_months_final, device = "png", type = "cairo", width = 25, height = 15, dpi = 300)

