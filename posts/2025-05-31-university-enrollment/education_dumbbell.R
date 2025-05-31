library(tidyverse)
library(readxl)
library(ggtext)
library(ggimage)
library(showtext)

font_add_google("Lato")
showtext_auto()
showtext_opts(dpi = 300)

transparent <- function(img) {
  magick::image_fx(img, expression = "0.5*a", channel = "alpha")
}


df = read_xlsx('tabn303.10.xlsx', skip = 3) |>
  janitor::clean_names() |>
  select(year, total = total_enrollment, male, female,
         full_time, part_time = part_time_4,
         public, private = total) |>
  # Removes the footers
  filter(!is.na(male)) |>
  # remove unnecessary column number header row
  filter(year != "1") |>
  # Need to clean up some strings before we convert to numeric
  mutate(
    # "---" = not available according to footnote
    full_time = str_replace(full_time, "---", NA_character_),
    part_time = str_replace(full_time, "---", NA_character_),
    # remove all the '\\1\\'  or '\r\n' strings.
    # We have to do some escaping of the backlashes though.
    year = str_replace(year, '\\\\1\\\\', ""),
    year = str_replace(year, "\\r\\n", "")
    # No other string oddities in year remain
    ) |>
  # Convert all columns to numeric
  mutate(across(everything(), as.numeric)) |>
  mutate(prop_male = male / total,
         prop_female = female / total,
         prop_public = public / total,
         prop_private = private / total) |>
  select(year, starts_with('prop'))

# Final year where male enrollment exceeded female enrollment
final_male_df = df |> filter(prop_male > prop_female) |> slice_max(order_by = year)

df = df |> filter(year %in% seq(2023, 1947, by = -2))

long_df = df |> pivot_longer(cols = c(prop_male, prop_female),
                   names_to = "sex",
                   names_prefix = "prop_",
                   values_to = "enrollment_pct")

long_df |> ggplot(aes(x = year, y = enrollment_pct, color = sex)) +
  geom_point(size = 2.1) +
  geom_segment(data = df,
               aes(x = year, y = prop_male, yend = prop_female),
               inherit.aes = F,
               alpha = 0.3,
               linewidth = 2.2,
               color = '#d95f02') +
  geom_segment(data = final_male_df, aes(x = year + 1), y = 0.25, yend = 0.7,
               color = '#d95f02', linewidth = 1, alpha = 0.5,
               linetype = 'dotdash',
               inherit.aes = F) +
  geom_image(data = tibble(year = 1998, enrollment_pct = 0.32),
             aes(x =year, y = enrollment_pct, image = 'generic_students.png'),
             image_fun = transparent,
             size =0.3,
             inherit.aes = F) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0.25, 0.73),
                     expand = c(0,0)) +
  scale_x_continuous(breaks = seq(1950, 2025, by = 10),
                     limits =c(1945, 2024),
                     expand = c(0,0)) +
  scale_color_manual(breaks = c("male", "female"),
                     values = c("#7570b3", '#1b9e77'),
                                guide = 'none') +
  labs(x = "",
       y = "",
       title = "<span style = 'color:#7570b3;'>**Male**</span> and <span style = 'color:#1b9e77;'>**Female**</span> University Enrollment has swapped places.",
       subtitle = "Proportion of Fall Enrollment between 1947 and 2023 (odd years only)",
       caption = "Data sourced from *The Institute of Education Sciences*.<br><br>Plot by Akshay Prasadan.")+
  theme_minimal() +
  theme(text = element_text(family = 'Lato', size = 7),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = 'grey90', linetype = 'dotted'),
        axis.ticks.x.bottom = element_line(color = 'black'),
        axis.ticks.length.x.bottom = unit(4, units ='pt'),
        axis.text.x.bottom = element_text(color = 'black'),
        axis.text.y.left = element_text(color = 'black'),
        plot.title = element_markdown(),
        plot.caption.position = 'plot',
        plot.caption = element_markdown(hjust = 0, family = ""))
