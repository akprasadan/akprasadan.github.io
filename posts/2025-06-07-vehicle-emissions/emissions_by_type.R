library(tidyverse)
library(showtext)
library(ggtext)
library(ggimage)

font_add_google("Fira Sans", "fira")
showtext_auto()
showtext_opts(dpi = 300)

vehicle_types = c(
  "Light-duty vehicles",
  "Light-duty trucks",
  "Buses",
  "Heavy-duty vehicles (other than buses)",
  "Motorcyclesb"
)

fuel_types = c("GASOLINE", "DIESEL", "ELECTRICc")

df = readxl::read_xlsx(
  'emissions_by_year.xlsx',
  skip = 2,
  col_names = c('header_row', 2000:2030),
  sheet = 2
) |>
  mutate(
    fuel_type = case_when(
      header_row == "GASOLINE" ~ "Gasoline",
      header_row == "DIESEL" ~ "Diesel",
      header_row == "ELECTRICc" ~ "Electric",
      header_row == "Average Emissions Per Vehicle: Gasoline and Diesel Fleet" ~ "average",
      str_detect(header_row, "KEY") ~ "notes",
      TRUE ~ NA_character_
    ),
    vehicle_type = case_when(header_row %in% vehicle_types ~ header_row, TRUE ~ NA_character_),
    vehicle_type = ifelse(vehicle_type == "Motorcyclesb", "Motorcycle", vehicle_type)
  ) |>
  fill(fuel_type, vehicle_type, .direction = "down") |>
  filter(
    fuel_type != "notes",!(header_row %in% fuel_types),!(header_row %in% vehicle_types)
  ) |>
  # Some of the year columns are character, because they have value "N"
  # To fix this, first I will convert EVERY column to numeric.
  # Then I will use na_if to convert the "N" values to NA values
  # Finally I can convert back to numeric, and avoid the errors
  # I combine these steps in a single, convoluted step
  mutate(across(starts_with("20"), ~ as.numeric(na_if(
    as.character(.), "N"
  )))) |>
  pivot_longer(
    cols = !c(header_row, fuel_type, vehicle_type),
    names_to = 'year',
    values_to = 'grams_per_mile'
  ) |>
  rename(pollutant = header_row) |>
  # This next filter throws out electric vehicles, which have no exhaust emissions
  filter(str_detect(pollutant, "Exhaust")) |>
  filter(vehicle_type != "Motorcycle") |>
  mutate(
    year = as.numeric(year),
    projected = ifelse(year >= 2022, "projected", "revised"),
         pollutant = case_when(str_detect(pollutant, "CO2") ~ "CO2",
                                     str_detect(pollutant, "CO") ~ "CO",
                                     str_detect(pollutant, "NOx") ~ "NOx",
                                     str_detect(pollutant, "PM2.5") ~ 'PM 2.5"')) |>
  arrange(pollutant, fuel_type, vehicle_type, year) |>
  group_by(pollutant, fuel_type, vehicle_type) |>
  mutate(relative_reduction = grams_per_mile  / first(grams_per_mile ) -1) |>
  ungroup() |>
  mutate(vehicle_type_short = case_when(
    str_detect(vehicle_type, "trucks") ~ "Small Trucks",
    str_detect(vehicle_type, "Heavy") ~ "Big Trucks",
    str_detect(vehicle_type, "Light-duty vehicles") ~ "Cars",
    TRUE ~ vehicle_type
  ),
  last_point = ifelse(year == 2021, relative_reduction, NA))

annotation_df <- tibble(
    x = 1996.5,
    y  = 0.45,
    vehicle_type_short = "Cars",
    fuel_type = "Gasoline",
    pollutant = NA
  )


plot = df |>
  filter(fuel_type != "average") |>
  ggplot(aes(x = year, y = relative_reduction, color = pollutant)) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.5,
    linetype = 'solid',
    color = 'grey80'
  ) +
  geom_line(aes(linetype = projected), linewidth = 0.7) +
  geom_point(x = 2021, aes(y = last_point, color = pollutant), inherit.aes = F)  +
  facet_grid(factor(vehicle_type_short,
                    levels = c("Cars", "Small Trucks", "Buses",
                               "Big Trucks")) ~ factor(fuel_type, levels = c("Gasoline", "Diesel")),
             switch = "y")  +
  theme_light() +
  geom_text(
    data = annotation_df,
    aes(x = x, y = y),
    label = "Growth",
    family = 'fira',
    fontface = 'bold',
    size = 3,
    color = 'black'
  ) +
  scale_color_manual(
    breaks = c("CO2", "CO", "NOx", 'PM 2.5"'),
    values = c('#e41a1c', '#377eb8', '#4daf4a', '#984ea3')
  ) +
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = seq(-1, 0.3, by = 0.25))  +
  coord_cartesian(
    expand = FALSE,
    clip = "off",
    ylim = c(-1, 0.4),
    xlim = c(2000, 2031)
  ) +
  scale_x_continuous(breaks = seq(2000, 2030, by = 10)) +
  scale_linetype_manual(
    breaks = c("projected", "revised"),
    values = c("dotted", "solid"),
    guide = 'none'
  ) +
  theme(
    text = element_text(family = "fira"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = 'grey95'),
    panel.spacing.x = unit(1, "lines"),
    axis.text.x.bottom = element_text(size  = 9, angle = 30, hjust = 1,
                                      margin = margin(b = 0)),
    axis.text.y.left = element_text(size = 8),
    strip.text.y.left = element_text(size = 12, angle = 0, margin = margin(r = 0)),
    strip.placement = "outside",
    strip.text.x.top = element_text(size = 11),
    strip.background.x = element_rect(fill = NA),
    legend.position = 'bottom',
    legend.text.position = "right",
    legend.margin = margin(t =-5, b =0),
    legend.text = element_text(size = 9),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(color = 'black', size = 12),
    strip.clip = 'off',
    plot.margin = margin(l = 3, r = 5, t = 20),
    plot.caption.position = 'plot',
    plot.caption = element_text(hjust = 0, size = 7),
    plot.title.position = 'plot',
    plot.title = element_text(size = 14)
  ) +
  guides(color = guide_legend(override.aes = list(linewidth = 3))) +
  labs(x = NULL, y = NULL, color = NULL,
       caption = 'CO2 = carbon dioxide; CO = carbon monoxide; NOx = nitrogen oxides; PM 2.5" = particulate matter of diameter \U2264 2.5 micrometers.\nSource: U.S. Environmental Protection Agency, Office of Transportation and Air Quality.\nPlot by Akshay Prasadan',
       title = "CO2 reduction remains a stubborn problem for most vehicle types.")

plot |>
  ggsave(
    filename = 'emissions_plot_by_type.png',
    height = 7,
    width = 6,
    dpi = 300
  )

# How about the actual levels? Let's look at actual CO2 emissions for gasoline by type, and then diesel by type

plot2 = df |> filter(pollutant == "CO2", fuel_type != "average") |>
  ggplot(aes(x = year, y = grams_per_mile, color = vehicle_type_short)) +
  geom_line(aes(linetype = fuel_type, alpha = projected), linewidth = 0.7) +
  geom_line(data = df |> filter(pollutant == "CO2", fuel_type == "average"),
            aes(x = year, y = grams_per_mile, alpha = projected), color = 'black',
            linewidth = 1.5) +
  theme_light() +
  scale_color_manual(breaks = c("Cars", "Small Trucks", "Buses", "Big Trucks"),
                     values = c('#1b9e77','#d95f02','#7570b3','#e7298a')) +
  scale_linetype_manual(
    breaks = c("Diesel", "Gasoline"),
    values = c('dotted', "solid")
  ) +
  scale_alpha_manual(breaks = c("revised", "projected"),
                     values = c(1, 0.3),
                     guide = 'none') +
  annotate(geom ="rect",
    xmin = 2021,
    xmax = 2022,
    ymin = 0,
    ymax = 1800,
    alpha = 0.5,
    fill = '#b2df8a',
    color =NA
  ) +
  annotate(geom = "text",
           x = 2014, y = 650, angle = -5,
           label = "Average", family = 'fira') +
  annotate(geom = "text",
           x = 1997.6, y = 1750,
           label = "grams/mile",
           family = 'fira', size = 3) +
  scale_x_continuous(breaks = seq(2000, 2030, by = 5)) +
  coord_cartesian(
    expand = FALSE,
    clip = "off",
    ylim = c(0, 1800),
    xlim = c(2000, 2030)
  ) +
  theme(
    text = element_text(family = "fira"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = 'grey95'),
    panel.spacing.x = unit(1, "lines"),
    axis.text.x.bottom = element_text(size  = 9, angle = 40, hjust = 1.2,
                                      margin = margin(b = 0)),
    axis.text.y.left = element_text(size = 8),
    strip.text.y.left = element_text(size = 12, angle = 0, margin = margin(r = 0)),
    strip.text.x.top = element_text(size = 11),
    strip.background.x = element_rect(fill = NA),
    legend.position = 'bottom',
    legend.text.position = "right",
    legend.text = element_text(size = 9),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(color = 'black', size = 12),
    strip.clip = 'off',
    plot.caption.position = 'plot',
    plot.caption = element_text(hjust = 0, size = 7),
    plot.title = element_markdown(size = 14),
    plot.subtitle = element_text(size = 10),
    plot.margin = margin(l = 45, t= 5, r = 5, b = 5)
  ) +
  guides(color = guide_legend(override.aes = list(linewidth = 3), title = NULL,
                              nrow = 2),
         linetype = guide_legend(title =NULL, nrow =2, override.aes = list(color = '#66a61e'))) +
  labs(x = NULL, y = NULL,
       caption = 'Source: U.S. Environmental Protection Agency, Office of Transportation and Air Quality.\nPlot by Akshay Prasadan',
       title = "Diesel <span style = 'color: #e41a1c'>**CO2**</span> emissions are much higher for buses and  <br> trucks than their gasoline analogue.",
       subtitle = "They've lost their head-start for cars too.")

plot2 |>
  ggsave(
    filename = 'CO2_plot_by_fuel_and_type.png',
    height = 6,
    width = 6,
    dpi = 300
  )

plot3 = df |> filter(pollutant == "NOx", fuel_type != "average") |>
  ggplot(aes(x = year, y = grams_per_mile, color = vehicle_type_short)) +
  geom_line(aes(linetype = fuel_type, alpha = projected), linewidth = 0.7) +
  geom_line(data = df |> filter(pollutant == "NOx", fuel_type == "average"),
            aes(x = year, y = grams_per_mile, alpha = projected), color = 'black',
            linewidth = 1.5) +
  theme_light() +
  scale_color_manual(breaks = c("Cars", "Small Trucks", "Buses", "Big Trucks"),
                     values = c('#1b9e77','#d95f02','#7570b3','#e7298a')) +
  scale_linetype_manual(
    breaks = c("Diesel", "Gasoline"),
    values = c('dotted', "solid")
  ) +
  scale_alpha_manual(breaks = c("revised", "projected"),
                     values = c(1, 0.3),
                     guide = 'none') +
  annotate(geom ="rect",
           xmin = 2021,
           xmax = 2022,
           ymin = 0,
           ymax = 28,
           alpha = 0.5,
           fill = '#b2df8a',
           color =NA
  ) +
  annotate(geom = "text",
           x = 2014, y = 20, angle = -5,
           label = "Average", family = 'fira') +
  annotate(geom = "text",
           x = 1997.6, y = 31,
           label = "grams/mile",
           family = 'fira', size = 3) +
  scale_x_continuous(breaks = seq(2000, 2030, by = 5)) +
  scale_y_continuous(breaks = seq(0, 30, by = 5)) +
  coord_cartesian(
    expand = FALSE,
    clip = "off",
    ylim = c(0, 28),
    xlim = c(2000, 2030)
  ) +
  theme(
    text = element_text(family = "fira"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = 'grey95'),
    panel.spacing.x = unit(1, "lines"),
    axis.text.x.bottom = element_text(size  = 9, angle = 40, hjust = 1.2,
                                      margin = margin(b = 0)),
    axis.text.y.left = element_text(size = 8),
    strip.text.y.left = element_text(size = 12, angle = 0, margin = margin(r = 0)),
    strip.text.x.top = element_text(size = 11),
    strip.background.x = element_rect(fill = NA),
    legend.position = 'bottom',
    legend.text.position = "right",
    legend.text = element_text(size = 9),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(color = 'black', size = 12),
    strip.clip = 'off',
    plot.caption.position = 'plot',
    plot.caption = element_text(hjust = 0, size = 7),
    plot.title = element_markdown(size = 14),
    plot.margin = margin(l = 45, t= 5, r = 5, b = 5)
  ) +
  guides(color = guide_legend(override.aes = list(linewidth = 3), title = NULL,
                              nrow = 2),
         linetype = guide_legend(title =NULL, nrow =2, override.aes = list(color = '#66a61e'))) +
  labs(x = NULL, y = NULL,
       caption = 'Source: U.S. Environmental Protection Agency, Office of Transportation and Air Quality.\nPlot by Akshay Prasadan',
       title = "Diesel <span style = 'color: #4daf4a'>**NOx**</span> emissions aren't much better.")

plot3 |>
  ggsave(
    filename = 'NOx_plot_by_fuel_and_type.png',
    height = 6,
    width = 6,
    dpi = 300
  )
