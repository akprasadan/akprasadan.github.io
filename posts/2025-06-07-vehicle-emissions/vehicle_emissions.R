library(tidyverse)
library(showtext)
library(ggtext)
library(ggimage)

font_add_google("Fira Sans", "fira")
showtext_auto()
showtext_opts(dpi = 300)




df = readxl::read_xlsx('emissions_by_year.xlsx', skip = 39,
                       col_names = c('pollutant', 2000:2030),
                       sheet = 1) |>
  pivot_longer(cols = -pollutant, names_to = "year", values_to = "quantity") |>
  mutate(year = as.numeric(year)) |>
  arrange(pollutant, year) |>
  group_by(pollutant) |>
  mutate(relative_reduction = quantity / first(quantity) -1) |>
  ungroup() |>
  mutate(projected = ifelse(year >= 2022, "projected", "revised")) |>
  mutate(pollutant_label = case_when(str_detect(pollutant, "CO2") ~ "CO2",
                                     str_detect(pollutant, "CO/10") ~ "CO",
                                     str_detect(pollutant, "Nox") ~ "NOx",
                                     str_detect(pollutant, "PM") ~ 'PM 2.5"'))

plot = df |>
  filter(pollutant %in% c(
    "Exhaust CO2/100",
    "Exhaust CO/10",
    "Exhaust Nox",
    "Exhaust PM2.5*10"
  )) |>
  ggplot(aes(x = year, y = relative_reduction, color = pollutant_label)) +
  geom_image(
    data = tibble(x = c(2009.5, 2015, 2019), y = c(-0.42, -0.69, -0.08), image = 'car_logo.png',
                  angle = c(45, 40, 15)),
    aes(x = x, y = y, image = image, angle = angle),
    size = 0.12,
    inherit.aes = F
  ) +
  geom_line(aes(linetype = projected), linewidth = 1.5) +
  annotate(
    geom ="rect",
    xmin = 2021,
    xmax = 2022,
    ymin = -1,
    ymax = 0,
    alpha = 0.5,
    fill = '#b2df8a',
    color = NA
  ) +
  annotate(
    "text",
    x = 1997.5,
    y = 0.03,
    label = "Reduction",
    hjust = 0,
    vjust = 0,
    size = 3,
    family = 'fira'
  ) +
  annotate(
    "text",
    x = 2024,
    y = -0.05,
    label = "Projected",
    color = 'grey60',
    family ='fira'
  ) +
  coord_cartesian(
    expand = FALSE,
    clip = "off",
    ylim = c(-1, 0.01),
    xlim = c(2000, 2031)
  ) +
  theme_classic() +
  scale_linetype_manual(
    breaks = c("projected", "revised"),
    values = c("dashed", "solid"),
    guide = 'none'
  ) +
  scale_x_continuous(breaks = seq(2000, 2030, by = 5)) +
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = c(seq(-0.95, 0, by = 0.20), 0)) +
  scale_color_manual(
    breaks = c(
      "CO2",
      "CO",
      "NOx",
      'PM 2.5"'
    ),
    values = c('#e41a1c', '#377eb8', '#4daf4a', '#984ea3')
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Average US vehicle exhaust emissions have declined from 2000 levels",
    subtitle = "Displaying gasoline/diesel fleet only. Updated annually using age distribution of fleet, regulatory standards, and driving behavior.",
    color = NULL,
    caption = 'CO2 = carbon dioxide; CO = carbon monoxide; NOx = nitrogen oxides; PM 2.5" = particulate matter of diameter \U2264 2.5 micrometers.\nSource: U.S. Environmental Protection Agency, Office of Transportation and Air Quality.\nPlot by Akshay Prasadan'
  ) +
  theme(
    text = element_text(family = 'fira'),
    legend.position = 'inside',
    legend.position.inside = c(0.2, 0.2),
    legend.background = element_rect(fill = NA),
    plot.caption.position = 'plot',
    plot.caption = element_text(hjust = 0, size = 7),
    axis.title.y = element_blank(),
    plot.margin = margin(5, 5, 5, 20),
    plot.subtitle = element_textbox_simple(
      margin = margin(b = 15),
      size = 9
    ),
    plot.title = element_markdown(hjust = 0)
  )
plot |>
  ggsave(
    filename = 'emissions_plot.png',
    height = 6,
    width = 7,
    dpi = 300
  )

