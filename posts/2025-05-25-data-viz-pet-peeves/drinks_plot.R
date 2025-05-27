
library(tidyverse)
library(fivethirtyeight)
library(countrycode)
library(showtext)

font_add_google("Fira Sans", family ='fira')
showtext_opts(dpi = 700)
showtext_auto()

drinks_df = fivethirtyeight::drinks |>
  mutate(
    continent =
      countrycode(
        sourcevar = country,
        origin = "country.name",
        destination = "continent",
        warn = F
      )
  ) |>
  mutate(continent = ifelse(country == "Micronesia", "Oceania", continent)) |>
  select(-total_litres_of_pure_alcohol) |>
  pivot_longer(cols = ends_with('servings'),
               names_to = "alcohol_type",
               values_to = "servings") |>
  group_by(continent, alcohol_type) |>
  summarize(mean_servings = mean(servings, na.rm = T),
            .groups = 'drop') |>
  mutate(
    alcohol_type = str_remove(alcohol_type, "_servings"),
    alcohol_type = factor(alcohol_type, levels = c("beer", "spirit", "wine")),
    continent = fct_reorder(continent, mean_servings)
  )

drinks_summary_df = drinks_df |> group_by(alcohol_type) |>
  summarize(mean_servings = mean(mean_servings), .groups = "drop") |>
  mutate(pretty_mean_servings = round(mean_servings, digits = 1))

drink_plot = drinks_df |> ggplot(aes(x = continent, y = mean_servings)) +
  geom_col(aes(fill = alcohol_type),
           position = position_dodge(),
           alpha = 0.9) +
  geom_hline(
    data = drinks_summary_df,
    aes(yintercept = mean_servings, color = alcohol_type),
    show.legend = F,
    linetype = 'dashed'
  ) +
  geom_text(
    data = drinks_summary_df,
    x = "Africa",
    aes(y = mean_servings, color = alcohol_type, label = pretty_mean_servings),
    show.legend =  F,
    vjust = -0.5,
    size = 4
  ) +
  labs(
    x = "",
    y = "Servings per Person",
    title = "Per capita alcohol consumption in 2010",
    fill = NULL,
    subtitle = "Global averages indicated by dotted lines.",
    caption = paste("Data constructed by FiveThirtyEight using World Health",
                    "Organization data.\nPlot made by Akshay Prasadan.")
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 210)) +
  scale_fill_manual(
    breaks = c("beer", "spirit", "wine"),
    labels = c("Beer", "Spirits", "Wine"),
    values = c("#F28E1C", '#0072B2', "#7F1734")
  ) +
  scale_color_manual(
    breaks = c("beer", "spirit", "wine"),
    labels = c("Beer", "Spirits", "Wine"),
    values = c("#F28E1C", '#0072B2', "#7F1734")
  ) +
  theme_light() +
  theme(
    legend.position = 'inside',
    legend.position.inside = c(0.1, 0.8),
    legend.box.background = element_rect(color = NA),
    legend.background = element_rect(color = "grey60", fill = '#FFF4C2'),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = 'grey80'),
    plot.caption.position = 'plot',
    plot.caption = element_text(hjust = 0),
    plot.background  = element_rect(fill = "#FFF4C2"),
    panel.background  = element_rect(fill = "#FFF4C2"),
    panel.border = element_rect(color = 'black'),
    text = element_text(family = "fira"),
    plot.title = element_text(size = 20, family = "fira"),
    plot.subtitle = element_text(
      size = 16,
      color = "gray30",
      family = "fira"
    )
  )

ggsave(filename = "drinks_plot.png", dpi = 700, width = 6, height = 5)

drink_plot_lazy = drinks_df |>
  ggplot(aes(x = continent, y = mean_servings)) +
  geom_col(aes(fill = alcohol_type), position = position_dodge(), alpha = 0.9) +
  geom_hline(
    data = drinks_summary_df,
    aes(yintercept = mean_servings, color = alcohol_type),
    show.legend = FALSE,
    linetype = 'dashed'
  ) +
  geom_text(
    data = drinks_summary_df,
    x = "Africa",
    aes(y = mean_servings, color = alcohol_type, label = pretty_mean_servings),
    show.legend = FALSE,
    vjust = -0.5,
    size = 4
  ) +
  labs(
    x = "",
    y = "Servings per Person",
    title = "Per capita alcohol consumption in 2010",
    fill = NULL,
    subtitle = "Global averages indicated by dotted lines.",
    caption = paste("Data constructed by FiveThirtyEight using World Health",
                    "Organization data.\nPlot made by Akshay Prasadan.")
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 210)) +
  scale_fill_discrete(
    breaks = c("beer", "spirit", "wine"),
    labels = c("Beer", "Spirits", "Wine")
  ) +
  scale_color_discrete(
    breaks = c("beer", "spirit", "wine"),
    labels = c("Beer", "Spirits", "Wine")
  ) +
  theme_light() +
  theme(
    legend.position = 'inside',
    legend.position.inside = c(0.1, 0.8),
    legend.box.background = element_rect(),
    legend.background = element_rect(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(),
    plot.caption.position = 'plot',
    plot.caption = element_text(hjust = 0),
    plot.background  = element_rect(),
    panel.background  = element_rect(),
    panel.border = element_rect(),
    text = element_text(family = "fira"),
    plot.title = element_text(size = 20, family = "fira"),
    plot.subtitle = element_text(
      size = 16,
      family = "fira"
    )
  )

ggsave(filename = "drinks_plot_lazy.png", plot = drink_plot_lazy, dpi = 700, width = 6, height = 5)
