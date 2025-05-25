library(tidyverse)
library(gt)

# Load data
df <- read_csv("https://ourworldindata.org/grapher/electricity-prod-source-stacked.csv?v=1&csvType=full&useColumnShortNames=true")

co2_ref <- tibble(
  energy_type = c("Coal", "Oil", "Gas", "Nuclear", "Hydro", "Wind", "Solar", "Bioenergy", "Other"),
  co2_intensity = c(900, 800, 469, 15, 4, 12, 22, 230, 0),
  Logo = c(
    "🪨",  # Coal
    "🛢️",  # Oil
    "🧯",  # Gas
    "☢️",  # Nuclear
    "💧",  # Hydro
    "🌬️",  # Wind
    "☀️",  # Solar
    "🪵",  # Bioenergy
    "🔶"   # Other
  )
)

# Clean and join
df_clean <- df |>
  filter(Year == 2023, Entity == "World") |>
  select(-Code, -Entity, -Year) |>
  pivot_longer(everything(), names_to = "energy_type", values_to = "amount") |>
  mutate(
    energy_type = str_remove(energy_type, "_generation__twh_chart_electricity_prod_source_stacked"),
    energy_type = ifelse(energy_type == "other_renewables_excluding_bioenergy", "Other", energy_type),
    energy_type = str_to_title(energy_type)
  ) |>
  left_join(co2_ref, by = "energy_type") |>
  arrange(desc(amount)) |>
  mutate(
    amount = round(amount, -1),                 # round to nearest 10
    percent = amount / sum(amount),
    co2_blank = co2_intensity
  ) |>
  select(Logo, everything())

# Build GT table with percent
df_clean |>
  gt() |>
  cols_hide(co2_intensity) |>
  cols_label(
    Logo = "",
    energy_type = "Energy Source",
    amount = "TWh (2023)",
    percent = "% of Total",
    co2_blank = "CO₂ Intensity"
  ) |>
  fmt_number(columns = amount, decimals = 0) |>
  fmt_percent(columns = percent, decimals = 0) |>
  fmt_number(columns = co2_blank, decimals = 0, pattern = " ") |>
  data_color(
    columns = co2_blank,
    colors = scales::col_numeric(
      palette = c("#f0f0f0", "#f88a8a", "#800000"),
      domain = range(df_clean$co2_intensity, na.rm = TRUE)
    )
  ) |>
  cols_align(columns = c(Logo, co2_blank), align = "center") |>
  tab_header(
    title = "Global Electricity Production by Source",
    subtitle = md("Data sourced from *Our World in Data*")
  )
library(knitr)

df_clean |>
  select(energy_type, amount, percent) |>
  mutate(
    amount = round(amount),
    percent = round(percent * 100, 1)
  ) |>
  rename(
    `Energy Type` = energy_type,
    `TWh (2023)` = amount,
    `% of Total` = percent
  ) |>
  kable()
