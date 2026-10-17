library(tidyverse)
unemp_raw <- read_rds("data/unemp_raw.rds")
head(unemp_raw)
unemp <- unemp_raw |>
  rename(unemployment_rate = price) |>
  mutate(
    region = case_when(
      symbol == "UNRATE" ~ "United States",
      symbol == "ORUR" ~ "Oregon",
      symbol == "CAUR" ~ "California",
      symbol == "TXUR" ~ "Texas",
      symbol == "NYUR" ~ "New York"
    )
  )
unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(
    unemployment_rate = replace(
      unemployment_rate,
      date == as.Date("2025-10-01"),
      unemployment_rate[date == as.Date("2025-09-01")]
    )
  ) |>
  ungroup()
unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(
    unemployment_rate_change =
      unemployment_rate - lag(unemployment_rate)
  ) |>
  ungroup()
unemp_recent <- unemp |>
  filter(date >= as.Date("2015-01-01"))
unemp_summary <- unemp |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate = min(unemployment_rate, na.rm = TRUE),
    max_rate = max(unemployment_rate, na.rm = TRUE)
  )
unemp_recent_summary <- unemp |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate = min(unemployment_rate, na.rm = TRUE),
    max_rate = max(unemployment_rate, na.rm = TRUE)
  )
unemp_top_increases <- unemp |>
  slice_max(unemployment_rate_change, n = 5, with_ties = TRUE)

unemp_top_decreases <- unemp |>
  slice_min(unemployment_rate_change, n = 5, with_ties = TRUE)
unemp_top_increases_no_covid <- unemp |>
  filter(date < as.Date("2020-01-01") |
           date >= as.Date("2021-01-01")) |>
  slice_max(unemployment_rate_change, n = 5, with_ties = TRUE)

unemp_top_decreases_no_covid <- unemp |>
  filter(date < as.Date("2020-01-01") |
           date >= as.Date("2021-01-01")) |>
  slice_min(unemployment_rate_change, n = 5, with_ties = TRUE)
ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +
  labs(
    title = "Monthly Unemployment Rates by Region",
    x = "Date",
    y = "Unemployment Rate (%)",
    color = "Region"
  ) +
  theme_minimal()
ggplot(unemp, aes(x = date, y = unemployment_rate)) +
  geom_line() +
  facet_wrap(~ region) +
  labs(
    title = "Monthly Unemployment Rates by Region",
    x = "Date",
    y = "Unemployment Rate (%)"
  ) +
  theme_minimal()
ggplot(unemp_summary,
       aes(x = fct_reorder(region, mean_rate),
           y = mean_rate)) +
  geom_col() +
  labs(
    title = "Average Unemployment Rate by Region",
    x = "Region",
    y = "Average Unemployment Rate (%)"
  ) +
  theme_minimal()
ggplot(unemp_recent,
       aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +
  labs(
    title = "Unemployment Rates Since 2015",
    x = "Date",
    y = "Unemployment Rate (%)",
    color = "Region"
  ) +
  theme_minimal()