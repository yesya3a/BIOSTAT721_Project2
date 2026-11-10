library(tidyverse)

# Load the cleaning function
source("functions.R")

# Clean each year's data
data_2018 <- clean_air_quality("data/AQ_2018.csv")
data_2019 <- clean_air_quality("data/AQ_2019.csv")
data_2020 <- clean_air_quality("data/AQ_2020.csv")

# Combine all three years
air_quality <- bind_rows(
  data_2018,
  data_2019,
  data_2020
)

# View combined data
head(air_quality)

# Check the dataset
glimpse(air_quality)

# Check for duplicate dates
sum(duplicated(air_quality$Date))

# Create summary tables for each year

summary_tables <- air_quality %>%
  mutate(Year = format(Date, "%Y")) %>%
  group_by(Year) %>%
  summarise(
    CO_min = min(CO, na.rm = TRUE),
    CO_max = max(CO, na.rm = TRUE),
    PM25_min = min(PM25, na.rm = TRUE),
    PM25_max = max(PM25, na.rm = TRUE),
    O3_min = min(O3, na.rm = TRUE),
    O3_max = max(O3, na.rm = TRUE),
    .groups = "drop"
  )

print(summary_tables)

# Create a formatted table for one year
make_summary_table <- function(year) {
  
  air_quality %>%
    filter(format(Date, "%Y") == as.character(year)) %>%
    summarise(
      `CO (ppm)` = list(c(min(CO, na.rm = TRUE),
                          max(CO, na.rm = TRUE))),
      `PM2.5 (ug/m3)` = list(c(min(PM25, na.rm = TRUE),
                               max(PM25, na.rm = TRUE))),
      `O3 (ppm)` = list(c(min(O3, na.rm = TRUE),
                          max(O3, na.rm = TRUE)))
    ) %>%
    pivot_longer(
      everything(),
      names_to = "Pollutant",
      values_to = "Values"
    ) %>%
    mutate(
      Minimum = sapply(Values, `[`, 1),
      Maximum = sapply(Values, `[`, 2)
    ) %>%
    select(-Values) %>%
    mutate(across(c(Minimum, Maximum), ~ sprintf("%.3f", .x)))
}

# Generate three tables
table_2018 <- make_summary_table(2018)
table_2019 <- make_summary_table(2019)
table_2020 <- make_summary_table(2020)

table_2018
table_2019
table_2020


# Prepare monthly data for Plot 1

monthly_data <- air_quality %>%
  mutate(Month = lubridate::month(Date)) %>%
  select(Month, CO, O3) %>%
  pivot_longer(
    cols = c(CO, O3),
    names_to = "Pollutant",
    values_to = "Concentration"
  ) %>%
  group_by(Month, Pollutant) %>%
  summarise(
    Mean = mean(Concentration, na.rm = TRUE),
    SD = sd(Concentration, na.rm = TRUE),
    N = sum(!is.na(Concentration)),
    .groups = "drop"
  ) %>%
  mutate(
    Lower = Mean - 1.96 * SD / sqrt(N),
    Upper = Mean + 1.96 * SD / sqrt(N)
  )

head(monthly_data)

# Plot 1: Monthly CO and O3 concentrations

plot1 <- ggplot(
  monthly_data,
  aes(x = Month, y = Mean, color = Pollutant)
) +
  geom_point(size = 2.5) +
  geom_errorbar(
    aes(ymin = Lower, ymax = Upper),
    width = 0.2
  ) +
  facet_wrap(~ Pollutant, scales = "free_y") +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  scale_color_manual(
    values = c("CO" = "#3B82A0", "O3" = "#D97757")
  ) +
  labs(
    title = "Monthly Average CO and O3 Concentrations",
    subtitle = "RDU Airport, 2018–2020",
    x = "Month",
    y = "Concentration (ppm)",
    color = "Pollutant",
    caption = "Error bars represent 95% confidence intervals."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

plot1

# Prepare monthly PM2.5 data for Plot 2

pm25_monthly <- air_quality %>%
  mutate(
    Year = factor(lubridate::year(Date)),
    Month = lubridate::month(Date)
  ) %>%
  group_by(Year, Month) %>%
  summarise(
    Mean_PM25 = mean(PM25, na.rm = TRUE),
    .groups = "drop"
  )
# Plot 2: Monthly PM2.5 concentrations by year

plot2 <- ggplot(
  pm25_monthly,
  aes(x = Month, y = Mean_PM25, color = Year, group = Year)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  scale_color_manual(
    values = c(
      "2018" = "#3B82A0",
      "2019" = "#D97757",
      "2020" = "#7D70A8"
    )
  ) +
  labs(
    title = "Monthly Average PM2.5 Concentrations by Year",
    subtitle = "RDU Airport, 2018–2020",
    x = "Month",
    y = expression("PM2.5 Concentration (" * mu * "g/m"^3 * ")"),
    color = "Year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

plot2