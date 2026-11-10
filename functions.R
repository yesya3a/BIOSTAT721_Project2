library(tidyverse)

# Clean daily air quality data from a CSV file.
# file_path: Path to the input CSV file.
# o3_unit: Output unit for ozone ("ppm" or "ppb").
# Returns: A cleaned tibble with one row per day.

clean_air_quality <- function(file_path, o3_unit = "ppm") {
  
  data <- read_csv(file_path, show_col_types = FALSE)
  
  # Rename columns and convert date
  data <- data %>%
    rename(
      CO = `Daily Max 8-hour CO Concentration`,
      PM25 = `Daily Mean PM2.5 Concentration`,
      O3 = `Daily Max 8-hour Ozone Concentration`
    ) %>%
    mutate(Date = as.Date(Date, format = "%m/%d/%Y"))
  
  # Remove duplicate rows
  data <- data %>%
    distinct()
  
  # Replace negative concentrations with zero
  pollutants <- c("CO", "PM25", "O3")
  
  for (pollutant in pollutants) {
    data[[pollutant]][!is.na(data[[pollutant]]) &
                        data[[pollutant]] < 0] <- 0
  }
  
  # Calculate daily average concentrations
  data <- data %>%
    group_by(Date) %>%
    summarise(
      CO = mean(CO, na.rm = TRUE),
      PM25 = mean(PM25, na.rm = TRUE),
      O3 = mean(O3, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Convert NaN values to NA
  data <- data %>%
    mutate(across(c(CO, PM25, O3), ~ replace(.x, is.nan(.x), NA_real_)))
  
  # Convert O3 units if requested
  if (o3_unit == "ppb") {
    data <- data %>%
      mutate(O3 = O3 * 1000)
  } else if (o3_unit != "ppm") {
    stop("o3_unit must be either 'ppm' or 'ppb'")
  }
  
  return(data)
}