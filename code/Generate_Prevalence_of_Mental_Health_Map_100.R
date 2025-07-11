# Load necessary libraries
library(sf)
library(dplyr)
library(DBI)
library(odbc)
library(viridis)
library(tidyr)
library(tmap)
library(ggspatial)
library(colorblindcheck)

# Connect to the SQL Server
sql_conn <- dbConnect(odbc::odbc(),
                      driver = "SQL Server",
                      server = "BHTS-CONNECTYOR",
                      database = "CB_PAA_2514",
                      Trusted_Connection = "True",
                      Timeout = 600)  # Set timeout to 600 seconds

# Query cohort data (Mental Health cases)
cohort_query <- "SELECT LSOA, person_id FROM [CB_PAA_2514].[dbo].[tbl_Mental_Health_distinct]"
cohort_data <- dbGetQuery(sql_conn, cohort_query)

# Query population data (Total population per LSOA)
population_query <- "SELECT LSOA, total_population FROM [CB_PAA_2514].[dbo].[LSOA_Population]"
population_data <- dbGetQuery(sql_conn, population_query)

# Close the SQL connection
dbDisconnect(sql_conn)

# Aggregate total cases per LSOA from cohort data
aggregated_cases <- cohort_data %>%
  filter(!is.na(LSOA)) %>%
  group_by(LSOA) %>%
  summarise(total_cases = n(), .groups = 'drop')

# Calculate prevalence per 100
aggregated_data <- population_data %>%
  left_join(aggregated_cases, by = "LSOA") %>%
  mutate(
    total_cases = ifelse(is.na(total_cases), 0, total_cases),  # Replace NA cases with 0
    rate_per_100 = ifelse(total_population > 0, (total_cases * 100) / total_population, 0)
  )

# Load the shapefile
shapefile_path <- "C:/Users/AsowoAyobodeP/Desktop/Bode BTHF/SRCode_SHAREANALYSIS/CHC_Fresh/Bradford shapefile"
lsoa_shapefile <- st_read(shapefile_path)

# Check that the shapefile has the correct LSOA column
if (!"LSOA21CD" %in% names(lsoa_shapefile)) {
  stop("The shapefile does not contain the 'LSOA21CD' column.")
}

# Join aggregated data with shapefile
aggregated_data_sf <- lsoa_shapefile %>%
  left_join(aggregated_data, by = c("LSOA21CD" = "LSOA"))

# Handle missing values for total_cases and rate_per_100
aggregated_data_sf$total_cases <- ifelse(is.na(aggregated_data_sf$total_cases), 0, aggregated_data_sf$total_cases)
aggregated_data_sf$rate_per_100 <- ifelse(is.na(aggregated_data_sf$rate_per_100), 0, aggregated_data_sf$rate_per_100)

# Transform CRS to WGS84
aggregated_data_sf <- st_transform(aggregated_data_sf, crs = 4326)

# Define a reusable color palette and breaks
color_palette <- "viridis"
custom_breaks <- function(values) {
  quantile(values, probs = seq(0, 1, 0.2), na.rm = TRUE)
}

# Generate breaks for population and rates
population_breaks <- custom_breaks(aggregated_data_sf$total_population)
rate_breaks <- custom_breaks(aggregated_data_sf$rate_per_100)

# Visualization with consistent palette
tmap_mode("view")

# Define the population map
map_population <- tm_shape(aggregated_data_sf) +
  tm_polygons("total_population",
              palette = color_palette,
              title = "Total Population",
              breaks = population_breaks,
              colorNA = "grey90") +
  tm_text("LSOA21NM", size = 0.7, col = "black") +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Total Population Distribution")

# Define the prevalence map
map_rate <- tm_shape(aggregated_data_sf) +
  tm_polygons("rate_per_100",
              palette = color_palette,
              title = "Mental Health Cases per 100 People",
              breaks = rate_breaks,
              colorNA = "grey90") +
  tm_text("LSOA21NM", size = 0.7, col = "black") +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Prevalence of Mental Health Cases")

# Save each map individually as HTML
tmap_save(tm = map_population, filename = "Total_Population_Map_100.html")
tmap_save(tm = map_rate, filename = "Prevalence_of_Mental_Health_Map_100.html")
