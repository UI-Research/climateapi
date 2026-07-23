# Wildfire Burn Zones

## Overview

The
[`get_wildfire_burn_zones()`](https://ui-research.github.io/climateapi/reference/get_wildfire_burn_zones.md)
function provides access to a harmonized dataset of wildfire burn zone
disasters in the United States from 2000-2025.

## Data sources and methodology

This dataset combines six authoritative wildfire data sources to
identify wildfires that burned near communities and resulted in civilian
fatalities, destroyed structures, or received federal disaster relief.
These sources are:

- **FIRED (Fire Event Delineation)**: Satellite-derived fire perimeters
- **MTBS (Monitoring Trends in Burn Severity)**: Burn severity and
  perimeters for large fires
- **NIFC (National Interagency Fire Center)**: Official fire incident
  data
- **ICS-209 (Incident Status Summary)**: Incident management records
- **RedBook**: Historical wildfire statistics
- **FEMA**: Federal disaster declarations

Wildfires are classified as “disasters” if they:

1.  Burned near a community AND
2.  Resulted in at least one of:
    - Civilian fatality
    - Destroyed structure
    - Federal disaster relief

These data are described and provided in association with the journal
article: Wilner, L.B., Piepmeier, L., Gordon, M. et al. Two and a half
decades of United States wildfire burn zone disaster data, 2000-2025.
Sci Data 12, 1948 (2025). <https://doi.org/10.1038/s41597-025-06226-8>.

## Loading the data

``` r

library(climateapi)
library(tidyverse)
library(sf)
library(urbnthemes)

set_urbn_defaults(style = "print")
```

``` r

burn_zones <- get_wildfire_burn_zones()
```

## Data structure

Each row in the dataset represents a single wildfire burn zone disaster.

``` r

glimpse(burn_zones)
#> Rows: 8,010
#> Columns: 19
#> $ wildfire_id                          <dbl> 1, 1, 2, 3, 4, 5, 6, 7, 8,…
#> $ id_fema                              <chr> NA, NA, "FM-5491-OK", NA, …
#> $ year                                 <int> 2018, 2018, 2024, 2017, 20…
#> $ wildfire_name                        <chr> "DONNELL", "DONNELL", "57"…
#> $ state_fips                           <chr> "06", "06", "40", "12", "0…
#> $ county_fips                          <chr> "06003", "06109", "40153",…
#> $ county_name                          <chr> "Alpine County", "Tuolumne…
#> $ area_sq_km                           <dbl> 146.200893, 146.200893, 19…
#> $ wildfire_complex_binary              <lgl> FALSE, FALSE, FALSE, FALSE…
#> $ date_start                           <date> 2018-08-01, 2018-08-01, 2…
#> $ date_containment                     <date> 2018-10-31, 2018-10-31, 2…
#> $ fatalities_total                     <int> NA, NA, NA, NA, NA, NA, NA…
#> $ injuries_total                       <int> 6, 6, 2, NA, NA, 1, NA, NA…
#> $ structures_destroyed                 <int> 135, 135, 1, 19, 5, 4, 4, …
#> $ structures_threatened                <int> NA, NA, 1720, NA, 0, 0, NA…
#> $ evacuation_total                     <int> NA, NA, NA, NA, NA, 7000, …
#> $ wui_type                             <chr> NA, NA, NA, "intermix", NA…
#> $ density_people_sq_km_wildfire_buffer <dbl> 0.07433545, 0.07433545, 4.…
#> $ geometry                             <POLYGON [m]> POLYGON ((-2033577…
```

Key variables include:

- `wildfire_id`: Unique identifier for each wildfire event
- `year`: Year the wildfire occurred (2000-2025)
- `wildfire_name`: Name of the wildfire or fire complex
- `county_fips`: Pipe-delimited string of county FIPS codes for all
  affected counties
- `county_name`: Pipe-delimited string of county names for all affected
  counties
- `area_sq_km`: Burned area in square kilometers
- `fatalities_total` and `injuries_total`: Human impacts
- `structures_destroyed` and `structures_threatened`: Built environment
  impacts
- `geometry`: Burn zone polygon boundaries

## Example analyses

### Annual trends in wildfire disasters

``` r

# Extract state FIPS from the first county in the pipe-delimited list
df1 = burn_zones |>
  st_drop_geometry() |>
  mutate(state_fips = str_sub(county_fips, 1, 2)) |>
  summarize(
    .by = c(year, state_fips),
    n_wildfires = n(),
    total_area_sq_km = sum(area_sq_km, na.rm = TRUE),
    total_structures_destroyed = sum(structures_destroyed, na.rm = TRUE)) |>
  left_join(
    tigris::fips_codes %>% distinct(state, state_code),
    by = c("state_fips" = "state_code"))

top_five_states = df1 %>%
  arrange(desc(n_wildfires)) %>%
  distinct(state) %>%
  slice(1:5)

df1 %>%
  filter(state %in% top_five_states$state) %>%
  mutate(state = factor(state, levels = top_five_states %>% pull(state), ordered = TRUE)) %>%
  ggplot(aes(x = year, y = n_wildfires)) +
  geom_col() +
  labs(
    title = "Many states frequently experience more than 50 wildfires per year",
    subtitle = "Disasters defined as wildfires causing fatalities, structure loss, or federal relief",
    x = "",
    y = "Number of wildfires") +
  facet_wrap(~state)
```

![plot of chunk
annual-trends](figure/get_wildfire_burn_zones-annual-trends-1.png)

plot of chunk annual-trends

### Geographic distribution of impacts

``` r

state_impacts <- burn_zones |>
  st_drop_geometry() |>
  mutate(state_fips = str_sub(county_fips, 1, 2)) |>
  summarize(
    .by = state_fips,
    n_wildfires = n_distinct(wildfire_id),
    total_structures_destroyed = sum(structures_destroyed, na.rm = TRUE),
    total_fatalities = sum(fatalities_total, na.rm = TRUE)
  ) |>
  left_join(
    tidycensus::fips_codes |>
      distinct(state_code, state_name),
    by = c("state_fips" = "state_code")
  ) |>
  filter(!is.na(state_name))

state_impacts |>
  slice_max(n_wildfires, n = 10) |>
  mutate(state_name = fct_reorder(state_name, n_wildfires)) |>
  ggplot(aes(y = state_name, x = n_wildfires)) +
  geom_col() +
  labs(
    title = "States with Most Wildfire Burn Zone Disasters (2000-2025)",
    x = "Number of distinct wildfires",
    y = ""
  )
```

![plot of chunk
state-impacts](figure/get_wildfire_burn_zones-state-impacts-1.png)

plot of chunk state-impacts

### Mapping wildfire burn zones

``` r

# Get wildfires from a recent year in California
ca_2020_fires <- burn_zones |>
  filter(str_detect(county_fips, "^06"), year == 2020)

# Get California counties for context
ca_counties <- tigris::counties(state = "CA", cb = TRUE, year = 2022, progress_bar = FALSE) |>
  st_transform(5070)

ggplot() +
  geom_sf(data = ca_counties, fill = "grey95", color = "grey70") +
  geom_sf(data = ca_2020_fires, aes(fill = structures_destroyed), alpha = 0.8) +
  scale_fill_continuous(trans = "reverse") +
  labs(
    title = "California Wildfire Burn Zone Disasters (2020)",
    subtitle = "Burn zone perimeters colored by structures destroyed",
    fill = "Structures\ndestroyed") +
  theme_urbn_map()
```

![plot of chunk
map-example](figure/get_wildfire_burn_zones-map-example-1.png)

plot of chunk map-example

### Analyzing structure loss severity

``` r

burn_zones |>
  st_drop_geometry() |>
  distinct(wildfire_id, year, wildfire_name, structures_destroyed) |>
  filter(!is.na(structures_destroyed), structures_destroyed > 0) |>
  mutate(
    severity = case_when(
      structures_destroyed >= 1000 ~ "1,000+ structures",
      structures_destroyed >= 100 ~ "100-999",
      structures_destroyed >= 10 ~ "10-99",
      TRUE ~ "1-9 structures"),
    severity = factor(
      severity,
      levels = c("1-9 structures", "10-99", "100-999", "1,000+ structures"))) |>
  count(year, severity) |>
  mutate(
    .by = year,
    percent = n / sum(n, na.rm = TRUE)) |>
  ggplot(aes(x = year, y = percent, fill = severity)) +
  geom_col() +
  scale_fill_manual(values = c(
    "1-9 structures" = palette_urbn_cyan[1],
    "10-99" = palette_urbn_cyan[3],
    "100-999" = palette_urbn_cyan[5],
    "1,000+ structures" = palette_urbn_cyan[7])) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Most wildifres destroy under 10 structures",
    x = "",
    y = "")
```

![plot of chunk
severity-analysis](figure/get_wildfire_burn_zones-severity-analysis-1.png)

plot of chunk severity-analysis

## See also

- [`get_current_fire_perimeters()`](https://ui-research.github.io/climateapi/reference/get_current_fire_perimeters.md):
  Access current/active wildfire perimeters
- [`get_fema_disaster_declarations()`](https://ui-research.github.io/climateapi/reference/get_fema_disaster_declarations.md):
  FEMA disaster declarations including fire-related declarations
- [`get_structures()`](https://ui-research.github.io/climateapi/reference/get_structures.md):
  Estimate structures within geographic boundaries
