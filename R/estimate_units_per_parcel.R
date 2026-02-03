####----HELPER FUNCTIONS----####
## 01: Deriving Tract-level Improvement Values to Assist with Imputation
prepare_improvement_values = function(data,  building_count_variable) {
  ## calculating medians/means at the tract level
  parcel_improvement_values1a = data %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(
      building_count == .data[[building_count_variable]],
      building_count >= 1) %>%
    dplyr::group_by(tract_geoid, municipality_name) %>%
    dplyr::summarize(
      municipality_name = dplyr::first(municipality_name),
      parcel_count = n(),
      mean_value_improvement = mean(value_improvement, na.rm = TRUE),
      median_value_improvement = stats::median(value_improvement, na.rm = TRUE)) %>%
    dplyr::ungroup()

  parcel_improvement_values1b = dplyr::bind_rows(
    parcel_improvement_values1a,
    data %>%
      dplyr::filter(!tract_geoid %in% parcel_improvement_values1a$tract_geoid) %>%
      dplyr::select(tract_geoid) %>%
      sf::st_drop_geometry %>%
      dplyr::distinct())

  ## calculating medians/means at the jurisdiction level
  parcel_improvement_values1c = data %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(
      building_count == .data[[building_count_variable]],
      building_count >= 1) %>%
    dplyr::group_by(municipality_name) %>%
    dplyr::summarize(
      parcel_count_jurisdiction = n(),
      mean_value_improvement_jurisdiction = mean(value_improvement, na.rm = TRUE),
      median_value_improvement_jurisdiction = stats::median(value_improvement, na.rm = TRUE)) %>%
    dplyr::ungroup()

  ## for tracts with tract-level parcel counts below 30, we use the jurisdiction-level values
  parcel_improvement_values = dplyr::left_join(
    parcel_improvement_values1b,
    parcel_improvement_values1c,
    by = "municipality_name") %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::matches("_value_improvement$"),
        .fns = ~ dplyr::if_else(is.na(.x) | parcel_count < 30, get(stringr::str_c(dplyr::cur_column(), "_jurisdiction")), .x)))
}

## 02: Aligning Imputed Unit Counts with ACS Estimates
benchmark_units_to_census = function(data) {
  data %>%
    dplyr::group_by(tract_geoid) %>%
    ## calculate the tract-level differences between imputed and ACS-reported units,
    ## by units-in-structure category
    dplyr::mutate(
      count_units_1 = sum(residential_unit_categories == "1", na.rm = TRUE),
      count_units_2 = sum(residential_unit_categories == "2", na.rm = TRUE),
      count_units_3_4 = sum(residential_unit_categories == "3-4", na.rm = TRUE),
      count_units_5_9 = sum(residential_unit_categories == "5-9", na.rm = TRUE),
      count_units_10_19 = sum(residential_unit_categories == "10-19", na.rm = TRUE),
      count_units_20_49 = sum(residential_unit_categories == "20-49", na.rm = TRUE),
      count_units_50_more = sum(residential_unit_categories == "50+", na.rm = TRUE),
      imputed_units_1 = sum(residential_unit_count[residential_unit_categories == "1"], na.rm = TRUE),
      imputed_units_2 = sum(residential_unit_count[residential_unit_categories == "2"], na.rm = TRUE),
      imputed_units_3_4 = sum(residential_unit_count[residential_unit_categories == "3-4"], na.rm = TRUE),
      imputed_units_5_9 = sum(residential_unit_count[residential_unit_categories == "5-9"], na.rm = TRUE),
      imputed_units_10_19 = sum(residential_unit_count[residential_unit_categories == "10-19"], na.rm = TRUE),
      imputed_units_20_49 = sum(residential_unit_count[residential_unit_categories == "20-49"], na.rm = TRUE),
      imputed_units_50_more = sum(residential_unit_count[residential_unit_categories == "50+"], na.rm = TRUE),
      dplyr::across(
        .cols = dplyr::matches("acs_units_"),
        .fns = ~ get(stringr::str_replace(dplyr::cur_column(), "acs", "imputed")) - .x,
        .names = "{.col}_difference")) %>%
    dplyr::ungroup() %>%
    ## distribute the mean, parcel-level difference (calculated above) across all
    ## parcels within each tract so that imputed counts are closer to reported counts
    dplyr::mutate(
      residential_unit_count_adjusted = dplyr::case_when(
        residential_unit_categories == "2" ~
          pmax(residential_unit_count - (acs_units_2_difference / count_units_2), 1),
        residential_unit_categories == "3-4" ~
          pmax(residential_unit_count - (acs_units_3_4_difference / count_units_3_4), 2),
        residential_unit_categories == "5-9" ~
          pmax(residential_unit_count - (acs_units_5_9_difference / count_units_5_9), 3),
        residential_unit_categories == "10-19" ~
          pmax(residential_unit_count - (acs_units_10_19_difference / count_units_10_19), 5),
        residential_unit_categories == "20-49" ~
          pmax(residential_unit_count - (acs_units_20_49_difference / count_units_20_49), 10),
        residential_unit_categories == "50+" ~
          pmax(residential_unit_count - (acs_units_50_more_difference / count_units_50_more), 20),
        TRUE ~ residential_unit_count) %>%
        round(0),
      ## reclassify each parcel's adjusted unit count
      residential_unit_categories = dplyr::case_when(
        is.na(residential_unit_count_adjusted) ~ "0",
        round(residential_unit_count_adjusted, 0) == 0 ~ "0",
        round(residential_unit_count_adjusted, 0) == 1 ~ "1",
        round(residential_unit_count_adjusted, 0) == 2 ~ "2",
        round(residential_unit_count_adjusted, 0) >= 50 ~ "50+",
        round(residential_unit_count_adjusted, 0) >= 20 ~ "20-49",
        round(residential_unit_count_adjusted, 0) >= 10 ~ "10-19",
        round(residential_unit_count_adjusted, 0) >= 5 ~ "5-9",
        round(residential_unit_count_adjusted, 0) >= 3 ~ "3-4") %>%
        factor(levels = c("0", "1", "2", "3-4", "5-9", "10-19", "20-49", "50+"),
               ordered = TRUE)) %>%
    dplyr::select(
      parcel_id,
      tract_geoid,
      jurisdiction,
      municipality_name,
      residential_unit_count = residential_unit_count_adjusted,
      residential_unit_categories,
      dplyr::matches("median|mean"),
      c(dplyr::matches("acs_units_"), -dplyr::matches("difference")))
}

####----MAIN FUNCTION----####
#' @title Estimate the number and types of structures per parcel
#'
#' @param structures A dataset returned by `get_structure()`.
#' @param parcels A spatial (polygon) dataset.
#' @param zoning A spatial (polygon) zoning dataset.
#' @param acs Optionally, a non-spatial dataset, at the tract level, returned from `urbnindicators::compile_acs_data()`.
#'
#' @return An `sf` object (point geometry, representing parcel centroids) containing the input parcel data augmented with estimated residential unit information. The returned object includes:
#' \describe{
#'   \item{parcel_id}{Character or numeric. The unique parcel identifier from the input data.}
#'   \item{tract_geoid}{Character. The 11-digit Census tract GEOID containing the parcel centroid.}
#'   \item{jurisdiction}{Character. The jurisdiction name associated with the parcel.}
#'   \item{municipality_name}{Character. The municipality name associated with the parcel.}
#'   \item{residential_unit_count}{Numeric. The estimated number of residential units on the parcel, benchmarked against ACS estimates at the tract level.}
#'   \item{residential_unit_categories}{Factor (ordered). Categorical classification of unit counts: "0", "1", "2", "3-4", "5-9", "10-19", "20-49", "50+".}
#'   \item{median_value_improvement_sf}{Numeric. Tract-level median improvement value for single-family parcels.}
#'   \item{median_value_improvement_mh}{Numeric. Tract-level median improvement value for manufactured home parcels.}
#'   \item{acs_units_N}{Numeric columns (acs_units_1, acs_units_2, etc.). ACS-reported housing unit counts by units-in-structure category for the tract.}
#'   \item{zone}{Character. Zoning designation from the zoning dataset.}
#'   \item{zoned_housing_type}{Character. Housing type allowed by zoning.}
#'   \item{far}{Numeric. Floor area ratio.}
#'   \item{setback_front}{Numeric. Front setback requirement in feet.}
#'   \item{setback_rear}{Numeric. Rear setback requirement in feet.}
#'   \item{setback_side}{Numeric. Side setback requirement in feet.}
#'   \item{height_maximum}{Numeric. Maximum building height allowed.}
#' }
#' @export
estimate_units_per_parcel = function(
    structures,
    parcels,
    zoning,
    acs = NULL) {

  relevant_geographies = get_spatial_extent_census(zoning)

  if ("state" %in% relevant_geographies$geography) {
    relevant_geographies = purrr::map_dfr(
        relevant_geographies$state_geoid,
        ~ tigris::tracts(cb = TRUE, year = 2022, state = .x)) %>%
      dplyr::transmute(GEOID, geography = "tract") %>%
      sf::st_drop_geometry() }

  ## in the case that sociodemographic data are not provided by the user,
  ## we query them
  if (is.null(acs)) {
    acs = urbnindicators::compile_acs_data(
        variables = NULL,
        years = c(2023),
        geography = "tract",
        states = relevant_geographies %>%
          dplyr::mutate(state_geoid = stringr::str_sub(GEOID, 1, 2)) %>%
          dplyr::pull(state_geoid) %>% unique(),
        counties = NULL,
        spatial = TRUE) %>%
      sf::st_as_sf() %>%
      sf::st_transform(projection) %>%
      dplyr::filter(GEOID %in% relevant_geographies$GEOID) } else {
    acs = acs %>%
      sf::st_as_sf() %>%
      sf::st_transform(projection) %>%
      dplyr::filter(GEOID %in% relevant_geographies$GEOID) }


  warning("Need to update code to address overlay zones.")
  ## join together our core input datasets
  parcel_structures_zoning1 = parcels %>% ## polygons
    sf::st_join(structures) %>%
    sf::st_point_on_surface() %>% ## points
    ## omitting overlay zones so that we have a one-to-one match
    ## (and because we have no useful information about the overlay zones...)
    sf::st_join(zoning) %>%
    dplyr::mutate(record_id = dplyr::row_number())

  ####----Parcel Centroids Fall in Unzoned Streets----####
  ## some parcels span streets; streets are not (always) zoned in the spatial data
  ## when the centroid (or point on surface) of a parcel falls within the street
  ## it does not join to the zoning data. accordingly, we take these parcels and
  ## if they're located within an incorporated jurisdiction, we join them to the
  ## nearest zoning feature
  parcels_no_matching_zoning = parcel_structures_zoning1 %>%
    dplyr::filter(is.na(zoning_type)) %>%
    sf::st_filter(mobile_places_acs %>% dplyr::filter(!stringr::str_detect(NAME, "CDP"))) %>%
    sf::st_join(x = ., y = zoning, join = sf::st_nearest_feature) %>%
    dplyr::select(-dplyr::matches(".x")) %>%
    dplyr::rename_with(.cols = dplyr::matches("\\.y"), .fn = ~ stringr::str_remove(.x, "\\.y"))

  parcel_structures_zoning = dplyr::bind_rows(
    parcel_structures_zoning1 %>%
      dplyr::filter(!record_id %in% parcels_no_matching_zoning$record_id),
    parcels_no_matching_zoning)

  ####----Aggregating Building Footprints to the Parcel Level----####
  non_residential_types = structures %>%
    dplyr::filter(
      occupancy_class != "Residential",
      occupancy_primary != "Unclassified") %>%
    dplyr::pull(occupancy_primary) %>%
    unique()

  residential_types = structures %>%
    dplyr::filter(
      occupancy_class %in% c("Residential", "Unclassified"),
      occupancy_primary != "Unclassified") %>%
    dplyr::pull(occupancy_primary) %>%
    unique()

  ## for speed, we drop geometries here and join them back following these
  ## calculations
  summarized_parcels1 = parcel_structures_zoning %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(parcel_id) %>%
    dplyr::summarize(
      building_count = dplyr::n_distinct(building_id, na.rm = TRUE),
      other_building_count = sum(
        occupancy_primary %in% c("Temporary Lodging", "Institutional Dormitory", "Nursing Home")),
      unclassified_building_count = sum(occupancy_primary == "Unclassified"),
      multifamily_building_count = sum(occupancy_primary == "Multi - Family Dwelling"),
      singlefamily_building_count = sum(occupancy_primary %in% c("Single Family Dwelling")),
      manufactured_building_count = sum(occupancy_primary %in% c("Manufactured Home")),
      nonresidential_building_count = sum(occupancy_primary %in% non_residential_types),
      residential_building_count = sum(occupancy_primary %in% residential_types),
      footprint_building_sqm = sum(building_area_sqm),
      average_building_footprint_sqm = sum(building_area_sqm / building_count),
      dplyr::across(
        .cols = c(
          acreage, condo_unit_count, dplyr::matches("value"), zoning_type, treatment, zone, jurisdiction,
          municipality_name, building_id),
        .fns = dplyr::first)) %>%
    dplyr::ungroup()

  ## joining back geometries
  summarized_parcels = parcels %>%
    dplyr::select(parcel_id) %>%
    sf::st_centroid() %>%
    dplyr::left_join(summarized_parcels1, by = "parcel_id") %>%
    sf::st_join(
      acs %>%
        dplyr::select(c(
          tract_geoid = GEOID,
          dplyr::matches("units_in_structure.*renter.*owner"),
          -dplyr::matches("percent|_M$|_cv$"))))

  ####----Imputing Parcel Residential Unit Counts----####

  ## calculating tract-level summary statistics describing improvement values
  single_family_parcel_improvement_values = prepare_improvement_values(
    data = summarized_parcels,
    building_count_variable = "singlefamily_building_count")
  manufactured_home_parcel_improvement_values = prepare_improvement_values(
    data = summarized_parcels,
    building_count_variable = "manufactured_building_count")

  #joining these tract-level summary statistics to our summarized_parcels dataset
  final_parcel <- summarized_parcels %>%
    dplyr::left_join(
      single_family_parcel_improvement_values %>%
        dplyr::select(
          municipality_name, tract_geoid,
          median_value_improvement_sf = median_value_improvement),
      by = c("tract_geoid", "municipality_name"),
      relationship = "many-to-one") %>%
    dplyr::left_join(
      manufactured_home_parcel_improvement_values %>%
        dplyr::select(
          municipality_name, tract_geoid,
          median_value_improvement_mh = median_value_improvement),
      by = c("tract_geoid", "municipality_name"),
      relationship = "many-to-one")

  # applying assumptions to arrive at preliminary estimated unit counts
  classified_parcels1 = final_parcel %>%
    dplyr::mutate(
      residential_unit_count = dplyr::case_when(
        !is.na(condo_unit_count) & residential_building_count > 0 ~ condo_unit_count,
        # for parcels with multifamily buildings
        multifamily_building_count > 0 &
          # where the residential share of all buildings times the estimated number of
          # single-family units (i.e., value_improvement / median_value_improvement_sf)
          # is greater than 20
          ((residential_building_count / building_count) *
             (value_improvement / median_value_improvement_sf) >= 20) ~
          # return the residential share of all buildings times the estimated number
          # of single-family units times 2.5 (because we assume these multifamily units
          # in large multifamily developments are much more affordable than single-family units)
          (residential_building_count / building_count) *
          (value_improvement / (median_value_improvement_sf * 2.5)),
        # for sub-20-unit multifamily developments, return the estimated sf unit count
        # (because we assume these units are roughly equivalent in price to an SF unit)
        multifamily_building_count > 0 &
          ((residential_building_count / building_count) *
             (value_improvement / median_value_improvement_sf) < 20) ~
          (residential_building_count / building_count) *
          (value_improvement / (median_value_improvement_sf)),
        # no buildings = no residential units
        building_count == 0 ~ 0,
        # all buildings are nonresidential = no residential units
        nonresidential_building_count == building_count ~ 0,
        # 1 manufactured home = 1 residential unit
        manufactured_building_count == 1 ~ 1,
        # 1 residential building with an improvement value < 600000 = 1 unit
        residential_building_count == 1 & value_improvement < 600000 ~ 1,
        # 1 unclassified building with an improvement value < 600000 = 1 unit, provided
        # it's in the county (no zoning) or is zoned "not residential" (i.e., it predates
        # the curent ordinance)
        unclassified_building_count == 1 &
          building_count == 1 &
          value_improvement < 600000 &
          (is.na(zoned_housing_type) | zoned_housing_type != "Not residential") ~ 1,
        # all buildings are unclassified = 0 residential units
        unclassified_building_count == building_count ~ 0,
        # one singlefamily building = 1 residential unit
        singlefamily_building_count == 1 ~ 1,
        # other buildings (dorms, elder housing, etc.) are akin to multifamily
        other_building_count > 0 ~
          (residential_building_count / building_count) *
          (value_improvement / median_value_improvement_sf),
        # parcels zoned single-family with 1-2 buildings that are not multifamily
        # have one residential unit
        zoned_housing_type == "One family" &
          building_count < 3 &
          multifamily_building_count == 0 ~ 1,
        # parcels zoned not residential with no multifamily have 0 units
        zoned_housing_type == "Not residential" &
          multifamily_building_count == 0 ~ 0,
        # parcels with no zoning (the county) and 1-2 buildings with a residential
        # building count and an improvement value under 600000 have one unit
        is.na(zoned_housing_type) &
          building_count < 3 &
          residential_building_count > 0 &
          value_improvement < 600000 ~ 1,
        # manufactured home parks have a residential unit count equal to the number
        # of manufactured homes
        manufactured_building_count > 0 ~ manufactured_building_count,
        # parcels with more than one residential building are treated akin to
        # multifamily
        residential_building_count > 1 & multifamily_building_count == 0 ~
          (residential_building_count / building_count) *
          (value_improvement / median_value_improvement_sf)),
      # max units on a parcel 300, regardless of above
      residential_unit_count = dplyr::case_when(
        residential_unit_count > 300 ~ 300,
        # if there are more residential buildings than units (as calculated above),
        # return the building count
        residential_building_count > 3 &
          residential_building_count > residential_unit_count ~
          residential_building_count,
        TRUE ~ residential_unit_count),
      residential_unit_categories = dplyr::case_when(
        is.na(residential_unit_count) ~ "0",
        round(residential_unit_count, 0) == 0 ~ "0",
        round(residential_unit_count, 0) == 1 ~ "1",
        round(residential_unit_count, 0) == 2 ~ "2",
        round(residential_unit_count, 0) >= 50 ~ "50+",
        round(residential_unit_count, 0) >= 20 ~ "20-49",
        round(residential_unit_count, 0) >= 10 ~ "10-19",
        round(residential_unit_count, 0) >= 5 ~ "5-9",
        round(residential_unit_count, 0) >= 3 ~ "3-4") %>%
        factor(levels = c("0", "1", "2", "3-4", "5-9", "10-19", "20-49", "50+"), ordered = TRUE),
      tenure_by_units_in_structure_renter_owner_occupied_housing_units_1 =
        tenure_by_units_in_structure_renter_owner_occupied_housing_units_mobile_home +
        tenure_by_units_in_structure_renter_owner_occupied_housing_units_1_detached +
        tenure_by_units_in_structure_renter_owner_occupied_housing_units_1_attached) %>%
    dplyr::rename_with(
      .cols = dplyr::matches("tenure_by_units"),
      .fn = ~ stringr::str_replace(.x, "tenure_by_units_in_structure_renter_owner_occupied_housing_", "acs_")) %>%
    dplyr::select(-acs_units, -dplyr::matches("detached|attached|boat|mobile")) %>%
    ## we repeatedly benchmark against census numbers because each time we do this,
    ## parcels shift across category boundaries in multiple directions
    benchmark_units_to_census() %>%
    benchmark_units_to_census() %>%
    benchmark_units_to_census() %>%
    benchmark_units_to_census() %>%
    benchmark_units_to_census() %>%
    benchmark_units_to_census() %>%
    benchmark_units_to_census() %>%
    benchmark_units_to_census() %>%
    benchmark_units_to_census() %>%
    benchmark_units_to_census() %>%
    ## finally, we join back our zoning attributes
    dplyr::left_join(
      parcel_buildings_zoning %>%
        sf::st_drop_geometry() %>%
        dplyr::select(
          parcel_id,
          acreage,
          zone,
          zoned_housing_type,
          far = floorarearatio,
          setback_front = frontsetbackft,
          setback_rear = rearsetbackft,
          setback_side =  sidesetbackft,
          height_maximum = maxheightft,
          maxlotcoverage_building,
          maxlotcoverage_buildingimpervious,
          maxunitsperacre,
          minparking_1br,
          minparking_2br,
          maxbedrooms,
          maxunitsperbldg,
          maxstories,
          minlotacres,
          minparking,
          minunitsqft) %>%
        dplyr::distinct(parcel_id, .keep_all = TRUE),
      by = "parcel_id")
}

utils::globalVariables(c(
  "NAME", "acreage", "acs_units", "building_area_sqm", "building_count", "building_id",
  "condo_unit_count", "floorarearatio", "frontsetbackft",
  "jurisdiction",  "maxbedrooms", "maxheightft", "maxlotcoverage_building",
  "maxlotcoverage_buildingimpervious", "maxstories", "maxunitsperacre",
  "maxunitsperbldg", "median_value_improvement", "minlotacres",
  "minparking", "minparking_1br", "minparking_2br", "minunitsqft",
  "mobile_places_acs", "municipality_name", "occupancy_primary",
  "parcel_buildings_zoning", "projection", "rearsetbackft", "record_id",
  "residential_unit_categories", "residential_unit_count",
  "residential_unit_count_adjusted", "sidesetbackft", "state_geoid",
  "tenure_by_units_in_structure_renter_owner_occupied_housing_units_1_attached",
  "tenure_by_units_in_structure_renter_owner_occupied_housing_units_1_detached",
  "tenure_by_units_in_structure_renter_owner_occupied_housing_units_mobile_home",
  "tract_geoid", "treatment", "value_improvement", "zone",
  "zoned_housing_type", "zoning_type"))
