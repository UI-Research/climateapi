#' @title Calculate the number of units allowed on a parcel
#'
#' @param parcels A dataframe containing parcel-level attributes and zoning regulations
#' @param development_size_maximum A cap on the number of units that can be developed on a parcel. Note that larger values increase the length of computation.
#' @param standard_unit_sqft_multifamily The square footage of a standard multifamily unit
#' @param standard_parking_stall_sqft The square footage of a standard parking space
#' @param parking_model One of c("singlestory", "multistory"). How to model parking requirements: are stalls all on the ground floor or distributed vertically?
#'
#' @return The input parcels dataframe with an additional column, `maximum_development_capacity_zoned`, that contains the maximum number of units that can be developed on each parcel per zoning regulations
#' @export
#'
#' @examples
#' df = tibble::tibble(
#'  parcel_id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#'  parcel_area_sqft = c(1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000),
#'  setback_front = c(10),
#'  setback_rear = c(10),
#'  setback_side = c(10),
#'  parcel_area_sqft_minimum = c(1000, 1000, 1000, 1000, 1000, 5000, 5000, 5000, 5000, 5000),
#'  units_per_parcel_maximum = c(10),
#'  units_per_acre_maximum = NA,
#'  parcel_coverage_percent_maximum = c(70),
#'  parcel_coverage_percent_maximum_building = c(70),
#'  open_space_ratio_minimum = c(0.2),
#'  floor_area_ratio_maximum = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5),
#'  height_stories_maximum = c(3, 3, 3, 3, 3, 5, 5, 5, 5, 5),
#'  height_feet_maximum = NA,
#'  parking_stalls_per_parcel_minimum = c(1),
#'  parking_stalls_per_unit_minimum = c(2, 2, 2, 2, 2, 1, 1, 1, 1, 1))
#'
#'  estimate_zoning_envelope(
#'    parcels = df)

estimate_zoning_envelope = function(
    parcels,
    development_size_maximum = 300,
    standard_unit_sqft_multifamily = 1000,
    standard_parking_stall_sqft = 325,
    parking_model = "singlestory") {

  required_column_names = c(
    "parcel_id",
    "parcel_area_sqft",
    "setback_front",
    "setback_rear",
    "setback_side",
    "parcel_area_sqft_minimum",
    "units_per_parcel_maximum",
    "units_per_acre_maximum",
    "parcel_coverage_percent_maximum",
    "parcel_coverage_percent_maximum_building",
    "open_space_ratio_minimum",
    "floor_area_ratio_maximum",
    "height_stories_maximum",
    "height_feet_maximum",
    "parking_stalls_per_parcel_minimum",
    "parking_stalls_per_unit_minimum")

  optional_column_names = c(
    "parcel_depth",
    "parcel_width")

  if (!all(required_column_names %in% names(parcels))) {
    stop(stringr::str_c("The `residential_unit_parcels` dataframe must contain the following columns: ",
         stringr::str_c(required_column_names, collapse = ", "), ". Either rename existing columns
         accordingly and/or create such columns using reasonable assumed values.")) }

  parcel_dimensions = c("parcel_depth", "parcel_width")
  if (all(parcel_dimensions %in% names(parcels)) & (any(parcel_dimensions %in% names(parcels)))) {
    stop("Include either both or neither of `parcel_depth` and `parcel_width`.") }

  if (nrow(parcels %>% dplyr::filter(is.na(height_stories_maximum) & is.na(height_feet_maximum))) > 1) {
    warning("One or more observations have no specified height constraints. Is this accurate?") }

  maximum_development_capacities1 = parcels %>%
    ## PARCEL DIMENSIONS AND BUILDABLE AREAS
    ## we assume the parcel is slightly taller/deeper than it is wide
    { if (all(parcel_dimensions %in% colnames(.))) { . } else dplyr::mutate(.,
        parcel_depth = (sqrt(parcel_area_sqft) * 1.1),
        parcel_width = parcel_area_sqft / parcel_depth) } %>%

    dplyr::mutate(
      ## baseline assumptions
      development_size_maximum = development_size_maximum,
      standard_unit_sqft_multifamily = standard_unit_sqft_multifamily,
      standard_parking_stall_sqft = standard_parking_stall_sqft,

      ## we calculate buildable square footage by subtracting setbacks from the parcel area
      ## and separately calculating the share of land developable after applying the
      ## maximum lot coverage requirement
      ## then we select the smaller area of the two as the buildable area
      square_footage_setbacks = parcel_area_sqft -
        (setback_front * parcel_width) -
        (setback_rear * parcel_width) -
        (setback_side * parcel_depth * 2) +
        (setback_front * setback_side * 2) +
        (setback_rear * setback_side * 2),
      ## assigning arbitrary maximum of 70% maximum lot coverage (for realism)
      parcel_coverage_percent_maximum_building = parcel_coverage_percent_maximum_building / 100,
      square_footage_parcel_coverage_maximum_building = parcel_area_sqft * parcel_coverage_percent_maximum_building,
      square_footage_buildable = pmin(square_footage_setbacks, square_footage_parcel_coverage_maximum_building),
      ## we apply assumptions about floor/story heights
      ## look into transition from steel to wood development
      height_stories_maximum = dplyr::case_when(
        is.na(height_stories_maximum) ~ ((height_feet_maximum - 12) / 10) + 1,
        ## first story is 12, others are ten
        !is.na(height_stories_maximum) ~ height_stories_maximum),

      ## MAXIMUM DEVELOPMENT CAPACITY (MDC) - ALTERNATIVES
      ## Our approach is to calculate multiple estimations of buildable capacity per parcel,
      ## based on different regulatory dimensions, then we will select (per parcel)
      ## the minimum of these estimates

      ## simply the acreage times the maximum units per acre
      ## when this is not specified, assume a maximum of 30 units per acre
      mdc_units_per_acre = dplyr::if_else(
        !is.na(units_per_acre_maximum),
        parcel_area_sqft * units_per_acre_maximum,
        development_size_maximum),

      ## the regulated maximum number of units per parcel, if any
      mdc_units_per_parcel = units_per_parcel_maximum,

      ## if a minimum lot size applies, otherwise this is ~infinite
      mdc_units_per_parcel_area = dplyr::case_when(
        parcel_area_sqft < parcel_area_sqft_minimum ~ 0,
        TRUE ~ development_size_maximum),

      ## if a floor area ratio (FAR) applies, we take the full parcel acreage and multiply by the FAR
      ## standardized by the per-unit square footage.
      ## e.g., on a 10000 sq ft parcel with a FAR of 2, we could develop 20000 sq ft
      ## divided by 1000 sq ft per unit = 20 units
      mdc_far = dplyr::if_else(
        !is.na(floor_area_ratio_maximum),
        parcel_area_sqft * floor_area_ratio_maximum / standard_unit_sqft_multifamily,
        development_size_maximum))

  ## Recursively calculate the number of units that can be built on a property,
  ## accounting for the fact that as unit counts increase, parking requirements eat into the available
  ## land for those units (because there are two interdependent variables, we solve recursively, iterating
  ## over unit counts to maximize developable units while meeting parking and buildable area constraints)
  mdc_buildable_areas = purrr::map_dfr(
    maximum_development_capacities1$parcel_id,
    function(id) {

      ## extract attributes of the parcel
      row = maximum_development_capacities1 %>% dplyr::filter(parcel_id == !!id)
      height_stories_maximum = row$height_stories_maximum
      standard_unit_sqft_multifamily = row$standard_unit_sqft_multifamily
      standard_parking_stall_sqft = row$standard_parking_stall_sqft
      parking_stalls_per_unit_minimum = row$parking_stalls_per_unit_minimum
      square_footage_buildable = row$square_footage_buildable

      tibble::tibble(
        parcel_id = id,
        square_footage_buildable = square_footage_buildable,
        height_stories_maximum = height_stories_maximum,
        ## recursion
        units = purrr::reduce(
          .x = seq(1, development_size_maximum) %>% sort(decreasing = TRUE),
          .f = function(units, other) {

            ## square footage on which development can occur (after accounting for open space and setback requirements)
            ## times the maximum vertical development allowed, providing a three-dimensional maximum buildable volume
            total_developable_volume = square_footage_buildable * height_stories_maximum
            ## the number of units we're currently evaluating recursively times the area per unit
            unit_volume = units * standard_unit_sqft_multifamily

            parking_area = dplyr::if_else(
              parking_model == "singlestory",
              units * standard_parking_stall_sqft * parking_stalls_per_unit_minimum,
              units * standard_parking_stall_sqft * parking_stalls_per_unit_minimum / height_stories_maximum)

            parking_volume = parking_area * height_stories_maximum

            ## the remaining buildable volume after accounting for parking
            residential_developable_volume = total_developable_volume - parking_volume

            ## if any of the below conditions are met, development is not feasible
            ## decrement the unit count and re-run the function
            if (
              ## the total parking and unit volume exceeds the maximum developable volume
              unit_volume + parking_volume > total_developable_volume |
              ## parking area exceeds the buildable area of the parcel
              parking_area > square_footage_buildable |
              ## the unit volume exceeds the remaining buildable area, after accounting for parking
              unit_volume > residential_developable_volume |
              ## the remaining buildable area is less than 1000 sq ft
              residential_developable_volume < standard_unit_sqft_multifamily) {

              return(units - 1) } else {

              return(units) } }),

        parking_stalls_per_unit_minimum = parking_stalls_per_unit_minimum,
        total_developable_volume = square_footage_buildable * height_stories_maximum,
        parking_area= dplyr::if_else(
          parking_model == "singlestory",
          units * standard_parking_stall_sqft * parking_stalls_per_unit_minimum,
          units * standard_parking_stall_sqft * parking_stalls_per_unit_minimum / height_stories_maximum),
        parking_volume = parking_area * height_stories_maximum,
        residential_developable_volume = total_developable_volume - parking_volume,
        unit_volume = units * standard_unit_sqft_multifamily,
        ## for the last check, testing that there is at least 1000 sq ft of
        ## ground area for any residential development
        feasible =
          (unit_volume + parking_volume <= total_developable_volume) &
          (unit_volume <= residential_developable_volume) &
          residential_developable_volume >= 1000) }) %>%
    ## if not feasible--this should only occur when a property can't even accommodate a single unit--return zeor
    dplyr::mutate(mdc_buildable_area = dplyr::if_else(feasible, units, 0)) %>%
    dplyr::select(parcel_id, mdc_buildable_area)

  maximum_development_capacities = maximum_development_capacities1 %>%
    dplyr::left_join(mdc_buildable_areas, by = "parcel_id") %>%
    dplyr::mutate(
      ## take the minimum across all of our different operationalizations of maximum development capacity
      maximum_development_capacity_zoned =
        pmin(mdc_units_per_parcel_area, mdc_units_per_parcel, mdc_units_per_acre, mdc_far, mdc_buildable_area, na.rm = TRUE))

  return(maximum_development_capacities)
}

utils::globalVariables(
  c("feasible", "floor_area_ratio_maximum", "height_feet_maximum",
    "height_stories_maximum", "mdc_acreage", "mdc_buildable_area",
    "mdc_far", "mdc_minimum_lot_size", "mdc_zoned_housing_units",
    "parcel_area_sqft", "parcel_coverage_percent_maximum_building",
    "parcel_depth", "parcel_id", "parcel_width", "parking_area",
    "parking_volume", "residential_developable_volume", "setback_front",
    "setback_rear", "setback_side", "square_footage_parcel_coverage_maximum_building",
    "square_footage_setbacks", "total_developable_volume", "unit_volume",
    "units_per_acre_maximum", "units_per_parcel_maximum", "mdc_units_per_acre",
    "mdc_units_per_parcel", "mdc_units_per_parcel_area"))
