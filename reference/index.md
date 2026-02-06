# Package index

## Point data

- [`get_structures()`](https://ui-research.github.io/climateapi/reference/get_structures.md)
  : Estimate counts of hazard-impacted structures by structure type

## Block data

- [`get_lodes()`](https://ui-research.github.io/climateapi/reference/get_lodes.md)
  : Get LEHD Origin-Destination Employment Statistics (LODES) data

## Cities data

- [`get_sba_loans()`](https://ui-research.github.io/climateapi/reference/get_sba_loans.md)
  : Access SBA data on disaster loans

## Government unit data

- [`get_government_finances()`](https://ui-research.github.io/climateapi/reference/get_government_finances.md)
  : Get county government revenues and expenditures

## County data

- [`get_business_patterns()`](https://ui-research.github.io/climateapi/reference/get_business_patterns.md)
  : Obtain County Business Patterns (CBP) Estimates per County
- [`get_fema_disaster_declarations()`](https://ui-research.github.io/climateapi/reference/get_fema_disaster_declarations.md)
  : Get major disaster declarations by county
- [`get_ihp_registrations()`](https://ui-research.github.io/climateapi/reference/get_ihp_registrations.md)
  : Get Individuals and Households Program (IHP) registrations
- [`get_nfip_policies()`](https://ui-research.github.io/climateapi/reference/get_nfip_policies.md)
  : Access county-level data on NFIP policies
- [`get_nfip_claims()`](https://ui-research.github.io/climateapi/reference/get_nfip_claims.md)
  : Access county-level data on NFIP claims
- [`get_nfip_residential_penetration()`](https://ui-research.github.io/climateapi/reference/get_nfip_residential_penetration.md)
  : Get the share of residential structures covered by NFIP
- [`get_public_assistance()`](https://ui-research.github.io/climateapi/reference/get_public_assistance.md)
  : Get FEMA Public Assistance (PA) funding
- [`get_sheldus()`](https://ui-research.github.io/climateapi/reference/get_sheldus.md)
  : Access temporal county-level SHELDUS hazard damage data
- [`get_preliminary_damage_assessments()`](https://ui-research.github.io/climateapi/reference/get_preliminary_damage_assessments.md)
  : Get Data from Preliminary Damage Assessments Submitted to FEMA for
  Disaster Declarations
- [`get_hazard_mitigation_assistance()`](https://ui-research.github.io/climateapi/reference/get_hazard_mitigation_assistance.md)
  : Get Hazard Mitigation Assistance (HMA) Project Details

## State data

- [`get_emergency_management_performance()`](https://ui-research.github.io/climateapi/reference/get_emergency_management_performance.md)
  : Get Emergency Management Performance Grant (EMPG) data
- [`get_disaster_dollar_database()`](https://ui-research.github.io/climateapi/reference/get_disaster_dollar_database.md)
  : Get Disaster Dollar Database Data

## Event boundaries

- [`get_current_fire_perimeters()`](https://ui-research.github.io/climateapi/reference/get_current_fire_perimeters.md)
  : Acquire current wildfire perimeters
- [`get_wildfire_burn_zones()`](https://ui-research.github.io/climateapi/reference/get_wildfire_burn_zones.md)
  : Get wildfire burn zones

## Land use

- [`estimate_zoning_envelope()`](https://ui-research.github.io/climateapi/reference/estimate_zoning_envelope.md)
  : Calculate the number of units allowed on a parcel
- [`estimate_units_per_parcel()`](https://ui-research.github.io/climateapi/reference/estimate_units_per_parcel.md)
  : Estimate the number and types of structures per parcel
- [`interpolate_demographics()`](https://ui-research.github.io/climateapi/reference/interpolate_demographics.md)
  : Interpolate tract-level sociodemographic data to zoning polygons

## Surveys and Qualtrics

- [`qualtrics_format_metadata()`](https://ui-research.github.io/climateapi/reference/qualtrics_format_metadata.md)
  : Prep Qualtrics metadata
- [`qualtrics_get_metadata()`](https://ui-research.github.io/climateapi/reference/qualtrics_get_metadata.md)
  : Access Qualtrics metadata
- [`qualtrics_plot_question()`](https://ui-research.github.io/climateapi/reference/qualtrics_plot_question.md)
  : Plot responses to Qualtrics survey questions
- [`qualtrics_define_missing()`](https://ui-research.github.io/climateapi/reference/qualtrics_define_missing.md)
  : Fill in missing and non-missing values across interrelated survey
  questions

## Utilities

- [`get_system_username()`](https://ui-research.github.io/climateapi/reference/get_system_username.md)
  : Get the user's username
- [`get_box_path()`](https://ui-research.github.io/climateapi/reference/get_box_path.md)
  : Get the path to the C&C Box folder
- [`get_dataset_columns()`](https://ui-research.github.io/climateapi/reference/get_dataset_columns.md)
  : Get the raw column names for a specified dataset
- [`get_geography_metadata()`](https://ui-research.github.io/climateapi/reference/get_geography_metadata.md)
  : Get geography metadata about states or counties
- [`convert_delimited_to_parquet()`](https://ui-research.github.io/climateapi/reference/convert_delimited_to_parquet.md)
  : Convert raw data to parquet to conserve memory / speed subsequent
  operations
- [`convert_table_text_to_dataframe()`](https://ui-research.github.io/climateapi/reference/convert_table_text_to_dataframe.md)
  : Use an LLM to Convert Table Text to a Dataframe
- [`get_spatial_extent_census()`](https://ui-research.github.io/climateapi/reference/get_spatial_extent_census.md)
  : Get the Census geographies that overlap with the input spatial
  dataset
- [`read_xlsx_from_url()`](https://ui-research.github.io/climateapi/reference/read_xlsx_from_url.md)
  : Download a .xlsx file(s) from a URL(s)
- [`subdivide_linestring()`](https://ui-research.github.io/climateapi/reference/subdivide_linestring.md)
  : Subdivide a linestring into segments of a specified length
- [`polygons_to_linestring()`](https://ui-research.github.io/climateapi/reference/polygons_to_linestring.md)
  : Convert polygons into their component linestrings
- [`read_ipums_cached()`](https://ui-research.github.io/climateapi/reference/read_ipums_cached.md)
  : Read IPUMS data leveraging a local cache
- [`inflation_adjust()`](https://ui-research.github.io/climateapi/reference/inflation_adjust.md)
  : Inflation adjust dollar values using annual PCE Index
- [`cache_it()`](https://ui-research.github.io/climateapi/reference/cache_it.md)
  : Cache an object to a parquet file; optionally read from disk
- [`get_naics_codes()`](https://ui-research.github.io/climateapi/reference/get_naics_codes.md)
  : Get NAICS Codes for County Business Patterns
