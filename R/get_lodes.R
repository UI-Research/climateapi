#' @importFrom magrittr %>%

rename_lodes_variables = function(.df) {
  ## renaming code from GH Copilot (in small part--just translating the codebook to a tribble)
  ## variable definitions/metadata from: https://lehd.ces.census.gov/data/lodes/LODES7/LODESTechDoc7.2.pdf
  ## table of variable definitions
  variable_metadata <- tibble::tribble(
    ~Pos, ~Variable, ~Type, ~Explanation,
    1, "GEOID", "Char15", "GEOID",
    2, "C000", "Num", "Total number of jobs",
    3, "CA01", "Num", "Number of jobs for workers age 29 or younger",
    4, "CA02", "Num", "Number of jobs for workers age 30 to 54",
    5, "CA03", "Num", "Number of jobs for workers age 55 or older",
    6, "CE01", "Num", "Number of jobs with earnings $1250/month or less",
    7, "CE02", "Num", "Number of jobs with earnings $1251/month to $3333/month",
    8, "CE03", "Num", "Number of jobs with earnings greater than $3333/month",
    9, "CNS01", "Num", "Number of jobs in NAICS sector 11 (Agriculture, Forestry, Fishing and Hunting)",
    10, "CNS02", "Num", "Number of jobs in NAICS sector 21 (Mining, Quarrying, and Oil and Gas Extraction)",
    11, "CNS03", "Num", "Number of jobs in NAICS sector 22 (Utilities)",
    12, "CNS04", "Num", "Number of jobs in NAICS sector 23 (Construction)",
    13, "CNS05", "Num", "Number of jobs in NAICS sector 31-33 (Manufacturing)",
    14, "CNS06", "Num", "Number of jobs in NAICS sector 42 (Wholesale Trade)",
    15, "CNS07", "Num", "Number of jobs in NAICS sector 44-45 (Retail Trade)",
    16, "CNS08", "Num", "Number of jobs in NAICS sector 48-49 (Transportation and Warehousing)",
    17, "CNS09", "Num", "Number of jobs in NAICS sector 51 (Information)",
    18, "CNS10", "Num", "Number of jobs in NAICS sector 52 (Finance and Insurance)",
    19, "CNS11", "Num", "Number of jobs in NAICS sector 53 (Real Estate and Rental and Leasing)",
    20, "CNS12", "Num", "Number of jobs in NAICS sector 54 (Professional, Scientific, and Technical Services)",
    21, "CNS13", "Num", "Number of jobs in NAICS sector 55 (Management of Companies and Enterprises)",
    22, "CNS14", "Num", "Number of jobs in NAICS sector 56 (Administrative and Support and Waste Management and Remediation Services)",
    23, "CNS15", "Num", "Number of jobs in NAICS sector 61 (Educational Services)",
    24, "CNS16", "Num", "Number of jobs in NAICS sector 62 (Health Care and Social Assistance)",
    25, "CNS17", "Num", "Number of jobs in NAICS sector 71 (Arts, Entertainment, and Recreation)",
    26, "CNS18", "Num", "Number of jobs in NAICS sector 72 (Accommodation and Food Services)",
    27, "CNS19", "Num", "Number of jobs in NAICS sector 81 (Other Services [except Public Administration])",
    28, "CNS20", "Num", "Number of jobs in NAICS sector 92 (Public Administration)",
    29, "CR01", "Num", "Number of jobs for workers with Race: White, Alone",
    30, "CR02", "Num", "Number of jobs for workers with Race: Black or African American Alone",
    31, "CR03", "Num", "Number of jobs for workers with Race: American Indian or Alaska Native Alone",
    32, "CR04", "Num", "Number of jobs for workers with Race: Asian Alone",
    33, "CR05", "Num", "Number of jobs for workers with Race: Native Hawaiian or Other Pacific Islander Alone",
    34, "CR07", "Num", "Number of jobs for workers with Race: Two or More Race Groups",
    35, "CT01", "Num", "Number of jobs for workers with Ethnicity: Not Hispanic or Latino",
    36, "CT02", "Num", "Number of jobs for workers with Ethnicity: Hispanic or Latino",
    37, "CD01", "Num", "Number of jobs for workers with Educational Attainment: Less than high school",
    38, "CD02", "Num", "Number of jobs for workers with Educational Attainment: High school or equivalent, no college",
    39, "CD03", "Num", "Number of jobs for workers with Educational Attainment: Some college or Associate degree",
    40, "CD04", "Num", "Number of jobs for workers with Educational Attainment: Bachelor's degree or advanced degree",
    41, "CS01", "Num", "Number of jobs for workers with Sex: Male",
    42, "CS02", "Num", "Number of jobs for workers with Sex: Female",
    43, "CFA01", "Num", "Number of jobs for workers at firms with Firm Age: 0-1 Years",
    44, "CFA02", "Num", "Number of jobs for workers at firms with Firm Age: 2-3 Years",
    45, "CFA03", "Num", "Number of jobs for workers at firms with Firm Age: 4-5 Years",
    46, "CFA04", "Num", "Number of jobs for workers at firms with Firm Age: 6-10 Years",
    47, "CFA05", "Num", "Number of jobs for workers at firms with Firm Age: 11+ Years",
    48, "CFS01", "Num", "Number of jobs for workers at firms with Firm Size: 0-19 Employees",
    49, "CFS02", "Num", "Number of jobs for workers at firms with Firm Size: 20-49 Employees",
    50, "CFS03", "Num", "Number of jobs for workers at firms with Firm Size: 50-249 Employees",
    51, "CFS04", "Num", "Number of jobs for workers at firms with Firm Size: 250-499 Employees",
    52, "CFS05", "Num", "Number of jobs for workers at firms with Firm Size: 500+ Employees",
    ## OD-specific variables
    53, "S000", "Num", "Total number of jobs",
    54, "SA01", "Num", "Number of jobs for workers age 29 or younger",
    55, "SA02", "Num", "Number of jobs for workers age 30 to 54",
    56, "SA03", "Num", "Number of jobs for workers age 55 or older",
    57, "SE01", "Num", "Number of jobs with earnings $1250/month or less",
    58, "SE02", "Num", "Number of jobs with earnings $1251/month to $3333/month",
    59, "SE03", "Num", "Number of jobs with earnings greater than $3333/month",
    60, "SI01", "Num", "Number of jobs industry Goods Producing",
    61, "SI02", "Num", "Number of jobs industry Trade, Transportation, Utilities",
    62, "SI03", "Num", "Number of jobs industry All Other Services") %>%
    dplyr::mutate(
      Explanation = Explanation %>%
        stringr::str_to_lower() %>%
        stringr::str_replace_all(c(
          " " = "_",
          "-" = "_",
          "[^[:alnum:]_]" = "",
          "number_of_jobs" = "jobs",
          "jobs_in_naics_sector_[0-9]{2}" = "jobs_industry",
          "^jobs_[0-9]{2}" = "jobs",
          "33_manufacturing" = "manufacturing",
          "45_retail" = "retail",
          "49_transportation" = "transportation")) %>%
        stringr::str_remove_all("for_|with_|workers_at_firms_")) %>%
    dplyr::filter(Variable %in% (.df %>% colnames()))

  renaming_vector = stats::setNames(variable_metadata$Variable, variable_metadata$Explanation)

  .df %>% dplyr::rename(!!!renaming_vector)
}

#' Get LEHD Origin-Destination Employment Statistics (LODES) data
#' Returned data are from LODES Version 8, which is enumerated in 2020-vintage geometries.
#'
#' @param lodes_type One of c("rac", "wac", "od"). "rac" = Residence Area
#'     Characteristics, where jobs are associated with employees' residences.
#'     "wac" = Workplace Area Characteristics, where jobs are associated with
#'     employees' workplaces. "od" = Origin-Destination data, where jobs are associated
#'     with both workers' residences and their workplaces.
#' @param jobs_type One of c("all", "primary"). Default is "all", which includes
#'     multiple jobs for workers with multiple jobs. "primary" includes only the
#'     highest-paying job per worker.
#' @param states A vector of state abbreviations.
#' @param years A vector of years.
#' @param geography One of c("block", "block group", "tract", "county", "state").
#'     Default is "tract".
#' @param state_part One of c("main", "aux"). Default is "main", which includes
#'     only workers who reside inside the state where they work. "aux" returns
#'     only workers who work in the specified state but live outside of that state.
#'
#' @details
#' The Longitudinal Employer-Household Dynamics (LEHD) data at the U.S. Census Bureau
#' is a quarterly database of linked employer-employee data covering over 95% of employment
#' in the United States. The LEHD data are generated by merging previously collected survey
#' and administrative data on jobs, businesses, and workers.
#'
#' LEHD Origin-Destination Employment Statistics (LODES)is a partially synthetic dataset
#' that describes geographic patterns of jobs by their employment locations and residential
#' locations as well as the connections between the two locations. The microdata link employee
#' and employer data by combining administrative state unemployment insurance wage records
#' with other administrative and survey data. The source data are aggregated and adjusted
#' to protect confidentiality.
#'
#' LODES data includes three datasets:
#'    Residence Area Characteristics (RAC):
#'    This file lists the total number of jobs by the census block where the employee lives.
#'
#'    Workplace Area Characteristics (WAC):
#'    This file lists the total number of jobs by the census block where the employee works.
#'
#'    Origin-Destination (OD):
#'    This file lists job totals by both the census block where the employee lives and the
#'    census block where the employee works
#'
#' While similar to County Business Patterns (CBP) data in it's coverage of employment
#' statistics, LODES differs mainly due to its more granular geographies (tract vs. county) and
#' focus on framing the statistics at the individual/job level found in LODES data.
#'
#' @return A tibble with one record per geography per year per job type. Attributes
#'     include total jobs and jobs by worker earnings, industry, and demographics;
#'     the origin-destination results have more limited demographics compared to
#'     the "wac" and "rac" results.
#'     \describe{
#'         \item{year}{the year for which LODES data is pulled from}
#'         \item{state}{A two-digit state identifier.}
#'         \item{GEOID}{11 digit identifier denoted as either h_GEOID representing the employees' residence census block code or w_GEOID representing the employees' workplace census block code}
#'         \item{job_type}{one of either 'all' jobs or only 'federal' jobs}
#'         \item{total_jobs}{total number of jobs in a given tract}
#'         \item{jobs_workers_age}{number of employees by given age range}
#'         \item{jobs_earnings}{number of employees by given monthly earnings range}
#'         \item{jobs_industry}{number of employees by given industry}
#'         \item{jobs_workers_race}{number of employees by given race, inclusive of hispanic or latino; only available in 'wac' and 'rac' datasets}
#'         \item{jobs_workers_ethnicity}{number of employess by hispanic or latino status, regardless of race; only available in 'wac' and 'rac' datasets}
#'         \item{jobs_workers_educational_attainment}{number of employees by highest level of education attained; only available in 'wac' and 'rac' datasets}
#'         \item{jobs_workers_sex}{number of employees by sex; only available in 'wac' and 'rac' datasets}
#'         \item{jobs_firm_age}{number of employees by the age of employing firm; only available in 'wac' datasets}
#'         \item{jobs_firm_size}{number of employees for a given range in employer size; only available in 'wac' datasets}
#'     }
#'
#'
#' @export
get_lodes = function(
    lodes_type,
    jobs_type = "all",
    states,
    years,
    geography = "tract",
    state_part = "main") {

  if (geography == "bg") { geography = "block group"}

  if (!jobs_type %in% c("all", "primary")) {
    stop("`jobs_type` must be one of 'all' or 'primary'.")}

  if (!lodes_type %in% c("rac", "wac", "od")) {
    stop("`lodes_type` must be one of 'rac', 'wac', or 'od'.")}

  if (!geography %in% c("block", "block group", "bg", "tract", "county", "state")) {
    stop("`geography` must be one of 'block', 'block group', 'bg', 'tract', 'county', or 'state'.")}

  if (!state_part %in% c("main", "aux")) {
    stop("`state_part` must be one of 'main' or 'aux'.")}


  # if states == "all" then set states parameter as all 50 states plus DC
  if ("all" %in% states) {
    states <-  c(state.abb, "DC")
    }

  years = years %>% as.numeric

  states = states %>% stringr::str_to_lower()

  if (any(!(states %in% (tidycensus::fips_codes$state %>% unique() %>% tolower)))) {
    stop("`states` must be a vector of state abbreviations corresponding to any of the 51 states.")}

  if (years %>% min < 2002) {
    stop(
"\nLODES data are available from 2002 onward. As of 2025-05-19, the most recently-
supported year is 2022, though these data are expected to be updated annually.")}

  if (years %>% min < 2015) {
    warning(
"\nReporting of federal jobs in LODES data has fluctuated over time. Prior to 2010,
federal jobs are not included in LODES data. Beginning in 2015, significantly fewer
federal jobs are included in LODES as compared to the volume included in 2014.
For these reasons, users are cautioned when comparing job counts over periods that
encompass heterogenous federal job reporting practices. To enable an adjusted
form of multi-year job count comparison, we return by default federal job counts
alongside those for all jobs; users can subtract federal job counts to create
a temporally-consistent measure of total jobs.\n") }


# list of state & year combinations that are missing data as of 12/2025. Sourced from
# here: https://lehd.ces.census.gov/doc/help/onthemap/LODESTechDoc.pdf
  state_years_missing = tibble::tribble(
    ~ year, ~ state,
    2002, "AK",
    2002, "AZ",
    2002, "DC",
    2002, "MA",
    2002, "MS",
    2002, "NH",
    2003, "AZ",
    2003, "DC",
    2003, "MA",
    2003, "MS",
    2004, "DC",
    2004, "MA",
    2005, "DC",
    2005, "MA",
    2006, "DC",
    2006, "MA",
    2007, "DC",
    2007, "MA",
    2008, "DC",
    2008, "MA",
    2009, "DC",
    2009, "MA",
    2010, "MA",
    2017, "AK",
    2018, "AK",
    2019, "AK",
    2020, "AK",
    2021, "AK",
    2022, "AK",
    2022, "MI",
    2023, "AK",
    2023, "MI")

  state_years_supplied = expand.grid(years, states %>% stringr::str_to_upper()) %>%
    tibble::as_tibble() %>%
    dplyr::rename(year = Var1, state = Var2) %>%
    dplyr::mutate(flag = 1)

  years_to_omit = state_years_missing %>%
    dplyr::left_join(state_years_supplied) %>%
    dplyr::filter(flag == 1) %>%
    dplyr::mutate(state = stringr::str_to_lower(state))

  if (nrow(years_to_omit) > 0) {
    warning(
"\nSome supplied state-year combinations are not included in the LODES data.
Returning for only those states that are available for all specified years.\n") }

  states = states[!states %in% years_to_omit$state]

  #https://lehd.ces.census.gov/doc/help/onthemap/LODESDataNote-FedEmp2015.pdf

  ## geography-identifying variables are variably named across different geography
  ## parameters; we standardize these to always be "GEOID"
  geoid_rename = c("_geocode|_tract|_bg|_county|_state" = "_GEOID")

  jobs_type_all = "JT00"
  jobs_type_federal = "JT04"
  if (jobs_type == "primary") {
    jobs_type_all = "JT01"
    jobs_type_federal = "JT05" }

  ## supress messages/warnings else this is noisy
  suppressWarnings({suppressMessages({
    lodes_all_jobs = lehdr::grab_lodes(
        state = states,
        year = years,
        job_type = jobs_type_all, ## all primary jobs, i.e., the highest-paying job per worker
        agg_geo = geography,
        segment = "S000", ## total number of jobs for workers
        lodes_type = lodes_type,
        version = "LODES8",
        state_part = state_part) %>% ## include out-of-state workers who work in the state of interest
      dplyr::rename_with(
        .cols = dplyr::everything(),
        .fn = ~ stringr::str_replace_all(.x, geoid_rename)) %>%
      dplyr::select(-dplyr::matches("create")) })})

  if (years %>% min < 2010 & years %>% max > 2010) {
    warning(
"\nFederal jobs are not available in LODES data prior to 2010. Returned results will
include federal jobs for 2010 and later. Records for pre-2010 federal jobs are listed
as NA.\n") }


  ## if only years are pre-2010, returns jobs without federal job data
  if (years %>% max < 2010) {

    return(lodes_all_jobs)

  } else {

  suppressWarnings({suppressMessages({
    lodes_federal_jobs = lehdr::grab_lodes(
        state = states,
        year = years[years > 2009],
        job_type = jobs_type_federal, ## federal jobs
        agg_geo = geography,
        segment = "S000",
        lodes_type = lodes_type,
        version = "LODES8",
      state_part = state_part) %>%
      dplyr::rename_with(
        .cols = dplyr::everything(),
        .fn = ~ stringr::str_replace_all(.x, geoid_rename)) %>%
      dplyr::select(-dplyr::matches("create")) })})

  join_by = c("year", "GEOID")

  if (lodes_type == "od") {
    join_by = c("year", "w_GEOID", "h_GEOID") } else if (lodes_type == "rac") {
      join_by = c("year", "h_GEOID")
    } else if (lodes_type == "wac") {
      join_by = c("year", "w_GEOID")
  }


  ## both all jobs and all federal jobs
  lodes_all_nonfederal_jobs = lodes_all_jobs %>%
    dplyr::rename_with(.cols = -c(year, dplyr::matches("GEOID"), state), .fn = ~ stringr::str_c("all_", .x)) %>%
    dplyr::mutate(year = as.numeric(year)) %>%
    dplyr::left_join(
      lodes_federal_jobs %>%
        dplyr::select(-state) %>%
        dplyr::rename_with(.cols = -c(year, dplyr::matches("GEOID")), .fn = ~ stringr::str_c("federal_", .x)) %>%
        dplyr::mutate(year = as.numeric(year)),
      by = join_by) %>%
    dplyr::mutate(
      dplyr::across(
        ## there are many fewer rows for the federal primary jobs data, so the join
        ## produces NAs for federal jobs for many tracts. however, these tracts in
        ## effect have zero federal jobs, so we replace the NAs with zeros.
        .cols = dplyr::matches("federal"),
        .fns = ~ dplyr::if_else(is.na(.x), 0, .x)),
      ## no federal data exists for years prior to 2010
      dplyr::across(
        .cols = dplyr::matches("federal"),
        .fns = ~ dplyr::if_else(year < 2010, NA_real_, .x))) %>%
    tidyr::pivot_longer(
      cols = -c(year, state, dplyr::matches("GEOID")),
      names_pattern = "(all|federal)_(.*)",
      names_to = c("job_type", "variable")) %>%
    tidyr::pivot_wider(names_from = "variable", values_from = value) %>%
    rename_lodes_variables()

  return(lodes_all_nonfederal_jobs)
  }
}

utils::globalVariables(c(
  "Explanation", "Variable", "Var1", "Var2", "flag"))
