# Changelog

## climateapi (development version)

- `get_preliminary_damage_assessments()` now returns `pda_ia_requested`
  and `pda_pa_requested` as TRUE/FALSE rather than 1/0, matching the
  `fema_*_requested` columns.
- `get_preliminary_damage_assessments()` now reads the Public Assistance
  cost estimate from reports that print the label as “Total Public
  Assistance Cost Estimate” rather than “cost estimate” (disasters
  1736-1739), which previously came back as missing.
- `get_preliminary_damage_assessments()` now fills in a missing or
  out-of-range countywide per capita impact indicator the same way it
  already filled the statewide one: with the value the other reports of
  the same federal fiscal year state.
- Adding `transform_pda_counties()`, which splits the county listing a
  preliminary damage assessment report prints for Public Assistance into
  one row per report-county, with the county’s FIPS code and its per
  capita impact as a number.

### v0.0.0.9001

- Updating README
- Reorganizing References
- Adding business patterns data
