# climateapi Package Development Notes

## Performance-Critical Functions

The following functions rely on large input datasets and are slow. Speed optimizations using `duckplyr`, `parquet`, `arrow`, and `tidytable` are critical for these functions:

- `get_ihp_registrations()` - IHP registration data can be very large (millions of records)
- `get_nfip_policies()` - NFIP policy data exceeds 80 million records nationally
- `get_nfip_claims()` - NFIP claims data exceeds 2 million records

### Testing Strategy for Large-Data Functions

Tests for these functions load data once at the top of the test file and reuse that object for all success tests. This avoids repeated I/O during test runs. Validation tests (expected to fail) call the function directly without using the cached data object.

### Performance Considerations

When modifying these functions:
- Prefer `arrow::read_parquet()` over CSV reads
- Use `tidytable` or `dtplyr` for grouped operations on large data
- Avoid loading full datasets into memory when filtering is possible
- Consider chunked processing for extremely large files

## Testing Philosophy

**Do not create skip functions for unavailable dependencies.** If a test requires a package (like `tidycensus`) or a resource (like Box), that dependency should be available when tests run. If something is missing, that's a real problem to fix, not work around with skip logic.

The only acceptable skip pattern is for tests that require external data sources that legitimately may not be configured in all environments (e.g., Box path for large data files). Even then, the validation and signature tests should still run.
