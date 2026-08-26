#' Compute polygon area
#'
#' Calculates the area of each polygon in an `sf` object and appends the
#' result as a new column.
#'
#' @section CRS behaviour:
#' For geographic (longitude/latitude) coordinate reference systems,
#' `spat_area()` uses [sf::st_area()] which computes geodesic area via the
#' s2 geometry library.
#' For projected CRS the computation is planar.
#' If `crs` is supplied, an internal copy is projected to that CRS before
#' computing area.
#'
#' @param x An `sf` object with POLYGON or MULTIPOLYGON geometry. Must have a
#'   known coordinate reference system.
#' @param unit Character.
#'   Area unit for the output: `"m2"`, `"km2"` (default), `"ha"`, or `"mi2"`.
#' @param name Character or `NULL`.
#'   Name of the output column.
#'   If `NULL` (default), the column is named dynamically based on `unit`
#'   (e.g., `"area_km2"`, `"area_m2"`, `"area_ha"`, `"area_mi2"`).
#' @param crs Optional.
#'   A CRS specification (EPSG code, WKT, or proj4string) to project to before
#'   computing area.
#'   If `NULL` (default), the input CRS is used.
#' @param repair Logical.
#'   If `TRUE` (default), invalid geometries are repaired on an internal copy
#'   before computation.
#'   The returned geometry is always the original target geometry.
#' @param diagnostics Logical.
#'   If `TRUE`, attach a diagnostics summary as an attribute.
#'   Default is `FALSE`.
#' @param overwrite Logical.
#'   If `TRUE`, overwrite an existing column named `name`.
#'   Default is `FALSE`.
#'
#' @return The input `sf` object with a new numeric column containing area
#'   values in the requested unit.
#'   Row count, row order, and the original geometry are preserved.
#'
#' @export
#'
#' @examples
#' regions <- example_polygons()
#' spat_area(regions)
#' spat_area(regions, unit = "ha")
#' spat_area(regions, unit = "m2", name = "custom_area")
spat_area <- function(x,
                      unit = "km2",
                      name = NULL,
                      crs = NULL,
                      repair = TRUE,
                      diagnostics = FALSE,
                      overwrite = FALSE) {

  validate_sf_polygons(x, "x")
  validate_crs(x, "x", "spat_area()")
  validate_unit(unit, area_units())

  if (is.null(name)) {
    name <- paste0("area_", unit)
  }
  validate_name(name)
  validate_name_collision(x, name, overwrite)

  n_input <- nrow(x)

  # Work on a copy for geometry operations
  x_work <- x
  n_repaired <- 0L

  if (isTRUE(repair)) {
    rep_result <- repair_geometries(x_work, "target")
    x_work <- rep_result$data
    n_repaired <- rep_result$n_repaired
  }

  if (!is.null(crs)) {
    x_work <- sf::st_transform(x_work, crs)
  }

  # Compute area — sf::st_area handles geodesic on geographic CRS
  area_raw <- sf::st_area(x_work)

  # Convert from units object to numeric in square metres, then to target unit
  area_m2 <- as.numeric(units::set_units(area_raw, "m^2"))
  area_out <- convert_area_unit(area_m2, unit)

  # Attach result to original (not work copy)
  x[[name]] <- area_out

  assert_row_count(x, n_input)

  if (isTRUE(diagnostics)) {
    crs_used <- if (!is.null(crs)) {
      as.character(sf::st_crs(crs)$input)
    } else {
      as.character(sf::st_crs(x)$input)
    }

    diag <- list(
      operation = "spat_area",
      n_target = n_input,
      n_repaired_target = n_repaired,
      n_na = sum(is.na(area_out)),
      crs_used = crs_used,
      unit = unit,
      timestamp = Sys.time()
    )
    attr(x, "spat_diagnostics") <- diag
  }

  x
}
