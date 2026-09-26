#' Construct spatial covariates from polygon data
#'
#' `spatcovar` provides a consistent interface for constructing commonly used
#' spatial covariates from polygon data.  It computes polygon areas, distances
#' to reference features, intersection counts, line lengths within polygons,
#' polygon overlap areas and shares, and raster zonal summaries while handling
#' coordinate reference system validation, geometry repair, unit conversion,
#' row preservation, and standardised missing value semantics.
#'
#' @section Start here:
#' Start with an `sf` object of target `POLYGON` or `MULTIPOLYGON` features
#' with a known coordinate reference system (CRS). Each operation appends
#' one or more numeric columns to those target rows; the original row order
#' and target geometry are preserved. The package supplies small synthetic
#' polygons, points, lines, and a raster, so the example below needs no files.
#'
#' @section Core functions:
#' \describe{
#'   \item{[spat_area()]}{Compute each target polygon's area.}
#'   \item{[spat_distance()]}{Minimum, centroid, or point-on-surface distance
#'     to reference features.}
#'   \item{[spat_count()]}{Count source features intersecting each polygon,
#'     including boundary touches.}
#'   \item{[spat_length()]}{Total length of line features within each polygon.}
#'   \item{[spat_overlap()]}{Overlap area, share, or count between polygon
#'     layers.}
#'   \item{[spat_raster()]}{Zonal statistics from one raster layer, including
#'     coverage-weighted mean, sum, and count.}
#' }
#'
#' @section Interpret the new columns:
#' Area, distance, and line length can be requested in explicit units. By
#' default, output names reflect the measure or unit (for example,
#' `area_km2`, `dist_km`, or `overlap_share`); use `name` to choose your own.
#' An existing column is never overwritten unless `overwrite = TRUE`.
#' Counts of source features include features touching a polygon boundary.
#' Overlap `"share"` sums source intersections relative to target area, so
#' it can exceed 1 if source polygons overlap each other. Raster `"count"`
#' is the effective number of covered, non-missing cells and can be
#' fractional. Polygons with no valid raster cell support receive `NA` for
#' all requested raster statistics; a valid raster value of zero remains zero.
#'
#' @section CRS handling:
#' Functions that compute metric quantities on geographic coordinates
#' ([spat_area()], [spat_distance()], [spat_overlap()]) use the geodesic
#' measurement capabilities provided by [sf] (via s2).
#' An optional projected CRS may be supplied via the `crs` argument when planar
#' computation is desired.
#'
#' In contrast, [spat_length()] strictly requires a projected (planar) CRS
#' (either on the input data or supplied via `crs`) because planar coordinates
#' are required for line clipping and length measurement.
#' [spat_count()] and [spat_raster()] align source and target CRS internally.
#'
#' @section Geometry repair:
#' When `repair = TRUE` (the default), invalid geometries are repaired on
#' internal copies used for computation.
#' The geometry returned to the user is always the original target geometry.
#'
#' @section Diagnostics:
#' Set `diagnostics = TRUE` on any core function to attach a lightweight
#' summary of the operation as an attribute.
#' Retrieve it with [spat_diagnostics()].
#'
#' @seealso [spat_area()], [spat_distance()], [spat_count()],
#'   [spat_length()], [spat_overlap()], [spat_raster()],
#'   [spat_diagnostics()], `vignette("spatcovar", package = "spatcovar")`
#'
#' @examples
#' regions <- example_polygons()
#' sites <- example_points()
#' routes <- example_lines()
#' zones <- example_grid()
#' raster <- example_raster()
#'
#' # Add several covariates without changing the target geometry or row order.
#' result <- spat_area(regions, unit = "km2")
#' result <- spat_count(result, sites, name = "n_sites")
#' result <- spat_length(result, routes, unit = "km")
#' result <- spat_distance(result, sites, name = "dist_to_sites")
#' result <- spat_overlap(result, zones, measure = "share")
#' result <- spat_raster(result, raster, stats = c("mean", "max"), name = "elev")
#' sf::st_drop_geometry(result)
#'
#' # Inspect diagnostics for a particular operation.
#' measured <- spat_area(regions, diagnostics = TRUE)
#' spat_diagnostics(measured)
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
