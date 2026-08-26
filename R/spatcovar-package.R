#' Construct spatial covariates from polygon data
#'
#' `spatcovar` provides a consistent interface for constructing commonly used
#' spatial covariates from polygon data.  It computes polygon areas, distances
#' to reference features, intersection counts, line lengths within polygons,
#' polygon overlap areas and shares, and raster zonal summaries while handling
#' coordinate reference system validation, geometry repair, unit conversion,
#' row preservation, and standardised missing value semantics.
#'
#' @section Core functions:
#' \describe{
#'   \item{[spat_area()]}{Compute polygon area with unit conversion.}
#'   \item{[spat_distance()]}{Minimum, centroid, or point-on-surface distance
#'     to reference features.}
#'   \item{[spat_count()]}{Count source features intersecting each polygon.}
#'   \item{[spat_length()]}{Total length of line features within each polygon.}
#'   \item{[spat_overlap()]}{Overlap area, share, or count between polygon
#'     layers.}
#'   \item{[spat_raster()]}{Zonal raster statistics for each polygon.}
#' }
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
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
