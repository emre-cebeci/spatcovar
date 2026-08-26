#' Compute raster zonal statistics for each polygon
#'
#' Extracts zonal statistics from a single-layer raster for each polygon in `x`
#' using the [exactextractr][exactextractr::exact_extract] engine for
#' coverage-fraction-weighted extraction.
#'
#' @section Extraction semantics:
#' Statistics are computed using [exactextractr::exact_extract()] which weights
#' pixel contributions by the fraction of each pixel covered by the polygon
#' (the coverage fraction):
#' \describe{
#'   \item{`"mean"`}{Mean cell value, weighted by the fraction of each cell
#'     covered by the polygon.}
#'   \item{`"sum"`}{Sum of non-NA cell values, multiplied by the fraction of
#'     each cell covered by the polygon.}
#'   \item{`"median"`}{Median cell value, weighted by the fraction of each cell
#'     covered by the polygon.}
#'   \item{`"min"`}{Minimum non-NA value across all cells wholly or partially
#'     covered by the polygon (unweighted).}
#'   \item{`"max"`}{Maximum non-NA value across all cells wholly or partially
#'     covered by the polygon (unweighted).}
#'   \item{`"count"`}{Sum of fractions of raster cells with non-NA values
#'     covered by the polygon (the effective number of covered cells). This
#'     value can be fractional for polygons that partially overlap raster
#'     cells.}
#'   \item{`"stdev"`}{Population standard deviation of cell values, weighted by
#'     the fraction of each cell covered by the polygon.}
#' }
#'
#' Valid raster values of zero are preserved as valid observations.
#' If a target polygon has no valid non-NA raster cells contributing to the
#' extraction (either because it lies outside the raster extent or because all
#' covered cells are `NA`/NoData), all requested statistics for that polygon
#' evaluate to `NA`.
#'
#' @section Multilayer rasters:
#' `spat_raster()` supports single-layer rasters only in this release.
#' To extract statistics from multiple layers, supply one raster layer at a
#' time.
#'
#' @section CRS handling:
#' Both `x` and `raster` must have known coordinate reference systems.
#' If the polygon CRS differs from the raster CRS, polygon copies are
#' transformed to match the raster CRS before extraction.
#'
#' @param x An `sf` object with POLYGON or MULTIPOLYGON geometry. Must have a
#'   known coordinate reference system.
#' @param raster A file path (character) or single-layer [terra::SpatRaster]
#'   object. Must have a known coordinate reference system.
#' @param stats Character vector.
#'   Statistics to compute.
#'   Allowed values: `"mean"`, `"median"`, `"min"`, `"max"`, `"sum"`,
#'   `"count"`, `"stdev"`.
#'   Default is `"mean"`.
#' @param name Character or `NULL`.
#'   Prefix for output column names.
#'   Output columns are named `"{name}_{stat}"` when `name` is supplied,
#'   or `"{stat}"` when `name` is `NULL` (default).
#' @param repair Logical.
#'   If `TRUE` (default), invalid geometries are repaired on internal copies.
#' @param diagnostics Logical.
#'   If `TRUE`, attach a diagnostics summary as an attribute.
#' @param overwrite Logical.
#'   If `TRUE`, overwrite existing columns.
#'
#' @return The input `sf` object with new numeric columns for each requested
#'   statistic.
#'
#' @export
#'
#' @examples
#' regions <- example_polygons()
#' rst <- example_raster()
#' spat_raster(regions, rst, stats = c("mean", "min", "max"))
#' spat_raster(regions, rst, stats = "count", name = "cell")
spat_raster <- function(x,
                        raster,
                        stats = "mean",
                        name = NULL,
                        repair = TRUE,
                        diagnostics = FALSE,
                        overwrite = FALSE) {

  validate_sf_polygons(x, "x")
  validate_crs(x, "x", "spat_raster()")

  # Validate stats
  allowed_stats <- c("mean", "median", "min", "max", "sum", "count", "stdev")
  bad_stats <- setdiff(stats, allowed_stats)
  if (length(bad_stats) > 0L) {
    stop(
      "Unknown raster statistic(s): ",
      paste0("\"", bad_stats, "\"", collapse = ", "), ". ",
      "Allowed: ", paste0("\"", allowed_stats, "\"", collapse = ", "), ".",
      call. = FALSE
    )
  }

  # Build output column names
  if (is.null(name)) {
    out_names <- stats
  } else {
    validate_name(name)
    out_names <- paste0(name, "_", stats)
  }

  # Check collisions for each output column
  for (nm in out_names) {
    validate_name_collision(x, nm, overwrite)
  }

  n_input <- nrow(x)

  # Load raster if path
  if (is.character(raster)) {
    if (length(raster) != 1L || !file.exists(raster)) {
      stop("Raster file not found: ", paste(raster, collapse = ", "), call. = FALSE)
    }
    raster <- terra::rast(raster)
  }

  if (!inherits(raster, "SpatRaster")) {
    stop(
      "`raster` must be a file path or terra::SpatRaster object.",
      call. = FALSE
    )
  }

  # Validate single-layer restriction
  if (terra::nlyr(raster) != 1L) {
    stop(
      "`spat_raster()` currently supports single-layer rasters only. ",
      "Supply one raster layer at a time.",
      call. = FALSE
    )
  }

  # Validate raster CRS
  raster_crs_raw <- terra::crs(raster)
  if (!nzchar(raster_crs_raw) || is.na(sf::st_crs(raster_crs_raw))) {
    stop(
      "`raster` has no CRS. `spat_raster()` requires a known CRS.",
      call. = FALSE
    )
  }
  raster_crs <- sf::st_crs(raster_crs_raw)

  # Work on copy
  x_work <- x
  n_repaired <- 0L

  if (isTRUE(repair)) {
    rep_result <- repair_geometries(x_work, "target")
    x_work <- rep_result$data
    n_repaired <- rep_result$n_repaired
  }

  # Align CRS: transform polygons to raster CRS if different
  poly_crs <- sf::st_crs(x_work)

  if (raster_crs != poly_crs) {
    x_work <- sf::st_transform(x_work, raster_crs)
  }

  # Extract using exactextractr
  # Always include count to identify valid non-NA cell support
  needed_stats <- unique(c(stats, "count"))
  extracted <- exactextractr::exact_extract(
    raster,
    x_work,
    fun = needed_stats,
    progress = FALSE
  )

  # Determine valid support per polygon (count > 0 of non-NA cells)
  count_vals <- if (is.data.frame(extracted)) extracted[["count"]] else extracted
  has_valid_support <- !is.na(count_vals) & count_vals > 0

  # Handle single stat vs multiple stats and enforce NA for rows with no valid support
  if (length(stats) == 1L) {
    vals <- if (is.data.frame(extracted)) extracted[[stats]] else extracted
    vals[is.nan(vals)] <- NA_real_
    vals[!has_valid_support] <- NA_real_
    x[[out_names]] <- vals
  } else {
    for (i in seq_along(stats)) {
      st <- stats[i]
      vals <- extracted[[st]]
      vals[is.nan(vals)] <- NA_real_
      vals[!has_valid_support] <- NA_real_
      x[[out_names[i]]] <- vals
    }
  }

  assert_row_count(x, n_input)

  if (isTRUE(diagnostics)) {
    n_na_list <- vapply(out_names, function(nm) sum(is.na(x[[nm]])), integer(1L))

    diag <- list(
      operation = "spat_raster",
      n_target = n_input,
      n_repaired_target = n_repaired,
      stats = stats,
      n_na = n_na_list,
      raster_crs = as.character(raster_crs$input),
      timestamp = Sys.time()
    )
    attr(x, "spat_diagnostics") <- diag
  }

  x
}
