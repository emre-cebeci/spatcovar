#' Compute total line length within each polygon
#'
#' Clips line features in `y` to each target polygon in `x` and computes the
#' total length of clipped line segments.
#'
#' @section CRS requirement:
#' `spat_length()` requires a projected (planar) coordinate reference system
#' for accurate length measurement.
#' If `x` has a geographic CRS and `crs` is not supplied, the function
#' raises an error asking the user to supply a projected CRS.
#' If `crs` is supplied, it must also be a projected (planar) CRS.
#' Both `x` and `y` must have known coordinate reference systems.
#'
#' @param x An `sf` object with POLYGON or MULTIPOLYGON geometry. Must have a
#'   known coordinate reference system.
#' @param y An `sf` object with LINESTRING or MULTILINESTRING geometry. Must
#'   have a known coordinate reference system.
#' @param unit Character.
#'   Length unit: `"m"`, `"km"` (default), or `"mi"`.
#' @param name Character or `NULL`.
#'   Name of the output column.
#'   If `NULL` (default), the column is named dynamically based on `unit`
#'   (e.g., `"length_km"`, `"length_m"`, `"length_mi"`).
#' @param crs Optional.
#'   A projected CRS specification to project to before clipping and measuring
#'   length. Required when `x` has a geographic CRS.
#' @param repair Logical.
#'   If `TRUE` (default), invalid geometries are repaired on internal copies.
#' @param diagnostics Logical.
#'   If `TRUE`, attach a diagnostics summary as an attribute.
#' @param overwrite Logical.
#'   If `TRUE`, overwrite an existing column named `name`.
#'
#' @return The input `sf` object with a new numeric column containing total
#'   line length in the requested unit.
#'   Polygons with no intersecting lines receive `0`.
#'
#' @export
#'
#' @examples
#' regions <- example_polygons()
#' routes <- example_lines()
#' spat_length(regions, routes)
#' spat_length(regions, routes, unit = "m")
spat_length <- function(x,
                        y,
                        unit = "km",
                        name = NULL,
                        crs = NULL,
                        repair = TRUE,
                        diagnostics = FALSE,
                        overwrite = FALSE) {

  validate_sf_polygons(x, "x")
  validate_sf_input(y, "y")
  validate_sf_geom_type(y, c("LINESTRING", "MULTILINESTRING"), "y")
  validate_crs(x, "x", "spat_length()")
  validate_crs(y, "y", "spat_length()")
  validate_unit(unit, length_units())

  if (is.null(name)) {
    name <- paste0("length_", unit)
  }
  validate_name(name)
  validate_name_collision(x, name, overwrite)

  n_input <- nrow(x)
  n_source <- nrow(y)

  # Work on copies
  x_work <- x
  y_work <- y
  n_repaired_x <- 0L
  n_repaired_y <- 0L

  if (isTRUE(repair)) {
    rep_x <- repair_geometries(x_work, "target")
    x_work <- rep_x$data
    n_repaired_x <- rep_x$n_repaired

    rep_y <- repair_geometries(y_work, "source")
    y_work <- rep_y$data
    n_repaired_y <- rep_y$n_repaired
  }

  # Enforce planar computation CRS
  if (!is.null(crs)) {
    x_work <- ensure_planar_crs(x_work, crs = crs,
                                operation = "spat_length()", arg_name = "crs")
    y_work <- sf::st_transform(y_work, sf::st_crs(x_work))
  } else {
    y_work <- align_crs(x_work, y_work, arg_x = "x", arg_y = "y",
                        operation = "spat_length()")
    # Check that input CRS is projected
    x_work <- ensure_planar_crs(x_work, crs = NULL,
                                operation = "spat_length()", arg_name = "x")
  }

  # Compute clipped lengths per polygon
  if (n_source == 0L) {
    length_out <- rep(0, n_input)
  } else {
    # Add a row identifier to x_work
    x_work[["..spat_row_id.."]] <- seq_len(n_input)

    # Suppress warnings from st_intersection about attribute values
    clipped <- suppressWarnings(
      sf::st_intersection(x_work["..spat_row_id.."], y_work)
    )

    if (nrow(clipped) == 0L) {
      length_out <- rep(0, n_input)
    } else {
      # Filter to line geometries (intersection can produce points at tangent)
      clipped_types <- as.character(sf::st_geometry_type(clipped))
      line_mask <- clipped_types %in% c(
        "LINESTRING", "MULTILINESTRING", "GEOMETRYCOLLECTION"
      )
      clipped <- clipped[line_mask, ]

      if (nrow(clipped) == 0L) {
        length_out <- rep(0, n_input)
      } else {
        # Measure lengths in metres explicitly via units package
        seg_lengths_m <- as.numeric(
          units::set_units(sf::st_length(clipped), "m")
        )
        seg_ids <- clipped[["..spat_row_id.."]]

        # Aggregate per polygon in metres
        length_by_id <- tapply(seg_lengths_m, seg_ids, sum, default = 0)
        length_out <- rep(0, n_input)
        matched_ids <- as.integer(names(length_by_id))
        length_out[matched_ids] <- as.numeric(length_by_id)
      }
    }
  }

  # Convert units from metres to target unit
  length_out <- convert_length_unit(length_out, unit)

  # Handle empty target geometries -> NA
  empty_geom <- sf::st_is_empty(sf::st_geometry(x))
  length_out[empty_geom] <- NA_real_

  x[[name]] <- length_out

  assert_row_count(x, n_input)

  if (isTRUE(diagnostics)) {
    crs_used <- if (!is.null(crs)) {
      as.character(sf::st_crs(crs)$input)
    } else {
      as.character(sf::st_crs(x)$input)
    }

    diag <- list(
      operation = "spat_length",
      n_target = n_input,
      n_source = n_source,
      n_repaired_target = n_repaired_x,
      n_repaired_source = n_repaired_y,
      n_zero = sum(length_out == 0, na.rm = TRUE),
      n_na = sum(is.na(length_out)),
      crs_used = crs_used,
      unit = unit,
      timestamp = Sys.time()
    )
    attr(x, "spat_diagnostics") <- diag
  }

  x
}
