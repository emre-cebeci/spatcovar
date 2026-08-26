#' Count source features intersecting each polygon
#'
#' Counts the number of source features in `y` that spatially intersect each
#' target polygon in `x`.
#'
#' @section Intersection semantics:
#' A source feature is counted if it spatially intersects the target polygon,
#' which includes features that touch the boundary.
#' The function uses [sf::st_intersects()] internally.
#'
#' Each feature in `y` counts as one source feature regardless of its
#' sub-geometry cardinality.
#' A `MULTIPOINT` feature with 10 sub-points is one source feature.
#' Cast to `POINT` with [sf::st_cast()] before calling `spat_count()` if
#' per-point counts are desired.
#' The same principle applies to `MULTILINESTRING` and `MULTIPOLYGON`.
#'
#' Both `x` and `y` must have known coordinate reference systems.
#'
#' @param x An `sf` object with POLYGON or MULTIPOLYGON geometry. Must have a
#'   known coordinate reference system.
#' @param y An `sf` object containing source features (any geometry type). Must
#'   have a known coordinate reference system.
#' @param name Character.
#'   Name of the output column.
#'   Default is `"count"`.
#' @param repair Logical.
#'   If `TRUE` (default), invalid geometries are repaired on internal copies.
#' @param diagnostics Logical.
#'   If `TRUE`, attach a diagnostics summary as an attribute.
#' @param overwrite Logical.
#'   If `TRUE`, overwrite an existing column named `name`.
#'
#' @return The input `sf` object with a new integer column containing the
#'   count of intersecting source features.
#'   Polygons with no intersecting features receive `0L`.
#'
#' @export
#'
#' @examples
#' regions <- example_polygons()
#' sites <- example_points()
#' spat_count(regions, sites)
spat_count <- function(x,
                       y,
                       name = "count",
                       repair = TRUE,
                       diagnostics = FALSE,
                       overwrite = FALSE) {

  validate_sf_polygons(x, "x")
  validate_sf_input(y, "y")
  validate_crs(x, "x", "spat_count()")
  validate_crs(y, "y", "spat_count()")
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

  # Align CRS (topological, so no projection needed, but CRS must match)
  y_work <- align_crs(x_work, y_work, arg_x = "x", arg_y = "y",
                      operation = "spat_count()")

  # Count intersections
  if (n_source == 0L) {
    count_out <- rep(0L, n_input)
  } else {
    ints <- sf::st_intersects(x_work, y_work)
    count_out <- lengths(ints)
  }

  # Handle empty target geometries -> NA
  empty_geom <- sf::st_is_empty(sf::st_geometry(x))
  count_out[empty_geom] <- NA_integer_

  x[[name]] <- as.integer(count_out)

  assert_row_count(x, n_input)

  if (isTRUE(diagnostics)) {
    n_zero <- sum(count_out == 0L, na.rm = TRUE)
    diag <- list(
      operation = "spat_count",
      n_target = n_input,
      n_source = n_source,
      n_repaired_target = n_repaired_x,
      n_repaired_source = n_repaired_y,
      n_zero = n_zero,
      n_na = sum(is.na(count_out)),
      timestamp = Sys.time()
    )
    attr(x, "spat_diagnostics") <- diag
  }

  x
}
