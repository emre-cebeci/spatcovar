#' Compute distance from polygons to reference features
#'
#' Calculates the distance from each polygon in `x` to the reference features
#' in `y` and appends the result as a new column.
#'
#' @section Distance methods:
#' \describe{
#'   \item{`"minimum"`}{Minimum geometry-to-geometry distance.
#'     For each polygon, this is the shortest distance to the nearest feature
#'     in `y`.
#'     Implemented using [sf::st_nearest_feature()] to identify the nearest
#'     candidate, then [sf::st_distance()] with `by_element = TRUE`.
#'     This avoids constructing a full pairwise distance matrix.}
#'   \item{`"centroid"`}{Distance from the centroid of each polygon in `x` to
#'     the nearest feature in `y`.}
#'   \item{`"point_on_surface"`}{Distance from a guaranteed-interior point of
#'     each polygon in `x` to the nearest feature in `y`.}
#' }
#'
#' @section CRS behaviour:
#' For geographic CRS, [sf::st_distance()] computes geodesic great-circle
#' distances via the s2 geometry library.
#' For projected CRS the computation is Euclidean.
#' If `crs` is supplied, internal copies of both `x` and `y` are projected
#' before computing distances.
#' Both `x` and `y` must have known coordinate reference systems.
#'
#' @param x An `sf` object with POLYGON or MULTIPOLYGON geometry. Must have a
#'   known coordinate reference system.
#' @param y An `sf` object containing the reference features (any geometry
#'   type). Must have a known coordinate reference system.
#' @param method Character.
#'   Distance method: `"minimum"` (default), `"centroid"`, or
#'   `"point_on_surface"`.
#' @param unit Character.
#'   Distance unit: `"m"`, `"km"` (default), or `"mi"`.
#' @param name Character or `NULL`.
#'   Name of the output column.
#'   If `NULL` (default), the column is named dynamically based on `unit`
#'   (e.g., `"dist_km"`, `"dist_m"`, `"dist_mi"`).
#' @param crs Optional.
#'   A CRS specification to project to before computing distances.
#' @param repair Logical.
#'   If `TRUE` (default), invalid geometries are repaired on internal copies.
#' @param diagnostics Logical.
#'   If `TRUE`, attach a diagnostics summary as an attribute.
#' @param overwrite Logical.
#'   If `TRUE`, overwrite an existing column named `name`.
#'
#' @return The input `sf` object with a new numeric column containing distances
#'   in the requested unit.
#'
#' @export
#'
#' @examples
#' regions <- example_polygons()
#' sites <- example_points()
#' spat_distance(regions, sites)
#' spat_distance(regions, sites, unit = "m")
#' spat_distance(regions, sites, method = "centroid", name = "centroid_dist_km")
spat_distance <- function(x,
                          y,
                          method = "minimum",
                          unit = "km",
                          name = NULL,
                          crs = NULL,
                          repair = TRUE,
                          diagnostics = FALSE,
                          overwrite = FALSE) {

  validate_sf_polygons(x, "x")
  validate_sf_input(y, "y")
  validate_crs(x, "x", "spat_distance()")
  validate_crs(y, "y", "spat_distance()")
  validate_unit(unit, length_units())

  if (is.null(name)) {
    name <- paste0("dist_", unit)
  }
  validate_name(name)
  validate_name_collision(x, name, overwrite)

  method <- match.arg(method, c("minimum", "centroid", "point_on_surface"))

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

  if (!is.null(crs)) {
    x_work <- sf::st_transform(x_work, crs)
    y_work <- sf::st_transform(y_work, crs)
  } else {
    y_work <- align_crs(x_work, y_work, arg_x = "x", arg_y = "y",
                        operation = "spat_distance()")
  }

  # Handle empty source
  if (n_source == 0L) {
    dist_out <- rep(NA_real_, n_input)
  } else {
    # Determine source geometry for distance computation
    x_geom <- switch(method,
      "minimum" = sf::st_geometry(x_work),
      "centroid" = sf::st_centroid(sf::st_geometry(x_work)),
      "point_on_surface" = sf::st_point_on_surface(sf::st_geometry(x_work))
    )

    if (n_source == 1L) {
      # Single feature: repeat to match x length for by_element
      y_rep <- rep(sf::st_geometry(y_work), n_input)
      dist_raw <- sf::st_distance(x_geom, y_rep, by_element = TRUE)
    } else {
      # Multi-feature: indexed nearest-feature approach
      # st_nearest_feature returns the index of the nearest feature in y
      nearest_idx <- sf::st_nearest_feature(x_geom, y_work)
      y_nearest <- sf::st_geometry(y_work)[nearest_idx]

      dist_raw <- sf::st_distance(x_geom, y_nearest, by_element = TRUE)
    }

    # Handle empty geometries -> NA
    empty_geom <- sf::st_is_empty(sf::st_geometry(x_work))
    dist_m <- as.numeric(units::set_units(dist_raw, "m"))
    dist_m[empty_geom] <- NA_real_
    dist_out <- convert_length_unit(dist_m, unit)
  }

  x[[name]] <- dist_out

  assert_row_count(x, n_input)

  if (isTRUE(diagnostics)) {
    crs_used <- if (!is.null(crs)) {
      as.character(sf::st_crs(crs)$input)
    } else {
      as.character(sf::st_crs(x)$input)
    }

    diag <- list(
      operation = "spat_distance",
      n_target = n_input,
      n_source = n_source,
      n_repaired_target = n_repaired_x,
      n_repaired_source = n_repaired_y,
      n_na = sum(is.na(dist_out)),
      method = method,
      crs_used = crs_used,
      unit = unit,
      timestamp = Sys.time()
    )
    attr(x, "spat_diagnostics") <- diag
  }

  x
}
