# Internal CRS helpers for spatcovar
# These are NOT exported.

# Validate that an object has a valid, non-NA coordinate reference system
validate_crs <- function(x, arg_name = "x", operation = NULL) {
  crs <- sf::st_crs(x)
  if (is.na(crs)) {
    op_msg <- if (!is.null(operation)) {
      paste0("`", operation, "` requires a known CRS.")
    } else {
      "A known coordinate reference system is required."
    }
    stop(
      "`", arg_name, "` has no CRS. ", op_msg,
      call. = FALSE
    )
  }
  invisible(crs)
}

# Check whether an sf object has a geographic (lon/lat) CRS
is_geographic <- function(x) {
  crs <- sf::st_crs(x)
  if (is.na(crs)) return(FALSE)
  isTRUE(sf::st_is_longlat(x))
}

# Ensure data is in a planar (projected) CRS
# If crs is supplied by user, project to it (and verify that it is planar).
# If data is already projected, return as-is.
# If data is geographic and no crs supplied, error with an informative message.
ensure_planar_crs <- function(x, crs = NULL, operation = "this operation", arg_name = "x") {
  if (!is.null(crs)) {
    target_crs <- tryCatch(sf::st_crs(crs), error = function(e) NA)
    if (is.na(target_crs)) {
      stop(
        "Invalid `crs` supplied. `", operation,
        "` requires a valid projected CRS.",
        call. = FALSE
      )
    }
    if (isTRUE(sf::st_is_longlat(target_crs))) {
      stop(
        "Geographic `crs` supplied. `", operation,
        "` requires a projected CRS for accurate measurement.",
        call. = FALSE
      )
    }
    return(sf::st_transform(x, target_crs))
  }

  validate_crs(x, arg_name, operation)

  if (is_geographic(x)) {
    stop(
      "Geographic CRS detected. `", operation,
      "` requires a projected CRS for accurate measurement. ",
      "Supply a projected CRS via the `crs` argument.",
      call. = FALSE
    )
  }

  x
}

# Estimate a UTM CRS from data centroid (private utility, not auto-called)
# Returns an EPSG integer code for the appropriate UTM zone
estimate_utm_crs <- function(x) {
  validate_crs(x, "x", "estimate_utm_crs()")

  # Get centroid in geographic coordinates
  x_geo <- if (is_geographic(x)) x else sf::st_transform(x, 4326L)
  centroid <- sf::st_coordinates(
    sf::st_centroid(sf::st_union(sf::st_geometry(x_geo)))
  )

  lon <- centroid[1L, "X"]
  lat <- centroid[1L, "Y"]

  zone <- as.integer(floor((lon + 180) / 6) + 1L)
  zone <- max(1L, min(60L, zone))

  epsg <- if (lat >= 0) 32600L + zone else 32700L + zone
  epsg
}

# Transform y to match CRS of x if they differ
align_crs <- function(x, y, arg_x = "x", arg_y = "y", operation = NULL) {
  validate_crs(x, arg_x, operation)
  validate_crs(y, arg_y, operation)

  crs_x <- sf::st_crs(x)
  crs_y <- sf::st_crs(y)

  if (crs_x != crs_y) {
    y <- sf::st_transform(y, crs_x)
  }

  y
}
