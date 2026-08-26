# Internal validation helpers for spatcovar
# These are NOT exported; used by every spat_* function.

# Validate that x is an sf object with polygon/multipolygon geometry
validate_sf_polygons <- function(x, arg_name = "x") {
  if (!inherits(x, "sf")) {
    stop(
      "`", arg_name, "` must be an sf object, not ",
      paste(class(x), collapse = "/"), ".",
      call. = FALSE
    )
  }

  geom_types <- unique(as.character(sf::st_geometry_type(x)))
  valid_types <- c("POLYGON", "MULTIPOLYGON")
  bad <- setdiff(geom_types, valid_types)

  if (length(bad) > 0L) {
    stop(
      "`", arg_name, "` must contain POLYGON or MULTIPOLYGON geometries, ",
      "but found: ", paste(bad, collapse = ", "), ".",
      call. = FALSE
    )
  }

  invisible(x)
}

# Validate that y is an sf object (any geometry type)
validate_sf_input <- function(y, arg_name = "y") {
  if (!inherits(y, "sf")) {
    stop(
      "`", arg_name, "` must be an sf object, not ",
      paste(class(y), collapse = "/"), ".",
      call. = FALSE
    )
  }
  invisible(y)
}

# Validate that y contains specific geometry types
validate_sf_geom_type <- function(y, allowed_types, arg_name = "y") {

  geom_types <- unique(as.character(sf::st_geometry_type(y)))
  bad <- setdiff(geom_types, allowed_types)

  if (length(bad) > 0L) {
    stop(
      "`", arg_name, "` must contain ",
      paste(allowed_types, collapse = " or "), " geometries, ",
      "but found: ", paste(bad, collapse = ", "), ".",
      call. = FALSE
    )
  }

  invisible(y)
}

# Check for output column name collision
validate_name_collision <- function(x, name, overwrite) {
  if (name %in% names(x) && !isTRUE(overwrite)) {
    stop(
      "Column '", name, "' already exists in target. ",
      "Use overwrite = TRUE to replace it.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

# Validate unit argument against allowed values
validate_unit <- function(unit, allowed, arg_name = "unit") {
  if (length(unit) != 1L || !unit %in% allowed) {
    stop(
      "`", arg_name, "` must be one of: ",
      paste0("\"", allowed, "\"", collapse = ", "), ".",
      call. = FALSE
    )
  }
  invisible(unit)
}

# Validate name argument
validate_name <- function(name) {

  if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name)) {
    stop("`name` must be a single non-empty character string.", call. = FALSE)
  }
  invisible(name)
}

# Internal row-count assertion (produces informative bug report message)
assert_row_count <- function(result, expected) {
  actual <- nrow(result)
  if (actual != expected) {
    stop(
      "Internal error: row count changed from ", expected, " to ", actual,
      ". This is a bug in spatcovar; please report it.",
      call. = FALSE
    )
  }
  invisible(result)
}
