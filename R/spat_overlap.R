#' Compute polygon-polygon overlap measures
#'
#' Calculates overlap between target polygons in `x` and source polygons in
#' `y`.
#' Returns one of three measures: total overlap area, share of target area
#' covered, or count of overlapping source features.
#'
#' @section Overlap measures:
#' \describe{
#'   \item{`"area"`}{Total area of overlap between each target polygon and
#'     all source polygons.
#'     When source polygons overlap each other, overlap is summed without
#'     deduplication.}
#'   \item{`"share"`}{Fraction of each target polygon's area that is covered
#'     by source polygons.
#'     A warning is issued if source polygons have positive-area duplicate
#'     coverage among themselves, because the share can exceed 1.}
#'   \item{`"count"`}{Number of source features that spatially intersect each
#'     target polygon.}
#' }
#'
#' @section CRS behaviour:
#' For the `"area"` and `"share"` measures, overlap area is computed using
#' [sf::st_area()] which handles geodesic area on geographic CRS.
#' If `crs` is supplied, internal copies are projected first.
#' Both `x` and `y` must have known coordinate reference systems.
#'
#' @param x An `sf` object with POLYGON or MULTIPOLYGON geometry. Must have a
#'   known coordinate reference system.
#' @param y An `sf` object with POLYGON or MULTIPOLYGON geometry (source
#'   polygons). Must have a known coordinate reference system.
#' @param measure Character.
#'   Overlap measure: `"area"` (default), `"share"`, or `"count"`.
#' @param unit Character.
#'   Area unit for the `"area"` measure: `"m2"`, `"km2"` (default), `"ha"`,
#'   or `"mi2"`.
#'   Ignored for other measures.
#' @param name Character or `NULL`.
#'   Name of the output column.
#'   If `NULL` (default), the column is named dynamically:
#'   `"overlap_{unit}"` for area (e.g., `"overlap_km2"`, `"overlap_m2"`),
#'   `"overlap_share"` for share, and `"overlap_count"` for count.
#' @param crs Optional.
#'   A CRS specification to project to before computing overlap area.
#' @param repair Logical.
#'   If `TRUE` (default), invalid geometries are repaired on internal copies.
#' @param diagnostics Logical.
#'   If `TRUE`, attach a diagnostics summary as an attribute.
#' @param overwrite Logical.
#'   If `TRUE`, overwrite an existing column named `name`.
#'
#' @return The input `sf` object with a new column containing the requested
#'   overlap measure.
#'
#' @export
#'
#' @examples
#' regions <- example_polygons()
#' zones <- example_grid()
#' spat_overlap(regions, zones)
#' spat_overlap(regions, zones, unit = "ha")
#' spat_overlap(regions, zones, measure = "share", name = "zone_coverage")
spat_overlap <- function(x,
                         y,
                         measure = "area",
                         unit = "km2",
                         name = NULL,
                         crs = NULL,
                         repair = TRUE,
                         diagnostics = FALSE,
                         overwrite = FALSE) {

  validate_sf_polygons(x, "x")
  validate_sf_polygons(y, "y")
  validate_crs(x, "x", "spat_overlap()")
  validate_crs(y, "y", "spat_overlap()")

  measure <- match.arg(measure, c("area", "share", "count"))

  if (measure == "area") {
    validate_unit(unit, area_units())
  }

  # Set default name based on measure and unit
  if (is.null(name)) {
    name <- switch(measure,
      "area"  = paste0("overlap_", unit),
      "share" = "overlap_share",
      "count" = "overlap_count"
    )
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

  if (!is.null(crs)) {
    x_work <- sf::st_transform(x_work, crs)
    y_work <- sf::st_transform(y_work, crs)
  } else {
    y_work <- align_crs(x_work, y_work, arg_x = "x", arg_y = "y",
                        operation = "spat_overlap()")
  }

  # Count measure: use st_intersects (same as spat_count)
  if (measure == "count") {
    if (n_source == 0L) {
      count_out <- rep(0L, n_input)
    } else {
      ints <- sf::st_intersects(x_work, y_work)
      count_out <- as.integer(lengths(ints))
    }

    empty_geom <- sf::st_is_empty(sf::st_geometry(x))
    count_out[empty_geom] <- NA_integer_

    x[[name]] <- count_out
    assert_row_count(x, n_input)

    if (isTRUE(diagnostics)) {
      diag <- list(
        operation = "spat_overlap",
        measure = "count",
        n_target = n_input,
        n_source = n_source,
        n_repaired_target = n_repaired_x,
        n_repaired_source = n_repaired_y,
        n_zero = sum(count_out == 0L, na.rm = TRUE),
        n_na = sum(is.na(count_out)),
        timestamp = Sys.time()
      )
      attr(x, "spat_diagnostics") <- diag
    }
    return(x)
  }

  # Area and share measures
  if (n_source == 0L) {
    overlap_out <- rep(0, n_input)
  } else {
    # Check for positive-area overlap among source polygons (relevant for share)
    if (measure == "share" && n_source > 1L) {
      src_areas <- as.numeric(units::set_units(sf::st_area(y_work), "m^2"))
      sum_src_area <- sum(src_areas, na.rm = TRUE)

      if (sum_src_area > 0) {
        union_geom <- sf::st_union(sf::st_geometry(y_work))
        union_area <- as.numeric(units::set_units(sf::st_area(union_geom), "m^2"))
        diff_rel <- (sum_src_area - union_area) / sum_src_area

        if (diff_rel > 1e-6) {
          warning(
            "Source polygons overlap each other with positive area. ",
            "Overlap share may exceed 1.0 for some target polygons.",
            call. = FALSE
          )
        }
      }
    }

    # Add row identifier
    x_work[["..spat_row_id.."]] <- seq_len(n_input)

    # Compute intersection
    inter <- suppressWarnings(
      sf::st_intersection(x_work["..spat_row_id.."], y_work)
    )

    if (nrow(inter) == 0L) {
      overlap_out <- rep(0, n_input)
    } else {
      # Filter to polygon geometries
      inter_types <- as.character(sf::st_geometry_type(inter))
      poly_mask <- inter_types %in% c(
        "POLYGON", "MULTIPOLYGON", "GEOMETRYCOLLECTION"
      )
      inter <- inter[poly_mask, ]

      if (nrow(inter) == 0L) {
        overlap_out <- rep(0, n_input)
      } else {
        # Compute intersection areas
        inter_areas <- as.numeric(
          units::set_units(sf::st_area(inter), "m^2")
        )
        inter_ids <- inter[["..spat_row_id.."]]

        area_by_id <- tapply(inter_areas, inter_ids, sum, default = 0)
        overlap_out <- rep(0, n_input)
        matched_ids <- as.integer(names(area_by_id))
        overlap_out[matched_ids] <- as.numeric(area_by_id)
      }
    }
  }

  # Convert based on measure
  if (measure == "area") {
    overlap_out <- convert_area_unit(overlap_out, unit)
  } else if (measure == "share") {
    target_areas <- as.numeric(
      units::set_units(sf::st_area(x_work), "m^2")
    )
    target_areas[target_areas == 0] <- NA_real_
    overlap_out <- overlap_out / target_areas
  }

  # Handle empty target geometries -> NA
  empty_geom <- sf::st_is_empty(sf::st_geometry(x))
  overlap_out[empty_geom] <- NA_real_

  x[[name]] <- overlap_out

  assert_row_count(x, n_input)

  if (isTRUE(diagnostics)) {
    crs_used <- if (!is.null(crs)) {
      as.character(sf::st_crs(crs)$input)
    } else {
      as.character(sf::st_crs(x)$input)
    }

    diag <- list(
      operation = "spat_overlap",
      measure = measure,
      n_target = n_input,
      n_source = n_source,
      n_repaired_target = n_repaired_x,
      n_repaired_source = n_repaired_y,
      n_zero = sum(overlap_out == 0, na.rm = TRUE),
      n_na = sum(is.na(overlap_out)),
      crs_used = crs_used,
      unit = if (measure == "area") unit else NA_character_,
      timestamp = Sys.time()
    )
    attr(x, "spat_diagnostics") <- diag
  }

  x
}
