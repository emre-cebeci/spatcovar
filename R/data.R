#' Example polygon regions
#'
#' Returns a small `sf` object with six synthetic polygon regions for use in
#' examples and tests.
#' All geometries use EPSG:32632 (UTM zone 32N).
#'
#' @return An `sf` object with 6 polygons and a `name` column.
#'
#' @export
#'
#' @examples
#' example_polygons()
example_polygons <- function() {
  # Six irregular-ish rectangular regions in UTM 32N
  polys <- list(
    sf::st_polygon(list(rbind(
      c(400000, 5400000), c(410000, 5400000), c(410000, 5410000),
      c(400000, 5410000), c(400000, 5400000)
    ))),
    sf::st_polygon(list(rbind(
      c(410000, 5400000), c(420000, 5400000), c(420000, 5410000),
      c(410000, 5410000), c(410000, 5400000)
    ))),
    sf::st_polygon(list(rbind(
      c(420000, 5400000), c(430000, 5400000), c(430000, 5410000),
      c(420000, 5410000), c(420000, 5400000)
    ))),
    sf::st_polygon(list(rbind(
      c(400000, 5410000), c(410000, 5410000), c(412000, 5420000),
      c(400000, 5420000), c(400000, 5410000)
    ))),
    sf::st_polygon(list(rbind(
      c(412000, 5410000), c(420000, 5410000), c(420000, 5420000),
      c(412000, 5420000), c(412000, 5410000)
    ))),
    sf::st_polygon(list(rbind(
      c(420000, 5410000), c(430000, 5410000), c(430000, 5420000),
      c(420000, 5420000), c(420000, 5410000)
    )))
  )

  sfc <- sf::st_sfc(polys, crs = 32632L)

  sf::st_sf(
    name = c("region_a", "region_b", "region_c",
             "region_d", "region_e", "region_f"),
    geometry = sfc
  )
}


#' Example point features
#'
#' Returns a small `sf` object with 15 synthetic point locations distributed
#' across the example polygon regions.
#' All geometries use EPSG:32632 (UTM zone 32N).
#'
#' @return An `sf` object with 15 points and a `site_id` column.
#'
#' @export
#'
#' @examples
#' example_points()
example_points <- function() {
  coords <- matrix(c(
    # Points in region_a
    402000, 5402000,
    405000, 5405000,
    408000, 5407000,
    # Points in region_b
    412000, 5403000,
    415000, 5406000,
    # Points in region_c
    422000, 5402000,
    425000, 5405000,
    428000, 5408000,
    # Points in region_d
    403000, 5415000,
    407000, 5418000,
    # Points in region_e
    415000, 5415000,
    # Points in region_f
    422000, 5412000,
    425000, 5415000,
    428000, 5418000,
    # Point outside all regions
    435000, 5430000
  ), ncol = 2L, byrow = TRUE)

  pts <- sf::st_sfc(
    lapply(seq_len(nrow(coords)), function(i) {
      sf::st_point(coords[i, ])
    }),
    crs = 32632L
  )

  sf::st_sf(
    site_id = paste0("site_", seq_len(nrow(coords))),
    geometry = pts
  )
}


#' Example line features
#'
#' Returns a small `sf` object with four synthetic line features.
#' Some lines cross polygon boundaries; others are contained within or outside
#' the example regions.
#' All geometries use EPSG:32632 (UTM zone 32N).
#'
#' @return An `sf` object with 4 lines and a `route_id` column.
#'
#' @export
#'
#' @examples
#' example_lines()
example_lines <- function() {
  lines <- list(
    # Route 1: horizontal line crossing regions a, b, c
    sf::st_linestring(rbind(
      c(398000, 5405000), c(432000, 5405000)
    )),
    # Route 2: vertical line crossing regions b and e
    sf::st_linestring(rbind(
      c(415000, 5398000), c(415000, 5422000)
    )),
    # Route 3: diagonal line crossing regions a and d
    sf::st_linestring(rbind(
      c(400000, 5400000), c(410000, 5420000)
    )),
    # Route 4: line fully outside all regions
    sf::st_linestring(rbind(
      c(440000, 5400000), c(450000, 5410000)
    ))
  )

  sfc <- sf::st_sfc(lines, crs = 32632L)

  sf::st_sf(
    route_id = paste0("route_", 1:4),
    geometry = sfc
  )
}


#' Example source polygon grid
#'
#' Returns a small `sf` object with three overlapping rectangular zones.
#' Each zone has a numeric `value` attribute for use in overlap examples.
#' All geometries use EPSG:32632 (UTM zone 32N).
#'
#' @return An `sf` object with 3 polygons, a `zone_id` column, and a `value`
#'   column.
#'
#' @export
#'
#' @examples
#' example_grid()
example_grid <- function() {

  zones <- list(
    # Zone 1: overlaps regions a, b, d, e
    sf::st_polygon(list(rbind(
      c(405000, 5405000), c(418000, 5405000),
      c(418000, 5418000), c(405000, 5418000),
      c(405000, 5405000)
    ))),
    # Zone 2: overlaps regions b, c, e, f
    sf::st_polygon(list(rbind(
      c(415000, 5402000), c(428000, 5402000),
      c(428000, 5415000), c(415000, 5415000),
      c(415000, 5402000)
    ))),
    # Zone 3: overlaps region f only
    sf::st_polygon(list(rbind(
      c(422000, 5415000), c(430000, 5415000),
      c(430000, 5420000), c(422000, 5420000),
      c(422000, 5415000)
    )))
  )

  sfc <- sf::st_sfc(zones, crs = 32632L)

  sf::st_sf(
    zone_id = paste0("zone_", 1:3),
    value = c(10.5, 25.0, 7.3),
    geometry = sfc
  )
}


#' Example raster
#'
#' Returns a small single-layer [terra::SpatRaster] with 16 rows and 20 columns
#' of known values covering the extent of [example_polygons()].
#' Includes some `NA` cells in the upper-right corner and valid zero values.
#'
#' The raster uses EPSG:32632 (UTM zone 32N) with bounding box
#' `xmin = 398000, xmax = 432000, ymin = 5398000, ymax = 5422000`
#' (1700 m in X by 1500 m in Y cell resolution).
#'
#' @return A single-layer [terra::SpatRaster] object.
#'
#' @export
#'
#' @examples
#' example_raster()
example_raster <- function() {
  # Create a 16x20 raster covering the example polygon extent
  r <- terra::rast(
    xmin = 398000, xmax = 432000,
    ymin = 5398000, ymax = 5422000,
    nrows = 16L, ncols = 20L,
    crs = "EPSG:32632"
  )

  # Fill with deterministic values: row-based gradient
  vals <- seq_len(terra::ncell(r))

  # Set some cells to zero (valid zeros)
  vals[c(1, 2, 3, 20, 21)] <- 0

  # Set some cells to NA (NoData region in upper-right corner)
  nr <- terra::nrow(r)
  nc <- terra::ncol(r)
  for (row_i in 1:4) {
    for (col_j in 17:20) {
      cell_idx <- (row_i - 1L) * nc + col_j
      vals[cell_idx] <- NA
    }
  }

  terra::values(r) <- vals
  r
}
