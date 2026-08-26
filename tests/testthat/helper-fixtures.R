# Shared test fixtures for spatcovar tests

# Create a simple 2x2 grid of unit squares in a projected CRS
make_test_grid <- function(crs = 32632L) {
  polys <- list(
    sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
    sf::st_polygon(list(rbind(c(1, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 0)))),
    sf::st_polygon(list(rbind(c(0, 1), c(1, 1), c(1, 2), c(0, 2), c(0, 1)))),
    sf::st_polygon(list(rbind(c(1, 1), c(2, 1), c(2, 2), c(1, 2), c(1, 1))))
  )

  sfc <- sf::st_sfc(polys, crs = crs)
  sf::st_sf(name = paste0("cell_", 1:4), geometry = sfc)
}

# Create test points
make_test_points <- function(crs = 32632L) {
  pts <- sf::st_sfc(
    sf::st_point(c(0.5, 0.5)),   # in cell_1
    sf::st_point(c(0.5, 0.5)),   # duplicate in cell_1
    sf::st_point(c(1.5, 0.5)),   # in cell_2
    sf::st_point(c(0.5, 1.5)),   # in cell_3
    sf::st_point(c(3.0, 3.0)),   # outside all cells
    crs = crs
  )
  sf::st_sf(id = paste0("pt_", 1:5), geometry = pts)
}

# Create test lines
make_test_lines <- function(crs = 32632L) {
  lines <- sf::st_sfc(
    # Horizontal line crossing cells 1 and 2
    sf::st_linestring(rbind(c(0, 0.5), c(2, 0.5))),
    # Vertical line crossing cells 1 and 3
    sf::st_linestring(rbind(c(0.5, 0), c(0.5, 2))),
    # Line fully outside
    sf::st_linestring(rbind(c(5, 5), c(6, 6))),
    crs = crs
  )
  sf::st_sf(route = paste0("route_", 1:3), geometry = lines)
}

# Create overlapping source polygons
make_test_source_polys <- function(crs = 32632L) {
  polys <- sf::st_sfc(
    # Overlaps cells 1,2,3,4
    sf::st_polygon(list(rbind(
      c(0.5, 0.5), c(1.5, 0.5), c(1.5, 1.5), c(0.5, 1.5), c(0.5, 0.5)
    ))),
    # Overlaps cells 2,4 partially
    sf::st_polygon(list(rbind(
      c(1.25, 0.25), c(2.5, 0.25), c(2.5, 2.5), c(1.25, 2.5), c(1.25, 0.25)
    ))),
    crs = crs
  )
  sf::st_sf(zone = c("zone_a", "zone_b"), value = c(10.0, 20.0), geometry = polys)
}

# Create a simple geographic CRS polygon (WGS84)
make_geo_polygon <- function() {
  poly <- sf::st_polygon(list(rbind(
    c(8, 48), c(9, 48), c(9, 49), c(8, 49), c(8, 48)
  )))
  sfc <- sf::st_sfc(poly, crs = 4326L)
  sf::st_sf(name = "geo_region", geometry = sfc)
}

# Create a small test raster
make_test_raster <- function() {
  r <- terra::rast(
    xmin = 0, xmax = 2,
    ymin = 0, ymax = 2,
    nrows = 4L, ncols = 4L,
    crs = "EPSG:32632"
  )
  vals <- c(
    10, 20, 30, 40,   # row 1 (top)
    50, 60, 70, 80,   # row 2
    0,  0,  0,  0,    # row 3 (valid zeros)
    NA, NA, 100, 110  # row 4 (partial NA)
  )
  terra::values(r) <- vals
  r
}
