test_that("spat_length computes correct clipped length in metre-based CRS", {
  grid <- make_test_grid()
  lines <- make_test_lines()

  result <- spat_length(grid, lines, unit = "m")

  # Route 1: horizontal at y=0.5 from x=0 to x=2, crosses cells 1 and 2 (1m each)
  # Route 2: vertical at x=0.5 from y=0 to y=2, crosses cells 1 and 3 (1m each)
  # Route 3: outside
  # Cell_1 should have route_1 (1m) + route_2 (1m) = 2m
  expect_equal(result$length_m[1], 2, tolerance = 0.01)
  expect_equal(result$length_m[2], 1, tolerance = 0.01)
  expect_equal(result$length_m[3], 1, tolerance = 0.01)
  expect_equal(result$length_m[4], 0, tolerance = 0.01)
})

test_that("spat_length dynamically generates column names based on unit", {
  grid <- make_test_grid()
  lines <- make_test_lines()

  r_m  <- spat_length(grid, lines, unit = "m")
  r_km <- spat_length(grid, lines, unit = "km")
  r_mi <- spat_length(grid, lines, unit = "mi")

  expect_true("length_m" %in% names(r_m))
  expect_true("length_km" %in% names(r_km))
  expect_true("length_mi" %in% names(r_mi))

  expect_equal(r_km$length_km, r_m$length_m / 1000, tolerance = 1e-6)
  expect_equal(r_mi$length_mi, r_m$length_m / 1609.344, tolerance = 1e-6)
})

test_that("spat_length allows explicit name override", {
  grid <- make_test_grid()
  lines <- make_test_lines()

  res <- spat_length(grid, lines, unit = "m", name = "custom_len")
  expect_true("custom_len" %in% names(res))
  expect_false("length_m" %in% names(res))
})

test_that("spat_length correctly handles foot-based projected CRS", {
  # EPSG:2227 (NAD83 / California zone 3 (ftUS))
  # 5280 US survey feet = 1609.347 m = 1.609347 km
  poly_ft <- sf::st_sf(
    name = "poly_ft",
    geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(6000000, 2000000), c(6010000, 2000000),
      c(6010000, 2010000), c(6000000, 2010000),
      c(6000000, 2000000)
    ))), crs = 2227L)
  )
  line_ft <- sf::st_sf(
    route = "mile_line",
    geometry = sf::st_sfc(sf::st_linestring(rbind(
      c(6000000, 2005000), c(6005280, 2005000)
    )), crs = 2227L)
  )

  res_m <- spat_length(poly_ft, line_ft, unit = "m")
  res_km <- spat_length(poly_ft, line_ft, unit = "km")
  res_mi <- spat_length(poly_ft, line_ft, unit = "mi")

  expected_m <- 5280 * (1200 / 3937)
  expect_equal(res_m$length_m[1], expected_m, tolerance = 0.01)
  expect_equal(res_km$length_km[1], expected_m / 1000, tolerance = 1e-4)
  expect_equal(res_mi$length_mi[1], 1.0, tolerance = 1e-4)
})

test_that("spat_length results agree between metre-based and foot-based CRS", {
  # Create a polygon and line in UTM 32N (metres)
  poly_m <- sf::st_sf(
    name = "p1",
    geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(400000, 5400000), c(410000, 5400000),
      c(410000, 5410000), c(400000, 5410000),
      c(400000, 5400000)
    ))), crs = 32632L)
  )
  line_m <- sf::st_sf(
    route = "r1",
    geometry = sf::st_sfc(sf::st_linestring(rbind(
      c(395000, 5405000), c(415000, 5405000)
    )), crs = 32632L)
  )

  # Transform to EPSG:2227 (feet)
  poly_ft <- sf::st_transform(poly_m, 2227L)
  line_ft <- sf::st_transform(line_m, 2227L)

  len_from_m <- spat_length(poly_m, line_m, unit = "km")
  len_from_ft <- spat_length(poly_ft, line_ft, unit = "km")

  expect_equal(len_from_m$length_km, len_from_ft$length_km, tolerance = 0.05)
})

test_that("spat_length errors informatively when CRS is missing", {
  grid <- make_test_grid()
  lines <- make_test_lines()

  grid_no_crs <- grid
  sf::st_crs(grid_no_crs) <- NA

  lines_no_crs <- lines
  sf::st_crs(lines_no_crs) <- NA

  # Missing target CRS
  expect_error(
    spat_length(grid_no_crs, lines),
    "`x` has no CRS"
  )

  # Missing source CRS
  expect_error(
    spat_length(grid, lines_no_crs),
    "`y` has no CRS"
  )

  # Both missing CRS
  expect_error(
    spat_length(grid_no_crs, lines_no_crs),
    "`x` has no CRS"
  )
})

test_that("spat_length enforces planar CRS and rejects geographic inputs or supplied CRS", {
  geo <- make_geo_polygon()
  line_geo <- sf::st_sf(
    id = "line",
    geometry = sf::st_sfc(
      sf::st_linestring(rbind(c(8.5, 48.5), c(8.5, 49))),
      crs = 4326L
    )
  )

  # 1. Geographic x and y, crs = NULL -> informative error
  expect_error(
    spat_length(geo, line_geo, crs = NULL),
    "Geographic CRS detected.*requires a projected CRS"
  )

  # 2. Geographic x and y, geographic supplied crs (e.g. 4326) -> informative error
  expect_error(
    spat_length(geo, line_geo, crs = 4326L),
    "Geographic `crs` supplied.*requires a projected CRS"
  )

  # 3. Projected x and y, geographic supplied crs (e.g. 4326) -> informative error
  grid <- make_test_grid()
  lines <- make_test_lines()
  expect_error(
    spat_length(grid, lines, crs = 4326L),
    "Geographic `crs` supplied.*requires a projected CRS"
  )

  # 4. Geographic x and y, valid projected supplied crs -> succeeds
  res_geo_proj <- spat_length(geo, line_geo, crs = 32632L, unit = "km")
  expect_true(res_geo_proj$length_km[1] > 0)

  # 5. Already projected inputs with crs = NULL -> succeeds
  res_proj <- spat_length(grid, lines, crs = NULL, unit = "m")
  expect_true(all(res_proj$length_m >= 0))
})

test_that("spat_length returns 0 for non-intersecting lines", {
  grid <- make_test_grid()
  outside_line <- sf::st_sf(
    route = "far",
    geometry = sf::st_sfc(
      sf::st_linestring(rbind(c(10, 10), c(20, 20))),
      crs = 32632L
    )
  )

  result <- spat_length(grid, outside_line, unit = "m")
  expect_equal(result$length_m, c(0, 0, 0, 0))
})

test_that("spat_length returns 0 for empty source", {
  grid <- make_test_grid()
  empty <- sf::st_sf(
    route = character(0),
    geometry = sf::st_sfc(crs = 32632L)
  )
  class(empty$geometry) <- c("sfc_LINESTRING", "sfc")

  result <- spat_length(grid, empty, unit = "m")
  expect_equal(result$length_m, c(0, 0, 0, 0))
})

test_that("spat_length rejects non-line geometry", {
  grid <- make_test_grid()
  pts <- make_test_points()

  expect_error(spat_length(grid, pts), "LINESTRING or MULTILINESTRING")
})

test_that("spat_length preserves rows, columns, and geometry", {
  grid <- make_test_grid()
  lines <- make_test_lines()
  n <- nrow(grid)
  orig_geom <- sf::st_geometry(grid)

  result <- spat_length(grid, lines)

  expect_equal(nrow(result), n)
  expect_identical(result$name, grid$name)
  expect_identical(sf::st_geometry(result), orig_geom)
})

test_that("spat_length errors on column collision without overwrite", {
  grid <- make_test_grid()
  lines <- make_test_lines()
  r1 <- spat_length(grid, lines, unit = "km")
  expect_error(spat_length(r1, lines, unit = "km"), "already exists")
})

test_that("spat_length allows overwrite = TRUE", {
  grid <- make_test_grid()
  lines <- make_test_lines()
  r1 <- spat_length(grid, lines, unit = "km")
  r2 <- spat_length(r1, lines, unit = "km", overwrite = TRUE)
  expect_true("length_km" %in% names(r2))
})

test_that("spat_length attaches diagnostics when requested", {
  grid <- make_test_grid()
  lines <- make_test_lines()
  result <- spat_length(grid, lines, diagnostics = TRUE)
  diag <- attr(result, "spat_diagnostics")
  expect_equal(diag$operation, "spat_length")
  expect_equal(diag$n_source, 3L)
})
