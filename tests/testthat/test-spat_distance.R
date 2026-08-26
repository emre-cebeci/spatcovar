test_that("spat_distance computes correct known distances", {
  grid <- make_test_grid()
  # Single point at (3, 0.5), nearest to cell_2 (edge at x=2)
  ref <- sf::st_sf(
    id = "far_point",
    geometry = sf::st_sfc(sf::st_point(c(3, 0.5)), crs = 32632L)
  )

  result <- spat_distance(grid, ref, unit = "m")
  # cell_2: 3 - 2 = 1m (nearest edge at (2, 0.5))
  expect_equal(result$dist_m[2], 1, tolerance = 0.01)
  # cell_1 farther than cell_2
  expect_true(result$dist_m[1] > result$dist_m[2])
})

test_that("spat_distance dynamically generates column names based on unit", {
  grid <- make_test_grid()
  ref <- sf::st_sf(
    id = "ref",
    geometry = sf::st_sfc(sf::st_point(c(3, 0.5)), crs = 32632L)
  )

  r_m  <- spat_distance(grid, ref, unit = "m")
  r_km <- spat_distance(grid, ref, unit = "km")
  r_mi <- spat_distance(grid, ref, unit = "mi")

  expect_true("dist_m" %in% names(r_m))
  expect_true("dist_km" %in% names(r_km))
  expect_true("dist_mi" %in% names(r_mi))

  expect_equal(r_km$dist_km, r_m$dist_m / 1000, tolerance = 1e-6)
  expect_equal(r_mi$dist_mi, r_m$dist_m / 1609.344, tolerance = 1e-6)
})

test_that("spat_distance allows explicit name override", {
  grid <- make_test_grid()
  ref <- sf::st_sf(
    id = "ref",
    geometry = sf::st_sfc(sf::st_point(c(3, 0.5)), crs = 32632L)
  )

  result <- spat_distance(grid, ref, unit = "m", name = "custom_dist")
  expect_true("custom_dist" %in% names(result))
  expect_false("dist_m" %in% names(result))
})

test_that("spat_distance indexed matches full pairwise on small fixture", {
  grid <- make_test_grid()
  pts <- make_test_points()

  # Indexed approach (default)
  result_indexed <- spat_distance(grid, pts, unit = "m")

  # Full pairwise approach
  grid_work <- grid
  pts_work <- pts
  full_matrix <- sf::st_distance(grid_work, pts_work)
  full_min <- apply(full_matrix, 1, min)
  full_min_m <- as.numeric(units::set_units(full_min, "m"))

  expect_equal(result_indexed$dist_m, full_min_m, tolerance = 1e-6)
})

test_that("spat_distance supports centroid method", {
  grid <- make_test_grid()
  ref <- sf::st_sf(
    id = "ref",
    geometry = sf::st_sfc(sf::st_point(c(0.5, 0.5)), crs = 32632L)
  )

  r_cent <- spat_distance(grid, ref, method = "centroid", unit = "m",
                          name = "cdist")

  # For cell_1, centroid is at (0.5, 0.5) = same as ref, dist = 0
  expect_equal(r_cent$cdist[1], 0, tolerance = 1e-6)
})

test_that("spat_distance supports point_on_surface method", {
  grid <- make_test_grid()
  ref <- sf::st_sf(
    id = "ref",
    geometry = sf::st_sfc(sf::st_point(c(0.5, 0.5)), crs = 32632L)
  )

  result <- spat_distance(grid, ref, method = "point_on_surface", unit = "m",
                          name = "pos_dist")
  expect_true(all(!is.na(result$pos_dist)))
})

test_that("spat_distance works with single-feature y", {
  grid <- make_test_grid()
  ref <- sf::st_sf(
    id = "single",
    geometry = sf::st_sfc(sf::st_point(c(5, 5)), crs = 32632L)
  )

  result <- spat_distance(grid, ref, unit = "m")
  # All cells should have positive distance to this far point
  expect_true(all(result$dist_m > 0))
})

test_that("spat_distance returns NA for empty source", {
  grid <- make_test_grid()
  empty_pts <- sf::st_sf(
    id = character(0),
    geometry = sf::st_sfc(crs = 32632L)
  )

  result <- spat_distance(grid, empty_pts)
  expect_true(all(is.na(result$dist_km)))
})

test_that("spat_distance errors informatively when CRS is missing", {
  grid <- make_test_grid()
  pts <- make_test_points()

  grid_no_crs <- grid
  sf::st_crs(grid_no_crs) <- NA

  pts_no_crs <- pts
  sf::st_crs(pts_no_crs) <- NA

  # Missing target CRS
  expect_error(
    spat_distance(grid_no_crs, pts),
    "`x` has no CRS"
  )

  # Missing source CRS
  expect_error(
    spat_distance(grid, pts_no_crs),
    "`y` has no CRS"
  )

  # Both missing CRS
  expect_error(
    spat_distance(grid_no_crs, pts_no_crs),
    "`x` has no CRS"
  )
})

test_that("spat_distance transforms when source has different known CRS", {
  grid_utm <- make_test_grid(crs = 32632L)
  ref_geo <- sf::st_sf(
    id = "geo_pt",
    geometry = sf::st_sfc(sf::st_point(c(8.5, 48.5)), crs = 4326L)
  )

  # Should transform ref_geo to EPSG:32632 internally and compute distance
  res <- spat_distance(grid_utm, ref_geo, unit = "km")
  expect_true(all(res$dist_km > 0))
})

test_that("spat_distance works in foot-based projected CRS", {
  # EPSG:2227 (ftUS)
  poly_ft <- sf::st_sf(
    name = "poly_ft",
    geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(6000000, 2000000), c(6010000, 2000000),
      c(6010000, 2010000), c(6000000, 2010000),
      c(6000000, 2000000)
    ))), crs = 2227L)
  )
  ref_ft <- sf::st_sf(
    id = "ref_5280ft_away",
    geometry = sf::st_sfc(sf::st_point(c(6015280, 2005000)), crs = 2227L)
  )

  res_mi <- spat_distance(poly_ft, ref_ft, unit = "mi")
  expect_equal(res_mi$dist_mi[1], 1.0, tolerance = 1e-4)
})

test_that("spat_distance preserves rows, columns, and geometry", {
  grid <- make_test_grid()
  pts <- make_test_points()
  n <- nrow(grid)
  orig_geom <- sf::st_geometry(grid)

  result <- spat_distance(grid, pts)

  expect_equal(nrow(result), n)
  expect_identical(result$name, grid$name)
  expect_identical(sf::st_geometry(result), orig_geom)
})

test_that("spat_distance errors on column collision without overwrite", {
  grid <- make_test_grid()
  pts <- make_test_points()
  r1 <- spat_distance(grid, pts)
  expect_error(spat_distance(r1, pts), "already exists")
})

test_that("spat_distance rejects non-sf input", {
  expect_error(
    spat_distance(data.frame(a = 1), data.frame(b = 2)),
    "must be an sf"
  )
})

test_that("spat_distance works with geographic CRS (geodesic)", {
  geo <- make_geo_polygon()
  ref <- sf::st_sf(
    id = "far",
    geometry = sf::st_sfc(sf::st_point(c(10, 50)), crs = 4326L)
  )

  result <- spat_distance(geo, ref, unit = "km")
  expect_true(result$dist_km > 0)
  expect_true(result$dist_km < 500)
})

test_that("spat_distance attaches diagnostics when requested", {
  grid <- make_test_grid()
  pts <- make_test_points()
  result <- spat_distance(grid, pts, diagnostics = TRUE)
  diag <- attr(result, "spat_diagnostics")
  expect_equal(diag$operation, "spat_distance")
  expect_equal(diag$method, "minimum")
})
