test_that("spat_area computes correct area for known squares", {
  grid <- make_test_grid()
  result <- spat_area(grid, unit = "m2")
  # Each cell is 1m x 1m = 1 m2

  expect_equal(result$area_m2, c(1, 1, 1, 1))
})

test_that("spat_area dynamically generates column names based on unit", {
  grid <- make_test_grid()

  r_m2  <- spat_area(grid, unit = "m2")
  r_km2 <- spat_area(grid, unit = "km2")
  r_ha  <- spat_area(grid, unit = "ha")
  r_mi2 <- spat_area(grid, unit = "mi2")

  expect_true("area_m2" %in% names(r_m2))
  expect_true("area_km2" %in% names(r_km2))
  expect_true("area_ha" %in% names(r_ha))
  expect_true("area_mi2" %in% names(r_mi2))

  expect_equal(r_km2$area_km2, r_m2$area_m2 / 1e6)
  expect_equal(r_ha$area_ha, r_m2$area_m2 / 1e4)
  expect_equal(r_mi2$area_mi2, r_m2$area_m2 / 2589988.110336)
})

test_that("spat_area allows explicit name override", {
  grid <- make_test_grid()
  result <- spat_area(grid, unit = "m2", name = "custom_area")
  expect_true("custom_area" %in% names(result))
  expect_false("area_m2" %in% names(result))
})

test_that("spat_area errors informatively when CRS is missing", {
  grid <- make_test_grid()
  sf::st_crs(grid) <- NA

  expect_error(
    spat_area(grid),
    "`x` has no CRS"
  )
})

test_that("spat_area works with geographic CRS (geodesic)", {
  geo <- make_geo_polygon()
  result <- spat_area(geo, unit = "km2")
  # A ~1 degree square at lat 48 should be roughly 7000-8500 km2

  expect_true(result$area_km2 > 5000)
  expect_true(result$area_km2 < 10000)
})

test_that("spat_area accepts user-supplied CRS", {
  geo <- make_geo_polygon()
  result <- spat_area(geo, unit = "km2", crs = 32632L)
  expect_true(result$area_km2 > 5000)
  expect_true(result$area_km2 < 10000)
})

test_that("spat_area correctly computes area in foot-based CRS", {
  # 5280 ft x 5280 ft square = 1 sq mile = 2.589988 km2
  poly_ft <- sf::st_sf(
    name = "sq_mile",
    geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(6000000, 2000000), c(6005280, 2000000),
      c(6005280, 2005280), c(6000000, 2005280),
      c(6000000, 2000000)
    ))), crs = 2227L)
  )

  res_mi2 <- spat_area(poly_ft, unit = "mi2")
  res_km2 <- spat_area(poly_ft, unit = "km2")

  expect_equal(res_mi2$area_mi2[1], 1.0, tolerance = 1e-4)
  expect_equal(res_km2$area_km2[1], 2.589988, tolerance = 1e-3)
})

test_that("spat_area preserves rows, columns, and geometry", {
  grid <- make_test_grid()
  n <- nrow(grid)
  orig_geom <- sf::st_geometry(grid)
  orig_names <- grid$name

  result <- spat_area(grid)

  expect_equal(nrow(result), n)
  expect_identical(result$name, orig_names)
  expect_identical(sf::st_geometry(result), orig_geom)
  expect_true("area_km2" %in% names(result))
})

test_that("spat_area errors on column collision without overwrite", {
  grid <- make_test_grid()
  result <- spat_area(grid, name = "area")
  expect_error(
    spat_area(result, name = "area"),
    "already exists"
  )
})

test_that("spat_area overwrites with overwrite = TRUE", {
  grid <- make_test_grid()
  r1 <- spat_area(grid, name = "area", unit = "m2")
  r2 <- spat_area(r1, name = "area", unit = "km2", overwrite = TRUE)
  expect_true("area" %in% names(r2))
  expect_true(r2$area[1] < r1$area[1])
})

test_that("spat_area rejects non-sf input", {
  expect_error(spat_area(data.frame(a = 1)), "must be an sf object")
})

test_that("spat_area rejects invalid unit", {
  grid <- make_test_grid()
  expect_error(spat_area(grid, unit = "furlongs"), "must be one of")
})

test_that("spat_area attaches diagnostics when requested", {
  grid <- make_test_grid()
  result <- spat_area(grid, diagnostics = TRUE)
  diag <- attr(result, "spat_diagnostics")
  expect_type(diag, "list")
  expect_equal(diag$operation, "spat_area")
  expect_equal(diag$n_target, 4L)
  expect_equal(diag$unit, "km2")
})

test_that("spat_area does not attach diagnostics by default", {
  grid <- make_test_grid()
  result <- spat_area(grid)
  expect_null(attr(result, "spat_diagnostics"))
})

test_that("spat_area returns original geometry when repair needed", {
  bowtie <- sf::st_polygon(list(rbind(
    c(0, 0), c(1, 1), c(1, 0), c(0, 1), c(0, 0)
  )))
  sfc <- sf::st_sfc(bowtie, crs = 32632L)
  bad <- sf::st_sf(name = "bowtie", geometry = sfc)

  orig_geom <- sf::st_geometry(bad)
  result <- spat_area(bad, unit = "m2", repair = TRUE)

  # Geometry returned is the original (invalid) geometry
  expect_identical(sf::st_geometry(result), orig_geom)
  # But area was still computed (repair happened internally)
  expect_true(!is.na(result$area_m2))
})
