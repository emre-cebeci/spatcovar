test_that("validate_sf_polygons rejects non-sf input", {
  expect_error(
    spatcovar:::validate_sf_polygons(data.frame(a = 1)),
    "must be an sf object"
  )
})

test_that("validate_sf_polygons rejects non-polygon geometry", {
  pts <- sf::st_sf(
    id = "pt",
    geometry = sf::st_sfc(sf::st_point(c(1, 1)), crs = 32632L)
  )
  expect_error(
    spatcovar:::validate_sf_polygons(pts),
    "POLYGON or MULTIPOLYGON"
  )
})

test_that("validate_sf_polygons accepts POLYGON and MULTIPOLYGON", {
  grid <- make_test_grid()
  expect_silent(spatcovar:::validate_sf_polygons(grid))
})

test_that("validate_crs rejects object without CRS", {
  grid <- make_test_grid()
  sf::st_crs(grid) <- NA

  expect_error(
    spatcovar:::validate_crs(grid, "x", "test_op()"),
    "`x` has no CRS. `test_op\\(\\)` requires a known CRS."
  )
})

test_that("validate_crs accepts object with known CRS", {
  grid <- make_test_grid()
  expect_silent(spatcovar:::validate_crs(grid))
})

test_that("validate_name_collision errors on collision", {
  grid <- make_test_grid()
  expect_error(
    spatcovar:::validate_name_collision(grid, "name", FALSE),
    "already exists"
  )
})

test_that("validate_name_collision allows overwrite", {
  grid <- make_test_grid()
  expect_silent(
    spatcovar:::validate_name_collision(grid, "name", TRUE)
  )
})

test_that("validate_name rejects empty string", {
  expect_error(
    spatcovar:::validate_name(""),
    "non-empty"
  )
})

test_that("validate_name rejects NA", {
  expect_error(
    spatcovar:::validate_name(NA_character_),
    "non-empty"
  )
})

test_that("validate_unit rejects invalid unit", {
  expect_error(
    spatcovar:::validate_unit("furlongs", c("m", "km")),
    "must be one of"
  )
})

test_that("assert_row_count produces informative error", {
  df <- data.frame(a = 1:3)
  expect_error(
    spatcovar:::assert_row_count(df, 5),
    "Internal error.*bug.*report"
  )
})

test_that("is_geographic detects geographic CRS", {
  geo <- make_geo_polygon()
  expect_true(spatcovar:::is_geographic(geo))

  proj <- make_test_grid()
  expect_false(spatcovar:::is_geographic(proj))
})

test_that("ensure_planar_crs errors on missing CRS", {
  grid <- make_test_grid()
  sf::st_crs(grid) <- NA

  expect_error(
    spatcovar:::ensure_planar_crs(grid, operation = "spat_length()"),
    "`x` has no CRS"
  )
})

test_that("ensure_planar_crs errors on geographic without crs", {
  geo <- make_geo_polygon()
  expect_error(
    spatcovar:::ensure_planar_crs(geo),
    "Geographic CRS detected"
  )
})

test_that("ensure_planar_crs projects when valid planar crs supplied", {
  geo <- make_geo_polygon()
  result <- spatcovar:::ensure_planar_crs(geo, crs = 32632L)
  expect_false(spatcovar:::is_geographic(result))
})

test_that("ensure_planar_crs rejects geographic supplied crs", {
  geo <- make_geo_polygon()
  expect_error(
    spatcovar:::ensure_planar_crs(geo, crs = 4326L, operation = "spat_length()"),
    "Geographic `crs` supplied.*requires a projected CRS"
  )

  proj <- make_test_grid()
  expect_error(
    spatcovar:::ensure_planar_crs(proj, crs = 4326L, operation = "spat_length()"),
    "Geographic `crs` supplied.*requires a projected CRS"
  )
})

test_that("ensure_planar_crs rejects invalid supplied crs", {
  proj <- make_test_grid()
  expect_error(
    spatcovar:::ensure_planar_crs(proj, crs = "invalid_crs_string", operation = "spat_length()"),
    "Invalid `crs` supplied"
  )
})

test_that("align_crs errors when either input has missing CRS", {
  x <- make_test_grid(crs = 32632L)
  y <- make_test_points(crs = 32632L)

  x_no_crs <- x
  sf::st_crs(x_no_crs) <- NA

  y_no_crs <- y
  sf::st_crs(y_no_crs) <- NA

  expect_error(spatcovar:::align_crs(x_no_crs, y), "`x` has no CRS")
  expect_error(spatcovar:::align_crs(x, y_no_crs), "`y` has no CRS")
  expect_error(spatcovar:::align_crs(x_no_crs, y_no_crs), "`x` has no CRS")
})

test_that("align_crs transforms y to match x when both have known CRSs", {
  x <- make_test_grid(crs = 32632L)
  y <- sf::st_sf(
    id = "pt",
    geometry = sf::st_sfc(sf::st_point(c(8.5, 48.5)), crs = 4326L)
  )

  y_aligned <- spatcovar:::align_crs(x, y)
  expect_equal(sf::st_crs(y_aligned), sf::st_crs(x))
})

test_that("repair_geometries counts repaired features", {
  bowtie <- sf::st_polygon(list(rbind(
    c(0, 0), c(1, 1), c(1, 0), c(0, 1), c(0, 0)
  )))
  sfc <- sf::st_sfc(bowtie, crs = 32632L)
  bad <- sf::st_sf(name = "bowtie", geometry = sfc)

  result <- spatcovar:::repair_geometries(bad)
  expect_equal(result$n_repaired, 1L)
  expect_true(all(sf::st_is_valid(result$data)))
})

test_that("convert_area_unit converts correctly", {
  expect_equal(spatcovar:::convert_area_unit(1e6, "km2"), 1)
  expect_equal(spatcovar:::convert_area_unit(1e4, "ha"), 1)
  expect_equal(spatcovar:::convert_area_unit(1e6, "m2"), 1e6)
})

test_that("convert_length_unit converts correctly", {
  expect_equal(spatcovar:::convert_length_unit(1000, "km"), 1)
  expect_equal(spatcovar:::convert_length_unit(1000, "m"), 1000)
  expect_equal(
    spatcovar:::convert_length_unit(1609.344, "mi"), 1,
    tolerance = 1e-6
  )
})

test_that("the intentional public API remains stable", {
  expected_exports <- sort(c(
    "spat_area",
    "spat_distance",
    "spat_count",
    "spat_length",
    "spat_overlap",
    "spat_raster",
    "spat_diagnostics",
    "example_polygons",
    "example_points",
    "example_lines",
    "example_grid",
    "example_raster"
  ))

  actual_exports <- sort(getNamespaceExports("spatcovar"))
  expect_equal(actual_exports, expected_exports)
})
