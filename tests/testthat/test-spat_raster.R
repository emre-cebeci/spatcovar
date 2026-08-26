test_that("spat_raster computes correct mean for known raster", {
  grid <- make_test_grid()
  r <- make_test_raster()

  result <- spat_raster(grid, r, stats = "mean")

  expect_true(all(!is.na(result$mean)))
})

test_that("spat_raster rejects multilayer rasters with informative error", {
  grid <- make_test_grid()
  r <- make_test_raster()

  # Create a 2-layer raster
  r_multi <- c(r, r)

  expect_error(
    spat_raster(grid, r_multi),
    "supports single-layer rasters only"
  )
})

test_that("spat_raster errors informatively when CRS is missing", {
  grid <- make_test_grid()
  r <- make_test_raster()

  grid_no_crs <- grid
  sf::st_crs(grid_no_crs) <- NA

  r_no_crs <- r
  terra::crs(r_no_crs) <- ""

  # Missing polygon CRS
  expect_error(
    spat_raster(grid_no_crs, r),
    "`x` has no CRS"
  )

  # Missing raster CRS
  expect_error(
    spat_raster(grid, r_no_crs),
    "`raster` has no CRS"
  )
})

test_that("spat_raster enforces strict missing-data semantics across all statistics", {
  # Construct a 2x2 raster with:
  # (0,0)-(1,1) [col 1, row 2]: value 0 (valid zero)
  # (1,0)-(2,1) [col 2, row 2]: value 10 (valid positive)
  # (0,1)-(1,2) [col 1, row 1]: value NA (NoData)
  # (1,1)-(2,2) [col 2, row 1]: value NA (NoData)
  # Extent: xmin=0, xmax=2, ymin=0, ymax=2
  r <- terra::rast(
    xmin = 0, xmax = 2,
    ymin = 0, ymax = 2,
    nrows = 2L, ncols = 2L,
    crs = "EPSG:32632"
  )
  # terra fills matrix by rows from top (row 1 is y=1..2, row 2 is y=0..1)
  terra::values(r) <- c(NA_real_, NA_real_, 0.0, 10.0)

  # 4 test polygons:
  # p_zero: covers (0,0)-(1,1) -> valid 0
  # p_val:  covers (1,0)-(2,1) -> valid 10
  # p_na:   covers (0,1)-(1,2) -> all NA cells in raster extent
  # p_out:  covers (10,10)-(11,11) -> completely outside raster extent
  polys <- list(
    sf::st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))),
    sf::st_polygon(list(rbind(c(1,0), c(2,0), c(2,1), c(1,1), c(1,0)))),
    sf::st_polygon(list(rbind(c(0,1), c(1,1), c(1,2), c(0,2), c(0,1)))),
    sf::st_polygon(list(rbind(c(10,10), c(11,10), c(11,11), c(10,11), c(10,10))))
  )
  test_sf <- sf::st_sf(
    poly_id = c("p_zero", "p_val", "p_na", "p_out"),
    geometry = sf::st_sfc(polys, crs = 32632L)
  )

  all_stats <- c("mean", "median", "min", "max", "sum", "count", "stdev")
  res <- spat_raster(test_sf, r, stats = all_stats)

  # Check Row count and ordering
  expect_equal(nrow(res), 4L)
  expect_identical(res$poly_id, c("p_zero", "p_val", "p_na", "p_out"))

  # 1. p_val: valid positive value (10)
  expect_equal(res$mean[2], 10.0)
  expect_equal(res$median[2], 10.0)
  expect_equal(res$min[2], 10.0)
  expect_equal(res$max[2], 10.0)
  expect_equal(res$sum[2], 10.0)
  expect_equal(res$count[2], 1.0)
  expect_equal(res$stdev[2], 0.0)

  # 2. p_zero: valid zero value (0) — must NOT be converted to NA
  expect_equal(res$mean[1], 0.0)
  expect_equal(res$median[1], 0.0)
  expect_equal(res$min[1], 0.0)
  expect_equal(res$max[1], 0.0)
  expect_equal(res$sum[1], 0.0)
  expect_equal(res$count[1], 1.0)
  expect_equal(res$stdev[1], 0.0)

  # 3. p_na: all covered cells are NA -> ALL stats must be NA_real_
  expect_identical(res$mean[3], NA_real_)
  expect_identical(res$median[3], NA_real_)
  expect_identical(res$min[3], NA_real_)
  expect_identical(res$max[3], NA_real_)
  expect_identical(res$sum[3], NA_real_)
  expect_identical(res$count[3], NA_real_)
  expect_identical(res$stdev[3], NA_real_)

  # 4. p_out: completely outside raster -> ALL stats must be NA_real_
  expect_identical(res$mean[4], NA_real_)
  expect_identical(res$median[4], NA_real_)
  expect_identical(res$min[4], NA_real_)
  expect_identical(res$max[4], NA_real_)
  expect_identical(res$sum[4], NA_real_)
  expect_identical(res$count[4], NA_real_)
  expect_identical(res$stdev[4], NA_real_)

  # 5. Verify single stat extractions adhere to the exact same NA rule
  res_mean <- spat_raster(test_sf, r, stats = "mean")
  expect_equal(res_mean$mean[1], 0.0)
  expect_equal(res_mean$mean[2], 10.0)
  expect_identical(res_mean$mean[3], NA_real_)
  expect_identical(res_mean$mean[4], NA_real_)

  res_sum <- spat_raster(test_sf, r, stats = "sum")
  expect_equal(res_sum$sum[1], 0.0)
  expect_equal(res_sum$sum[2], 10.0)
  expect_identical(res_sum$sum[3], NA_real_)
  expect_identical(res_sum$sum[4], NA_real_)

  res_count <- spat_raster(test_sf, r, stats = "count")
  expect_equal(res_count$count[1], 1.0)
  expect_equal(res_count$count[2], 1.0)
  expect_identical(res_count$count[3], NA_real_)
  expect_identical(res_count$count[4], NA_real_)
})

test_that("spat_raster count statistic accurately reflects fractional coverage", {
  # 1x1 raster cell with value 100
  r <- terra::rast(
    xmin = 0, xmax = 1,
    ymin = 0, ymax = 1,
    nrows = 1L, ncols = 1L,
    crs = "EPSG:32632"
  )
  terra::values(r) <- 100

  # Polygon 1: covers full cell (1.0)
  # Polygon 2: covers quarter of cell (0.25)
  # Polygon 3: outside (0.0 -> NA_real_)
  polys <- list(
    sf::st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))),
    sf::st_polygon(list(rbind(c(0,0), c(0.5,0), c(0.5,0.5), c(0,0.5), c(0,0)))),
    sf::st_polygon(list(rbind(c(5,5), c(6,5), c(6,6), c(5,6), c(5,5))))
  )
  test_sf <- sf::st_sf(
    id = paste0("p", 1:3),
    geometry = sf::st_sfc(polys, crs = 32632L)
  )

  res <- spat_raster(test_sf, r, stats = "count")

  expect_equal(res$count[1], 1.0, tolerance = 1e-4)
  expect_equal(res$count[2], 0.25, tolerance = 1e-4)
  expect_identical(res$count[3], NA_real_)
})

test_that("spat_raster mean statistic is coverage-fraction weighted", {
  # 2x1 raster with left cell = 10, right cell = 100
  r <- terra::rast(
    xmin = 0, xmax = 2,
    ymin = 0, ymax = 1,
    nrows = 1L, ncols = 2L,
    crs = "EPSG:32632"
  )
  terra::values(r) <- c(10, 100)

  # Polygon covering 0.9 of cell 1 (x: 0.1 to 1.0 = 0.9) and 0.1 of cell 2 (x: 1.0 to 1.1 = 0.1)
  # Total covered area = 1.0
  # Weighted mean = (0.9 * 10 + 0.1 * 100) / 1.0 = 9 + 10 = 19
  # Naive unweighted mean would be (10 + 100) / 2 = 55
  poly <- sf::st_sf(
    id = "partial",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(0.1, 0), c(1.1, 0), c(1.1, 1), c(0.1, 1), c(0.1, 0)
      ))),
      crs = 32632L
    )
  )

  res <- spat_raster(poly, r, stats = "mean")
  expect_equal(res$mean[1], 19.0, tolerance = 1e-3)
})

test_that("spat_raster preserves valid zeros", {
  grid <- make_test_grid()
  r <- make_test_raster()

  result <- spat_raster(grid, r, stats = "min")

  # Row 3 of raster has all zeros — min should be 0 for cells covering that row
  expect_true(any(result$min == 0))
})

test_that("spat_raster returns NA for all-NoData cells", {
  r_na <- terra::rast(
    xmin = 0, xmax = 2,
    ymin = 0, ymax = 2,
    nrows = 2L, ncols = 2L,
    crs = "EPSG:32632"
  )
  terra::values(r_na) <- rep(NA_real_, 4)

  grid <- make_test_grid()
  result <- spat_raster(grid, r_na, stats = "mean")

  expect_true(all(is.na(result$mean)))
})

test_that("spat_raster supports multiple stats", {
  grid <- make_test_grid()
  r <- make_test_raster()

  result <- spat_raster(grid, r, stats = c("mean", "min", "max"))

  expect_true("mean" %in% names(result))
  expect_true("min" %in% names(result))
  expect_true("max" %in% names(result))
})

test_that("spat_raster uses name prefix for columns", {
  grid <- make_test_grid()
  r <- make_test_raster()

  result <- spat_raster(grid, r, stats = c("mean", "max"), name = "elev")

  expect_true("elev_mean" %in% names(result))
  expect_true("elev_max" %in% names(result))
})

test_that("spat_raster handles file path input", {
  grid <- make_test_grid()
  r <- make_test_raster()

  tmp <- tempfile(fileext = ".tif")
  terra::writeRaster(r, tmp, overwrite = TRUE)

  result <- spat_raster(grid, tmp, stats = "mean")
  expect_true("mean" %in% names(result))
  expect_true(all(!is.na(result$mean)))

  unlink(tmp)
})

test_that("spat_raster errors on missing file", {
  grid <- make_test_grid()
  expect_error(
    spat_raster(grid, "/nonexistent/raster.tif"),
    "not found"
  )
})

test_that("spat_raster errors on invalid stat", {
  grid <- make_test_grid()
  r <- make_test_raster()
  expect_error(
    spat_raster(grid, r, stats = "variance"),
    "Unknown raster statistic"
  )
})

test_that("spat_raster errors on non-SpatRaster input", {
  grid <- make_test_grid()
  expect_error(
    spat_raster(grid, matrix(1:4, 2, 2)),
    "must be a file path or terra::SpatRaster"
  )
})

test_that("spat_raster preserves rows, columns, and geometry", {
  grid <- make_test_grid()
  r <- make_test_raster()
  n <- nrow(grid)
  orig_geom <- sf::st_geometry(grid)

  result <- spat_raster(grid, r, stats = "mean")

  expect_equal(nrow(result), n)
  expect_identical(result$name, grid$name)
  expect_identical(sf::st_geometry(result), orig_geom)
})

test_that("spat_raster errors on column collision", {
  grid <- make_test_grid()
  r <- make_test_raster()
  r1 <- spat_raster(grid, r, stats = "mean")
  expect_error(spat_raster(r1, r, stats = "mean"), "already exists")
})

test_that("spat_raster handles CRS mismatch between polygons and raster", {
  geo <- make_geo_polygon()
  r <- terra::rast(
    xmin = 430000, xmax = 510000,
    ymin = 5310000, ymax = 5430000,
    nrows = 4L, ncols = 4L,
    crs = "EPSG:32632"
  )
  terra::values(r) <- 1:16

  result <- spat_raster(geo, r, stats = "mean")
  expect_true("mean" %in% names(result))
})

test_that("spat_raster attaches diagnostics when requested", {
  grid <- make_test_grid()
  r <- make_test_raster()
  result <- spat_raster(grid, r, stats = c("mean", "max"), diagnostics = TRUE)
  diag <- attr(result, "spat_diagnostics")
  expect_equal(diag$operation, "spat_raster")
  expect_equal(diag$stats, c("mean", "max"))
})
