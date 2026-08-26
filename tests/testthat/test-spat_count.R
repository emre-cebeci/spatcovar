test_that("spat_count counts intersecting features correctly", {
  grid <- make_test_grid()
  pts <- make_test_points()  # 2 in cell_1, 1 in cell_2, 1 in cell_3, 1 outside

  result <- spat_count(grid, pts)

  expect_equal(result$count[1], 2L)  # cell_1: pt_1 and pt_2
  expect_equal(result$count[2], 1L)  # cell_2: pt_3
  expect_equal(result$count[3], 1L)  # cell_3: pt_4
  expect_equal(result$count[4], 0L)  # cell_4: none
})

test_that("spat_count errors informatively when CRS is missing", {
  grid <- make_test_grid()
  pts <- make_test_points()

  grid_no_crs <- grid
  sf::st_crs(grid_no_crs) <- NA

  pts_no_crs <- pts
  sf::st_crs(pts_no_crs) <- NA

  # Missing target CRS
  expect_error(
    spat_count(grid_no_crs, pts),
    "`x` has no CRS"
  )

  # Missing source CRS
  expect_error(
    spat_count(grid, pts_no_crs),
    "`y` has no CRS"
  )

  # Both missing CRS
  expect_error(
    spat_count(grid_no_crs, pts_no_crs),
    "`x` has no CRS"
  )
})

test_that("spat_count transforms when source has different known CRS", {
  grid_utm <- make_test_grid(crs = 32632L)
  pts_geo <- sf::st_sf(
    id = "pt_in_c1",
    geometry = sf::st_sfc(sf::st_point(c(0.0000045, 0.0000045)), crs = 4326L)
  )

  # Should transform and succeed without error
  res <- spat_count(grid_utm, pts_geo)
  expect_true(is.integer(res$count))
})

test_that("spat_count returns 0 for empty source", {
  grid <- make_test_grid()
  empty <- sf::st_sf(
    id = character(0),
    geometry = sf::st_sfc(crs = 32632L)
  )

  result <- spat_count(grid, empty)
  expect_equal(result$count, c(0L, 0L, 0L, 0L))
})

test_that("spat_count treats MULTIPOINT as one feature", {
  grid <- make_test_grid()

  # One MULTIPOINT with 3 sub-points in cell_1
  mpt <- sf::st_multipoint(rbind(c(0.2, 0.2), c(0.5, 0.5), c(0.8, 0.8)))
  multi <- sf::st_sf(
    id = "multi",
    geometry = sf::st_sfc(mpt, crs = 32632L)
  )

  result <- spat_count(grid, multi)
  expect_equal(result$count[1], 1L)  # One feature, not three
})

test_that("spat_count counts boundary-touching features", {
  grid <- make_test_grid()
  # Point exactly on boundary between cell_1 and cell_2 at (1, 0.5)
  boundary_pt <- sf::st_sf(
    id = "boundary",
    geometry = sf::st_sfc(sf::st_point(c(1, 0.5)), crs = 32632L)
  )

  result <- spat_count(grid, boundary_pt)
  # Should count for both cell_1 and cell_2 (st_intersects includes boundary)
  expect_true(result$count[1] >= 1L)
  expect_true(result$count[2] >= 1L)
})

test_that("spat_count works with line features", {
  grid <- make_test_grid()
  lines <- make_test_lines()

  result <- spat_count(grid, lines, name = "line_count")
  # Route 1 crosses cells 1,2; Route 2 crosses cells 1,3; Route 3 is outside
  expect_true(result$line_count[1] >= 2L)  # cell_1: route_1 and route_2
})

test_that("spat_count works with polygon features", {
  grid <- make_test_grid()
  src <- make_test_source_polys()

  result <- spat_count(grid, src, name = "poly_count")
  # Source poly 1 overlaps all 4 cells, source poly 2 overlaps cells 2,4
  expect_true(all(result$poly_count >= 1L))
})

test_that("spat_count preserves rows, columns, and geometry", {
  grid <- make_test_grid()
  pts <- make_test_points()
  n <- nrow(grid)
  orig_geom <- sf::st_geometry(grid)

  result <- spat_count(grid, pts)

  expect_equal(nrow(result), n)
  expect_identical(result$name, grid$name)
  expect_identical(sf::st_geometry(result), orig_geom)
})

test_that("spat_count returns integer type", {
  grid <- make_test_grid()
  pts <- make_test_points()
  result <- spat_count(grid, pts)
  expect_type(result$count, "integer")
})

test_that("spat_count errors on column collision", {
  grid <- make_test_grid()
  pts <- make_test_points()
  r1 <- spat_count(grid, pts)
  expect_error(spat_count(r1, pts), "already exists")
})

test_that("spat_count rejects non-sf target", {
  expect_error(
    spat_count(data.frame(a = 1), data.frame(b = 2)),
    "must be an sf"
  )
})

test_that("spat_count attaches diagnostics when requested", {
  grid <- make_test_grid()
  pts <- make_test_points()
  result <- spat_count(grid, pts, diagnostics = TRUE)
  diag <- attr(result, "spat_diagnostics")
  expect_equal(diag$operation, "spat_count")
  expect_equal(diag$n_source, 5L)
})
