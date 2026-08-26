test_that("spat_overlap computes correct overlap area", {
  grid <- make_test_grid()
  src <- make_test_source_polys()

  result <- spat_overlap(grid, src, measure = "area", unit = "m2")

  # Source poly 1: 1x1 square from (0.5,0.5) to (1.5,1.5)
  # Overlap with cell_1 (0-1, 0-1): 0.5 * 0.5 = 0.25 m2
  # Overlap with cell_2 (1-2, 0-1): 0.5 * 0.5 = 0.25 m2
  # Overlap with cell_3 (0-1, 1-2): 0.5 * 0.5 = 0.25 m2
  # Overlap with cell_4 (1-2, 1-2): 0.5 * 0.5 = 0.25 m2
  # Source poly 2: from (1.25,0.25) to (2.5,2.5)
  # Overlap with cell_2 (1-2, 0-1): 0.75 * 0.75 = 0.5625 m2
  # Overlap with cell_4 (1-2, 1-2): 0.75 * 1.0 = 0.75 m2

  # cell_1 total: 0.25 (from src1 only)
  expect_equal(result$overlap_m2[1], 0.25, tolerance = 0.01)
  # cell_2 total: 0.25 + 0.5625 = 0.8125
  expect_equal(result$overlap_m2[2], 0.8125, tolerance = 0.01)
})

test_that("spat_overlap dynamically generates column names based on measure and unit", {
  grid <- make_test_grid()
  src <- make_test_source_polys()

  r_m2  <- spat_overlap(grid, src, measure = "area", unit = "m2")
  r_km2 <- spat_overlap(grid, src, measure = "area", unit = "km2")
  r_ha  <- spat_overlap(grid, src, measure = "area", unit = "ha")
  r_mi2 <- spat_overlap(grid, src, measure = "area", unit = "mi2")
  r_sh  <- suppressWarnings(spat_overlap(grid, src, measure = "share"))
  r_cnt <- spat_overlap(grid, src, measure = "count")

  expect_true("overlap_m2" %in% names(r_m2))
  expect_true("overlap_km2" %in% names(r_km2))
  expect_true("overlap_ha" %in% names(r_ha))
  expect_true("overlap_mi2" %in% names(r_mi2))
  expect_true("overlap_share" %in% names(r_sh))
  expect_true("overlap_count" %in% names(r_cnt))
})

test_that("spat_overlap allows explicit name override", {
  grid <- make_test_grid()
  src <- make_test_source_polys()

  res <- spat_overlap(grid, src, measure = "area", unit = "m2", name = "custom_ovl")
  expect_true("custom_ovl" %in% names(res))
  expect_false("overlap_m2" %in% names(res))
})

test_that("spat_overlap computes share correctly", {
  grid <- make_test_grid()
  # Source polygon that covers exactly half of cell_1
  half <- sf::st_sf(
    id = "half",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(0, 0), c(0.5, 0), c(0.5, 1), c(0, 1), c(0, 0)
      ))),
      crs = 32632L
    )
  )

  result <- spat_overlap(grid, half, measure = "share")
  expect_equal(result$overlap_share[1], 0.5, tolerance = 0.01)
  expect_equal(result$overlap_share[2], 0, tolerance = 0.01)
})

test_that("spat_overlap warning behaves correctly for touching vs overlapping sources", {
  grid <- make_test_grid()

  p1 <- sf::st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
  p_disjoint <- sf::st_polygon(list(rbind(c(2,0), c(3,0), c(3,1), c(2,1), c(2,0))))
  p_edge <- sf::st_polygon(list(rbind(c(1,0), c(2,0), c(2,1), c(1,1), c(1,0))))
  p_point <- sf::st_polygon(list(rbind(c(1,1), c(2,1), c(2,2), c(1,2), c(1,1))))
  p_overlap <- sf::st_polygon(list(rbind(c(0.5,0), c(1.5,0), c(1.5,1), c(0.5,1), c(0.5,0))))
  p_contained <- sf::st_polygon(list(rbind(c(0.2,0.2), c(0.8,0.2), c(0.8,0.8), c(0.2,0.8), c(0.2,0.2))))

  src_disjoint <- sf::st_sf(geometry = sf::st_sfc(p1, p_disjoint, crs = 32632L))
  src_edge     <- sf::st_sf(geometry = sf::st_sfc(p1, p_edge, crs = 32632L))
  src_point    <- sf::st_sf(geometry = sf::st_sfc(p1, p_point, crs = 32632L))
  src_overlap  <- sf::st_sf(geometry = sf::st_sfc(p1, p_overlap, crs = 32632L))
  src_contain  <- sf::st_sf(geometry = sf::st_sfc(p1, p_contained, crs = 32632L))
  src_dup      <- sf::st_sf(geometry = sf::st_sfc(p1, p1, crs = 32632L))

  # 1. Disjoint -> no warning
  expect_no_warning(spat_overlap(grid, src_disjoint, measure = "share"))

  # 2. Edge-touching -> no warning
  expect_no_warning(spat_overlap(grid, src_edge, measure = "share"))

  # 3. Point-touching -> no warning
  expect_no_warning(spat_overlap(grid, src_point, measure = "share"))

  # 4. Partial overlap -> warning
  expect_warning(
    spat_overlap(grid, src_overlap, measure = "share"),
    "positive area"
  )

  # 5. Contained polygon -> warning
  expect_warning(
    spat_overlap(grid, src_contain, measure = "share"),
    "positive area"
  )

  # 6. Duplicate / coextensive -> warning
  expect_warning(
    spat_overlap(grid, src_dup, measure = "share"),
    "positive area"
  )
})

test_that("spat_overlap count measure works", {
  grid <- make_test_grid()
  src <- make_test_source_polys()

  result <- spat_overlap(grid, src, measure = "count")
  # Source poly 1 overlaps all 4 cells, source poly 2 overlaps cells 2,4
  expect_equal(result$overlap_count[1], 1L)
  expect_equal(result$overlap_count[2], 2L)
  expect_equal(result$overlap_count[4], 2L)
})

test_that("spat_overlap returns 0 for non-overlapping polygons", {
  grid <- make_test_grid()
  far <- sf::st_sf(
    id = "far",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(10, 10), c(11, 10), c(11, 11), c(10, 11), c(10, 10)
      ))),
      crs = 32632L
    )
  )

  result <- spat_overlap(grid, far, measure = "area", unit = "m2")
  expect_equal(result$overlap_m2, c(0, 0, 0, 0))
})

test_that("spat_overlap returns 0 for empty source", {
  grid <- make_test_grid()
  empty <- sf::st_sf(
    id = character(0),
    geometry = sf::st_sfc(crs = 32632L)
  )
  class(empty$geometry) <- c("sfc_POLYGON", "sfc")

  result <- spat_overlap(grid, empty, unit = "m2")
  expect_equal(result$overlap_m2, c(0, 0, 0, 0))
})

test_that("spat_overlap errors informatively when CRS is missing", {
  grid <- make_test_grid()
  src <- make_test_source_polys()

  grid_no_crs <- grid
  sf::st_crs(grid_no_crs) <- NA

  src_no_crs <- src
  sf::st_crs(src_no_crs) <- NA

  # Missing target CRS
  expect_error(
    spat_overlap(grid_no_crs, src),
    "`x` has no CRS"
  )

  # Missing source CRS
  expect_error(
    spat_overlap(grid, src_no_crs),
    "`y` has no CRS"
  )

  # Both missing CRS
  expect_error(
    spat_overlap(grid_no_crs, src_no_crs),
    "`x` has no CRS"
  )
})

test_that("spat_overlap preserves rows, columns, and geometry", {
  grid <- make_test_grid()
  src <- make_test_source_polys()
  n <- nrow(grid)
  orig_geom <- sf::st_geometry(grid)

  result <- spat_overlap(grid, src)

  expect_equal(nrow(result), n)
  expect_identical(result$name, grid$name)
  expect_identical(sf::st_geometry(result), orig_geom)
})

test_that("spat_overlap errors on column collision", {
  grid <- make_test_grid()
  src <- make_test_source_polys()
  r1 <- spat_overlap(grid, src)
  expect_error(spat_overlap(r1, src), "already exists")
})

test_that("spat_overlap rejects non-polygon source", {
  grid <- make_test_grid()
  pts <- make_test_points()
  expect_error(spat_overlap(grid, pts), "POLYGON or MULTIPOLYGON")
})

test_that("spat_overlap attaches diagnostics when requested", {
  grid <- make_test_grid()
  src <- make_test_source_polys()
  result <- spat_overlap(grid, src, diagnostics = TRUE)
  diag <- attr(result, "spat_diagnostics")
  expect_equal(diag$operation, "spat_overlap")
  expect_equal(diag$measure, "area")
})
