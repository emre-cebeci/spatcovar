# spatcovar

<!-- badges: start -->
[![R-CMD-check](https://github.com/emre-cebeci/spatcovar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/emre-cebeci/spatcovar/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`spatcovar` provides a consistent interface for constructing commonly used
spatial covariates from polygon data. It computes polygon areas, distances to
reference features, intersection counts, line lengths within polygons, polygon
overlap areas and shares, and raster zonal summaries while handling coordinate
reference system validation, geometry repair, unit conversion, row
preservation, and standardised missing-value semantics.

`spatcovar` builds on established spatial engines
([sf](https://r-spatial.github.io/sf/),
[terra](https://rspatial.github.io/terra/),
[exactextractr](https://isciences.gitlab.io/exactextractr/))
rather than reimplementing the underlying geometry operations.

## Installation

Install the development version from GitHub:

```r
# install.packages("pak")
pak::pak("emre-cebeci/spatcovar")
```

## Quick start

```r
library(spatcovar)

regions <- example_polygons()
sites   <- example_points()
routes  <- example_lines()
zones   <- example_grid()
rst     <- example_raster()

# Build several covariates in a pipeline
result <- regions |>
  spat_area(unit = "km2") |>
  spat_count(sites, name = "n_sites") |>
  spat_length(routes, unit = "km") |>
  spat_distance(sites, name = "dist_to_sites") |>
  spat_overlap(zones, measure = "share") |>
  spat_raster(rst, stats = c("mean", "max"), name = "elev")

result
```

## Core functions

| Function | Description |
|---|---|
| `spat_area()` | Polygon area with unit conversion |
| `spat_distance()` | Distance to reference features (minimum, centroid, or point-on-surface) |
| `spat_count()` | Count of source features intersecting each polygon |
| `spat_length()` | Total length of line features within each polygon |
| `spat_overlap()` | Overlap area, target-area share, or intersecting-feature count |
| `spat_raster()` | Zonal raster statistics (mean, median, min, max, sum, count, stdev) |
| `spat_diagnostics()` | Retrieve operation diagnostics from a result |

## CRS handling

- **Area, distance, and overlap** (`spat_area()`, `spat_distance()`,
  `spat_overlap()`) use geodesic measurement on geographic CRS (via sf/s2).
  An optional `crs` argument is provided when user-selected planar projection
  is desired.
- **Line length** (`spat_length()`) strictly requires a projected CRS for
  planar clipping and measurement (either on the input data or supplied via
  the `crs` argument).
- **Feature counting and raster extraction** (`spat_count()`, `spat_raster()`)
  validate coordinate reference systems and align source layers internally.

## Geometry repair

When `repair = TRUE` (the default), invalid geometries are repaired on
internal copies used for computation. The geometry returned to the user is
always the original target geometry.

## What `spatcovar` does not do

`spatcovar` is a focused package for constructing common spatial covariates
from polygon data. It does not provide general GIS manipulation, mapping,
geocoding, boundary harmonization, persistent spatial identifiers, areal
interpolation methodology, raster processing pipelines, or causal inference
tools.

## License

MIT
