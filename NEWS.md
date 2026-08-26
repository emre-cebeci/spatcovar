# spatcovar 0.1.0

Initial release of `spatcovar`, an R package providing a consistent, pipeable
interface for constructing commonly used spatial covariates directly on `sf`
polygon data frames.

## Core Functions

- `spat_area()`: Calculates polygon geometric areas with conversion across
  metric and customary area units (`"m2"`, `"km2"`, `"ha"`, `"mi2"`).
  Uses geodesic measurement on geographic coordinate reference systems (via
  `sf`/`s2`) and planar measurement on projected systems.
- `spat_distance()`: Computes shortest geometry-to-geometry (`"minimum"`),
  centroid-to-feature (`"centroid"`), or interior point-on-surface
  (`"point_on_surface"`) distances from target polygons to reference features.
  Employs indexed nearest-feature search to avoid computing full pairwise
  distance matrices. Supports output in `"m"`, `"km"`, and `"mi"`.
- `spat_count()`: Counts source points, lines, or polygons intersecting each
  target polygon using explicit topological intersection semantics (including
  boundary-touching features). Multi-geometry features count as single source
  features.
- `spat_length()`: Clips line features to polygon boundaries and measures total
  linework length per polygon. Explicitly handles native linear units of the
  projected coordinate reference system before converting to the requested unit
  (`"m"`, `"km"`, `"mi"`).
- `spat_overlap()`: Evaluates spatial overlap between target and source polygon
  layers, returning total overlap area (`measure = "area"`), coverage fraction
  relative to target area (`measure = "share"`), or intersecting polygon counts
  (`measure = "count"`). Issues an informative warning when source polygons have
  positive-area duplicate coverage among themselves.
- `spat_raster()`: Computes coverage-fraction-weighted zonal summary statistics
  (`"mean"`, `"median"`, `"min"`, `"max"`, `"sum"`, `"count"`, `"stdev"`) from
  single-layer raster grids using the `exactextractr` engine. Preserves valid
  zero values and returns `NA` across all requested statistics when a polygon
  has zero valid non-NA raster cell support.
- `spat_diagnostics()`: Retrieves structured operation metadata attached when
  `diagnostics = TRUE` is passed to any core function.

## Synthetic Example Datasets

- Provides five lightweight, self-contained synthetic fixtures in EPSG:32632
  for examples, testing, and reproducible workflows without external file
  dependencies: `example_polygons()`, `example_points()`, `example_lines()`,
  `example_grid()`, and `example_raster()`.

## Design Guarantees & Semantics

- **Row and Geometry Preservation**: Output data frames retain the exact row
  count, row order, and original geometry of the input target polygons.
- **Safe Geometry Repair**: Invalid geometries are repaired on internal copies
  used for computation (`repair = TRUE` by default); the geometry column
  returned to the user is never altered.
- **Coordinate Reference System Policy**: Enforces strict validation requiring
  known coordinate reference systems on all vector and raster inputs.
  `spat_length()` strictly requires a projected (planar) CRS.
- **Dynamic Output Column Naming**: Derives deterministic, unit-aware column
  names by default (e.g. `area_km2`, `dist_m`, `length_km`, `overlap_share`),
  with user override via the `name` argument.
- **Collision Protection**: Prevents accidental overwriting of existing data
  frame columns unless explicitly allowed with `overwrite = TRUE`.
