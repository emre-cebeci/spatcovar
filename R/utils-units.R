# Internal unit conversion helpers for spatcovar
# These are NOT exported.

# Convert area values from square metres to the requested unit
convert_area_unit <- function(area_m2, unit) {
  switch(unit,
    "m2"  = area_m2,
    "km2" = area_m2 / 1e6,
    "ha"  = area_m2 / 1e4,
    "mi2" = area_m2 / 2589988.110336,
    stop("Unknown area unit: ", unit, call. = FALSE)
  )
}

# Convert length values from metres to the requested unit
convert_length_unit <- function(length_m, unit) {
  switch(unit,
    "m"  = length_m,
    "km" = length_m / 1000,
    "mi" = length_m / 1609.344,
    stop("Unknown length unit: ", unit, call. = FALSE)
  )
}

# Allowed area units
area_units <- function() c("m2", "km2", "ha", "mi2")

# Allowed length/distance units
length_units <- function() c("m", "km", "mi")
