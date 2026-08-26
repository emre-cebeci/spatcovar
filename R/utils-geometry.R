# Internal geometry helpers for spatcovar
# These are NOT exported.

# Repair invalid geometries on a COPY of the sf object
# Returns a list with the repaired sf and the count of repaired geometries
repair_geometries <- function(x, label = "target") {
  validity <- sf::st_is_valid(x)
  n_invalid <- sum(!validity, na.rm = TRUE)

  if (n_invalid > 0L) {
    x <- sf::st_make_valid(x)
  }

  list(data = x, n_repaired = n_invalid)
}
