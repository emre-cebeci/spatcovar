#' Retrieve diagnostics from a spatcovar result
#'
#' Extracts the diagnostics summary attached by a `spat_*()` function when
#' called with `diagnostics = TRUE`.
#'
#' @section Diagnostic attributes:
#' Diagnostics represent the most recent `spatcovar` operation performed on
#' the object.
#' Intermediate transformations (subsetting, column manipulation, joins) may
#' drop the attribute; this is expected behaviour and not an error.
#'
#' @param x An `sf` object returned by a `spat_*()` function with
#'   `diagnostics = TRUE`.
#'
#' @return A named list with operation details, or `NULL` if no diagnostics
#'   are attached.
#'   When printed, produces a human-readable summary.
#'
#' @export
#'
#' @examples
#' regions <- example_polygons()
#' result <- spat_area(regions, diagnostics = TRUE)
#' spat_diagnostics(result)
spat_diagnostics <- function(x) {
  diag <- attr(x, "spat_diagnostics")

  if (is.null(diag)) {
    message("No spatcovar diagnostics found on this object.")
    return(invisible(NULL))
  }

  # Pretty print
  message("--- spatcovar diagnostics ---")
  message("Operation: ", diag$operation)

  if (!is.null(diag$n_target)) {
    message("Target polygons: ", diag$n_target)
  }
  if (!is.null(diag$n_source)) {
    message("Source features: ", diag$n_source)
  }
  if (!is.null(diag$n_repaired_target) && diag$n_repaired_target > 0L) {
    message("Target geometries repaired: ", diag$n_repaired_target)
  }
  if (!is.null(diag$n_repaired_source) && diag$n_repaired_source > 0L) {
    message("Source geometries repaired: ", diag$n_repaired_source)
  }
  if (!is.null(diag$n_zero)) {
    message("Zero-value results: ", diag$n_zero)
  }
  if (!is.null(diag$n_na)) {
    if (is.numeric(diag$n_na) && length(diag$n_na) == 1L) {
      message("NA results: ", diag$n_na)
    } else if (is.numeric(diag$n_na)) {
      for (nm in names(diag$n_na)) {
        message("NA results (", nm, "): ", diag$n_na[[nm]])
      }
    }
  }
  if (!is.null(diag$method)) {
    message("Method: ", diag$method)
  }
  if (!is.null(diag$measure)) {
    message("Measure: ", diag$measure)
  }
  if (!is.null(diag$crs_used)) {
    message("CRS used: ", diag$crs_used)
  }
  if (!is.null(diag$unit) && !is.na(diag$unit)) {
    message("Unit: ", diag$unit)
  }
  if (!is.null(diag$stats)) {
    message("Stats: ", paste(diag$stats, collapse = ", "))
  }

  invisible(diag)
}
