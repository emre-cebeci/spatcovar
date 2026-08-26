#!/usr/bin/env Rscript

# Privacy & Hygiene Audit for spatcovar
# Scans all repository files and built source tarballs to ensure
# zero private filesystem paths, credentials/tokens, or forbidden data artifacts exist.

cat("=== Starting spatcovar Privacy & Hygiene Audit ===\n\n")

# Generic public-safe patterns for private paths and secrets
forbidden_patterns <- c(
  "/Users/[^/]+/",
  "/home/[^/]+/",
  "[A-Za-z]:\\\\Users\\\\[^\\\\]+\\\\",
  "Mobile Documents/com~apple~CloudDocs",
  "github_pat_[A-Za-z0-9_]+",
  "ghp_[A-Za-z0-9]{20,}",
  "gho_[A-Za-z0-9]{20,}",
  "glpat-[A-Za-z0-9_-]{20,}",
  "sk-[A-Za-z0-9]{16,}",
  "BEGIN (RSA|OPENSSH|EC) PRIVATE KEY"
)

# 1. Audit tracked and local files in repo
cat("[1/3] Scanning repository files for sensitive paths and secrets...\n")

files_to_check <- list.files(
  path = ".",
  recursive = TRUE,
  all.files = TRUE,
  full.names = TRUE
)

# Exclude .git, dev/privacy_audit.R, workflow yaml, build artifacts
files_to_check <- files_to_check[
  !grepl("^\\./\\.git/", files_to_check) &
  !grepl("dev/privacy_audit\\.R$", files_to_check) &
  !grepl("\\.github/workflows/privacy-audit\\.yaml$", files_to_check) &
  !grepl("\\.Rcheck/", files_to_check) &
  !grepl("\\.tar\\.gz$", files_to_check)
]

found_issues <- 0L

for (f in files_to_check) {
  if (file.info(f)$isdir) next

  tryCatch({
    lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
    for (pat in forbidden_patterns) {
      matches <- grep(pat, lines, perl = TRUE, ignore.case = FALSE)
      if (length(matches) > 0L) {
        cat(sprintf("  FAIL: Found '%s' in %s (lines: %s)\n",
                    pat, f, paste(matches, collapse = ", ")))
        found_issues <- found_issues + 1L
      }
    }
  }, error = function(e) {
    # Binary file or unreadable
  })
}

if (found_issues == 0L) {
  cat("  PASS: No private paths or secrets found in repository files.\n\n")
} else {
  cat(sprintf("  FAIL: %d sensitive token occurrences found!\n\n", found_issues))
}

# 2. Check for forbidden file extensions
cat("[2/3] Checking for forbidden data and swap files...\n")

forbidden_exts <- "\\.(gpkg|shp|shx|dbf|prj|cpg|xlsx|xls|csv|tsv|parquet|feather|sqlite|rds|rdata|rda|swp|swo|swx)$"
forbidden_files <- files_to_check[grepl(forbidden_exts, files_to_check, ignore.case = TRUE)]

if (length(forbidden_files) == 0L) {
  cat("  PASS: No forbidden data or swap files present.\n\n")
} else {
  cat("  FAIL: Forbidden data files detected:\n")
  for (ff in forbidden_files) {
    cat("    -", ff, "\n")
  }
  found_issues <- found_issues + length(forbidden_files)
  cat("\n")
}

# 3. Audit built source tarball if present
cat("[3/3] Auditing built source tarball contents...\n")

tarballs <- list.files(".", pattern = "^spatcovar_.*\\.tar\\.gz$", full.names = TRUE)

if (length(tarballs) > 0L) {
  for (tb in tarballs) {
    cat("  Inspecting archive:", tb, "\n")
    archive_contents <- utils::untar(tb, list = TRUE)

    # Check for forbidden files inside tarball
    bad_inside <- archive_contents[grepl(forbidden_exts, archive_contents, ignore.case = TRUE)]
    # Allow vignette.rds if created during build
    bad_inside <- bad_inside[!grepl("build/vignette\\.rds$", bad_inside)]

    if (length(bad_inside) > 0L) {
      cat("  FAIL: Forbidden files inside archive:\n")
      for (bi in bad_inside) cat("    -", bi, "\n")
      found_issues <- found_issues + length(bad_inside)
    } else {
      cat("  PASS: Archive contains only clean, expected files.\n")
    }
  }
} else {
  cat("  INFO: No built tarball found yet. Run after R CMD build.\n")
}

cat("\n=== Audit Summary ===\n")
if (found_issues == 0L) {
  cat("PRIVACY AUDIT PASSED: Repository is clean and safe.\n")
} else {
  stop(sprintf("PRIVACY AUDIT FAILED with %d issues.", found_issues), call. = FALSE)
}
