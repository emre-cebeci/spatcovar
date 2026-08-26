# CRAN Submission Comments

## Package: spatcovar 0.1.0

This is a new package submission.

## Test environments

- local: macOS Tahoe 26.5.1 (aarch64-apple-darwin23), R 4.6.1 (2026-06-24)
- GitHub Actions:
  - Ubuntu 24.04 (x86_64-pc-linux-gnu): R-release, R-devel, R-oldrel-1
  - macOS 14 (aarch64-apple-darwin23): R-release
  - Windows Server 2022 (x86_64-w64-mingw32): R-release

## R CMD check results

- 0 errors | 0 warnings | 1 note

### Notes

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Emre Cebeci <cebeciemre1@gmail.com>'

New submission
```

This is expected for an initial submission.

## Additional validation

- Unit tests: 250 tests passed, 0 failures, 0 skipped.
- Test coverage: 92.48% (via `covr::package_coverage()`).
- All functions include runnable examples with synthetic fixtures.
- Vignette builds cleanly.
