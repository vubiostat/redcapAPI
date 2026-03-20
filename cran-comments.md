## R CMD check results

Duration: 58.6s

❯ checking CRAN incoming feasibility ... [4s/20s] NOTE
  Maintainer: ‘Shawn Garbett <shawn.garbett@vumc.org>’

0 errors ✔ | 0 warnings ✔ | 1 note ✖

## revdepcheck results

We checked 3 reverse dependencies (2 from CRAN + 1 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

## rccola

revdepcheck is still showing that the package `rccola` is present on CRAN. An earlier email had we asked that this package be delisted. It has been replaced by `shelter`. The strategy employed to manage keys in `rccola` has been found to NOT be good security practice and it should be treated as a security risk. We recommend once again that `rccola` be delisted.
