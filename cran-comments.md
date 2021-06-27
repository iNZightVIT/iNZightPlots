## Test environments
* local ubuntu 20.04, R 4.0.5
* ubuntu 20.04 (GitHub Actions) R release, R devel
* macos (GitHub Actions), R release
* win-builder, R release and devel

****
## R CMD check results

0 errors | 0 warnings | 0 notes

I have fixed problems shown on https://cran.r-project.org/web/checks/check_results_iNZightPlots.html
* tests fail gracefully when required files are unavailable
* some tests are skipped if any of the required packages are missing

## Downstream dependencies

I have run R CMD CHECK on the 2 downstream dependencies of iNZightPlots, and both passed.
