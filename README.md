# MALDIquant

<!-- badges: start -->
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/sgibb/MALDIquant/workflows/R-CMD-check/badge.svg)](https://github.com/sgibb/MALDIquant/actions)
[![codecov.io](https://codecov.io/github/sgibb/MALDIquant/coverage.svg?branch=master)](https://codecov.io/github/sgibb/MALDIquant?branch=master)
[![license](https://img.shields.io/badge/license-GPL%20%28%3E=%203%29-brightgreen.svg?style=flat)](https://www.gnu.org/licenses/gpl-3.0.html)
[![cran checks](https://cranchecks.info/badges/worst/MALDIquant)](https://cran.r-project.org/web/checks/check_results_MALDIquant.html)
[![metacran version](https://www.r-pkg.org/badges/version/MALDIquant)](https://cran.r-project.org/web/packages/MALDIquant/index.html)
[![metacran downloads](https://cranlogs.r-pkg.org/badges/MALDIquant?color=brightgreen)](https://cran.r-project.org/web/packages/MALDIquant/index.html)
  <!-- badges: end -->

Quantitative Analysis of Mass Spectrometry Data


## Description

MALDIquant provides a complete analysis pipeline for
matrix-assisted laser desorption/ionization-time-of-flight (MALDI-TOF)
and other two-dimensional mass spectrometry data.

In addition to commonly used plotting and processing methods it
includes distinctive features, namely baseline
subtraction methods such as morphological filters (TopHat) or the
statistics-sensitive non-linear iterative peak-clipping algorithm
(SNIP), peak alignment using warping functions, handling of replicated
measurements as well as allowing spectra with different resolutions.

Please visit: https://www.strimmerlab.org/software/maldiquant/


## Contact

You are welcome to:

* submit suggestions and bug-reports at: <https://github.com/sgibb/MALDIquant/issues>
* send a pull request on: <https://github.com/sgibb/MALDIquant/>
* compose an e-mail to: <mail@sebastiangibb.de>


## Install

You can install the latest release directly from
[CRAN](https://cran.r-project.org/web/packages/MALDIquant/index.html).

```r
install.packages("MALDIquant")
```

## Install development version (not recommended)

[GitHub](https://github.com) is not directly supported by the basic
`install.packages` command. You could use the
[devtools](https://cran.r-project.org/web/packages/devtools/index.html) package
to install the development version of MALDIquant
(you will need a complete development environment to compile the some c code).

```r
install.packages("devtools")
library("devtools")
install_github("sgibb/MALDIquant")
```

## Examples
To illustrate the application of MALDIquant for analyses of mass spectrometry
data please find a number of example R scripts in the
[MALDIquantExamples](https://github.com/sgibb/MALDIquantExamples) repository.
