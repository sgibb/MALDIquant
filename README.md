# MALDIquant
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![build status](https://travis-ci.org/sgibb/MALDIquant.svg?branch=master)](https://travis-ci.org/sgibb/MALDIquant)
[![codecov.io](http://codecov.io/github/sgibb/MALDIquant/coverage.svg?branch=master)](http://codecov.io/github/sgibb/MALDIquant?branch=master)
[![license](http://img.shields.io/badge/license-GPL%20%28%3E=%203%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html)
[![metacran version](http://www.r-pkg.org/badges/version/MALDIquant)](http://cran.r-project.org/web/packages/MALDIquant/index.html)
[![metacran downloads](http://cranlogs.r-pkg.org/badges/MALDIquant?color=brightgreen)](http://cran.r-project.org/web/packages/MALDIquant/index.html)
[![cran checks](https://cranchecks.info/badges/worst/MALDIquant)](https://cran.r-project.org/web/checks/check_results_MALDIquant.html)

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

Please visit: http://strimmerlab.org/software/maldiquant/


## Contact

You are welcome to:

* submit suggestions and bug-reports at: <https://github.com/sgibb/MALDIquant/issues>
* send a pull request on: <https://github.com/sgibb/MALDIquant/>
* compose an e-mail to: <mail@sebastiangibb.de>


## Install

You can install the latest release directly from
[CRAN](http://cran.r-project.org/web/packages/MALDIquant/index.html).

```r
install.packages("MALDIquant")
```

## Install development version (not recommended)

[GitHub](https://github.com) is not directly supported by the basic
`install.packages` command. You could use the
[devtools](http://cran.r-project.org/web/packages/devtools/index.html) package
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
