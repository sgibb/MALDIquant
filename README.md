# MALDIquant
[![build status](https://travis-ci.org/sgibb/MALDIquant.svg?branch=master)](https://travis-ci.org/sgibb/MALDIquant)
[![codecov.io](https://img.shields.io/codecov/c/github/sgibb/MALDIquant.svg?branch=master)](https://codecov.io/github/sgibb/MALDIquant/?branch=master)
[![license](http://img.shields.io/badge/license-GPL%20%28%3E=%203%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html)
[![metacran version](http://www.r-pkg.org/badges/version/MALDIquant)](http://cran.r-project.org/web/packages/MALDIquant/index.html)
[![metacran downloads](http://cranlogs.r-pkg.org/badges/MALDIquant?color=brightgreen)](http://cran.r-project.org/web/packages/MALDIquant/index.html)
[CRAN check result](http://cran.r-project.org/web/checks/check_results_MALDIquant.html)

Quantitative Analysis of Mass Spectrometry Data


## Description

MALDIquant provides a complete analysis pipeline for MALDI-TOF and other mass
spectrometry data. Distinctive features include baseline subtraction methods
such as TopHat or SNIP, peak alignment using warping functions,
handling of replicated measurements as well as allowing spectra with
different resolutions.

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
[drat](http://dirk.eddelbuettel.com/code/drat.html) package
to install the development version of MALDIquant
(you will need a complete development environment to compile the some c code).

```r
# first install drat from CRAN if you haven't already done it:
install.packages("drat")

# add our repository
# (add this line to your `~/.Rprofile` to add it for future use,
#  i.e. if you want to get updates via `update.packages()` ):
drat::addRepo("sgibb")

# now use `install.packages`
install.packages("MALDIquant")
```

## Examples
To illustrate the application of MALDIquant for analyzis of mass spectrometry
data please find a number of example R scripts in the
[MALDIquantExamples](https://github.com/sgibb/MALDIquantExamples) repository.

