# MALDIquant

[MALDIquant on CRAN](http://cran.r-project.org/web/packages/MALDIquant/)


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

```s
install.packages("MALDIquant")
```

## Install development version (not recommended)

```s
install.packages("devtools")
library("devtools")
install_github("MALDIquant", "sgibb")
```

## Examples
To illustrate the application of MALDIquant for analyzis of mass spectrometry
data please find a number of example R scripts in the
[MALDIquantExamples](https://github.com/sgibb/MALDIquantExamples) repository.


## Build status

[![Build Status](https://travis-ci.org/sgibb/MALDIquant.png?branch=master)](https://travis-ci.org/sgibb/MALDIquant)
