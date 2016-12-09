## .irregularScore
##  calculate frequency of irregular data
##
## params:
##  x: double
##
## returns:
##  double, frequency of irregular data
##
.irregularScore <- function(x) {
  d <- diff(x)
  d <- d[-1L] < head(d, -1L)

  mean(d)
}
