sparseMatrixNA <- function(i, j, x, dims, dimnames,
                           keep.zeros = TRUE) {
  if (keep.zeros) {
    x[x == 0] <- .Machine$double.xmin
  }
  M <- sparseMatrix(i=i, j=j, x=x, dims = dims, dimnames = dimnames)

  return(M)
}


as.sparseMatrixNA <- function(x, keep.zeros = TRUE) {
  if (keep.zeros) {
    x[x == 0] <- .Machine$double.xmin
  }
  x[is.na(x)] <- 0
  x <- as(x, 'sparseMatrix')
  
  return(x)
}