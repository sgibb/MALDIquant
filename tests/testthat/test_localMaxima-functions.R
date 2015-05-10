context(".localMaxima")

y <- c(1, 1, 2, 1, 1, 3, 4)
set.seed(1)
b <- rnorm(5e4)

test_that(".localMaxima", {
  expect_identical(MALDIquant:::.localMaxima(y, 1),
                   c(T, F, T, F, F, F, T))
})
