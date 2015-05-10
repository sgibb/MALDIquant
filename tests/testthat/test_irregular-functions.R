context(".irregular")

test_that(".irregularScore", {
  expect_equal(MALDIquant:::.irregularScore(1:10), 0)
  expect_equal(MALDIquant:::.irregularScore((1:10)[-8]), 1/7)
})
