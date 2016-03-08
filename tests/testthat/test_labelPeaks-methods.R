context("labelPeaks")

test_that("labelPeaks", {
  pdf(NULL)
  on.exit(dev.off())
  plot.new()

  p <- createMassPeaks(1:5, 1:5)
  expect_error(labelPeaks(p, index=-1),
               "No valid .*index.* nor .*mass.* given.")
  expect_error(labelPeaks(p, index=1:10),
               "No valid .*index.* nor .*mass.* given.")
  expect_error(labelPeaks(p, index=1, labels=LETTERS[1:5]),
               "Lenghts of .*index.*/.*mass.* and .*labels.* have to be equal")
  expect_error(labelPeaks(p, srt=45, avoidOverlap=TRUE),
               ".*avoidOverlap = TRUE.* and .*srt != x \\* 90.* is not supported.")
})
