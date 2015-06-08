context("show")

test_that("show", {
  x <- createMassSpectrum(mass=1:10, intensity=11:20,
                          metaData=list(name="example", file="example.mzML"))
  r <- paste0(format(c("S4 class type",
                       "Number of m/z values",
                       "Range of m/z values",
                       "Range of intensity values",
                       "Memory usage",
                       "Name",
                       "File"), justify="left"), ": ",
              c(format(c("MassSpectrum",
                         "10",
                         "1 - 10",
                         "11 - 20",
                         MALDIquant:::.memoryUsageStr(x),
                         "example"), justify="left"),
                "example.mzML"))

  expect_identical(capture.output(x), r)

  x <- createMassPeaks(mass=1:10, intensity=11:20,
                       metaData=list(name="example"))
  r <- paste0(format(c("S4 class type",
                       "Number of m/z values",
                       "Range of m/z values",
                       "Range of intensity values",
                       "Range of snr values",
                       "Memory usage",
                       "Name"), justify="left"), ": ",
              format(c("MassPeaks",
                       "10",
                       "1 - 10",
                       "11 - 20",
                       "NA - NA",
                       MALDIquant:::.memoryUsageStr(x),
                       "example"), justify="left"))

  expect_identical(capture.output(x), r)

  x <- createMassPeaks(mass=1:10, intensity=11:20, snr=1:10,
                       metaData=list(name=c("example1", "example2"),
                                     file=c("example1.txt", "example2.txt")))
  r <- paste0(format(c("S4 class type",
                       "Number of m/z values",
                       "Range of m/z values",
                       "Range of intensity values",
                       "Range of snr values",
                       "Memory usage",
                       "Name1",
                       "Name2",
                       "File1",
                       "File2"), justify="left"), ": ",
              c(format(c("MassPeaks",
                         "10",
                         "1 - 10",
                         "11 - 20",
                         "1 - 10",
                         MALDIquant:::.memoryUsageStr(x),
                         "example1",
                         "example2"), justify="left"),
                "example1.txt", "example2.txt"))

  expect_identical(capture.output(x), r)

  x <- createMassPeaks(double(), double())
  r <- paste0(format(c("S4 class type",
                       "Number of m/z values",
                       "Range of m/z values",
                       "Range of intensity values",
                       "Range of snr values",
                       "Memory usage"), justify="left"), ": ",
              format(c("MassPeaks",
                       "0",
                       "NA",
                       "NA",
                       "NA",
                       MALDIquant:::.memoryUsageStr(x)), justify="left"))

  expect_identical(capture.output(x), r)
})
