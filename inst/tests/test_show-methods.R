context("show")

test_that("show", {
  x <- createMassSpectrum(mass=1:10, intensity=11:20,
                          metaData=list(name="example", file="example.mzML"))
  r <- c(
   "S4 class type            : MassSpectrum",
   "Number of m/z values     : 10          ",
   "Range of m/z values      : 1 - 10      ",
   "Range of intensity values: 11 - 20     ",
   "Name                     : example     ",
   "File                     : example.mzML")

  expect_identical(capture.output(show(x)), r)

  x <- createMassPeaks(mass=1:10, intensity=11:20,
                       metaData=list(name="example", file="example.mzML"))

  r <- c(
   "S4 class type            : MassPeaks",
   "Number of m/z values     : 10       ",
   "Range of m/z values      : 1 - 10   ",
   "Range of intensity values: 11 - 20  ",
   "Range of snr values      : NA - NA  ",
   "Name                     : example  ",
   "File                     : example.mzML")

  expect_identical(capture.output(show(x)), r)

  x <- createMassPeaks(mass=1:10, intensity=11:20, snr=1:10,
                       metaData=list(name=c("example1", "example2"),
                                     file=c("example1.txt", "example2.txt")))
  r <- c(
   "S4 class type            : MassPeaks",
   "Number of m/z values     : 10       ",
   "Range of m/z values      : 1 - 10   ",
   "Range of intensity values: 11 - 20  ",
   "Range of snr values      : 1 - 10   ",
   "Name1                    : example1 ",
   "Name2                    : example2 ",
   "File1                    : example1.txt",
   "File2                    : example2.txt")

  expect_identical(capture.output(show(x)), r)
})

