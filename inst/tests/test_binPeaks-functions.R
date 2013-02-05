context("binPeaks")

p <- list(createMassPeaks(mass=seq(100, 500, 100), intensity=1:5),
          createMassPeaks(mass=c(seq(100.2, 300.2, 100), 395), intensity=1:4))

p2 <- c(createMassPeaks(mass=c(1.009, 1.01, 3), intensity=c(2, 1, 1), snr=1:3),
        createMassPeaks(mass=c(1, 3), intensity=1:2, snr=1:2),
        createMassPeaks(mass=c(1.03, 3), intensity=1:2, snr=1:2))

test_that("binPeaks throws errors", {
  expect_error(binPeaks(list()), "no list of MALDIquant::MassPeaks")
  expect_error(binPeaks(p, method="foobar"),
               ".*arg.* should be one of .*strict.*, .*relaxed.*")

})

test_that("binPeaks bins peaks strict", {
  b <- binPeaks(p, tolerance=0.002)
  expect_true(all(b[[1]]@mass[1:3]==b[[2]]@mass[1:3]))
  expect_false(all(b[[1]]@mass[4]==b[[2]]@mass[4]))
  expect_true(length(b[[1]])==5)
  expect_true(length(b[[2]])==4)
  expect_false(all(p[[1]]@mass==b[[1]]@mass))
  expect_false(all(p[[2]]@mass==b[[2]]@mass))

  b <- binPeaks(p, tolerance=0.1)
  expect_true(all(b[[1]]@mass[1:4]==b[[2]]@mass[1:4]))
  expect_true(length(b[[1]])==5)
  expect_true(length(b[[2]])==4)
  expect_false(all(p[[1]]@mass==b[[1]]@mass))
  expect_false(all(p[[2]]@mass==b[[2]]@mass))
})

test_that("binPeaks bins peaks releaxed", {
  b <- binPeaks(p2, method="relaxed", tolerance=0.05)
  expect_true(all(b[[1]]@mass==c(1.01, 1.013, 3)))
  ip <- sort(p2[[1]]@mass, index.return=TRUE)
  ib <- sort(b[[1]]@mass, index.return=TRUE)
  expect_equal(ip$ix, ib$ix)
  expect_false(all(p2[[1]]@intensity == b[[1]]@intensity))
  expect_false(all(p2[[1]]@snr== b[[1]]@snr))
  expect_true(all(b[[1]]@intensity == c(1, 2, 1)))
  expect_true(all(b[[2]]@intensity == 1:2))
  expect_true(all(b[[3]]@intensity == 1:2))
  expect_true(all(b[[1]]@snr == c(2, 1, 3)))
  expect_true(all(b[[2]]@snr == 1:2))
  expect_true(all(b[[3]]@snr == 1:2))
})
