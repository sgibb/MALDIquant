context("calibrateIntensity")

s <- list(createMassSpectrum(mass=1:5, intensity=1:5),
          createMassSpectrum(mass=1:5, intensity=2:6),
          createMassSpectrum(mass=1:5, intensity=3:7))

test_that("calibrateIntensity,MassSpectrum throws errors", {
  expect_error(calibrateIntensity(s[[1]], method="foobar"),
               ".*arg.* should be one of .*TIC.*, .*PQN.*, .*median.*")
  expect_error(calibrateIntensity(s[[1]], method="PQN"),
               ".*PQN.* is not supported for a single MassSpectrum object")
})

test_that("calibrateIntensity,list throws errors", {
  expect_error(calibrateIntensity(list(1:10), method="TIC"),
               "no list of MALDIquant::MassSpectrum objects")
  expect_error(calibrateIntensity(s, method="foobar"),
               ".*arg.* should be one of .*TIC.*, .*PQN.*, .*median.*")
})

test_that("calibrateIntensity,MassSpectrum throws warnings", {
  # median == zero; see #51
  m <- createMassSpectrum(mass=1:5, intensity=c(rep(0, 4), 5))
  expect_warning(calibrateIntensity(m, method="median"),
               "Scaling factor is zero. No calibration applied.")
})

test_that("calibrateIntensity works with TIC", {
  sTIC <- calibrateIntensity(s[[1]], method="TIC")
  expect_equal(totalIonCurrent(sTIC), 1)
  sTIC <- calibrateIntensity(s, method="TIC")
  expect_equal(unlist(lapply(sTIC, totalIonCurrent)), rep(1, 3))
  sTIC <- calibrateIntensity(s, method="TIC", range=c(2, 4))
  expect_equal(unlist(lapply(sTIC, totalIonCurrent)), rep(2, 3))
})

test_that("calibrateIntensity works with median", {
  sMed <- calibrateIntensity(s[[1]], method="median")
  expect_equal(intensity(sMed), intensity(s[[1]])/median(intensity(s[[1]])))
  sMed <- calibrateIntensity(s, method="median")
  expect_equal(lapply(sMed, intensity),
               lapply(s, function(x)intensity(x)/median(intensity(x))))
  sMed <- calibrateIntensity(s, method="median", range=c(2, 4))
  expect_equal(unlist(lapply(sMed, intensity)),
               as.vector(mapply(function(sp, m)intensity(sp)/m, sp=s, m=3:5)))
  # median == zero; see #51
  m <- createMassSpectrum(mass=1:5, intensity=c(rep(0, 4), 5))
  expect_equal(suppressWarnings(calibrateIntensity(m, method="median")), m)
})

test_that("calibrateIntensity works with PQN", {
  sPQN <- calibrateIntensity(s, method="PQN")
  expect_equal(sPQN, calibrateIntensity(s, method="TIC"))
  m <- list(s[[1]], createMassSpectrum(1:5, rep(2, 5)),
            createMassSpectrum(1:5, c(4:6, 2:1)))
  mPQN <- calibrateIntensity(m, method="PQN")
  expect_equal(unlist(lapply(mPQN, totalIonCurrent)),
               c(1, 1, 0.96875))
  sPQN <- calibrateIntensity(s, method="PQN", range=c(2, 4))
  expect_equal(sPQN, calibrateIntensity(s, method="TIC", range=c(2, 4)))
})
