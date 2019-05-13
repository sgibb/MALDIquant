context("options")

test_that("options", {
    opt <- options()
    on.exit(options(opt))

    options(MALDIquant=NULL)
    expect_false(MALDIquant:::.negativeIntensitiesAllowed())
    options(MALDIquant=list(allowNegativeIntensities=TRUE))
    expect_true(MALDIquant:::.negativeIntensitiesAllowed())

    options(MALDIquant=list(allowNegativeIntensities=TRUE))
    expect_equal(
        MALDIquantOptions(),
        list(allowNegativeIntensities=TRUE)
    )
})
