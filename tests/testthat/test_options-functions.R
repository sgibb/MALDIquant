context("options")

test_that("options", {
    on.exit(options(MALDIquant=NULL))

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
