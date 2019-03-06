context("OnDiskVector")

test_that(".valid.OnDiskVector.path", {
    f <- tempfile()
    on.exit(unlink(f))
    file.create(f)
    expect_null(.valid.OnDiskVector.path(f))
    expect_match(.valid.OnDiskVector.path(1:2), "length 1")
    expect_match(.valid.OnDiskVector.path(tempfile()), "exists")
})

test_that(".valid.OnDiskVector.n", {
    expect_null(.valid.OnDiskVector.n(1))
    expect_match(.valid.OnDiskVector.n(1:2), "length 1")
})

test_that(".valid.OnDiskVector.size", {
    expect_null(.valid.OnDiskVector.size(1))
    expect_null(.valid.OnDiskVector.size(8))
    expect_match(.valid.OnDiskVector.size(1:2), "length 1")
    expect_match(.valid.OnDiskVector.size(3), "has to be 2\\^x")
})

test_that(".valid.OnDiskVector.n", {
    expect_null(.valid.OnDiskVector.offset(1))
    expect_match(.valid.OnDiskVector.offset(1:2), "length 1")
    expect_match(.valid.OnDiskVector.offset(-1), ">= 0")
})

test_that("validity", {
    odv <- OnDiskVector(1:3)
    f <- odv@path
    odv@path <- "foobar"
    expect_error(validObject(odv), "exists")
    odv@path <- f
    odv@offset <- -1
    expect_error(validObject(odv), ">= 0")
})

test_that(".isModified.OnDiskVector", {
    odv1 <- OnDiskVector(1:3)
    odv2 <- odv1
    odv2[] <- 4:6
    expect_error(odv1[1], "was modified")
    expect_error(odv1[], "was modified")
    expect_equal(odv2[1], 4)
    expect_equal(odv2[], 4:6)
})

test_that("constructor", {
    expect_error(OnDiskVector(), "necessary")
})

test_that("length", {
    odv <- OnDiskVector(1:10)
    expect_length(odv, 10)
})

test_that("[", {
    odv <- OnDiskVector(1:10)
    expect_error(odv[0], "out of boundaries")
    expect_error(odv[11], "out of boundaries")
    expect_equal(odv[3], 3)
    expect_equal(odv[c(1, 10)], c(1, 10))
    expect_equal(odv[c(1, 2, 10)], c(1, 2, 10))
    expect_equal(odv[], 1:10)
    expect_equal((odv[] <- 11:20)[1:10], 11:20)

    fn <- tempfile()
    f <- file(fn, "wb")
    writeBin(as.double(1:20), f, size=4L, endian="little")
    close(f)
    on.exit(unlink(fn))

    odv2 <- OnDiskVector(path=fn, offset=4L * 10L, n=10L, size=4L)
    expect_equal(odv2[1:3], 11:13)
})
