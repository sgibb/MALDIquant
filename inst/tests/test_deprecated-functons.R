context(".deprecated")

test_that(".deprecated", {
  expect_error(.deprecated("0.1", "foobar", "foobar"))
  expect_message(.deprecated(packageVersion("MALDIquant"), "foobar", "foobar"))
})

