context(".deprecated")

test_that(".deprecated", {
  expect_error(.deprecated("0.1", "foobar", "foobar"))
  expect_message(.deprecated(packageVersion("MALDIquant"), "foobar", "foobar"))
})

test_that(".deprecatedFunction", {
  expect_error(.deprecatedFunction("0.1", old="foobar",
                                   "\"foobar\" is deprecated."))
  expect_error(.deprecatedFunction("0.1", old="foo", new="bar",
    "\"foo\" is deprecated.\nUse \"bar\" instead. See help(\"bar\")."))
})

test_that(".deprecatedArgument", {
  foobar <- function(x, y) {
    .deprecatedArgument("0.1", old="x", new="y")
  }
  expect_error(foobar(1L, 1L),
    "Argument .*x.* is deprecated..*Use .*y.* instead. See help(.*foobar.*).")
  foobar <- function(x, y) {
    .deprecatedArgument("0.1", old="x")
  }
  expect_error(foobar(1L, 1L),
    "Argument \"x\" is deprecated.")
})

