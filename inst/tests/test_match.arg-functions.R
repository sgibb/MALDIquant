context(".match.arg")

fun <- function(x=c("A", "B", "Fail", "Foobar")) {
  return(MALDIquant:::.match.arg(x))
}

test_that(".match.arg", {
  expect_identical(fun("A"), "A")
  expect_identical(fun("a"), "A")
  expect_identical(fun("fA"), "Fail")
  expect_identical(fun("FO"), "Foobar")
  expect_error(fun("X"),
               ".*arg.* should be one of .*A.*, .*B.*, .*Fail.*, .*Foobar.*")
})

