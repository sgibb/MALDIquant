context(".unlist")

test_that(".unlist doesn't work recursive", {
  expect_identical(MALDIquant:::.unlist(list(a=list(a=1:3, b=4:6),
                                             b=list(c=7:9))),
                   list(1:3, 4:6, 7:9))
})
