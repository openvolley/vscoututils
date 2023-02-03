test_that("coordinate conversion works", {
    expect_equal(as.numeric(pv_parse_ballstring(c("51, 143.33333"))), c(2.03, 2.20), tolerance = 1e-04)
})
