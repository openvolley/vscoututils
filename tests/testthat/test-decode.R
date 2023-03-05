test_that("decoding works", {
    ## evaluation is irrelevant here
    expect_true(all(dv_decode_special_code("Reception", "P", c(NA_character_, "#")) == "Position error"))
    expect_equal(dv_decode_special_code("Attack", "N", "Error"), "Attack in net")
    ## different interpretation if not an error
    expect_equal(dv_decode_special_code("Attack", "N", "Winning attack"), "Let")

    ## "C" is not valid for winning attack
    temp <- dv_decode_special_code("Attack", "C", "Winning attack")
    expect_true(grepl("Unknown", temp))
    expect_is(get_dvmsg(temp), "data.frame")
    expect_equal(nrow(get_dvmsg(temp)), 1L)
    ## but is valid for attacks in play
    expect_equal(dv_decode_special_code("Attack", "C", "Positive, good attack"), "Block control")
    ## this is invalid everywhere
    expect_true(grepl("Unknown", dv_decode_special_code("Attack", "Q", "Positive, good attack")))
})
