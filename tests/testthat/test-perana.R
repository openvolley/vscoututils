test_that("internal perana checks", {
    ## make sure that our 'default' data don't have duplicated entries, etc
    expect_false(any(duplicated(pv_default_eventtypes("beach")$eventtype)))
    expect_equal(nrow(pv_default_eventgrades("beach")), nrow(unique(as.data.frame(pv_default_eventgrades("beach")[, c("skill", "eventgrade")]))))
})
