test_that("file type detection works", {
    expect_identical(dv_file_type(ovdata::ovdata_example("190301_kats_beds")), "dvw")
    expect_identical(dv_file_data_type(readLines(ovdata::ovdata_example("190301_kats_beds"))), "dvw")

    expect_identical(dv_file_type(ovdata::ovdata_example("clickscout")), "dvw")
    expect_identical(dv_file_data_type(readLines(ovdata::ovdata_example("clickscout"))), "dvw")

    expect_identical(dv_file_type(ovdata::ovdata_example("2017_AVL_mens_HEAT_vs_UTSSU")), "psvb")
    expect_identical(dv_file_data_type(readLines(ovdata::ovdata_example("2017_AVL_mens_HEAT_vs_UTSSU"))), "psvb")

    expect_identical(dv_file_type(tempfile(fileext = ".dvw")), "unknown")
    expect_identical(dv_file_type(tempfile(fileext = ".txt")), "unknown")
    expect_identical(expect_warning(dv_file_data_type(readLines(tempfile(fileext = ".txt")))), "unknown")
    expect_identical(dv_file_data_type(""), "unknown")
    expect_identical(dv_file_data_type(NULL), "unknown")

    tf <- tempfile(fileext = ".vsm")
    temp <-  '{"gameType":"indoor","version":1,"startDate":"2022-11-04T04:56:28.681Z","scout":{"sets":[{"_id":"63649c8c62b3d751","startingLineup":{"home":{"positions"'
    cat(temp, file = tf)
    expect_identical(dv_file_type(tf), "vsm")
    unlink(tf)
    expect_identical(dv_file_data_type(temp), "vsm")

    tf <- tempfile(fileext = ".xml")
    temp <- c("blah", "blah", "<ALL_INSTANCES>")
    cat(temp, file = tf, sep = "\n")
    expect_identical(dv_file_type(tf), "hxml")
    unlink(tf)
    expect_identical(dv_file_data_type(temp), "hxml")

    ## multiple files
    expect_identical(dv_file_type(c(ovdata::ovdata_example("190301_kats_beds"), tempfile(fileext = ".dvw"))), c("dvw", "unknown"))
})
