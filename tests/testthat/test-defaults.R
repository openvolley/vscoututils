## ensure that default tables don't have duplicated entries, etc
test_that("default tables are ok", {
    for (sty in c("default", "volleymetrics", "german")) {
        for (dt in c("indoor")) {
            ## indoor only
            tbl <- dv_default_setter_calls(data_type = dt, style = sty)
            expect_equal(nrow(tbl), nrow(dplyr::distinct(tbl[, c("code")])))
        }
        for (dt in c("indoor", "beach")) {
            ## indoor and beach
            tbl <- dv_default_attack_combos(data_type = dt, style = sty)
            expect_equal(nrow(tbl), nrow(dplyr::distinct(tbl[, c("code")])))
            tbl <- dv_default_attack_combos(simplified = TRUE, data_type = dt, style = sty)
            expect_equal(nrow(tbl), nrow(dplyr::distinct(tbl[, c("code")])))

            tbl <- dv_default_winning_symbols(data_type = dt, style = sty)
            expect_equal(nrow(tbl), nrow(dplyr::distinct(tbl[, c("skill", "code")])))

            tbl <- dv_default_scouting_table(data_type = dt, style = sty)
            expect_equal(nrow(tbl), nrow(dplyr::distinct(tbl[, c("skill")])))

            tbl <- dv_default_skill_subtypes(data_type = dt, style = sty)
            expect_equal(nrow(tbl), nrow(dplyr::distinct(tbl[, c("skill", "skill_subtype_code")])))

            tbl <- dv_default_skill_types(data_type = dt, style = sty)
            expect_equal(nrow(tbl), nrow(dplyr::distinct(tbl[, c("skill", "skill_type_code")])))

            tbl <- dv_default_skill_evaluations(data_type = dt, style = sty)
            expect_equal(nrow(tbl), nrow(dplyr::distinct(tbl[, c("skill", "evaluation_code")])))

            tbl <- dv_default_num_players(data_type = dt, style = sty)
            expect_equal(nrow(tbl), nrow(dplyr::distinct(tbl[, c("skill", "num_players_code")])))

            tbl <- dv_default_special_codes(data_type = dt, style = sty)
            expect_equal(nrow(tbl), nrow(dplyr::distinct(tbl[, c("skill", "evaluation", "special_code")])))
        }
    }
})
