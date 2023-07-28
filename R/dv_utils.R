## utility functions for working with datavolley objects

#' Update the metadata component of a datavolley object
#'
#' These elements will be updated:
#' * the `played`, `duration`, and score-related columns in `x$meta$result`
#' * the `sets_won` and `won_match` columns in `x$meta$teams`
#' * the starting positions and substitutions in `x$meta$players_h` and `x$meta$players_v`
#'
#' @param x datavolley: as returned by e.g. `datavolley::dv_read()`
#'
#' @return A modified copy of `x`
#'
#' @export
dv_update_meta <- function(x) {
    is_beach <- any(grepl("beach", x$meta$match$regulation))
    pseq <- seq_len(if (is_beach) 2L else 6L)
    ## update all set results, including durations
    set_start_rows <- which(grepl(">LUp", x$plays$code, ignore.case = TRUE) & (!grepl(">LUp", dplyr::lag(x$plays$code), ignore.case = TRUE) | seq_along(x$plays$code) == 1))
    if (nrow(x$meta$result) < length(set_start_rows)) {
        temp <- length(set_start_rows) - nrow(x$meta$result)
        x$meta$result <- bind_rows(x$meta$result, tibble(played = rep(NA, temp), score_intermediate1 = rep(NA_character_, temp), score_intermediate2 = rep(NA_character_, temp), score_intermediate3 = rep(NA_character_, temp), score = rep(NA_character_, temp), duration = rep(NA_real_, temp), X7 = rep(NA, temp), score_home_team = rep(NA_real_, temp), score_visiting_team = rep(NA_real_, temp)))
    }
    x$meta$result$played[seq_along(set_start_rows)] <- TRUE
    set_end_rows <- grep("^\\*\\*[[:digit:]]set", x$plays$code)
    if (length(set_start_rows) == (length(set_end_rows) + 1) && length(set_start_rows) > 1 && all(set_end_rows > head(set_start_rows, -1))) {
        ## we have an uncompleted set, that isn't the first set
        ## so we can still update the meta for the earlier sets
        set_start_rows <- head(set_start_rows, -1)
    }
    if (length(set_start_rows) == length(set_end_rows) && all(set_end_rows > set_start_rows)) {
        sets_won <- c(0L, 0L) ## sets won by home, visiting teams
        for (si in seq_along(set_start_rows)) {
            ##message("updating scores for set ", si)
            set_plays <- x$plays[seq(set_start_rows[si], set_end_rows[si]), ]
            ## scores
            scores <- c(max(set_plays$home_team_score, na.rm = TRUE), max(set_plays$visiting_team_score, na.rm = TRUE))
            x$meta$result$score[si] <- paste0(scores[1], "-", scores[2])
            x$meta$result$score_home_team[si] <- scores[1]
            x$meta$result$score_visiting_team[si] <- scores[2]
            ## duration
            set_start_end_time <- if (!all(is.na(set_plays$video_time))) range(set_plays$video_time, na.rm = TRUE) else NA_real_
            thisdur <- x$meta$result$duration[si]
            ## if the existing duration is non-NA and looks valid, keep it
            if (is.na(thisdur) || thisdur < 1 || thisdur > 60) {
                ## otherwise attempt to re-generate it from video times
                x$meta$result$duration[si] <- if (any(is.infinite(set_start_end_time) | is.na(set_start_end_time))) NA_real_ else round(diff(set_start_end_time) / 60)
            }
            ## sets won
            ## need scores at end of points
            temp <- do.call(rbind, stringr::str_match_all(set_plays$code, "^[a\\*]p([[:digit:]]+):([[:digit:]]+)"))
            scores <- c(max(as.numeric(temp[, 2]), na.rm = TRUE), max(as.numeric(temp[, 3]), na.rm = TRUE))
            if (is_beach) {
                if (((si < 3 && max(scores) >= 21) || (si > 2 && max(scores) >= 15)) && abs(diff(scores)) >= 2) {
                    sets_won[which.max(scores)] <- sets_won[which.max(scores)] + 1L
                }
            } else {
                if (((si < 5 && max(scores) >= 25) || (si > 4 && max(scores) >= 15)) && abs(diff(scores)) >= 2) {
                    sets_won[which.max(scores)] <- sets_won[which.max(scores)] + 1L
                }
            }
            iss <- if (is_beach) c(5, 10, 15) else if (si >= 5) c(5, 10, 12) else c(8, 16, 21) ## intermediate score levels
            for (issi in seq_along(iss)) {
                idx <- which(set_plays$home_score_start_of_point == iss[issi] | set_plays$visiting_score_start_of_point == iss[issi])
                if (length(idx) > 0) {
                    idx <- min(idx)
                    if (!is.na(set_plays$home_score_start_of_point[idx]) && !is.na(set_plays$visiting_score_start_of_point[idx])) {
                        x$meta$result[[paste0("score_intermediate", issi)]][si] <- paste0(set_plays$home_score_start_of_point[idx], "-", set_plays$visiting_score_start_of_point[idx])
                    }
                }
            }
        }
        x$meta$teams$sets_won <- sets_won
        if ((is_beach && max(sets_won) > 1) || max(sets_won) > 2) x$meta$teams$won_match <- (sets_won == max(sets_won))
    }
    ## starting lineups and subs
    ## this can be done even for sets that haven't been completed
    for (si in seq_len(max(x$plays$set_number, na.rm = TRUE))) {
        ## use the final >LUp row for starting lineup
        final_lup_row <- which(x$plays$set_number == si & grepl(">LUp", x$plays$code, ignore.case = TRUE))
        if (length(final_lup_row) > 0) final_lup_row <- max(final_lup_row)
        if (length(final_lup_row) == 1) {
            home_starting_lineup <- as.numeric(x$plays[final_lup_row, paste0("home_p", pseq)])
            if (!paste0("starting_position_set", si) %in% names(x$meta$players_h)) x$meta$players_h[[paste0("starting_position_set", si)]] <- NA_character_
            for (j in seq_along(home_starting_lineup)) {
                pl_row <- which(x$meta$players_h$number == home_starting_lineup[j])
                if (length(pl_row) == 1) x$meta$players_h[[paste0("starting_position_set", si)]][pl_row] <- as.character(j)
            }
            ## visiting team
            visiting_starting_lineup <- as.numeric(x$plays[final_lup_row, paste0("visiting_p", pseq)])
            if (!paste0("starting_position_set", si) %in% names(x$meta$players_v)) x$meta$players_v[[paste0("starting_position_set", si)]] <- NA_character_
            for (j in seq_along(visiting_starting_lineup)) {
                pl_row <- which(x$meta$players_v$number == visiting_starting_lineup[j])
                if (length(pl_row) == 1) x$meta$players_v[[paste0("starting_position_set", si)]][pl_row] <- as.character(j)
            }
        }
        ## subs
        all_home_pl <- unique(na.omit(as.numeric(unlist(x$plays[which(x$plays$set_number == si & !grepl("^(>LUp|\\*\\*[[:digit:]]set)", x$plays$code, ignore.case = TRUE)), paste0("home_p", pseq)]))))
        ## also any players recorded making a play, because liberos won't appear in the home_pX lineup columns
        all_home_pl <- unique(c(all_home_pl, c(unique(na.omit(x$plays$player_number[which(!is.na(x$plays$skill) & x$plays$team == x$plays$home_team & x$plays$set_number == si)])))))
        home_subs <- na.omit(setdiff(all_home_pl, home_starting_lineup))
        x$meta$players_h[[paste0("starting_position_set", si)]][x$meta$players_h$number %in% home_subs] <- "*"
        ## visiting
        all_visiting_pl <- unique(na.omit(as.numeric(unlist(x$plays[which(x$plays$set_number == si & !grepl("^(>LUp|\\*\\*[[:digit:]]set)", x$plays$code, ignore.case = TRUE)), paste0("visiting_p", pseq)]))))
        all_visiting_pl <- unique(c(all_visiting_pl, c(unique(na.omit(x$plays$player_number[which(!is.na(x$plays$skill) & x$plays$team == x$plays$visiting_team & x$plays$set_number == si)])))))
        visiting_subs <- na.omit(setdiff(all_visiting_pl, visiting_starting_lineup))
        x$meta$players_v[[paste0("starting_position_set", si)]][x$meta$players_v$number %in% visiting_subs] <- "*"
    }
    x
}

