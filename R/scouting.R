#' Add green codes to the scouted codes from a rally
#'
#' @param code character: a character vector of scouted codes for a rally. The codes must be non-compound but need only be the first 6 characters (i.e. up to and including the evaluation_code). The code vector must have a valid point code at the end (the `*p` or `ap` code)
#' @param meta list: meta component from a datavolley object
#'
#' @return A character vector of codes. This will be the same as the input `code` vector, but potentially with additional green code entries (`$$&H#`, `$$&H=`) inserted before the last entry
#'
#' @export
dv_green_codes <- function(code, meta) {
    if (all(tolower(substr(code, 2, 2)) %in% c("t", "c"))) return(code) ## timeout or sub
    if (!grepl("^[a\\*]p[[:digit:]]+:[[:digit:]]+", tail(code, 1))) stop("the final element of the code vector should be the '*p' or 'ap' code")
    won_by <- substr(tail(code, 1), 1, 1)
    if (!won_by %in% c("*", "a")) stop("won_by must be '*' or 'a'")
    team_char <- substr(code, 1, 1)
    team_char[!team_char %in% c("*", "a")] <- NA_character_ ## "*" or "a"
    skill <- substr(code, 4, 4)
    skill[!skill %in% c("S", "R", "A", "B", "D", "E", "F")] <- NA_character_
    evaluation_code <- substr(code, 6, 6)
    evaluation_code[!evaluation_code %in% c("#", "+", "!", "-", "/", "=")] <- NA_character_
    wswin <- meta$winning_symbols$win_lose == "W"
    ## for each skill/eval pair in code, is it a W or L or neither?
    my_wl <- vapply(seq_along(skill), function(i) {
        out <- meta$winning_symbols[meta$winning_symbols$skill %eq% skill[i] & meta$winning_symbols$code %eq% evaluation_code[i], ]
        if (nrow(out) == 1) out$win_lose else NA_character_
    }, FUN.VALUE = "A", USE.NAMES = FALSE)
    wsidx <- my_wl %eq% "W"
    lsidx <- my_wl %eq% "L"
    green_codes <- c()
    lost_by <- setdiff(c("*", "a"), won_by)
    if (sum(team_char %eq% won_by & wsidx) < 1) {
        green_codes <- paste0(won_by, "$$&H#")
    } else if (sum(team_char %eq% lost_by & wsidx) > 0) {
        warning("winning code for wrong team?")
    }
    if (sum(team_char %eq% lost_by & lsidx) < 1) {
        green_codes <- c(green_codes, paste0(lost_by, "$$&H="))
    } else if (sum(team_char %eq% won_by & lsidx) > 0) {
        warning("losing code for wrong team?")
    }
    c(head(code, -1), green_codes, tail(code, 1))
}



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
            scores <- c(max(set_plays$home_score_start_of_point, na.rm = TRUE), max(set_plays$visiting_score_start_of_point, na.rm = TRUE))
            x$meta$result$score[si] <- paste0(scores[1], "-", scores[2])
            x$meta$result$score_home_team[si] <- scores[1]
            x$meta$result$score_visiting_team[si] <- scores[2]
            ## duration
            set_start_end_time <- range(set_plays$video_time, na.rm = TRUE)
            x$meta$result$duration[si] <- if (any(is.infinite(set_start_end_time))) NA_real_ else round(diff(set_start_end_time) / 60)
            ## sets won
            ## need scores at end of points
            temp <- do.call(rbind, stringr::str_match_all(set_plays$code, "^[a\\*]p([[:digit:]]+):([[:digit:]]+)"))
            scores <- c(max(as.numeric(temp[, 2]), na.rm = TRUE), max(as.numeric(temp[, 3]), na.rm = TRUE))
            if (is_beach) {
                if (max(scores) >= 21 && abs(diff(scores)) >= 2) {
                    sets_won[which.max(scores)] <- sets_won[which.max(scores)] + 1L
                }
            } else {
                if ((si < 5 && max(scores) >= 25 && abs(diff(scores)) >= 2) || max(scores) >= 15 && abs(diff(scores)) >= 2) {
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
            ## subs
            all_home_pl <- unique(na.omit(as.numeric(unlist(x$plays[which(x$plays$set_number == si & !grepl(">LUp", x$plays$code, ignore.case = TRUE)), paste0("home_p", pseq)]))))
            home_subs <- na.omit(setdiff(all_home_pl, home_starting_lineup))
            x$meta$players_h[[paste0("starting_position_set", si)]][x$meta$players_h$number %in% home_subs] <- "*"
            ## visiting team
            visiting_starting_lineup <- as.numeric(x$plays[final_lup_row, paste0("visiting_p", pseq)])
            if (!paste0("starting_position_set", si) %in% names(x$meta$players_v)) x$meta$players_v[[paste0("starting_position_set", si)]] <- NA_character_
            for (j in seq_along(visiting_starting_lineup)) {
                pl_row <- which(x$meta$players_v$number == visiting_starting_lineup[j])
                if (length(pl_row) == 1) x$meta$players_v[[paste0("starting_position_set", si)]][pl_row] <- as.character(j)
            }
            ## subs
            all_visiting_pl <- unique(na.omit(as.numeric(unlist(x$plays[which(x$plays$set_number == si & !grepl(">LUp", x$plays$code, ignore.case = TRUE)), paste0("visiting_p", pseq)]))))
            visiting_subs <- na.omit(setdiff(all_visiting_pl, visiting_starting_lineup))
            x$meta$players_v[[paste0("starting_position_set", si)]][x$meta$players_v$number %in% visiting_subs] <- "*"
        }
    }
    x
}
