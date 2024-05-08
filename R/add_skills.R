#' Find attacks for which we could insert missing setting actions
#'
#' @param x datavolley: datavolley object as returned by [datavolley::dv_read()]
#' @param no_set_attacks character: vector of attack codes for which we will not automatically insert sets (e.g. setter tips, overpass attacks)
#' @param phase_select character: play phase(s) of attacks to consider. One or more of "Reception", "Transition"
#'
#' @return The row numbers of attacks in the `plays` component of x, for which sets could be inserted
#'
#' @export
dv_insert_sets_check <- function(x, no_set_attacks = c("PP", "P2", "PR"), phase_select = "Reception") {
    if (!(is.list(x) && "plays" %in% names(x))) stop("x should be a datavolley object")
    which(x$plays$skill == "Attack" & !(x$plays$attack_code %in% no_set_attacks) & !(lag(x$plays$skill) %eq% "Set") & x$plays$phase %in% phase_select)
}

#' Insert setting actions for attacks that have not been scouted with sets
#'
#' This function will insert setting actions prior to attacks, where those attacks do not already have a set scouted. The sets are assigned to the setter on court for that team. It is therefore possible to scout a match, only manually scouting the setting actions where they were made by a player other than the designated setter. The remaining setting actions (made by the designated setter) can be filled in using this function, making the live scouting a little more efficient. Note, however, that automatically-inserted sets do not have the full information that can be included when scouting manually, including setter calls (but see the note on the `set_call_table` parameter) and location of the set.
#'
#' @param x datavolley: datavolley object as returned by [datavolley::dv_read()]
#' @param no_set_attacks character: vector of attack codes for which we will not automatically insert sets (e.g. setter tips, overpass attacks)
#' @param phase_select character: play phase(s) of attacks to consider. One or more of "Reception", "Transition"
#' @param default_set_evaluation string: the default evaluation code for a set (used unless the attack was against 0 or 1 blockers, in which case it gets "#")
#' @param set_call_table data.frame: a data.frame with columns `attack_code` and `set_call`. Setter calls will be added to sets associated with attack codes in this list. Note that setter calls from this table will NOT be inserted on sets where the setter did not set the middle hitter (e.g. if the middle ran X1 but the setter set someone else, no "K1" call can be inserted because there is no way of knowing what the middle was running). This gives a biased set of setter call entries that are unlikely to be useful for analysis purposes. It is therefore recommended that you provide `set_call_table` to this function ONLY if you are then going to manually insert setter calls on the remaining rows
#' @param attack_rows integer: a vector of row numbers of attacks for which sets should be inserted. Automatically calculated if not provided
#'
#' @return A modified copy of `x`
#'
#' @examples
#' \dontrun{
#'   x <- dv_read(dv_example_file())
#'   sum(plays(x)$skill == "Set", na.rm = TRUE)
#'   x <- dv_insert_sets(x)
#'   sum(plays(x)$skill == "Set", na.rm = TRUE)
#' }
#'
#' @export
dv_insert_sets <- function(x, no_set_attacks = c("PP", "P2", "PR"), phase_select = "Reception", default_set_evaluation = "+", attack_rows, set_call_table) {
    if (missing(attack_rows) || is.null(attack_rows)) attack_rows <- dv_insert_sets_check(x, no_set_attacks = no_set_attacks, phase_select = phase_select)
    if (length(attack_rows) < 1) return(x)
    set_data <- x$plays %>% mutate(passQ = case_when(lag(.data$skill) == "Reception" ~ lag(.data$evaluation)),
                                   digQ = case_when(lag(.data$skill) == "Dig" ~ lag(.data$evaluation)))
    set_data <- set_data[attack_rows, ] %>%
        mutate(team_oncourt_setter_number = case_when(.data$team == .data$home_team ~ case_when(.data$home_setter_position == 1 ~ .data$home_p1,
                                                                                                .data$home_setter_position == 2 ~ .data$home_p2,
                                                                                                .data$home_setter_position == 3 ~ .data$home_p3,
                                                                                                .data$home_setter_position == 4 ~ .data$home_p4,
                                                                                                .data$home_setter_position == 5 ~ .data$home_p5,
                                                                                                .data$home_setter_position == 6 ~ .data$home_p6),
                                                      .data$team == .data$visiting_team ~ case_when(.data$visiting_setter_position == 1 ~ .data$visiting_p1,
                                                                                                    .data$visiting_setter_position == 2 ~ .data$visiting_p2,
                                                                                                    .data$visiting_setter_position == 3 ~ .data$visiting_p3,
                                                                                                    .data$visiting_setter_position == 4 ~ .data$visiting_p4,
                                                                                                    .data$visiting_setter_position == 5 ~ .data$visiting_p5,
                                                                                                    .data$visiting_setter_position == 6 ~ .data$visiting_p6)),
               s_team = str_sub(.data$code, 1, 1), ## team code
               s_tempo = str_sub(.data$code, 5, 5),
               s_eval = case_when(.data$num_players_numeric %in% c(0, 1) ~ "#", ## FIVB conventions on set quality
                                  TRUE ~ default_set_evaluation),
               set_scout_code = paste0(enforce_width(.data$s_team), ## team
                                       ldz2(.data$team_oncourt_setter_number), ## setter player_number, leading zeros
                                       "E", ## set skill
                                       enforce_width(.data$s_tempo), ## hitting tempo
                                       enforce_width(.data$s_eval))) ## evaluation code. No setter call etc appended yet, done below
    set_scout_code <- set_data$set_scout_code

    ## setter calls
    tempsc <- rep("~~", length(set_scout_code)) ## default (no) setter call
    if (!missing(set_call_table) && is.data.frame(set_call_table) && nrow(set_call_table) > 0 && all(c("attack_code", "set_call") %in% names(set_call_table))) {
        set_call_table$set_call <- enforce_width(set_call_table$set_call, width = 2)
        for (sci in seq_len(nrow(set_call_table))) tempsc[which(set_data$attack_code == set_call_table$attack_code[sci])] <- set_call_table$set_call[sci]
        ## previously added "KK"("unknown") setter calls where pass was reasonable but the setter did not set the middle
        ##tempsc[grepl("^(Perfect|Positive|OK)", set_data$passQ) & tempsc %eq% "~~"] <- "KK"
    }
    set_scout_code <- paste0(set_scout_code, tempsc)

    ## populate the target attacker using the x$meta$attacks table
    temp_tgt <- rep("~", length(set_scout_code))
    if (nrow(x$meta$attacks) > 0 && all(c("code", "attacker_position", "set_type") %in% names(x$meta$attacks))) {
        for (ai in seq_len(nrow(x$meta$attacks))) {
            idx <- which(set_data$attack_code == x$meta$attacks$code[ai])
            if (length(idx) > 0) {
                this_tgt <- x$meta$attacks$set_type[ai]
                if (nchar(this_tgt) == 1) {
                    if (!this_tgt %in% c("B", "C", "F", "P", "S")) this_tgt <- "~"
                    temp_tgt[idx] <- this_tgt
                }
            }
        }
    }
    ## append target, start_zone, end_zone (end zone is start zone of attack)
    set_scout_code <- paste0(set_scout_code, temp_tgt, "~", enforce_width(as.character(set_data$start_zone)))
    ## the hard-coded "~" is the start zone, which is not populated for sets, the end zone is used instead

    x$plays <- mutate(x$plays, .TEMP_row_number = row_number())
    temp <- dv_get_player_meta(team = set_data$s_team, number = set_data$team_oncourt_setter_number, meta = x$meta)
    newline <- x$plays[attack_rows, ]
    newline <- newline %>% mutate(##video_time = .data$video_time - 2, ## could adjust video time, but would need to make sure it doesn't end up being before the preceding (reception/dig) skill. Leave that for now (better handled in the dv_sync_video function)
                               code = set_scout_code,
                               player_number = set_data$team_oncourt_setter_number,
                               player_name = temp$name, player_id = temp$player_id,
                               skill = "Set",
                               skill_type = sub(" attack", " set", .data$skill_type),
                               evaluation_code = set_data$s_eval,
                               evaluation = dv_decode_evaluation("Set", set_data$s_eval),
                               attack_code = NA_character_,
                               attack_description = NA_character_,
                               set_code = tilde_as(tempsc, "character"),## this is the setter call, not the scouted input code
                               set_type = tilde_as(temp_tgt, "character"),
                               start_zone = NA_integer_,
                               end_zone = set_data$start_zone,
                               end_subzone = NA_character_, end_cone = NA_integer_, skill_subtype = NA_character_,
                               num_players = NA_character_, num_players_numeric = NA_integer_,
                               special_code = NA_character_,
                               ##    file_line_number will be modified below
                               start_coordinate = NA_integer_, mid_coordinate = NA_integer_, end_coordinate = NA_integer_,
                               start_coordinate_x = NA_real_, start_coordinate_y = NA_real_, mid_coordinate_x = NA_real_,  mid_coordinate_y = NA_real_,
                               end_coordinate_x = NA_real_, end_coordinate_y = NA_real_,
                               winning_attack = FALSE,
                               .TEMP_row_number = .data$.TEMP_row_number - 0.5, .inserted = 1L) %>%
        ## replace set_description, but take care to keep column ordering the same
        left_join(x$meta$sets %>% dplyr::select("code", set_description2 = "description"), by = c(set_code = "code")) %>%
        mutate(set_description = .data$set_description2) %>% dplyr::select(-"set_description2")

    x$plays <- bind_rows(x$plays %>% mutate(.inserted = 0L), newline) %>% dplyr::arrange(.data$.TEMP_row_number) %>%
        mutate(file_line_number = .data$file_line_number + lag(cumsum(.data$.inserted), default = 0)) %>%
        dplyr::select(-".TEMP_row_number", -".inserted")
    x
}

## tilde-pad strings, replacing NAs and empty strings
enforce_width <- function(z, width = 1) {
    z[is.na(z) | !nzchar(z)] <- if (width == 1) "~" else paste0(rep("~", width), collapse = "")
    z[nchar(z) < width] <- stringr::str_pad(z[nchar(z) < width], width = width, side = "right", pad = "~")
    z[nchar(z) > width] <- stringr::str_sub(z[nchar(z) > width], 1L, width)
    z
}

## take character vector, strip the tildes and optionally cast to new type (Class)
tilde_as <- function(z, Class = "integer") {
    z <- gsub("~", "", z)
    if (Class == "character") {
        z[!nzchar(z)] <- NA_character_
        z
    } else {
        as(z, Class)
    }
}
