## decoding abbrevations in dvw/other files

#' Decode skill type (tempo) codes
#'
#' @param skill character: full skill names (Serve, Reception, etc)
#' @param skill_type_code character: skill type (tempo) codes (Q, M, H, etc)
#' @param table data.frame: optional table with columns `skill`, `skill_type_code`, `skill_type`. If not provided, the default table for `data_type` and `style` will be used
#' @param data_type string: "indoor" or "beach"
#' @param style string: "default", "volleymetrics"
#'
#' @return A character vector of skill types, with a "dvmessages" attribute if any issues were found
#'
#' @examples
#' dv_decode_skill_type("Serve", "Q")
#' dv_decode_skill_type("Attack", "Z")
#'
#' @export
dv_decode_skill_type <- function(skill, skill_type_code, table, data_type = "indoor", style = "default") {
    if (missing(table)) table <- dv_default_skill_types(data_type = data_type, style = style)
    out <- left_join(tibble(skill = skill, skill_type_code = skill_type_code), table, by = c("skill", "skill_type_code")) %>%
        mutate(skill_type = case_when(is.na(.data$skill_type) & !is.na(.data$skill_type_code) ~ paste0("Unknown ", tolower(.data$skill), " skill type ", .data$skill_type_code), TRUE ~ .data$skill_type)) %>% dplyr::pull(.data$skill_type)
    #### anything with skill_type_code "*" gets applied to all such skills that don't have NA skill_type_code
    ##table <- table %>% dplyr::filter(.data$skill_type_code == "*")
    ##for (i in seq_len(nrow(table))) {
    ##    out[which(skill == table$skill[i] & !is.na(skill_type_code))] <- table$skill_type[i]
    ##}
    ## warn on missing skill with non-missing skill_type_code
    msgs <- NULL
    idx <- which(is.na(skill) & !is.na(skill_type_code))
    if (length(idx) > 0) msgs <- tibble(line_number = idx, message = paste0("Missing skill for non-missing skill type: ", skill_type_code[idx]), severity = 2)
    ## don't warn on missing skill_type_code but do warn on unrecognized ones
    idx <- which(grepl("Unknown", out) & !is.na(skill_type_code) & !is.na(skill))
    if (length(idx) > 0) msgs <- bind_rows(msgs, tibble(line_number = idx, message = paste0("Unexpected skill type: ", skill_type_code[idx], " for skill: ", skill[idx]), severity = 2))
    set_dvmsg(out, msgs)
}

#' Decode evaluation codes
#'
#' @param skill character: full skill names (Serve, Reception, etc)
#' @param evaluation_code character: evaluation codes (#, +, etc)
#' @param table data.frame: optional table with columns `skill`, `evaluation_code`, `evaluation`. If not provided, the default table for `data_type` and `style` will be used
#' @param data_type string: "indoor" or "beach"
#' @param style string: "default", "volleymetrics"
#'
#' @return A character vector of evaluations, with a "dvmessages" attribute if any issues were found
#'
#' @examples
#' dv_decode_evaluation("Serve", "#")
#' dv_decode_evaluation("Attack", "!")
#'
#' @export
dv_decode_evaluation <- function(skill, evaluation_code, table, data_type = "indoor", style = "default") {
    if (missing(table)) table <- dv_default_skill_evaluations(data_type = data_type, style = style)
    out <- left_join(tibble(skill = skill, evaluation_code = evaluation_code), table, by = c("skill", "evaluation_code")) %>%
        mutate(evaluation = case_when(is.na(.data$evaluation) & !is.na(.data$evaluation_code) ~ paste0("Unknown ", tolower(.data$skill), " evaluation ", .data$evaluation_code), TRUE ~ .data$evaluation)) %>% dplyr::pull(.data$evaluation)
    ## warn on missing skill with non-missing evaluation_code
    msgs <- NULL
    idx <- which(is.na(skill) & !is.na(evaluation_code))
    if (length(idx) > 0) msgs <- tibble(line_number = idx, message = paste0("Missing skill for non-missing evaluation code: ", evaluation_code[idx]), severity = 2)
    ## TO DECIDE - use default evaluations?
    ## don't warn on missing evaluation_code but do warn on unrecognized ones
    idx <- which(grepl("Unknown", out) & !is.na(evaluation_code) & !is.na(skill))
    if (length(idx) > 0) msgs <- bind_rows(msgs, tibble(line_number = idx, message = paste0("Unexpected skill type: ", evaluation_code[idx], " for skill: ", skill[idx]), severity = 2))
    set_dvmsg(out, msgs)
}


#' Decode skill subtypes
#'
#' @param skill character: full skill names (Serve, Reception, etc)
#' @param skill_subtype_code character: subtype codes (H, P, etc)
#' @param evaluation character: skill evaluations (Ace, Perfect pass, etc). Only used to make small adjustments for certain scouting styles, which will be skipped if `evaluation` is not provided
#' @param table data.frame: optional table with columns `skill`, `skill_subtype_code`, `skill_subtype`. If not provided, the default table for `data_type` and `style` will be used
#' @param data_type string: "indoor" or "beach"
#' @param style string: "default", "volleymetrics"
#'
#' @return A character vector of skill subtypes, with a "dvmessages" attribute if any issues were found
#'
#' @examples
#' dv_decode_skill_subtype("Reception", "M")
#' dv_decode_skill_subtype("Attack", "P")
#' dv_decode_skill_subtype("Dig", "Q")
#'
#' @export
dv_decode_skill_subtype <- function(skill, skill_subtype_code, evaluation, table, data_type = "indoor", style = "default") {
    if (missing(table)) table <- dv_default_skill_subtypes(data_type = data_type, style = style)
    dat <- tibble(skill = skill, skill_subtype_code = skill_subtype_code)
    if (!missing(evaluation)) dat$evaluation <- evaluation
    out <- left_join(dat, table, by = c("skill", "skill_subtype_code"))
    ## some adjustments for style
    if ("evaluation" %in% names(out)) {
        out <- out %>% mutate(skill_subtype = case_when(is.na(.data$skill_subtype_code) & .data$skill == "Dig" & grepl("(Positive|Poor) block cover", .data$evaluation) ~ "Spike cover", ## volleymetrics scouted block cover D/ or D!
                                                        TRUE ~ .data$skill_subtype))
    }
    out <- out %>% mutate(skill_subtype = case_when(is.na(.data$skill_subtype) & !is.na(.data$skill_subtype_code) ~ paste0("Unknown ", tolower(.data$skill), " subtype ", .data$skill_subtype_code), TRUE ~ .data$skill_subtype)) %>% dplyr::pull(.data$skill_subtype)
    ## warn on missing skill with non-missing skill_subtype_code
    msgs <- NULL
    idx <- which(is.na(skill) & !is.na(skill_subtype_code))
    if (length(idx) > 0) msgs <- tibble(line_number = idx, message = paste0("Missing skill for non-missing skill subtype code: ", skill_subtype_code[idx]), severity = 2)
    ## don't warn on missing skill_subtype_code but do warn on unrecognized ones
    idx <- which(grepl("Unknown", out) & !is.na(skill_subtype_code) & !is.na(skill))
    if (length(idx) > 0) msgs <- bind_rows(msgs, tibble(line_number = idx, message = paste0("Unexpected skill subtype code: ", skill_subtype_code[idx], " for skill: ", skill[idx]), severity = 2))
    set_dvmsg(out, msgs)
}

#' Decode number of players
#'
#' @param skill character: full skill names (Serve, Reception, etc)
#' @param num_players_code character: number of players codes (0, 1, etc)
#' @param table data.frame: optional table with columns `skill`, `num_players_code`, `num_players`. If not provided, the default table for `data_type` and `style` will be used
#' @param data_type string: "indoor" or "beach"
#' @param style string: "default", "volleymetrics"
#'
#' @return A character vector of number of players, with a "dvmessages" attribute if any issues were found
#'
#' @examples
#' dv_decode_num_players("Reception", 7)
#' dv_decode_num_players("Attack", 3, data_type = "beach")
#'
#' @export
dv_decode_num_players <- function(skill, num_players_code, table, data_type = "indoor", style = "default") {
    if (missing(table)) table <- dv_default_num_players(data_type = data_type, style = style)
    out <- left_join(tibble(skill = skill, num_players_code = as.integer(num_players_code)), table, by = c("skill", "num_players_code")) %>%
        mutate(num_players = case_when(is.na(.data$num_players) & !is.na(.data$num_players_code) ~ paste0("Unknown ", tolower(.data$skill), " number of players ", .data$num_players_code), TRUE ~ .data$num_players)) %>% dplyr::pull(.data$num_players)
    ## warn on missing skill with non-missing num_players_code
    msgs <- NULL
    idx <- which(is.na(skill) & !is.na(num_players_code))
    if (length(idx) > 0) msgs <- tibble(line_number = idx, message = paste0("Missing skill for non-missing number of players code: ", num_players_code[idx]), severity = 2)
    ## don't warn on missing num_players_code but do warn on unrecognized ones
    idx <- which(grepl("Unknown", out) & !is.na(num_players_code) & !is.na(skill))
    if (length(idx) > 0) msgs <- bind_rows(msgs, tibble(line_number = idx, message = paste0("Unexpected number of players code: ", num_players_code[idx], " for skill: ", skill[idx]), severity = 2))
    set_dvmsg(out, msgs)
}


#' Decode special codes
#'
#' @param skill character: full skill names (Serve, Reception, etc)
#' @param special_code character: special codes (Z, U, etc)
#' @param evaluation character: skill evaluations (Ace, Perfect pass, etc)
#' @param table data.frame: optional table with columns `skill`, `special_code`, `special`. If not provided, the default table for `data_type` and `style` will be used
#' @param data_type string: "indoor" or "beach"
#' @param style string: "default", "volleymetrics"
#'
#' @return A character vector of special code interpretations, with a "dvmessages" attribute if any issues were found
#'
#' @examples
#' dv_decode_special_code("Reception", "P", NA_character_) ## evaluation is irrelevant here
#' dv_decode_special_code("Attack", "N", "Error")
#' dv_decode_special_code("Attack", "N", "Winning attack") ## different interpretation if not an error
#' dv_decode_special_code("Attack", "C", "Winning attack") ## "C" is not valid for winning attack
#' dv_decode_special_code("Attack", "C", "Positive, good attack") ## but is for attacks in play
#' dv_decode_special_code("Attack", "Q", "Positive, good attack") ## this is invalid everywhere
#'
#' @export
dv_decode_special_code <- function(skill, special_code, evaluation, table, data_type = "indoor", style = "default") {
    if (missing(table)) table <- dv_default_special_codes(data_type = data_type, style = style)
    out <- left_join(tibble(skill = skill, special_code = special_code, evaluation = evaluation),
                     table %>% dplyr::filter(!is.na(.data$evaluation)) %>% dplyr::rename(special1 = "special"), by = c("skill", "evaluation", "special_code")) %>%
        ## then special codes that aren't evaluation-specific
        left_join(table %>% dplyr::filter(is.na(.data$evaluation)) %>% dplyr::select("skill", "special_code", special2 = "special"), by = c("skill", "special_code"))
    ## the non-evaluation-specific interpretations can only be applied to evaluations that do not have ANY specific interpretations
    temp <- table %>% dplyr::filter(!is.na(.data$evaluation)) %>% dplyr::distinct(.data$skill, .data$evaluation)
    ## so special2's cannot be used with any of these
    out <- left_join(out, temp %>% mutate(no_special2 = TRUE), by = c("skill", "evaluation")) %>%
        ## choose the specific one over the generic one, and reject the generic if not allowed
        mutate(special = case_when(!is.na(.data$special1) ~ .data$special1, is.na(.data$no_special2) ~ .data$special2)) %>%
        dplyr::select(-"special1", -"special2", -"no_special2") %>%
        mutate(special = case_when(is.na(.data$special) & !is.na(.data$special_code) ~ paste0("Unknown ", tolower(.data$skill), " special code ", .data$special_code), TRUE ~ .data$special)) %>% dplyr::pull(.data$special)
    ## warn on missing skill with non-missing special_code
    msgs <- NULL
    idx <- which(is.na(skill) & !is.na(special_code))
    if (length(idx) > 0) msgs <- tibble(line_number = idx, message = paste0("Missing skill for non-missing special code: ", special_code[idx]), severity = 2)
    ## don't warn on missing special_code but do warn on unrecognized ones
    idx <- which(grepl("Unknown", out) & !is.na(special_code) & !is.na(skill))
    if (length(idx) > 0) msgs <- bind_rows(msgs, tibble(line_number = idx, message = paste0("Unexpected special code: ", special_code[idx], " for skill: ", skill[idx], ifelse(is.na(evaluation[idx]), "", paste0(" and evaluation: ", evaluation[idx]))), severity = 2))
    set_dvmsg(out, msgs)
}
