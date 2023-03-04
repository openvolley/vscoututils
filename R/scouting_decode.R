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
    out <- left_join(tibble(skill = skill, skill_type_code = skill_type_code), table, by = c("skill", "skill_type_code"))$skill_type
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
    idx <- which(is.na(out) & !is.na(skill_type_code) & !is.na(skill))
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
#' @return A character vector of skill types, with a "dvmessages" attribute if any issues were found
#'
#' @examples
#' dv_decode_evaluation("Serve", "#")
#' dv_decode_evaluation("Attack", "!")
#'
#' @export
dv_decode_evaluation <- function(skill, evaluation_code, table, data_type = "indoor", style = "default") {
    if (missing(table)) table <- dv_default_skill_evaluations(data_type = data_type, style = style)
    out <- left_join(tibble(skill = skill, evaluation_code = evaluation_code), table, by = c("skill", "evaluation_code"))$evaluation
    ## warn on missing skill with non-missing evaluation_code
    msgs <- NULL
    idx <- which(is.na(skill) & !is.na(evaluation_code))
    if (length(idx) > 0) msgs <- tibble(line_number = idx, message = paste0("Missing skill for non-missing evaluation code: ", evaluation_code[idx]), severity = 2)
    ## TO DECIDE - use default evaluations?
    ## don't warn on missing evaluation_code but do warn on unrecognized ones
    idx <- which(is.na(out) & !is.na(evaluation_code) & !is.na(skill))
    if (length(idx) > 0) msgs <- bind_rows(msgs, tibble(line_number = idx, message = paste0("Unexpected skill type: ", evaluation_code[idx], " for skill: ", skill[idx]), severity = 2))
    set_dvmsg(out, msgs)
}

