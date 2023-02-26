#' Add automatic codes to the scouted codes from a rally
#'
#' @param code character: a character vector of scouted codes for a rally. The codes must be non-compound but need only be the first 6 characters (i.e. up to and including the evaluation_code). The code vector must have a valid point code at the end (the `*p` or `ap` code)
#' @param meta list: meta component from a datavolley object
#'
#' @return A character vector of codes. This will be the same as the input `code` vector, but potentially with additional entries inserted before the last entry
#'
#' @export
dv_auto_codes <- function(code, meta) {
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
