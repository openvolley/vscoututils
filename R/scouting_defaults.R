#' Default winning symbols table
#'
#' @return A tibble
#'
#' @export
dv_default_winning_symbols <- function() {
    tribble(~skill, ~win_lose, ~code,
            "S", "L", "=",
            "S", "W", "#",
            "R", "L", "=",
            "A", "L", "=",
            "A", "L", "/",
            "A", "W", "#",
            "B", "L", "=",
            "B", "L", "/",
            "B", "W", "#",
            "D", "L", "=",
            "E", "L", "=",
            "F", "L", "=")
}

#' Default scouting (type and evaluation for each skill) table
#'
#' @return A tibble
#'
#' @export
dv_default_scouting_table <- function() {
    dplyr::tribble(~skill, ~default_skill, ~tempo, ~evaluation_code,
                   "S", FALSE, "H", "+",
                   "R", FALSE, "H", "+",
                   "A", FALSE, "H", "+",
                   "B", FALSE, "H", "+",
                   "D", TRUE, "H", "+",
                   "E", FALSE, "H", "+",
                   "F", FALSE, "H", "+")
}
