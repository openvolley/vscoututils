## various tables that define default scouting behaviour

#' Default attack combination codes table
#' @param data_type string: "indoor", "beach"
#' @param simplified logical: if `TRUE`, just the most common ones
#'
#' @return A tibble
#'
#' @export
dv_default_attack_combos <- function(data_type = "indoor", simplified = TRUE) {
    data_type <- match.arg(data_type, c("indoor"))
    out <- dplyr::tribble(~code, ~attacker_position, ~side, ~type, ~description, ~X6, ~colour, ~start_coordinate, ~set_type,
                          "CB", 2, "L", "N", "Slide next to setter", NA, 16711680, 4976, "C",
                          "CD", 2, "L", "N", "Slide away from setter", NA, 16711680, 4970, "C",
                          "CF", 2, "L", "N", "Slide close to setter", NA,16711680, 4986, "C",
                          "C0", 7, "C", "U", "Medium Backrow D", NA, 0, 4114, "F",
                          "C5", 4, "R", "U", "11", NA, 0, 4912, "F",
                          "C6", 2, "L", "U", "Medium Red", NA, 0, 4988, "B",
                          "C8", 9, "C", "U", "Medium Backrow A", NA, 0, 4186, "B",
                          "II", 4, "R", "M", "Attack 4 AW", NA, 0, 4912, "F",
                          "JJ", 2, "L", "M", "Back AW", NA, 0, 4988, "B",
                          "PP", 3, "L", "O", "Setter Dump", NA, 16711680, 4964, "S",
                          "PR", 3, "C", "O", "Overpass hit", NA, 255, 4949, "-",
                          "P2", 3, "C", "O", "Attack on 2nd contact", NA, 255, 4949, "-",
                          "VB", 8, "C", "H", "High B Pipe", NA, 255, 4163, "P",
                          "VI", 2, "L", "H", "High banc AW", NA, 255, 4988, "B",
                          "VJ", 4, "R", "H", "High 4 AW", NA, 255, 4912, "F",
                          "VO", 9, "C", "H", "High Pipe 6-1 - oppo", NA, 255, 4163, "B",
                          "VP", 8, "C", "H", "High Pipe", NA, 255, 4150, "P",
                          "VR", 8, "C", "H", "High C Pipe", NA, 255, 4137, "P",
                          "VV", 7, "R", "H", "Emerg 4 high", NA, 0, 4912, "F",
                          "V0", 7, "C", "H", "High Ball Backrow D", NA, 255, 4114, "F",
                          "V3", 3, "C", "O", "High Ball Pos 3", NA, 255, 4950, "-",
                          "V4", 2, "L", "H", "High back short", NA, 255, 4868, "B",
                          "V5", 4, "R", "H", "High Ball Pos 4", NA, 255, 4912, "F",
                          "V6", 2, "L", "H", "High Ball Pos 2", NA, 255, 4988, "B",
                          "V8", 9, "C", "H", "High Ball Backrow A", NA, 255, 4186, "B",
                          "XB", 8, "C", "M", "B Pipe", NA, 16711680, 4163, "P",
                          "XC", 3, "R", "Q", "E Quick", NA, 65280, 4947, "C",
                          "XD", 3, "R", "Q", "Floating B Quick", NA, 65280, 4941, "C",
                          "XF", 2, "L", "N", "Slide by Opposite", NA, 65280, 4976, "B",
                          "XG", 3, "R", "Q", "Soup ball", NA, 65280, 4946, "C",
                          "XL", 2, "C", "Q", "A Quick in Pos 2", NA, 65280, 4868, "C",
                          "XM", 3, "C", "Q", "B Quick in Pos 3", NA, 65280, 4949, "C",
                          "XO", 2, "L", "Q", "C Quick by Opposite", NA, 65280, 4973, "B",
                          "XP", 8, "C", "M", "Pipe", NA, 16711680, 4150, "P",
                          "XQ", 2, "L", "M", "Mezza Dietro C.D.", NA, 16711680, 4976, "B",
                          "XR", 8, "C", "M", "C Pipe", NA, 16711680, 4138, "P",
                          "XS", 2, "L", "Q", "D Quick", NA, 65280, 4981, "C",
                          "XT", 3, "R", "M", "3 by Pos 4 Attacker", NA, 16711680, 4950, "F",
                          "XZ", 4, "R", "Q", "Short B Quick", NA, 65280, 4941, "C",
                          "X0", 7, "C", "T", "Backrow D", NA, 16711680, 4114, "F",
                          "X1", 3, "R", "Q", "A Quick", NA, 65280, 4956, "C",
                          "X2", 2, "L", "Q", "Quick behind", NA, 65280, 4868, "C",
                          "X3", 3, "L", "M", "3 by Pos 2 Attacker", NA, 16711680, 4950, "B",
                          "X4", 2, "L", "M", "4 (Inside 2)", NA, 16711680, 4976, "B",
                          "X5", 4, "R", "T", "Black", NA, 16711680, 4912, "F",
                          "X6", 2, "L", "T", "Red", NA, 16711680, 4988, "B",
                          "X7", 4, "R", "Q", "B Quick", NA, 65280, 4932, "C",
                          "X8", 9, "C", "T", "Backrow A", NA, 16711680, 4186, "B",
                          "X9", 4, "R", "M", "2 (Inside 4)", NA, 16711680, 4924, "F",
                          "ZP", 8, "C", "M", "Medium Pipe", NA, 0, 4150, "P",
                          "Z5", 4, "R", "M", "11", NA, 0, 4912, "F",
                          "Z6", 2, "L", "M", "Medium Red", NA, 0, 4988, "B",
                          "Z8", 9, "C", "M", "Medium Backrow A", NA, 0, 4186, "B"
                          )
    out$X10 <- out$X11 <- NA ## some other, unpopulated columns
    if (isTRUE(simplified)) {
        dplyr::filter(out, .data$code %in% c("X1", "X2", "X7", "XD", "CF", "CD", "PP", "PR", "P2",
                                             "VP", "V0", "V3", "V4", "V5", "V6", "V8",
                                             "XP", "X0", "X5", "X6", "X8"))
    } else {
        out
    }
}

#' Default setter calls table
#'
#' @param data_type string: "indoor", "beach"
#' @return A tibble
#'
#' @export
dv_default_setter_calls <- function(data_type = "indoor") {
    data_type <- match.arg(data_type, c("indoor"))
    dplyr::tribble(~code, ~X2, ~description, ~X4, ~colour, ~start_coordinate, ~mid_coordinate, ~end_coordinate, ~path, ~path_colour, ~X11,
                   "K1", NA, "Front Quick", NA, 16711680L, 3949L, 4454L, 4958L, NA_character_, NA_integer_, NA,
                   "K2", NA, "Back Quick", NA, 16711680L, 3864L, 4278L, 4974L, NA_character_, NA_integer_, NA,
                   "K7", NA, "Seven", NA, 16711680L, 3923L, 4426L, 4930L, NA_character_, NA_integer_, NA,
                   "KC", NA, "Quick in 3", NA, 16711680L, 3849L, 4449L, 5049L, NA_character_, NA_integer_, NA,
                   "KM", NA, "Shifted to 2", NA, 16711680L, 0L, 0L, 0L, "4924,5524,5530,6332,6312,5012,5024", 12632256L, NA,
                   "KP", NA, "Shifted to 4", NA, 16711680L, 0L, 0L, 0L, "5457,5057,5557,5552,6352,6364,5377,5077,5058,5058", 12632256L, NA,
                   "KE", NA, "No First Tempo", NA, 0L, 0L, 0L, 0L, "5858,5826,6426,6458,6458", 12632256L, NA)
}

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
    dplyr::tribble(~skill, ~default_skill, ~skill_type, ~evaluation_code,
                   "S", FALSE, "H", "+",
                   "R", FALSE, "H", "+",
                   "A", FALSE, "H", "+",
                   "B", FALSE, "H", "+",
                   "D", TRUE, "H", "+",
                   "E", FALSE, "H", "+",
                   "F", FALSE, "H", "+")
}


#' Default skill_subtype (type of hit) table
#'
#' @param data_type string: "indoor", "beach"
#' @return A tibble
#'
#' @export
dv_default_skill_subtypes <- function(data_type = "indoor") {
    data_type <- match.arg(data_type, c("indoor"))
    dplyr::tribble(~skill, ~skill_subtype_code, ~skill_subtype,
                   "Attack", "H", "Hard spike",
                   "Attack", "P", "Soft spike/topspin",
                   "Attack", "T", "Tip",
                   "Block", "A", "Block assist",
                   "Block", "T", "Block attempt",
                   "Block", "P", "Block on soft spike",
                   "Reception", "L", "On left",
                   "Reception", "R", "On right",
                   "Reception", "W", "Low",
                   "Reception", "O", "Overhand",
                   "Reception", "M", "Middle line",
                   "Set", "1", "1 hand set",
                   "Set", "2", "2 hands set",
                   "Set", "3", "Bump set",
                   "Set", "4", "Other set",
                   "Set", "5", "Underhand set",
                   "Set", "O", "Hand set",
                   "Set", "U", "Bump set",
                   "Dig", "S", "On spike",
                   "Dig", "C", "Spike cover",
                   "Dig", "B", "After block",
                   "Dig", "E", "Emergency",
                   "Dig", "T", "Tip",
                   "Dig", "P", "Soft spike")
}


#' Default skill_type (tempo) table
#'
#' @param data_type string: "indoor", "beach"
#' @param style string: conventions "default", "volleymetrics", etc
#' @return A tibble
#'
#' @export
dv_default_skill_types <- function(data_type = "indoor", style = "default") {
    rs <- if (style == "volleymetrics") {
              ## same for beach and indoor
              tribble(~skill, ~skill_type_code, ~skill_type,
                      "Serve", "Q", "Jump serve",
                      "Serve", "M", "Jump-float serve",
                      "Serve", "H", "Float serve", ## "float far" from the service line
                      "Serve", "T", "Float serve") ## "float near" from the service line
          } else if (grepl("beach", data_type)) {
              tribble(~skill, ~skill_type_code, ~skill_type,
                      "Serve", "Q", "Jump serve",
                      "Serve", "T", "Jump-float serve",
                      "Serve", "M", "Jump-float serve",
                      "Serve", "H", "Standing serve")
          } else {
              ## standard indoor
              ## also see N = hybrid
              tribble(~skill, ~skill_type_code, ~skill_type,
                      "Serve", "Q", "Jump serve",
                      "Serve", "M", "Jump-float serve",
                      "Serve", "H", "Float serve",
                      "Serve", "T", "Topspin serve")
          }
    abde <- tribble(~skill, ~skill_type_code, ~skill_type,
                    "Attack", "H", "High ball attack",
                    "Attack", "M", "Half ball attack",
                    "Attack", "Q", "Quick ball attack",
                    "Attack", "T", "Head ball attack",
                    "Attack", "U", "Super ball attack",
                    "Attack", "F", "Fast ball attack",
                    "Attack", "N", "Slide ball attack",
                    "Attack", "O", "Other attack")
    bind_rows(rs, rs %>% mutate(skill = "Reception"),
              abde, abde %>% mutate(skill = "Block"),
              abde %>% mutate(skill = "Dig"), abde %>% mutate(skill = "Set"),
              tibble(skill = "Freeball", skill_type_code = "H", skill_type = "High freeball"))

}

#' Default skill evaluation table
#'
#' @param data_type string: "indoor", "beach"
#' @param style string: conventions "default", "volleymetrics", etc
#' @return A tibble
#'
#' @export
dv_default_skill_evaluations <- function(data_type = "indoor", style = "default") {
    out <- tribble(~skill, ~evaluation_code, ~evaluation,
                   "Serve", "=", "Error",
                   "Serve", "/", "Positive, no attack",
                   "Serve", "-", "Negative, opponent free attack",
                   "Serve", "+", "Positive, opponent some attack",
                   "Serve", "#", "Ace",
                   "Serve", "!", "OK, no first tempo possible",
                   "Reception", "=", "Error",
                   "Reception", "/", "Poor, no attack",
                   "Reception", "-", "Negative, limited attack",
                   "Reception", "+", "Positive, attack",
                   "Reception", "#", "Perfect pass",
                   "Reception", "!", "OK, no first tempo possible",
                   "Attack", "=", "Error",
                   "Attack", "/", "Blocked",
                   "Attack", "-", "Poor, easily dug",
                   "Attack", "!", "Blocked for reattack",
                   "Attack", "+", "Positive, good attack",
                   "Attack", "#", "Winning attack",
                   "Block", "=", "Error",
                   "Block", "/", "Invasion",
                   "Block", "-", "Poor, opposition to replay",
                   "Block", "+", "Positive, block touch",
                   "Block", "#", "Winning block",
                   "Block", "!", "Poor, opposition to replay",
                   "Dig", "=", "Error",
                   "Dig", "/", "Ball directly back over net",
                   "Dig", "-", "No structured attack possible",
                   "Dig", "#", "Perfect dig",
                   "Dig", "+", "Good dig",
                   "Dig", "!", "OK, no first tempo possible",
                   "Set", "=", "Error",
                   "Set", "-", "Poor",
                   "Set", "/", "Poor",
                   "Set", "+", "Positive",
                   "Set", "#", "Perfect",
                   "Set", "!", "OK",
                   "Freeball", "=", "Error",
                   "Freeball", "/", "Poor",
                   "Freeball", "!", "OK, no first tempo possible",
                   "Freeball", "-", "OK, only high set possible",
                   "Freeball", "+", "Good",
                   "Freeball", "#", "Perfect")
    if (style == "volleymetrics") {
        out <- mutate(out, evaluation = case_when(
                               .data$skill == "B" & .data$evaluation_code == "/" ~ "Poor, opposition to replay",
                               .data$skill == "B" & .data$evaluation_code == "!" ~ "Poor, blocking team cannot recover", ## negative block, unplayable to our side
                               .data$skill == "B" & .data$evaluation_code == "-" ~ "Poor block", ## negative block touch, either back to opposition or poor on our side
                               .data$skill == "B" & .data$evaluation_code == "+" ~ "Positive block", ## positive block touch, either to our defense or difficult for opposition
                               .data$skill == "D" & .data$evaluation_code == "/" ~ "Positive block cover", ## D/ is block cover that gives attacking team a chance to re-attack
                               .data$skill == "D" & .data$evaluation_code == "!" ~ "Poor block cover", ## block cover that does not give attacking team a chance to re-attack or is an error
                               .data$skill == "E" & .data$evaluation_code == "/" ~ "Error (reach over net)", ## E/ is a reach
                               TRUE ~ .data$evaluation))
    } else if (style == "german") {
        ## swap B= Error and B/ Invasion
        out <- mutate(out, evaluation = case_when(.data$skill=="B" & .data$evaluation_code == "/" ~ "Error",
                                                  .data$skill=="B" & .data$evaluation_code == "=" ~ "Invasion",
                                                  TRUE ~ .data$evaluation))
    }
    out
}
