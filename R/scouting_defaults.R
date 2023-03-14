## various tables that define default scouting behaviour

.dv_default_styles <- c("default", "volleymetrics", "german")

#' Default attack combination codes table
#' @param data_type string: "indoor", "beach"
#' @param style string: conventions "default", "volleymetrics", "german"
#' @param simplified logical: if `TRUE`, just the most common ones (indoor only)
#'
#' @return A tibble
#'
#' @export
dv_default_attack_combos <- function(data_type = "indoor", style = "default", simplified = TRUE) {
    ##data_type <- match.arg(data_type, c("indoor", "beach"))
    data_type <- match.arg(data_type, c("indoor", "beach"))
    style <- match.arg(style, .dv_default_styles)
    out <- if (data_type == "indoor") {
               tribble(~code, ~attacker_position, ~side, ~type, ~description, ~X6, ~colour, ~start_coordinate, ~set_type,
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
           } else {
               ## sx <- list(FL = 0.6, L = 1.2, M = 2, R = 2.8, FR = 3.4) ## x-coords for far left, left, middle, right, far right
               ## sy <- list(X = 3.4, V = 2.8, On2 = 3.35) ## y coords for standard, away (off the net), on2
               tribble(~code, ~attacker_position, ~side, ~type, ~description, ~X6, ~colour, ~start_coordinate, ~set_type,
                       "X1", 4, "R", "M", "Standard far left", NA, "#FF0000", 4913, "-", ## dv_xy2index(sx$FL, sy$X)
                       "X2", 4, "R", "M", "Standard left", NA, "#FF0000", 4929, "-", ## dv_xy2index(sx$L, sy$X)
                       "X3", 3, "C", "M", "Standard middle", NA, "#FF0000", 4951, "-", ## dv_xy2index(sx$M, sy$X)
                       "X4", 2, "L", "M", "Standard right", NA, "#FF0000", 4972, "-", ## dv_xy2index(sx$R, sy$X)
                       "X5", 2, "L", "M", "Satndard far right", NA, "#FF0000", 4988, "-", ## dv_xy2index(sx$FR, sy$X)
                       "V1", 4, "R", "H", "Away far left", NA, "#00FF00", 4113, "-", ## dv_xy2index(sx$FL, sy$V)
                       "V2", 4, "R", "H", "Away left", NA, "#00FF00", 4129, "-", ## dv_xy2index(sx$L, sy$V)
                       "V3", 3, "C", "H", "Away middle", NA, "#00FF00", 4151, "-", ## dv_xy2index(sx$M, sy$V)
                       "V4", 2, "L", "H", "Away right", NA, "#00FF00", 4172, "-", ## dv_xy2index(sx$R, sy$V)
                       "V5", 2, "L", "H", "Away far right", NA, "#00FF00", 4188, "-", ## dv_xy2index(sx$FR, sy$V)
                       "@on2@1", 4, "C", "O", "On 2 far left", NA, "#0000FF", 4813, "-", ## dv_xy2index(sx$FL, sy$On2)
                       "@on2@2", 4, "C", "O", "On 2 left", NA, "#0000FF", 4829, "-", ## dv_xy2index(sx$L, sy$On2)
                       "@on2@3", 3, "C", "O", "On 2 middle", NA, "#0000FF", 4851, "-", ## dv_xy2index(sx$M, sy$On2)
                       "@on2@4", 2, "C", "O", "On 2 right", NA, "#0000FF", 4872, "-", ## dv_xy2index(sx$R, sy$On2)
                       "@on2@5", 2, "C", "O", "On 2 far right", NA, "#0000FF", 4888, "-", ## dv_xy2index(sx$FR, sy$On2)
                       "XX", 3, "C", "O", "Attack on opponent freeball", NA, "#0000FF", 4949, "-",
                       ##"C1", 4, "C", "H", "Cross far left", NA, "#00FFFF", 4916 , "-",
                       ##"C2", 4, "C", "H", "Cross left", NA, "#00FFFF", 4834 , "-",
                       ##"C3", 3, "C", "H", "Cross middle", NA, "#00FFFF", 4951 , "-",
                       ##"C4", 2, "C", "H", "Cross right", NA, "#00FFFF", 4865 , "-",
                       ##"C5", 2, "C", "H", "Cross far right", NA, "#00FFFF", 4887 , "-",
                       "L1", 4, "C", "O", "After block far left", NA, "#000000", 4813, "-", ## dv_xy2index(sx$FL, sy$On2)
                       "L2", 4, "C", "O", "After block left", NA, "#000000", 4829, "-", ## dv_xy2index(sx$L, sy$On2)
                       "L3", 3, "C", "O", "After block middle", NA, "#000000", 4851, "-", ## dv_xy2index(sx$M, sy$On2)
                       "L4", 2, "C", "O", "After block right", NA, "#000000", 4872, "-", ## dv_xy2index(sx$R, sy$On2)
                       "L5", 2, "C", "O", "After block far right", NA, "#000000", 4888, "-", ## dv_xy2index(sx$FR, sy$On2)
                       "P1", 4, "C", "Q", "Quick far left", NA, "#FF00FF", 4913, "-", ## dv_xy2index(sx$FL, sy$X)
                       "P2", 4, "C", "Q", "Quick left", NA, "#FF00FF", 4929, "-", ## dv_xy2index(sx$L, sy$X)
                       "P3", 3, "C", "Q", "Quick middle", NA, "#FF00FF", 4951, "-", ## dv_xy2index(sx$M, sy$X)
                       "P4", 2, "C", "Q", "Quick right", NA, "#FF00FF", 4972, "-", ## dv_xy2index(sx$R, sy$X)
                       "P5", 2, "C", "Q", "Quick far right", NA, "#FF00FF", 4988, "-") %>% ## dv_xy2index(sx$FR, sy$X)
                   dplyr::mutate(code = sub("@on2@", if (style == "usa") "C" else "Z", .data$code))
           }

    out$X10 <- out$X11 <- NA ## some other, unpopulated columns
    if (isTRUE(simplified) && data_type == "indoor") {
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
#' @param style string: conventions "default", "volleymetrics", "german"
#' @return A tibble
#'
#' @export
dv_default_setter_calls <- function(data_type = "indoor", style = "default") {
    data_type <- match.arg(data_type, c("indoor", "beach"))
    style <- match.arg(style, .dv_default_styles)
    if (data_type == "beach") {
        tibble(code = character(), X2 = logical(), description = character(), X4 = logical(), colour = character(), start_coordinate = integer(),
               mid_coordinate = integer(), end_coordinate = integer(), path = character(), path_colour = character(), X11 = logical())
    } else {
        tribble(~code, ~X2, ~description, ~X4, ~colour, ~start_coordinate, ~mid_coordinate, ~end_coordinate, ~path, ~path_colour, ~X11,
                "K1", NA, "Front Quick", NA, 16711680L, 3949L, 4454L, 4958L, NA_character_, NA_integer_, NA,
                "K2", NA, "Back Quick", NA, 16711680L, 3864L, 4278L, 4974L, NA_character_, NA_integer_, NA,
                "K7", NA, "Seven", NA, 16711680L, 3923L, 4426L, 4930L, NA_character_, NA_integer_, NA,
                "KC", NA, "Quick in 3", NA, 16711680L, 3849L, 4449L, 5049L, NA_character_, NA_integer_, NA,
                "KM", NA, "Shifted to 2", NA, 16711680L, 0L, 0L, 0L, "4924,5524,5530,6332,6312,5012,5024", 12632256L, NA,
                "KP", NA, "Shifted to 4", NA, 16711680L, 0L, 0L, 0L, "5457,5057,5557,5552,6352,6364,5377,5077,5058,5058", 12632256L, NA,
                "KE", NA, "No First Tempo", NA, 0L, 0L, 0L, 0L, "5858,5826,6426,6458,6458", 12632256L, NA)
    }
}

#' Default winning symbols table
#'
#' @param data_type string: "indoor", "beach"
#' @param style string: conventions "default", "volleymetrics", "german"
#' @return A tibble
#'
#' @export
dv_default_winning_symbols <- function(data_type = "indoor", style = "default") {
    data_type <- match.arg(data_type, c("indoor", "beach"))
    style <- match.arg(style, .dv_default_styles)
    out <- tribble(~skill, ~win_lose, ~code,
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
    if (style == "volleymetrics") {
        out %>% dplyr::filter(!(.data$skill == "B" & .data$code == "/")) %>% ## poor block, not a loss
            bind_rows(tibble(skill = "E", win_lose = "L", code = "/")) ## E/ is a reach
    } else {
        out
    }
}

#' Default scouting (type and evaluation for each skill) table
#'
#' @param data_type string: "indoor", "beach"
#' @param style string: conventions "default", "volleymetrics", "german"
#' @return A tibble
#'
#' @export
dv_default_scouting_table <- function(data_type = "indoor", style = "default") {
    data_type <- match.arg(data_type, c("indoor", "beach"))
    style <- match.arg(style, .dv_default_styles)
    tribble(~skill, ~default_skill, ~skill_type, ~evaluation_code,
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
#' @param style string: conventions "default", "volleymetrics", "german"
#' @return A tibble
#'
#' @export
dv_default_skill_subtypes <- function(data_type = "indoor", style = "default") {
    data_type <- match.arg(data_type, c("indoor", "beach"))
    style <- match.arg(style, .dv_default_styles)
    if (data_type == "indoor") {
        tribble(~skill, ~skill_subtype_code, ~skill_subtype,
                "Attack", "H", "Hard spike",
                "Attack", "P", "Soft spike/topspin",
                "Attack", "T", "Tip",
                "Block", "A", "Block assist",
                "Block", "T", "Block attempt",
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
                "Dig", "S", "On spike",
                "Dig", "C", "Spike cover",
                "Dig", "B", "After block",
                "Dig", "E", "Emergency",
                "Dig", "T", "Tip",
                "Dig", "P", "Soft spike",
                "Dig", "H", "Hard spike")
    } else {
        tribble(~skill, ~skill_subtype_code, ~skill_subtype,
                "Attack", "H", "Power",
                "Attack", "P", "Shot",
                "Attack", "T", "Poke",
                "Block", "A", "Block assist",
                "Block", "T", "Block attempt",
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
                ## O and U were custom codes used by some beach scouts prior to DV4
                "Set", "O", "Hand set",
                "Set", "U", "Bump set",
                "Dig", "S", "On spike",
                "Dig", "C", "Spike cover",
                "Dig", "B", "After block",
                "Dig", "E", "Emergency",
                "Dig", "T", "Poke",
                "Dig", "P", "Shot",
                "Dig", "H", "Power")
    }
}


#' Default skill_type (tempo) table
#'
#' @param data_type string: "indoor", "beach"
#' @param style string: conventions "default", "volleymetrics", "german"
#' @return A tibble
#'
#' @export
dv_default_skill_types <- function(data_type = "indoor", style = "default") {
    data_type <- match.arg(data_type, c("indoor", "beach"))
    style <- match.arg(style, .dv_default_styles)
    rs <- if (style == "volleymetrics") {
              if (data_type == "beach") {
                  tribble(~skill, ~skill_type_code, ~skill_type,
                          "Serve", "Q", "Jump serve",
                          "Serve", "M", "Jump-float serve",
                          "Serve", "N", "Hybrid serve",
                          "Serve", "T", "Float serve", ## "float near" from the service line
                          "Serve", "H", "Float serve", ## "float far" from the service line
                          "Serve", "U", "Underarm serve")
              } else {
                  ## indoor
                  tribble(~skill, ~skill_type_code, ~skill_type,
                          "Serve", "Q", "Jump serve",
                          "Serve", "M", "Jump-float serve",
                          "Serve", "H", "Float serve", ## "float far" from the service line
                          "Serve", "T", "Float serve") ## "float near" from the service line
              }
          } else if (grepl("beach", data_type)) {
              tribble(~skill, ~skill_type_code, ~skill_type,
                      "Serve", "Q", "Jump serve",
                      "Serve", "T", "Jump-float serve",
                      "Serve", "M", "Jump-float serve",
                      "Serve", "H", "Standing serve")
          } else {
              ## standard indoor
              ## also perhaps N = hybrid
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
    bind_rows(rs,
              rs %>% mutate(skill = "Reception", skill_type = paste0(.data$skill_type, " reception")),
              abde,
              abde %>% mutate(skill = "Block", skill_type = sub("attack", "block", .data$skill_type)),
              abde %>% mutate(skill = "Dig", skill_type = sub("attack", "dig", .data$skill_type)),
              abde %>% mutate(skill = "Set", skill_type = sub("attack", "set", .data$skill_type)),
              tibble(skill = "Freeball", skill_type_code = "H", skill_type = "High freeball"))

}

#' Default skill evaluation table
#'
#' @param data_type string: "indoor", "beach"
#' @param style string: conventions "default", "volleymetrics", "german"
#' @return A tibble
#'
#' @export
dv_default_skill_evaluations <- function(data_type = "indoor", style = "default") {
    data_type <- match.arg(data_type, c("indoor", "beach"))
    style <- match.arg(style, .dv_default_styles)
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
                               .data$skill == "Block" & .data$evaluation_code == "/" ~ "Poor, opposition to replay",
                               .data$skill == "Block" & .data$evaluation_code == "!" ~ "Poor, blocking team cannot recover", ## negative block, unplayable to our side
                               .data$skill == "Block" & .data$evaluation_code == "-" ~ "Poor block", ## negative block touch, either back to opposition or poor on our side
                               .data$skill == "Block" & .data$evaluation_code == "+" ~ "Positive block", ## positive block touch, either to our defense or difficult for opposition
                               .data$skill == "Dig" & .data$evaluation_code == "/" ~ "Positive block cover", ## D/ is block cover that gives attacking team a chance to re-attack
                               .data$skill == "Dig" & .data$evaluation_code == "!" ~ "Poor block cover", ## block cover that does not give attacking team a chance to re-attack or is an error
                               .data$skill == "Set" & .data$evaluation_code == "/" ~ "Error (reach over net)", ## E/ is a reach
                               TRUE ~ .data$evaluation))
    } else if (style == "german") {
        ## swap B= Error and B/ Invasion
        out <- mutate(out, evaluation = case_when(.data$skill=="Block" & .data$evaluation_code == "/" ~ "Error",
                                                  .data$skill=="Block" & .data$evaluation_code == "=" ~ "Invasion",
                                                  TRUE ~ .data$evaluation))
    }
    out
}


#' Default number of players table
#'
#' @param data_type string: "indoor", "beach"
#' @param style string: conventions "default", "volleymetrics", "german"
#' @return A tibble
#'
#' @export
dv_default_num_players <- function(data_type = "indoor", style = "default") {
    data_type <- match.arg(data_type, c("indoor", "beach"))
    style <- match.arg(style, .dv_default_styles)
    if (data_type == "beach") {
        if (style == "volleymetrics") {
            ## TODO check
            tribble(~skill, ~num_players_code, ~num_players,
                    "Attack", 0L, "No block",
                    "Attack", 1L, "Line block",
                    "Attack", 2L, "Crosscourt block",
                    "Attack", 3L, "Drop", ## no block, blocker defends instead
                    "Block", 0L, "No block",
                    "Block", 1L, "Line block",
                    "Block", 2L, "Crosscourt block",
                    "Block", 3L, "Drop") ## no block, blocker defends instead
        } else {
            tribble(~skill, ~num_players_code, ~num_players,
                    "Attack", 0L, "No block",
                    "Attack", 1L, "Line block",
                    "Attack", 2L, "Crosscourt block",
                    "Attack", 3L, "Block jumps to line",
                    "Attack", 4L, "Block jumps to crosscourt",
                    "Block", 0L, "No block",
                    "Block", 1L, "Line block",
                    "Block", 2L, "Crosscourt block",
                    "Block", 3L, "Block jumps to line",
                    "Block", 4L, "Block jumps to crosscourt")
        }
    } else {
        ## indoor
        tribble(~skill, ~num_players_code, ~num_players,
                "Attack", 0L, "No block",
                "Attack", 1L, "1 player block",
                "Attack", 2L, "2 player block",
                "Attack", 3L, "3 player block",
                "Attack", 4L, "Hole block",
                "Block", 0L, "No block",
                "Block", 1L, "1 player block",
                "Block", 2L, "2 player block",
                "Block", 3L, "3 player block",
                "Block", 4L, "Hole block",
                "Reception", 1L, "Two players receiving, the player on left receives",
                "Reception", 2L, "Two players receiving, the player on right receives",
                "Reception", 3L, "Three players receiving, the player on left receives",
                "Reception", 4L, "Three players receiving, the player in center receives",
                "Reception", 5L, "Three players receiving, the player on right receives",
                "Reception", 6L, "Four players receiving, the player on left receives",
                "Reception", 7L, "Four players receiving, the player on center-left receives",
                "Reception", 8L, "Four players receiving, the player on center-right receives",
                "Reception", 9L, "Four players receiving, the player on right receives")
    }
}



#' Default special codes table
#'
#' @param data_type string: "indoor", "beach"
#' @param style string: conventions "default", "volleymetrics", "german"
#' @return A tibble
#'
#' @export
dv_default_special_codes <- function(data_type = "indoor", style = "default") {
    data_type <- match.arg(data_type, c("indoor", "beach"))
    style <- match.arg(style, .dv_default_styles)
    ## entries with NA evaluation only apply to special codes that don't meet the specific evaluation entries for the same skill
    tribble(~skill, ~evaluation, ~special_code, ~special,
            "Attack", "Error", "S", "Attack out - side",
            "Attack", "Error", "O", "Attack out - long",
            "Attack", "Error", "N", "Attack in net",
            "Attack", "Error", "I", "Net contact",
            "Attack", "Error", "Z", "Referee call",
            "Attack", "Error", "A", "Antenna",
            "Attack", "Winning attack", "S", "Block out - side",
            "Attack", "Winning attack", "O", "Block out - long",
            "Attack", "Winning attack", "F", "Block on floor",
            "Attack", "Winning attack", "X", "Direct on floor",
            "Attack", "Winning attack", "N", "Let",
            "Attack", NA_character_, "C", "Block control",
            "Attack", NA_character_, "N", "Let",
            "Block", NA_character_, "S", "Ball out - side",
            "Block", NA_character_, "O", "Ball out - long",
            "Block", NA_character_, "F", "Ball on floor",
            "Block", NA_character_, "X", "Between hands",
            "Block", NA_character_, "N", "Hands - net",
            "Block", NA_character_, "I", "Net contact",
            "Block", NA_character_, "A", "Antenna",
            "Block", NA_character_, "P", "No jump",
            "Block", NA_character_, "T", "Position error",
            "Block", NA_character_, "Z", "Referee call",
            "Reception", NA_character_, "U", "Unplayable",
            "Reception", NA_character_, "X", "Body error",
            "Reception", NA_character_, "P", "Position error",
            "Reception", NA_character_, "Z", "Referee call",
            "Reception", NA_character_, "E", "Lack of effort",
            "Freeball", NA_character_, "U", "Unplayable",
            "Freeball", NA_character_, "X", "Body error",
            "Freeball", NA_character_, "P", "Position error",
            "Freeball", NA_character_, "Z", "Referee call",
            "Freeball", NA_character_, "E", "Lack of effort", ## NB DV doesn't actually define 'Lack of effort' for freeball
            "Dig", NA_character_, "U", "Unplayable",
            "Dig", NA_character_, "X", "Body error",
            "Dig", NA_character_, "P", "Position error",
            "Dig", NA_character_, "Z", "Referee call",
            "Dig", NA_character_, "F", "Ball on floor",
            "Dig", NA_character_, "O", "Ball out",
            "Dig", NA_character_, "E", "Lack of effort",
            "Set", NA_character_, "U", "Cannot be hit",
            "Set", NA_character_, "I", "Net touch",
            "Set", NA_character_, "Z", "Referee call",
            "Serve", "Error", "O", "Ball out - long",
            "Serve", "Error", "L", "Ball out - left",
            "Serve", "Error", "R", "Ball out - right",
            "Serve", "Error", "N", "Ball in net",
            "Serve", "Error", "Z", "Referee call",
            "Serve", NA_character_, "N", "Let")
}
