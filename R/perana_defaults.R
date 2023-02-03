#' Lookup tables for Perana Sports database codes
#'
#' @param data_type string: "indoor" or "beach"
#' @return A tibble
#'
#' @export
pv_default_eventtypes <- function(data_type) {
    if (grepl("beach", tolower(data_type))) {
        tribble(~skill, ~eventtype,
                "Serve", 1L,
                "Reception", 2L,
                "Set", 3L,
                "Attack", 4L,
                "Block", 5L,
                "Dig", 6L,
                "Cover", 7L,
                "CoachTag", 8L,
                "EndOfSet", 9L,
                "TransitionSO", 10L,
                "TransitionPoint", 11L,
                "PointWonServe", 12L,
                "PointLostServe", 13L,
                "PointWonReceive", 14L,
                "PointLostReceive", 15L,
                "iCodaAttack", 16L,
                "iCodaDefense", 17L,
                "CodeError", 18L,
                "OppositionError", 19L,
                "Set", 20L, ## set, but not sure why. Only in one match?
                "SpeedServe", 30L,
                "Serve", 401L, ## SkillOppositionServe
                "Attack", 404L, ## SkillOppositionSpike
                "OppositionScore", 100L,
                "OppositionError", 101L,
                "OppositionHitKill", 102L,
                "OppositionHitError", 103L,
                "OppositionServeError", 104L,
                "OppositionServeAce", 105L,
                "SkillCommentary", 200L,
                "Timeout", 201L,
                "Technical timeout", 202L,
                "Substitution", 250L)
    } else if (grepl("indoor", tolower(data_type))) {
        ## NB this not actually used by the peranavolley package (yet)
        tribble(~skill, ~eventtype,
                "Serve", 1L,
                "Reception", 2L,
                "Set", 3L,
                "Attack", 4L,
                "Block", 5L,
                "Dig", 6L,
                "Freeball", 7L)
    } else {
        stop("unrecognised data_type: ", data_type)
    }
}

#' Define eventgrade interpretations
#'
#' Define the interpretation of eventgrades associated with events.
#'
#' @param data_type string: "indoor" or "beach"
#' @return a tibble with columns "skill", "eventgrade", "evaluation_code" (the equivalent DataVolley code, if there is one), "evaluation", and "win_loss"
#'
#' @examples
#' pv_default_eventgrades("beach")
#'
#' @export
pv_default_eventgrades <- function(data_type) {
    if (grepl("beach", tolower(data_type))) {

        ## todo, we have attack, reception, and dig but ps use spike, pass, and defense
        tribble(~skill, ~eventgrade, ~evaluation_code, ~evaluation, ~win_loss,
                "Serve", 0L, "=", "Error", -1L,
                "Serve", 1L, "-", "Negative, opponent free attack", 0L,
##                "Serve", 2L, "+", "Positive, opponent some attack", 0L,
                "Serve", 2L, "!", "OK, no first tempo possible", 0L,
                "Serve", 3L, "#", "Ace", 1L,
                "Serve", 4L, "+", "Positive, opponent some attack", 0L,
                "Reception", 0L, "=", "Error", -1L,
                "Reception", 1L, "-/", "Negative/poor pass", 0L, ## highball-only, or no attack at all (only freeball)
                ##"Reception", 1L, "-", "Negative, limited attack", 0L,
##                "Reception", 2L, "+", "Positive, attack", 0L,
##                "Reception", 3L, "#", "Perfect pass", 0L,
                "Reception", 2L, "!", "OK, no first tempo possible", 0L,
                "Reception", 3L, "#+", "Perfect/positive pass", 0L,
                "Attack", 0L, "=", "Error", -1L,
                ##"Attack", "/", "/", "Blocked", -1L,
                "Attack", 1L, "-", "Poor, easily dug", 0L,
                ##"Attack", "!", "!", "Blocked for reattack", 0L,
                "Attack", 2L, "+", "Positive, good attack", 0L,
                "Attack", 3L, "#", "Winning attack", 1L,
                "Block", 0L, "/", "Invasion", -1L, ## net touch or invasion. Note that block errors by convention in DV are kills off the block
                ##"Block", "/", "/", "Invasion",
                "Block", 1L, "+", "Positive, block touch", 0L,
                "Block", 2L, "#", "Winning block", 0L, ## solo block
                "Block", 3L, "#", "Winning block", 1L, ## multiplayer block
                ##"Block", "!", "!", "Poor, opposition to replay", 0L,
                "Dig", 0L, "=", "Error", -1L,
                ##"Dig", "/", "/", "Ball directly back over net", 0L,
                "Dig", 1L, "-", "No structured attack possible", 0L,
                "Dig", 2L, "+", "Good dig", 0L,
                "Dig", 3L, "#", "Perfect dig", 0L,
                ##"Dig", "!", "!", "OK, no first tempo possible", 0L,
                "Set", 0L, "=", "Error", -1L,
                "Set", 1L, "-", "Poor", 0L,
                "Set", 2L, "+", "Positive", 0L,
                "Set", 3L, "#", "Perfect", 0L)
        ##"Freeball", 0L, "=", "Error", -1L,
        ##"Freeball", 1L, "-", "Poor", 0L,
        ##"Freeball", 2L, "+", "Good", 0L,
        ##"Freeball", 3L, "#", "Perfect", 0L)
    } else if (grepl("indoor", tolower(data_type))) {
        tribble(~skill, ~eventgrade, ~evaluation_code, ~evaluation, ~win_loss,
            "Serve", 0L, "=", "Error", -1L,
            "Serve", 1L, "-", "Negative, opponent free attack", 0L, ## opp perfect pass
            "Serve", 2L, "!", "OK, no first tempo possible", 0L,
            "Serve", 3L, "#", "Ace", 1L,
            "Serve", 4L, "+", "Positive, opponent some attack", 0L,
            "Pass", 0L, "=", "Error", -1L,
            "Pass", 1L, "-/", "Negative/poor pass", 0L, ## highball-only, or no attack at all (only freeball)
            "Pass", 2L, "!", "OK, no first tempo possible", 0L,
            "Pass", 3L, "#+", "Perfect/positive pass", 0L,
            "Spike", 0L, "=", "Error", -1L,
            "Spike", 1L, "~", "Spike in play", 0L,
            "Spike", 3L, "#", "Winning attack", 1L,
            "Block", 0L, "/", "Invasion", -1L, ## net touch or invasion. Note that block errors by convention in DV are kills off the block
            "Block", 1L, "+", "Positive, block touch", 0L,
            "Block", 2L, "#", "Winning block", 1L, ## solo block
            "Block", 3L, "#", "Winning block", 1L, ## multiplayer block
            "Defense", 0L, "=", "Error", -1L,
            "Defense", 1L, "-/", "Negative/poor dig", 0L, ## highball-only, or no attack at all (only freeball)
            "Defense", 2L, "!", "OK, no first tempo possible", 0L,
            "Defense", 3L, "#+", "Perfect/positive dig", 0L,
            "Set", 0L, "=", "Error", -1L,
            "Set", 1L, NA_character_, "Set in play", 0L,
            "Set", 2L, NA_character_, "Assist", 0L)##,
            ##"Freeball", 0L, "Error", -1L,
            ##"Freeball", 1L, "Poor", 0L,
            ##"Freeball", 2L, "Good", 0L,
            ##"Freeball", 3L, "Perfect", 0L)
    } else {
        stop("unrecognised data_type: ", data_type)
    }
}

#' Define errortype interpretations
#'
#' Define the interpretation of errortypes associated with events.
#'
#' @param data_type string: "indoor" or "beach"
#' @return a tibble with columns "skill", "errortype", and "evaluation"
#'
#' @examples
#' pv_default_errortypes("beach")
#'
#' @export
pv_default_errortypes <- function(data_type) {
    if (grepl("beach", tolower(data_type))) {
        ## NOTE not sure that serve error types are relevant for beach
        tribble(~skill, ~errortype, ~interpretation,
                "Attack", 0L, NA_character_, ## not recorded
                "Attack", 1L, "Attack out",
                "Attack", 2L, "Blocked",
                "Attack", 3L, "Attack in net",
                "Attack", 4L, "Net contact",

                "Serve", 0L, NA_character_, ## not recorded
                "Serve", 1L, "Serve fault",
                ##"Serve", 1L, "Ball in net",
                ##"Serve", 2L, "Ball out - long",
                ##"Serve", 3L, "Ball out - side",

                "Reception", 0L, NA_character_, ## not recorded
                "Reception", 1L, "Reception fault",

                "Set", 0L, NA_character_, ## not recorded
                "Set", 1L, "Set fault",
                "Set", 2L, "Unknown set fault",

                "Block", 0L, NA_character_, ## not recorded
                "Block", 1L, "Block fault", ## see subevent for fault type

                "Dig", 0L, NA_character_, ## not recorded
                "Dig", 1L, "Dig fault"
                )
    } else if (grepl("indoor", tolower(data_type))) {
        tribble(~skill, ~errortype, ~interpretation,
                "Spike", 0L, NA_character_, ##"No error"
                "Spike", 1L, "Attack out",
                "Spike", 2L, "Blocked",
                "Spike", 3L, "Attack in net",
                "Spike", 4L, "Net contact",

                "Serve", 0L, NA_character_, ## not an error
                "Serve", 1L, "Ball in net",
                "Serve", 2L, "Ball out - long",
                "Serve", 3L, "Ball out - side"
                )
    } else {
        stop("unrecognised data_type: ", data_type)
    }
}

#' Define subevent interpretations
#'
#' Define the interpretation of subevents associated with events
#'
#' @param data_type string: "indoor" or "beach"
#' @return a tibble with columns "skill", "subevent", and "interpretation"
#'
#' @examples
#' pv_default_subevents("beach")
#'
#' @export
pv_default_subevents <- function(data_type) {
    if (grepl("beach", tolower(data_type))) {
        tribble(~skill, ~subevent, ~interpretation,
                "Serve", 0L, "Unknown serve type",
                "Serve", 1L, "Jump serve",
                "Serve", 2L, "Jump-float serve",
                "Serve", 3L, "Float serve",
                "Serve", 4L, "Skyball serve",

                "Reception", 0L, "Unknown reception type",
                "Reception", 1L, "Jump reception",
                "Reception", 2L, "Jump-float reception",
                "Reception", 3L, "Float reception",
                "Reception", 4L, "Skyball reception",

                "Set", 1L, "Over set",
                "Set", 2L, "Double contact",
                "Set", 3L, "Catch",
                "Set", 4L, "Net touch",
                "Set", 5L, "Referee call",

                ##"Attack", 0L, "Unknown attack type", ## unspecified
                "Attack", 1L, "Power",
                "Attack", 2L, "Shot",
                "Attack", 3L, "Freeball",
                "Attack", 4L, "Poke",
                "Attack", 5L, "On 2",
                "Attack", 6L, "Flat spike",

                ##"Block", 0L, "Unknown block type", ## unspecified
                "Block", 1L, "Block on power spike",
                "Block", 2L, "Block on shot",
                "Block", 3L, "Block on freeball",
                "Block", 4L, "Block on poke",
                "Block", 5L, "Block on 2",
                "Block", 6L, "Block on flat spike",

                ##"Dig", 0L, "Unknown dig type", ## unspecified
                "Dig", 1L, "Dig on power spike",
                "Dig", 2L, "Dig on shot",
                "Dig", 3L, "Dig on freeball",
                "Dig", 4L, "Dig on poke",
                "Dig", 5L, "Dig on 2",
                "Dig", 6L, "Dig on flat spike",

                ## ???
##                "Freeball", 0L, "",
##                "Freeball", 1L, "",
##                "Freeball", 2L, "",
##                "Freeball", 3L, "",
##                "Freeball", 4L, "",
##                "Freeball", 5L, "",
                )


## case_when(skill %eq% "Timeout" & ZSUBEVENT %eq% 1 ~ "Technical timeout",
##           skill %eq% "Timeout" & ZSUBEVENT %eq% 2 ~ "Medical timeout",
##           skill %eq% "Timeout" & ZSUBEVENT %in% c(3, 4) ~ "Timeout",

##           skill %eq% "Timeout" & ZSUBEVENT %eq% 3 ~ home_team,
##           skill %eq% "Timeout" & ZSUBEVENT %eq% 4 ~ visiting_team,


    } else if (grepl("indoor", tolower(data_type))) {
        tribble(~skill, ~subevent, ~interpretation,
                "Spike", 0L, "Hard spike", ## unspecified, defaults to hard spike
                "Spike", 1L, "Soft spike/topspin",
                "Spike", 2L, "Setter tip",
                "Spike", 3L, "Hard spike",
                "Spike", 4L, "Spike off the block",
                "Serve", 0L, "Unknown serve type",
                "Serve", 1L, "Jump serve",
                "Serve", 2L, "Jump-float serve",
                "Serve", 3L, "Float serve",
                "Serve", 4L, "Topspin serve",
                "Reception", 0L, "Unknown reception type",
                "Reception", 1L, "Jump reception",
                "Reception", 2L, "Jump-float reception",
                "Reception", 3L, "Float reception",
                "Reception", 4L, "Topspin reception"
                )
    } else {
        stop("unrecognised data_type: ", data_type)
    }
}

#' Define subevent2 interpretations
#'
#' Define the interpretation of subevents2 associated with events
#'
#' @param data_type string: "indoor" or "beach"
#' @return a tibble with columns "skill", "subevent2", and "interpretation"
#'
#' @examples
#' pv_default_subevents("beach")
#'
#' @export
pv_default_subevents2 <- function(data_type) {
    if (grepl("beach", tolower(data_type))) {
        tribble(~skill, ~subevent2, ~interpretation,
                ##"Attack", 0L, "Unknown block",
                "Attack", 1L, "Line block",
                "Attack", 2L, "Crosscourt block",
                "Attack", 3L, "No block",
                "Attack", 4L, "Line drop", ##Block jumps to line",
                "Attack", 5L, "Crosscourt drop", ##Block jumps to crosscourt",

                "Block", 1L, "Line block",
                "Block", 2L, "Crosscourt block",
                "Block", 3L, "No block",
                "Block", 4L, "Line drop", ## no block, blocker defends line
                "Block", 5L, "Crosscourt drop", ## no block, blocker defends cross

                "Set", 0L, NA_character_, ## not recorded
                "Set", 1L, "???",
                "Set", 2L, "???",
                "Set", 3L, "???",

                "Cover", 1L, "???",
                "Cover", 2L, "???",

                "Dig", 1L, "Dig with line block",
                "Dig", 2L, "Dig with crosscourt block",
                "Dig", 3L, "Dig with no block",
                "Dig", 4L, "Dig with line drop",
                "Dig", 5L, "Dig with crosscourt drop",

                "Reception", 0L, "Not recorded???",
                "Reception", 1L, "???",
                "Reception", 2L, "???",
                "Reception", 3L, "???",

                "Serve", 0L, "Not recorded???",
                "Serve", 1L, "???",
                "Serve", 2L, "???",
                "Serve", 3L, "???"
                )
    } else {
        stop("no subevent2 for indoor")
    }
}
