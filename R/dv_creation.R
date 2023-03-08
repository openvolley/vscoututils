## in these functions, issue errors for critical problems, warnings for immediate/informative messages, but issues that might matter later are prppagated into the dvmessages attribute of the returned object

#' Create the `file_meta` component of a datavolley object
#'
#' @param file_type string: "indoor", "beach"
#' @param date_format string: preferred date format
#' @param generator_day datetime: date and time of file creation
#' @param generator_idp,generator_prg,generator_release,generator_version,generator_name string: information about the software that generated the file. If missing, defaults will be used
#'
#' @return A tibble
#'
#' @export
dv_create_file_meta <- function(file_type = "indoor", date_format = "%Y/%m/%d", generator_day, generator_idp, generator_prg, generator_release, generator_version, generator_name) {
    tdnow <- time_utc()
    tdformat <- function(z) {
        if (is.character(z)) {
            z ## as-is
        } else {
            tryCatch(format(z, paste0(date_format, " %H.%M.%S")), error = function(e) NULL)
        }
    }
    if (missing(generator_day) || is.null(generator_day)) generator_day <- tdformat(tdnow)
    if (missing(generator_idp) || is.null(generator_idp)) generator_idp <- "openvolley"
    if (missing(generator_prg) || is.null(generator_prg)) generator_prg <- "openvolley-R"
    if (missing(generator_release) || is.null(generator_release)) generator_release <- packageVersion("vscoututils")
    if (missing(generator_version) || is.null(generator_version)) generator_version <- ""
    if (missing(generator_name) || is.null(generator_name)) generator_name <- ""
    tibble(fileformat = "2.0",
           generator_day = generator_day, generator_idp = generator_idp, generator_prg = generator_prg, generator_rel = generator_release, generator_ver = generator_version, generator_nam = generator_name,
           lastchange_day = tdformat(tdnow),
           lastchange_idp = "openvolley", lastchange_prg = "openvolley-R", lastchange_rel = packageVersion("vscoututils"), lastchange_ver = "", lastchange_nam = "",
           preferred_date_format = tolower(gsub("[%/]", "", date_format)), file_type = file_type)
}

#' Create the `match` metadata component of a datavolley object
#'
#' @param date Date or datetime: match date and time
#' @param season string: season name
#' @param league string: league name
#' @param phase string: match phase (e.g. "Playoffs", "Finals")
#' @param home_away string: typically "Home" or "Away"
#' @param day_number,match_number numeric: day and match number
#' @param regulation string: "indoor sideout", "indoor rally point" (default), or "beach rally point"
#' @param zones_or_cones string: are attacks being scouted with "zones" or "cones" (or "Z" or "C")
#'
#' @return A tibble
#'
#' @export
dv_create_meta_match <- function(date, season, league, phase, home_away, day_number, match_number, regulation = "indoor rally point", zones_or_cones) {
    regulation <- match.arg(regulation, c("indoor sideout", "indoor rally point", "beach rally point"))
    assert_that(is.string(zones_or_cones))
    zones_or_cones <- substr(toupper(zones_or_cones), 1, 1)
    if (!zones_or_cones %in% c("Z", "C")) stop("zones_or_cones must be one of \"zones\", \"z\", \"cones\", or \"c\"")
    if (missing(date) || is.null(date)) {
        date <-  as.Date(NA)
        time <- NA_character_
    } else {
        time <- tryCatch(lubridate::period(format(date, "%HH %MM %SS")), error = function(e) lubridate::as.period(NA))
        date <- as.Date(date)
    }
    season <- if (missing(season) || is.null(season)) NA_character_ else season
    league <- if (missing(league) || is.null(league)) NA_character_ else league
    phase <- if (missing(phase) || is.null(phase)) NA_character_ else phase
    home_away <- if (missing(home_away) || is.null(home_away)) NA_character_ else home_away
    day_number <- if (missing(day_number) || is.null(day_number)) NA_integer_ else day_number
    match_number <- if (missing(match_number) || is.null(match_number)) NA_integer_ else match_number
    tibble(date = date, time = time, season = season, league = league, phase = phase, home_away = home_away, day_number = day_number,
           match_number = match_number, text_encoding = "UTF-8", regulation = regulation, zones_or_cones = zones_or_cones, X12 = NA)
}

#' Create the `more` metadata component of a datavolley object
#'
#' @param referees,spectators,receipts,city,arena string: match descriptors
#' @param scout string: name of the scout
#'
#' @return A tibble
#'
#' @export
dv_create_meta_more <- function(referees, spectators, receipts, city, arena, scout) {
    referees <- if (missing(referees) || is.null(referees)) NA_character_ else referees
    spectators <- if (missing(spectators) || is.null(spectators)) NA_character_ else spectators
    receipts <- if (missing(receipts) || is.null(receipts)) NA_character_ else receipts
    city <- if (missing(city) || is.null(city)) NA_character_ else city
    arena <- if (missing(arena) || is.null(arena)) NA_character_ else arena
    scout <- if (missing(scout) || is.null(scout)) NA_character_ else scout
    tibble(referees = referees, spectators = spectators, receipts = receipts, city = city,
           arena = arena, scout = scout, X7 = NA, X8 = NA, X9 = NA, X10 = NA, X11 = NA)
}

#' Create the `comments` metadata component of a datavolley object
#'
#' @param summary,match_description,home_coach_comments,visiting_coach_comments string: comments
#'
#' @return A tibble
#'
#' @export
dv_create_meta_comments <- function(summary, match_description, home_coach_comments, visiting_coach_comments) {
    summary <- if (missing(summary) || is.null(summary)) NA_character_ else summary
    match_description <- if (missing(match_description) || is.null(match_description)) NA_character_ else match_description
    home_coach_comments <- if (missing(home_coach_comments) || is.null(home_coach_comments)) NA_character_ else home_coach_comments
    visiting_coach_comments <- if (missing(visiting_coach_comments) || is.null(visiting_coach_comments)) NA_character_ else visiting_coach_comments
    tibble(comment_1 = summary, comment_2 = match_description, comment_3 = home_coach_comments,
           comment_4 = visiting_coach_comments, comment_5 = NA_character_)
}

#' Create the `result` metadata component of a datavolley object
#'
#' Note that the returned object has some NA values. It is expected that the user will call [dv_update_meta()] on the full `datavolley` object, which will update entries in this metadata component.
#'
#' @param home_team_scores integer: vector of home team scores in each set
#' @param visiting_team_scores integer: vector of visiting team scores in each set
#' @param durations integer: vector of set durations (in minutes). If not provided, these will be calculated from video times by [dv_update_meta()]
# @param score_intermediate1,score_intermediate2,score_intermediate3 string: vector of intermediate scores at the three intermediate score checkpoints, typically:
# * when the first team reaches 5, 10, 15 points (beach)
# * when the first team reaches 8, 16, 21 points (indoor sets 1-4)
# * when the first team reaches 5, 10, 12 points (indoor set 5 or golden set)
#'
#' @return A tibble
#'
#' @export
dv_create_meta_result <- function(home_team_scores = NA_integer_, visiting_team_scores = NA_integer_, durations = NA_integer_) {##, score_intermediate1, score_intermediate2, score_intermediate3) {
    assert_that(length(home_team_scores) == length(visiting_team_scores))
    assert_that(length(durations) == 1 || length(durations) == length(home_team_scores))
    ## mostly NAs, to be populated by dv_update_meta
    tibble(played = TRUE,
           score_intermediate1 = NA_character_, score_intermediate2 = NA_character_, score_intermediate3 = NA_character_,
           score = if (!all(is.na(home_team_scores))) paste0(home_team_scores, "-", visiting_team_scores),
           duration = durations,
           X7 = NA,
           score_home_team = home_team_scores, score_visiting_team = visiting_team_scores)
}


#' Create the `teams` metadata component of a datavolley object
#'
#' `dv_create_meta_teams2` provides an alternative parameterization with home and visiting team information specified separately.
#'
#' @param team_ids character: (required) 2-element vector of home and visiting team IDs
#' @param teams character: (required) 2-element vector of home and visiting team names
#' @param sets_won integer: 2-element vector of the number of sets won by the home team and visiting team
#' @param coaches character: 2-element vector of home and visiting team coach names
#' @param assistants character: 2-element vector of home and visiting team assistant coach names
#' @param shirt_colours character: 2-element vector of home and visiting team shirt colours in "#RRGGBB" format
#' @param home_team_id,visiting_team_id string: home and visiting team IDs
#' @param home_team,visiting_team string: home and visiting team names
#' @param home_coach,visiting_coach string: home and visiting coach name
#' @param home_assistant,visiting_assistant string: home and visiting assistant coach name
#' @param home_shirt_colour,visiting_shirt_colour string: home and visiting shirt colours in "#RRGGBB# format
#'
#' @return A tibble
#'
#' @export
dv_create_meta_teams <- function(team_ids, teams, sets_won, coaches, assistants, shirt_colours) {
    assert_that(is.character(team_ids), length(team_ids) == 2, !any(is.na(team_ids)))
    msgs <- c()
    if (team_ids[1] == team_ids[2]) {
        msgs <- "The two team IDs are identical. They will be modified here but this may still cause problems"
        team_ids <- paste(team_ids, c(" (home)", " (visiting)"))
    }
    assert_that(is.character(teams), length(teams) == 2, !any(is.na(teams)))
    if (teams[1] == teams[2]) {
        msgs <- c(msgs, "The two team names are identical. They will be modified here but this may still cause problems")
        teams <- paste(teams, c(" (home)", " (visiting)"))
    }
    if (missing(sets_won) || length(sets_won) != 2) sets_won <- NA_integer_
    if (missing(coaches) || length(coaches) != 2) coaches <- NA_integer_
    if (missing(assistants) || length(assistants) != 2) assistants <- NA_integer_
    if (missing(shirt_colours) || length(shirt_colours) != 2) shirt_colours <- NA_integer_
    out <- tibble(team_id = team_ids, team = teams, sets_won = sets_won, coach = coaches, assistant = assistants, shirt_colour = shirt_colours,
                  X7 = NA, X8 = NA, X9 = NA, X10 = NA,
                  home_away_team = c("*", "a"),
                  won_match = if (any(is.na(sets_won)) || length(sets_won) != 2 || sets_won[1] == sets_won[2]) NA else if (sets_won[1] > sets_won[2]) c(TRUE, FALSE) else c(FALSE, TRUE))
    if (length(msgs) > 0) set_dvmsg(out, tibble(line_number = NA, message = msgs, severity = 1)) else out
}

#'@rdname dv_create_meta_teams
#' @export
dv_create_meta_teams2 <- function(home_team_id, home_team, home_coach, home_assistant, home_shirt_colour,
                                  visiting_team_id, visiting_team, visiting_coach, visiting_assistant, visiting_shirt_colour,
                                  sets_won) {

    dv_create_meta_teams(team_ids = as.character(c(ok_or(home_team_id), ok_or(visiting_team_id))),
                         teams = as.character(c(ok_or(home_team), ok_or(visiting_team))),
                         sets_won = sets_won,
                         coaches = as.character(c(ok_or(home_coach), ok_or(visiting_coach))),
                         assistants = as.character(c(ok_or(home_assistant), ok_or(visiting_assistant))),
                         shirt_colours = as.character(c(ok_or(home_shirt_colour), ok_or(visiting_shirt_colour))))
}

#' Create the `players_h` or `players_v` metadata component of a datavolley object
#'
#' @param players data.frame: the team roster, as a data.frame. `players` must contain the columns `lastname`, `firstname`, and `number`. Columns `player_id`, `role`, `nickname`, `special_role`, `foreign`, and `starting_position_set1` - `starting_position_set5` are optional
#'
#' @return A tibble
#'
#' @export
dv_create_meta_players <- function(players) {
    assert_that(is.data.frame(players))
    req <- c("lastname", "firstname", "number") ## absolutely required
    if (!all(req %in% names(players))) stop("players data.frame is missing columns: ", paste(setdiff(req, names(players)), collapse = ", "))
    if (!"player_id" %in% names(players)) players$player_id <- make_player_id(players$lastname, players$firstname)
    players$player_id <- make.unique(players$player_id)
    if (!"role" %in% names(players)) players$role <- "unknown"
    if (!"nickname" %in% names(players)) players$nickname <- ""
    if (!"special_role" %in% names(players)) players$special_role <- ""
    ## reset liberos and reassign (but keep e.g. "C" captain)
    players$special_role <- gsub("L", "", players$special_role)
    players$special_role[tolower(players$role) %eq% "libero"] <- paste0(players$special_role[tolower(players$role) %eq% "libero"], "L")
    if (!"foreign" %in% names(players)) players$foreign <- FALSE
    for (nm in paste0("starting_position_set", 1:5)) if (!nm %in% names(players)) players[[nm]] <- NA_character_
    temp <- tibble(X1 = 0L,
                   number = players$number,
                   X3 = seq_len(nrow(players)),
                   starting_position_set1 = players$starting_position_set1,
                   starting_position_set2 = players$starting_position_set2,
                   starting_position_set3 = players$starting_position_set3,
                   starting_position_set4 = players$starting_position_set4,
                   starting_position_set5 = players$starting_position_set5,
                   player_id = players$player_id,
                   lastname = players$lastname,
                   firstname = players$firstname,
                   nickname = players$nickname,
                   special_role = toupper(players$special_role),
                   role = tolower(players$role),
                   foreign = players$foreign)
    temp$X16 <- temp$X17 <- temp$X18 <- temp$X19 <- temp$X20 <- temp$X21 <- temp$X22 <- temp$X23 <- NA
    temp$name <- paste(temp$firstname, temp$lastname)
    temp %>% dplyr::arrange(.data$number) %>% mutate(X3 = dplyr::row_number())
}
make_player_id <- function(lastname, firstname) toupper(paste0(substr(lastname, 1, 3), "-", substr(firstname, 1, 3)))

#' Create the `attacks` metadata component of a datavolley object
#'
#' The `attacks` metadata component of a datavolley object describes the attack combination codes
#'
#' @param code character: vector of two-character (uppercase) attack codes. Attack codes must start with C, G, I, J, L, P, V, W, X, Y, or Z
#' @param start_zone integer: vector of start zones
#' @param side character: vector of L, C, R
#' @param tempo character: vector of tempo codes F, H, M, N, O, Q, T, U
#' @param description character: vector of attack descriptions
#' @param colour character: vector of colour codes (maybe) used when plotting, in "#RRGGBB" format
#' @param start_coordinate integer: vector of start locations as single-index coordinates (see e.g. `datavolley:dv_xy2index`)
#' @param target_attacker character: vector of single-character codes giving the target attacker: B (back/right side attacker), C (centre), F (front/left side attacker), P (pipe), S (setter attack)
#'
#' @return A tibble
#'
#' @export
dv_create_meta_attack_combos <- function(code, start_zone, side = NA_character_, tempo, description, colour = NA_character_, start_coordinate = NA_integer_, target_attacker) {
    assert_that(is.character(code), !any(is.na(code)))
    msgs <- c()
    code <- toupper(code)
    allowed <- c("C", "G", "I", "J", "L", "P", "V", "W", "X", "Y", "Z")
    if (!all(substr(code, 1, 1) %in% allowed)) stop("attack combination codes must start with ", paste(allowed, collapse = ", "))
    if (!all(nchar(code) == 2)) stop("attack combination codes must be two letters")
    start_zone <- as.integer(start_zone)
    assert_that(!any(is.na(start_zone)), all(start_zone %in% 1:9))
    unusual <- sort(unique(start_zone[start_zone %in% c(5, 6, 1)]))
    if (length(unusual) > 0) {
        repl <- c(9, 0, 0, 0, 7, 8)[unusual]
        m <- length(unusual) > 1
        msgs <- c(msgs, paste0("start zone", if (m) "s", " ", paste(unusual, collapse = ", "), " ", if (m) "are" else "is", " legal but unusual for attacks, are you sure you did not mean ", paste(repl, collapse = ", "), "?"))
    }
    assert_that(is.character(side))
    side <- toupper(side)
    assert_that(all(side %in% c("R", "L", "C", NA_character_))) ## allow NA pending further checking
    assert_that(is.character(tempo))
    tempo <- toupper(tempo)
    assert_that(all(tempo %in% c("F", "H", "M", "N", "O", "Q", "T", "U")))
    assert_that(is.character(description))
    assert_that(is.character(colour))
    assert_that(is.integer(start_coordinate))
    start_coordinate[which(start_coordinate < 1 | start_coordinate > 10100)] <- NA_integer_
    assert_that(is.character(target_attacker))
    target_attacker <- toupper(target_attacker)
    target_attacker[is.na(target_attacker) | !nzchar(target_attacker)] <- "-"
    assert_that(all(target_attacker %in% c("B", "C", "F", "P", "S", "-")))
    ax <- tibble(code = code, attacker_position = start_zone, side = side, type = tempo, description = description,
           X6 = NA, colour = colour, start_coordinate = start_coordinate, set_type = target_attacker, X10 = NA, X11 = NA)
    if (any(duplicated(ax$code))) {
        msgs <- c(msgs, paste0("ignoring duplicate attack combination codes: ", paste0(unique(ax$code[duplicated(ax$code)]), collapse = ", ")))
        ax <- ax[!duplicated(ax$code), , drop = FALSE]
    }
    if (length(msgs) > 0) set_dvmsg(ax, tibble(line_number = NA, message = msgs, severity = 3)) else ax
}

#' Create the `sets` (setter calls) metadata component of a datavolley object
#'
#' @param code character: vector of two-character (uppercase) setter call codes. Setter call codes must start with K
#' @param description character: vector of setter call descriptions
#' @param colour character: vector of colour codes (maybe) used when plotting, in "#RRGGBB" format
#' @param start_coordinate,mid_coordinate,end_coordinate integer: vector of start/mid/end locations as single-index coordinates (see e.g. `datavolley:dv_xy2index`)
#' @param path character: vector of paths, each a comma-separated list of single-index coordinates
#' @param path_colour character: vector of colour codes in "#RRGGBB" format
#'
#' @return A tibble
#'
#' @export
dv_create_meta_setter_calls <- function(code, description, colour = NA_character_, start_coordinate = NA_integer_, mid_coordinate = NA_integer_, end_coordinate = NA_integer_, path = NA_character_, path_colour = NA_character_) {
    assert_that(is.character(code), !any(is.na(code)))
    msgs <- c()
    code <- toupper(code)
    if (!all(substr(code, 1, 1) %in% c("K"))) stop("attack combination codes must start with K")
    if (!all(nchar(code) == 2)) stop("setter call codes must be two letters")
    assert_that(is.character(description))
    assert_that(is.integer(start_coordinate))
    start_coordinate[which(start_coordinate < 1 | start_coordinate > 10100)] <- NA_integer_
    assert_that(is.integer(mid_coordinate))
    mid_coordinate[which(mid_coordinate < 1 | mid_coordinate > 10100)] <- NA_integer_
    assert_that(is.integer(end_coordinate))
    end_coordinate[which(end_coordinate < 1 | end_coordinate > 10100)] <- NA_integer_
    assert_that(is.character(colour))
    assert_that(is.character(path))
    assert_that(is.character(path_colour))
    sx <- tibble(code = code, X2 = NA, description = description, X4 = NA, colour = colour,
                 start_coordinate = start_coordinate, mid_coordinate = mid_coordinate, end_coordinate = end_coordinate,
                 path = path, path_colour = path_colour, X11 = NA)
    if (any(duplicated(sx$code))) {
        msgs <- c(msgs, paste0("ignoring duplicate setter calls: ", paste0(unique(sx$code[duplicated(sx$code)]), collapse = ", ")))
        sx <- sx[!duplicated(sx$code), , drop = FALSE]
    }
    if (length(msgs) > 0) set_dvmsg(sx, tibble(line_number = NA, message = msgs, severity = 3)) else sx
}

#' Create the `video` metadata component of a datavolley object
#'
#' @param video_file character: path to video file. More than one is allowed, but anything other than the first might be ignored by some functions
#'
#' @return A data.frame
#'
#' @export
dv_create_meta_video <- function(video_file) {
    if (missing(video_file)) video_file <- character()
    assert_that(is.character(video_file))
    video_file <- na.omit(video_file)
    data.frame(camera = if (length(video_file) < 1) character() else paste0("Camera", seq_along(video_file) - 1L), file = video_file)
}

#' Create the `match_id` metadata component of a datavolley object
#'
#' The `match_id` is generated from the `match` and `teams` components of the `mx` object, so ensure that these are fully populated before generating a match_id
#'
#' @param mx list: the `meta` component of a datavolley object
#'
#' @return A string
#'
#' @export
dv_create_meta_match_id <- function(mx) {
    temp <- mx$match[1:8]
    temp$home_team <- mx$teams$team[mx$teams$home_away_team %eq% "*"]
    temp$visiting_team <- mx$teams$team[mx$teams$home_away_team %eq% "a"]
    digest::digest(temp)
}



#' Create metadata component of a datavolley object
#'
#' @param match tibble: as returned by [dv_create_meta_match()]
#' @param more tibble: as returned by [dv_create_meta_more()]
#' @param comments tibble: as returned by [dv_create_meta_comments()]
#' @param result tibble: as returned by [dv_create_meta_result()]
#' @param teams tibble: as returned by [dv_create_meta_teams()]
#' @param players_h tibble: home team table as returned by [dv_create_meta_players()]
#' @param players_v tibble: visiting team table as returned by [dv_create_meta_players()]
#' @param video tibble: as returned by [dv_create_meta_video()]
#' @param attacks tibble: as returned by [dv_create_meta_attack_combos()]
#' @param setter_calls tibble: as returned by [dv_create_meta_setter_calls()]
#' @param winning_symbols tibble: as returned by [dv_default_winning_symbols()]
#'
#' @return A list
#'
#' @examples
#'  dv_create_meta(match = dv_create_meta_match(zones_or_cones = "Z"), more = dv_create_meta_more(),
#'                 comments = dv_create_meta_comments(),
#'                 teams = dv_create_meta_teams(
#'                     team_ids = c("ardv", "badg"), teams = c("Aardvarks", "Badgers")))
#'
#' @export
dv_create_meta <- function(match, more, comments, result, teams, players_h, players_v, video, attacks = dv_default_attack_combos(simplified = TRUE), setter_calls = dv_default_setter_calls(), winning_symbols = dv_default_winning_symbols()) {
    assert_that(is.data.frame(match), nrow(match) == 1)
    if (!match$regulation %in% c("indoor sideout", "indoor rally point", "beach rally point")) stop("regulation: '", match$regulation, "' is unrecognized")
    assert_that(is.data.frame(more), nrow(more) == 1)
    assert_that(is.data.frame(comments), nrow(comments) == 1)
    if (missing(result)) result <- dv_create_meta_result()
    assert_that(is.data.frame(result), nrow(result) == 1)
    assert_that(is.data.frame(teams), nrow(teams) == 2)
    msgs <- c()
    meta <- list(match = match, more = more, comments = comments, result = result, teams = teams)
    ## players
    if (missing(players_h)) players_h <- dv_create_meta_players(tibble(lastname = character(), firstname = character(), number = integer()))
    if (missing(players_v)) players_v <- dv_create_meta_players(tibble(lastname = character(), firstname = character(), number = integer()))
    assert_that(is.data.frame(players_h))
    assert_that(is.data.frame(players_v))
    players_v$X1 <- 1L
    players_v$X3 <- players_v$X3 + nrow(players_h)
    pids <- c(players_h$player_id, players_v$player_id)
    if (any(duplicated(pids))) {
        dup <- pids[duplicated(pids)]
        stop("duplicated player_id(s): ", paste(dup, collapse = ", "), ". If these have been automatically generated you might need to explicitly provide them in the players_h and players_v data.frames")
    }
    roles <- c("libero", "outside", "opposite", "middle", "setter", "unknown")
    if (!all(players_h$role %in% roles)) {
        msgs <- c(msgs, paste0("unknown role(s) in home players list: ", paste(setdiff(players_h$role, roles), collapse = ", "), ". Replacing with 'unknown'"))
        players_h$role[!players_h$role %in% roles] <- "unknown"
    }
    if (!all(players_v$role %in% roles)) {
        msgs <- c(msgs, paste0("unknown role(s) in visiting players list: ", paste(setdiff(players_v$role, roles), collapse = ", "), ". Replacing with 'unknown'"))
        players_v$role[!players_v$role %in% roles] <- "unknown"
    }
    min_n_players <- if (grepl("beach", match$regulation)) 2L else 6L
    if (nrow(players_h) < min_n_players) warning("less than ", min_n_players, " players in home player list")
    if (nrow(players_v) < min_n_players) warning("less than ", min_n_players, " players in visiting player list")
    assert_that(is.data.frame(attacks))
    assert_that(is.data.frame(setter_calls))
    assert_that(is.data.frame(winning_symbols))
    if (missing(video)) video <- dv_create_meta_video()
    assert_that(is.data.frame(video))
    meta <- c(meta, list(players_h = players_h, players_v = players_v, attacks = attacks, sets = setter_calls, winning_symbols = winning_symbols, video = video, filename = ""))
    meta$match_id <- dv_create_meta_match_id(meta)
    if (length(msgs) > 0) set_dvmsg(meta, tibble(line_number = NA, message = msgs, severity = 2)) else meta
}

