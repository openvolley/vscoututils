camel_to_underscore <- function(x) {
    s <- gsub("([a-z0-9])([A-Z])", "\\1_\\L\\2", x, perl = TRUE)
    sub("^(.[a-z])", "\\L\\1", s, perl = TRUE) # make 1st char lower case if second is lower
}
names_to_underscore <- function(x) {
    names(x) <- camel_to_underscore(names(x))
    x
}

check_id <- function(z) {
    varname <- deparse(substitute(z))
    assert_that(is.numeric(z), length(z) == 1, !is.na(z), msg = paste0(varname, " should be a scalar integer"))
    as.integer(z)
}

`%eq%` <- function(x,y) x==y & !is.na(x) & !is.na(y)

## leading zeros on numbers, e.g. jersey numbers
ldz <- function(nn, width = 2) formatC(suppressWarnings(as.integer(nn)), flag = "0", width = width)
## same but forcing NAs, negative numbers, and numbers > 99 to "00"
ldz2 <- function(n, width = 2) {
    z <- suppressWarnings(as.integer(n))
    z[is.na(z) | z < 0 | z > 99] <- 0L
    formatC(z, flag = "0", width = width)
}

## Accumulate messages for later display
## Internal function, not exported
## severity: 1=critical, 2=informative, may lead to misinterpretation of data, 3=minor, esp. those that might have resulted from selective post-processing of combo codes
collect_messages <- function(msgs, msg_text, line_nums, raw_lines, severity, fatal = FALSE) {
    if (missing(line_nums)) line_nums <- NA
    if (missing(raw_lines)) raw_lines <- "[unknown]"
    if (missing(severity)) severity <- NA
    vt <- rep(NA_real_, length(line_nums))
    ##if (!missing(raw_lines)) vt <- video_time_from_raw(raw_lines)
    if (fatal) {
        lnt <- as.character(line_nums)
        lnt[is.na(lnt)] <- "[unknown]"
        txt <- paste0("line ", lnt,": ", msg_text, " (line in file is: \"", raw_lines, "\")")
        if (fatal) stop(paste(txt, collapse = " / "))
    } else {
        msgs[[length(msgs)+1]] <- list(file_line_number = line_nums, video_time = vt, message = msg_text, file_line = unname(raw_lines), severity = severity)
    }
    msgs
}

time_utc <- function() {
    nw <- Sys.time()
    lubridate::tz(nw) <- "UTC"
    round(nw)
}

## check a single-element function parm was provided and was not NULL or all-NA
ok_or <- function(z, or = NA) {
    was_missing <- eval(parse(text = paste0("missing(", deparse(substitute(z)), ")")), envir = parent.frame(n = 1))
    if (was_missing || is.null(z) || is.na(z)) or else z
}

## messages stored as attributes of an object
get_dvmsg <- function(x) attr(x, "dvmessages", exact = TRUE)
set_dvmsg <- function(x, msg) {
    attr(x, "dvmessages") <- msg
    x
}

lead0 <- function(x, width = 2, pad = "0", na = NULL) {
    out <- character(length(x))
    naidx <- rep(FALSE, length(x))
    if (!is.null(na)) {
        naidx <- is.na(x) | !nzchar(x)
        out[naidx] <- na
    }
    out[!naidx] <- stringr::str_pad(as.numeric(x[!naidx]), width = width, pad = pad)
    out
}

dv_guess_data_type <- function(x) {
    tryCatch({
        if ("meta" %in% names(x)) {
            ## is datavolley object
            if (isTRUE(grepl("indoor", x$meta$match$regulation))) {
                "indoor"
            } else if (isTRUE(grepl("beach", x$meta$match$regulation))) {
                "beach"
            }
        } else if ("file_meta" %in% names(x)) {
            if (isTRUE(grepl("indoor", x$file_meta$file_type))) {
                "indoor"
            } else if (isTRUE(grepl("beach", x$file_meta$file_type))) {
                "beach"
            }
        } else if ("home_player_id1" %in% names(x)) {
            ## plays dataframe
            if (!"home_player_id3" %in% names(x)) {
                if ("eventgrade" %in% names(x)) "perana_beach" else "beach"
            } else {
                if ("eventgrade" %in% names(x)) "perana_indoor" else "indoor"
            }
        } else {
            NA_character_
        }
    }, error = function(e) NA_character_)
}
