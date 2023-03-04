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
