#' Detect scout file type
#'
#' Returns the general file type (format), but does not differentiate e.g. indoor from beach. Type is based on the file extension and a minimal check of the contents, but without fully parsing the file. For `dv_file_data_type` the type is based only on a minimal check of the contents (provided in `x`)
#'
#' @param filename character: paths to files. Files that do not exist will be classified as "unknown" type
#' @param x character: a character vector with the contents of a scout file
#' @param error_on_unknown logical: if `TRUE` an error will be thrown if an unknown file type is encountered
#'
#' @return Character vector containing:
#' * "dvw"
#' * "vsm"
#' * "psvb"
#' * "hxml"
#' * "unknown"
#'
#' @examples
#' \dontrun{
#'   library(ovdata)
#'   dv_file_type(ovdata_example("190301_kats_beds")) ## dvw
#'   x <- readLines(ovdata_example("190301_kats_beds"), warn = FALSE)
#'   dv_file_data_type(x)
#'
#'   dv_file_type(ovdata_example("clickscout")) ## dvw
#'   dv_file_data_type(readLines(ovdata_example("clickscout"), warn = FALSE))
#'
#'   dv_file_type(ovdata_example("2017_AVL_mens_HEAT_vs_UTSSU")) ## psvb
#'   dv_file_data_type(readLines(ovdata_example("2017_AVL_mens_HEAT_vs_UTSSU"), warn = FALSE))
#'
#'   dv_file_type(tempfile(fileext = ".dvw")) ## unknown (contents not as expected for dvw)
#'   dv_file_data_type(readLines(tempfile(fileext = ".dvw"))) ## "unknown", with warning
#'
#' }
#' @export
dv_file_type <- function(filename, error_on_unknown = FALSE) {
    assert_that(is.character(filename))
    if_is <- function(x, ifis) if (identical(x, ifis)) x else "unknown"
    if (length(filename) == 1) {
        ext <- if (!file.exists(filename)) "" else tolower(tools::file_ext(filename))
        out <- if (!ext %in% c("dvw", "vsm", "xml", "psvb")) {
                   "unknown"
               } else if (ext == "dvw") {
                   chk <- tryCatch({
                       this <- readLines(filename, warn = FALSE, n = 1L)
                       if (grepl("^$", this, useBytes = TRUE)) {
                           ## empty first line
                           ## some files have a wacky string of 0x00 bytes at the start, which means that the first line is empty (should be "[3DATAVOLLEYSCOUT]")
                           this <- tryCatch(readLines(filename, warn = FALSE, n = 40L), error = function(e) "")
                           if (grepl("^$", this[1], useBytes = TRUE) && length(grep("[3MATCH]", this, useBytes = TRUE, fixed = TRUE)) == 1 &&
                               length(grep("[3TEAMS]", this, useBytes = TRUE, fixed = TRUE)) == 1 && !any(grepl("[3DATAVOLLEYSCOUT]", this, useBytes = TRUE, fixed = TRUE))) {
                               this[1] <- "[3DATAVOLLEYSCOUT]"
                           }
                       }
                       this
                   }, error = function(e) "")
                   if_is(dv_file_data_type(chk), "dvw")
               } else if (ext == "vsm") {
                   ## minimal check that this is a json file, without actually parsing it
                   chk <- suppressWarnings(tryCatch(readChar(filename, 500L), error = function(e) ""))
                   if_is(dv_file_data_type(chk), "vsm")
               } else if (ext == "xml") {
                   chk <- readLines(filename, n = 200L, warn = FALSE)
                   if_is(dv_file_data_type(chk), "hxml")
               } else if (ext == "psvb") {
                   chk <- suppressWarnings(tryCatch(readChar(filename, 20), error = function(e) NULL))
                   if_is(dv_file_data_type(chk), "psvb")
               } else {
                   "unknown"
               }
        if (out == "unknown" && isTRUE(error_on_unknown)) stop("scout file of unknown type: ", filename)
        out
    } else {
        vapply(filename, dv_file_type, FUN.VALUE = "", USE.NAMES = FALSE)
    }
}

#' @rdname dv_file_type
#' @export
dv_file_data_type <- function(x, error_on_unknown = FALSE) {
    tryCatch({
        if (grepl("[3DATAVOLLEYSCOUT]", x[1], fixed = TRUE, useBytes = TRUE)) {
            "dvw"
        } else if (identical(substr(str_trim(x[1]), 1, 1), "{") && any(grepl("gameType", x), na.rm = TRUE)) {
            ## minimal check that this is a json file, without actually parsing it
            "vsm"
        } else if (any(grepl("<ALL_INSTANCES>", x, fixed = TRUE), na.rm = TRUE)) {
            "hxml"
        } else if (identical(base64enc::base64decode(substr(x[1], 1, 10))[5:6], as.raw(c(31, 8*16+11)))) {
            "psvb"
        } else {
            "unknown"
        }
    }, error = function(e) "unknown")
}
