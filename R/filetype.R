#' Detect scout file type
#'
#' Returns the general file type (format), but does not differentiate e.g. indoor from beach. Type is based on the file extension and a minimal check of the contents, but without fully parsing the file.
#'
#' @param filename character: paths to files. Files that do not exist will be classified as "unknown" type
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
#'   dv_file_type(ovdata_example("clickscout")) ## dvw
#'   dv_file_type(ovdata_example("2017_AVL_mens_HEAT_vs_UTSSU")) ## psvb
#'   dv_file_type(tempfile(fileext = ".dvw")) ## unknown (contents not as expected for dvw)
#' }
#' @export
dv_file_type <- function(filename, error_on_unknown = FALSE) {
    assert_that(is.character(filename))
    if (length(filename) == 1) {
        ext <- if (!file.exists(filename)) "" else tolower(tools::file_ext(filename))
        out <- if (!ext %in% c("dvw", "vsm", "xml", "psvb")) {
                   "unknown"
               } else if (ext == "dvw") {
                   chk <- tryCatch(readLines(filename, warn = FALSE, n = 1L), error = function(e) "")
                   if (grepl("[3DATAVOLLEYSCOUT]", chk[1], fixed = TRUE)) "dvw" else "unknown"
               } else if (ext == "vsm") {
                   ## minimal check that this is a json file, without actually parsing it
                   chk <- suppressWarnings(tryCatch(readChar(filename, 2), error = function(e) ""))
                   if (identical(chk, "{\"")) "vsm" else "unknown"
               } else if (ext == "xml") {
                   chk <- readLines(filename, n = 200L)
                   if (any(grepl("<ALL_INSTANCES>", chk, fixed = TRUE))) "hxml" else "unknown"
               } else if (ext == "psvb") {
                   chk <- suppressWarnings(tryCatch(readChar(filename, 20), error = function(e) NULL))
                   if (identical(base64enc::base64decode(chk)[5:6], as.raw(c(31, 8*16+11)))) "psvb" else "unknown"
               } else {
                   "unknown"
               }
        if (out == "unknown" && isTRUE(error_on_unknown)) stop("scout file of unknown type: ", filename)
        out
    } else {
        vapply(filename, dv_file_type, FUN.VALUE = "", USE.NAMES = FALSE)
    }
}
