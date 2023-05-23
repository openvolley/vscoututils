#' Parse a ballstring found in Perana Sports files
#'
#' @param z character: vector of ballstring entries
#' @param which string: "start", "mid", or "end". Will be used to name the columns of the returned data.frame
#'
#' @return A data.frame with columns `start_coordinate_x` and `start_coordinate_y` (or `mid_*` or `end_*`, depending on `which`)
#'
#' @examples
#' pv_parse_ballstring(c("51.25, 143.33"))
#'
#' @export
pv_parse_ballstring <- function(z, which = "start") {
    temp <- str_trim(gsub("\\|[[:digit:]]+[[:space:]]*$", "", z)) ## the trailing |N's are zones in a 4x4 grid, ignore these
    temp[temp %in% c("", "NA", "(null)")] <- "0, 0"
    temp[grep("|", temp, fixed = TRUE)] <- NA_character_
    temp <- read.csv(text = temp, header = FALSE)
    if (ncol(temp) != 2) {
        temp <- data.frame(x = rep(NA_real_, length(z)), y = rep(NA_real_, length(z)))
        names(temp) <- paste0(which, "_coordinate_", c("x", "y"))
    } else {
        names(temp) <- paste0(which, "_coordinate_", c("x", "y"))
        zidx <- abs(temp[[1]]) < 0.001 & abs(temp[[2]]) < 0.001
        temp[[1]][zidx] <- NA_real_
        temp[[2]][zidx] <- NA_real_
        ## convert to dv coordinate grid
        temp[[1]] <- temp[[1]]*0.03 + 0.5
        temp[[2]] <- (200-temp[[2]])*0.03 + 0.5
    }
    temp
}
