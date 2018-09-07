#' Escape non-ASCII characters for portable R strings
#'
#' The function converts non-ASCII characters to \code{"\\u1234"} (when
#' sufficient) or \code{"\\U12345678"} notation using the corresponding Unicode
#' code point. A backslash is literally included in the escape code, equivalent
#' to two backslashes when typing a string.
#'
#' @param x a single \code{character} string.
#' @param ranges a \code{logical} flag. If \code{FALSE} (the default),
#'   the function only returns the possibly modified string. If \code{TRUE},
#'   the locations of the modifications are also returned.
#' @return If \code{ranges} is \code{FALSE}, returns a version of \code{x}
#'   where non-ASCII characters have been encoded (\code{character} string).
#'   If \code{ranges} is \code{TRUE}, returns a \code{list} with two elements:
#'   \item{\code{"text"}}{the escaped string,}
#'   \item{\code{"ranges"}}{a \code{data.frame} with two columns:
#'     \code{"first"} denotes the beginning of each escape sequence added to
#'     the output string, and \code{"last"} is the final character of the
#'     sequence, in the \code{\link{substr}} sense.}
#' @examples
#' x <- "Mot\u00f6rhead"
#' u_escape(x, ranges = TRUE)
#' x2 <- c(charToRaw("grinning face "), as.raw(c(0xf0, 0x9f, 0x98, 0x80)),
#'         charToRaw(" is code point U+1f600"))
#' x2 <- rawToChar(x2)
#' Encoding(x2) <- "UTF-8"
#' u_escape(x2)
#' @export
u_escape <- function(x, ranges = FALSE) {
    stopifnot(is.character(x), length(x) == 1L)
    stopifnot(Encoding(x) != "bytes")
    x_int <- utf8ToInt(enc2utf8(x))
    not_ascii <- which(x_int > 127L)
    all_ascii <- length(not_ascii) == 0L
    x2 <- if (all_ascii) {
        x
    } else {
        ans <- strsplit(x, "", fixed = TRUE)[[1L]]
        is_bmp <- x_int[not_ascii] <= 0xffffL
        idx_bmp <- not_ascii[is_bmp]
        idx_astral <- not_ascii[!is_bmp]
        ans[idx_bmp] <- sprintf("\\u%04x", x_int[idx_bmp])
        ans[idx_astral] <- sprintf("\\U%08x", x_int[idx_astral])
        paste0(ans, collapse = "")
    }
    if (!isTRUE(ranges)) {
        x2
    } else if (all_ascii) {
        list(text = x2,
             ranges = data.frame(first = integer(0), last = integer(0)))
    } else {
        nx <- length(ans)
        nc <- rep.int(1L, nx)
        nc[idx_bmp] <- 6L
        nc[idx_astral] <- 10L
        last <- cumsum(nc)
        first <- c(1L, last[-nx] + 1L)
        idx <- sort(c(idx_bmp, idx_astral))
        list(text = x2,
             ranges = data.frame(first = first[idx], last = last[idx]))
    }
}
