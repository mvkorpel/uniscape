#' Find R character strings in input text
#'
#' Finds \R \code{character} strings in vector \code{x}. The input is
#' interpreted as consecutive lines of \R code. A function may be supplied for
#' mutating each string found.
#'
#' @param x input text. A \code{character} vector.
#' @param mutate_func optional function for mutating every string found. The
#'   function must accept a string as its first argument and return a string (or
#'   something coercible to a string). The function operates on the contents of
#'   the string, i.e., the delimiters (quotes) will remain unchanged.
#' @param \dots optional arguments to \code{mutate_func}.
#' @return
#'   If no function is specified (the default), the location of each \R string
#'   is returned in a \code{data.frame}. The columns are \code{"row_first"},
#'   \code{"first"}, \code{"row_last"}, and \code{"last"}. These point
#'   to the row and column numbers of the first and last character of each
#'   string (contents only, i.e., delimiters excluded). The numbering is based
#'   on one item of \code{x} representing one line of code, i.e., no newlines in
#'   \code{x}. Empty strings have a \code{"last"} character one smaller than
#'   the \code{"first"}.
#'
#'   If \code{mutate_func} is specified, the main function returns a \code{list}
#'   with items \code{"text"} and \code{"ranges"} described in
#'   \code{\link{string_mutate}}. In \code{"text"}, every \R string in \code{x}
#'   has been run through the mutation function, possibly in multiple parts.
#' @section Note: The mutation function, if specified, should preferably operate
#'   on a character-by-character basis. See \code{\link{string_mutate}}.
#' @seealso \code{\link{u_escape}} can be used as \code{mutate_func}.
#' @examples
#' x <- c("### A sample R script",
#'        "foo <- function(x) {",
#'        "    ## Comments are 'ignored'",
#'        "    paste(\"\\\"foo\", x, '#bar\\\\', \"'baz'\") #, \"more\")",
#'        "}")
#' find_strings(x)
#' x_upper <- find_strings(x, mutate_func = toupper)
#' cat(x_upper[["text"]], sep = "\n")
#' x_nchar <- find_strings(x, mutate_func = nchar)
#' cat(x_nchar[["text"]], sep = "\n")
#' @export
find_strings <- function(x, mutate_func = NULL, ...) {
    n <- length(x)
    stopifnot(is.character(x), !is.na(x))
    stopifnot(Encoding(x) != "bytes")
    state <- "clean"
    row_first <- integer(0L)
    col_first <- integer(0L)
    row_last  <- integer(0L)
    col_last  <- integer(0L)
    pending_row <- NA_integer_
    pending_col <- NA_integer_
    for (k in seq_len(n)) {
        tmp <- find_strings_single(x[k], state)
        state <- tmp[["state"]]
        tmp_first <- tmp[["first"]]
        tmp_pending <- tmp[["pending_first"]]
        if (length(tmp_first) >= 1L) {
            tmp_last <- tmp[["last"]]
            tmp_row <- rep.int(k, length(tmp_first))
            if (is.na(tmp_first[1L])) {
                tmp_row[1L] <- pending_row
                tmp_first[1L] <- pending_col
            }
            pending_row <- k
            pending_col <- tmp_pending
            row_first <- c(row_first, tmp_row)
            col_first <- c(col_first, tmp_first)
            row_last <- c(row_last, rep.int(k, length(tmp_first)))
            col_last <- c(col_last, tmp_last)
        } else if (!is.na(tmp_pending)) {
            pending_row <- k
            pending_col <- tmp_pending
        }
    }
    ranges <- data.frame(row_first = row_first, first = col_first,
                         row_last = row_last, last = col_last)
    if (is.null(mutate_func)) {
        ranges
    } else {
        string_mutate(x, first = col_first, last = col_last,
                      mutate_func = mutate_func,
                      row_first = row_first, row_last = row_last,
                      check = FALSE, ...)
    }
}

find_strings_single <- function(x, state = "clean") {
    nc <- nchar(x)
    newlines <- gregexpr("\r?\n", x, perl = TRUE)[[1L]]
    if (newlines[1L] == -1L) {
        return(find_strings_no_newline(x, state = state))
    }
    match_len <- attr(newlines, "match.length")
    newline_first <- c(newlines)
    newline_last  <- newline_first - 1L + match_len
    n_newlines <- length(newline_first)
    n_lines <- n_newlines + 1L
    line_first <- c(1L, newline_last + 1L)
    line_last <- c(newline_first - 1L, nc)
    if (line_first[n_lines] > nc) {
        line_first <- line_first[-n_lines]
        line_last <- line_last[-n_lines]
    }
    keep <- line_last >= line_first
    line_first <- line_first[keep]
    line_last <- line_last[keep]
    nonempty <- substring(x, line_first, line_last)
    first <- integer(0L)
    last <- integer(0L)
    state2 <- state
    pending_first <- NA_integer_
    for (k in seq_along(nonempty)) {
        first_k <- line_first[k] - 1L
        tmp <- find_strings_no_newline(nonempty[k], state2)
        state2 <- tmp[["state"]]
        tmp_first <- first_k + tmp[["first"]]
        tmp_pending <- first_k + tmp[["pending_first"]]
        if (length(tmp_first) >= 1L) {
            if (is.na(tmp_first[1L])) {
                tmp_first[1L] <- pending_first
            }
            pending_first <- tmp_pending
            first <- c(first, tmp_first)
            last <- c(last, first_k + tmp[["last"]])
        } else if (!is.na(tmp_pending)) {
            pending_first <- tmp_pending
        }
    }
    list(first = first, last = last, state = state2,
         pending_first = pending_first)
}

find_strings_no_newline <- function(x, state = c("clean", "single", "double")) {
    k_clean <- 1L
    k_single <- 2L
    k_double <- 3L
    k_comment <- 4L
    k_states <- c(clean = k_clean, single = k_single, double = k_double)
    state2 <- k_states[match.arg(state)]
    pending_first <- NA_integer_
    comments <- c(gregexpr("#+", x, perl = TRUE)[[1L]])
    if (comments[1L] == -1L) {
        comments <- integer(0L)
    }
    singles <- c(gregexpr("'", x, fixed = TRUE)[[1L]])
    if (singles[1L] == -1L) {
        singles <- integer(0L)
    }
    doubles <- c(gregexpr("\"", x, fixed = TRUE)[[1L]])
    if (doubles[1L] == -1L) {
        doubles <- integer(0L)
    }
    first <- integer(0L)
    last <- integer(0L)
    if (length(singles) == 0L && length(doubles) == 0L) {
        return(list(first = first, last = last, state = state,
                    pending_first = pending_first))
    }
    one_backslash <- c(gregexpr("\\", x, fixed = TRUE)[[1L]]) + 1L
    if (one_backslash[1L] != 0L) {
        two_backslashes <- c(gregexpr("\\\\", x, fixed = TRUE)[[1L]]) + 2L
        if (two_backslashes[1L] != 1L) {
            one_backslash <- setdiff(one_backslash, two_backslashes)
        }
        singles <- setdiff(singles, one_backslash)
        doubles <- setdiff(doubles, one_backslash)
    }
    char_pos <- sort(c(comments, singles, doubles))
    char_type <- integer(length(char_pos))
    char_type[char_pos %in% comments] <- k_comment
    char_type[char_pos %in% singles]  <- k_single
    char_type[char_pos %in% doubles]  <- k_double
    for (k in seq_along(char_pos)) {
        pos_k  <- char_pos[k]
        type_k <- char_type[k]
        if (state2 == k_clean) {
            if (type_k == k_comment) {
                break
            } else {
                pending_first <- pos_k + 1L
                state2 <- type_k
            }
        } else if (state2 == type_k) {
            pending_last <- pos_k - 1L
            first <- c(first, pending_first)
            last  <- c(last, pending_last)
            state2 <- k_clean
            pending_first <- NA_integer_
        }
    }
    list(first = first, last = last,
         state = names(k_states)[match(state2, k_states)],
         pending_first = pending_first)
}
