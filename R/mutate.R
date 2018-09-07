#' Mutate parts of strings
#'
#' Mutates substrings of the input. Also reports the parts that changed.
#'
#' Arguments \code{first}, \code{last}, \code{row_first}, and \code{row_last}
#' must have a common length or one item only.
#'
#' @param x a \code{character} vector containing text to be mutated.
#'   Interpreted as lines (rows) of text.
#' @param first a \code{numeric} vector. First character of substring(s)
#'   to be changed.
#' @param last a \code{numeric} vector. Last character of substring(s)
#'   to be changed.
#' @param mutate_func a function that accepts a single \code{character}
#'   string as the first argument and returns something coercible to a string.
#'   Used for mutating the substring(s) indicated by \code{first}, \code{last},
#'   \code{row_first}, and \code{row_last}.
#' @param row_first on which row (\code{numeric} index to \code{x}) does each
#'   substring begin?
#' @param row_last on which row (\code{numeric}) does each substring end?
#' @param check a \code{logical} flag. Check the arguments?
#' @param \dots arguments passed to \code{mutate_func}.
#' @return a \code{list} with two components:
#'   \item{\code{"text"}}{the modified text as a \code{character} vector. }
#'   \item{\code{"ranges"}}{a \code{data.frame} as returned by
#'   \code{\link{find_strings}}, but describing the location of each
#'   \emph{modified} string \emph{in the output}. Sections left unchanged by
#'   \code{mutate_func} are not reported.}
#' @section Note: The mutation function should preferably operate
#'   on a character-by-character basis. This is because strings streched across
#'   multiple items in \code{x} are mutated in pieces, and \emph{not} combined
#'   before mutation. See \sQuote{Examples}.
#' @examples
#' first <- seq(from = 1, to = length(letters), by = 8)
#' last <- first + seq(to = 0, by = -2, length.out = length(first))
#' a_to_z <- paste0(letters, collapse = "")
#' string_mutate(a_to_z, first, last, toupper)
#'
#' ## Logically operating on one range stretching across two rows,
#' ## but the rows are processed separately.
#' string_mutate(c("abc", "def"), 3, 1, nchar, row_first = 1, row_last = 2)
#' @export
string_mutate <- function(x, first = 1L, last = -1L,
                          mutate_func = function(x) x,
                          row_first = seq_along(x), row_last = row_first,
                          check = TRUE, ...) {
    if (length(x) == 0L) {
        return(list(text = x,
                    ranges = data.frame(row_first = integer(0),
                                        first = integer(0),
                                        row_last = integer(0),
                                        last = integer(0))))
    }
    FUN <- match.fun(mutate_func)
    arg_lengths <-
        c(length(first), length(last), length(row_first), length(row_last))
    n_sub <- max(arg_lengths)
    if (isTRUE(check)) {
        stopifnot(is.character(x), is.numeric(first), is.numeric(last),
                  is.numeric(row_first), is.numeric(row_last), !is.na(x),
                  !is.na(first), !is.na(last), !is.na(row_first),
                  !is.na(row_last), round(first) == first, round(last) == last,
                  row_first >= 1, row_first <= length(x),
                  row_last >= row_first, row_last <= length(x),
                  round(row_first) == row_first, round(row_last) == row_last)
        stopifnot(Encoding(x) != "bytes")
        if (any(arg_lengths != 1 & arg_lengths != n_sub)) {
            stop("lengths of 'first', 'last', 'row_first' and 'row_last' must be equal or 1")
        }
    }
    first2 <- rep_len(first, n_sub)
    last2 <- rep_len(last, n_sub)
    row_first2 <- rep_len(row_first, n_sub)
    row_last2 <- rep_len(row_last, n_sub)
    neg_first <- first2 < 0L
    neg_last <- last2 < 0L
    any_neg_first <- any(neg_first)
    any_neg_last <- any(neg_last)
    nc_x <- nchar(x)
    if (any_neg_first) {
        first2[neg_first] <- nc_x[row_first2] + 1L + first2[neg_first]
    }
    if (any_neg_last) {
        last2[neg_last] <- nc_x[row_last2] + 1L + last2[neg_last]
    }
    first2 <- pmin(pmax(1L, first2), nc_x[row_first2] + 1L)
    last2 <- pmin(pmax(0L, last2), nc_x[row_last2])
    arg_order <- order(row_first2, first2)
    first2 <- first2[arg_order]
    last2 <- last2[arg_order]
    row_first2 <- row_first2[arg_order]
    row_last2 <- row_last2[arg_order]
    rs2 <- row_first2[-1L]
    re2 <- row_last2[-n_sub]
    s2 <- first2[-1L]
    e2 <- last2[-n_sub]
    if (any(rs2 < re2 | (rs2 == re2 & s2 <= e2))) {
        stop("There must be no overlap between substrings")
    }
    x2 <- x
    work_row <- NA_integer_
    offset <- 0L
    max_int <- .Machine[["integer.max"]]
    mod_flag <- logical(n_sub)
    out_first <- integer(n_sub)
    out_last <- integer(n_sub)
    for (k in seq_len(n_sub)) {
        firstrow_k <- row_first2[k]
        lastrow_k <- row_last2[k]
        firstcol_k <- first2[k]
        lastcol_k <- last2[k]
        if (is.na(work_row) || work_row != firstrow_k) {
            offset <- 0L
        }
        if (firstrow_k == lastrow_k) {
            fc <- firstcol_k + offset
            lc <- lastcol_k + offset
            x_tmp <- x2[firstrow_k]
            x_in <- substr(x_tmp, fc, lc)
            nc_in <- max(0, lc - fc + 1L)
            x_out <- FUN(x_in, ...)
            nc_out <- nchar(x_out)
            if (nc_in != nc_out || x_in != x_out) {
                x2[firstrow_k] <- paste0(substr(x_tmp, 1L, fc - 1L), x_out,
                                         substr(x_tmp, lc + 1L, max_int))
                offset <- offset - nc_in + nc_out
                mod_flag[k] <- TRUE
                out_first[k] <- fc
                out_last[k] <- fc - 1L + nc_out
            }
        } else {
            fc <- firstcol_k + offset
            x_tmp1 <- x2[firstrow_k]
            x_in1 <- substr(x_tmp1, fc, max_int)
            x_out1 <- as.character(FUN(x_in1, ...))
            if (x_in1 != x_out1) {
                x2[firstrow_k] <- paste0(substr(x_tmp1, 1L, fc - 1L),
                                         x_out1)
                mod_flag[k] <- TRUE
            }
            mid_rows <- seq.int(from = firstrow_k + 1L, by = 1L,
                                length.out = lastrow_k - firstrow_k - 1L)
            x_in2 <- x2[mid_rows]
            x_out2 <- vapply(lapply(x_in2, FUN, ...), as.character, "",
                             USE.NAMES = FALSE)
            if (any(x_in2 != x_out2)) {
                x2[mid_rows] <- x_out2
                mod_flag[k] <- TRUE
            }
            x_tmp3 <- x2[lastrow_k]
            x_in3 <- substr(x_tmp3, 1, lastcol_k)
            x_out3 <- as.character(FUN(x_in3, ...))
            nc_out <- nchar(x_out3)
            if (lastcol_k != nc_out || x_in3 != x_out3) {
                x2[lastrow_k] <-
                    paste0(x_out3, substr(x_tmp3, lastcol_k + 1L, max_int))
                offset <- nc_out - lastcol_k
                mod_flag[k] <- TRUE
            }
            out_first[k] <- fc
            out_last[k] <- nc_out
        }
        work_row <- lastrow_k
    }
    list(text = x2,
         ranges = data.frame(row_first = row_first2[mod_flag],
                             first = out_first[mod_flag],
                             row_last = row_last2[mod_flag],
                             last = out_last[mod_flag]))
}
