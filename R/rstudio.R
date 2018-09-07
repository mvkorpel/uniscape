#' RStudio addins provided by uniscape
#' @name uniscape_rstudio
NULL

#' @importFrom rstudioapi document_position
#' @importFrom rstudioapi document_range
convert_ranges <- function(start_pos, ranges, text) {
    if (nrow(ranges) == 0L) {
        return(list(document_range(start_pos, start_pos)))
    }
    row1 <- start_pos[["row"]]
    col1 <- start_pos[["column"]]
    first <- ranges[["first"]]
    last <- ranges[["last"]]
    nc <- nchar(text)
    col2 <- seq.int(from = col1, by = 1, length.out = nc)
    newlines <- gregexpr("\r?\n", text, perl = TRUE)[[1L]]
    if (newlines[1L] == -1L) {
        row_first <- row1
        row_last <- row1
    } else {
        newlines <- c(newlines) + attr(newlines, "match.length")
        row2 <- integer(nc)
        row2[newlines] <- 1L
        row2 <- cumsum(row2) + row1
        row_first <- row2[first]
        row_last <- row2[last]
        for (k in seq_along(newlines)) {
            newline_k <- newlines[k]
            idx_seq <- seq.int(from = newline_k, by = 1,
                               length.out = nc - newline_k + 1L)
            col2[idx_seq] <- seq_along(idx_seq)
        }
    }
    mapply(document_range,
           mapply(document_position, row_first, col2[first],
                  SIMPLIFY = FALSE, USE.NAMES = FALSE),
           mapply(document_position, row_last, col2[last] + 1L,
                  SIMPLIFY = FALSE, USE.NAMES = FALSE),
           SIMPLIFY = FALSE, USE.NAMES = FALSE)
}

#' @describeIn uniscape_rstudio Escape all non-ASCII characters in selection
#' @importFrom rstudioapi document_range
#' @importFrom rstudioapi getSourceEditorContext
#' @importFrom rstudioapi modifyRange
#' @importFrom rstudioapi primary_selection
#' @importFrom rstudioapi setSelectionRanges
#' @export
escapeAllAddin <- function() {
    ctxt <- getSourceEditorContext()
    ed_id <- ctxt[["id"]]
    sel <- primary_selection(ctxt[["selection"]])
    escaped <- u_escape(sel[["text"]], ranges = TRUE)
    ranges <- join_ranges(escaped[["ranges"]])
    sel_range <- sel[["range"]]
    sel_start <- sel_range[["start"]]
    if (nrow(ranges) > 0L) {
        new_text <- escaped[["text"]]
        modifyRange(sel_range, text = new_text, id = ed_id)
    }
    ranges2 <- convert_ranges(start_pos = sel_start, ranges = ranges,
                              text = new_text)
    setSelectionRanges(ranges2, id = ed_id)
}

#' @describeIn uniscape_rstudio
#'   Escape non-ASCII characters inside R strings in selection
#' @importFrom rstudioapi document_range
#' @importFrom rstudioapi getSourceEditorContext
#' @importFrom rstudioapi modifyRange
#' @importFrom rstudioapi primary_selection
#' @importFrom rstudioapi setSelectionRanges
#' @export
escapeStringsAddin <- function() {
    ctxt <- getSourceEditorContext()
    ed_id <- ctxt[["id"]]
    sel <- primary_selection(ctxt[["selection"]])
    escaped <- find_strings(sel[["text"]], mutate_func = u_escape)
    ranges <- escaped[["ranges"]]
    sel_range <- sel[["range"]]
    sel_start <- sel_range[["start"]]
    if (nrow(ranges) > 0L) {
        new_text <- escaped[["text"]]
        modifyRange(sel_range, text = new_text, id = ed_id)
    }
    ranges2 <- convert_ranges(start_pos = sel_start, ranges = ranges,
                              text = new_text)
    setSelectionRanges(ranges2, id = ed_id)
}

#' @describeIn uniscape_rstudio
#'   Escape non-ASCII characters inside R strings in current source file
#' @export
escapeFileAddin <- function() {
    selectAll()
    escapeStringsAddin()
}

#' @describeIn uniscape_rstudio Select all in current source file.
#'   Internal use; not registered as RStudio addin.
#' @importFrom rstudioapi document_position
#' @importFrom rstudioapi document_range
#' @importFrom rstudioapi getSourceEditorContext
#' @importFrom rstudioapi setSelectionRanges
#' @export
selectAll <- function() {
    ctxt <- getSourceEditorContext()
    ed_id <- ctxt[["id"]]
    doc_start <- document_position(1, 1)
    doc_end <- document_position(length(ctxt[["contents"]]) + 1, 1)
    range_all <- document_range(doc_start, doc_end)
    setSelectionRanges(list(range_all), id = ed_id)
}
