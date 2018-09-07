context("RStudio interface (with mock functions)")

if (requireNamespace("mockery")) {

    contents <- c("### foobar \u00f6\uea",
                  "x <- \"el ba\u00f1o\"; y <- \"jam\u00f3n",
                  "ib\u00e9rico\"",
                  "5",
                  "## '\u00aabc'")
    mock_getSourceEditorContext <- function(first, last, final_newline = TRUE) {
        function() {
            if (final_newline) {
                last_row <- last + 1L
                last_col <- 1L
            } else {
                last_row <- last
                last_col <- nchar(contents[last]) + 1L
            }
            range <-
                rstudioapi::document_range(rstudioapi::document_position(first, 1L),
                                           rstudioapi::document_position(last_row, last_col))
            Text <- paste0(c(contents[first:last], if (final_newline) ""),
                           collapse = "\n")
            structure(list(id = "F00",
                           path = "",
                           contents = contents,
                           selection = structure(list(list(range = range,
                                                           text = Text)),
                                                 class = "document_selection")),
                      class = "document_context")
        }
    }

    ## modifyRange
    ## setSelectionRanges
    do_nothing <- function(...) NULL
    verifyRanges <- function(start_row, start_col, end_row, end_col,
                             x_is_list) {
        function(x, ...) {
            if (x_is_list) {
                stopifnot(is.list(x))
                x2 <- x
            } else {
                x2 <- list(x)
            }
            stopifnot(vapply(x2, rstudioapi::is.document_range, FALSE))
            Start <- lapply(x2, `[[`, "start")
            stopifnot(length(Start) == length(start_row))
            End   <- lapply(x2, `[[`, "end")
            Start_row <- vapply(Start, `[`, 0, "row")
            End_row   <- vapply(End, `[`, 0, "row")
            Start_col <- vapply(Start, `[`, 0, "column")
            End_col   <- vapply(End, `[`, 0, "column")
            stopifnot(Start_row == start_row,
                      End_row == end_row,
                      Start_col == start_col,
                      End_col == end_col)
        }
    }

    test_that("escapeAllAddin works", {
        mockery::stub(escapeAllAddin, "getSourceEditorContext",
                      mock_getSourceEditorContext(1, 4))
        vr1a <- verifyRanges(start_row = 1, start_col = 1,
                             end_row = 5, end_col = 1, x_is_list = FALSE)
        mockery::stub(escapeAllAddin, "modifyRange", vr1a)
        vr1b <- verifyRanges(start_row = c(1, 2, 2, 3),
                             start_col = c(12, 12, 31, 3),
                             end_row = c(1, 2, 2, 3),
                             end_col = c(24, 18, 37, 9), x_is_list = TRUE)
        mockery::stub(escapeAllAddin, "setSelectionRanges", vr1b)
        expect_silent(escapeAllAddin())

        mockery::stub(escapeAllAddin, "getSourceEditorContext",
                      mock_getSourceEditorContext(3, 3))
        vr1c <- verifyRanges(start_row = 3, start_col = 1,
                             end_row = 4, end_col = 1, x_is_list = FALSE)
        mockery::stub(escapeAllAddin, "modifyRange", vr1c)
        vr1d <- verifyRanges(start_row = 3, start_col = 3,
                             end_row = 3, end_col = 9, x_is_list = TRUE)
        mockery::stub(escapeAllAddin, "setSelectionRanges", vr1d)
        expect_silent(escapeAllAddin())

        mockery::stub(escapeAllAddin, "getSourceEditorContext",
                      mock_getSourceEditorContext(3, 3, FALSE))
        vr1e <- verifyRanges(start_row = 3, start_col = 1,
                             end_row = 3, end_col = nchar(contents[3]) + 1,
                             x_is_list = FALSE)
        mockery::stub(escapeAllAddin, "modifyRange", vr1e)
        vr1f <- verifyRanges(start_row = 3, start_col = 3,
                             end_row = 3, end_col = 9, x_is_list = TRUE)
        mockery::stub(escapeAllAddin, "setSelectionRanges", vr1f)
        expect_silent(escapeAllAddin())
    })
    test_that("escapeStringsAddin works", {
        mockery::stub(escapeStringsAddin, "getSourceEditorContext",
                      mock_getSourceEditorContext(1, 4))
        vr2a <- verifyRanges(start_row = 1, start_col = 1,
                             end_row = 5, end_col = 1,
                             x_is_list = FALSE)
        mockery::stub(escapeStringsAddin, "modifyRange", vr2a)
        vr2b <- verifyRanges(start_row = c(2, 2), start_col = c(7, 28),
                             end_row = c(2, 3), end_col = c(19, 13),
                             x_is_list = TRUE)
        mockery::stub(escapeStringsAddin, "setSelectionRanges", vr2b)
        expect_silent(escapeStringsAddin())
        mockery::stub(escapeStringsAddin, "getSourceEditorContext",
                      mock_getSourceEditorContext(5, 5, FALSE))
        vr2c <- verifyRanges(start_row = 5, start_col = 1,
                             end_row = 5, end_col = nchar(contents[5]) + 1,
                             x_is_list = FALSE)
        mockery::stub(escapeStringsAddin, "modifyRange", vr2c)
        vr2d <- verifyRanges(start_row = 5, start_col = 1,
                             end_row = 5, end_col = 1, x_is_list = TRUE)
        mockery::stub(escapeStringsAddin, "setSelectionRanges", vr2d)
        expect_silent(escapeStringsAddin())
    })
    test_that("selectAll works", {
        mockery::stub(selectAll, "getSourceEditorContext",
                      mock_getSourceEditorContext(1, 5))
        vr3 <- verifyRanges(start_row = 1, start_col = 1,
                            end_row = 6, end_col = 1, x_is_list = TRUE)
        mockery::stub(selectAll, "setSelectionRanges", vr3)
        expect_silent(selectAll())
    })
    test_that("escapeFileAddin works", {
        get_context <- mock_getSourceEditorContext(1, 5)
        mockery::stub(selectAll, "getSourceEditorContext", get_context)
        vr4a <- verifyRanges(start_row = 1, start_col = 1,
                             end_row = 6, end_col = 1, x_is_list = TRUE)
        mockery::stub(selectAll, "setSelectionRanges", vr4a)

        mockery::stub(escapeStringsAddin, "getSourceEditorContext", get_context)
        vr4b <- verifyRanges(start_row = 1, start_col = 1,
                             end_row = 6, end_col = 1, x_is_list = FALSE)
        mockery::stub(escapeStringsAddin, "modifyRange", vr4b)
        vr4c <- verifyRanges(start_row = c(2, 2), start_col = c(7, 28),
                             end_row = c(2, 3), end_col = c(19, 13),
                             x_is_list = TRUE)
        mockery::stub(escapeStringsAddin, "setSelectionRanges", vr4c)

        mockery::stub(escapeFileAddin, "selectAll", selectAll)
        mockery::stub(escapeFileAddin, "escapeStringsAddin", escapeStringsAddin)
        expect_silent(escapeFileAddin())
    })
}
