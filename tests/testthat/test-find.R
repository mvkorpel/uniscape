context("Finding R strings")

test_that("Find (without mutate) works", {
    x1 <- c("### Testing \"testing\"", "### Second line")
    f1 <- find_strings(x1)
    expect_s3_class(f1, "data.frame")
    expect_named(f1, c("row_first", "first", "row_last", "last"))
    expect_equal(nrow(f1), 0)
    x2 <- c(x1, "\"\"", "\"abc\"", "  ''", " 'a' %in% 'b'#'c'",
            "\"start\nstop\"\n", "'start", "stop'", "\"start\\\\", "stop'\\\"")
    f2 <- find_strings(x2)
    expect_equal(nrow(f2), 7)
    expect_equal(f2[["row_first"]], c(3, 4, 5, 6,  6,  7, 8))
    expect_equal(f2[["row_last"]],  c(3, 4, 5, 6,  6,  7, 9))
    expect_equal(f2[["first"]],     c(2, 2, 4, 3, 12,  2, 2))
    expect_equal(f2[["last"]],      c(1, 4, 3, 3, 12, 11, 4))
})

test_that("Mutate works", {
    x <- c("\"\"", "\"abc\"", "  ''", " 'a' %in% 'b'#'c'",
           "\"euro\n\u20acfoo\"", "'st\u00e4rt", "st\u00f6p'",
           "\"start", "stop'")
    f1 <- find_strings(x, mutate_func = u_escape)
    f2 <- find_strings(x, mutate_func = toupper)
    r1 <- f1[["ranges"]]
    r2 <- f2[["ranges"]]
    txt1 <- f1[["text"]]
    txt2 <- f2[["text"]]

    expect_equal(nrow(r1), 2)
    expect_equal(r1[["row_first"]], c(5,  6))
    expect_equal(r1[["row_last"]],  c(5,  7))
    expect_equal(r1[["first"]],     c(2,  2))
    expect_equal(r1[["last"]],      c(15, 9))
    unchanged1 <- c(1:4, 8:9)
    expect_identical(txt1[unchanged1], x[unchanged1])
    expect_identical(txt1[5:7],
                     c("\"euro\n\\u20acfoo\"", "'st\\u00e4rt", "st\\u00f6p'"))

    expect_equal(nrow(r2), 5)
    expect_equal(r2[["row_first"]], c(2, 4,  4,  5, 6))
    expect_equal(r2[["row_last"]],  c(2, 4,  4,  5, 7))
    expect_equal(r2[["first"]],     c(2, 3, 12,  2, 2))
    expect_equal(r2[["last"]],      c(4, 3, 12, 10, 4))
    unchanged2 <- c(1L, 3L, 8:9)
    expect_identical(txt2[unchanged2], x[unchanged2])
    expect_identical(txt2[c(2L, 4:5)],
                     c("\"ABC\"",
                       " 'A' %in% 'B'#'c'",
                       "\"EURO\n\u20acFOO\""))
    expect_identical(c(substr(txt2[6], 2, 3), substr(txt2[6], 5, 6),
                       substr(txt2[7], 1, 2), substr(txt2[7], 4, 4)),
                     c("ST", "RT", "ST", "P"))
})
