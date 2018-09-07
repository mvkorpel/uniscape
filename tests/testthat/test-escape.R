context("Unicode escape")

test_that("ASCII strings do not change", {
    expect_identical(u_escape(""), "")
    x1 <- "foo"
    e1 <- u_escape(x1)
    expect_identical(e1, x1)
    e2 <- u_escape(x1, ranges = TRUE)
    expect_type(e2, "list")
    expect_named(e2, c("text", "ranges"))
    expect_identical(e2[["text"]], e1)
    ranges <- e2[["ranges"]]
    expect_s3_class(ranges, "data.frame")
    expect_named(ranges, c("first", "last"))
    expect_equal(nrow(ranges), 0)
})

test_that("non-ASCII characters are encoded", {
    x1 <- "\u00f6ljy\u00e4"
    y1 <- "\\u00f6ljy\\u00e4"
    e1 <- u_escape(x1, ranges = TRUE)
    expect_identical(e1[["text"]], y1)
    ranges <- e1[["ranges"]]
    expect_equal(ranges[["first"]], c(1, 10))
    expect_equal(ranges[["last"]], c(6, 15))
    ace_of_spades <- rawToChar(as.raw(c(0xf0, 0x9f, 0x82, 0xa1)))
    Encoding(ace_of_spades) <- "UTF-8"
    expect_identical(u_escape(ace_of_spades), "\\U0001f0a1")
    x1_rep <- paste0(rep.int(x1, 500), collapse = "")
    y1_rep <- paste0(rep.int(y1, 500), collapse = "")
    e1_rep <- u_escape(x1_rep, ranges = TRUE)
    expect_identical(e1_rep[["text"]], y1_rep)
    ranges_rep <- e1_rep[["ranges"]]
    expect_equal(nrow(ranges_rep), 1000)
    expect_equal(ranges_rep[["first"]],
                 c(rbind(seq.int(from = 1, by = 15, length.out = 500),
                         seq.int(from = 10, by = 15, length.out = 500))))
    expect_equal(ranges_rep[["last"]],
                 c(rbind(seq.int(from = 6, by = 15, length.out = 500),
                         seq.int(from = 15, by = 15, length.out = 500))))
})