context("Mutate")

test_that("Empty input works", {
    ans1 <- string_mutate(character(0))
    expect_identical(ans1[["text"]], character(0))
    expect_equal(nrow(ans1[["ranges"]]), 0)
})

test_that("Argument checks work", {
    expect_error(string_mutate(1), "character")
    expect_error(string_mutate("a", first = TRUE), "numeric")
    expect_error(string_mutate("a", last = TRUE), "numeric")
    expect_error(string_mutate("a", row_first = TRUE), "numeric")
    expect_error(string_mutate("a", row_last = TRUE), "numeric")
    func_stem <- "does_not_exist"
    func_name <- func_stem
    count <- 0
    while (exists(func_name, mode = "function")) {
        count <- count + 1
        func_name <- paste0(func_stem, count)
    }
    expect_error(string_mutate("a", mutate_func = func_name), "does_not_exist",
                 fixed = TRUE)
    expect_error(string_mutate("a", first = integer(0)), "length", fixed = TRUE)
    expect_error(string_mutate("a", last = integer(0)), "length", fixed = TRUE)
    expect_error(string_mutate("a", row_first = integer(0)), "length",
                 fixed = TRUE)
    expect_error(string_mutate("a", row_last = integer(0)), "length",
                 fixed = TRUE)
    expect_error(string_mutate("abc", first = 1:3, last = 1:2), "length",
                 fixed = TRUE)
    expect_error(string_mutate(NA_character_))
    bytes <- "\u00e6"
    Encoding(bytes) <- "bytes"
    expect_error(string_mutate(bytes), "bytes", fixed = TRUE)
    expect_error(string_mutate("a", first = NA_integer_))
    expect_error(string_mutate("a", last = NA_integer_))
    expect_error(string_mutate("a", row_first = NA_integer_))
    expect_error(string_mutate("a", row_last = NA_integer_))
    expect_error(string_mutate("a", first = rep(1, 2), last = rep(1, 2)),
                 "overlap", fixed = TRUE)
    expect_error(string_mutate(rep("a", 4), row_first = 1:2, row_last = 3:4),
                 "overlap", fixed = TRUE)
    expect_error(string_mutate("a", row_first = 0), ">=", fixed = TRUE)
    expect_error(string_mutate("a", row_last = 2), "<=", fixed = TRUE)
    expect_error(string_mutate("ab", first = 1.5), "round", fixed = TRUE)
    expect_error(string_mutate("ab", last = 1.5), "round", fixed = TRUE)
    expect_error(string_mutate(rep("a", 3), row_first = 1.5, row_last = 3),
                 "round", fixed = TRUE)
    expect_error(string_mutate(rep("a", 3), row_first = 1, row_last = 2.5),
                 "round", fixed = TRUE)
    expect_error(string_mutate(rep("a", 2), row_first = 2, row_last = 1), ">=",
                 fixed = TRUE)
})

test_that("Single row mutation works",{
    ans0 <- string_mutate("abc", first = integer(0), last = integer(0),
                          row_first = integer(0), row_last = integer(0),
                          mutate_func = toupper)
    expect_identical(ans0[["text"]], "abc")
    ranges0 <- ans0[["ranges"]]
    expect_s3_class(ranges0, "data.frame")
    expect_named(ranges0, c("row_first", "first", "row_last", "last"))
    expect_equal(nrow(ranges0), 0)
    ans00 <- string_mutate("", mutate_func = toupper)
    expect_identical(ans00[["text"]], "")
    ans1 <- string_mutate("abc", mutate_func = toupper)
    expect_identical(ans1[["text"]], "ABC")
    ranges1 <- ans1[["ranges"]]
    expect_named(ranges1, c("row_first", "first", "row_last", "last"))
    expect_equal(ranges1[["row_first"]], 1)
    expect_equal(ranges1[["row_last"]], 1)
    expect_equal(ranges1[["first"]], 1)
    expect_equal(ranges1[["last"]], 3)
    ans2 <- string_mutate("abc", first = 2, last = 3, mutate_func = toupper)
    expect_identical(ans2[["text"]], "aBC")
    ranges2 <- ans2[["ranges"]]
    expect_equal(ranges2[["row_first"]], 1)
    expect_equal(ranges2[["row_last"]], 1)
    expect_equal(ranges2[["first"]], 2)
    expect_equal(ranges2[["last"]], 3)
    ans3 <- string_mutate("abc", first = 1, last = -2, mutate_func = toupper)
    expect_identical(ans3[["text"]], "ABc")
    ranges3 <- ans3[["ranges"]]
    expect_equal(ranges3[["row_first"]], 1)
    expect_equal(ranges3[["row_last"]], 1)
    expect_equal(ranges3[["first"]], 1)
    expect_equal(ranges3[["last"]], 2)
    ans4 <- string_mutate("abc", mutate_func = nchar)
    expect_identical(ans4[["text"]], "3")
    ans5 <- string_mutate("abc", first = 2, last = 2, mutate_func = nchar)
    expect_identical(ans5[["text"]], "a1c")
    ans6 <- string_mutate("abc", first = 2, last = 1, mutate_func = nchar)
    expect_identical(ans6[["text"]], "a0bc")
    ans7 <- string_mutate("abc", first = 1, last = 0, mutate_func = nchar)
    expect_identical(ans7[["text"]], "0abc")
    ans8 <- string_mutate("abc", first = 4, last = 3, mutate_func = nchar)
    expect_identical(ans8[["text"]], "abc0")
    ans9 <- string_mutate("abc", first = c(1, 3), last = c(1, 3),
                          mutate_func = toupper)
    expect_identical(ans9[["text"]], "AbC")
    ranges9 <- ans9[["ranges"]]
    expect_equal(ranges9[["row_first"]], c(1, 1))
    expect_equal(ranges9[["row_last"]], c(1, 1))
    expect_equal(ranges9[["first"]], c(1, 3))
    expect_equal(ranges9[["last"]], c(1, 3))
})

test_that("Multirow mutation works", {
    x1 <- c("abc", "def", "ghi")
    ans1 <- string_mutate(x1, mutate_func = toupper)
    expect_identical(ans1[["text"]], c("ABC", "DEF", "GHI"))
    ranges1 <- ans1[["ranges"]]
    expect_equal(ranges1[["row_first"]], 1:3)
    expect_equal(ranges1[["row_last"]], 1:3)
    expect_equal(ranges1[["first"]], rep(1, 3))
    expect_equal(ranges1[["last"]], rep(3, 3))
    ans2 <- string_mutate(x1, first = 1, last = 3, mutate_func = toupper,
                          row_first = 1, row_last = 3)
    expect_identical(ans2[["text"]], c("ABC", "DEF", "GHI"))
    ranges2 <- ans2[["ranges"]]
    expect_equal(ranges2[["row_first"]], 1)
    expect_equal(ranges2[["row_last"]], 3)
    expect_equal(ranges2[["first"]], 1)
    expect_equal(ranges2[["last"]], 3)
    ans3 <- string_mutate(x1, first = -2, last = 2, mutate_func = nchar,
                          row_first = 1, row_last = 3)
    expect_identical(ans3[["text"]], c("a2", "3", "2i"))
    ranges3 <- ans3[["ranges"]]
    expect_equal(ranges3[["row_first"]], 1)
    expect_equal(ranges3[["row_last"]], 3)
    expect_equal(ranges3[["first"]], 2)
    expect_equal(ranges3[["last"]], 1)
})
