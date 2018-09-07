## Join consecutive selection ranges into one. This means ranges where the
## last character of a range is immediately followed by the first character of
## another range. Assumes that the ranges correspond to substr() positions
## in a single string.
join_ranges <- function(ranges) {
    n <- nrow(ranges)
    if (n < 2L)
        return(ranges)
    first <- ranges[["first"]]
    last <- ranges[["last"]]
    begin <- c(1L, which(first[-1L] != last[-n] + 1L) + 1L)
    end <- c(begin[-1L] - 1L, n)
    data.frame(first = first[begin], last = last[end])
}
