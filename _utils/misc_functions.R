age_brks <- function(x) {
  splits <- x %>%
    stringr::str_remove_all("[\\[\\]\\(\\)]") %>%
    stringr::str_split(",")
  splits %>%
    purrr::map_chr(function(xs) {
      x1 <- xs[1]
      x2 <- as.numeric(xs[2]) - 1
      sprintf("ages%s_%s", x1, x2)
    })
}


jenks <- function(x, n = 5, ...) {
  brks <- classInt::classIntervals(x, n, "jenks")$brk
  brks <- unique(brks)
  cut(x, breaks = brks, include.lowest = TRUE, ...)
}

