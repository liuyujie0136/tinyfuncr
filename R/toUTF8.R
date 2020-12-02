#' Convert file(s) with different encoding(s) to UTF-8
#'
#' @description Convert files with different encoding(s) to UTF-8 and then processing them with various R functions.
#'
#' @param file input file(s)
#' @param return whether to generate a character vector containing all the files' content
#'
#' @import readr
#'
#' @author Yujie Liu
#' @export
#'


toUTF8 <- function(file, return = FALSE) {

  cv <- vector()

  for (f in file) {
    x <- read_file(f)
    ge <- guess_encoding(x)
    ec <- ge$encoding[1]
    if (ge$confidence[1] != 1)
      warning("This covertion may have errors, since the original encoding is ambiguous!")

    y <- parse_character(x, locale = locale(encoding = ec))

    name <- strsplit(f, "\\.")[[1]]
    name[-1] <- sub(name[-1], paste0("_UTF8.", name[-1]), name[-1])
    fname = ""
    for (j in seq_along(name)) {
      fname <- paste0(fname, name[j])
    }

    write_file(y, fname)
    cv <- c(cv, y)

  }

  if (return) {
    return(cv)
  }

}
