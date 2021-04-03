#' Expand one entry into multiple lines
#'
#' @description Expand one entry, which is separated into different part (which may have different meanings) by a common separator, into multiple lines. Note: this function may destroy your data in some way, since its simply "copy" the rest columns to each entry, so you should be well aware of the meaning of your data.
#'
#' @param x a data frame or tibble
#' @param id column index of variable needs to expand, which has separators defined as `sep`. Default the first column.
#' @param sep separator
#'
#' @author Yujie Liu
#' @export
#'
#' @examples
#' x <-
#'   data.frame(
#'     x1 = c("A,A,A", "B,B", "C"),
#'     x2 = c(1, 2, 3),
#'     x3 = c("100,100,200", "200,200", "300")
#'   )
#' expandDups(x, id = 1)


expandDups <- function(x, id = 1, sep = ",") {

  # change to RE form, some others should not be used as sep at all
  sep <- switch(sep,
                "." = "\\.",
                "*" = "\\*",
                "+" = "\\+",
                "?" = "\\?",
                "|" = "\\|",
                "(" = "\\(",
                ")" = "\\)",
                "[" = "\\[",
                "]" = "\\]",
                "{" = "\\{",
                "}" = "\\}",
                sep)

  # detect duplications
  dup <- grepl(sep, x[[id]])
  x_dup <- x[dup,]
  x_nodup <- x[!dup,]

  # expand
  x_dup_expand <- data.frame()
  for (j in 1:nrow(x_dup)) {
    keys <- strsplit(x_dup[j, id], sep)[[1]]
    for (key in keys) {
      if (id == 1) {
        expand_data <- data.frame(key, x_dup[j, -id])
      } else if (id == ncol(x_dup)) {
        expand_data <- data.frame(x_dup[j, -id], key)
      } else {
        expand_data <-
          data.frame(x_dup[j, 1:(id - 1)], key, x_dup[j, (id + 1):ncol(x_dup)])
      }
      colnames(expand_data) <- colnames(x_dup)
      x_dup_expand <- rbind(x_dup_expand, expand_data)
    }
  }

  # sort
  x_all <- rbind(x_nodup, x_dup_expand)
  x_all_ordered <- x_all[order(x_all[[id]]),]
  rownames(x_all_ordered) <- 1:nrow(x_all_ordered)

  return(x_all_ordered)
}
