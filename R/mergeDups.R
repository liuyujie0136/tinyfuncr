#' Merge duplicated lines into one entry
#'
#' @description Merge duplicated lines into one entry according to one or some identifier(s). Note: this function may be quite slow when input data is large.
#'
#' @param x a data frame or tibble
#' @param id column index of identical variable, which is used to detect duplication, default the first column.
#' @param sep separator. If column(s) to merge only contain(s) numeric values, they can be merged with the default `sep = NULL` simply by adding them together. If character or string occurred, `sep` should be set to a string.
#'
#' @author Yujie Liu
#' @export
#'
#' @examples
#' x <-
#'   data.frame(
#'     x1 = c("A", "A", "B", "A", "C", "B"),
#'     x2 = c(1, 1, 2, 1, 3, 2),
#'     x3 = c(100, 100, 200, 100, 300, 200),
#'     x4 = c(100, 200, 300, 400, 500, 600)
#'   )
#'
#' mergeDups(x, id = 1)
#' mergeDups(x, id = 2, sep = "+")
#' mergeDups(x, id = 1:3, sep = ";")


mergeDups <- function(x, id = 1, sep = NULL) {

  dup <- duplicated(x[id])
  x_dup <- x[dup, ]
  x_nodup <- x[!dup, ]

  if (length(id) == 1){
    for (i in 1:nrow(x_dup)) {
      idx <- match(x_dup[i, id], x_nodup[[id]])
      if (is.null(sep)) {
        x_nodup[idx,-id] <- x_nodup[idx,-id] + x_dup[i,-id]
      } else {
        x_nodup[idx,-id] <-
          paste(x_nodup[idx,-id], x_dup[i,-id], sep = sep)
      }
    }
  } else {
    for (i in 1:nrow(x_dup)) {
      for (idx in 1:nrow(x_nodup)) {
        if (sum(x_dup[i, id] == x_nodup[idx, id]) == length(id)) {
          if (is.null(sep)) {
            x_nodup[idx,-id] <- x_nodup[idx,-id] + x_dup[i,-id]
          } else {
            x_nodup[idx,-id] <-
              paste(x_nodup[idx,-id], x_dup[i,-id], sep = sep)
          }
        }
      }
    }
  }

  if (class(x_nodup)[1] == "data.frame") {
    rownames(x_nodup) <- 1:nrow(x_nodup)
  }
  return(x_nodup)
}

