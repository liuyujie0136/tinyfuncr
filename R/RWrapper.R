#' @name RWrapper
#' @title Wrapper for reading/writing table/csv files
#' @description Wrapper for reading/writing table/csv files with sensible defaults.
#'
#' @param file file's full name to read in or write to
#' @param type file type, either "tsv" or "csv"
#' @param x typically a data frame or tibble
#' @param ... see more at [utils::read.table()] and [utils::write.table()]
#'
#' @importFrom utils read.table write.table
#'
#' @author Yujie Liu

#' @export read_tcsv
read_tcsv <- function(file,
                      type = "tsv",
                      header = TRUE,
                      quote = "",
                      sep = NULL,
                      row.names,
                      col.names,
                      comment.char = "",
                      stringsAsFactors = FALSE,
                      fileEncoding = "UTF-8",
                      ...
                      ){

  if (type == "tsv"){
    if (is.null(sep)){
      sep <- "\t"
    }
  } else if (type == "csv"){
    sep <- ","
  } else {
    stop("Type Error!")
  }
  x <- read.table(
    file = file,
    header = header,
    sep = sep,
    quote = quote,
    row.names = row.names,
    col.names = col.names,
    comment.char = comment.char,
    stringsAsFactors = stringsAsFactors,
    fileEncoding = fileEncoding,
    ...
  )
  return(x)
}


#' @export write_tcsv
#' @rdname RWrapper
write_tcsv <- function(x,
                       file = "tmp",
                       type = "tsv",
                       append = FALSE,
                       quote = FALSE,
                       sep = NULL,
                       col.names = TRUE,
                       row.names = FALSE,
                       fileEncoding = "UTF-8",
                       ...){

  if (type == "tsv"){
    if (is.null(sep)){
      sep <- "\t"
    }
    if (file == "tmp"){
      file <- paste0(file, ".txt")
    }
  } else if (type == "csv"){
    sep <- ","
    if (file == "tmp"){
      file <- paste0(file, ".csv")
    }
  } else {
    stop("Type Error!")
  }

  write.table(
    x = x,
    file = file,
    append = append,
    quote = quote,
    sep = sep,
    row.names = row.names,
    col.names = col.names,
    fileEncoding = "UTF-8",
    ...
  )
}

