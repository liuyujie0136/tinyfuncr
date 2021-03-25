#' Read/Write fasta/fastq files
#'
#' @description Read or write fasta or fastq files.
#' @details This function can automatically detect whether to read from or write to a file for provided parameters. To READ, you should provide path to an EXISTING fasta/fastq file, x should be kept `NULL`, and this function will return a data frame containing the content. To WRITE, you should provide path to a NEW fasta/fastq file, x should be a data frame object. Also, file format will be inferred from the content.
#'
#' For FASTA files, the data frame should contain sequence names (without `>`) in the first column, and sequence content in the second. For FASTQ files, the data frame should contain sequence information (without `@`) in the first column, sequence content in the second, additional information (without `+`) in the third, and quality information in the fourth.
#'
#' @param file file name (path). If not exist, will create one for output.
#' @param x a data frame object. Provide only if you want to write its content to file. If kept `NULL`, will create one to read content in and return it.
#'
#' @import readr
#' @import stringr
#' @importFrom magrittr %>%
#'
#' @author Yujie Liu
#' @export
#'


fastIO <- function(file, x = NULL) {

  if (file.exists(file) & is.null(x)) {

    content <- read_lines(file, n_max = 10L)

    fa <- str_detect(content, "^>") %>% any()
    fq <- str_detect(content, "^@") %>% any()

    if (fa) {
      format <- "fasta"
    } else if (fq) {
      format <- "fastq"
    } else {
      stop("Format ERROR!")
    }

    df <- fastIN(file, format = format)
    return(df)

  } else if (!file.exists(file) & !is.null(x)) {

    n <- ncol(x)

    if (n == 2) {
      format <- "fasta"
    } else if (n == 4) {
      format <- "fastq"
    } else {
      stop("Format ERROR!")
    }

    fastOUT(x, file, format = format)

  } else if (file.exists(file) & !is.null(x)) {

    warning("File for output exists! Append after that file.")

    if (ncol(x) == 2) {

      n <- nrow(x) * 2
      content <- character(n)

      content[seq(1, n, 2)] <- str_c(">", x[[1]], sep = "")
      content[seq(2, n, 2)] <- x[[2]]

      write_lines(content, file, append = TRUE)

    } else if (ncol(x) == 4) {

      n <- nrow(x) * 4
      content <- character(n)

      content[seq(1, n, 4)] <- str_c("@", x[[1]], sep = "")
      content[seq(2, n, 4)] <- x[[2]]
      content[seq(3, n, 4)] <- str_c("+", x[[3]], sep = "")
      content[seq(4, n, 4)] <- x[[4]]

      write_lines(content, file, append = TRUE)

    } else {
      stop("Format ERROR!")
    }

  } else if (!file.exists(file) & is.null(x)) {
    stop("No input or output exists! Please check your code!")
  } else {
    stop("Unknown ERROR!")
  }

}

