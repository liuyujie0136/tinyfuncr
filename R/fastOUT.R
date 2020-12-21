#' Write fasta/fastq files
#'
#' @description Write fasta or fastq files.
#'
#' @param x file content in data frame format. For FASTA files, it should contain sequence names (without ">") in the first column, and sequence content in the second. For FASTQ files, it should contain sequence information (without "@") in the first column, sequence content in the second, additional information (without "+") in the third, and quality information in the fourth.
#' @param file output file name (path)
#' @param format output file format, must be one of "fasta" or "fastq".
#'
#' @import readr
#' @import stringr
#'
#' @author Yujie Liu
#' @export
#'


fastOUT <- function(x, file, format) {

  if (format == "fasta") {

    n <- nrow(x) * 2
    content <- character(n)

    content[seq(1, n, 2)] <- str_c(">", x[[1]], sep = "")
    content[seq(2, n, 2)] <- x[[2]]

    write_lines(content, file)

  } else if (format == "fastq") {

    n <- nrow(x) * 4
    content <- character(n)

    content[seq(1, n, 4)] <- str_c("@", x[[1]], sep = "")
    content[seq(2, n, 4)] <- x[[2]]
    content[seq(3, n, 4)] <- str_c("+", x[[3]], sep = "")
    content[seq(4, n, 4)] <- x[[4]]

    write_lines(content, file)

  } else {
    stop("File Format ERROR!")
  }

}
