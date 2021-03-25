#' Read fasta/fastq files
#'
#' @description Read fasta or fastq file into a data frame. For FASTA files, it will contain sequence names (without `>`) in the first column, and sequence content in the second. For FASTQ files, it will contain sequence information (without `@`) in the first column, sequence content in the second, additional information (without `+`) in the third, and quality information in the fourth.
#'
#' @param file input file name (path)
#' @param format input file format, must be one of `fasta` or `fastq`.
#'
#' @import readr
#' @import stringr
#' @importFrom magrittr %>% %<>%
#'
#' @author Yujie Liu
#' @export
#'


fastIN <- function(file, format) {

  if (format == "fasta"){

    content <- read_lines(file)

    header <- str_detect(content, "^>")
    name <- content[header] %>% str_replace('^>', '')

    content[header] <- ">"
    content %<>% str_c(collapse = "")
    seq <- str_split(content, ">")[[1]][-1] # the first line is empty

    fasta <- data.frame(name, seq, stringsAsFactors = FALSE)
    return(fasta)

  } else if (format == "fastq") {

    content <- read_lines(file)
    n <- length(content)

    name <- content[seq(1, n, 4)] %>% str_replace("^@", "")
    seq <- content[seq(2, n, 4)]
    info <- content[seq(3, n, 4)] %>% str_replace("^\\+", "")
    qual <- content[seq(4, n, 4)]

    fastq <- data.frame(name, seq, info, qual, stringsAsFactors = FALSE)
    return(fastq)

  } else {
    stop("File Format ERROR!")
  }

}

