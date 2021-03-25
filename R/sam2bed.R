#' Convert SAM/BAM file to BED
#'
#' @description Convert SAM (Sequence Alignment/Mapping) or BAM (binary form of SAM) file to BED (Browser Extensible Data) file. SAM files can only contain single-end-reads now.
#'
#' @param input input SAM/BAM file
#' @param output output BED file's name, without suffix
#' @param gr whether to generate a GRange object, default `FALSE`
#'
#' @import readr
#' @import stringr
#' @importFrom magrittr %>%
#' @importFrom purrr map_chr map_int
#' @importFrom Rsamtools scanBam
#' @importFrom IRanges IRanges
#' @importFrom GenomicRanges GRanges
#' @importFrom utils tail
#'
#' @author Yujie Liu
#' @export
#'
#' @examples
#' samfile <- system.file(
#'               "extdata",
#'               "THA2.sam",
#'               package = "tinyfuncr",
#'               mustWork = TRUE
#'            )
#' bamfile <- system.file(
#'               "extdata",
#'               "THA2_sorted.bam",
#'               package = "tinyfuncr",
#'               mustWork = TRUE
#'            )
#'
#' sam2bed(input = samfile, output = "THA2")
#' gr <- sam2bed(input = bamfile,
#'               output = "THA2_sorted",
#'               gr = TRUE)


sam2bed <- function(input, output, gr = FALSE) {

  format <- str_split(input, "\\.")[[1]] %>% tail(1) %>% tolower()

  if (format == "sam") {

    samlist <- read_lines(input) %>%
      .[!str_detect(., "^@")] %>%
      str_split(., "\t")

    name <- map_chr(samlist, function(x) x[1])
    flag <- map_chr(samlist, function(x) x[2])
    chrom <- map_chr(samlist, function(x) x[3])
    chromStart <- map_int(samlist, function(x) as.integer(x[4]) - 1L)
    score <- map_chr(samlist, function(x) x[5])

    width <- map_chr(samlist, function(x) x[6]) %>%
      map_int(function(x) {
        w <- 0L
        w <- w + str_extract_all(x, "(\\d+)M", simplify = T) %>%
          str_replace_all("M", "") %>%
          as.integer() %>%
          sum()
        w <- w - str_extract_all(x, "(\\d+)I", simplify = T) %>%
          str_replace_all("I", "") %>%
          as.integer() %>%
          sum()
        w <- w + str_extract_all(x, "(\\d+)D", simplify = T) %>%
          str_replace_all("D", "") %>%
          as.integer() %>%
          sum()
        return(w)
      })

    chromEnd <- chromStart + width
    strand <- ifelse(flag == "0", "+", ifelse(flag == "16", "-", NA))
    # May need modification!

    bedf <-
      cbind.data.frame(chrom, chromStart, chromEnd, name, score, strand)

    bedf <- bedf[!bedf$chrom == "*",]

  } else if (format == "bam") {

    bedfraw <- scanBam(input)[[1]] %>% as.data.frame()

    bedf <-
      data.frame(
        chrom = bedfraw$rname,
        chromStart = bedfraw$pos - 1L,  # SAM and BAM are both 1-indexed
        chromEnd = bedfraw$pos - 1L + bedfraw$qwidth,
        name = bedfraw$qname,
        score = bedfraw$mapq,
        strand = bedfraw$strand
      )

    bedf <- bedf[!is.na(bedf$chrom),]

  } else {
    stop("File Format ERROR!")
  }

  write.table(
    bedf,
    file = str_c(output, ".bed", sep = ""),
    sep = "\t",
    col.names = FALSE,
    row.names = FALSE,
    quote = FALSE,
    fileEncoding = "UTF-8"
  )

  if (gr) {
    grdata <-
      GRanges(
        seqnames = bedf$chrom,
        ranges = IRanges(start = bedf$chromStart, end = bedf$chromEnd),
        strand = bedf$strand
      )
    return(grdata)
  }

}
