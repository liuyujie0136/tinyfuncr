#' Quality Check for fastq files
#'
#' @description Quality Check for fastq files, which mainly derived from high-throughput next-generation sequencing. It is not just a collection of fastQual, fastGC, fastSC, fastSQ, etc., some new features are also added.
#'
#' @param file fastq file, not in gzip
#' @param start QC start location, default the first read
#' @param end QC end location, default the 1000th read
#'
#' @import ggplot2
#' @import stringr
#' @importFrom reshape2 melt
#' @importFrom magrittr %>%
#' @importFrom grDevices pdf dev.off
#'
#' @author Yujie Liu
#' @export
#'


fastQCer <- function(file, start = 1L, end = 1000L) {

  ## file read-in
  fq.raw <- fastIN(file, format = "fastq")
  fq <- fq.raw[start:end, ]


  ## get phred and quality
  quality <- data.frame()
  phred33 <- FALSE
  phred64 <- FALSE
  for (i in seq_along(fq$qual)) {
    ascii <- utf8ToInt(fq$qual[i])
    quality <- rbind(quality, ascii)
    if (any(ascii >= 75)) {
      phred64 <- TRUE
    } else if (any(ascii <= 58)) {
      phred33 <- TRUE
    }
  }

  phred <- 0L
  if (phred33 && !phred64) {
    phred <- 33L
  } else if (!phred33 && phred64) {
    phred <- 64L
  } else if (phred33 && phred64) {
    stop("Phred Coding may be ERROR, check your data!")
  } else {
    phred <- 33L
    message("Phred+33 is inferred with modest evidence...")
  }

  quality <- t(quality) %>% as.data.frame()
  colnames(quality) <-
    paste0("read_", seq_along(quality)) # one read in a column
  rownames(quality) <-
    paste0("base_", 1:nrow(quality)) # one base in a row
  q <- quality - phred  # q: quality, Q=-10log10(p); p: possibility of error, lower is better

  ## get seq
  s <- fq[[2]] %>% toupper() %>% as.data.frame()

  seq <-
    str_split(s[[1]], "") %>%
    as.data.frame() %>%
    t() %>%
    as.data.frame() %>%
    lapply(function(x) str_c(x, collapse = "")) %>%
    as.data.frame() %>%
    t() %>%
    as.data.frame()


  ## basic statistics
  gc.percent <-
    sum(str_count(s[[1]], "G|C")) / (nrow(s) * nrow(seq)) * 100

  bs <- data.frame(
    fname = file,
    ftype = "FASTQ",
    enc = switch (as.character(phred),
                  "33" = "Sanger / Illumina 1.8+",
                  "64" = "Illumina 1.3+ / 1.5+"),
    totalseq = nrow(fq.raw),
    usedseq = end - start + 1,
    seqlength = nrow(seq),
    totalgc = as.character(round(gc.percent, 2)) %>% str_c("%")
  )

  pbs <- ggplot() +
    geom_text(aes(x = 0, y = 18), label = "fastQCer Report", size = 15) +
    geom_text(aes(x = 0, y = 15), label = "R package: tinyfuncr", size = 6) +
    geom_text(aes(x = 0, y = 14), label = "Author: Yujie Liu", size = 5) +
    geom_text(aes(x = 0, y = 9), label = "Basic Statistics", color = "dodgerblue3", size = 8) +
    geom_text(aes(x = -5, y = 7), label = "Filename") +
    geom_text(aes(x = -5, y = 6), label = "File Type") +
    geom_text(aes(x = -5, y = 5), label = "Encoding") +
    geom_text(aes(x = -5, y = 4), label = "Total Sequences") +
    geom_text(aes(x = -5, y = 3), label = "Used Sequences") +
    geom_text(aes(x = -5, y = 2), label = "Sequence Length") +
    geom_text(aes(x = -5, y = 1), label = "Total GC content") +
    geom_text(aes(x = 5, y = 7), label = bs$fname) +
    geom_text(aes(x = 5, y = 6), label = bs$ftype) +
    geom_text(aes(x = 5, y = 5), label = bs$enc) +
    geom_text(aes(x = 5, y = 4), label = bs$totalseq) +
    geom_text(aes(x = 5, y = 3), label = bs$usedseq) +
    geom_text(aes(x = 5, y = 2), label = bs$seqlength) +
    geom_text(aes(x = 5, y = 1), label = bs$totalgc) +
    scale_x_continuous(limits = c(-14, 16)) +
    scale_y_continuous(limits = c(-3, 20)) +
    theme(
      panel.background = element_rect(fill = "white"),
      axis.line = element_line(color = NA),
      axis.ticks = element_line(color = NA),
      axis.title = element_text(color = NA),
      axis.text = element_text(color = NA)
    )


  ## per base sequence quality
  tq <- t(q) %>% as.data.frame()
  colnames(tq) <- seq_along(tq)
  tq$read <- rownames(tq)
  tqm <-
    melt(
      tq,
      id.vars = "read",
      variable.name = "base",
      value.name = "qual"
    )

  pbsq <- ggplot(data = tqm) +
    geom_boxplot(
      aes(x = base, y = qual),
      fill = "yellow",
      outlier.size = .1,
      outlier.alpha = .1
    ) +
    geom_hline(yintercept = 28, color = "green") +
    geom_hline(yintercept = 20, color = "red") +
    labs(title = "Quality scores across all bases",
         x = "Position in read",
         y = "Quality") +
    scale_y_continuous(breaks = seq(0, 42, 2)) +
    theme(
      axis.text.x = element_text(size = 4),
      panel.background = element_rect(fill = "whitesmoke"),
      plot.title = element_text(
        colour = "dodgerblue3",
        hjust = 0.5,
        size = 20
      ),
      axis.title = element_text(size = 12)
    )


  ## per sequence quality score
  tq <- tq[-ncol(tq)]
  sq <- data.frame(read = 1:nrow(tq))
  sq$mean <- rowMeans(tq)

  psqs <- ggplot(data = sq) +
    geom_density(aes(x = mean), size = 1) +
    labs(title = "Quality score over all reads",
         x = "Mean quality score",
         y = "Density") +
    scale_x_continuous(limits = c(0, 41), breaks = seq(0, 41, 1)) +
    theme(
      panel.background = element_rect(fill = "whitesmoke"),
      plot.title = element_text(
        colour = "dodgerblue3",
        hjust = 0.5,
        size = 20
      ),
      axis.title = element_text(size = 12)
    )


  ## per base sequence content
  seq$a <- str_count(seq[[1]], "A") / str_length(seq[[1]]) * 100
  seq$t <- str_count(seq[[1]], "T") / str_length(seq[[1]]) * 100
  seq$c <- str_count(seq[[1]], "C") / str_length(seq[[1]]) * 100
  seq$g <- str_count(seq[[1]], "G") / str_length(seq[[1]]) * 100

  bp <- nrow(seq)

  pbsc <- ggplot() +
    geom_line(aes(x = 1:bp, y = a),
              data = seq,
              size = .5,
              color = "green3") +
    geom_line(aes(x = 1:bp, y = t),
              data = seq,
              size = .5,
              color = "red") +
    geom_line(aes(x = 1:bp, y = c),
              data = seq,
              size = .5,
              color = "deepskyblue") +
    geom_line(aes(x = 1:bp, y = g),
              data = seq,
              size = .5,
              color = "purple") +
    labs(title = "Sequence content across all bases",
         x = "Position in read",
         y = "Content (%)") +
    geom_line(aes(x = (bp-5):(bp-2), y = 100), size = .8, color = "green3") +
    geom_text(aes(x = bp, y = 100), label = "A") +
    geom_line(aes(x = (bp-5):(bp-2), y = 96), size = .8, color = "red") +
    geom_text(aes(x = bp, y = 96), label = "T") +
    geom_line(aes(x = (bp-5):(bp-2), y = 92), size =.8, color = "deepskyblue") +
    geom_text(aes(x = bp, y = 92), label = "C") +
    geom_line(aes(x = (bp-5):(bp-2), y = 88), size = .8, color = "purple") +
    geom_text(aes(x = bp, y = 88), label = "G") +
    scale_x_continuous(breaks = seq(0, bp, 2)) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 5)) +
    theme(
      axis.text.x = element_text(size = 6),
      panel.background = element_rect(fill = "whitesmoke"),
      plot.title = element_text(
        colour = "dodgerblue3",
        hjust = 0.5,
        size = 20
      ),
      axis.title = element_text(size = 12)
    )


  ## per sequence GC content
  s$gc <- str_count(s[[1]], "G|C") / str_length(s[[1]]) * 100

  psgc <- ggplot(data = s) +
    geom_density(aes(x = gc), size = 1) +
    labs(title = "GC distribution over all reads",
         x = "Mean GC content (%)",
         y = "Density") +
    scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 5)) +
    theme(
      panel.background = element_rect(fill = "whitesmoke"),
      plot.title = element_text(
        colour = "dodgerblue3",
        hjust = 0.5,
        size = 20
      ),
      axis.title = element_text(size = 12)
    )


  ## per base N content
  seq$n <- str_count(seq[[1]], "N") / str_length(seq[[1]]) * 100

  pbnc <- ggplot() +
    geom_line(aes(x = 1:bp, y = n),
              data = seq,
              size = .5,
              color = "red") +
    labs(title = "N content across all bases",
         x = "Position in read",
         y = "Content (%)") +
    geom_line(aes(x = (bp-5):(bp-2), y = 10), size = .8, color = "red") +
    geom_text(aes(x = bp, y = 10), label = "N") +
    scale_x_continuous(breaks = seq(0, bp, 1)) +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 1)) +
    theme(
      axis.text.x = element_text(size = 4),
      panel.background = element_rect(fill = "whitesmoke"),
      plot.title = element_text(
        colour = "dodgerblue3",
        hjust = 0.5,
        size = 20
      ),
      axis.title = element_text(size = 12)
    )


  ### FEATURES not available now:
  ## seq length distribution (not that meaningful)
  ## per tile sequence quality (may have no tile)
  ## seq duplication level
  ## overrepresented sequences
  ## adapter content
  ## Kmers content

  ## output
  pdf("fastQCer-Report.pdf", width = 8, height = 6)
  print(pbs) # basic statistics
  print(pbsq) # per base sequence quality
  print(psqs) # per sequence quality score
  print(pbsc) # per base sequence content
  print(psgc) # per sequence GC content
  print(pbnc) # per base N content
  if (!is.null(dev.list())) dev.off()

}
