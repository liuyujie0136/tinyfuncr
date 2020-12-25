#' Mean sequence quality scores across all reads in fastq files
#'
#' @description Compute mean sequence quality scores all reads in fastq files, which mainly derived from high-throughput next-generation sequencing.
#'
#' @param file fastq file, not in gzip
#' @param start visualization start location, default the first read
#' @param end visualization end location, default the 1000th read
#' @param verbose whether to display the result on the screen
#'
#' @import ggplot2
#' @import stringr
#' @importFrom magrittr %>%
#'
#' @author Yujie Liu
#' @export
#'


fastSQ <-
  function(file,
           start = 1L,
           end = 1000L,
           verbose = FALSE) {

    fq <- fastIN(file, format = "fastq")
    fq <- fq[start:end, ]

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

    colnames(quality) <- seq_along(quality)
    quality <- quality - phred
    q <- data.frame(read = 1:nrow(quality))
    q$mean <- rowMeans(quality)

    gp <- ggplot(data = q) +
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

    ggsave(filename = "SQVis.pdf",
           gp,
           width = 8,
           height = 6)
    if (verbose)
      print(gp)

}

