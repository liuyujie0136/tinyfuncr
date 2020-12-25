#' Compute GC content across all reads in fastq files
#'
#' @description Compute GC content across all reads in fastq files, which mainly derived from high-throughput next-generation sequencing.
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


fastGC <-
  function(file,
           start = 1L,
           end = 1000L,
           verbose = FALSE) {

    fq <- fastIN(file, format = "fastq")

    seq <- fq[[2]] %>% toupper() %>% .[start:end] %>% as.data.frame()
    colnames(seq) <- "read"

    seq$gc <- str_count(seq$read, "G|C") / str_length(seq$read) * 100

    gp <- ggplot(data = seq) +
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

    ggsave(filename = "GCVis.pdf",
           gp,
           width = 8,
           height = 6)
    if (verbose)
      print(gp)

}

