#' Compute sequence content across all bases in fastq files
#'
#' @description Compute sequence content across all bases in fastq files, which mainly derived from high-throughput next-generation sequencing.
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


fastSC <-
  function(file,
           start = 1L,
           end = 1000L,
           verbose = FALSE) {

    fq <- fastIN(file, format = "fastq")

    s <- fq[[2]] %>% toupper() %>% .[start:end] %>% as.data.frame()

    seq <-
      str_split(s[[1]], "") %>%
      as.data.frame() %>%
      t() %>%
      as.data.frame() %>%
      lapply(function(x) str_c(x, collapse = "")) %>%
      as.data.frame() %>%
      t() %>%
      as.data.frame()

    seq$a <- str_count(seq[[1]], "A") / str_length(seq[[1]]) * 100
    seq$t <- str_count(seq[[1]], "T") / str_length(seq[[1]]) * 100
    seq$c <- str_count(seq[[1]], "C") / str_length(seq[[1]]) * 100
    seq$g <- str_count(seq[[1]], "G") / str_length(seq[[1]]) * 100

    bp <- nrow(seq)

    gp <- ggplot() +
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

    ggsave(filename = "SCVis.pdf",
           gp,
           width = 8,
           height = 6)
    if (verbose)
      print(gp)

}

