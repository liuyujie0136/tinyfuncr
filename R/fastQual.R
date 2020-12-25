#' Visualize quality information in fastq files
#'
#' @description Visualize quality information in fastq files, which mainly derived from high-throughput next-generation sequencing. There are two modes: in "text" mode, it will generate text-based bar plots of relative possibility of error (the lower the better); in "plot" mode, it will return a plot displaying per base sequence quality (the higher the better).
#'
#' @param file fastq file, not in gzip
#' @param mode visualization mode, now support "text" and "plot"
#' @param start visualization start location, default the first read
#' @param end visualization end location, default the 1000th read
#' @param verbose whether to display the result on the screen
#'
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom magrittr %>%
#'
#' @author Yujie Liu
#' @export
#'


fastQual <-
  function(file,
           mode = "text",
           start = 1L,
           end = 1000L,
           verbose = FALSE) {

  fq <- fastIN(file, format = "fastq")

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
  colnames(quality) <- paste0("read_", seq_along(quality)) # one read in a column
  rownames(quality) <- paste0("base_", 1:nrow(quality)) # one base in a row
  q <- quality - phred  # quality, Q=-10log10(p)
  p <- 10 ^ (q / -10)  # possibility of error, lower is better

  if (mode == "text") {

    text <- lapply(p[start:end], function(x) {
      rp <- x / max(x) # relative possibility of error
      bars <-
        intToUtf8(c(9601:9603, 9605:9607)) %>% strsplit("") %>% .[[1]]
      fac <- cut(
        rp,
        breaks = seq(0, 1, length = length(bars) + 1),
        labels = bars,
        include.lowest = TRUE
      )
      chars <- as.character(fac)
      paste0(chars, collapse = "")
    }) # modified from pillar:::spark_bar()

    tdf <- as.data.frame(text) %>% t() %>% as.data.frame()
    write.table(
      tdf,
      file = "QualVis.txt",
      col.names = FALSE,
      row.names = TRUE,
      quote = FALSE,
      fileEncoding = "UTF-8"
    )
    if (verbose) print(text)

  } else if (mode == "plot"){

    tq <- t(q[start:end]) %>% as.data.frame()
    colnames(tq) <- seq_along(tq)
    tq$read <- rownames(tq)
    tqm <-
      melt(tq,
           id.vars = "read",
           variable.name = "base",
           value.name = "qual")

    gp <- ggplot(data = tqm) +
      geom_boxplot(
        aes(x = base, y = qual),
        fill = "yellow",
        outlier.size = .8,
        outlier.alpha = .5
      ) +
      geom_hline(yintercept = 28, color = "green") +
      geom_hline(yintercept = 20, color = "red") +
      labs(title = "Quality scores across all bases",
           x = "Position in read",
           y = "Quality") +
      scale_y_continuous(breaks = seq(0, 42, 2)) +
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

    ggsave(filename = "QualVis.pdf", gp, width = 12, height = 8)
    if (verbose) print(gp)

  } else stop("MODE ERROR!")

}

