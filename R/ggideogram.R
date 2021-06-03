#' Plot interested genes on chromosome
#'
#' @description Plot interested genes on chromosome.
#'
#' @param karyotype_info tab-separated file (or a data frame) containing karyotype information, with first column being "Chr", second being "Start", third being "End". File should NOT have header.
#' @param gff_file GFF file, used only when `chrom_display = "gene_density"`
#' @param slide_window length of each slide window, defaut 100kb
#' @param plot_direction direction of chromosome, one of "vertical" and "horizontal"
#' @param chrom_display method to display chromosome, one of "gene_density" and "border_only"
#' @param density_color when `chrom_display = "gene_density"`, set "low" and "high" color to display gene density (1st: "low", 2nd: "high"), used in `scale_fill_gradient`
#' @param circ_ratio ratio to make chromosome endtip round, where x and y scale are not the same, change according to plot size
#' @param ann_type type of annotation added to chromosome plot, one of "none", "text", "shape", "textshape", "line", "multiline", "textline", "shapeline", and "textshapeline"
#' @param ann_data a data frame containing annotation information, colnames should be set as following:
#' - For **text** type (in `ann_type`), first column is "Chr", second is "Pos", third is "Text", fouth is "TextSize", fifth is "TextColor", where fouth and fifth column is optional. When "TextSize" and "TextColor" are not given to each annotation, use global options from `text_size` and `text_color` in `ggfanplot_args`.
#' - For **shape** type, first column is "Chr", second is "Pos", third is "Shape", fouth is "ShapeSize", fifth is "ShapeColor", where fouth and fifth column is optional. When "ShapeSize" and "ShapeColor" are not given to each shape, use global options from `shape_size` and `shape_color` in `ggfanplot_args`.
#' - For **textshape** type, first column is "Chr", second is "Pos", third is "Text", fouth is "Shape", fifth is "TextSize", sixth is "TextColor", seventh is "ShapeSize", eighth is "ShapeColor", where 5-8 columns are optional.
#' - For **line** type, first column is "Chr", second is "Pos", third is "Value"
#' - For **multiline** type, first column is "Chr", second is "Pos", the rest columns are "Value"s, where "Value"s' name can be changed to any string to match its meaning
#' - For **textline** type, first column is "Chr", second is "Pos", third is "Value", fouth is "Text", fifth is "TextSize", sixth is "TextColor", where 5-6 columns are optional.
#' - For **shapeline** type, first column is "Chr", second is "Pos", third is "Value", fouth is "Shape", fifth is "ShapeSize", sixth is "ShapeColor", where 5-6 columns are optional.
#' - For **textshapeline** type, first column is "Chr", second is "Pos", third is "Value", fouth is "Text", fifth is "Shape", sixth is "TextSize", seventh is "TextColor", eighth is "ShapeSize", nineth is "ShapeColor", where 6-9 columns are optional.
#' @param ggfanplot_args args used in `ggfanplot`, see [tinyfuncr::ggfanplot()]
#' @param ann_line_args args used in `geom_path`, see [ggplot2::geom_path())]
#'
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom magrittr %>%
#' @import ggsci
#'
#' @author Yujie Liu
#' @export
#'


ggideogram <- fucntion(
  karyotype_info,
  gff_file = NULL,
  slide_window = 1e5,
  plot_direction = c("vertical", "horizontal"),
  chrom_display = c("gene_density", "border_only"),
  density_color = c("skyblue", "red"), #low,high
  circ_ratio = NULL, # change according to plot size
  ann_type = c(
    "none",
    "text",
    "shape",
    "textshape",
    "line",
    "multiline",
    "textline",
    "shapeline",
    "textshapeline"
  ),
  ann_data = NULL,
  ggfanplot_args = list(
    htext_direction = c("right", "left"), #only in horizontal
    text_color = NULL,
    replusive = 1e6
  ),
  ann_line_args = list(
    size = 0.4,
    color = "black",
    linetype = "solid"
  )
) {

  ##############################################################
  ## make chromsome end info (circle end), vertical mod
  if (is.character(karyotype_info)) {
    karyotype <- read_tcsv(karyotype_info, header = FALSE)
  } else {
    karyotype <- karyotype_info
  }

  if (is.null(circ_ratio)) {
    circ_ratio <-
      max(karyotype[[3]]) / nrow(karyotype) / 6 #6 means xlim length
  }

  chr_endtip_info <- data.frame() # used in "gene_density"
  chr_border_info_long <-
    data.frame() # used in "border_only", long border
  chr_border_info_endtip <-
    data.frame() # used in "border_only", endtip circle

  for (i in 1:nrow(karyotype)) {
    chr <- karyotype[i, 1]
    start <- karyotype[i, 2]
    end <- karyotype[i, 3]
    R <- 0.5

    chr_endtip_info <-
      rbind(
        chr_endtip_info,
        data.frame(
          Chr = chr,
          x1 = c(0,-0.1,-0.1, 0.5, 0.5),
          y1 = c(
            start + circ_ratio * 0.5,
            start + circ_ratio * 0.5,
            start - circ_ratio * 0.05,
            start - circ_ratio * 0.05,
            start
          ),
          x2 = c(0.5, 0.5, 1.1, 1.1, 1),
          y2 = c(
            start,
            start - circ_ratio * 0.05,
            start - circ_ratio * 0.05,
            start + circ_ratio * 0.5,
            start + circ_ratio * 0.5
          ),
          x3 = c(0,-0.1,-0.1, 0.5, 0.5),
          y3 = c(
            end - circ_ratio * 0.5,
            end - circ_ratio * 0.5,
            end + circ_ratio * 0.05,
            end + circ_ratio * 0.05,
            end
          ),
          x4 = c(0.5, 0.5, 1.1, 1.1, 1),
          y4 = c(
            end,
            end + circ_ratio * 0.05,
            end + circ_ratio * 0.05,
            end - circ_ratio * 0.5,
            end - circ_ratio * 0.5
          )
        )
      )

    chr_border_info_long <-
      rbind(
        chr_border_info_long,
        data.frame(
          Chr = chr,
          x_left = c(0, 0),
          y_left = c(start + circ_ratio * 0.5,
                     end - circ_ratio * 0.5),
          x_right = c(1, 1),
          y_right = c(start + circ_ratio * 0.5,
                      end - circ_ratio * 0.5)
        )
      )

    for (theta in (0:90 * pi / 180)) {
      x1 <- 0.5 - R * sin(theta)
      y1 <- circ_ratio * (0.5 - R * cos(theta))
      x2 <- 0.5 + R * cos(theta)
      y2 <- circ_ratio * (0.5 - R * sin(theta))
      x3 <- 0.5 - R * sin(theta)
      y3 <- end - circ_ratio * (0.5 - R * cos(theta))
      x4 <- 0.5 + R * cos(theta)
      y4 <- end - circ_ratio * (0.5 - R * sin(theta))

      tmp <- data.frame(
        Chr = chr,
        x1 = x1,
        y1 = y1,
        x2 = x2,
        y2 = y2,
        x3 = x3,
        y3 = y3,
        x4 = x4,
        y4 = y4
      )

      chr_endtip_info <-
        rbind(chr_endtip_info, tmp)
      chr_border_info_endtip <-
        rbind(chr_border_info_endtip, tmp)
    }
  }


  ##############################################################
  ## plot chromosome
  p <-
    ggplot() +
    theme_void() +
    labs(fill = paste0("GeneNum/", slide_window / 1e3, "kb")) +
    theme(
      legend.title = element_text(size = 6),
      legend.key.size = unit(0.4, "cm"),
      legend.text = element_text(size = 4)
    )

  if (chrom_display[1] == "gene_density") {
    gene_density <- calcDensityGFF(gff_file,
                                   karyotype_info,
                                   feature = "gene",
                                   window = slide_window)
    # gff_file = "D:/Rice-NJU/data/Ref/IRGSP-1.0_annotation.gff3.gz"
    # karyotype_info = "Os_karyotype.txt"

    # add density color scale
    p <-
      p +
      #coord_cartesian(expand = FALSE) +
      scale_fill_gradient(low = density_color[1], high = density_color[2])

    if (plot_direction[1] == "vertical") {
      # plot chrom
      p <- p + geom_rect(
        data = gene_density,
        xmin = 0,
        xmax = 1,
        mapping = aes(
          ymin = Start,
          ymax = End,
          fill = Count
        )
      ) +
        xlim(c(-0.5, 5.5)) +
        facet_grid(. ~ Chr)
      # add round tip
      for (endtip_idx in 1:4) {
        p <- p + geom_polygon(
          data = chr_endtip_info,
          mapping = aes_string(
            x = paste0("x", endtip_idx),
            y = paste0("y", endtip_idx)
          ),
          fill = "white"
        )
      }

    } else if (plot_direction[1] == "horizontal") {
      p <- p + geom_rect(
        data = gene_density,
        ymin = 0,
        ymax = 1,
        mapping = aes(
          xmin = Start,
          xmax = End,
          fill = Count
        )
      ) +
        ylim(c(-0.5, 5.5)) +
        facet_grid(Chr ~ .)
      for (endtip_idx in 1:4) {
        p <- p + geom_polygon(
          data = chr_endtip_info,
          mapping = aes_string(
            x = paste0("y", endtip_idx),
            y = paste0("x", endtip_idx)
          ),
          fill = "white"
        )
      }
    }
  } else if (chrom_display[1] == "border_only") {
    if (plot_direction[1] == "vertical") {
      p <- p + xlim(c(-0.5, 5.5)) + facet_grid(. ~ Chr)
      # plot only chrom border
      for (border_idx in c("left", "right")) {
        p <- p + geom_path(
          data = chr_border_info_long,
          mapping = aes_string(
            x = paste0("x_", border_idx),
            y = paste0("y_", border_idx)
          ),
          color = "black"
        )
      }
      for (border_idx in 1:4) {
        p <- p + geom_path(
          data = chr_border_info_endtip,
          mapping = aes_string(
            x = paste0("x", border_idx),
            y = paste0("y", border_idx)
          ),
          color = "black"
        )
      }

    } else if (plot_direction[1] == "horizontal") {
      p <- p + ylim(c(-0.5, 5.5)) + facet_grid(Chr ~ .)
      for (border_idx in c("left", "right")) {
        p <- p + geom_path(
          data = chr_border_info_long,
          mapping = aes_string(
            x = paste0("y_", border_idx),
            y = paste0("x_", border_idx)
          ),
          color = "black"
        )
      }
      for (border_idx in 1:4) {
        p <- p + geom_path(
          data = chr_border_info_endtip,
          mapping = aes_string(
            x = paste0("y", border_idx),
            y = paste0("x", border_idx)
          ),
          color = "black"
        )
      }
    }
  }


  ##############################################################
  ## parse and make annotation info

  ##############################
  # using ggfanplot
  if (ann_type[1] == "text") {
    p <- ggfanplot(
      p,
      plot_type = ann_type[1],
      plot_direction = plot_direction[1],
      fan_start = 0.8,
      fan_end = 3,
      tip_length = 0.2,
      ann_data = ann_data,
      text_size = 0.4,
      text_color = ggfanplot_args$textshape_color,
      htext_direction = ggfanplot_args$htext_direction[1],
      replusive = ggfanplot_args$replusive
    )
  }
  if (ann_type[1] == "shape") {
    p <- ggfanplot(
      p,
      plot_type = ann_type[1],
      plot_direction = plot_direction[1],
      fan_start = 0.8,
      fan_end = 3.6,
      tip_length = 0.2,
      ann_data = ann_data,
      text_size = 0.4,
      text_color = ggfanplot_args$textshape_color,
      htext_direction = ggfanplot_args$htext_direction[1],
      replusive = ggfanplot_args$replusive
    )
  }
  if (ann_type[1] == "textshape") {
    p <- ggfanplot(
      p,
      plot_type = ann_type[1],
      plot_direction = plot_direction[1],
      fan_start = 0.8,
      fan_end = 2.6,
      tip_length = 0.2,
      ann_data = ann_data,
      text_size = 0.4,
      text_color = ggfanplot_args$textshape_color,
      htext_direction = ggfanplot_args$htext_direction[1],
      replusive = ggfanplot_args$replusive
    )
  }

  ##############################
  # line
  if (ann_type[1] == "line") {
    # scale data
    ann_data$Value <-
      ann_data$Value / ((max(ann_data$Value) - min(ann_data$Value)) / (5 - 1.5)) + (1.5 - min(ann_data$Value) / (5 - 1.5))

    if (plot_direction[1] == "vertical") {
      p <- p +
        geom_path(data = ann_data,
                  mapping = aes(x = Value,
                                y = Pos),
                  size = ann_line_args$size,
                  color = ann_line_args$color,
                  linetype = ann_line_args$linetype)
    } else if (plot_direction[1] == "horizontal") {
      p <- p +
        geom_path(data = ann_data,
                  mapping = aes(x = Pos,
                                y = Value),
                  size = ann_line_args$size,
                  color = ann_line_args$color,
                  linetype = ann_line_args$linetype)
    }
  }

  ##############################
  # multiline
  if (ann_type[1] == "multiline") {
    # scale data
    for (idx in 3:ncol(ann_data)) {
      ann_data[[idx]] <-
        ann_data[[idx]] / ((max(ann_data[[idx]]) - min(ann_data[[idx]])) / (5 - 1.5)) + (1.5 - min(ann_data[[idx]]) / (5 - 1.5))
    }

    # reshape
    ann_data_long <- reshape2::melt(
      data = ann_data,
      id.vars = c("Chr", "Pos"),
      variable.name = "Var",
      value.name = "Value"
    )

    if (plot_direction[1] == "vertical") {
      p <- p +
        geom_path(data = ann_data_long,
                  mapping = aes(x = Value,
                                y = Pos,
                                group = Var,
                                color = Var),
                  size = ann_line_args$size,
                  linetype = ann_line_args$linetype)
    } else if (plot_direction[1] == "horizontal") {
      p <- p +
        geom_path(data = ann_data_long,
                  mapping = aes(x = Pos,
                                y = Value,
                                group = Var,
                                color = Var),
                  size = ann_line_args$size,
                  linetype = ann_line_args$linetype)
    }
  }





  # print plot
  ggsave("p.pdf", p2, height = 8, width = 8)
}
