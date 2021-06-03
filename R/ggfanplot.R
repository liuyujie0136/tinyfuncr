#' ggfanplot
#'
#' @description ggfanplot
#'
#' @param p a ggplot object
#' @param plot_type type of fanplot annotation, one of "text", "shape", and "textshape"
#' @param plot_direction direction of chromosome, one of "vertical" and "horizontal"
#' @param fan_start "fan" start position
#' @param fan_end "fan" end position
#' @param tip_length "fan" tip length, both sides are the same
#' @param ann_data a data frame containing annotation information, colnames should be set as following:
#' - For **text** type (in `plot_type`), first column is "Chr", second is "Pos", third is "Text", fouth is "TextSize", fifth is "TextColor", where fouth and fifth column is optional. When "TextSize" and "TextColor" are not given to each annotation, use global options from `text_size` and `text_color`.
#' - For **shape** type, first column is "Chr", second is "Pos", third is "Shape", fouth is "ShapeSize", fifth is "ShapeColor", where fouth and fifth column is optional. When "ShapeSize" and "ShapeColor" are not given to each shape, use global options from `shape_size` and `shape_color`.
#' - For **textshape** type, first column is "Chr", second is "Pos", third is "Text", fouth is "Shape", fifth is "TextSize", sixth is "TextColor", seventh is "ShapeSize", eighth is "ShapeColor", where 5-8 columns are optional.
#' @param htext_direction from which side to read the text when chromosomes lying down (i.e. `plot_direction = "horizontal"`), one of "right" and "left"
#' @param replusive replusive of texts or shapes, set according to chr length
#' @param text_size global text size
#' @param text_color global text color
#' @param shape_size global shape size
#' @param shape_color global shape color
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom magrittr %>%
#' @import ggsci
#'
#' @author Yujie Liu
#' @export
#'


ggfanplot <-
  function(p,
           plot_type = c("text", "shape", "textshape"),
           plot_direction = c("vertical", "horizontal"),
           fan_start = 0.8,
           fan_end = 3,
           tip_length = 0.2,
           ann_data = NULL,
           htext_direction = c("right", "left"),
           replusive = 1e6,
           text_size = 1,
           text_color = "black",
           shape_size = 1,
           shape_color = "skyblue",
           ...) {

    ## make plot info replusive
    plot_info <- data.frame()
    for (chr in levels(as.factor(ann_data[[1]]))) {
      tmp <- ann_data[ann_data[[1]] == chr,]
      tmp <- tmp[order(tmp[[2]]),]
      mid_pos <- nrow(tmp) %/% 2
      tmp[mid_pos, "y_Label"] <- tmp[mid_pos, "Pos"]
      for (idx in (mid_pos - 1):1) {
        if (tmp[idx + 1, "y_Label"] - tmp[idx, "Pos"] < 1e6) {
          tmp[idx, "y_Label"] <- tmp[idx + 1, "y_Label"] - 1e6
        } else {
          tmp[idx, "y_Label"] <- tmp[idx, "Pos"]
        }
      }
      for (idx in (mid_pos + 1):nrow(tmp)) {
        if (tmp[idx, "Pos"] - tmp[idx - 1, "y_Label"] < 1e6) {
          tmp[idx, "y_Label"] <- tmp[idx - 1, "y_Label"] + 1e6
        } else {
          tmp[idx, "y_Label"] <- tmp[idx, "Pos"]
        }
      }
      plot_info <- rbind(plot_info, tmp)
    }

    ## make genes label
    label_all <- data.frame()
    line_all <- data.frame()
    for (idx in 1:nrow(plot_info)) {
      # label
      label_info <- plot_info[idx,]
      label_info$x_Label <- 2.2
      label_info$Symbol <- strsplit(label_info$Symbol, "\\|")[[1]][1]
      label_all <- rbind(label_all, label_info)
      # line
      line_info <- data.frame(
        GeneID = rep(label_info$GeneID, 4),
        Chr = rep(label_info$Chr, 4),
        Symbol = rep(label_info$Symbol, 4),
        x = c(0, 0.5, 1.5, 2),
        y = c(rep(label_info$Pos, 2),
              rep(label_info$y_Label, 2)),
        stringsAsFactors = F
      )
      line_all <- rbind(line_all, line_info)
    }

    ## plot
    p <- p +
      geom_line(
        data = line_all,
        mapping = aes(x = x,
                      y = y,
                      group = GeneID),
        size = 0.1
      ) +
      geom_text(
        data = label_all,
        mapping = aes(
          x = x_Label,
          y = y_Label,
          label = Symbol,
          color = GeneSet * 5
        ),
        size = 1,
        hjust = 0
      )

  }
