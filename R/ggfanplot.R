#' Add "fanplot" to chromosome
#'
#' @description Add "fanplot" to chromosome. "fanplot" means repel annotation lines linking between original positions and annotations, named after the fan made by paper due to their similarity in appearance.
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
#' @param shape_repel shape repulsive from fan end, absolute length against x/y axis
#' @param ... currently ignored
#'
#' @import ggplot2
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
           shape_repel = 0.25,
           ...) {
    ################################################################
    ## make plot info repulsive
    plot_info <- data.frame()
    for (chr in levels(as.factor(ann_data$Chr))) {
      tmp <- ann_data[ann_data$Chr == chr, ]
      tmp <- tmp[order(tmp$Pos), ]
      mid_pos <- nrow(tmp) %/% 2
      tmp[mid_pos, "AnnYpos"] <- tmp[mid_pos, "Pos"]
      for (idx in (mid_pos - 1):1) {
        if (tmp[idx + 1, "AnnYpos"] - tmp[idx, "Pos"] < replusive) {
          tmp[idx, "AnnYpos"] <- tmp[idx + 1, "AnnYpos"] - replusive
        } else {
          tmp[idx, "AnnYpos"] <- tmp[idx, "Pos"]
        }
      }
      for (idx in (mid_pos + 1):nrow(tmp)) {
        if (tmp[idx, "Pos"] - tmp[idx - 1, "AnnYpos"] < replusive) {
          tmp[idx, "AnnYpos"] <- tmp[idx - 1, "AnnYpos"] + replusive
        } else {
          tmp[idx, "AnnYpos"] <- tmp[idx, "Pos"]
        }
      }
      plot_info <- rbind(plot_info, tmp)
    }


    ################################################################
    ## make annotation label and line info
    label_info <- data.frame()
    line_info <- data.frame()
    for (idx in 1:nrow(plot_info)) {
      # label
      label_tmp <- data.frame(plot_info[idx, ], AnnXpos = fan_end)
      label_info <- rbind(label_info, label_tmp)
      # line
      line_tmp <- data.frame(
        GroupID = rep(paste0(label_tmp$Chr, "_", label_tmp$Pos), 4),
        Chr = rep(label_tmp$Chr, 4),
        LineXpos = c(
          fan_start,
          fan_start + tip_length,
          fan_end - tip_length,
          fan_end
        ),
        LineYpos = c(rep(label_tmp$Pos, 2),
                     rep(label_tmp$AnnYpos, 2)),
        stringsAsFactors = F
      )
      line_info <- rbind(line_info, line_tmp)
    }


    ################################################################
    ## fan plot
    ##############################
    # plot line
    if (plot_direction[1] == "vertical") {
      #p <- ggplot() + theme_void() + facet_grid( ~ Chr) + xlim(-0.5, 5.5)
      p <- p +
        geom_path(
          data = line_info,
          mapping = aes(x = LineXpos,
                        y = LineYpos,
                        group = GroupID),
          size = 0.1
        )
    } else if (plot_direction[1] == "horizontal") {
      #p <- ggplot() + theme_void() + facet_grid(Chr ~ .) + ylim(-0.5, 5.5)
      p <- p +
        geom_path(
          data = line_info,
          mapping = aes(x = LineYpos,
                        y = LineXpos,
                        group = GroupID),
          size = 0.1
        )
    }

    ##############################
    # plot text ann
    if (plot_type[1] == "text") {
      if (plot_direction[1] == "vertical") {
        p <- p + geom_text(
          data = label_info,
          mapping = aes(x = AnnXpos,
                        y = AnnYpos,
                        label = Text),
          size = ifelse(
            is.null(label_info$TextSize),
            text_size,
            label_info$TextSize
          ),
          color = ifelse(
            is.null(label_info$TextColor),
            text_color,
            label_info$TextColor
          ),
          hjust = 0
        )
      } else if (plot_direction[1] == "horizontal") {
        p <- p +
          geom_text(
            data = label_info,
            mapping = aes(x = AnnYpos,
                          y = AnnXpos,
                          label = Text),
            size = ifelse(
              is.null(label_info$TextSize),
              text_size,
              label_info$TextSize
            ),
            color = ifelse(
              is.null(label_info$TextColor),
              text_color,
              label_info$TextColor
            ),
            hjust = ifelse(htext_direction[1] == "right", 0, 1),
            angle = ifelse(htext_direction[1] == "right", 90, -90)
          )
      }
    }

    ##############################
    # plot shape ann
    if (plot_type[1] == "shape") {
      if (plot_direction[1] == "vertical") {
        p <- p + geom_point(
          data = label_info,
          mapping = aes(x = AnnXpos + shape_repel,
                        y = AnnYpos),
          shape = ifelse(is.null(label_info$Shape),
                         19,
                         label_info$Shape),
          size = ifelse(
            is.null(label_info$ShapeSize),
            shape_size,
            label_info$ShapeSize
          ),
          color = ifelse(
            is.null(label_info$ShapeColor),
            shape_color,
            label_info$ShapeColor
          )
        )
      } else if (plot_direction[1] == "horizontal") {
        p <- p +
          geom_point(
            data = label_info,
            mapping = aes(x = AnnYpos,
                          y = AnnXpos + shape_repel),
            shape = ifelse(is.null(label_info$Shape),
                           19,
                           label_info$Shape),
            size = ifelse(
              is.null(label_info$ShapeSize),
              shape_size,
              label_info$ShapeSize
            ),
            color = ifelse(
              is.null(label_info$ShapeColor),
              shape_color,
              label_info$ShapeColor
            )
          )
      }
    }

    ##############################
    # plot textshape ann
    if (plot_type[1] == "textshape") {
      if (plot_direction[1] == "vertical") {
        p <- p + geom_point(
          data = label_info,
          mapping = aes(x = AnnXpos + shape_repel,
                        y = AnnYpos),
          shape = ifelse(is.null(label_info$Shape),
                         19,
                         label_info$Shape),
          size = ifelse(
            is.null(label_info$ShapeSize),
            shape_size,
            label_info$ShapeSize
          ),
          color = ifelse(
            is.null(label_info$ShapeColor),
            shape_color,
            label_info$ShapeColor
          ),
        ) +
          geom_text(
            data = label_info,
            mapping = aes(
              x = AnnXpos + 2 * shape_repel,
              y = AnnYpos,
              label = Text
            ),
            size = ifelse(
              is.null(label_info$TextSize),
              text_size,
              label_info$TextSize
            ),
            color = ifelse(
              is.null(label_info$TextColor),
              text_color,
              label_info$TextColor
            ),
            hjust = 0
          )
      } else if (plot_direction[1] == "horizontal") {
        p <- p +
          geom_point(
            data = label_info,
            mapping = aes(x = AnnYpos,
                          y = AnnXpos + shape_repel),
            shape = ifelse(is.null(label_info$Shape),
                           19,
                           label_info$Shape),
            size = ifelse(
              is.null(label_info$ShapeSize),
              shape_size,
              label_info$ShapeSize
            ),
            color = ifelse(
              is.null(label_info$ShapeColor),
              shape_color,
              label_info$ShapeColor
            )
          ) +
          geom_text(
            data = label_info,
            mapping = aes(
              x = AnnYpos,
              y = AnnXpos + 2 * shape_repel,
              label = Text
            ),
            size = ifelse(
              is.null(label_info$TextSize),
              text_size,
              label_info$TextSize
            ),
            color = ifelse(
              is.null(label_info$TextColor),
              text_color,
              label_info$TextColor
            ),
            hjust = ifelse(htext_direction[1] == "right", 0, 1),
            angle = ifelse(htext_direction[1] == "right", 90, -90)
          )
      }
    }

    ## output
    p
  }
