#' Calculate density in a given region
#'
#' @description Calculate density in a given region with a slide window
#'
#' @param pos_info tab-separated file (or a data frame) containing position information of items to calculate density, with first column being "ID", second being "Pos". File should NOT have header.
#' @param region_info tab-separated file (or a data frame) containing information about the full-length region to calculate density in, with first column being "ID", second being "Start", third being "End". File should NOT have header.
#' @param slide_window length of each slide window, default 100kb
#'
#' @importFrom tidyr separate
#' @author Yujie Liu
#' @export
#'


calcDensity <- function(pos_info,
                        region_info,
                        slide_window = 1e5) {
  # read in files
  if (is.character(pos_info)) {
    pos <- read_tcsv(pos_info, header = FALSE)
  } else {
    pos <- pos_info
  }
  if (is.character(region_info)) {
    region <- read_tcsv(region_info, header = FALSE)
  } else {
    region <- region_info
  }

  # select only interested IDs
  all_ids <- region[[1]]
  pos <- pos[pos[[1]] %in% all_ids, ]

  # make output data frame
  density <- data.frame()

  # loop against each ID
  for (id in all_ids) {
    id_start <- region[region[[1]] == id, 2]
    id_end <- region[region[[1]] == id, 3]

    # count
    tmp <-
      data.frame(table(cut(pos[pos[[1]] == id, 2],
                           breaks = c(
                             seq(id_start, id_end, slide_window),
                             id_end
                           ))))

    # make outcome tidy
    tmp <-
      tidyr::separate(tmp,
                      1,
                      into = c("Start", "End"),
                      sep = ",")
    tmp$Start <-
      as.numeric(gsub("\\(", "", tmp$Start)) + 1
    tmp$End <-
      as.numeric(gsub("\\]", "", tmp$End))

    # add id info
    tmp$ID <- id

    # reorder columns
    tmp <- tmp[c(4, 1:3)]
    colnames(tmp) <- c("ID", "Start", "End", "Count")

    # modify last end_pos to id_end
    tmp[nrow(tmp), "End"] <- id_end

    # combine
    density <- rbind(density, tmp)
  }

  return(density)
}
