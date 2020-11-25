#' Get IGV online reference genomes table
#'
#' @description Get IGV pre-defined reference genomes and annotation. It will return a table containing current list of pre-defined reference genomes' names and IDs. Note: You should only use IDs available in this table in the function makeigvref().
#'
#' @import readr
#' @import stringr
#' @import magrittr
#' @importFrom stats na.omit
#'
#' @author Yujie Liu
#' @export
#'
#' @examples
#' getigvtable()


getigvtable <- function() {

  gfile <- "https://s3.amazonaws.com/igv.org.genomes/genomes.json"

  glist <- read_file(gfile) %>% str_split("\\},\\{")

  gtable <- as.data.frame(glist, col.names = "data", stringsAsFactors = FALSE)

  for (i in 1:nrow(gtable)) {
    gtable[i, 1] <- str_replace(gtable[i, 1], '^\\[\\{', '')
    gtable[i, 1] <-
      str_replace(gtable[i, 1], '\\}\\]\\}\\]$', '\\}\\]')
    if (!is.na(str_match(gtable[i, 1], '^"name"'))[1, 1]) {
      for (j in (i - 1):1) {
        if (str_detect(gtable[j, 1], '^"id"')) {
          for (k in (j + 1):i) {
            gtable[j, 1] <- str_c(gtable[j, 1], gtable[k, 1], sep = '},{')
            gtable[k, 1] <- NA
          }
          break
        }
      }
    }
  }

  gtable <- na.omit(gtable)
  colnames(gtable) <- "data"

  id <- data.frame()
  for (i in 1:nrow(gtable)) {
    id[i, 1] <-
      str_split(gtable[[1]][i], ",")[[1]][2] %>%
      str_split(":") %>%
      .[[1]] %>%
      .[2] %>%
      str_replace_all('"', '')
    id[i, 2] <-
      str_split(gtable[[1]][i], ",")[[1]][1] %>%
      str_split(":") %>%
      .[[1]] %>%
      .[2] %>%
      str_replace_all('"', '')
  }
  colnames(id) <- c("Names", "ID")

  return(id)

}
