#' Make IGV reference genome information
#'
#' @description Make reference genome information used in IGViewer.
#'
#' @param refid reference genome id
#'
#' @import readr
#' @import stringr
#' @importFrom magrittr %>%
#' @importFrom stats na.omit
#'
#' @author Yujie Liu
#'
#' @examples
#' makeigvref("hg38")


makeigvref <- function(refid) {

  refile <- "https://s3.amazonaws.com/igv.org.genomes/genomes.json"

  reflist <- read_file(refile) %>% str_split("\\},\\{")

  reftable <-
    as.data.frame(reflist, col.names = "data", stringsAsFactors = FALSE)

  for (i in 1:nrow(reftable)) {
    reftable[i, 1] <- str_replace(reftable[i, 1], '^\\[\\{', '')
    reftable[i, 1] <-
      str_replace(reftable[i, 1], '\\}\\]\\}\\]$', '\\}\\]')
    if (!is.na(str_match(reftable[i, 1], '^"name"'))[1, 1]) {
      for (j in (i - 1):1) {
        if (str_detect(reftable[j, 1], '^"id"')) {
          for (k in (j + 1):i) {
            reftable[j, 1] <- str_c(reftable[j, 1], reftable[k, 1], sep = '},{')
            reftable[k, 1] <- NA
          }
          break
        }
      }
    }
  }

  reftable <- na.omit(reftable)
  colnames(reftable) <- "data"

  detectid <- str_c('"id":"', refid, '"', sep = "")

  ref <- reftable[str_detect(reftable[, 1], detectid), 1]

  if (length(ref) != 0) {
    refer <- str_c('reference: {', ref, '},', sep = "")
    return(refer)
  } else {
    stop("No such reference ID! Please check it carefully!")
  }

}
