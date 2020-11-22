#' Convert html format bookmarks to markdown
#'
#' @description Convert html format bookmarks to markdown. Useful especially in publishing it on GitHub. Only bookmarks files exported from Microsoft Edge (old version) or Google Chrome are supported now.
#'
#' @param file bookmarks file in html format, exported from Microsoft Edge or Google Chrome. Default: first html file in current directory
#' @param out output markdown file's name, without suffix
#' @param add.rules additional rules for some strange items. It should be a data frame or a tibble, with first column being "pattern", second being "replacement" (used in function stringr::str_replace_all()). All items should be written in string form of regular expression.
#'
#' @import readr
#' @import stringr
#' @import tibble
#' @importFrom utils write.table
#'
#' @author Yujie Liu
#' @export
#'
#' @examples
#' file <- "~/myfile/mybookmarks.html"
#' bm2md(file)
#'
#' df <- tibble(x = c('.+HREF="(.+)" (LAST|ADD).+>(.+)', "test"), c("* [\\3](\\1)", "TEST"))
#' bm2md(file, add.rules = df)


bm2md <- function(file, out = "bookmarks", add.rules = NULL) {

  file <- dir(pattern = "html$")[1]
  html <- read_table(file, col_names = FALSE, col_types = "c")
  html <- html[-(1:5), 1]

  for (i in 1:nrow(html)) {
    # Basic rules
    html[[1]][i] <- str_replace(html[[1]][i],
                                '.+HREF="(.+)" (LAST|ADD).+>(.+)</A>',
                                "* [\\3](\\1)")
    html[[1]][i] <-
      str_replace(html[[1]][i], '<TITLE>(.+)</TITLE>', "# \\1")
    html[[1]][i] <-
      str_replace(html[[1]][i], '.+>(.+)</H3>', "## \\1")
    html[[1]][i] <- str_replace(html[[1]][i], '&amp;', "&")
    html[[1]][i] <- str_replace(html[[1]][i],
                                '(</?DL><p>)|(<H1>Bookmarks</H1>)', "")
    # Rule for "zhihu"
    html[[1]][i] <- str_replace(html[[1]][i],
                              '.+HREF="(.+zhihu.+)" (LAST|ADD).+>(.+)',
                              "* [\\3](\\1)")
    # Additional rules
    if (!is.null(add.rules)) {
      for (j in 1:nrow(add.rules)) {
        html[[1]][i] <- str_replace_all(html[[1]][i],
                                        add.rules[[1]][j],
                                        add.rules[[2]][j])
      }
    }
  }

  write.table(
    html,
    file = str_c(out, ".md", sep=""),
    col.names = FALSE,
    row.names = FALSE,
    quote = FALSE,
    fileEncoding = "UTF-8"
  )

  return("File Saved!")

}
