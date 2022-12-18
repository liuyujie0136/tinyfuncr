#' Convert html format bookmarks to markdown
#'
#' @description Convert html format bookmarks to markdown. Useful especially in publishing it on GitHub. Only bookmarks files exported from Microsoft Edge, Google Chrome and Safari are supported now.
#'
#' @param file bookmarks file in html format. Default: first html file in the current directory
#' @param out output markdown file's name, without suffix
#' @param safari whether this file is exported from MacOS Safari, default FALSE
#' @param add.rules additional rules for some strange items. It should be a tibble or data frame, with first column being `pattern`, second being `replacement` (used in function `stringr::str_replace_all()`). All items should be written in string form of regular expression.
#'
#' @importFrom stringr str_replace str_replace_all
#' @importFrom utils read.table write.table
#'
#' @author Yujie Liu
#' @export
#'
#' @examples
#'
#' ### Example additional rules
#'
#' add.rules <- data.frame(
#'   pattern = c('.+HREF="(.+)" (LAST|ADD).+>(.+)',
#'               "test"),
#'   replacement = c("* [\\3](\\1)",
#'                   "TEST")
#' )
#'



bm2md <-
  function(file = dir(pattern = ".html$")[1],
           out = "bookmarks",
           safari = FALSE,
           add.rules = NULL) {

    html <-
      read.table(
        file,
        sep = "\t",
        quote = "",
        comment.char = "",
        fileEncoding = "UTF-8"
      )

    if (safari) {
      html <- html[-(1:3), 1]

      for (i in 1:nrow(html)) {
        # Basic rules
        html[i] <- stringr::str_replace(html[i],
                                        '<DT><A HREF="(.+)">(.+)</A>',
                                        "* [\\2](\\1)")
        html[i] <-
          stringr::str_replace(html[i], '<Title>(.+)</Title>', "# \\1")
        html[i] <-
          stringr::str_replace(html[i], '.+>(.+)</H3>', "## \\1")
        html[i] <- stringr::str_replace(html[i], '&amp;', "&")
        html[i] <- stringr::str_replace(html[i],
                                        '(</?DL><p>)|(<H1>.+</H1>)|(</?HTML>)', "")

        # Additional rules
        if (!is.null(add.rules)) {
          for (j in 1:nrow(add.rules)) {
            html[i] <- stringr::str_replace_all(html[i],
                                                add.rules[j],
                                                add.rules[[2]][j])
          }
        }
      }

    } else {
      html <- html[-(1:5), 1]

      for (i in 1:length(html)) {
        # Basic rules
        html[i] <- stringr::str_replace(html[i],
                                        '.+HREF="(.+)" (LAST|ADD).+>(.+)</A>',
                                        "* [\\3](\\1)")
        html[i] <-
          stringr::str_replace(html[i], '<TITLE>(.+)</TITLE>', "# \\1")
        html[i] <-
          stringr::str_replace(html[i], '.+>(.+)</H3>', "## \\1")
        html[i] <- stringr::str_replace(html[i], '&amp;', "&")
        html[i] <- stringr::str_replace(html[i],
                                        '(</?DL><p>)|(<H1>Bookmarks</H1>)', "")
        # Rule for "zhihu"
        html[i] <- stringr::str_replace(html[i],
                                        '.+HREF="(.+zhihu.+)" (LAST|ADD).+>(.+)',
                                        "* [\\3](\\1)")
        # Additional rules
        if (!is.null(add.rules)) {
          for (j in 1:nrow(add.rules)) {
            html[i] <- stringr::str_replace_all(html[i],
                                                add.rules[j],
                                                add.rules[[2]][j])
          }
        }
      }

    }

    write.table(
      html,
      file = paste0(out, ".md"),
      col.names = FALSE,
      row.names = FALSE,
      quote = FALSE,
      fileEncoding = "UTF-8"
    )

    return("File Saved!")

  }
