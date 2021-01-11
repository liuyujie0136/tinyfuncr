#' Open file with system default application
#'
#' @description Open file with system default application.
#'
#' @param file file to open
#'
#' @author Yujie Liu
#' @export
#'


fopen <- function(file) {

  os <- Sys.info()[1]

  if (os == "Windows") {
    cmd <- paste("start", file)
    shell(cmd)
  } else if (os == "Linux") {
    cmd <- paste("xdg-open", file, "&")
    system(cmd)
  } else if (os == "Darwin") {
    cmd <- paste("open", file)
    system(cmd)
  }

}
