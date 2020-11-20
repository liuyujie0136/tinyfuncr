#' Save plot to device with various formats
#'
#' @description Save the current graph or a specific plot item to device.
#'
#' @param plot a given plot object or current plot (default).
#' @param type file type, in "pdf", "eps", "svg", "ppt(x)", "doc(x)", "png", "jp(e)g", "tif", "html". Note: "html" type work for ggplot2 object only.
#' @param filename file name, without suffix. Default: combines "Rplot" and current time together
#' @param height graph height
#' @param width graph width
#' @param open whether to open graph file after saving, default FALSE
#'
#' @import tidyverse
#' @import eoffice
#' @import export
#' @import rvcheck
#'
#' @author Yujie Liu
#' @export
#'
#' @examples
#' df <- data.frame(x = 1:5, y = 6:10)
#' plot(df)
#' g <- ggplot(df) + geom_point(aes(x, y))
#' plotsave(open = TRUE)
#' plotsave(g, "pptx", "myggplot")



plotsave <- function(plot = NULL, type = "pdf", filename = NULL,
                     height = 6, width = 6, open = FALSE){

  if (is.null(filename)){
    t <- as.character(Sys.time())
    t <- gsub(" |:", "-", t)
    fname <- paste0("Rplot-", t, ".", type)
  } else {
    fname <- paste0(filename, ".", type)
  }

  if (type %in% c("pdf", "eps", "svg")){
    savefun <- function(...) graph2vector(...)
  } else if (type %in% c("ppt", "doc", "pptx", "docx")){
    type <- switch(type,
                   "pptx" = "ppt",
                   "docx" = "doc",
                   ... = type)
    savefun <- function(...) graph2office(...)
  } else if (type %in% c("png", "jpg", "jpeg", "tif")){
    type <- switch(type,
                   "jpeg" = "jpg",
                   ... = type)
    savefun <- function(...) graph2bitmap(...)
  } else if (type == "html"){
    if (is.null(plot)){
      tohtml(filename = fname)
    } else {
      tohtml(figure = plot, filename = fname)
    }
    return("File saved!")
  } else {
    return("ERROR! Type not supported!")
  }

  savefun(x = plot, file = fname, type = type,
            height = height, width = width)

  if (open){
    o(fname)
  }

  return("File saved!")
}
