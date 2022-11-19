#' Save plot to device with various formats
#'
#' @description Save the current graph or a specific plot object to device.
#'
#' @param plot a given plot object or current plot (default).
#' @param type file type, in "pdf", "eps", "svg", "ppt(x)", "doc(x)", "png", "jp(e)g", "tif", "html". Note: "html" type will return a plotly object, so it works for ggplot2 object only.
#' @param filename file name, without suffix. Default: combines "Rplot" and current time together
#' @param height graph height, default 5 (especially for plots with legend on the side)
#' @param width graph width, default 6
#' @param open whether to open graph file after saving, default `FALSE`
#' @param ... other arguments used in package "export"
#'
#' @import export
#' @import eoffice
#'
#' @author Yujie Liu
#' @export
#'
#' @examples
#' #df <- data.frame(x = 1:5, y = 6:10)
#' #plot(df)
#' #g <- ggplot(df) + geom_point(aes(x, y))
#' #plotsave(open = TRUE)
#' #plotsave(g, "pptx", "myggplot")



plotsave <- function(plot = NULL, type = "pdf", filename = NULL,
                     height = 5, width = 6, open = FALSE, ...){

  if (is.null(filename)){
    t <- as.character(Sys.time())
    t <- gsub(" |:", "-", t)
    fname <- paste0("Rplot-", t)
  } else {
    fname <- filename
  }

  if (type %in% c("pdf", "eps", "svg")){
    savefun <- function(...) graph2vector(...)
  } else if (type %in% c("ppt", "doc", "pptx", "docx")){
    type <- switch(type,
                  "pptx" = "ppt",
                  "docx" = "doc",
                  type)
    savefun <- function(...) graph2office(...)
  } else if (type %in% c("png", "jpg", "jpeg", "tif")){
    type <- switch(type,
                   "jpeg" = "jpg",
                   type)
    savefun <- function(...) graph2bitmap(...)
  } else if (type == "html"){
    fname <- paste0(fname, ".html")
    if (is.null(plot)){
      tohtml(filename = fname)
    } else {
      tohtml(figure = plot, filename = fname)
    }
    if (open){
      fopen(fname)
    }
    return("File saved!")
  } else {
    return("ERROR! Type not supported!")
  }

  savefun(x = plot, file = fname, type = type,
            height = height, width = width, ...)

  if (open){
    fopen(dir(pattern = fname))
  }

  return("File saved!")
}
