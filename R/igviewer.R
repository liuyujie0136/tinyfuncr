#' Web-based IGViewer
#'
#' @description Use igv.js to show tracks in a html file. For more information, please visit: https://github.com/igvteam/igv.js
#'
#' @param ref a one-row data frame containing Reference Genome's information: name in the first column, fastaURL in the second, indexURL in the third. Note: URL can be an address to local files.
#' @param refmake whether to make reference information from function makeigvref(), default FALSE
#' @param refid if you set refmake as TRUE, provide reference genome id
#' @param anno a one-row data frame containing Annotation Track's information: name in the first column, url in the second. Note: if you set refmake as TRUE, you don't need to provide annotation information.
#' @param align a multi-row data frame containing Alignment Tracks' information: name in the first column, url in the second, indexURL in the third. Note: set stringsAsFactors = FALSE in data frame is preferred.
#' @param var a multi-row data frame containing Variant Tracks' information: name in the first column, url in the second, indexURL in the third
#' @param wig a multi-row data frame containing WIG Tracks' information: name in the first column, url in the second
#' @param seg a multi-row data frame containing Segmented Copy Number Tracks' information: name in the first column, url in the second
#' @param gwas a multi-row data frame containing GWAS Tracks' information: name in the first column, url in the second
#' @param out output file name, without suffix
#'
#' @importFrom utils write.table
#' @importFrom rvcheck o
#' @author Yujie Liu
#' @export
#'
#' @examples
#' align <-
#'   data.frame(
#'     name = c("THA1", "THA2"),
#'     url = c(
#'       paste0(
#'         "file:///",
#'         system.file(
#'           "extdata",
#'           "THA1_sorted.bam",
#'           package = "tinyfuncr",
#'           mustWork = TRUE
#'         )
#'       ),
#'       paste0(
#'         "file:///",
#'         system.file(
#'           "extdata",
#'           "THA2_sorted.bam",
#'           package = "tinyfuncr",
#'           mustWork = TRUE
#'         )
#'       )
#'     ),
#'     index = c(
#'       paste0(
#'         "file:///",
#'         system.file(
#'           "extdata",
#'           "THA1_sorted.bam.bai",
#'           package = "tinyfuncr",
#'           mustWork = TRUE
#'         )
#'       ),
#'       paste0(
#'         "file:///",
#'         system.file(
#'           "extdata",
#'           "THA2_sorted.bam.bai",
#'           package = "tinyfuncr",
#'           mustWork = TRUE
#'         )
#'       )
#'     ),
#'     stringsAsFactors = FALSE
#'   )
#'
#' igviewer(refmake = TRUE,
#'          refid = "sacCer3",
#'          align = align)


igviewer <-
  function(ref,
           refmake = FALSE,
           refid = NULL,
           anno = NULL,
           align = NULL,
           var = NULL,
           wig = NULL,
           seg = NULL,
           gwas = NULL,
           out = "IGViewer") {

    header <- data.frame(
      data = c(
        '<html lang="en">',
        '<head>',
        '<meta charset="UTF-8">',
        '<meta name="viewport" content="width=device-width, initial-scale=1.0">',
        '<title>IGViewer</title>',
        '<script src="https://cdn.jsdelivr.net/npm/igv@2.7.2/dist/igv.js"></script>',  # OR:https://igv.org/web/release/2.7.1/dist/igv.min.js
        '</head>',
        '<body>',
        '<div id="igv-div"></div>',
        '</body>',
        '<script>',
        'var igvDiv = document.getElementById("igv-div");',
        'var options = {',
        'showNavigation: true,',
        'showRuler: true,'
      )
    )

    if (refmake) {
      if (!is.null(refid)) {
        refer <- data.frame(data = makeigvref(refid))
      } else {
        return("ERROR: No reference ID!")
      }
    } else {
      refer <- data.frame(data = c(
        'reference: {',
        paste0('name: "', ref[1, 1], '",'),
        paste0('fastaURL: "', ref[1, 2], '",'),
        paste0('indexURL: "', ref[1, 3], '"'),
        '},'
      ))
    }

    annoter <- NULL
    if (!is.null(anno)) {
      annoter <- data.frame(data = c(
        '{',
        'type: "annotation",',
        paste0('name: "', anno[1,1], '",'),
        paste0('url: "', anno[1,2], '",'),
        'indexed: false',
      # paste0('indexURL: "', anno[[3]], '",'),
      # paste0('format: "', tail(strsplit(anno[[2]], "\\.")[[1]], 1), '"'),
        '},'
      ))
    }

    aligner <- NULL
    if (!is.null(align)) {
      for (i in 1:nrow(align)) {
        aligner <- rbind(aligner, data.frame(
          data = c(
            '{',
            'type: "alignment",',
            paste0('name: "', align[i, 1], '",'),
            paste0('url: "', align[i, 2], '",'),
            paste0('indexURL: "', align[i, 3], '",'),
          # paste0('format: "', tail(strsplit(align[i, 2], "\\.")[[1]], 1), '",'),
            'visibilityWindow: 1000000',
            '},'
          )
        ))
      }
    }

    varyer <- NULL
    if (!is.null(var)) {
      for (i in 1:nrow(var)) {
        varyer <- rbind(varyer, data.frame(
          data = c(
            '{',
            'type: "variant",',
            paste0('name: "', var[i, 1], '",'),
            paste0('url: "', var[i, 2], '",'),
            paste0('indexURL: "', var[i, 3], '",'),
          # paste0('format: "', tail(strsplit(var[i, 2], "\\.")[[1]], 1), '"'),
            '},'
          )
        ))
      }
    }

    wigger <- NULL
    if (!is.null(wig)) {
      for (i in 1:nrow(wig)) {
        wigger <- rbind(wigger, data.frame(
          data = c(
            '{',
            'type: "wig",',
            paste0('name: "', wig[i, 1], '",'),
            paste0('url: "', wig[i, 2], '",'),
          # paste0('format: "', tail(strsplit(wig[i, 2], "\\.")[[1]], 1), '"'),
            '},'
          )
        ))
      }
    }

    segger <- NULL
    if (!is.null(seg)) {
      for (i in 1:nrow(seg)) {
        segger <- rbind(segger, data.frame(
          data = c(
            '{',
            'type: "seg",',
            'format: "seg",',
            'indexed: false,',
            paste0('name: "', seg[i, 1], '",'),
            paste0('url: "', seg[i, 2], '"'),
            '},'
          )
        ))
      }
    }

    gwasser <- NULL
    if (!is.null(gwas)) {
      for (i in 1:nrow(gwas)) {
        gwasser <- rbind(gwasser, data.frame(
          data = c(
            '{',
            'type: "gwas",',
            paste0('name: "', gwas[i, 1], '",'),
            paste0('url: "', gwas[i, 2], '",'),
            'indexed: false,',
          # paste0('format: "', tail(strsplit(gwas[i, 2], "\\.")[[1]], 1), '"'),
            '},'
          )
        ))
      }
    }


    tracker <- rbind(data.frame(data = 'tracks: ['), annoter, aligner, varyer, wigger, segger, gwasser)

    tailer <- data.frame(
      data = c(
        ']',
        '};',
        'igv.createBrowser(igvDiv, options)',
        '.then(function (browser) {',
        'console.log("Created IGV browser");',
        '}',
        ')',
        '</script>',
        '</html>'
      )
    )

    html <- rbind(header, refer, tracker, tailer)

    outfilename <- paste0(out, ".html")

    write.table(
      html,
      outfilename,
      quote = FALSE,
      row.names = FALSE,
      col.names = FALSE,
      fileEncoding = "UTF-8"
    )

    o(outfilename)

  }
