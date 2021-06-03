#' Calculate gene density on chromosome
#'
#' @description Calculate density of genes, CDS, mRNAs, tRNAs, etc on chromosome from a given GFF file with a slide window
#'
#' @param gff_file GFF file
#' @param karyotype_info tab-separated file (or a data frame) containing karyotype information, with first column being "Chr", second being "Start", third being "End". File should NOT have header.
#' @param feature interested feature to calculate, should be in GFF file
#' @param window length of each slide window, defaut 100kb
#'
#' @importFrom tidyr separate
#' @author Yujie Liu
#' @export
#'


calcDensityGFF <- function(gff_file,
                           karyotype_info,
                           feature = "gene",
                           window = 1e5) {
  # read in files
  gff <- read_tcsv(gff_file, header = FALSE, comment.char = "#")
  if (is.character(karyotype_info)) {
    karyotype <- read_tcsv(karyotype_info, header = FALSE)
  } else {
    karyotype <- karyotype_info
  }

  # select only interested feature
  all_chrs <- karyotype[[1]]
  gff <- gff[gff[[1]] %in% all_chrs & gff[[3]] == feature, ]

  # make output data frame
  density <- data.frame()

  # loop against each chromosome
  for (chr in all_chrs) {
    chr_end <- karyotype[karyotype[[1]] == chr, 3]

    # count, but only consider the start of each gene
    tmp <-
      data.frame(table(cut(gff[gff[[1]] == chr, 4],
                           breaks = c(
                             seq(0, chr_end, window),
                             chr_end
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

    # add chr info
    tmp$Chr <- chr

    # reorder columns
    tmp <- tmp[c(4, 1:3)]
    colnames(tmp) <- c("Chr", "Start", "End", "Count")

    # modify last end_pos to chr_end
    tmp[nrow(tmp), "End"] <- chr_end

    # combine
    density <- rbind(density, tmp)
  }

  return(density)
}
