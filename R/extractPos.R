#' Extract All Elements from GFF3 File
#'
#' @description Extract all elements (promoter, UTR, CDS, intron) of each gene from GFF3 file into BED format
#'
#' @param gff_file GFF3 file
#' @param chr_len_file file containing chromosome length
#' @param bed_file BED file for output
#'
#' @import magrittr
#' @importFrom rtracklayer import.gff
#' @importFrom GenomicRanges promoters
#'
#' @author Yujie Liu
#' @export
#'

extractPos <- function(gff_file, chr_len_file, bed_file) {
  # load data
  ann <- rtracklayer::import.gff(gff_file)
  chr_len <-
    read.table(
      chr_len_file,
      header = F,
      sep = "\t",
      quote = "",
      col.names = c("Chr", "Length")
    )
  
  # extract promoter info
  promoters <-
    GenomicRanges::promoters(ann[ann$type == "gene"], upstream = 1500, downstream = 0) %>% as.data.frame() %>% .[c("seqnames", "start", "end", "strand", "ID")]
  
  # convert to df
  ann %<>% as.data.frame()
  ann$Parent %<>% as.character()
  
  # extract core info
  genes <-
    ann[ann$type == "gene", c("seqnames", "start", "end", "strand", "ID")]
  utr5 <-
    ann[ann$type == "five_prime_UTR", c("seqnames", "start", "end", "strand", "Parent")]
  utr3 <-
    ann[ann$type == "three_prime_UTR", c("seqnames", "start", "end", "strand", "Parent")]
  CDS <-
    ann[ann$type == "CDS", c("seqnames", "start", "end", "strand", "Parent")]
  
  # get intron info
  exon <-
    ann[ann$type == "exon", c("seqnames", "start", "end", "strand", "Parent")]
  introns <- data.frame()
  for (tr_name in unique(exon$Parent)) {
    exon_this_tr <- exon[exon$Parent == tr_name, ]
    if (nrow(exon_this_tr) > 1) {
      intron_this_tr <-
        data.frame(
          exon_this_tr[1:nrow(exon_this_tr) - 1, "seqnames"],
          exon_this_tr[1:nrow(exon_this_tr) - 1, "end"] + 1,
          exon_this_tr[2:nrow(exon_this_tr), "start"] - 1,
          exon_this_tr[1:nrow(exon_this_tr) - 1, "strand"],
          exon_this_tr[1:nrow(exon_this_tr) - 1, "Parent"]
        )
      colnames(intron_this_tr) <-
        c("seqnames", "start", "end", "strand", "Parent")
      introns %<>% rbind(intron_this_tr)
    }
  }
  
  # print pos data
  print_pos <- function(df, prefix, file) {
    for (idx in 1:nrow(df)) {
      chr <- df[idx, 1] %>% as.character()
      s <- df[idx, 2] %>% as.integer() - 1  # bed format
      e <- df[idx, 3] %>% as.integer()
      st <- df[idx, 4] %>% as.character()
      base_name <- df[idx, 5] %>% as.character()
      name <- paste0(prefix, "_", base_name)
      
      # avoid index out of range
      if (s < 0) {
        s <- 0
      }
      if (e > chr_len[chr_len$Chr == chr, 2]) {
        e <- chr_len[chr_len$Chr == chr, 2]
      }
      
      # wirte pos
      write_file(paste0(chr, "\t", s, "\t", e, "\t", name, "\t.\t", st, "\n"),
                 file,
                 append = TRUE)
    }
  }
  
  print_pos(promoters, "Promoter", bed_file)
  print_pos(utr5, "5UTR", bed_file)
  print_pos(utr3, "3UTR", bed_file)
  print_pos(CDS, "CDS", bed_file)
  print_pos(introns, "Intron", bed_file)
}
