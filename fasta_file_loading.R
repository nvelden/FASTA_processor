library(tidyverse)
#BiocManager::install("Biostrings")

setwd("C:/R/Github repositories/FASTA_processor")


#Convert FASTA to df
fasta2df <- function(data){
  bs_fasta <- Biostrings::readBStringSet(data)
  if(length(bs_fasta) == 0){
    return("No sequences detected in file.")
  }
  df_fasta <- 
    data.frame(
      width=as.integer(bs_fasta@ranges@width),
      names=as.character(bs_fasta@ranges@NAMES),
      seq=as.character(bs_fasta)
    )
  rownames(df_fasta) <- NULL
  return(df_fasta)
}

#Duplicate sequences
duplicate_seq <- function(data) {
  seq <- data %>% filter(duplicated(seq))
  return(seq)
}
duplicate_names <- function(data) {
  names <- data %>% filter(duplicated(names))
  return(names)
}

duplicate_names(df_fasta)



df_fasta <- fasta2df("fasta_error.fasta")
df_fasta %>% filter(duplicated_seq(seq))  







