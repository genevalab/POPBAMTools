#' Read and annotate popbam output
#'
#' This function reads \code{POPBAM} output (nucdiv, halpo, ld and diverge functions) and creates a appropriately labelled dataframe
#' @param \code{file} filename of popbam output
#' @keywords POPBAM, parse output
#' @export
#' @examples
#' read_popbam()

read_popbam <- function(file){
  dat <- read.table(file, stringsAsFactors=F)
  cols <- ncol(dat)
  if (is.numeric(dat[1,4]))
  {
    nums <- dat[,c(1,2,3,4,seq(6,cols,by=2))]
    labs <- dat[1,seq(5,cols,by=2)]
    labs <- sub(":","",labs)
    labs <- gsub("\\[","_", colnames(labs))
    labs <- gsub("\\]","", colnames(labs))
    colnames(nums) <- c("chr", "start", "end","nsites",labs)           
  }
  else{
    nums <- dat[,c(1,2,3,seq(5,cols,by=2))]
    labs <- dat[1,seq(4,cols,by=2)]
    labs <- sub(":","",labs)
    labs <- gsub("\\[","_", colnames(labs))
    labs <- gsub("\\]","", colnames(labs))
    
    colnames(nums) <- c("chr", "start", "end",labs)
  }
  return(nums)  
}