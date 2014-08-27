#' Read popbam trees
#'
#' This function reads \code{POPBAM} tree output and creates a \code{multiphylo} object with aligmnent details added to tree details
#' @param \code{x} filename of popbam tree output
#' @keywords POPBAM, trees
#' @export
#' @examples
#' read_popbam_trees()


read_popbam_trees <- function(x){
  require(ape)
  con <- pipe(paste("cut -f5 ",x,sep=""))
  trees <- read.tree(con)
  close(con)
  deets <- read.table(pipe(paste("cut -f1,2,3,4 ",x,sep="")),stringsAsFactors=F)
  for (i in 1:length(trees)){
    trees[[i]]$chr <- deets[i,1]
    trees[[i]]$start <- deets[i,2]
    trees[[i]]$end <- deets[i,3]
    trees[[i]]$nsites <- deets[i,4]
  }  
  return(trees)
}
