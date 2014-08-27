#' Calculate Gmin
#'
#' Gmin is the ratio of minimum between population divergence and average between population divergence. This function reads \code{POPBAM haplo} dataframe created by \code{read_popbam() and returns new dataframe with Gmin and Z-score appended to original dataframe}
#' @param \code{data}  \code{POPBAM haplo} dataframe 
#' @keywords POPBAM, Gmin
#' @export
#' @examples
#' calc_gmin()


calc_gmin <- function(data){
  pops <- sqrt(ncol(data)-4)
  for (i in 1:((pops*(pops-1))/2)){
    gmin <- NULL
    pi1 <- NULL
    pi2 <- NULL
    gmin <- data[,(2*i+4+pops)]/data[,(2*i+3+pops)]
    gmin.z <- scale(gmin)
    gmin.p <- pnorm(gmin.z)
    #fst <- #ADD FST functionality - THIS IS KINDA DIFFICULT
    data <- cbind(data,gmin,gmin.p)
    colnames(data)[(ncol(data)-1)] <- paste("G",colnames(data)[(2*i+4+pops)],sep="")
    colnames(data)[(ncol(data))] <- paste("G",colnames(data)[(2*i+4+pops)],".p",sep="")
  }  
  return(data)
}