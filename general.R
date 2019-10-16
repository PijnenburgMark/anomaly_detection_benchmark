# general.R


scaleminmax <- function(DF){
  # removes last column (label) and performs min/max scaling
  # DS   -  dataframe with all (but the last) columns being numeric and the last
  #         column containing the label
  # Output is the original dataframe scaled, minus the last column.
  dsun <- DF[,-ncol(DF)]
  outds <- apply(dsun, 2, function(x) (x - min(x))/(max(x)-min(x)))
  return(as.matrix(outds))
}


discr_thermometer <- function(DF, NBIN){
  # This function discretizes the columns of DF in NBIN bins based on equal width 
  # thermometer discretization.
  # DF   - A data frame, including the label of the outliers (which are then removed)
  # NBIN - Integer representing the number of bins
  ncol <- ncol(DF) - 1
  outds <- matrix(0, nrow = nrow(DF), ncol = NBIN*ncol)
  # first min max transformation
  dfmm <- data.frame(sapply(DF[,-ncol(DF)], function(x) (x - min(x))/diff(range(x))))
  # now determine boundary points
  boundaryp <- (1:(NBIN))/(NBIN + 1)
  # make for each column dummy variables and glue them together.
  colnr = 1
  for(coldf in 1:ncol(dfmm)){
    for(cutp in boundaryp){
      outds[, colnr] <- as.numeric(dfmm[,coldf] >= cutp)
      colnr <- colnr + 1
    }
  }
  return(outds)
}

# tic toc function for measuring the execution time
tic <- function(gcFirst = TRUE, type=c("elapsed","user.self","sys.self")){
  type <- match.arg(type)
  assign(".type", type, envir=baseenv())
  if(gcFirst) gc(FALSE)
  tic <- proc.time()[type]
  assign(".tic", tic, envir=baseenv())
  invisible(tic)
}

toc <- function(){
  type <- get(".type", envir=baseenv())
  toc <- proc.time()[type]
  tic <- get(".tic", envir=baseenv())
  print(toc-tic)
  invisible(toc)
  return(toc-tic)
}

#example: tic(); invisible(qr(matrix(runif(1e6), nrow = 1e3))); toc()

