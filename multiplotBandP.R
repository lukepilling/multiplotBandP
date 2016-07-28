##################################################################################################################
#### Function to create a series of plots to compare a number of parallel regression screens to one another
#### Luke C. Pilling  --  L.Pilling@exeter.ac.uk
####
#### v 0.160728

#### Description
#### =====
#### The function takes as input the results of a regression screen (e.g. a series of analysis of genomic data against an outcome)
####   from multiple cohorts/populations, and compares these to one another. The results of each cohort must be in a data.frame, 
####   with the P-values and Estimates in separate columns (in that order). The data.frame's must then be in a list for the function.
####
#### You must order the data.frame's before providing them to the function, to make sure the first "row" is the same variable for each
####   data.frame, otherwise you are not comparing like for like.
####
#### It is recommended you transform the P-values prior to inputting to the function, for example -log10 transformation, so that
####   the "extreme" values can be clearly seen.

#### Inputs/options
#### ==============
#### r       ::  {REQUIRED} must be a "list" of data.frame's, each data.frame must have two columns; P-values and Estimates
#### labs    ::  {REQUIRED} labels for the individuals analyses; must be of same length as number of data.frame's
#### main    ::  {optional} title of plot {Default="Comparison of results from regression screens"}
#### corr    ::  {optional} "pearson", "spearman" or "kendall" correlation? {Default="pearson"}
#### corr.p  ::  {optional} Include P-value for correlation? {Default=FALSE}
#### z.score ::  {optional} Z-transform coefficients {Default=FALSE}
#### neg.log ::  {optional} Negative-log10 transform P-values {Default=FALSE}


multiPlotBandP <- function(r=stop("Must provide a list of data.frame's of regression results, column.1=Pvals, column.2=Effects\n"),
                           labs=stop("Must provide a vector of labels that correspond to the data.frame's in list [r]\n"),
                           main="Comparison of results from regression screens",
                           corr="pearson",
                           corr.p=FALSE,
                           z.score=FALSE,
                           neg.log=FALSE
                          )
{
    #### check format of inputs
    if (class(r) != "list")     stop("Input [r] must be a list of data.frames's\n")
    if (class(labs) != "character" & class(labs) != "vector")  stop("Input [labs] must be a character vector\n")
    if (length(r) != length(labs))  stop("Number of labels must match number of data.frame's\n")
    if (corr != "pearson"  &  corr != "spearman"  &  corr != "kendall")  stop("[corr] must be either \"pearson\", \"spearman\" or \"kendall\"\n")
    if (class(corr.p) != "logical")  stop("[corr.p] must be either TRUE or FALSE\n")
    if (class(z.score) != "logical")  stop("[z.score] must be either TRUE or FALSE\n")
    if (class(neg.log) != "logical")  stop("[neg.log] must be either TRUE or FALSE\n")
    
    #### check data.frame's in the list
    for (i in 1:length(r))  
    {
      if (class(r[[i]]) != "data.frame")  stop(paste("List number [", i, "] is not a data.frame\n", sep=""))
      if (dim(r[[i]])[2] < 2)             stop(paste("List number [", i, "] does not have enough columns\n", sep=""))
      if (dim(r[[i]])[2] > 2)             print(paste("WARNING: List number [", i, "] has >2 columns; I will only use the first 2", sep=""))
    }
    
    #### set-up plotting parameters
    op <- par(no.readonly = TRUE)
    par(mfrow = c(length(r), length(r)))
    par(mar = c(0.2, 0.2, 0.2, 0.2))
    par(mgp = c(1.5, 0.5, 0))       ## This accepts a vector of 3 elements specifying the distance in text lines between the  
                                    ##   figure (data area) margin and the axis title, axis labels and axis line respectively
    par(oma = c(2, 2, 5, 2))        ## outer margin for title
    
    #### loop through data frames for comparisons
    for (i in 1:length(r)) 
    {
      for (j in 1:length(r)) 
      {
        if (i == j)  ## if x and y axis are same DF, then plot the label for the DF instead
        {
          plot.new()
          legend("center", labs[i], col="black", ncol=1, bty ="n", cex=1.5)
          
          coefLab <- "Effect size"
          if (z.score)  coefLab <- paste(coefLab, "(Z-scored)", sep=" ")
          legend("topright", coefLab, col="black", ncol=1, bty ="n", cex=1.1)
          
          pvalLab <- "P-values"
          if (neg.log)  pvalLab <- paste(pvalLab, "(-log10)", sep=" ")
          legend("bottomleft", pvalLab, col="black", ncol=1, bty ="n", cex=1.1)          
        }  else  
        {
          x <- r[[j]][,1]
          y <- r[[i]][,1]
          if (neg.log)
          {
            x <- -log(x, 10)
            y <- -log(y, 10)
          }
          
          if (i < j) ## if we're in the top-right then plot the effects instead of the P-values
          {
            x <- r[[j]][,2]
            y <- r[[i]][,2]
            if (z.score)
            {
              x <- (x - mean(na.omit(x))) / sd(na.omit(x))
              y <- (y - mean(na.omit(y))) / sd(na.omit(y))
            }
          }
                    
          plot(1, xlab="", ylab="", main="", axes=F, cex.axis = 0.9, xlim=c(min(makeOK(x)),max(makeOK(x))), ylim=c(min(makeOK(y)),max(makeOK(y))) )
          rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col ="grey97")
          abline(a=0, b=1, lty=2, col="grey60")
            
          if (i < j) ## if we're in the top-right then also add vertical reference lines
          {
            abline(h=0, lty=2, col="grey60")
    	    abline(v=0, lty=2, col="grey60")
    	    
    	    ## if "last" plot in the row then add "right"  OR  if "top" row then add to "top"
    	    if (i == 1)          axis(3)
    	    if (j == length(r))  axis(4)
          }  else  
          {
    	    ## if "first" plot in the row then add "left"  OR  if "bottom" row then add to "bottom"
    	    if (j == 1)          axis(2)
    	    if (i == length(r))  axis(1)
          }
          
          ## draw points
          points( x, y, cex = 0.4, pch = 16 )
          
          ## plot correlation statistics and line
          cor.dat     <- na.omit(data.frame(x,y))
          correlation <- cor.test(cor.dat[,1], cor.dat[,2], method=corr)

          if (z.score)  abline(a=0, b=correlation$estimate[[1]], lty=1, col="red")  ## if Z-transforming coefficients can just plot the R, otherwise use linear regression
          if (!z.score) abline(lm(cor.dat[,2] ~ cor.dat[,1]), lty=1, col="red")  ## if Z-transforming coefficients can just plot the R, otherwise use linear regression
          
          legend("topleft", paste("R=", signif(correlation$estimate[[1]], 2), sep=""), bty="n", cex=1.1)
          if (corr.p)  legend("bottomright", paste("P=", signif(correlation$p.value, 2), sep=""), bty="n", cex=1.1)
        }
      }
    }
    
    mtext(main, outer = TRUE, side = 3, cex = 1.2, line = 2)
    #mtext("Bottom-left: P-values  |  Top-right: Effect estimates", outer = TRUE, side = 3, cex = 0.9, line = 0.4)
    
    #### resest plotting parameters
    par(op)
}

makeOK <- function(x)
{
    is.na(x) <- is.infinite(x)
    x <- as.vector( na.omit(x) )
    return(x)
}
