multiplotBandP
==============

Function to create a series of plots to compare a number of parallel regression screens to one another

##Description
The function takes as input the results of a regression screen (e.g. a series of analysis of genomic data against an outcome) from multiple cohorts/populations, and compares these to one another. The results of each cohort must be in a data.frame, with the P-values and Estimates in separate columns (in that order). The data.frame's must then be in a list for the function.

You must order the data.frame's before providing them to the function, to make sure the first "row" is the same variable for each data.frame, otherwise you are not comparing like for like.

It is recommended you transform the P-values prior to inputting to the function, for example -log10 transformation, so that the "extreme" values can be clearly seen.

## Inputs/options
* r       ::  {REQUIRED} must be a "list" of data.frame's, each data.frame must have two columns; P-values and Estimates
* labs    ::  {REQUIRED} labels for the individuals analyses; must be of same length as number of data.frame's
* main    ::  {optional} title of plot {Default="Comparison of results from regression screens"}
* corr    ::  {optional} "pearson", "spearman" or "kendall" correlation? {Default="pearson"}
* corr.p  ::  {optional} Include P-value for correlation? {Default=FALSE}
* z.score ::  {optional} Z-transform coefficients {Default=FALSE}
* neg.log ::  {optional} Negative-log10 transform P-values {Default=FALSE}

#### Note on P-values
For many analyses of this nature (e.g. 48803 probes on an Illumina microarray) a small correlation can be very significant, so the P-value may be reported as "0" from cor.test() even if the correlation seems quite weak.


## Example
```
## load data
r1 <- read.csv("1.random-analysis-results.txt", sep=",", header=T)
r2 <- read.csv("2.random-analysis-results.txt", sep=",", header=T)
r3 <- read.csv("3.random-analysis-results.txt", sep=",", header=T)
r4 <- read.csv("4.random-analysis-results.txt", sep=",", header=T)

## create list of N results DF's with only the 2 columns we want comparing: bottom-left[1] and top-right [2]
r1.sub <- r1[ , c("P", "Fx") ]
r2.sub <- r2[ , c("P", "Fx") ]
r3.sub <- r3[ , c("P", "Fx") ]
r4.sub <- r4[ , c("P", "Fx") ]

## wrap into a list
r <- list( r1.sub , r2.sub , r3.sub , r4.sub )

## create labels
r.lab <- c("Analysis.1", "Analysis.2", "Analysis.3", "Analysis.4")

## create plot
png("random.plot.png", width=700, height=700)

multiplotBandP( r,           ## provide list of data.frames (one per analysis)
                r.lab,       ## provide labels for each analysis
                neg.log=T    ## transform P-values using -log10
                )

dev.off()
```
![](http://s15.postimg.org/n1nyfnafv/random_plot.png)
