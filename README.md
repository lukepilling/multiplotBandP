multiplotBandP
==============

Function to create a series of plots to compare a number of parallel regression screens to one another

##Description
The function takes as input the results of a regression screen (e.g. a series of analysis of genomic data against an outcome) from multiple cohorts/populations, and compares these to one another. The results of each cohort must be in a data.frame, with the P-values and Estimates in separate columns (in that order). The data.frame's must then be in a list for the function.

You must order the data.frame's before providing them to the function, to make sure the first "row" is the same variable for each data.frame, otherwise you are not comparing like for like.

It is recommended you transform the P-values prior to inputting to the function, for example -log10 transformation, so that the "extreme" values can be clearly seen.

## Inputs/options
* r     ::  {REQUIRED} must be a "list" of data.frame's, each data.frame must have two columns; P-values and Estimates
* labs  ::  {REQUIRED} labels for the individual analyses; must be of same length as number of data.frame's
* main  ::  {optional} title of plot

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

## transform P-values using -log10
r1.sub$P <- -log(r1.sub$P, 10)
r2.sub$P <- -log(r2.sub$P, 10)
r3.sub$P <- -log(r3.sub$P, 10)
r4.sub$P <- -log(r4.sub$P, 10)

## wrap into a list
r <- list( r1.sub , r2.sub , r3.sub , r4.sub )

## create labels
r.lab <- c("Analysis.1", "Analysis.2", "Analysis.3", "Analysis.4")

## create plot
png("random.plot.png", width=700, height=700)

multiplotBandP( r , r.lab )

dev.off()
```
![](http://s30.postimg.org/lnynragv5/random_plot.png)
