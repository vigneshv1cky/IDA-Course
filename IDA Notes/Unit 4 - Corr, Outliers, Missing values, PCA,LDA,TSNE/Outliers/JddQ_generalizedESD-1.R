#generalized ESD test -- adapted from code available on stackoverflow.com


# helper function
# Compute the critical value for ESD Test
esd.critical <- function(alpha, n, i) {
    p = 1 - alpha/(2*(n-i+1))
    t = qt(p,(n-i-1))
    return(t*(n-i) / sqrt((n-i-1+t**2)*(n-i+1)))
}

#main function
removeoutliers = function(y,k=5,alpha=0.05) {

    if (k<1 || k >= length(y))
         stop ("the number of suspected outliers, k, must be in [1,n-1]")
  
    ## Define values and vectors.
    y2 = y
    n = length(y)
    toremove = 0
    tval<-NULL
    ris<-NULL

    ## Compute test statistic until r values have been removed from the sample.
    for (i in 1:k){
        if(sd(y2)==0) break
        ares = abs(y2 - mean(y2))/sd(y2)
        Ri = max(ares)
        y2 = y2[ares!=Ri]

        tval<-c(tval,esd.critical(alpha,n,i))
        ris<-c(ris,Ri)
        ## Compute critical value.
        if(Ri>esd.critical(alpha,n,i))
            toremove = i
    }

    # Values to keep
    if(toremove>0){
        outlierLevel = sort(abs(y-mean(y)),decreasing=TRUE)[toremove]
        o = y[abs(y-mean(y)) >= outlierLevel]
        y = y[abs(y-mean(y)) < outlierLevel]
    }
    
    RVAL <- list(numOutliers=toremove,outliers=o,cleandata=y,critical=tval,teststat=ris)
    return (RVAL) 
}


#example with the Rosner (1983) data

rosner<-c(-0.25, 0.68, 0.94, 1.15, 1.20, 1.26, 1.26,
           1.34, 1.38, 1.43, 1.49, 1.49, 1.55, 1.56,
           1.58, 1.65, 1.69, 1.70, 1.76, 1.77, 1.81,
           1.91, 1.94, 1.96, 1.99, 2.06, 2.09, 2.10,
           2.14, 2.15, 2.23, 2.24, 2.26, 2.35, 2.37,
           2.40, 2.47, 2.54, 2.62, 2.64, 2.90, 2.92,
           2.92, 2.93, 3.21, 3.26, 3.30, 3.59, 3.68,
           4.30, 4.64, 5.34, 5.42, 6.01)

removeoutliers(rosner,10,0.05)   #usage: data, r=number of suspected outliers, alpha

#returns:  number of outliers, outliers, the non-outlying data, 
#          the critical values for each outlier test,
#          the test statistic for each outlier test

