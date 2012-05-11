#!/usr/bin/Rscript
# 
# OUTSKEWER detect outliers in numeric samples and time series.
#
# S. Heymann, M. Latapy, and C. Magnien. "Outskewer: Using Skewness to Spot
# Outliers in Samples and Time Series", submitted to ASONAM'12.
# 
# Version: 1.0   (2012 April)
#
# Copyright (C) 2012 Sébastien Heymann <sebastien.heymann@lip6.fr>
# see LICENSE.txt
# see http://outskewer.sebastien.pro
# 
# Usage: source("path/to/outskewer.R")
#
# DEBUG mode:
# options(error=utils::recover)


Skewness <- function(x, verbose = FALSE) {
    # Compute the sample skewness for a distribution of values.
    #
    # Args:
    #   x: Vector of numbers whose skewness is to be calculated.
    #   verbose: If TRUE, prints sample skewness; if not, not. Default is FALSE.
    #
    # Returns:
    #   The skewness of x.
    if (missing(x))
        stop("Argument x is missing.")
    if (is.null(x))
        stop("Argument x should not be null.")
    
    n <- length(x)
    if (n < 3)
        skew <- NA
    else {
        m <- mean(x)
        s <- sd(x)
        skew <- (n / ((n-1) * (n-2))) * sum(((x - m)/s)^3)
    }
    if (verbose)
        cat("Sample skewness = ", skew, ".\n", sep = "")
    return(skew)
}

SkewnessSignature <- function(x) {
    # Compute half of the skewness signature for a distribution of values.
    #
    # Args:
    #   x: Vector of numbers whose skewness signature is to be calculated.
    #
    # Returns:
    #   The half of the skewness signature of x.
    if (missing(x))
        stop("Argument x is missing.")
    if (is.null(x))
        stop("Argument x should not be null.")
    
    i <- 1
    j <- length(x)
    half.n <- floor(length(x) / 2)
    signature <- rep(NA, half.n)
    h <- half.n
    
    skew <- Skewness(x)
    # Sort with index.return returns a sorted x in sorted$x and the 
    # indices of the sorted values from the original x in sorted$ix
    sorted <- sort(x, decreasing=FALSE, index.return=TRUE)
    new.x <- x[sorted$ix[i:j]]
    while (h > 0 && !is.na(skew)) {
        signature[h] <- skew
        h <- h - 1
        if (skew > 0) {
            # Remove max from x
            j <- j - 1
        }
        else {
            # Remove min from x
            i <- i + 1
        }
        new.x <- x[sorted$ix[i:j]]
        skew <- Skewness(new.x)
    }
    return(signature)
}

IsNotPStable <- function(skew.signature, n, p) {
    # Do we reject H_0 : "the skewness signature is p-stable"?
    #
    # Args:
    #   skew.signature: Vector of half of the skewness signature to be tested.
    #   n: Size of the original vector of values.
    #   p: Number between 0 and 1, proportion of the extremal values.
    #
    # Returns:
    #   The skewness signature of x.
    if (missing(skew.signature))
        stop("Argument skew.signature is missing.")
    if (missing(n))
        stop("Argument n is missing.")
    if (missing(p))
        stop("Argument p is missing.")
    if (n <= 0)
        stop("Argument n must be positive.")
    if (is.null(skew.signature))
        stop("Argument skew.signature should not be null.")
    
    m <- 0
    # Count the number of skewess values until |skew| > p
    if (length(skew.signature) != 0) {
        for (i in seq(along=skew.signature)) {
            skew <- skew.signature[i]
            if (abs(skew) > p)
                break
            m <- m + 1
        }
    }
    reject_h0 <- ( (m / n) < p)  # Decision
    return(reject_h0)
}

Findt <- function(x, skew.signature) {
    # Compute 0.5 - t.
    #
    # Args:
    #   x: Vector of numbers whose largest skewness threshold is to be calculated.
    #   skew.signature: Vector of half of the skewness signature of x.
    #
    # Returns:
    #   The largest skewness threshold of x.
    n <- length(x)
    if (n < 3)
        stop("Argument x should contain more than 2 element.")
    
    from <- length(skew.signature) / n
    by_ <- 1 / n
    return(FindP(x, skew.signature, n, from=from, to=by_, by_=-by_))
}

FindT <- function(x, skew.signature) {
    # Compute 0.5 - T.
    #
    # Args:
    #   x: Vector of numbers whose smallest skewness threshold is to be calculated.
    #   skew.signature: Vector of half of the skewness signature of x.
    #
    # Returns:
    #   The smallest skewness threshold of x.
    n <- length(x)
    if (n < 3)
        stop("Argument x should contain more than 2 element.")
    
    to <- length(skew.signature) / n
    by_ <- 1 / n
    return(FindP(x, skew.signature, n, from=by_, to=to, by_=by_))
}

FindP <- function(x, skew.signature, n, from, to, by_) {
    # Find the first skewness value where
    # the skewness signature of 'x' is p-stable,
    # starting skewness from 'from' to 'to'.
    #
    # Args:
    #   x: Vector of numeric values.
    #   skew.signature: Vector being half of the skewness signature of x.
    #   n: Length of x.
    #   from: largest skewness value possible.
    #   to: smallest skewness value possible.
    #   by_: gap between two skewness values.
    #
    # Returns:
    #   The first skewness value where the skewness signature of 'x' is p-stable,
    #   and a boolean of value TRUE if no value where found.
    if (missing(x))
        stop("Argument x is missing.")
    if (missing(skew.signature))
        stop("Argument skew.signature is missing.")
    if (missing(n))
        stop("Argument n is missing.")
    if (missing(from))
        stop("Argument from is missing.")
    if (missing(to))
        stop("Argument to is missing.")
    if (missing(by_))
        stop("Argument by_ is missing.")
    if (is.null(x))
        stop("Argument x should not be null.")
    if (is.null(skew.signature))
        stop("Argument skew.signature should not be null.")
    
    p <- from
    rejected <- TRUE
    
    for (i in seq(from, to, by_)) {
        reject_h0 <- IsNotPStable(skew.signature, n, i)
        if (!reject_h0) { # if s is p-stable
            p <- i
            rejected <- FALSE
            break
        }
    }
    
    return(list(t = p, 
                rejected = rejected ))
}

RemoveOutliers <- function(x, p) {
    # Remove outliers in the vector 'x'.
    #
    # Args:
    #   x: Vector of numeric values.
    #   p: Number between 0 and 1, proportion of extremal values to remove.
    #
    # Returns:
    #   The vector of values without a proportion p of extremal values.
    if (missing(x))
        stop("Argument x is missing.")
    if (missing(p))
        stop("Argument p is missing.")
    if (is.null(x))
        stop("Argument x should not be null.")
    
    # Sort with index.return returns a sorted x in sorted$x and the 
    # indices of the sorted values from the original x in sorted$ix
    sorted <- sort(x, decreasing=FALSE, index.return=TRUE)
    i <- 1
    j <- length(x)
    new.x <- x[sorted$ix[i:j]]
    skew <- Skewness(x)
    while (!is.na(skew) && abs(skew) > p) {
        if( skew > 0 ) {
            # Remove max from x
            j <- j - 1
            new.x <- x[sorted$ix[i:j]]
        }
        else {
            # Remove min from x
            i <- i + 1
            new.x <- x[sorted$ix[i:j]]
        }
        skew <- Skewness(new.x)
    }
    return(new.x)
}

DetectOutliers <- function(x, verbose=FALSE) {
    # Compute outlier scores ('yes', 'maybe', 'no', 'unknown')
    # on the entire data set.
    #
    # Args:
    #   df_: Vector of numeric values.
    #   verbose: If TRUE, prints t and T; if not, not. Default is FALSE.
    #
    # Returns:
    #   The data frame with outliers score computed, and the skewness signature.
    if (missing(x))
        stop("Argument x is missing.")
    if (is.null(x))
        stop("Argument x should not be null.")
    
    # Prepare data frame
    init <- rep.int(0, length(x))
    df_ <- data.frame(x = x, yes = init, maybe = init, no = init, unknown = init)
    
    # Compute skewness thresholds
    skew.signature <- SkewnessSignature(x)
    res.high <- Findt(x, skew.signature)
    res.low <- FindT(x, skew.signature)
    t_ <- res.low$t
    T_ <- res.high$t
    
    if (verbose)
        cat("T = ", T_, "\nt = ", t_, "\n", sep = "")
    
    # Apply outlier status
    if (res.high$rejected && res.low$rejected) {
        df_$unknown <- 1
    }
    else {
        # Sort with index.return returns a sorted x in sorted$x and the 
        # indices of the sorted values from the original x in sorted$ix
        sorted <- sort(x, decreasing=FALSE, index.return=TRUE)
        new.x <- x
        i <- 1
        j <- length(x)
        skew <- Skewness(x)
        below.t <- FALSE
        while (!is.na(skew) && !below.t) {
            if (verbose)
                cat("|skew| = ", abs(skew), "\nmax = ", max(new.x), "\nmin = ", min(new.x), "\n", sep = "")
            
            if (abs(skew) <= t_) {
                below.t <- TRUE
            }
            colname <- 'maybe'
            if (abs(skew) > T_) {
                colname <- 'yes'
            }
            if (skew > 0) {
                # Remove max from x
                df_[df_$x==sorted$x[j], colname] <- 1
                j <- j - 1
            }
            else {
                # Remove min from x
                df_[df_$x==sorted$x[i], colname] <- 1
                i <- i + 1
            }
            new.x <- x[sorted$ix[i:j]]
            skew <- Skewness(new.x)
        }
        df_[df_$yes != 1 & df_$maybe != 1, 'no'] <- 1
    }
    return(list(df=df_, skew.signature=skew.signature))
}

DetectOutliersDF <- function(df_) {
    # Compute outlier scores ('yes', 'maybe', 'no', 'unknown')
    # on the entire data set.
    #
    # Args:
    #   df_: Data frame of values as rows and 'x' as column.
    #
    # Returns:
    #   The data frame with outliers score computed.
    if (missing(df_))
        stop("Argument df_ is missing.")
    if (is.null(df_))
        stop("Argument df_ should not be null.")
    
    # Compute skewness thresholds
    skew.signature <- SkewnessSignature(df_$x)
    res.high <- Findt(df_$x, skew.signature)
    res.low <- FindT(df_$x, skew.signature)
    t_ <- res.low$t
    T_ <- res.high$t
    
    # Apply outlier status
    if (res.high$rejected && res.low$rejected) {
        df_$unknown <- df_$unknown + 1
    }
    else {
        x2 <- RemoveOutliers(df_$x, T_)
        x1 <- RemoveOutliers(x2, t_)
        
        outliers <- setdiff(df_$x, x2)
        maybe_outliers <- setdiff(x2, x1)
        not_outliers <- x1
        
        df_[df_$x %in% outliers, 'yes']         <- df_[df_$x %in% outliers, 'yes'] + 1
        df_[df_$x %in% maybe_outliers, 'maybe'] <- df_[df_$x %in% maybe_outliers, 'maybe'] + 1
        df_[df_$x %in% not_outliers, 'no']      <- df_[df_$x %in% not_outliers, 'no'] + 1
    }
    return(df_)
}

DetectOutliersDynamically <- function(df_, w=100) {
    # Compute outlier scores ('yes', 'maybe', 'no', 'unknown')
    # on the data set using a sliding time window.
    #
    # Args:
    #   df_: Data frame of values as rows and 'x', 't' as columns.
    #
    # Returns:
    #   The data frame with outliers score computed.
    init <- rep.int(0, nrow(df_))
    df_$yes <- init
    df_$maybe <- init
    df_$no <- init
    df_$unknown <- init
    for (i in (w + 1):(nrow(df_) + 1)) {
        subset.df <- df_[(i - w):(i - 1), ]
        subset.df <- DetectOutliersDF(subset.df)
        df_[(i - w):(i - 1), ] <- subset.df
    }
    return(df_)
}

SetFinalOutlierStatus <- function(df_) {
    # Assign the final outlier status of values.
    #
    # Args:
    #   df_: Data frame of values as rows and outliers scores 
    #        ('yes', 'maybe', 'no', 'unknown') as columns.
    #
    # Returns:
    #   The data frame with a filled 'status' column.
    if (missing(df_))
        stop("Argument df_ is missing.")
    if (is.null(df_))
        stop("Argument df_ should not be null.")
    
    df_$status <- 'unknown'
    for (i in 1:nrow(df_)) {
        v <- max(df_[i,c('yes', 'maybe', 'no', 'unknown')])
        if(v != 0) {
            if (df_[i, 'yes'] == v) {
               df_[i, 'status'] <- 'yes'
            }
            else if (df_[i, 'maybe'] == v) {
                df_[i, 'status'] <- 'maybe'
            }
            else if (df_[i, 'no'] == v) {
                df_[i, 'status'] <- 'no'
            }
        }
    }
    return(df_)
}


# ----------------------------------------------------------------------------
# Extra Functions
# ----------------------------------------------------------------------------

SkewnessSignature_ <- function(x) {
    # Create half of the skewness signature for a distribution of values
    # (non-optimized version).
    #
    # Args:
    #   x: Vector of numbers whose skewness signature is to be calculated.
    #
    # Returns:
    #   The half of the skewness signature of x.
    if (missing(x))
        stop("Argument x is missing.")
    if (is.null(x))
        stop("Argument x should not be null.")
    
    half.n <- floor(length(x) / 2)
    signature <- rep(NA, half.n)
    h <- half.n
    
    skew <- Skewness(x)
    while (h > 0 && !is.na(skew)) {
        signature[h] <- skew
        h <- h - 1
        if (skew > 0) {
            # Remove max from x
            x <- x[ -which.max(x)]
        }
        else {
            # Remove min from x
            x <- x[ -which.min(x)]
        }
        skew <- Skewness(x)
    }
    return(signature)
}

FullSkewnessSignature_ <- function(x) {
    # Compute the skewness signature for a distribution of values.
    #
    # Args:
    #   x: Vector of numbers whose skewness signature is to be calculated.
    #
    # Returns:
    #   The skewness signature of x.
    if (missing(x))
        stop("Argument x is missing.")
    if (is.null(x))
        stop("Argument x should not be null.")
    
    i <- 1
    j <- length(x)
    signature <- rep(NA, j)
    h <- j
    
    skew <- Skewness(x)
    # Sort with index.return returns a sorted x in sorted$x and the 
    # indices of the sorted values from the original x in sorted$ix
    sorted <- sort(x, decreasing=FALSE, index.return=TRUE)
    new.x <- x[sorted$ix[i:j]]
    while (length(new.x) >= 3 && !is.na(skew)) {
        signature[h] <- skew
        h <- h - 1
        if (skew > 0) {
            # Remove max from x
            j <- j - 1
        }
        else {
            # Remove min from x
            i <- i + 1
        }
        new.x <- x[sorted$ix[i:j]]
        skew <- Skewness(new.x)
    }
    return(na.omit(signature))
}


# ----------------------------------------------------------------------------
# Examples
# ----------------------------------------------------------------------------

RunStaticExample <- function() {
    setwd('path/to/outskewer') # TO EDIT
    
    # Load libraries.
    source("outskewer.r")
    source("utils.r")
    
    # Data, example used in the Figure 3 of the paper.
    x <- c(0.73, 0.18, 0.46, 1.12, -1.21, -1.54, 0.47, -0.1, 0.6, -0.75, -0.87, 1.72, 0.24, -1.36, -0.97, 
       -0.5, 1.3, -1.23, 0.44, -0.46, -2.36, 1.8, 0.63, 0, 0.24, -0.47, -0.9, 0.2, -0.84, -0.3, 0.74, 
       0.8, -0.7, 0.58, 0.45, -0.06, 0.66, -0.13, 1, 0.66, -7.74, -2, -2.5, 1.3, -1.8, -3.3, -4.1, 
       -7.8, -7.7, -5)

    # Run outskewer.
    tmp <- DetectOutliers(x)
    results.df <- SetFinalOutlierStatus(tmp$df)
    
    # Display the number of outliers detected.
    summary(factor(results.df$status))
    
    # Plot the cumulative frequency with points colored by outlier status,
    # like in the Figure 3 of the paper.
    # It requires ggplot2
    g <- PlotCumulative(results.df)
    g <- DecoratePlot(g)
    g
    
    # Plot the skewness signature of x, like in Figure 3 of the paper.
    s.g <- PlotSignature(x)
    s.g
    
    # Save results in a file.
#   ExportData(results.df, filename='my_results')
    
    # Save the plot in a file.
#   ExportPlot(g, filename='my_results', width=2.5, height=2)
}

RunDynamicExample <- function() {
    setwd('path/to/outskewer') # TO EDIT
    
    # Load libraries.
    source("outskewer.r")
    source("utils.r")
    
    # Load data, give proper names to columns.
    data.df <- read.table('data/nodes.dat')
    colnames(data.df) <- c("t","x")
    
    # Run outskewer with a time window of size 100 values.
    results.df <- DetectOutliersDynamically(data.df, w=100)
    results.df <- SetFinalOutlierStatus(results.df)
    
    # Display the number of outliers detected.
    summary(factor(results.df$status))
    
    # Plot results with points colored by outlier status.
    # It requires ggplot2
    g <- PlotTimeSeries(results.df)
    g <- DecoratePlot(g)
    g
    
    # Plot only values above 10,000 like the Figure 11 in the paper.
    filtered.df <- subset(results.df, x > 10000)
    g2 <- PlotTimeSeries(filtered.df)
    g2 <- DecoratePlot(g2)
    g2
    
    # Save results in a file.
#   ExportData(results.df, filename='my_dynamic_results')
    
    # Save the plot in a file.
#   ExportPlot(g, filename='my_dynamic_results', width=10, height=5)
}
