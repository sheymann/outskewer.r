#!/usr/bin/Rscript
#
# Half-normal rejection rate
#
# usage: 
#   ./halfnorm_rejection_rate.R '../lib/outskewer/'
#   source("theta_proba.R", chdir = T)

# CONTEXT
args <- commandArgs(TRUE)
lib_outliers = args[1] # ../lib/outskewer/

source(paste(lib_outliers, "utils.r", sep = ""))
source(paste(lib_outliers, "outskewer.R", sep = ""))

generate.halfnorm <- function(n) {
    x <- rnorm(n, mean=0, sd=1)
    return(abs(x))
}

Simulation <- function(generator, times=1000) {
    rate.df <- data.frame()
    ni <- c(seq(3, 201, 1), seq(210, 500, 10), seq(205, 505, 10), seq(550, 1000, 50))
    #ni <- c(seq(3, 201, 1), 250, 251, 300, 301, 350, 351, 400, 401, 450, 451, 500, 501, 600, 601, 700, 701, 800, 801, 900, 901, 999, 1000)
    #ni <- c(seq(130, 200, 1))
    for (n in ni) {
        res <- c()
        for (i in 1:times) {
            x <- generator(n)
            heap <- SkewnessSignature(x)
            res_high <- HigherSkewnessThreshold(x, heap)
            res_low <- LowerSkewnessThreshold(x, heap)
            reject <- FALSE
            if (res_high$rejected && res_low$rejected) {
                reject <- TRUE
            }
            res <- c(res, reject)
        }
        r <- sum(res)
        df_ <- data.frame(n=n, reject=r)
        rate.df <- rbind(rate.df, df_)
    }
    rate.df$reject <- rate.df$reject / times  # normalize in [0;1]
    return(rate.df)
}

PlotRate <- function(rate.df) {
    if (!require(ggplot2))
        stop("Can't load ggplot2")
    
    q <- ggplot(data=rate.df)
    q <- q + geom_point(aes(x=n, y=reject, ymin=0, ymax=1, xmin=0, xmax=max(n), shape=1, xlabel="n", ylabel = "rejection rate"))
    
    q <- DecoratePlot(q) + opts(axis.text.x=theme_text(size=16), 
        axis.text.y=theme_text(size=16), 
        axis.title.x=theme_text(size=16),
        axis.title.y=theme_text(size=16, angle=90))
    q <- q + scale_x_continuous(breaks=c(0, max(rate.df$n)))
    q <- q + scale_y_continuous(breaks=c(0, 0.5, 1))
    return(q)
}

halfnormRate.df <- Simulation(generate.halfnorm)
ExportData(halfnormRate.df, 'halfnorm_rejection_rate')
save.image('halfnorm_rejection_rate')

qHalfnorm <- PlotRate(halfnormRate.df) 
#qHalfnorm <- qHalfnorm + scale_x_continuous(breaks=c(10,100, 500), labels=c(10,100,500))
#qHalfnorm <- qHalfnorm + scale_y_continuous("rejection rate")
qHalfnorm <- qHalfnorm + scale_x_log(breaks=c(10,100, 1000), labels=c(10,100,1000))
qHalfnorm <- qHalfnorm + scale_y_log("rejection rate", breaks=c(0.001,0.01,0.1,0.2,0.5,1), labels=c(0.001,0.01,0.1,0.2,0.5,1))
ExportPlot(qHalfnorm, 'halfnorm_rejection_rate', 4,3)

