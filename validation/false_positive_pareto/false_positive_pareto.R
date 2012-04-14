#!/usr/bin/Rscript
#
# Symmetric Pareto rejection rate
#
# usage: 
#   ./false_positive_pareto.R '../lib/outskewer/'
#   source("theta_proba.R", chdir = T)

# CONTEXT
args <- commandArgs(TRUE)
lib_outliers = args[1] # ../lib/outskewer/

source(paste(lib_outliers, "utils.r", sep = ""))
source(paste(lib_outliers, "outskewer.R", sep = ""))

library(actuar)
generate.pareto <- function(n) { return(actuar::rpareto(n, shape=6, scale=2)) }

Simulation <- function(generator, times=1000) {
    rate.df <- data.frame()
    #ni <- c(seq(3, 100, 1), seq(200, 1000, 100))
    #ni <- c(400, 401, 450, 451, 500, 501, 550, 551, 600, 601, 650, 651, 700, 701)
    ni <- c(seq(3, 201, 1), seq(210, 500, 10), seq(205, 505, 10), seq(550, 1000, 50))
    for (n in ni) {
        all_nb_yes <- c()
        all_nb_maybe <- c()
        for (i in 1:times) {
            x <- generator(n)
            tmp <- DetectOutliers(x)
            nb_yes <- sum(tmp$df['yes'])
            nb_maybe <- sum(tmp$df['maybe'])
            all_nb_yes <- c(all_nb_yes, nb_yes)
            all_nb_maybe <- c(all_nb_maybe, nb_maybe)
        }
        ratio_nb_yes <- sum(all_nb_yes) / n
        ratio_nb_maybe <- sum(all_nb_maybe) / n
        df <- data.frame(n=n, yes=ratio_nb_yes, maybe=ratio_nb_maybe)
        rate.df <- rbind(rate.df, df)
    }
    # normalize in [0;1]
    rate.df$yes <- rate.df$yes / times
    rate.df$maybe <- rate.df$maybe / times
    return(rate.df)
}

PlotRate <- function(rate.df) {
    if (!require(ggplot2))
        stop("Can't load ggplot2")
    mysep <- function(x, ...) { format(x, big.mark = ' ', trim = TRUE, scientific = FALSE, ...) }
   
    #g <- ggplot(rate.df) + geom_line(aes(x=n, y=yes, linetype=1)) + geom_line(aes(x=n, y=maybe, linetype=3))
    g <- ggplot(rate.df) + geom_point(aes(x=n, y=yes, shape=1))
    g <- DecoratePlot(g) + opts(axis.text.x=theme_text(size=16), 
        axis.text.y=theme_text(size=16), 
        axis.title.x=theme_text(size=16),
        axis.title.y=theme_text(size=16, angle=90))
    g <- g + scale_x_continuous(breaks=c(0, 100, 500, max(rate.df$n)))
    g2 <- g + scale_x_log10(breaks=c(0, 10, 100, 1000), labels=c(0, 10, 100, 1000))
    g2 <- g2 + scale_y_log10("outlier rate", formatter = mysep)
    return(list(continuous=g,log=g2))
}

paretoRate.df <- Simulation(generate.pareto, times=1000)
ExportData(paretoRate.df, 'false_positive_pareto')
save.image('false_positive_pareto')

qPareto <- PlotRate(paretoRate.df)
qPareto$continuous <- qPareto$continuous + scale_y_continuous("outlier rate", breaks=c(0, 0.01, 0.02, 0.03, 0.04, 0.05))
qPareto$log <- qPareto$log + scale_y_log10("outlier rate", breaks=c(0.05, 0, 0.01, 0.001, 0.0001, 0.00001), labels=c(0.05, 0, 0.01, 0.001, '0.0001', '0.00001'))
qPareto$continuous
ExportPlot(qPareto$continuous, 'false_positive_pareto', 4.5, 1.5)
ExportPlot(qPareto$log, 'false_positive_pareto_log', 4, 3)

