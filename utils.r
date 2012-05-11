#!/usr/bin/Rscript
#
# Utility functions to make better plots using the ggplot2 package (v0.9.0).
#


# ----------------------------------------------------------------------------
# Rudolf Cardinal, March 2011
# Simple extensions to ggplot2 (v0.8.7); see http://www.psychol.cam.ac.uk/statistics/R/
# ----------------------------------------------------------------------------

theme_L_border <- function(colour = "black", size = 1, linetype = 1) {
  # use with e.g.: ggplot(...) + opts( panel.border=theme_L_border() ) + ...
  structure(
    function(x = 0, y = 0, width = 1, height = 1, ...) {
      polylineGrob(
        x=c(x+width, x, x), y=c(y,y,y+height), ..., default.units = "npc",
        gp=gpar(lwd=size, col=colour, lty=linetype),
      )
    },
    class = "theme",
    type = "box",
    call = match.call()
  )
}

theme_bottom_border <- function(colour = "black", size = 1, linetype = 1) {
  # use with e.g.: ggplot(...) + opts( panel.border=theme_bottom_border() ) + ...
  structure(
    function(x = 0, y = 0, width = 1, height = 1, ...) {
      polylineGrob(
        x=c(x, x+width), y=c(y,y), ..., default.units = "npc",
        gp=gpar(lwd=size, col=colour, lty=linetype),
      )
    },
    class = "theme",
    type = "box",
    call = match.call()
  )
}

theme_left_border <- function(colour = "black", size = 1, linetype = 1) {
  # use with e.g.: ggplot(...) + opts( panel.border=theme_left_border() ) + ...
  structure(
    function(x = 0, y = 0, width = 1, height = 1, ...) {
      polylineGrob(
        x=c(x, x), y=c(y,y+height), ..., default.units = "npc",
        gp=gpar(lwd=size, col=colour, lty=linetype),
      )
    },
    class = "theme",
    type = "box",
    call = match.call()
  )
}

theme_border_numerictype <- function(type, colour = "black", size = 1, linetype = 1) {
  # use with e.g.: ggplot(...) + opts( panel.border=theme_border(type=9) ) + ...
  structure(
    function(x = 0, y = 0, width = 1, height = 1, ...) {
      # numerical types from: library(gridExtra); example(borderGrob)
      # 1=none, 2=bottom, 3=right, 4=top, 5=left, 6=B+R, 7=T+R, 8=T+L, 9=B+L, 10=T+B, 11=L+R, 12=T+B+R, 13=T+L+R, 14=T+B+L, 15=B+L+R, 16=T+B+L+R
      xlist <- c()
      ylist <- c()
      idlist <- c()
      if (type==2 || type==6 || type==9 || type==10 || type==12 || type==14 || type==15 || type==16) { # bottom
        xlist <- append(xlist, c(x, x+width))
        ylist <- append(ylist, c(y, y))
        idlist <- append(idlist, c(1,1))
      }
      if (type==4 || type==7 || type==8 || type==10 || type==12 || type==13 || type==14 || type==16) { # top
        xlist <- append(xlist, c(x, x+width))
        ylist <- append(ylist, c(y+height, y+height))
        idlist <- append(idlist, c(2,2))
      }
      if (type==5 || type==8 || type==9 || type==11 || type==13 || type==14 || type==15 || type==16) { # left
        xlist <- append(xlist, c(x, x))
        ylist <- append(ylist, c(y, y+height))
        idlist <- append(idlist, c(3,3))
      }
      if (type==3 || type==6 || type==7 || type==11 || type==12 || type==13 || type==15 || type==16) { # right
        xlist <- append(xlist, c(x+width, x+width))
        ylist <- append(ylist, c(y, y+height))
        idlist <- append(idlist, c(4,4))
      }
      if (type==1) { # blank; can't pass absence of coordinates, so pass a single point and use an invisible line
        xlist <- c(x,x)
        ylist <- c(y,y)
        idlist <- c(5,5)
        linetype <- "blank"
      }
      polylineGrob(
        x=xlist, y=ylist, id=idlist, ..., default.units = "npc",
        gp=gpar(lwd=size, col=colour, lty=linetype),
      )
    },
    class = "theme",
    type = "box",
    call = match.call()
  )
}

theme_border <- function(type = c("left", "right", "bottom", "top", "none"), colour = "black", size = 1, linetype = 1) {
  # use with e.g.: ggplot(...) + opts( panel.border=theme_border(type=c("bottom","left")) ) + ...
  type <- match.arg(type, several.ok=TRUE)
  structure(
    function(x = 0, y = 0, width = 1, height = 1, ...) {
      xlist <- c()
      ylist <- c()
      idlist <- c()
      if ("bottom" %in% type) { # bottom
        xlist <- append(xlist, c(x, x+width))
        ylist <- append(ylist, c(y, y))
        idlist <- append(idlist, c(1,1))
      }
      if ("top" %in% type) { # top
        xlist <- append(xlist, c(x, x+width))
        ylist <- append(ylist, c(y+height, y+height))
        idlist <- append(idlist, c(2,2))
      }
      if ("left" %in% type) { # left
        xlist <- append(xlist, c(x, x))
        ylist <- append(ylist, c(y, y+height))
        idlist <- append(idlist, c(3,3))
      }
      if ("right" %in% type) { # right
        xlist <- append(xlist, c(x+width, x+width))
        ylist <- append(ylist, c(y, y+height))
        idlist <- append(idlist, c(4,4))
      }
      if (length(type)==0 || "none" %in% type) { # blank; can't pass absence of coordinates, so pass a single point and use an invisible line
        xlist <- c(x,x)
        ylist <- c(y,y)
        idlist <- c(5,5)
        linetype <- "blank"
      }
      polylineGrob(
        x=xlist, y=ylist, id=idlist, ..., default.units = "npc",
        gp=gpar(lwd=size, col=colour, lty=linetype),
      )
    },
    class = "theme",
    type = "box",
    call = match.call()
  )
}

# Examples:
# library(ggplot2)
# df = data.frame( x=c(1,2,3), y=c(4,5,6) )
# ggplot(data=df, aes(x=x, y=y)) + geom_point() + theme_bw() + opts( panel.border = theme_border_numerictype(9) ) 
# ggplot(data=df, aes(x=x, y=y)) + geom_point() + theme_bw() + opts( panel.border = theme_border(c("bottom","left")) ) 



# ----------------------------------------------------------------------------
# Plot and Export Functions
# Sébastien Heymann, April 2012
# ----------------------------------------------------------------------------

DecoratePlot <- function(gplot) {
    # Make the plot ready for publication.
    if (!require(ggplot2))
        stop("Can't load ggplot2.")
    theme_set(theme_bw(12))
    gplot <- gplot + opts(panel.grid.major = theme_blank())
    gplot <- gplot + opts(panel.grid.minor = theme_blank())
    gplot <- gplot + opts(panel.border = theme_L_border())
    gplot <- gplot + opts(plot.margin = unit(c(0.2,0.4,0,0), "lines"))  # delete axis titles: c(0,0,-1,-1)
    
    return(gplot)
}

PlotGraphic  <- function(df_) {
    # Plot the data with points colored by outlier status.
    #
    # Args:
    #   df_: Data frame made by the outskewer method.
    #
    # Returns:
    #   The plot of the values.
    if (!require(ggplot2))
        stop("Can't load ggplot2.")
    
    # colors from http://wearecolorblind.com/example/wordfeud/
    colour <- c(yes     = "#93421B", #D55E00
                maybe   = "#B57120", #E69F00
                no      = "#208592", #009E73
                unknown = "#739D6A") #666666
    shape  <- c(yes     = 4, 
                maybe   = 2, 
                no      = 1, 
                unknown = 5)
    
    breaks_tmp <- c(unique(df_$status))
    labels_ <- c()
    breaks_ <- c()
    if( "no" %in% breaks_tmp) {
        labels_  <- c(labels_, "not outlier")
        breaks_ <- c(breaks_, "no")
    }
    if( "maybe" %in% breaks_tmp) {
        labels_  <- c(labels_, "potential outlier")
        breaks_ <- c(breaks_, "maybe")
    }
    if( "yes" %in% breaks_tmp) {
        labels_  <- c(labels_, "outlier")
        breaks_ <- c(breaks_, "yes")
    }
    if( "unknown" %in% breaks_tmp) {
        labels_  <- c(labels_, "unknown")
        breaks_ <- c(breaks_, "unknown")
    }
    
    g <- qplot(data = df_, x = t, y = x, colour = status, shape = status)
    g <- g + scale_colour_manual(values=colour, breaks=breaks_, labels=labels_)
    g <- g + scale_shape_manual(values=shape, breaks=breaks_, labels=labels_)
    return(g)
}

PlotTimeSeries <- function(df_) { 
    # Alias of PlotGraphic
    return(PlotGraphic(df_)) 
}

PlotCumulative <- function(df_) {
    # Plot the cumulative frequency distribution of the data set,
    # with points colored by outlier status.
    #
    # Args:
    #   df_: Data frame made by the outskewer method.
    #
    # Returns:
    #   The plot of the cumulative frequency distribution of the values.
    if (!require(ggplot2))
        stop("Can't load ggplot2.")
    # colors from http://wearecolorblind.com/example/wordfeud/
    colour <- c(yes     = "#93421B", #D55E00
                maybe   = "#B57120", #E69F00
                no      = "#208592", #009E73
                unknown = "#739D6A") #666666
    shape  <- c(yes     = 4, 
                maybe   = 2, 
                no      = 1, 
                unknown = 5)

    breaks_ <- c(unique(df_$status))
    labels_ <- c()
    if( "no" %in% breaks_)
        labels_ <- c(labels_, "not outlier")
    if( "maybe" %in% breaks_)
        labels_ <- c(labels_, "potential outlier")
    if( "yes" %in% breaks_)
        labels_ <- c(labels_, "outlier")
    if( "unknown" %in% breaks_)
        labels_ <- c(labels_, "unknown")

    g <- ggplot(data=df_[with(df_, order(x)), ]) + geom_point(aes(x=x, y=(1:length(x))/length(x), xmin=-8, xmax=2, ymin=0, colour=status, shape=status))
    g <- g + scale_colour_manual(values=colour, breaks=breaks_, labels=labels_)
    g <- g + scale_shape_manual(values=shape, breaks=breaks_, labels=labels_)
    g <- g + scale_y_continuous("cumulative frequency")
    g <- g + opts(legend.position = c(0.5,0.86),
              legend.title = theme_blank(),
              legend.text = theme_text(size=11),
              legend.key.size = unit(1, "lines"),
              legend.key = theme_blank())
    g <- DecoratePlot(g)
    return(g)
}

PlotSignature <- function(x) {
    # Plot the skewness signature of x.
    #
    # Args:
    #   x: Vector of numbers whose skewness signature is to be ploted.
    #
    # Returns:
    #   The skewness signature of x.
    if (!require(ggplot2))
        stop("Can't load ggplot2.")
    signature <- FullSkewnessSignature_(x)
    df_ <- data.frame(skewness=abs(signature), p=seq(length(signature),1,-1))
    g <- ggplot(df_) + geom_line(aes(x=p, y=skewness)) + geom_point(aes(x=p, y=skewness, shape=1, ymin=0))
    g <- g + scale_x_continuous(breaks=c(1, 0.14*length(x), floor(length(x)/2), length(x)), labels=c(0, 0.14, 0.5, 1))
    g <- g + scale_y_continuous("|skewness|")
    g <- DecoratePlot(g)
    return(g)
}

ExportPlot <- function(gplot, filename, width=2, height=1.5) {
    # Export plot in PDF and EPS.
    # Notice that A4: width=11.69, height=8.27
    ggsave(paste(filename, '.pdf', sep=""), gplot, width = width, height = height)
    postscript(file = paste(filename, '.eps', sep=""), width = width, height = height)
    print(gplot)
    dev.off()
    png(file = paste(filename, '_.png', sep=""), width = width * 100, height = height * 100)
    print(gplot)
    dev.off()
}

ExportData <- function(data, filename) {
    # Export data in a tabular file.
    sink(paste(filename, '.txt', sep=""), append=FALSE, split=FALSE)
    print(data)
    sink()
}
