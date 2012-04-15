Free and open-source implementation in **R** of [*Outskewer*](https://github.com/sheymann/outskewer), a method to detect outliers in data sets of numbers and in time series. It is distributed under the [MIT License](https://github.com/sheymann/outskewer.r/blob/master/LICENSE.txt).

###Usage
####Load functions

```R
setwd('path/to/outskewer')  # to edit
source("outskewer.r")
source("utils.r")
```

####Execute on a vector of numbers

```R
tmp <- DetectOutliers(x)
results.df <- SetFinalOutlierStatus(tmp$df)
```

####Execute on a time series

```R
# Load data, give proper names to the data frame columns.
# File structure: timestamp, value
# 1 10807
# 2 10873
# 3 10902
data.df <- read.table('ts.dat')
colnames(data.df) <- c("t","x")

# Run outskewer with a time window with size of w values.
results.df <- DetectOutliersDynamically(data.df, w=100)
results.df <- SetFinalOutlierStatus(results.df)
```

###Display results
####Number of outliers detected

```R
summary(factor(results.df$status))
```

####Cumulative frequency with points colored by outlier status

```R
g <- PlotCumulative(results.df)
g <- DecoratePlot(g)  # optional
g
```

####Skewness signature of a distribution of values

```R
PlotSignature(x)
```

####Time series with points colored by outlier status

```R
g <- PlotTimeSeries(results.df)
g <- DecoratePlot(g)  # optional
g
```

###Save in file
####Results

```R
# Save in a tabular format
ExportData(data=results.df, filename='my_results')
```


####Plot

```R
# Save in PDF, EPS and PNG 
ExportPlot(gplot=g, filename='my_results', width=10, height=5)
```
