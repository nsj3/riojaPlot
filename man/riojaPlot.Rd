\name{riojaPlot}
\alias{riojaPlot}
\alias{addRPZone}
\alias{addRPClust}
\alias{addRPClustZone}
\alias{addRPZoneNames}
\alias{makeStyles}
\alias{listStyles}

\title{ Plot a stratigraphic diagram }
\description{
Plots a diagram of multiple biological, physical or chemical parameters agains depth or time, as used in geology & palaeoecology.
}
\usage{
riojaPlot(x, y, selVars=NULL, groups=NULL, style=NULL, clust=NULL, 
          lithology=NULL, riojaPlot=NULL, verbose=TRUE, \dots)

addRPClust(riojaPlot, clust, xLeft=NULL, xRight=0.99, verbose=TRUE, \dots)

addRPClustZone(riojaPlot, clust, nZone="auto", xLeft=NULL, xRight=NULL, 
               verbose=TRUE, \dots)

addRPZone(riojaPlot, upper, lower=NULL, xLeft=NULL, xRight=NULL, col="red", 
          alpha=0.1, border=NA, verbose=TRUE, \dots)

addRPZoneNames(riojaPlot, zones, showColumn=TRUE, xLeft=NULL, xRight=0.99, \dots)

listStyles()

makeStyles(...)

}
\arguments{
  \item{x}{ a data frame or tibble containing variables to plot. }
  \item{y}{ a data frame or tibble containing depth and/or age variables for the y-axis. }
  \item{selVars}{ a character vector of variable names to include in the plot. }
  \item{groups}{ a data frame or tibble with 2 columns giving the variable name (col 1) and factor containing group membership (col 2), used to colour variables by group and / or to generate a summary cumulative plot of group totals. }
  \item{clust}{ a constrained cluster object returned by \code{chclust}. }
  \item{nZone}{ number of zones to include or "auto" to add number of zones suggested by a broken stick model (see details). }
  \item{style}{ a riojaPlot style object to control visual aspects of the plot (see details). }
  \item{riojaPlot}{ object returned by riojaPlot. }
  \item{upper, lower}{ upper and lower values of a zone to add. }
  \item{xLeft, xRight}{ left and right position of figure on page (as a fraction of page width). }
  \item{verbose}{ show warnings (default TRUE). }
  \item{col, alpha, border}{ colour, alpha value (transparency) and border colour of zones. }
  \item{zones}{ a data frame with 2 columns, the first giving the y-value to draw the zone label and the second the zones names as character. }
  \item{showColumn}{ draw a column around the zone names? }
  \item{lithology}{ data frame or tibble that is passed to fun.lithology for plotting the lithological column. }
  \item{...}{ additional arguments to control the style of the figure (see details below) or colour of zones. }
}

\details{
\code{riojaPlot} plots a set of variables in a stratigraphic diagram. Diagrams can be plotted as line graphs, silhouettes or bar charts.  {riojaPlot} extends strat.plot in several ways: variables can be grouped and displayed with different colours, a cumulative summary plot can be added automatically based on the grouping, a cluster dendrogram and resulting zones can be added automatically, a secondary y-axis can be added to plot a depth and age axis, and the figures margins are determined automatically depending on the size and length of labels.  

See \code{vignette("riojaPlot")} for a user guide or \code{vignette("riojaPlotPDF")} for a pdf version.

\itemize{
  \item{\code{yvar.name}}{ name of the variable to use for primary y-axis scale. If not specified the first column in the y table will be used. If the primary axis is character data text labels will be drawn at each sample position. }
  \item{\code{sec.yvar.name}}{ name of the variable to use for an optional secondary y-axis scale. }
  \item{\code{plot.sec.axis}}{ show secondary y-axis.  Default FALSE. }
  \item{\code{yLabel, secyLabel}} { labels for the primary and secondary y-axes.  Uses column names if not specified. }
  \item{\code{scale.percent}}{ scale the diagram for percentage data (default TRUE). }
  \item{\code{scale.minmax}}{ for non-percentage data, only label min and max values of x-axes to avoid crowding (default FALSE). }
  \item{\code{y.rev}}{ reverse the y-axis (default TRUE). }
  \item{\code{ymin, ymax, sec.ymin, sec.ymax}}{ minimum and maximum of primary and secondary y-axes. }
  \item{\code{yinterval, sec.yinterval}}{ intervals for primary and secondary y-axes. }
  \item{\code{ytks1, ytks2}}{ numeric vector of y-values to print on primary and secondary y-axes. }
  \item{\code{wa.order}}{ rearrange the curves to show trend from bottom left to top right (default FALSE). }
  \item{\code{plot.bar, plot.line, plot.poly, plot.symb}}{ show bars, lines, polygons (silhouettes) and / or symbols (defaults to polygons with thin horizintal lines).}
  \item{\code{lwd.bar, lwd.line, lwd.pol.line, lwd,.cumul.line}}{ thickness of bars and lines (default thickness for base graphics is 1). }
  \item{\code{col.bar, col.poly.line, col.poly, col.symb, col.zones, col.cumul.line, col.exag.line}}{ colour of bars, lines, polygon outlines and fills, symbols, zones and lines on cumulative plot. }
  \item{\code{bar.back}}{, plot horizontal bars on top or below shaded silhouettes (default TRUE). }
  \item{\code{cex.axis, cex.yaxis, cex.ylabel, cex.xlabel, cex.cumul}}, { font sizes (size relative to default of 1). }
  \item{\code{tcl}}{ tick length for axes, in logical units (default -0.3). }
  \item{\code{srt.xlabel, srt.ylabel}}{ angle (in degrees) of names (default=90). }
  \item{\code{do.clust}}{ do a CONISS constrained clustering. }
  \item{\code{clust.data.trans}}{ transform data before cluster analysis ("none", "sqrt", or "scale", default "none"). }
  \item{\code{clust.use.selected}}{ use selected variables (ie. those listed in selVars) or all variables when calculating cluster dendrogram (default FALSE). }
  \item{\code{plot.clust}}{ add a CONISS cluster dendrogram. }
  \item{\code{clust.width}}{ width of the dendrogram (as fraction of page width). }
  \item{\code{exag}}{ show exaggereted curves (default FALSE. }
  \item{\code{col.exag}}{ colour for exaggerations, either a colour or "auto". }
  \item{\code{exag.mult}}{ multiplication factor for exaggerations (default 2). }
  \item{\code{exag.alpha}}{ alpha value for exaggeration curves. }
  \item{\code{plot.groups}}{ show curves with different colours (default FALSE). }
  \item{\code{plot.cumul}}{ add a cumulative summary based on groups on right of diagram (default FALSE). }
  \item{\code{col.group}}{ character vector of 10 colours for groups (will be recycled to 10). }
  \item{\code{names.break.long}}{ break long names (default TRUE). }
  \item{\code{names.break.n}}{ number of characters to break after (default 20). }
  \item{\code{names.italicize}}{ italicize names. }
  \item{\code{xRight}}{ right hand position of plot (as fraction of page width). }
  \item{\code{xLeft, xRight}}{ left and right hand position of plot (as fraction of page width). Will be determined automatically depending of label sizes, override to fine-tune. }
  \item{\code{yTop, yBottom}}{ top and bottom positions of plot (as fraction of page height). Will be determined automatically depending of label sizes, override to fine-tune. yBottom default is 0.05, increase if axis labels are truncated. }
  \item{\code{las.axis}}{ orientation of x-axis labels (1=horz, 2=vert, default 1). }
  \item{\code{fun.xback, fun.xfront}}{ custom functions of add elements to a plot above (fun.xfront) or behind (fun.xback) data. See below. }
  \item{\code{x.pc.omit0}}{ omit zero value of axis values when scaling for percent data (default TRUE). }
  \item{\code{x.pc.inc}}{ for percentage diagrams single value or numeric vector of intervals for x-axes. }
  \item{\code{min.width.pc}}{ for percentage diagrams the minimim data width of the x-axes. }
  \item{\code{graph.widths}}{ vector of relative widths for each graph (default 1). }
  \item{\code{xSpace}}{ space between individual graphs (fraction of page width, default=0.01). }
  \item{\code{xGap}}{ space between datasets plotted by repeated calls to riojaPlot (fraction of page width, default=0.01). }
  \item{\code{ylabSpace}}{ x-position of y-axis label when secondary y-axis is not used, in lines (use to tweak position of y-axis label, try values of 2-3). }
  \item{\code{sep.bar}}{ plot different colours for bars for different levels?  Default FALSE. }
  \item{\code{col.sep.bar}}{ character vector with colours for bars for each sample. }
  \item{\code{plot.top.axis, plot.bottom.axis}}{ show x-axes on top and/or bottom of plots (default FALSE, TRUE respectively. }
  \item{\code{omitMissing}}{ Strip NAs from data before plotting (default TRUE).  Setting to FALSE will leave gaps in lines with NA. }
  \item{\code{cumulSpace}}{ gap to leave between top axis and cumulative plot legend.  Default NA for riojaPlot to guess. }
  \item{\code{fun.lithology}}{ function that passes a single dat frame or tibble of information for plotting the lithology column. }
  \item{\code{lithology.width}}{ width of lithology column, as fraction of page width. }
}

\code{fun.xback} and \code{fun.xfront} are user-defined functions of the form `myfun(x, y, i, nm)` that pass four arguments: x and y values, the index of the plot (i, from 1 to nplots, left to right) and the variable name (nm) to be plotted, as a named character vector.  

\code{addRPClust} adds a dendrogram to an existing riojaPlot diagram.

\code{addRPZone} adds user-defined horizontal zone lines to an existing diagram. 

\code{addRPClustZone} adds zone lines from a constrained clustering to an existing riojaPlot diagram.

\code{makeStyles} creates a riojaPlot style object that can be applied to riojaPlots.

\code{listStyles} returns a list of modifiable styles and their default values.

These function uses base graphics and \code{fig} to split the screen and may be incompatible with \code{par(mfrow)} and 
\code{split.screen}.
}

\value{
\code{riojaPlot} returns (invisibly) an object of class riojaPlot containing details of the overall diagram and individual curves and styles.

\code{makeStyles} returns an object of class riojaPlot.style.
}

\author{ 
Steve Juggins
}

\seealso{ 
\code{\link{strat.plot}}. 
}

\examples{

# use built-in data from Abernethy Forest
# see ?aber for citation

library(rioja)
data(aber)

# extract pollen percentages
spec <- aber$spec
# replace species codes with full taxon names
colnames(spec) <- aber$names$Name 
yvar <- aber$ages

# plot on depth scale
riojaPlot(spec, yvar, 
   yvar.name="Depth (cm)")

# scale for percentage data 
riojaPlot(spec, yvar, 
   yvar.name="Depth (cm)", 
   scale.percent=TRUE)
   
# reduce number of taxa and add exaggerations
# make a vector of taxon names with max abundance greater that 5 percent
mx <- apply(spec, 2, max)
selTaxa <- names(mx[mx > 5])

riojaPlot(spec, yvar, selTaxa,
   yvar.name="Depth (cm)", 
   scale.percent=TRUE,
   exag=TRUE)

# group taxa by type and add cumulative graph
# extract types
types <- aber$names[, -1]
# convert pollen types to a factor
types$Group <- factor(types$Group, levels=c("Trees", "Shrubs", "Herbs"))
riojaPlot(spec, yvar, selTaxa, types,
   yvar.name="Depth (cm)", 
   scale.percent=TRUE,
   plot.groups=TRUE,
   plot.cumul=TRUE,
   exag=TRUE)

# or plot on age scale with depth as secondary and italicise names
riojaPlot(spec, yvar, selTaxa, types,
   sec.yvar.name="Depth (cm)", 
   yvar.name="Age (years BP)",
   ymin=6000,
   ymax=14300,
   yinterval=500,
   scale.percent=TRUE,
   plot.sec.axis=TRUE, 
   plot.groups=TRUE, 
   plot.cumul=TRUE,
   exag=TRUE,
   names.italicise = TRUE)

# add a zonation

riojaPlot(spec, yvar, selTaxa, types,
   sec.yvar.name="Depth (cm)", 
   yvar.name="Age (years BP)",
   ymin=6000,
   ymax=14300,
   yinterval=500,
   plot.sec.axis=TRUE, 
   scale.percent=TRUE,
   plot.groups=TRUE, 
   plot.cumul=TRUE,
   exag=TRUE, 
   do.clust=TRUE,
   plot.clust=TRUE, 
   plot.zones="auto", 
   names.italicise = TRUE)

# save settings as a style and apply to figure, fine-tuning the y-axis:

mystyle <- makeStyles(
   plot.sec.axis=TRUE, 
   scale.percent=TRUE,
   plot.groups=TRUE, 
   plot.cumul=TRUE,
   exag=TRUE, 
   plot.bar="full",
   plot.poly=TRUE,
   bar.back=TRUE,
   lwd.bar=0.5,
   col.bar="lightgrey",
   do.clust=TRUE,
   plot.clust=TRUE, 
   plot.zones="auto",
   srt.xlabel=45)

riojaPlot(spec, yvar, selTaxa, types, mystyle,
   sec.yvar.name="Depth (cm)", 
   yvar.name="Age (years BP)",
   ymin=6250,
   ymax=14300,
   yinterval=500, 
   ytks1=seq(6000, 14500, by=500))
}

\keyword{ hplot }
