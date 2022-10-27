library(vegan) ## decorana
data(RLGH)

# remove less abundant taxa
mx <- apply(RLGH$spec, 2, max)
spec <- RLGH$spec[, mx > 3]
depth <- RLGH$depths$Depth
#basic stratigraphic plot
strat.plot(spec, y.rev=TRUE)
#scale for percentage data
strat.plot(spec, y.rev=TRUE, scale.percent=TRUE)
# plot by sample depth
strat.plot(spec, yvar = depth, y.rev=TRUE, scale.percent=TRUE,
title="Round Loch of Glenhead", ylabel="Depth (cm)")
# add a dendromgram from constrained cluster analysis
diss <- dist(sqrt(RLGH$spec/100)^2)
clust <- chclust(diss, method="coniss")
# broken stick model suggest 3 significant zones
bstick(clust)
x <- strat.plot(spec, yvar = depth, y.rev=TRUE,
scale.percent=TRUE, title="Round Loch of Glenhead", ylabel="Depth (cm)",
clust=clust)
# add zones
addClustZone(x, clust, 3, col="red")
# use fig to control diagram size and position
x <- strat.plot(spec, xRight = 0.7, yvar = depth, y.rev=TRUE,
scale.percent=TRUE, title="Round Loch of Glenhead", ylabel="Depth (cm)")
# add curves for first two DCA components of diatom data
dca <- decorana(spec, iweigh=1)
sc <- scores(dca, display="sites", choices=1:2)
strat.plot(sc, xLeft = 0.7, yvar = depth, y.rev=TRUE, xRight=0.99,
y.axis=FALSE, clust=clust, clust.width=0.08, add=TRUE)

# Use custom function to add smooth to curve

sm.fun <- function(x, y, i, nm) {
  tmp <- data.frame(x=y, y=x)
  tmp <- na.omit(tmp)
  lo <- lowess(tmp, f=0.3)
  lines(lo$y, lo$x, col="red", lwd=1)
}

x <- strat.plot(spec, yvar = depth, y.rev=TRUE, scale.percent=TRUE, 
title="Round Loch of Glenhead", ylabel="Depth (cm)", fun1=sm.fun)

# Pollen diagram using built-in Abernethy Forest dataset
data(aber)
age <- aber$ages$`Age (years BP)`
spec <- aber$spec

# basic silhouette plot
strat.plot(spec, yvar = age, y.rev=TRUE, scale.percent=TRUE, ylabel="Age (years BP)", 
plot.poly=TRUE, col.poly="darkgreen", col.poly.line=NA)

# add exaggerated curves
strat.plot(spec, yvar = depth, y.rev=TRUE, scale.percent=TRUE, ylabel="Depth (cm)", 
plot.poly=TRUE, col.poly="darkgreen", col.poly.line=NA, exag=TRUE)

# use different colours for trees shrubs
xx <- 1:ncol(spec)
cc <- ifelse(xx < 8, "darkgreen", "darkred")
strat.plot(spec, yvar = depth, y.rev=TRUE, scale.percent=TRUE, ylabel="Depth (cm)", 
plot.poly=TRUE, col.poly=cc, col.poly.line=NA, exag=TRUE, col.exag="auto")

# fix y-axis scale
strat.plot(spec, yvar = depth, y.rev=TRUE, scale.percent=TRUE, ylabel="Depth (cm)", 
plot.poly=TRUE, col.poly=cc, col.poly.line=NA, exag=TRUE, col.exag="auto", 
ylim=c(6000, 14400))

# or use custom y-axis intervals

yvals <- seq(6000, 14400, by=500)
strat.plot(spec, yvar = depth, y.rev=TRUE, scale.percent=TRUE, ylabel="Depth (cm)", 
plot.poly=TRUE, col.poly=cc, col.poly.line=NA, exag=TRUE, col.exag="auto", 
ylim=c(6200, 14200), y.tks=yvals)

# move y-axis label, change label font size
strat.plot(spec, yvar = depth, y.rev=TRUE, scale.percent=TRUE, ylabel="Depth (cm)", 
plot.poly=TRUE, col.poly=cc, col.poly.line=NA, exag=TRUE, col.exag="auto", 
ylim=c(6200, 14200), y.tks=yvals, ylabPos=3, xLeft=0.1, cex.xlabel=0.8)
