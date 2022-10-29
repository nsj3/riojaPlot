# use built-in data from Abernethy Forest
# see ?aber for citation

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

# save settings as a style and apply to figure:

mystyle <- makeStyles(
   plot.sec.axis=TRUE, 
   scale.percent=TRUE,
   plot.groups=TRUE, 
   plot.cumul=TRUE,
   exag=TRUE, 
   plot.bar="full",
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
   ymin=6000,
   ymax=14300,
   yinterval=500)






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
