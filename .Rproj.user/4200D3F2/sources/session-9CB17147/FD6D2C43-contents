suppressPackageStartupMessages(library(riojaPlot))
suppressPackageStartupMessages(library(tidyverse))

library(riojaPlot)

# use built-in data from Abernethy Forest
# see ?aber for citation
data(aber)

names(aber)

# extract pollen percentages
poll <- aber$spec
head(poll)

# replace species codes with full taxon names
colnames(poll) <- aber$names$Name 
head(poll)

chron <- aber$ages
head(chron)

# plot on depth scale (depth is the first column in chron)
riojaPlot::riojaPlot(poll, chron)

# plot as percentage data
# default is to plot darkgreen silhouettes with black outline
riojaPlot::riojaPlot(poll, chron,
          scale.percent=TRUE)

#turn off bars
riojaPlot(poll, chron,
          scale.percent=TRUE,
          plot.bar=FALSE)

#turn off silhouettes
riojaPlot(poll, chron,
          scale.percent=TRUE,
          plot.bar=FALSE, 
          plot.poly=FALSE)

# plot bars only
riojaPlot(poll, chron,
          scale.percent=TRUE,
          plot.line=FALSE, 
          plot.poly=FALSE,
          plot.bar=TRUE,
          lwd.bar=2,
          col.bar="black")

# plot on an age scale
riojaPlot(poll, chron,
          yvar.name="Age (years BP)",
          scale.percent=TRUE,
          )

# fix y-axis
sapply(chron, range)
riojaPlot(poll, chron,
          yvar.name="Age (years BP)",
          ymin=6000, ymax=14400, yinterval=500,
          scale.percent=TRUE,
          )

# show age and depth scale
riojaPlot(poll, chron,
          yvar.name="Age (years BP)",
          ymin=6000, ymax=14400, yinterval=500,
          sec.yvar.name="Depth (cm)",
          plot.sec.axis = TRUE,
          scale.percent=TRUE)

# remove rare taxa
# either remove from original data
mx <- apply(poll, 2, max) > 5
mx
poll2 <- poll[, mx]

riojaPlot(poll2, chron,
          yvar.name="Age (years BP)",
          ymin=6000, ymax=14400, yinterval=500,
          sec.yvar.name="Depth (cm)",
          plot.sec.axis = TRUE,
          scale.percent=TRUE)

# or give a character vector of names to include 
mx5 <- mx[mx]
mx5
riojaPlot(poll, chron, selVars=names(mx5),
          yvar.name="Age (years BP)",
          ymin=6000, ymax=14400, yinterval=500,
          sec.yvar.name="Depth (cm)",
          plot.sec.axis = TRUE,
          scale.percent=TRUE)

# show groups
head(aber$names)
types <- aber$names[, -1]
riojaPlot(poll2, chron, groups=types,
          yvar.name="Age (years BP)",
          ymin=6000, ymax=14400, yinterval=500,
          sec.yvar.name="Depth (cm)",
          plot.sec.axis = TRUE,
          plot.groups=TRUE,
          scale.percent=TRUE)

# show cumulative plot
riojaPlot(poll2, chron, groups=types,
          yvar.name="Age (years BP)",
          ymin=6000, ymax=14400, yinterval=500,
          sec.yvar.name="Depth (cm)",
          plot.sec.axis = TRUE,
          plot.groups=TRUE,
          plot.cumul=TRUE,
          scale.percent=TRUE)

# reorder groups
types$Group <- factor(types$Group, levels=c("Trees", "Shrubs", "Herbs"))
riojaPlot(poll2, chron, groups=types,
          yvar.name="Age (years BP)",
          ymin=6000, ymax=14400, yinterval=500,
          sec.yvar.name="Depth (cm)",
          plot.sec.axis = TRUE,
          plot.groups=TRUE,
          plot.cumul=TRUE,
          scale.percent=TRUE)

# calculate cumulative plot based on whole dataset
riojaPlot(poll, chron, groups=types, selVars=names(mx5),
          yvar.name="Age (years BP)",
          ymin=6000, ymax=14400, yinterval=500,
          sec.yvar.name="Depth (cm)",
          plot.sec.axis = TRUE,
          plot.groups=TRUE,
          plot.cumul=TRUE,
          scale.percent=TRUE)

# rotate and italicise names, add top axis,
# reduce size of axis labels
riojaPlot(poll, chron, groups=types, selVars=names(mx5),
          yvar.name="Age (years BP)",
          ymin=6000, ymax=14400, yinterval=500,
          sec.yvar.name="Depth (cm)",
          plot.sec.axis = TRUE,
          plot.groups=TRUE,
          plot.cumul=TRUE,
          scale.percent=TRUE,
          plot.top.axis=TRUE,
          srt.xlabel=45,
          cex.axis=0.5)

# add dendrogram
riojaPlot(poll, chron, groups=types, selVars=names(mx5),
          yvar.name="Age (years BP)",
          ymin=6000, ymax=14400, yinterval=500,
          sec.yvar.name="Depth (cm)",
          plot.sec.axis = TRUE,
          plot.groups=TRUE,
          plot.cumul=TRUE,
          scale.percent=TRUE,
          plot.top.axis=TRUE,
          srt.xlabel=45,
          cex.axis=0.5,
          do.clust=TRUE,
          plot.clust=TRUE)

riojaPlot(poll, chron, groups=types, selVars=names(mx5),
          yvar.name="Age (years BP)",
          ymin=6000, ymax=14400, yinterval=500,
          sec.yvar.name="Depth (cm)",
          plot.sec.axis = TRUE,
          plot.groups=TRUE,
          plot.cumul=TRUE,
          scale.percent=TRUE,
          plot.top.axis=TRUE,
          srt.xlabel=45,
          cex.axis=0.5,
          do.clust=TRUE,
          plot.clust=TRUE,
          plot.zones="auto")


# fine tune y-axis scale
sapply(chron, range)
riojaPlot(poll, chron, groups=types, selVars=names(mx5),
          yvar.name="Age (years BP)",
          ymin=6290, ymax=14250, yinterval=500,
          sec.yvar.name="Depth (cm)",
          plot.sec.axis = TRUE,
          plot.groups=TRUE,
          plot.cumul=TRUE,
          scale.percent=TRUE,
          plot.top.axis=TRUE,
          ytks1=seq(6000, 14500, by=500),
          srt.xlabel=45,
          do.clust=TRUE,
          plot.clust=TRUE,
          plot.zones="auto")

# Add second dataset
pca <- vegan::rda(sqrt(poll)) %>% 
  vegan::scores(display="sites") %>%
  as_tibble()

rp <- riojaPlot(poll, chron, groups=types, selVars=names(mx5),
          yvar.name="Age (years BP)",
          ymin=6290, ymax=14250, yinterval=500,
          sec.yvar.name="Depth (cm)",
          plot.sec.axis = TRUE,
          plot.groups=TRUE,
          plot.cumul=TRUE,
          scale.percent=TRUE,
          plot.top.axis=TRUE,
          ytks1=seq(6000, 14500, by=500),
          srt.xlabel=45,
          do.clust=TRUE,
          plot.clust=TRUE,
          plot.zones="auto", 
          xRight=0.8)

riojaPlot(pca, chron, riojaPlot=rp)


