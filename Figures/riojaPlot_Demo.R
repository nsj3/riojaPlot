# remotes::install_github("nsj3/riojaPlot", built_vignettes=TRUE, dependencies=TRUE)
# or
# install.packages("riojaPlot", repos="https://nsj3.r-universe.dev")
library(riojaPlot)
options(tidyverse.quiet = TRUE)
library(tidyverse)
library(readxl)
library(rioja)

# browseVignettes("riojaPlot")

# use built-in data from Abernethy Forest
# see ?aber for citation
data(aber)
names(aber)

head(aber$spec)
head(aber$ages)

# extract pollen percentages
poll <- aber$spec
head(poll)

# replace species codes with full taxon names
colnames(poll) <- aber$names$Name 

chron <- aber$ages
head(chron)

# plot on depth scale (depth is the first column in chron)
riojaPlot(poll, chron)

# plot as percentage data
# default is to plot darkgreen silhouettes with black outline
riojaPlot(poll, chron,
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
          cex.xlabel=0.7,
          col.bar="black")

# plot on an age scale
riojaPlot(poll, chron,
          yvar.name="Age (years BP)",
          scale.percent=TRUE)

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
# The base R way
mx <- apply(poll, 2, max) > 5
mx
poll2 <- poll[, mx]

# The tidyverse way
fun.max <- function(x, cut=2) {
   max(x, na.rm=TRUE) > cut 
}
poll2 <- poll %>% select(where(~ fun.max(.x, 5)))

riojaPlot(poll2, chron,
          yvar.name="Age (years BP)",
          ymin=6000, ymax=14400, yinterval=500,
          sec.yvar.name="Depth (cm)",
          plot.sec.axis = TRUE,
          scale.percent=TRUE)

# or give a character vector of names to include 
mx5_names <- names(mx[mx])
mx5_names

# Taxa will be plotted in the order they appear in the 
# character vector, so this method allows you to manually 
# select and re-order taxa

riojaPlot(poll, chron, selVars=mx5_names,
          yvar.name="Age (years BP)",
          ymin=6000, ymax=14400, yinterval=500,
          sec.yvar.name="Depth (cm)",
          plot.sec.axis = TRUE,
          scale.percent=TRUE)

# Plot groups
# for this we need a df or tibble with 2 cols:
# col1 = names, col2=groups

# show groups for the aber data
head(aber$names)

# need to pass a data frame with 2 cols
# col 1 = variable names spelled exactly as in data
# character vector or factor of group names
types <- aber$names[, -1]
head(types)

riojaPlot(poll2, chron, groups=types,
          yvar.name="Age (years BP)",
          ymin=6000, ymax=14400, yinterval=500,
          sec.yvar.name="Depth (cm)",
          plot.sec.axis = TRUE,
          plot.groups=TRUE,         # turns on plotting of groups
          scale.percent=TRUE)

# show cumulative plot
riojaPlot(poll2, chron, groups=types,
          yvar.name="Age (years BP)",
          ymin=6000, ymax=14400, yinterval=500,
          sec.yvar.name="Depth (cm)",
          plot.sec.axis = TRUE,
          plot.groups=TRUE,   # turns on plotting of groups
          plot.cumul=TRUE,    # turns on cumulative plot
          scale.percent=TRUE)

# groups are plotted alphabetically by default
# to reorder supply the grouping column as a factor, 
# and specify the order of the levels
types$Group <- factor(types$Group, levels=c("Trees", "Shrubs", "Herbs"))
riojaPlot(poll2, chron, groups=types,
          yvar.name="Age (years BP)",
          ymin=6000, ymax=14400, yinterval=500,
          sec.yvar.name="Depth (cm)",
          plot.sec.axis = TRUE,
          plot.groups=TRUE,
          plot.cumul=TRUE,
          scale.percent=TRUE)

# cumulative plots are calculated from the data passed to riojaPlot
# if you have removed rare taxa from the data the cumul plot will 
# no longer sum to 100%
# So, pass the whole dataset to riojaPlot and use selVArs to select 
# taxa for plotting
riojaPlot(poll, chron, groups=types, selVars=mx5_names,
          yvar.name="Age (years BP)",
          ymin=6000, ymax=14400, yinterval=500,
          sec.yvar.name="Depth (cm)",
          plot.sec.axis = TRUE,
          plot.groups=TRUE,
          plot.cumul=TRUE,
          scale.percent=TRUE)

# Adjust some cosmetics
# rotate and italicise names, add top axis,
# reduce size of axis labels
riojaPlot(poll, chron, groups=types, selVars=mx5_names,
          yvar.name="Age (years BP)",
          ymin=6000, ymax=14400, yinterval=500,
          sec.yvar.name="Depth (cm)",
          plot.sec.axis = TRUE,
          plot.groups=TRUE,
          plot.cumul=TRUE,
          names.italicise=TRUE,  # italicise names
          scale.percent=TRUE,
          plot.top.axis=TRUE,    # add an x-axis at top
          srt.xlabel=45,         # rotate names
          cex.axis=0.5)          # reduce font of x-axes

# add dendrogram
# clust.data.trans="sqrt" transforms data to sqrt. This gives Hellinger's 
# distances with percentage data
riojaPlot(poll, chron, groups=types, selVars=mx5_names,
          yvar.name="Age (years BP)",
          ymin=6000, ymax=14400, yinterval=500,
          sec.yvar.name="Depth (cm)",
          plot.sec.axis = TRUE,
          plot.groups=TRUE,
          plot.cumul=TRUE,
          scale.percent=TRUE,
          srt.xlabel=45,
          cex.axis=0.5,
          do.clust=TRUE,     # perform clustering
          clust.data.trans="sqrt", # transform data to sqrt before calculating dissimilarities
          plot.clust=TRUE)

riojaPlot(poll, chron, groups=types, selVars=mx5_names,
          yvar.name="Age (years BP)",
          ymin=6000, ymax=14400, yinterval=500,
          sec.yvar.name="Depth (cm)",
          plot.sec.axis = TRUE,
          plot.groups=TRUE,
          plot.cumul=TRUE,
          scale.percent=TRUE,
          plot.top.axis=TRUE,
          srt.xlabel=45,
          cex.xlabel=0.7,
          cex.axis=0.5,
          do.clust=TRUE,
          clust.data.trans="sqrt",
          plot.clust=TRUE,
          plot.zones="auto")

# fine tune y-axis scale by specifying tick-value
sapply(chron, range)
riojaPlot(poll, chron, groups=types, selVars=mx5_names,
          yvar.name="Age (years BP)",
          ymin=6290, ymax=14250, yinterval=500,
          sec.yvar.name="Depth (cm)",
          plot.sec.axis = TRUE,
          plot.groups=TRUE,
          plot.cumul=TRUE,
          scale.percent=TRUE,
          ytks1=seq(6000, 14500, by=500),
          srt.xlabel=45,
          do.clust=TRUE,
          clust.data.trans="sqrt",
          plot.clust=TRUE,
          plot.zones="auto")

# Add second dataset
# we do a PCA of the pollen data and plot scores of the first
# two components
pca <- vegan::rda(sqrt(poll)) %>% 
  vegan::scores(display="sites") %>%
  as_tibble()
pca

# We set xRight = 0.8 so the main plot takes up 80% of the page width
# we save the output from riojaPlot and pass this to a second call to 
# riojaPlot to plot the pca scores

rp <- riojaPlot(poll, chron, groups=types, selVars=mx5_names,
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
          clust.data.trans="sqrt",
          plot.zones="auto", 
          xRight=0.8)

riojaPlot(pca, chron, riojaPlot=rp, 
          yvar.name="Age (years BP)",
          plot.bar=TRUE,
          xGap=0.02)

# Custom plotting function to show PCA scores as deviations 
# from zero.  Not a super-useful thing to do but it 
# illustrates how you can customise all or individual plots

rp <- riojaPlot(poll, chron, groups=types, selVars=mx5_names,
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
          clust.data.trans="sqrt",
          plot.clust=TRUE,
          plot.zones="auto", 
          xRight=0.85)

myfun <- function(x, y, i, nm, style) {
  usr <- par("usr")
  segments(0, usr[3], 0, usr[4], col="lightgrey")
  segments(0, y, x, y, col="lightgrey")
}

riojaPlot(pca, chron, riojaPlot=rp, 
          yvar.name="Age (years BP)",
          plot.bar=FALSE,
          xGap=0.02,
          col.axis=NA,
          fun.xback=myfun)

# We can put dendrogram on right by pre-calculating the zonation
# and adding to the plot

clust <- chclust(dist(sqrt(poll)))
rp1 <- riojaPlot(poll, chron, groups=types, selVars=mx5_names,
          yvar.name="Age (years BP)",
          ymin=6290, ymax=14250, yinterval=500,
          sec.yvar.name="Depth (cm)",
          plot.sec.axis = TRUE,
          plot.groups=TRUE,
          plot.cumul=TRUE,
          scale.percent=TRUE,
          plot.top.axis=TRUE,
          ytks1=seq(6000, 14500, by=500),
          xRight=0.75)

rp2 <- riojaPlot(pca, chron, riojaPlot=rp1, 
          yvar.name="Age (years BP)",
          plot.bar=FALSE,
          col.axis=NA,
          xRight=0.9,
          fun.xback=myfun)

addRPClust(rp2, clust)
addRPClustZone(rp2, clust, xLeft=rp1$box[1], col="red")

# Add a zone column with names

clust <- chclust(dist(sqrt(poll)))
rp1 <- riojaPlot(poll, chron, groups=types, selVars=mx5_names,
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
          xRight=0.7)

rp2 <- riojaPlot(pca, chron, riojaPlot=rp1, 
          yvar.name="Age (years BP)",
          plot.bar=FALSE,
          col.axis=NA,
          xRight=0.82,
          fun.xback=myfun)

zone.y <- c(7000, 9000, 10500, 12000, 13500)
zone.names <- paste("Zone", 1:5)
zones <- data.frame(zone.y, zone.names)
zones
addRPZoneNames(rp1, zones, xLeft=0.82, xRight=0.9, cex=0.6)  
  
addRPClust(rp1, clust, xLeft=0.9)
addRPClustZone(rp1, clust, xLeft=rp1$box[1], xRight=0.9, col="red")

# Add a column of lithology using a custom function

lithology <- data.frame(
  top=   c(300, 325, 375, 400, 420, 440, 464, 510, 535), 
  bottom=c(325, 375, 400, 420, 440, 464, 510, 535, 550),
  lithology=c("Peat", "Mud", "NS", "Mud", "Mud2", "SiltyMud", "SiltyMud2", "Mud3", "Silt"),
  colour=c("burlywood4", "saddlebrown", "NA", "brown4", "brown3", "navajowhite4", 
           "navajowhite3", "brown4", "lightsteelblue")
)

myfun2 <- function(x, style) {
   fun <- function(x) { rect(0, as.numeric(x[1]), 1, as.numeric(x[2]), col=x[4]) }
   apply(x, 1, fun)
}

mx <- apply(poll, 2, max)
mx5 <- mx[mx>10]

rp <- riojaPlot(poll, chron, selVars=names(mx5), lithology=lithology, 
          sec.yvar.name="Age (years BP)",
          sec.ymin=6000, sec.ymax=14000, sec.yinterval=500,
          yvar.name="Depth (cm)",
          plot.sec.axis = TRUE,
          scale.percent=TRUE, 
          fun.lithology=myfun2)

# lithology boundaries are on depth scale, use approx to convert to age scale:

lith_age <- lithology 

lith_age$top <- approx(chron$`Depth (cm)`, chron$`Age (years BP)`, xout=lithology$top)$y
lith_age$bottom <- approx(chron$`Depth (cm)`, chron$`Age (years BP)`, xout=lithology$bottom)$y

rp <- riojaPlot(poll, chron, selVars=names(mx5), lithology=lith_age, 
          yvar.name="Age (years BP)",
          ymin=6000, ymax=14000, yinterval=500,
          sec.yvar.name="Depth (cm)",
          plot.sec.axis = TRUE,
          scale.percent=TRUE, 
          fun.lithology=myfun2)


# Plot modern dataset with lake names on y-axis

data(Ponds)
# reorder the rows in decreasing TP
o <- order(Ponds$env$TP, decreasing=TRUE)
ponds.diat <- Ponds$spec[o, ]
ponds.TP <- data.frame(TP=round(Ponds$env$TP[o]), LakeName=Ponds$env$Name[o]) 
# replace taxon codes with names
colnames(ponds.diat) <- Ponds$names$Name
# remove rare species
mx <- apply(ponds.diat, 2, max)
ponds.sel <- colnames(ponds.diat)[mx > 10]

riojaPlot(ponds.diat, ponds.TP, selVars=ponds.sel,
          yvar.name="LakeName",
          sec.yvar.name="TP",
          sec.yinterval = 20,
          plot.sec.axis=TRUE,
          scale.percent=TRUE, 
          plot.poly=FALSE,
          plot.line=FALSE,
          plot.bar=TRUE,
          col.bar="black",
#          plot.symb=TRUE,
          lwd.bar = 2,
          symb.cex=0.4,
          cex.xlabel=0.6, 
          cex.yaxis=0.6,
          wa.order="topleft", 
          fun.xfront=myfun,
          names.italicise=TRUE,
          las.axis=2, 
          cex.axis=0.5)

ponds.TP <- ponds.TP %>%  mutate(bar.cols=case_when(TP < 100 ~ "green",
                            TP >= 100 & TP < 200 ~ "blue",
                            TP >= 200 & TP < 500 ~ "red",
                            TRUE ~ "orange")) 

ylab <- expression(Total~Phosph.~(mu*gL^{-1}))
riojaPlot(ponds.diat, ponds.TP, selVars=ponds.sel,
          yvar.name="LakeName",
          sec.yvar.name="TP",
          sec.yinterval = 20,
          plot.sec.axis=TRUE,
          scale.percent=TRUE, 
          plot.poly=FALSE,
          plot.line=FALSE,
          plot.bar=TRUE,
          col.bar="black",
          sep.bar=TRUE,
          col.sep.bar=ponds.TP$bar.cols,
          lwd.bar = 10,
          symb.cex=0.4,
          cex.xlabel=0.6, 
          cex.yaxis=0.6,
          wa.order="bottomleft", 
          names.italicise=TRUE,
          las.axis=2, 
          cex.axis=0.5)


# Different styles for different curves

fpath <- system.file("extdata/maule2020geochem.txt", package="riojaPlot")
maule <- readr::read_delim(fpath, skip=158, show_col_types = FALSE) 

maule.data <- maule %>% select(-(1:4))
maule.chron <- maule %>% select(1:4)

# How many measurements do we have for each variable:
unlist(lapply(maule.data, function(x) sum(!is.na(x))))

selsymb <- rep(FALSE, 9)
selsymb[c(1, 8:9)] <- TRUE
selbar <- rep(FALSE, 9)
selline <- rep(TRUE, 9)
selline[1] <- FALSE

groups <- data.frame(names=colnames(maule.data), groups=c(1, 2, 2, 2, 2, 2, 2, 3, 3))

riojaPlot(maule.data, maule.chron, groups=groups,
          yvar.name="age_BP", 
          plot.bar=selbar,
          plot.poly=FALSE,
          plot.line=selline,
          plot.symb=selsymb,
          plot.groups=TRUE,
          ylabPos=2,
          symb.cex=0.5,
          )

# also rotate x-axis values
# and tidy up y-axis values

myticks <- seq(0, 13000, by=500)
rp <- riojaPlot(maule.data, maule.chron, groups=groups,
          yvar.name="age_BP", 
          ylabel="Age (years BP)",
          ymin=-100, 
          ymax=13300,
          yinterval=500,
          plot.bar=FALSE,
          plot.poly=FALSE,
          plot.line=selline,
          plot.symb=selsymb,
          plot.groups=TRUE,
          ylabPos=2.2,
          symb.cex=0.5,
          ytks1=myticks, 
          las.axis=2,
          yBottom=0.06
          )

# Plot symbols for rare types

aber.poll <- aber$spec
colnames(aber.poll) <- aber$names$Name 
aber.chron <- aber$ages
# calculate of max of each column
mx <- apply(aber.poll, 2, max)
#create a logical vector which is TRUE for taxa with max < 5
sel <- mx < 5
# define a custom function to plot symbols 
symb.fun <- function(x, y, i, nm, style) {
   sel <- x > 0
   if (sum(sel) > 0) {
      points(rep(3, sum(sel)), y[sel], cex=0.4, pch=19)
   }
}

# create a list of functions of length equal to the number of columns in the data
funlist <- lapply(1:ncol(aber.poll), function(x) symb.fun)
# now set the elements of the list where we don;t want to plot symbols to NULL
funlist[!sel] <- list(NULL)

# plot silhouettes and lines for taxa > 5% and apply our function to the others
riojaPlot(aber.poll, aber.chron, 
   scale.percent=TRUE,
   plot.poly=!sel,
   plot.bar = !sel,
   plot.line=FALSE,
   lwd.bar=0.6,
   exag=TRUE,
   fun.xfront=funlist)


# Plotting multiple datasets, add smooths

fpath <- system.file("extdata/allen1999.xlsx", package="riojaPlot")
pollen <- read_excel(fpath, sheet="Pollen data", skip=2)
pollen.chron <- pollen %>% select(1)
pollen <- pollen %>% select(Pinus:`Other herbaceous taxa`)
types <- data.frame(Name=colnames(pollen), Group="Woody taxa")
types$Group[9:12] <- "Herbs"
types$Group <- factor(types$Group, levels=c("Woody taxa", "Herbs"))
mag <- read_excel(fpath, sheet="Magnetic susceptibility", skip=2)
mag.chron <- mag %>% select(1) 
mag <- mag %>% select(`Mag Susc`) %>% mutate(`Mag Susc` = `Mag Susc` / 1000)
loi <- read_excel(fpath, sheet="Loss on ignition", skip=2)
loi.chron <- loi %>% select(1)
loi <- loi %>% select("LOI (wt %)")
BSi <- read_excel(fpath, sheet="Biogenic silica", skip=2)
BSi.chron <- BSi %>% select(1)
BSi <- BSi %>% select("BSi")

fun.gam <- function(x, y, i, nm, style) {
  tmp <- data.frame(x=y, y=x)
  gam <- mgcv::gam(y ~ s(x, k=50), data=tmp)
  x2 <- predict(gam, type="response")
  lines(x2, y, col="red", lwd=2)
}

rp1 <- riojaPlot(pollen, pollen.chron, groups=types, 
          yinterval = 5000,
          ymin = 0, 
          ymax=102000,
          yvar.name = "Age BP",
          scale.percent=TRUE,
          plot.groups=TRUE,
          do.clust = TRUE, 
          plot.zones = "auto",
          plot.clust=TRUE,
          plot.cumul=TRUE,
          cex.cumul=0.6,
          srt.xlabel=45,
          xSpace = 0.01,
          plot.bar=FALSE,
          tcl=-0.1,
          cex.yaxis=0.7,
          cex.xlabel=0.8,
          xRight = 0.7,
          plot.line=FALSE,
          yTop=0.8  )

xlab1 <- expression(Mag.~susc.~(10^{-3}~SI))
rp2 <- riojaPlot(mag, mag.chron[, "Age BP", drop=FALSE], 
           riojaPlot=rp1, xGap = 0.01,
           xRight=0.8, scale.minmax=FALSE, 
           plot.bar=FALSE, plot.line=F, 
           plot.symb=TRUE, symb.cex=0.3, fun.xfront=fun.gam,
           x.names=xlab1)
rp3 <- riojaPlot(loi, loi.chron[, "Age BP", drop=FALSE], 
           riojaPlot=rp2,
           xRight=0.9, 
           scale.minmax=FALSE, plot.bar=F, 
          plot.line=F, plot.symb=TRUE, symb.cex=0.3, fun.xfront=fun.gam)
riojaPlot(BSi, BSi.chron[, "Age BP", drop=FALSE], 
           riojaPlot=rp3,
           xRight=0.99, 
           scale.minmax=FALSE, plot.bar=FALSE, 
           plot.line=FALSE, plot.symb=TRUE, symb.cex=0.3, fun.xfront=fun.gam,
          x.names="Biog. silica (wt %)")

# changing widths

widths <- rep(1, ncol(poll))
inc <- rep(10, ncol(poll))
mx <- apply(poll, 2, max)
selTaxa <- names(mx[mx > 2])
sel <- which(mx < 5)
widths[sel] <- 5
inc[sel] <- 1

riojaPlot(poll, chron, selVars=selTaxa,
          scale.percent=TRUE,
          graph.widths=widths,
          min.width.pc=5, 
          x.pc.inc=inc,
          cex.axis=0.5,
          las.axis=2)


