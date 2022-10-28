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
          names.italicise=TRUE,
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

riojaPlot(pca, chron, riojaPlot=rp, 
          yvar.name="Age (years BP)",
          plot.bar=TRUE,
          xGap=0.02)

# Custom plotting function

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

myfun <- function(x, y, i, nm) {
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

# put dendrogram on right

clust <- chclust(dist(sqrt(poll)))

rp1 <- riojaPlot(poll, chron, groups=types, selVars=names(mx5),
          yvar.name="Age (years BP)",
          ymin=6290, ymax=14250, yinterval=500,
          sec.yvar.name="Depth (cm)",
          plot.sec.axis = TRUE,
          plot.groups=TRUE,
          plot.cumul=TRUE,
          scale.percent=TRUE,
          plot.top.axis=TRUE,
          ytks1=seq(6000, 14500, by=500),
          xRight=0.7)

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
rp1 <- riojaPlot(poll, chron, groups=types, selVars=names(mx5),
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
          xRight=0.8,
          fun.xback=myfun)

zone.y <- c(7000, 9000, 10500, 12000, 13500)
zone.names <- paste("Zone", 1:5)
zones <- data.frame(zone.y, zone.names)
addRPZoneNames(rp1, zones, xLeft=0.8, xRight=0.9, cex=0.6)  
  
addRPClust(rp1, clust, xLeft=0.9)
addRPClustZone(rp1, clust, xLeft=rp1$box[1], xRight=0.9, col="red")


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
symb.fun <- function(x, y, i, nm) {
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
          plot.line=FALSE
          )

fun.gam <- function(x, y, i, nm) {
  tmp <- data.frame(x=y, y=x)
  gam <- mgcv::gam(y ~ s(x, k=50), data=tmp)
  x2 <- predict(gam, type="response")
  lines(x2, y, col="red", lwd=2)
}

rp2 <- riojaPlot(mag, mag.chron[, "Age BP", drop=FALSE], 
           riojaPlot=rp1, xGap = 0.01,
           xRight=0.8, scale.minmax=FALSE, 
           plot.bar=FALSE, plot.line=F, 
           plot.symb=TRUE, symb.cex=0.3, fun.xfront=fun.gam)
rp3 <- riojaPlot(loi, loi.chron[, "Age BP", drop=FALSE], 
           riojaPlot=rp2,
           xRight=0.9, 
           scale.minmax=FALSE, plot.bar=F, 
          plot.line=F, plot.symb=TRUE, symb.cex=0.3, fun.xfront=fun.gam)
riojaPlot(BSi, BSi.chron[, "Age BP", drop=FALSE], 
           riojaPlot=rp3,
           xRight=0.99, 
           scale.minmax=FALSE, plot.bar=FALSE, 
           plot.line=FALSE, plot.symb=TRUE, symb.cex=0.3, fun.xfront=fun.gam)

# combining multiple plots

svg("Test/test.svg", width=10, height=6)
riojaPlot(poll, chron,
          yvar.name="Age (years BP)",
          ymin=6000, ymax=14400, yinterval=500,
          scale.percent=TRUE,
          )
dev.off()

allpoll_list <- rio::import_list("Test/Woodbridge_et_al_2014_Data.xlsx")
lcc_lookup <- read_excel("Test/LCC_info.xlsx", sheet="LCC_Lookup")

allpoll_nested <- tibble(Site=names(allpoll_list), polldata=allpoll_list) 

types <- lcc_lookup %>% select(VarName, LCC_name)
types$LCC_name <- factor(types$LCC_name, levels=sort(unique(types$LCC_name))[c(2, 3, 5, 4, 6, 1)])

fun_plot <- function(x, y) {
   non_pollen <- c("Sample",
                "Radiocarbon years B.P.",
                "EPD default [yrs.BP.]",
                "EPD [yrs.BP.]",
                "Fossilva [yrs.BP.]", 
                "Sum")
   x <- x %>% select(!contains(non_pollen)) %>%
   rename("Depth"=`Depth (cm)`, "Age_BP"=`Cal. yr. BP`) %>%
   filter(Age_BP < 9000) 
   spec <- x[, -c(1:2)]
   spec <- spec / rowSums(spec) * 100
   spec <- spec[, apply(spec, 2, max) > 2]
   ymin <- min(x$Age_BP)
   ymax <- max(x$Age_BP)
   yinterval <- 200
   ytks1 <- seq(0, 9000, by=200)
   svg(paste0("Test/svgs/", y, ".svg"), width=10, height=6)
   riojaPlot(spec, x[, 1:2], 
             yvar.name="Age_BP",
             groups=types,
             plot.groups=TRUE,
             plot.cumul=TRUE,
             scale.percent=TRUE, ymin=ymin, ymax=ymax, 
             yinterval=200, ytks1=ytks1,
             srt.xlabel=45)
   mtext(y, side=3, outer=F, line=3, adj=0)
   dev.off()
}

  allpoll_nested[1:16, ] %>%
    walk2(.x=.$polldata, .y=.$Site, .f=~fun_plot(.x, .y))

library(cowplot)
fnames <- list.files("Test/svgs", "*.svg", full.names=TRUE)
plts <- map(fnames[1:9], function(x) { cowplot::ggdraw() + cowplot::draw_image(x, halign=0, valign=0) } )

cowplot::plot_grid(plotlist=plts)

x <- 1:10
usethis::use_data(x)
