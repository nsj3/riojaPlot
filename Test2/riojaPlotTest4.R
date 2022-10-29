library(rioja)
library(readxl)
library(dplyr)


polldata <- read_excel("Test/Woodbridge_et_al_2014_Data.xlsx", sheet="REDMERE")
lcc_lookup <- read_excel("Test/LCC_info.xlsx", sheet="LCC_Lookup")
non_pollen <- c("Sample",
                "Radiocarbon years B.P.",
                "EPD default [yrs.BP.]",
                "EPD [yrs.BP.]",
                "Fossilva [yrs.BP.]", 
                "Sum")
del <- colnames(polldata) %in% non_pollen
polldata <- polldata[, !del]
depth_age <- subset(polldata, select=c(`Depth (cm)`, `Cal. yr. BP`))
poll_count <- subset(polldata, select=-c(`Depth (cm)`, `Cal. yr. BP`))
colnames(depth_age) <- c("Depth", "Age_BP")
poll_pc <- poll_count / rowSums(poll_count) * 100

grps <- data.frame(VarName=colnames(poll_pc)) %>%
  left_join(lcc_lookup) %>%
  select(VarName, P_Group)

grps2 <- lcc_lookup %>% select(VarName, LCC_name)
grps2$P_Group <- factor(grps2$LCC_name)

riojaPlot(poll_pc, depth_age, groups=grps2, 
         scale.percent=TRUE,
         plot.groups=TRUE,
         plot.cumul=TRUE,
         yvar.name="Age_BP",
         cex.cumul=0.5)

stop()

pollen <- read_excel("inst/extdata/aishihik2013.xlsx", sheet="Pollen Counts")
sed <- read_excel("inst/extdata/aishihik2013.xlsx", sheet="Sediment Parameters")
char <- read_excel("inst/extdata/aishihik2013.xlsx", sheet="Macrocharcoal")

pollen.chron <- pollen %>% select(1:2)
pollen <- pollen %>% select(`Picea glacua`: Umbelliferae)
pollen[is.na(pollen)] <- 0
pollen.pc <- vegan::decostand(pollen, method="total", margin=2) * 100

types <- data.frame(Name=colnames(pollen.pc), Group="Herbs")
types$Group[1:15] <- "Trees & shrubs"
types$Group <- factor(types$Group, levels=c("Trees & shrubs", "Herbs"))

sed.chron <- sed %>% select(1:2)
sed <- sed %>% select(3:5)
char.chron <- char %>% select(1:2)
char <- char %>% 
  select(`Macrocharcoal influx`) %>% 
  mutate(`Macrocharcoal influx` = log10(`Macrocharcoal influx`)) 

selVars <- colnames(pollen.pc)[apply(pollen.pc, 2, sum) > 20] 

rp <- riojaPlot(pollen.pc, pollen.chron, groups=types, selVars=selVars, 
          ymin = -100,
          ymax = 12000,
          yinterval = 500,
          yvar.name = "Age BP",
          plot.sec.axis = TRUE,
          sec.yvar.name = "Mean cm",
          scale.percent=TRUE,
          plot.groups=TRUE,
          plot.bar=FALSE, 
          do.clust = F, 
          plot.zones = "auto",
          plot.clust=TRUE,
          plot.cumul=TRUE,
          xRight = 0.6,
          exag=TRUE,
          exag.mult=10,
          srt.xlabel=45)

riojaPlot2(rp, sed, sed.chron[, "Age BP", drop=FALSE], xRight=0.9)
riojaPlot2(rp, char, char.chron[, "Age BP", drop=FALSE], xLeft=0.9, xRight=0.99)


pollen <- read_excel("inst/extdata/allen1999.xlsx", sheet="Pollen data", skip=2)
pollen.chron <- pollen %>% select(1)
pollen <- pollen %>% select(Pinus:`Other herbaceous taxa`)
types <- data.frame(Name=colnames(pollen), Group="Woody taxa")
types$Group[9:12] <- "Herbs"
types$Group <- factor(types$Group, levels=c("Woody taxa", "Herbs"))

mag <- read_excel("inst/extdata/allen1999.xlsx", sheet="Magnetic susceptibility", skip=2)
mag.chron <- mag %>% select(1) 
mag <- mag %>% select(`Mag Susc`) %>% mutate(`Mag Susc` = `Mag Susc` / 1000)
loi <- read_excel("inst/extdata/allen1999.xlsx", sheet="Loss on ignition", skip=2)
loi.chron <- loi %>% select(1)
loi <- loi %>% select("LOI (wt %)")
BSi <- read_excel("inst/extdata/allen1999.xlsx", sheet="Biogenic silica", skip=2)
BSi.chron <- BSi %>% select(1)
BSi <- BSi %>% select("BSi")

clust <- chclust(dist(sqrt(pollen)))
bs <- bstick(clust, plot=FALSE)
bs2 <- bs$dispersion <= bs$bstick

fun.sm <- function(x, y, i, nm) {
  tmp <- data.frame(x=y, y=x)
  tmp <- na.omit(tmp)
  lo <- lowess(tmp, f=0.02)
  lines(lo$y, lo$x, col="darkgrey", lwd=1)
}

fun.rm <- function(x, y, i, nm) {
   x <- as.numeric(stats::filter(x, rep(1, 9))) / 9
   n <- length(y)
   lines(x, y, col="red")
}

library(strucchange)
fun.bp <- function(x, y, i, nm) {
  print(nm)
  tmp <- data.frame(x=x, y=y)
  bp <- breakpoints(x ~ 1, data=tmp, h=0.05) #, hpc="foreach")
  if(!is.na(bp$breakpoints[1])) {
    h1 <- tmp$y[bp$breakpoints]
    h2 <- tmp$y[bp$breakpoints+1]
    h3 <- (h1+h2) / 2
#    abline(h=h3, col="orange", lty=1, lwd=1)
    bf <- breakfactor(bp)
    pr <- factor(levels(bf))
    fm1 <- lm(x ~ bf, data=tmp)
    pd <- predict(fm1, data.frame(bf=pr))
    ft <- fitted(fm1)
    h4 <- NULL
    x4 <- NULL
    for (i in 1:length(h3)) {
      h4 <- c(h4, h3[i], h3[i])
      x4 <- c(x4, pd[i], pd[i+1])
    }
    y5 <- c(y, h4)
    o <- order(y5)
    y5 <- y5[o]
    x5 <- c(ft, x4)[o]
    with(tmp, lines(x5, y5, col="red", lwd=2))
  }
}  

fun.gam <- function(x, y, i, nm) {
  tmp <- data.frame(x=y, y=x)
  gam <- mgcv::gam(y ~ s(x, k=100), data=tmp)
  x2 <- predict(gam, type="response")
  lines(x2, y, col="blue", lwd=1)
}

rp <- riojaPlot(pollen, pollen.chron, groups=types, 
          yinterval = 5000,
          ymin = 0, 
          ymax=102000,
          yvar.name = "Age BP",
          scale.percent=TRUE,
          plot.groups=TRUE,
          plot.bar=FALSE, 
          do.clust = T, 
          plot.zones = "auto",
          plot.clust=T,
          plot.cumul=TRUE,
          xRight = 0.7,
          exag=FALSE,
          exag.mult=5,
          srt.xlabel=45,
          xSpace = 0.01,
          plot.line=F,
          tcl=-0.1,
          cex.yaxis=0.7,
          )

# riojaPlot2(rp, pollen, pollen.chron, xSpace=0.001, scale.percent=FALSE, scale.minmax=TRUE)

riojaPlot2(rp, mag, mag.chron[, "Age BP", drop=FALSE], xRight=0.8, scale.minmax=TRUE, gap=0.02, fun2=fun.sm, plot.bar=F, plot.line=F, plot.symb=T, symb.cex=0.2)
riojaPlot2(rp, loi, loi.chron[, "Age BP", drop=FALSE], xLeft=0.8, xRight=0.9, scale.minmax=FALSE, fun2=fun.rm, plot.bar=F, plot.line=F, plot.symb=T, symb.cex=0.2)

riojaPlot2(rp, loi, loi.chron[, "Age BP", drop=FALSE], xLeft=0.8, xRight=0.9, scale.minmax=FALSE, fun2=fun.gam, plot.bar=F, plot.line=F, plot.symb=T, symb.cex=0.2)

riojaPlot2(rp, BSi, BSi.chron[, "Age BP", drop=FALSE], xLeft=0.9, xRight=0.99, scale.minmax=FALSE, fun2=fun.bp, plot.bar=F, plot.line=F, plot.symb=T, symb.cex=0.3)

riojaPlot2(rp, BSi, BSi.chron[, "Age BP", drop=FALSE], xLeft=0.9, xRight=0.99, scale.minmax=FALSE, fun2=fun.gam, plot.bar=F, plot.line=F, plot.symb=T, symb.cex=0.3)

# example in help file

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

# or plot on age scale with depth as secondary
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
   plot.bar="full",
   bar.back=TRUE,
   lwd.bar=0.5,
   col.bar="lightgrey")

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
   col.exag.line="black",
   lwd.exag.line=0.2,
   col.exag="white",
   lwd.axis=1,
)

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
   yinterval=500,
)




