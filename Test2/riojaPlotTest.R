library(rioja)
#library(dplyr)
#library(forcats)

data(aber)

spec <- aber$spec
depth <- aber$ages$`Depth (cm)`
yvar <- aber$ages[, 1:2]
types <- aber$names %>% select(Code, Group) %>% mutate(Group=factor(Group, levels=c("Trees", "Shrubs", "Herbs")))

# types$Name[1] <- "Bet"

mx <- apply(spec, 2, max)
selTaxa <- names(mx[mx > 5])

# pdf("test.pdf", width=12, height=8)

# svg("test.svg", width=10, height=6)

ylab <- expression(""^{14}*C~years~BP)

# colnames(spec)[1] <- "Bet"

dca <- vegan::decorana(spec, iweigh=1)
sc <- vegan::scores(dca, display="sites", choices=1:2)

diss <- dist(sqrt(spec))
diss <- dist(spec)

myclust <- chclust(diss)
bs <- bstick(myclust)
bs2 <- bs$dispersion > bs$bstick
bs2
which(bs2)

x <- riojaPlot(spec, yvar, selVar=selTaxa, groups=types, 
   yvar.name="Age (14C years BP)",
   sec.yvar.name="Depth (cm)", 
   ylabel = ylab,
   plot.sec.axis=TRUE, 
   plot.groups=TRUE, 
   plot.cumul=TRUE,
   exag=TRUE, 
   col.exag="auto",
   col.poly.line="grey50",
   cex.cumul=0.7,
   srt.xlabel=45,
   cex.xlabel=0.8,
   cex.yaxis=0.8,
   plot.clust=TRUE, 
   yinterval=250, 
   ymin=5500,
   plot.bar="full",
   bar.back=FALSE, 
   names.break.n=15,
   lwd.bar=0.5, 
   plot.zones="auto",
   col.zones="blue", 
   clust.width=0.04, 
   xRight = 0.8, 
   cumul.mult=2,
   do.clust=TRUE, 
   x.names = aber$names$Name)

addRPClustZone(x, myclust, "auto")


x2 <- riojaPlot2(x, sc, yvar[, 2, drop=FALSE], myclust, scale.minmax=TRUE)
addClustZone2(x2, myclust, "auto")

myStyle <- makeStyle(
   secYvarName="Depth (cm)", 
   yvarName="Age (14C years BP)",
   yLabel = ylab,
   showSecAxis=TRUE, 
   showGroups=TRUE, 
   showCumul=TRUE,
   showExag=FALSE, 
   exagCol="grey90",
   outlineCol="grey50",
   cumulFontSize=0.7,
   nameAngle=45,
   nameFontSize=1,
   yAxisFontSize=0.8,
   showClust=FALSE, 
   yInterval=250, yMin=5500,
   showBars="full",
   barTop=FALSE, 
   nameStylenBreak=15,
   lwdBar=0.5, 
   showZones="auto",
   zoneCol="blue")

stop()

riojaPlot(spec, yvar, selVar=selTaxa, nameAngle=45)

svg("Test.svg", width=8, height=4.5)
riojaPlot(spec, yvar, selVar=selTaxa, groups=types, style=myStyle, showExag=TRUE)
dev.off()
# dev.off()

addZone2(x, 7500, 9000, border="red")

data(aber)

spec <- aber$spec
# replace species codes with full taxon names
colnames(spec) <- aber$names$Name 
yvar <- aber$ages
types <- aber$types
# convert pollen types to a factor
types$Group <- factor(types$Group, levels=c("Trees", "Shrubs", "Herbs"))

# remove taxa < 5% max abundance
mx <- apply(spec, 2, max)
selTaxa <- names(mx[mx > 5])

riojaPlot(spec, yvar, selTaxa, types,
   yvarName="Depth (cm)", 
   secYvarName="Age (14C years BP)",
   showSecAxis=TRUE, 
   secyLabel = expression(""^{14}*C~Years~BP), 
   showGroups=T, 
   showCumul=TRUE,
   showExag=F, 
   exagCol="grey90",
   outlineCol="grey50",
   cumulFontSize=0.7,
   tcl=-.3)


fun <- function(...) {
      fcall <- match.call(expand.dots=TRUE)
      print(fcall)
      print(eval(eval(fcall$xxx)))
      d <- list(...)
      print(d)
      
}

xx <- 10

fun(xxx=xx)
