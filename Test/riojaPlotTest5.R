data(aber)

spec <- aber$spec
# replace species codes with full taxon names
colnames(spec) <- aber$names$Name 
yvar <- aber$ages
types <- aber$names[, -1]
# convert pollen types to a factor
types$Group <- factor(types$Group, levels=c("Trees", "Shrubs", "Herbs"))

# remove taxa < 5% max abundance
mx <- apply(spec, 2, max)
selTaxa <- names(mx[mx > 5])

riojaPlot(spec, yvar, selTaxa, types,
   yvar.name="Depth (cm)", 
   yinterval=20,
   sec.yvar.name="Age (14C years BP)",
   plot.sec.axis=T, 
   sec.ylabel = "Age", 
   plot.groups=TRUE, 
   plot.cumul=TRUE,
   exag=TRUE,
   exag.mult=5,
   col.exag="grey95",
   col.poly.line="grey50",
   cex.cumul=0.7)
