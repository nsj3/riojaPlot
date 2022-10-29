library(riojaPlot)
options(tidyverse.quiet = TRUE)
library(tidyverse)

data(aber)
poll <- aber$spec
colnames(poll) <- aber$names$Name 
chron <- aber$ages

riojaPlot(poll, chron,
          yvar.name="Age (years BP)",
          ymin=6290, ymax=14000, yinterval=500,
          sec.yvar.name="Depth (cm)",
          plot.sec.axis = TRUE,
          scale.percent=TRUE,
          plot.top.axis=TRUE)

mx <- apply(poll, 2, max)
mx5 <- mx[mx>5]
riojaPlot(poll, chron, selVars=names(mx5),
          yvar.name="Age (years BP)",
          ymin=6290, ymax=14250, yinterval=500,
          sec.yvar.name="Depth (cm)",
          plot.sec.axis = TRUE,
          scale.percent=TRUE,
          ytks1=seq(6000, 14500, by=500),
          srt.xlabel=45, cex.xlabel=0.8, xRight=0.96)
