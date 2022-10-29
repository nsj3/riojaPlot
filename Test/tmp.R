### 15. How do I add my own zones to a diagram?

Arbitrary zones can be added using function `addRPZone`, as either single lines, or as shaded rectangles.  Simply save the riojaPlot object and pass it to `addRPZone`.

```{r FAQ15A, fig.width=fWidth, fig.height=fHeight, warning=FALSE, message=FALSE}
mx <- apply(aber.poll, 2, max)
aber.sel <- names(mx[mx > 2])

rp <- riojaPlot(aber.poll, aber.chron, aber.sel,
   yvar.name="Age (years BP)",
   ymin=6000,
   ymax=14300,
   yinterval=500,
   scale.percent=TRUE,
   exag=TRUE)

# define zone lines at 11000 and 13000 years BP
myzones <- c(11000, 13000)
addRPZone(rp, myzones, col="red")

# add shaded zone from 6000 to 8000 years BP
addRPZone(rp, 6000, 8000)

# change colour and shading
addRPZone(rp, 13000, 13500, col="blue", alpha = 0.05)

# The zones above are plotted over the existing diagram.
# It is possible to plot zones or other annotatiosn behind 
# the diagram using a cuctom function supplied to style fun.plotback
# this takes 2 arguments, usr = vector of 4 numbers giving the 
# coordinates of the plotting area in data units (x is 0-1)
# and fig,coordinates of the plotting area as fractions of the page size

fun.back <- function(usr, fig) {
  print(usr)
  rect(0, 8000, 1, 11000, col="lightblue1", border=NA)
}

rp <- riojaPlot(aber.poll, aber.chron, aber.sel,
   yvar.name="Age (years BP)",
   ymin=6000,
   ymax=14300,
   yinterval=500,
   scale.percent=TRUE,
   exag=TRUE,
   fun.plotback=fun.back
   )
```

### 16. How do I include special symbols in the variable names

We can pass a character vector of custom names for each column using style `x.names` and use `expression` to create names with superscripts, subscripts or greek letters.  

```{r FAQ16A, fig.width=fWidth, fig.height=fHeight, warning=FALSE, message=FALSE, eval=FALSE}
nms <- colnames(maule.data)
#nms[8] <- expression(delta^15*N)

riojaPlot(maule.data, maule.chron, groups=groups,
          yvar.name="age_BP", 
          x.names=nms,
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
          graph.widths=widths,
          yBottom=0.06
          )
```

We can also use function `label_geochem` in package  `tidypaleo` to automatically add annotations for a slected range of common geochemical variables.

```{r FAQ16B, fig.width=fWidth, fig.height=fHeight, warning=FALSE, message=FALSE}
library(tidypaleo)
nms <- colnames(maule.data)
#nms2 <- as.expression(sapply(nms, function(x) unlist(label_geochem(x)[[1]])))
nms2 <- nms
riojaPlot(maule.data, maule.chron, groups=groups,
          yvar.name="age_BP", 
          x.names=nms2,
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
          graph.widths=widths,
          yBottom=0.06
          )
```
