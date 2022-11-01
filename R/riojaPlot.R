utils::globalVariables(c("groupData", "cumulLine", "cumulLineCol", "cumulLineLwd", 
                         "groupColours", "groupCex", "groupNames", "lwd.axis", "col.axis", "cumulSpace"))

#riojaPlot2 <- function(rp, x, y, clust=NULL, xLeft=NULL, xRight=0.99, gap=0.01, verbose=TRUE, ...) {
#   if (is.null(xRight))
#     xRight <- 0.99
#   if (is.null(xLeft))
#     xLeft <- rp$xRight + gap
#  
#   ylim <- c(min(rp$ylim), max(rp$ylim))
#   
#   .riojaPlot2(x, yvar = y[, 1, drop=FALSE], y.rev=rp$y.rev, xRight=xRight, xLeft=xLeft,
#      y.axis=FALSE, add=TRUE, yBottom=rp$box[3], yTop=rp$box[4], mgp=NULL, # c(3, 0.6/3, 0.2), 
#      srt.xlabel=rp$style$srt.xlabel, cex.xlabel=rp$style$cex.xlabel, cex.axis=rp$style$cex.axis, 
#      clust=clust, ylim=ylim, tcl=rp$style$tcl,  
#      exag.alpha=rp$style$exag.alpha, col.axis=rp$style$col.axis, plot.top.axis=rp$style$plot.top.axis, ...)
#}

riojaPlot <- function(x, y, selVars=NULL, groups=NULL, style=NULL, clust=NULL, riojaPlot=NULL, verbose=TRUE, ...) {
    
   if (!is.null(riojaPlot)) {
     if (!is(riojaPlot, "riojaPlot")) {
        stop("riojaPlot should be a riojaPlot object")
     }
     if (!is.null(style)) {
        message("Argument to style ignored when argument riojaPlot is given.")
     }
     style <- NULL
   }
   if (!is(x, "data.frame"))
     stop("x should be a data frame or tibble")
   if (!is(y, "data.frame"))
     stop("y should be a data frame or tibble")
  
   plotdata <- list()
   plotdata$spec <- x
   plotdata$chron <- y
   plotdata$selVars <- selVars
   plotdata$groups <- groups
   plotdata$clust <- clust
   args <- list(...)
   argNames <- names(args)
   
   if (!is.null(style)) {
      if (!methods::is(style, "riojaPlot.style"))
        stop("style is not a riojaPlot style object.")
   } else {
      style <- makeStyles()
   }
   
   if (!is.null(riojaPlot)) {
      style$xLeft <- riojaPlot$style$xRight 
      style$xRight <- 0.99
      style$plot.yaxis <- FALSE
      style$start.new.plot <- FALSE
#      style$y.rev  <- riojaPlot$style$y.rev
      style$yBottom <- riojaPlot$box[3] 
      style$yTop <- riojaPlot$box[4] # , mgp=NULL, # c(3, 0.6/3, 0.2), 
      style$srt.xlabel <- riojaPlot$style$srt.xlabel
      style$cex.xlabel <- riojaPlot$style$cex.xlabel
      style$cex.axis <- riojaPlot$style$cex.axis
#      style$ylim <- riojaPlot$style$ylim  
      style$tcl <- riojaPlot$style$tcl  
      style$exag.alpha <- riojaPlot$style$exag.alpha
      style$col.axis <- riojaPlot$style$col.axis
      style$plot.top.axis <- riojaPlot$style$plot.top.axis
   }
   
   validStyles <- names(makeStyles())
   for (i in argNames) {
      if (!(i %in% validStyles)) 
         stop(paste("Style ", i, "is not a valid riojaPlot style"))
      style[i] <- args[i]
   }
   
   if ("scale.percent" %in% argNames & !("plot.poly" %in% argNames)) {
     style$plot.poly <- TRUE
   }
   if ("xGap" %in% argNames) {
     style$xLeft <- style$xLeft + as.numeric(args["xGap"])
   }
   if (length(style$x.pc.inc)>1 & length(style$x.pc.inc) != ncol(x))
      stop("x.pc.inc should be a logical vector of length equal to 1 or the number of columns of x")
   if (length(style$graph.widths)>1 & length(style$graph.widths) != ncol(x))
      stop("graph.widths should be a logical vector of length equal to 1 or the number of columns of x")
   if (length(style$plot.poly)>1 & length(style$plot.poly) != ncol(x))
      stop("plot.poly should be a logical vector of length equal to 1 or the number of columns of x")
   if (length(style$plot.line)>1 & length(style$plot.line) != ncol(x))
      stop("plot.line should be a logical vector of length equal to 1 or the number of columns of x")
   if (length(style$plot.bar)>1 & length(style$plot.bar) != ncol(x))
      stop("plot.bar should be a logical vector of length equal to 1 or the number of columns of x")
   if (length(style$plot.symb)>1 & length(style$plot.symb) != ncol(x))
      stop("plot.symb should be a logical vector of length equal to 1 or the number of columns of x")
   if (length(style$exag)>1 & length(style$exag) != ncol(x))
      stop("exag should be a logical vector of length equal to 1 or the number of columns of x")

   ncol <- ncol(x)
   if (length(style$plot.poly)==1)
       style$plot.poly <- rep(style$plot.poly, ncol)
   if (length(style$plot.bar)==1)
       style$plot.bar <- rep(style$plot.bar, ncol)
   if (length(style$plot.line)==1)
       style$plot.line <- rep(style$plot.line, ncol)
   if (length(style$plot.symb)==1)
       style$plot.symb <- rep(style$plot.symb, ncol)
   if (length(style$exag)==1)
       style$exag <- rep(style$exag, ncol)
   if (length(style$graph.widths)==1)
       style$graph.widths <- rep(style$graph.widths, ncol)
   if (length(style$x.pc.inc)==1)
       style$x.pc.inc <- rep(style$x.pc.inc, ncol)

   .riojaPlot1(plotdata, style, riojaPlot=riojaPlot, verbose=verbose)  
#   on.exit({
#     rm("groupData", "cumulLine", "cumulLineCol", "cumulLineLwd", 
#                         "groupColours", "groupCex", "groupNames", lwd.axis, envir=.GlobalEnv)
#   })
} 

listStyles <- function() {
  styles <- unlist(makeStyles())
  x <- data.frame(Style=names(styles), Value=styles)
  rownames(x) <- NULL
  x
}

makeStyles <- function(...) {
   style <- list()
   style$yvar.name <- ""
   style$sec.yvar.name <- ""
   style$ylabel <- ""
   style$sec.ylabel <- ""
   style$plot.sec.axis <- FALSE
   style$scale.percent <- FALSE
   style$scale.minmax <- FALSE
   style$y.rev <- TRUE
   style$ymin <- NA
   style$ymax <- NA
   style$yinterval <- NA
   style$sec.ymin <- NA
   style$sec.ymax <- NA
   style$sec.yinterval <- NA
   style$x.names <- list(NULL)
   style$wa.order <- "none"
   style$plot.bar <- TRUE
   style$plot.line <- TRUE
   style$plot.poly <- FALSE
   style$plot.symb <- FALSE
   style$do.clust <- FALSE
   style$plot.clust <- FALSE
   style$plot.zones <- 0
   style$lwd.bar <- 1
   style$lwd.line <- 1
   style$lwd.poly.line <- 0.6
   style$lwd.cumul.line <- 0.6
   style$col.bar <- "grey"
   style$bar.back <- FALSE
   style$col.symb <- "black"
   style$col.line <- "black"
   style$col.poly <- "darkgreen"
   style$col.poly.line <- NA
   style$col.cumul.line <- NA
   style$col.zones <- "red"
   style$symb.pch <- 19
   style$symb.cex <- 1
   style$cex.axis <- 0.6
   style$cex.yaxis <- 0.7
   style$cex.ylabel <- 0.9
   style$cex.xlabel <- 0.9
   style$srt.xlabel <- 90
   style$srt.xlabel <- 90
   style$tcl <- -0.2
   style$cex.cumul <- 0.7
   style$clust.data.trans <- "none"
   style$clust.use.selected <- FALSE
   style$clust.width <- 0.05
   style$graph.widths <- 1
   style$exag <- FALSE
   style$col.exag <- "auto"
   style$col.exag.line <- NA
   style$lwd.exag.line <- 0.6
   style$exag.mult <- 2
   style$exag.alpha <- 0.2
   style$names.break.long <- TRUE
   style$names.break.n <- 20
   style$names.italicise <- FALSE
   style$plot.groups <- FALSE
   style$plot.cumul <- FALSE
   style$cumul.mult <- 1.0
   style$col.group <- c("darkgreen", "darkkhaki", 
                        "darkorange", "darkred",
                        "deepskyblue", "sienna3", 
                        "darkgoldenrod3", "darkseagreen",
                        "yellow3", "darkgrey")  
   style$xRight <- 0.99
   style$xLeft <- NA
   style$yBottom <- 0.05
   style$yTop <- NA
   style$fun.xback <- NA
   style$fun.xfront <- NA
   style$fun.plotback <- NA
   style$fun.yaxis <- NA
   style$ylabPos <- NA
   style$xlabPos <- 0.1
   style$xSpace <- 0.01
   style$x.pc.omit0 <- TRUE
   style$lwd.axis <- 1
   style$col.axis <- "grey"
   style$min.width.pc <- 5
   style$las.axis <- 1
   style$las.yaxis <- 1
   style$ytks1 <- NA
   style$ytks2 <- NA
   style$omitMissing <- TRUE
   style$col.sep.bar <- "black"
   style$sep.bar <- FALSE
   style$plot.bottom.axis <- TRUE
   style$plot.top.axis <- FALSE
   style$cumulSpace <- NA
   style$plot.yaxis <- TRUE
   style$start.new.plot <- TRUE
   style$xGap <- 0.01
   style$x.pc.inc <- 10
   
#   style$orig.fig <- c(0, 1, 0, 1)
   args <- list(...)
   argNames <- names(args)
   validStyles <- names(style)

   for (i in argNames) {
      if (!(i %in% validStyles)) 
         stop(paste("Style ", i, "is not a valid riojaPlot style"))
      style[i] <- args[i]
   }
   class(style) <- "riojaPlot.style"
   style
}

.riojaPlot1 <- function(mydata, style, riojaPlot=NULL, verbose) 
{
   orig.fig <- par("fig")

   if (is.null(mydata$spec) | is.null(mydata$chron) )
      stop("You must specify a table of data to plot (x) and a dataframe with at least one variable for the y-axis scale (y) data");
  
   if (is.null(style$x.names[[1]])) {
      style$x.names <- colnames(mydata$spec)
      names(style$x.names) <- style$x.names
   } else {
      names(style$x.names) <- colnames(mydata$spec)
   }

   names(style$plot.poly) <- colnames(mydata$spec)    
   names(style$plot.bar) <- colnames(mydata$spec)    
   names(style$plot.line) <- colnames(mydata$spec)    
   names(style$plot.symb) <- colnames(mydata$spec)    
   names(style$exag) <- colnames(mydata$spec)
   names(style$x.pc.inc) <- colnames(mydata$spec)
   names(style$graph.widths) <- colnames(mydata$spec)

   if (!is.null(mydata$selVars) & length(mydata$selVars) > 2) {
      tmp <- !(mydata$selVars %in% colnames(mydata$spec))
      if (any(tmp) & verbose) {
         message("The following variables are listed in selVars but are not found in the data, did you spell them correctly?")
         message(paste(mydata$selVars[tmp], collapse="\n"))
      } 
      d <- mydata$spec[, mydata$selVars[!tmp]]
      tmp2 <- which(colnames(mydata$spec) %in% mydata$selVars[!tmp])
#      style$x.names <- style$x.names[tmp2]
      style$x.names <- style$x.names[mydata$selVars[!tmp]]
      style$exag <- style$exag[mydata$selVars[!tmp]]
      style$plot.poly <- style$plot.poly[mydata$selVars[!tmp]]
      style$plot.line <- style$plot.line[mydata$selVars[!tmp]]
      style$plot.bar <- style$plot.bar[mydata$selVars[!tmp]]
      style$plot.symb <- style$plot.symb[mydata$selVars[!tmp]]
      style$graph.widths <- style$graph.widths[mydata$selVars[!tmp]]
#      print(style$graph.widths)
#      style$min.width.pc <- style$min.width.pc[mydata$selVars[!tmp]]
      style$x.pc.inc <- style$x.pc.inc[mydata$selVars[!tmp]]
   } else {
      d <- mydata$spec
   }
   
   if (is.na(style$cumulSpace)) {
      if (style$plot.top.axis) {
         cumulSpace <- 0.1
         cumulSpace <<- 0.1
      } else {
         cumulSpace <- 0
         cumulSpace <<- 0
      }
   } else {
       cumulSpace <- style$cumulSpace
       cumulSpace <<- style$cumulSpace
   }
   
   if (is.na(style$xLeft))
     style$xLeft <- NULL
   if (is.na(style$yTop))
     style$yTop <- NULL
   
   col.group <- rep(style$col.group, length.out=10)
   
   yvarName <- style$yvar.name
   if (nchar(stringr::str_trim(yvarName)) > 1) {
     if (!(yvarName %in% colnames(mydata$chron))) {
        stop(paste(yvarName, " is not a column in the chronology data."))
     }
     yvar <- mydata$chron[, yvarName, drop=FALSE]
   } else {
     yvar <- mydata$chron[, 1, drop=FALSE]
     yvarName <- colnames(mydata$chron)[1]
   }
   if (is(style$fun.xback, "logical"))
     style$fun.xback <- NULL
   if (is(style$fun.xfront, "logical"))
     style$fun.xfront <- NULL
   if (is(style$fun.plotback, "logical"))
     style$fun.plotback <- NULL
   if (is(style$fun.yaxis, "logical"))
     style$fun.yaxis <- NULL
   if (is.na(style$ylabPos))
     style$ylabPos <- NULL

   clust <- mydata$clust
   if (!is.null(clust))
     style$do.clust <- FALSE
   if (style$do.clust) {
     if (style$clust.use.selected) {
        d2 <- d
     } else {
        d2 <- mydata$spec
     }
     if (style$clust.data.trans == "sqrt") {
        d2 <- sqrt(d2)
     } 
     if (style$clust.data.trans == "scale") {
        d2 <- scale(d2, TRUE, TRUE)
     }
     diss <- dist(d2)
     clust <- rioja::chclust(diss)
   } 
   if (is.null(clust) & !style$do.clust) {
     style$plot.clust <- FALSE; style$plot.zones <- 0;
   }

   if (style$exag.mult < 1) {
      style$exag.mult <- 1.0
      style$exag <- FALSE
   }
   if (style$names.break.long) {
     warn <- options()$warn
     options(warn=-1)    
     style$x.names <- sjmisc::word_wrap(style$x.names, style$names.break.n)
     options(warn=warn)    
   }
   if (style$names.italicise) {
#     dont_italicize = c("\\(.*?\\)", "spp", "sp\\.", "type", "-complex", "[Oo]ther", "var\\.")
#     style$x.names <- as.expression(
#            sapply(style$x.names, 
#                   function(x) unlist(tidypaleo::label_species(x, dont_italicize=dont_italicize)[[1]]))
#     )
     style$x.names <- as.expression(sapply(style$x.names, function(x) bquote(italic(.(x))) ))
   }
   
   style$groupColours <- rep(style$col.poly, ncol(d))
   groupID <- rep(1, ncol(d))

   if (is.null(mydata$groups)) {
     style$plot.groups <- FALSE; 
     style$plot.cumul <- FALSE;
   }
   
   if ((style$plot.groups | style$plot.cumul)) {
      if (is.null(dim(mydata$groups)) | dim(mydata$groups)[2] < 2) {
         stop("Grouping object must have at least 2 columns.")
      }
      colnames(mydata$groups)[1:2] <- c("Names", "Group")
      if (!is.factor(mydata$groups$Group)) {
          mydata$groups$Group <- factor(mydata$groups$Group)
      }
      gr_names_d <- data.frame(Names=colnames(d))
      gr_names_d <- dplyr::left_join(gr_names_d, mydata$groups, by="Names")
      gr_names_d2 <- data.frame(Names=colnames(mydata$spec))
      gr_names_d2 <- dplyr::left_join(gr_names_d2, mydata$groups, by="Names")
      gr_names_d2$Group <- droplevels(gr_names_d2$Group)
      lev <- levels(gr_names_d2$Group)
      gr_names_d$Group <- factor(gr_names_d$Group, levels=lev)

      if (sum(is.na(gr_names_d$Group)) == nrow(gr_names_d)) {
         stop("None of the variables names found in the grouping variable.")
      }
      if (any(is.na(gr_names_d2$Group)) & verbose) {
         message("The following variable names are not found in the grouping variable:")
         message(paste(colnames(mydata$spec)[is.na(gr_names_d2$Group)], collapse="\n"))
         gr_names_d$Group <- forcats::fct_explicit_na(gr_names_d$Group, na_level = "Unkn")
         gr_names_d2$Group <- forcats::fct_explicit_na(gr_names_d2$Group, na_level = "Unkn")
      }
      if (length(levels(gr_names_d2$Group)) > 10)
         stop("Too many groups specified, maximum allowed is 10.")
      groupID <- as.integer(gr_names_d[, 2, drop=TRUE])
      groupID2 <- as.integer(gr_names_d2[, 2, drop=TRUE])
      groupColours <<- col.group
      groupColours <- col.group
      groupNames <<- levels(gr_names_d2$Group)
      groupNames <- levels(gr_names_d2$Group)
      if (style$plot.groups)
         style$groupColours <- groupColours[groupID]
   }

   if (!is.null(dim(yvar)))
     yvar <- yvar[, 1, drop=FALSE]
#   style$yLabels <- NULL
   
   if (is.na(style$ytks1[1]))
      style$ytks1 <- NULL
   if (is.na(style$ytks2[1]))
      style$ytks2 <- NULL
   ylim <- NULL
   ylim2 <- NULL
   yLab <- yvarName
   if (nchar(style$ylabel[1])>0) {
      yLab <- style$ylabel
   }
   
   if (!is.null(riojaPlot)) {
     ylim <- riojaPlot$ylim
     style$y.rev <- FALSE
   } else {
     
   if (is.character(yvar[, 1, drop=TRUE])) {
      style$yLabels <- yvar[, 1, drop=TRUE]
      yvar <- data.frame(SampleNo=1:length(yvar[, 1, drop=TRUE]))
      style$ytks1 <- yvar[, 1, drop=TRUE]
      expan <- abs(diff(range(yvar)))
      ylim <- c(min(yvar)-expan*0.01, max(yvar)+expan*0.01)
   } else {
      ylim <- range(yvar[, 1], na.rm=TRUE)
      if (!is.na(style$ymin) & is.na(style$ymax)) {
         ylim[1] <- style$ymin
#         ylim[2] <- max(yvar[, 1], na.rm=TRUE)
      } else if (!is.na(style$ymax) & is.na(style$ymin)) {
         ylim[2] <- style$ymax
#         ylim[1] <- min(yvar[, 1], na.rm=TRUE)
      } else if (!is.na(style$ymax) & !is.na(style$ymin)) {
         ylim[1] <- style$ymin
         ylim[2] <- style$ymax
         if (ylim[1] >= ylim[2])
           stop("ymin > ymax")
      }

      if (is.null(style$ytks1)) {
         if (!is.null(ylim) & !is.na(style$yinterval)) {
            style$ytks1 <- seq(ylim[1], ylim[2], by=style$yinterval)
         }
      }
   }
   secYvarName <- style$sec.yvar.name
   doSecYvar <- FALSE
   secYvar <- NULL
   if (style$plot.sec.axis & nchar(stringr::str_trim(secYvarName)) > 0) {
      if (yvarName != secYvarName) {
         if (!(secYvarName %in% colnames(mydata$chron))) {
           stop(paste(secYvarName, " is not a column in the chronology data."))
         }
         secYvar <- mydata$chron[, secYvarName, drop=FALSE]
         if (!is.numeric(secYvar[, 1, drop=TRUE]) & verbose) {
            message("Secondary Y axis variable must be numeric, not character.")
         } else {
            doSecYvar <- TRUE
            if (nchar(style$sec.ylabel[1])>0) {
               secYvarName <- style$sec.ylabel
            }
            yvar <- as.data.frame(cbind(yvar, secYvar))
            yLab <- c(yLab, secYvarName)
         }
      }
   }
   style$ytks <- style$ytks1
   if (doSecYvar) {
      ylim2 <- range(yvar[, 2], na.rm=TRUE)
      if (!is.na(style$sec.ymin) & is.na(style$sec.ymax)) {
         ylim2[1] <- style$sec.ymin
#         ylim2[2] <- max(yvar[, 2], na.rm=TRUE)
      } else if (!is.na(style$sec.ymax) & is.na(style$sec.ymin)) {
         ylim2[2] <- style$sec.ymax
#         ylim2[1] <- min(yvar[, 2], na.rm=TRUE)
      } else if (!is.na(style$sec.ymax) & !is.na(style$sec.ymin)) {        
          ylim2[1] <- style$sec.ymin
          ylim2[2] <- style$sec.ymax
      
      }
      if (!is.null(ylim2)) {
         if (is.null(style$ytks2[1])) {
            if (is.na(style$sec.yinterval)) {
               style$ytks2 <- pretty(ylim2, n=10)
            } else {
               style$ytks2 <- seq(ylim2[1], ylim2[2], by=style$sec.yinterval)
            }
         }
         style$ytks <- list(style$`ytks1`, style$`ytks2`)
      } 
   }
   
   }
   

# Groups   
   funlist <- style$fun.xfront
   
   if (style$plot.cumul) {
#      groupData <<- t(apply(d, 1, 
#                            function(x) cumsum(tapply(unlist(x), 
#                            groupID, sum, na.rm=TRUE))))
#      tt <- table(groupID)
      groupData <<- t(apply(mydata$spec, 1, 
                            function(x) cumsum(tapply(unlist(x), 
                            groupID2, sum, na.rm=TRUE))))
      tt <- table(groupID2)
      if (length(tt) == 1) {
         groupData <- t(groupData)
         colnames(groupData) <- names(tt)
      }
      d <- data.frame(d, Cumulative=c(100, rep(0, nrow(d)-1)))
      style$x.names <- c(style$x.names, "Cumulative")
      nCol <- ncol(d)
      if (is.null(funlist)) 
         funlist <- lapply(1:(nCol), function(x) NULL)
      else if (length(funlist)==1)
         funlist <- lapply(1:(nCol-1), function(x) funlist)
      funlist[[nCol]] <- plotCumul
      style$groupColours <- c(style$groupColours, NA)
#      cumulLine <<- style$plot.cumul.line
      cumulLineCol <<- style$col.cumul.line
      cumulLineLwd <<- style$lwd.cumul.line
#      cumulLine <- style$plot.cumul.line
      cumulLineCol <- style$col.cumul.line
      cumulLineLwd <- style$lwd.cumul.line
      lwd.axis <<- style$lwd.axis
      lwd.axis <- style$lwd.axis
      col.axis <<- style$col.axis
      col.axis <- style$col.axis
   } # else {
#     funlist <- style$fun2
#   }
   
   fin <- par("fin")
   xSpace <- style$xSpace * 10 / fin[1]
   groupCex <<- style$cex.cumul
   groupCex <- style$cex.cumul
   
#   style$col.line <- style$col.poly.line
   style$col.line <- rep(style$col.line, length(style$groupColours))
   style$col.bar <- rep(style$col.bar, length(style$groupColours))
   style$col.symb <- rep(style$col.symb, length(style$groupColours))
   
   sub <- ifelse(style$plot.cumul, 1, 0)
   
   for (i in 1:(length(style$groupColours)-sub)) {
      if (style$plot.groups & !style$plot.poly[i] & style$plot.line[i]) {
         style$col.line[i] <- style$groupColours[i]
         style$col.symb[i] <- style$groupColours[i]
      } 
      pb <- !(class(style$plot.bar[i]) == "logical" & style$plot.bar[i]==FALSE)
      if (style$plot.groups & !style$plot.poly[i] & !style$plot.line[i] & pb) {
         style$col.bar[i] <- style$groupColours[i]
      } 
      if (style$plot.groups & style$plot.symb[i]) {
         style$col.symb[i] <- style$groupColours[i]
      }
   }
   oldfig <- par("fig")
   oldmar <- par("mar")
   oldusr <- par("usr")

   on.exit({ par(mar=oldmar); par(fig=oldfig); par(usr=oldusr) }) 
   
#   if (style$nameStyleBreakLong) {
#      yLab <- sjmisc::word_wrap(yLab, style$nameStylenBreak)
#   }
   mclust <- NULL
   if (style$plot.clust)
     mclust <- clust

   if (style$plot.cumul) {
     style$x.pc.inc <- c(style$x.pc.inc, 10)
     style$graph.widths <- c(style$graph.widths, 1)
     style$exag <- c(style$exag, FALSE)
     style$plot.poly <- c(style$plot.poly, FALSE)
     style$plot.bar <- c(style$plot.bar, FALSE)
     style$plot.line <- c(style$plot.line, FALSE)
     style$plot.symb <- c(style$plot.symb, FALSE)
     style$col.poly <- c(style$col.poly, "black")
     style$col.bar <- c(style$col.bar, "black")
     style$col.line <- c(style$col.line, "black")
     style$col.symb <- c(style$col.symb, "black")
   }

   x <- .riojaPlot2(d, yvar = yvar, y.rev=style$y.rev, scale.percent=style$scale.percent, 
                plot.bar=style$plot.bar, plot.line=style$plot.line, plot.poly=style$plot.poly, 
                plot.symb=style$plot.symb, yTop=style$yTop, 
                col.poly=style$groupColours, col.bar=style$col.bar, lwd.bar=style$lwd.bar, 
                col.symb=style$col.symb, col.poly.line=style$col.poly.line, col.line=style$col.line, 
                symb.cex=style$symb.cex, exag=style$exag, wa.order=style$wa.order, 
                bar.back=style$bar.back, symb.pch=style$symb.pch, 
                clust=mclust, cex.xlabel=style$cex.xlabel, srt.xlabel=style$srt.xlabel, 
                ylabel=yLab, cex.yaxis=style$cex.yaxis, cex.axis=style$cex.axis, 
                cex.ylabel=style$cex.ylabel, scale.minmax=style$scale.minmax, ylim=ylim, y.tks=style$ytks, 
                y.tks.labels=style$yLabels, col.bg=NULL, col.exag=style$col.exag, exag.mult=style$exag.mult, 
                x.names=style$x.names, fun2=funlist, xSpace=xSpace, tcl=style$tcl,
                clust.width=style$clust.width, xRight=style$xRight, cumul.mult=style$cumul.mult, 
                orig.fig=orig.fig, exag.alpha=style$exag.alpha, fun1=style$fun.xback,
                ylabPos=style$ylabPos, x.pc.omit0=style$x.pc.omit0, lwd.poly.line=style$lwd.poly.line,
                lwd.line=style$lwd.line, col.exag.line=style$col.exag.line,
                lwd.exag.line=style$lwd.exag.line, lwd.axis=style$lwd.axis, col.axis=style$col.axis, 
                min.width=style$min.width.pc, las=style$las.axis, yBottom=style$yBottom,
                omitMissing=style$omitMissing, col.sep.bar=style$col.sep.bar, sep.bar=style$sep.bar,
                plot.top.axis=style$plot.top.axis, plot.bottom.axis=style$plot.bottom.axis,
                xlabPos=style$xlabPos, las.yaxis=style$las.yaxis,
                y.axis=style$plot.yaxis, xLeft=style$xLeft, add=!style$start.new.plot, 
                fun.plotback=style$fun.plotback, fun.yaxis=style$fun.yaxis, 
                graph.widths=style$graph.widths, x.pc.inc=style$x.pc.inc)

   if (!is.null(clust)) {
     if (style$plot.zones == "auto") {
        bs <- rioja::bstick(clust, plot=FALSE)
        bs2 <- bs$dispersion > bs$bstick
        style$plot.zones <- max(which(bs2)) 
        if (style$plot.zones < 2 & verbose) {
          message("There are no significant zones in these data.")
        }
     } 
   }    
   if (!is.null(clust) & style$plot.zones > 1) {
      addRPClustZone(x, clust, style$plot.zones, col=style$col.zones, yaxs="i")
   }
   x$style <- style
   invisible(x)
}

.riojaPlot2 <- function(d, yvar = NULL, scale.percent = FALSE, graph.widths=1, minmax=NULL, 
                  scale.minmax=TRUE, xLeft=NULL, xRight=NULL, yBottom=NULL, yTop=NULL, 
                  title="", cex.title=1.8, y.axis=TRUE, x.axis=TRUE, min.width=5, 
                  ylim=NULL, y.rev=FALSE, y.tks=NULL, y.tks.labels=NULL, ylabel=NULL,
                  cex.ylabel=1, cex.yaxis=0.8, xSpace=0.01, x.pc.inc=10, x.pc.lab=TRUE, 
                  x.pc.omit0=TRUE, wa.order="none", plot.line=TRUE, col.line="black", 
                  lwd.line=1, col.symb="black", plot.bar=TRUE, lwd.bar=1, col.bar="grey",
                  sep.bar=FALSE, col.sep.bar="black", bar.back=FALSE, plot.poly=FALSE, col.poly="grey", 
                  col.poly.line=NA, lwd.poly.line=1, plot.symb=FALSE, symb.pch=19, symb.cex=1,
                  x.names=NULL, cex.xlabel=1.0, srt.xlabel=90, mgp=NULL, #c(3, cex.axis/3, 0.2),
                  ylabPos=NULL, cex.axis=0.8, clust=NULL, clust.width=0.1, orig.fig=c(0, 1, 0, 1), 
                  exag=FALSE, exag.mult=5, col.exag="grey90", exag.alpha=0.2, 
                  col.bg=NULL, fun1=NULL, fun2=NULL, add=FALSE,  
                  cumul.mult = 1.0, col.exag.line=NA, lwd.exag.line=0.6, lwd.axis=1, 
                  col.axis="black", omitMissing=TRUE, plot.top.axis=FALSE, plot.bottom.axis=TRUE, 
                  xlabPos=0.1, las.yaxis=1, fun.plotback=NULL, fun.yaxis=NULL, ...)
{

   d <- as.data.frame(d)
   fcall <- match.call(expand.dots=TRUE)
   if (!is.null(clust)) {
     if (!is(clust, "chclust"))
        stop("clust must be a chclust object")
   }
   if (!is.null(clust)) {
      if (is.null(xRight))
#         xRight <- 1.0
         xRight <- 0.99
      xRight = xRight - clust.width
   }
   doSecYvar <- FALSE

   if (is.null(yvar)) {
      yvar <- data.frame(SampleNo=1:nrow(d))
      if (is.null(ylim)) {
         ylim=c(1, nrow(d))
      }
   } else {
      if (is.null(dim(yvar))) {
         nm <- substitute(yvar)
         yvar <- data.frame(tmp=yvar)
         colnames(yvar) <- as.character(nm)
      }
      else {
         yvar <- as.data.frame(yvar)
         if (ncol(yvar)>1)
            doSecYvar <- TRUE
      }
   }

   yNames <- c("", "")
   if (!is.null(ylabel)) {
      if (length(ylabel)==1)
         yNames <- c(ylabel, "")
      else
         yNames <- ylabel[1:2]
   } else {
      yNames <- colnames(yvar)     
   }
  
   if (is.null(x.names))
      x.names=colnames(d)   
   if (is.null(ylim)) {
      ylim = range(yvar[, 1], na.rm=TRUE)
   } else {
      if (is.na(yvar[1, 1]))
         ylim[1] <- min(yvar[, 1], na.rm=TRUE)
      if (is.na(ylim[2]))
         ylim[2] <- max(yvar[, 1], na.rm=TRUE)
   }
   
   oldfig = par("fig")
   oldmai <- par("mai")
   if (is.null(orig.fig)) {
      orig.fig = par("fig")
   }
   if (exag.mult < 1.0)
      exag <- FALSE
   nsp <- ncol(d)
   nsam <- nrow(d)

   if (scale.percent & length(x.pc.inc) > 1) {
#      if (length(x.pc.inc) != nsp) 
#         stop("length of x.pc.inc should equal number of curves")
   } else {
      x.pc.inc <- rep(x.pc.inc[1], nsp)
   }
   if (!is.null(minmax)) {
     if (ncol(minmax) != 2) 
        stop("minmax should have 2 columns")
     if (nrow(minmax) != nsp) 
        stop("number of rows of minmax should equal number of curves")
   }
   par(mai = c(0, 0, 0, 0))
   if (length(graph.widths) == 1)
      graph.widths <- rep(1, nsp)
#   if (length(graph.widths) != nsp) 
#      stop("Length of graph.widths should equal number of curves")
   if (length(exag) == 1)
      exag <- rep(exag[1], nsp)
   if (length(exag) != nsp) 
      stop("Length of exag should equal number of curves")
   if (length(exag.mult) == 1)
      exag.mult <- rep(exag.mult[1], nsp)
   if (length(exag.mult) != nsp) 
      stop("Length of exag.mult should equal number of curves")
   if (length(col.exag) == 1)
      col.exag <- rep(col.exag[1], nsp)
   if (length(col.exag) != nsp) 
      stop("Length of col.exag should equal number of curves")
   if (!is.null(fun1)) {
      if (length(fun1) == 1)
         fun1 <- lapply(1:nsp, function(x) fun1)
      if (length(fun1) != nsp)
         stop("Length of fun1 should equal number of curves")
   }
   if (!is.null(fun2)) {
      if (length(fun2) == 1)
         fun2 <- lapply(1:nsp, function(x) fun2)
      if (length(fun2) != nsp)
         stop("Length of fun2 should equal number of curves")
   }
   if (length(x.axis) == 1)
      x.axis <- rep(x.axis[1], nsp)
   if (length(x.axis) != nsp)
      stop("Length of x.axis should equal number of curves")
   cc.line <- rep(col.line, length.out=nsp)
   if (sep.bar & length(col.sep.bar) > 1 & length(col.sep.bar) != nsam) 
      warning("length of col.sep.bar does not equal number of rows in data.")
   cc.sep.bar <- rep(col.sep.bar, length.out=nsam)
#      cc.sep.bar <- col.sep.bar
   cc.bar <- rep(col.bar, length.out=nsp)
   cc.poly <- rep(col.poly, length.out=nsp)
   cc.symb <- rep(col.symb, length.out=nsp)
   cc.poly.line <- rep(col.poly.line, length.out=nsp)
#  if(plot.poly)
#    plot.line <- FALSE
   make.col <- function(x, alpha) {
      apply(col2rgb(x)/255, 2, function(x) rgb(x[1], x[2], x[3], alpha))
   }
   if (col.exag[1] == "auto")
      col.exag <- make.col(cc.poly, exag.alpha)
   inc <- 0.002
   if (wa.order == "topleft" || wa.order == "bottomleft") {
      V1 <- 1:nrow(d)
      colsum <- base::colSums(d, na.rm=TRUE)
#    opt <- (t(d) %*% yvar)/colsum
     opt <- (t(d) %*% V1)/colsum
     if ((wa.order == "topleft" & !y.rev) | (wa.order == "bottomleft" & y.rev))
        opt.order <- rev(order(opt))
     else 
       opt.order <- order(opt)
     
     d <- d[, opt.order]
     if (!is.null(minmax)) 
        minmax <- minmax[opt.order, ]
     if (!is.null(x.names))
        x.names <- x.names[opt.order]
     graph.widths <- graph.widths[opt.order]
     exag <- exag[opt.order]
     exag.mult <- exag.mult[opt.order]
     if (!is.null(fun1))
        fun1 <- fun1[opt.order]
     if (!is.null(fun2))
        fun2 <- fun2[opt.order]
     x.axis <- x.axis[opt.order]
     cc.poly <- cc.poly[opt.order]
     cc.poly.line <- cc.poly.line[opt.order]
     cc.line <- cc.line[opt.order]
     cc.symb <- cc.symb[opt.order]
     cc.bar <- cc.bar[opt.order]
     x.pc.inc <- x.pc.inc[opt.order]
   }
   
   if (scale.percent) {
      colM <- apply(d, 2, max, na.rm=TRUE)
#      colM <- floor((colM + 4.9)/5) * 5
      colM[colM < min.width] <- min.width
#      if ("CUMULATIVE" %in% toupper(x.names))
#        graph.widths <- c(graph.widths, 1)
      colM <- colM * graph.widths
      colM.sum <- sum(colM, na.rm=TRUE)
   } else {
      colM.sum <- sum(graph.widths, na.rm=TRUE)
      colM <- graph.widths
   }
   if ("CUMULATIVE" %in% toupper(x.names)) {
      tmp <- which("CUMULATIVE" == toupper(x.names))
      colM.sum <- colM.sum - colM[tmp] + (colM[tmp] * cumul.mult)
      colM[tmp] <- colM[tmp] *  cumul.mult
   }

# determine fig margins  

   ylab2 <- NULL
   yAxis2Pos <- 0
   tcll <- -.3
   spc <- 0
   
   args <- list(...)
   if ("tcl" %in% names(args)) {
       tcll <- args[["tcl"]]
   }
   if ("las" %in% names(args)) {
       if(args[["las"]]==2) {
         spc <- 0.3
       }
   }

   maxlen <- max(sapply(x.names, function(x) strwidth(x, units="figure", cex=cex.xlabel))) 
   if (doSecYvar) {
      maxlen <- max(maxlen, strwidth(yNames[1], units="figure", cex=cex.xlabel))
      maxlen <- max(maxlen, strwidth(yNames[2], units="figure", cex=cex.xlabel))
   }
   fin <- par("fin")
   plotRatio <- fin[1] / fin[2]
   
   xLabSpace <- xlabPos
   if(plot.top.axis)
     xLabSpace <- xLabSpace + 0.15
   if (is.null(yTop)) {
      xlSpace <- xLabSpace / fin[2]
      yTop <- 1.0 - (maxlen * plotRatio * cos(pi/180 * (90-srt.xlabel))) - xlSpace - 0.01
      yTop <- min(yTop, 0.95)
      if (srt.xlabel > 0) {
           yTop <- yTop - strwidth("m", units='figure', cex=cex.xlabel)
      }
   }
   
   if (is.null(xLeft)) {
      if (!is.null(y.tks.labels))
        ylabs <- y.tks.labels
      else 
#        ylabs <- as.character(yvar[, 1])
        ylabs <- pretty(yvar[, 1], n=10)
      incX <- strheight("M", units="figure", cex=cex.ylabel) / plotRatio # distance to axis values
      mx1 <- max(sapply(ylabs, function(x) strwidth(x, units='figure', cex=cex.yaxis))) # width of axis labels
      xLeft <- incX + mx1 + 0.02 / plotRatio

# without label
      incX <- strwidth("0", units='figure', cex=1)
      if (doSecYvar)
         xLeft <- mx1 + incX * 4
      else 
         xLeft <- mx1 + incX * 3
      
# now label
#      if (nchar(stringr::str_trim(yNames[1])) > 0 & !doSecYvar) {
      if (nchar(yNames[1]) > 0 & !doSecYvar) {
         line2fig <- strheight(yNames[1], units='figure', cex=1) / plotRatio
         if (is.null(ylabPos)) {
#            ylabPos <- 1 + mx1 / line2fig
            ylabPos <-  (mx1 / line2fig) - tcll
         }
         xLeft <- xLeft + (line2fig + line2fig * cex.ylabel) 
      }
   }

   if (y.axis & doSecYvar) {
      if (is(y.tks, "list") & !is.null(y.tks[[2]])) {
         y.tks2 <- y.tks[[2]]
         xout <- y.tks2
      } else {
         xout <- pretty(yvar[, 2], 10)
      }
      options(warn=-1)
      if (as.integer(R.Version()$major) > 3)
         ylab2 <- stats::approx(yvar[, 2, drop=TRUE], yvar[, 1, drop=TRUE], xout=xout, na.rm=TRUE)
      else 
         ylab2 <- stats::approx(yvar[, 2, drop=TRUE], yvar[, 1, drop=TRUE], xout=xout)
      options(warn=0)
      mx1 <- max(sapply(as.character(ylab2$x), function(x) strwidth(x, units='figure', cex=cex.yaxis)))
      yAxis2Pos <- mx1 + incX * 3 
      incX <- strwidth("0", units='figure', cex=1)
      xLeft <- xLeft + yAxis2Pos
   }

   if (is.null(clust) & is.null(xRight)) {
        xRight <- 1.0
#        xRight <- 0.99
        xLen <- xRight - xLeft
        xInc <- xLen - ((nsp + 1) * xSpace)
        n <- length(colM)
        inc <- xInc * colM[n]/colM.sum
        wid <- strwidth(x.names[length(x.names)], units='figure', 
                               cex=cex.xlabel) * 0.9 * sin(pi/180 * (90-srt.xlabel))
        if (srt.xlabel > 0)
           wid <- wid + strwidth("m", units='figure', cex=cex.xlabel)
        if (wid > inc) {
          xRight <- 1 - (wid-inc)
        }
   } 
   if (is.null(yBottom)) {
      yBottom <- 0.05
   }

   xLen <- xRight - xLeft
   xInc <- xLen - ((nsp + 1) * xSpace)
   inc <- xInc/colM.sum
   if (inc < 0.0)
     stop("Too many variables, curves will be too small.")
   x1 <- xLeft
    #    par(fig = c(x1, x1+0.4, yStart, yTop))
   if (y.rev) {
     tmp <- ylim[1]
     ylim[1] <- ylim[2]
     ylim[2] <- tmp
   }
   usr1 <- c(0, 1, ylim)

   if (y.axis) {
     mgpX <- if (is.null(mgp)) { c(3, max(0.0, 0.3 + 0.1 - tcll), 0.3) } else { mgp }

     if (doSecYvar) {
       par(fig = rioja::figCnvt(orig.fig, c(yAxis2Pos, yAxis2Pos+0.2, yBottom, yTop)), new=add)
       plot(0, cex = 0.5, xlim = c(0, 1), axes = FALSE, type = "n", xaxs="i", yaxs = "i", ylim = ylim, tcl=tcll, ...)
       axis(side=2, las=las.yaxis, at=ylab2$y, labels = as.character(format(ylab2$x)), cex.axis=cex.yaxis, xpd=TRUE, 
            tcl=tcll, mgp=mgpX) # c(3, 0.6, 0))
       addName(yNames[2], xLabSpace, srt.xlabel, cex.xlabel, y.rev, offset=-2)     
       add <- TRUE
     }

     par(fig = rioja::figCnvt(orig.fig, c(x1, x1+0.2, yBottom, yTop)), new=add)
     plot(NA, cex = 0.5, xlim = c(0, 1), axes = FALSE, type = "n", xaxs="i", yaxs = "i", ylim = ylim, tcl=tcll, ...)
     if (mode(y.tks)=="list") {
       y.tks <- y.tks[[1]]
     }     
     if (is.null(y.tks))
       y.tks <- axTicks(2)
     if (is.null(y.tks.labels))
       y.tks.labels <- format(y.tks, trim=TRUE)
     else
       y.tks.labels <- y.tks.labels
     y.tks.labels = as.character(y.tks.labels)
     
     ax <- axis(side=2, las=las.yaxis, at=y.tks, labels=y.tks.labels, cex.axis=cex.yaxis, xpd=TRUE, 
                tcl=tcll, mgp=mgpX) # c(3, 0.6, 0))
     x1 <- x1 + xSpace
#     mtext(title, adj = 0, line = 5, cex = cex.title)
#     if (nchar(stringr::str_trim(yNames[1])) > 0) {
     
     if (nchar(yNames[1]) > 0) {
        if (!doSecYvar) {
           mtext(yNames[1], side=2, line=ylabPos, cex=cex.ylabel)
        } else {
           addName(yNames[1], xLabSpace, srt.xlabel, cex.xlabel, y.rev, offset=-2)     
        }
     }
   }

   figs <- vector("list", length=nsp)
   usrs <- vector("list", length=nsp)

   if(!is.null(fun.plotback)) {
      fbox=c(xLeft=xLeft, xRight=xRight, yBottom=yBottom, yTop=yTop)     
      myfig <- par("fig")
      par(fig=fbox)
      fun.plotback(usr1, fbox)
      par(fig=myfig)
   }
      
   for (i in 1:nsp) {
     ty <- ifelse(plot.line[i], "l", "n")

   # omit missing values  
     y_var <- yvar[, 1, drop=TRUE]
     names(y_var) <- rownames(yvar)
     x_var <- d[, i, drop=TRUE]
  
     cumulPlot <- FALSE
     if (toupper(x.names[i])=="CUMULATIVE") {
        cumulPlot <- TRUE
     }
     
     nsam2 <- nsam
     if (omitMissing) {
        miss <- is.na(y_var) | is.na(x_var)
        nsam2 <- sum(!miss)
        if (nsam2 < nsam) {
           y_var <- y_var[!miss]
           x_var <- x_var[!miss]
           cc.sep.bar <- cc.sep.bar[!miss]
        }
     }
     par(new = TRUE)
     par(lend = "butt")
     if (scale.percent) {
        inc2 <- inc * colM[i]
        par(fig = rioja::figCnvt(orig.fig, c(x1, x1 + inc2, yBottom, yTop)))
        xxlim <- c(0, ifelse(cumulPlot, colM[i]/cumul.mult, colM[i]/graph.widths[i]))
        plot(0, 0, cex = 0.5, xlim = xxlim, 
             axes = FALSE, xaxs = "i", type = "n", yaxs = "i", ylim = ylim, xlab="", ylab="", ...)
#        plot(0, 0, cex = 0.5, xlim = c(0, colM[i]), axes = FALSE, 
#           xaxs = "i", type = "n", yaxs = "i", ylim = ylim, xlab="", ylab="", ...)
        if (!is.null(col.bg))
           rect(par("usr")[1],ylim[1],par("usr")[2],ylim[2], col=col.bg, border=NA)
        if (!is.null(fun1[[i]])) {
           fun1[[i]](x=x_var, y=y_var, i=i, nm=x.names[i])
        }
        if (plot.poly[i] & exag[i] & !cumulPlot) {
           y <- c(y_var[1], y_var, y_var[nsam2])
           x2 <- c(0, x_var*exag.mult[i], 0)
           polygon(x2, y, col=col.exag[i], border=col.exag.line, lwd=lwd.exag.line, xpd=FALSE)
        }        
        if (bar.back  & !cumulPlot) {
           if (is.logical(plot.bar[i])) {
              if (plot.bar[i]) {
                if (sep.bar) {
                   segments(rep(0, nsam2), y_var, x_var, y_var, lwd = lwd.bar, col = cc.sep.bar)
                } else {
                   segments(rep(0, nsam2), y_var, x_var, y_var, lwd = lwd.bar, col = cc.bar[i])
                }
              }
           } else {
              if (plot.bar[i]=="full") {
                 abline(h=y_var, col=cc.bar, lwd=lwd.bar)
              }
           }
        }
        if (plot.poly[i]) {
           y <- c(y_var[1], y_var, y_var[nsam2])
           x <- c(0, x_var, 0)
           polygon(x, y, col = cc.poly[i], border = cc.poly.line[i], lwd=lwd.poly.line)
        }
        if (!bar.back & !cumulPlot) {
           if (is.logical(plot.bar[i])) {
              if (plot.bar[i]) {
                 if (sep.bar) {
                    segments(rep(0, nsam2), y_var, x_var, y_var, lwd = lwd.bar, col = cc.sep.bar)
                 } else {
                    segments(rep(0, nsam2), y_var, x_var, y_var, lwd = lwd.bar, col = cc.bar[i])
                 }
              }
           } else {
              if (plot.bar[i]=="full") {
                 abline(h=y_var, col=cc.bar, lwd=lwd.bar)
              }
           }
        }
        
       yus <- par("usr")
       yymin <- max(c(min(y_var, na.rm=TRUE), min(yus[3:4]))) 
       yymax <- min(c(max(y_var, na.rm=TRUE), max(yus[3:4]))) 

       lines(c(0, 0), c(yymin, yymax), lwd=lwd.axis, xpd=NA, col=col.axis, ...)
#       lines(c(0, 0), c(min(y_var, na.rm=TRUE), max(y_var, na.rm=TRUE)), lwd=lwd.axis, xpd=NA, col=col.axis, ...)
       if (ty == "l") 
          lines(x_var, y_var, col = cc.line[i], lwd = lwd.line)
       if (plot.symb[i] & !cumulPlot) {
          points(x_var, y_var, pch=symb.pch, cex=symb.cex, col=cc.symb[i], xpd=FALSE)
       }
       if (!is.null(fun2[[i]])) {
          fun2[[i]](x=x_var, y=y_var, i=i, nm=x.names[i])
       }
       if (!cumulPlot) {
          xlabb <- seq(0, colM[i], by = x.pc.inc[i])
       }
       else
          xlabb <- seq(0, colM[i]/cumul.mult, by = x.pc.inc[i])
       if (x.axis[i]) {
          if (x.pc.lab) {
             xlabbt <- as.character(xlabb)
             if (x.pc.omit0)
                xlabbt[1] <- ""
             mgpX <- if (is.null(mgp)) { c(3,max(0.0, spc-tcll), 0.3 ) } else { mgp }
             mgpX3 <- if (is.null(mgp)) { c(3, max(0, 0.2-tcll), 0.3 ) } else { mgp }
             if (plot.bottom.axis) 
                axis(side=1, at=xlabb, labels=xlabbt, mgp=mgpX, cex.axis=cex.axis, tcl=tcll, ...)
             if (plot.top.axis) {
                axis(side=3, at=xlabb, labels=xlabbt, mgp=mgpX3, cex.axis=cex.axis, tcl=tcll, ...)
             }
         } else {
             if (plot.bottom.axis) 
                axis(side=1, at=xlabb, labels=FALSE, mgp=mgpX, ...)
             if (plot.top.axis)
                axis(side=3, at=xlabb, labels=FALSE, mgp=mgpX3, ...)
         }
       }
       x1 <- x1 + inc2 + xSpace
     } else {
       inc2 <- inc * colM[i]
       par(fig = rioja::figCnvt(orig.fig, c(x1, min(1, x1 + inc2, na.rm=TRUE), yBottom, yTop)))
       if (!is.null(minmax)) {
          plot(x_var, y_var, cex = 0.5, axes = FALSE, xaxs = "i", 
               type = "n", yaxs = "i", ylim = ylim, xlim=c(minmax[i, 1], minmax[i,2]), tcl=tcll, ...)
       } else {
          plot(x_var, y_var, cex = 0.5, axes = FALSE, xaxs = "i", 
             type = "n", yaxs = "i", ylim = ylim, tcl=tcll, ...)
       }
       if (!is.null(col.bg))
          rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col=col.bg)
       tks <- axTicks(1)
       us <- par("usr")
       if (!is.null(fun1[[i]])) {
          fun1[[i]](x=x_var, y=y_var, i=i, nm=x.names[i])
       }
       if (plot.poly[i] & exag[i] & !cumulPlot) {
          y <- c(y_var[1], y_var, y_var[nsam2])
          x2 <- c(us[1], x_var*exag.mult[i], us[1])
          polygon(x2, y, col = col.exag[i], border=col.exag.line, lwd=lwd.exag.line)
       }
       if (bar.back & !cumulPlot) {
          if (is.logical(plot.bar[i])) {
            if (plot.bar[i]) {
              if (sep.bar) {
                 segments(rep(us[1], nsam2), y_var, x_var, y_var, lwd = lwd.bar, col = cc.sep.bar)
              } else {
                 segments(rep(us[1], nsam2), y_var, x_var, y_var, lwd = lwd.bar, col = cc.bar[i])                            }
            }
          } else {
             if (plot.bar[i]=="full") {
                abline(h=y_var, col=cc.bar, lwd=lwd.bar)
             }
          }
       }
       if (plot.poly[i]) {
          y <- c(y_var[1], y_var, y_var[nsam2])
          x <- c(us[1], x_var, us[1])
          if (exag[i]) {
             x2 <- c(us[1], x_var*exag.mult[i], us[1])
             polygon(x2, y, col = col.exag[i], border=col.exag.line, lwd=lwd.exag.line)
          }
          polygon(x, y, col = cc.poly[i], border = cc.poly.line[i], lwd=lwd.poly.line)
       }
       if (!bar.back & !cumulPlot) {
          if (is.logical(plot.bar[i])) {
             if (plot.bar[i]) {
               if (sep.bar) {
                  segments(rep(us[1], nsam2), y_var, x_var, y_var, lwd = lwd.bar, col = cc.sep.bar)
               } else {
                  segments(rep(us[1], nsam2), y_var, x_var, y_var, lwd = lwd.bar, col = cc.bar[i])                              }
             }
          } else {
             if (plot.bar[i]=="full") {
                abline(h=y_var, col=cc.bar, lwd=lwd.bar)
             }
          }
       }
       yus <- par("usr")
       yymin <- max(c(min(y_var, na.rm=TRUE), min(yus[3:4]))) 
       yymax <- min(c(max(y_var, na.rm=TRUE), max(yus[3:4]))) 

       lines(c(us[1], us[1]), c(yymin, yymax), lwd=lwd.axis, 
#       lines(c(us[1], us[1]), c(min(y_var, na.rm=TRUE), max(y_var, na.rm=TRUE)), lwd=lwd.axis, 
             col=col.axis, xpd=NA, ...)
       if (ty == "l") 
          lines(x_var, y_var, col = cc.line[i], lwd = lwd.line)
       if (plot.symb[i] & !cumulPlot) {
          points(x_var, y_var, pch=symb.pch, cex=symb.cex, col=cc.symb[i], xpd=FALSE)
       }
       if (!is.null(fun2[[i]])) {
          fun2[[i]](x=x_var, y=y_var, i=i, nm=x.names[i])
       }
       mgpX <- if (is.null(mgp)) { c(3, max(0.0, spc-tcll), 0.3) } else { mgp }
       mgpX3 <- if (is.null(mgp)) { c(3, max(0, 0.2-tcll), 0.3 ) } else { mgp }
       if (x.axis[i]) {
          if (scale.minmax) {
             nn <- length(axTicks(1))
             tk <- c(axTicks(1)[1], axTicks(1)[nn])
             if (plot.bottom.axis) 
                axis(side=1, at=tk, labels=as.character(tk), cex.axis=cex.axis, mgp=mgpX, tcl=tcll, ...)
             if (plot.top.axis)
                axis(side=3, at=tk, labels=as.character(tk), cex.axis=cex.axis, mgp=mgpX3, tcl=tcll, ...)
          } else {
             if (plot.bottom.axis) 
                axis(side=1, cex.axis=cex.axis, mgp=mgpX, ...)
             if (plot.top.axis)
                axis(side=3, cex.axis=cex.axis, mgp=mgpX3, ...)
          }
       }
       x1 <- x1 + inc2 + xSpace
     }
     usr2 <- par("usr")
     tks1 <- usr2[1]
     fin <- par("fin")
     rD <- abs((usr2[4] - usr2[3]))
     rF <- fin[2]
     r <- rD/rF*xLabSpace
     pos <- usr2[4] + r
     if (usr1[3]-usr1[4] > 0)
        pos <- usr2[4]-r
     
     if (!cumulPlot) {
        par("lheight" = 0.7)
        if (srt.xlabel < 90)
           text(tks1[1], pos, labels=x.names[i], adj=c(0, 0), srt=srt.xlabel, cex = cex.xlabel, xpd=NA)
        else
           text(tks1[1], pos, labels=x.names[i], adj=c(0, 1), srt=srt.xlabel, cex = cex.xlabel, xpd=NA)
        par("lheight" = 1)
     }
     usrs[[i]] <- usr2   
     figs[[i]] <- par("fig")
   }
   if (!is.null(clust)) {
      par(fig = rioja::figCnvt(orig.fig, c(x1, xRight+clust.width, yBottom, yTop)))
      par(mar=c(0,0,0,0))
      par(new = TRUE)
      if (y.rev)
         xl <- rev(ylim)
      else
         xl <- ylim
     plot(clust, xvar=yvar[, 1, drop=TRUE], horiz=TRUE, x.rev=y.rev, labels=rep("", length(yvar[, 1, drop=TRUE])), 
          hang=-1, mgp=mgpX, cex.axis=cex.axis, xlim=xl, yaxs="i", xpd=FALSE, tcl=tcll, ...)
     if (plot.top.axis) {
       axis(side=3, mgp=mgpX3, cex.axis=cex.axis, tcl=tcll)
     }
   }
   par(mai = oldmai)
   oldfig[oldfig < 0] <- 0
   par(fig = oldfig)
   xRight2 <- xRight + ifelse(is.null(clust), 0, clust.width)
   fbox <- c(xLeft=xLeft, xRight=xRight, yBottom=yBottom, yTop=yTop)
   names(fbox) <- c("xLeft", "xRight", "yBottom", "yTop")

   ll <- list(call=fcall, box=fbox, 
              usr = usr1, mgpX=mgpX, mgpX3=mgpX3, xRight=xRight2, orig.fig=orig.fig,
              yvar=yvar[, 1, drop=TRUE], ylim=ylim, y.rev=y.rev, figs=figs, usrs=usrs)
   class(ll) <- "riojaPlot"
   invisible(ll)
}

addName <- function(x, xLabSpace, srt.xlabel, cex.xlabel, y.rev, offset=0)
{
    usr2 <- par("usr")
    tks1 <- usr2[1]
    fig <- par("fin")
    rD <- abs((usr2[4] - usr2[3]))
    rF <- fig[2]
    r <- rD/rF * xLabSpace
    yPos <- max(usr2[3:4]) + r 
    if (y.rev)
      yPos <- min(usr2[3:4]) - r 
    rD <- abs((usr2[1] - usr2[2]))
    rf <- fig[1]
    r <- rD/rF * .4 # offset
    xPos <- tks1[1] - r

    if (srt.xlabel < 90)
      text(xPos, yPos, labels=x, adj = c(0, 0), srt=srt.xlabel, cex = cex.xlabel, xpd=NA)
    else
      text(xPos, yPos, labels=x, adj = c(0, 1), srt=srt.xlabel, cex = cex.xlabel, xpd=NA)
}

shiny_running = function () {
  # Look for runApp call somewhere in the call stack.
  # from https://stackoverflow.com/questions/32806974/detecting-whether-shiny-runs-the-r-code
  
  frames = sys.frames()
  calls = lapply(sys.calls(), `[[`, 1)
  call_name = function (call)
    if (is.function(call)) '<closure>' else deparse(call)
  call_names = vapply(calls, call_name, character(1))
  
  #  target_call = grep('^runApp$', call_names)
  target_call = grep('runApp$', call_names)
  
  if (length(target_call) == 0)
    return(FALSE)
  
  # Found a function called runApp, verify that it's Shiny's.
  target_frame = frames[[target_call]]
  namespace_frame = parent.env(target_frame)
  isNamespace(namespace_frame) && environmentName(namespace_frame) == 'shiny'
}

plotCumul <- function(x, y, i, nm) 
{
  nG <- ncol(groupData)
  groupN <- as.integer(colnames(groupData))
  N <- length(x)
  usr <- par("usr")
#  segments(usr[2], usr[3], usr[2], usr[4], col="grey")
  
  yymin <- max(min(y, na.rm=TRUE), min(usr[3:4]))
  yymax <- min(max(y, na.rm=TRUE), max(usr[3:4]))
  
  rect(usr[1], yymin, usr[2], yymax, border=col.axis, lwd=lwd.axis, xpd=NA)
#  rect(usr[1], min(y, na.rm=TRUE), usr[2], max(y, na.rm=TRUE), border=col.axis, lwd=lwd.axis, xpd=NA)
#  segments(usr[2], min(y, na.rm=TRUE), usr[2], max(y, na.rm=TRUE), col="grey")
  for (j in nG:1) {
    y2 <- c(y[1], y, y[N])
    x2 <- c(usr[1], groupData[, j, drop=TRUE], usr[1])
#    bord <- NA
#    if (exists("cumulLine") & exists("cumulLineCol") & cumulLine)
       bord <- cumulLineCol
    polygon(x2, y2, col=groupColours[groupN[j]], border = bord, lwd=cumulLineLwd, xpd=FALSE)
  } 
  fig <- par("fig")
  oldmar <- par("mar")
  par(mar=c(0, 0, 0, 0))
  oldusr <- par("usr")
  oldfig <- fig
  fig[3] <- fig[4]
  fig[4] <- 1
  par(fig=fig, new=TRUE)
  plot(0, xlim=c(0,1), ylim=c(0, 1), axes=FALSE, type="n", xlab="", ylab="", xaxs="i", yaxs="i")
  fin <- par("fin")
  scale <- 1.0 / fin[2] 
  lineHeight_in <- strheight("M", units="figure", cex=groupCex) * fin[2]
  inc <- min(0.25, lineHeight_in) * scale
  for (i in nG:1) {
     y <- (nG-i)*inc*1.3 + (0.1* scale) + cumulSpace
     rect(0.8, y, 1.0, y+inc, col=groupColours[groupN[i]], xpd=NA)
     if (!is.null(groupNames)) {
        text(0.75, y+inc/2, groupNames[i], adj=c(1, 0.5), cex=groupCex, xpd=NA)
     }
  }
  par(mar=oldmar)
  par(fig=oldfig)
  par(usr=oldusr)
}

addRPZone <- function(riojaPlot, upper, lower=NULL, xLeft=NULL, xRight=NULL, col="red", 
                      alpha=0.1, border=NA, verbose=TRUE, ...) {
  if (!is(riojaPlot, "riojaPlot")) {
     stop("riojaPlot should be a riojaPlot object")
  }
  oldpar <- par(c("fig", "mar", "usr"))
  if (!is.null(xLeft))
    riojaPlot$box[1] <- xLeft
  if (!is.null(xRight))
    riojaPlot$box[2] <- xRight
  make.col <- function(x, alpha) {
      apply(col2rgb(x)/255, 2, function(x) rgb(x[1], x[2], x[3], alpha))
  }
  fillcol <- make.col(col, alpha)
  par(fig=rioja::figCnvt(riojaPlot$orig.fig, riojaPlot$box))
  par(mar=c(0,0,0,0))
  par(usr=c(0, 1, riojaPlot$usr[3], riojaPlot$usr[4]))
  if (is.null(lower))
    segments(0, upper, 1, upper, xpd=NA, col=col, ...)
  else
    rect(0, lower, 1, upper, col=fillcol, border=border, ...)
  par(oldpar)
}

addRPClust <- function(riojaPlot, clust, xLeft=NULL, xRight=0.99, verbose=TRUE, ...) {
  if (!is(riojaPlot, "riojaPlot")) {
       stop("riojaPlot should be a riojaPlot object")
  }
  if (is.null(clust) | !is(clust, "chclust"))
    stop("clust show be a chclust object.")
  oldpar <- par(c("fig", "mar", "usr"))
  if (!is.null(xLeft))
    riojaPlot$box[2] <- xLeft
  par(fig = c(riojaPlot$box[2], xRight, riojaPlot$box[3], riojaPlot$box[4]))
  par(mar=c(0,0,0,0))
  par(new = TRUE)
  ylim <- riojaPlot$ylim
  if (riojaPlot$y.rev)
    xl <- rev(ylim)
  else
    xl <- ylim
  plot(clust, xvar=riojaPlot$yvar, horiz=TRUE, x.rev=riojaPlot$y.rev, labels=rep("", length(riojaPlot$yvar)), 
       hang=-1, mgp=riojaPlot$mgpX, cex.axis=riojaPlot$style$cex.axis, xlim=xl, yaxs="i", xpd=FALSE, tcl=riojaPlot$style$tcl, ...)
     if (riojaPlot$style$plot.top.axis) {
       axis(side=3, mgp=riojaPlot$mgpX3, cex.axis=riojaPlot$style$cex.axis, tcl=riojaPlot$style$tcl)
     }
  
   par(oldpar)
}

addRPClustZone <- function(riojaPlot, clust, nZone="auto", xLeft=NULL, xRight=NULL, verbose=TRUE, ...) {
  if (!is(riojaPlot, "riojaPlot")) {
     stop("riojaPlot should be a riojaPlot object")
  }
  if (nZone == "auto") {
    bs <- rioja::bstick(clust, plot=FALSE)
    bs2 <- bs$dispersion <= bs$bstick
    nZone <- which(bs2)[1]
    if (nZone < 2 & verbose) {
        message("There are no significant zones in these data.")
     }
  } 
  if (nZone > 1) {
    oldpar <- par(c("fig", "mar", "usr"))
    if (!is.null(xLeft))
      riojaPlot$box[1] <- xLeft
    if (!is.null(xRight))
      riojaPlot$box[2] <- xRight
    par(fig=riojaPlot$box)
    par(mar=c(0,0,0,0))
    par(usr=c(0, 1, riojaPlot$usr[3], riojaPlot$usr[4]))
    cc <- cutree(clust, k=nZone)
    zn <- which(diff(cc)>0)
    zone <- (riojaPlot$yvar[zn] + riojaPlot$yvar[zn+1]) / 2
    r <- range(c(riojaPlot$usr[3], riojaPlot$usr[4]))
    sel <- which (zone >= r[1] & zone <= r[2])
    if (length(sel) > 0) {
      zone <- zone[sel]
      segments(0, zone, 1, zone, xpd=NA, ...)
    }
    par(oldpar)
  }
}

addRPZoneNames <- function(riojaPlot, zones, showColumn=TRUE, xLeft=NULL, xRight=0.99, ...) {
   fun.zone <- function(x, y, i, nm) {
      usr <- par("usr")
      names <- names(y)
      text(0.5, y, labels=names, adj=c(0.5, 0.5), ...)
      if (showColumn) {
         segments(usr[1], usr[3], usr[1], usr[4], col=riojaPlot$style$col.axis, xpd=NA, ...)
         segments(usr[2], usr[3], usr[2], usr[4], col=riojaPlot$style$col.axis, xpd=NA, ...)
      }
   }
   x <- data.frame(x=rep(c(0, 1), length.out=nrow(zones)))
   y <- zones[, 1, drop=FALSE]
   rownames(y) <- zones[, 2, drop=TRUE]
   
   if (is.null(xLeft))
     xLeft <- riojaPlot(xRight)
   riojaPlot(x, y,
            riojaPlot=riojaPlot, 
            col.axis=NA,
            xRight=xRight,
            xLeft=xLeft,
            x.names="",
            plot.line=FALSE,
            plot.bar=FALSE,
            plot.top.axis=FALSE,
            plot.bottom.axis=FALSE,
            fun.xfront=fun.zone)
}
