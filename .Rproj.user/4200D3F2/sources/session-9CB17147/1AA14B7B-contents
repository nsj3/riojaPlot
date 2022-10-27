.onAttach <- function(lib, pkg)  {
   line1 <- paste("This is rioja ", utils::packageDescription("rioja", fields="Version"))
   line2 <- "New function riojaPlot replaces strat.plot for plotting stratigraphic diagrams\nSee ?riojaPlot and vignette(\"riojaPlot\") or vignette(\"riojaPlotPDF\") for details."
   packageStartupMessage(paste(line1, line2, sep="\n"), appendLF = TRUE)
}

.onUnload <- function(libpath) {
    library.dynam.unload("rioja", libpath)
}
