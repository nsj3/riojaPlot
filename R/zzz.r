.onAttach <- function(lib, pkg)  {
   line1 <- paste("This is riojaPlot ", utils::packageDescription("riojaPlot", fields="Version"))
   line2 <- "riojaPlot replaces strat.plot for plotting stratigraphic diagrams\nSee ?riojaPlot and browseVignettes(\"riojaPlot\")."
   packageStartupMessage(paste(line1, line2, sep="\n"), appendLF = TRUE)
}

