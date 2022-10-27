library(readxl)
library(rioja)

d <- read_excel("../../../riojatest/clado.xlsx", sheet="clado count")

cols <- sapply(d, mode)

any(cols == "character")

depth <- d[, 1, drop=TRUE]
spec <- d[, -1]

strat.plot(spec, yvar=depth, y.rev=TRUE, scale.percent=TRUE, ylim=c(170, 250))

diss <- dist(sqrt(spec))

clust <- chclust(diss)

x <- strat.plot(spec, yvar=depth, y.rev=TRUE, scale.percent=TRUE, clust=clust)

addClustZone(x, clust, nZone=5)

bstick(clust)

d <- read_excel("../riojatest/pollen percentage data Bol Schuchye.xlsx", sheet="Sheet2")
spec <- d[, -c(1,2)]
chron <- d[, 1:2]
spec <- spec[, apply(spec, 2, max) > 5]

clust <- chclust(dist(sqrt(spec)))

x <- bstick(clust)
x2 <- x$dispersion <= x$bstick
showZones <- which(x2)[1]


bstick(clust)

splot(spec, chron[, 1], scale.percent=TRUE, y.rev=TRUE, clust=clust)

pt <- strat.plot(fos, yvar = chron$Depth, y.rev = TRUE, xLeft = .22, scale.percent = TRUE)


transpose_df <- function(df) {
# assume col1 are species names
  cnms<- colnames(df)
  nms <- df[, 1, drop=TRUE]
  char_cols <- sapply(df, mode)
  del_cols <- c(1, which (char_cols == "character"))
  t_df <- t(df[, -del_cols, drop=FALSE])
  colnames(t_df) <- nms
  t_df <- data.frame(as.numeric(cnms[-1]), t_df) %>% tibble::as_tibble(.)
  colnames(t_df)[1] <- cnms[1]
  return(t_df)
}


transpose_df(d)

library(ggpalaeo)
library(rioja)

secondary_scale

data(RLGH)
fos <- RLGH$spec
chron <- RLGH$depths
pt <- strat.plot(fos, yvar = chron$Depth, y.rev = TRUE, xLeft = .22, scale.percent = TRUE)
secondary_scale(pt, yvar = chron$Depth, yvar2 = chron$Age, n = 5, ylabel2 = "Years")

library(rioja)
library(tibble)
data(RLGH)
fos <- RLGH$spec
chron <- RLGH$depths
dates <- tribble(~up, ~down, ~age, ~error,
                  4,      5,   1970,   5,
                  15,     20,  1900,  20)
pt <- strat.plot(fos, yvar = chron$Depth, y.rev = TRUE, xLeft = 0.3, scale.percent = TRUE)
plot_chron_info(pt, yvar = chron$Depth, dates = dates)

