## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 5,
  fig.height = 6
)

## ----eval= FALSE--------------------------------------------------------------
#  # remotes::install_git(url = "https://github.com/neocarto/flowmapper")

## -----------------------------------------------------------------------------
library(flowmapper)

## ----out.width = 200----------------------------------------------------------
library(sf)

# Import
crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

subregions <- st_read(system.file("subregions.gpkg", package="flowmapper")) %>% st_transform(crs)
migr <- read.csv(system.file("migrantstocks2019.csv", package="flowmapper"))

#  Selection, filtrage, traitement en amont

threshold <- 1500
migr <- migr[migr$fij >= threshold,]

## -----------------------------------------------------------------------------
c <- flowmap(
 x = subregions,
 xid = "id",
 df = migr,
 dfid = c("i","j"),
 dfvar = "fij"
)

## -----------------------------------------------------------------------------
intra <- migr[migr$i == migr$j,c("i","fij")]
colnames(intra) <- c("id","nb")

plot(st_geometry(subregions), col = "#CCCCCC", border = "white", lwd = 0.5)
c <- flowmap(
 x = subregions,
 xid = "id",
 df = migr,
 dfid = c("i","j"),
 dfvar = "fij",
 size = "thickness",
 type = "rect",
 decreasing = FALSE,
 add = TRUE,
 lwd = 1,
 col = "#00FF0090",
 border = "#4a0c25",
 k = NULL,
 df2 = intra,
 df2id = "id",
 df2var = "nb"
)


## -----------------------------------------------------------------------------
crs <- "+proj=aeqd +lat_0=90 +lon_0=50 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "
subregions2 <- st_transform(subregions, crs)

plot(st_geometry(subregions2), col = "#CCCCCC", border = "white", lwd = 0.5)
c <- flowmap(
 x = subregions2,
 xid = "id",
 df = migr,
 dfid = c("i","j"),
 dfvar = "fij",
 add = TRUE
)

## -----------------------------------------------------------------------------
plot(st_geometry(subregions), col = "#CCCCCC", border = "white", lwd = 0.5)
flows <- flowmap(
  x = subregions,
  xid = "id",
  df = migr,
  dfid = c("i","j"),
  dfvar = "fij",
  size = "thickness",
  type = "rect",
  decreasing = FALSE,
  add = TRUE,
  lwd = 1,
  col = "#00FF0090",
  border = "#4a0c25",
  k = NULL,
  df2 = intra,
  df2id = "id",
  df2var = "nb"
)

flowlegend(x = flows, title = "inter", title2 = "intra")


## -----------------------------------------------------------------------------
# library(smoothr)
# crs <- "+proj=ortho +lat_0=42.5333333333 +lon_0=-72.53333333339999 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
# flows <- smoothr::densify(c[[3]], n = 30) %>% st_transform(crs)
# plot(st_geometry(subregions) %>% st_transform(crs), col ="#CCCCCC", border = "white")
# plot(st_geometry(flows), col ="#FF000099", add = TRUE)
# plot(st_centroid(st_geometry(c[[2]])) %>% st_transform(crs), add = TRUE, pch = 20, cex = 1.3, col ="black")


