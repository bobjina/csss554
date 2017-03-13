###########################################################
### Author: Patrick Liu (pyliu@uw.edu)
### Date: 1/31/2017
### Class: CSS&SS554
### Homework 2
###########################################################

###################
### Setting up ####
###################

rm(list=ls())
pacman::p_load(SpatialEpi, sp, data.table, dplyr, maps, maptools, RColorBrewer, ggplot2, spdep)

## Settings
root <- strsplit(getwd(), "csss554")[[1]][1] %>% paste0(., "csss554/")
root <- paste0(root, "hw/hw2/")

## Load
df <- fread('http://faculty.washington.edu/jonno/SISMIDmaterial/penn-data.txt')
load("USA_adm2.RData")
nb <- read.gal("penn_nb.gal") # File is on canvas site

###################
### Exercises
###################

##################
# (1) Describe precisely what thetai represents.

##################
# (2,3,4) Calculate Y, E, SMR, se(SMR) and provide maps of each
df <- df[, y := cases]
df <- df[, e := pop * (sum(cases)/sum(pop))]
df <- df[, smr := y/e]
df <- df[, smr_se := sqrt(smr/e)]


map <- gadm[which(gadm$NAME_1=="Pennsylvania"),]
map <- as(map, "SpatialPolygons") %>% latlong2grid

ncol <- 6
p <- seq(0,1,length=ncol+1)
shading <- brewer.pal(ncol,"BuPu")

makemap <- function(var, title) {
  br <- round(quantile(df[[var]],probs=p),2)
  data.grp <- findInterval(df[[var]],vec=br,rightmost.closed=T,all.inside=T)
  data.shad <- shading[data.grp]
  map("county", "penn", fill=TRUE, col=data.shad)
  title(main=title,cex=1.5)
}

makelegend <- function(var, title) {
  br <- round(quantile(df[[var]],probs=p),2)
  data.grp <- findInterval(df[[var]],vec=br,rightmost.closed=T,all.inside=T)
  data.shad <- shading[data.grp]
  plot.new()
  legend("bottom", legend=leglabs(br), fill=shading, bty="n")
}

vars <- list(
        c("y", "Pennsylvania Breast Cancer Observed Cases"),
        c("e", "Pennsylvania Breast Cancer Expected Cases"),
        c("smr", "Pennsylvania Breast Cancer SMRs"),
        c("smr_se", "Pennsylvania Breast Cancer s.e(SMRs)")
        )

pdf(paste0(root, "/maps.pdf"))
lapply(vars, function(x) makemap(x[1], x[2]))
lapply(vars, function(x) makelegend(x[1], x[2]))
dev.off()

##################
# (5) Plot SMR and se(SMR)

pdf(paste0(root, "/smr_scatter.pdf"))
ggplot(df) + 
  geom_point(aes(x=smr, y=smr_se)) +
  geom_point(data=df[smr_se > 0.25], aes(x=smr, y=smr_se), colour="red") +
  ggtitle("se(SMR) vs SMR of Breast Cancer Incidence in Pennsylvania")
dev.off()

##################
# (6) Fit a Poisson model

mod <- glm(y ~ 1 + offset(log(e)), family="poisson", data=df)
map$resid <- residuals(mod, type = "pearson")

pdf(paste0(root, "residuals.pdf"))
makemap("resid", "Residuals from Poisson Model")
makelegend("resid", "Residuals from Poisson Model")
dev.off()

##################
# (7) Examing clustering via Moran's I via diff weighting schemes

col.W <- nb2listw(nb, style="W", zero.policy=T)
col.B <- nb2listw(nb, style="B", zero.policy=T)
col.C <- nb2listw(nb, style="C", zero.policy=T)
col.U <- nb2listw(nb, style="U", zero.policy=T)
col.S <- nb2listw(nb, style="S", zero.policy=T)

moran.test(map$resid, col.W)
moran.test(map$resid, col.B)
moran.test(map$resid, col.C)
moran.test(map$resid, col.U)
moran.test(map$resid, col.S)

##################
# (8) Examing clustering via Geary C via diff weighting schemes

geary.test(map$resid, col.W)
geary.test(map$resid, col.B)
geary.test(map$resid, col.C)
geary.test(map$resid, col.U)
geary.test(map$resid, col.S)

##################
# (9) Are there influential clusters?

lisa <- moran.plot(map$resid, col.W, xlab = "Residuals", ylab = "Spatially Lagged Residuals")

lm1 <- localmoran(map$res, col.W)
gry <- c(rev(brewer.pal(8, "Reds")[1:6]), brewer.pal(6,"Blues"))
map$localM <- lm1[, 1]

zplot <- spplot(map, zcol = "localM", at = c(-2.5, -1.4, -0.6, -0.2, 0, 0.2, 0.6, 1.4, 2.5), col.regions = gry, main="Z-scores of local contributions")

pdf(paste0(root, "local.pdf")) 
lisa
zplot
dev.off()
