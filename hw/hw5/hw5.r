###########################################################
### Author: Patrick Liu (pyliu@uw.edu)
### Date: 1/31/2017
### Class: CSS&SS554
### Homework 5
###########################################################

###################
### Setting up ####
###################

rm(list=ls())
pacman::p_load(SpatialEpi, sp, data.table, dplyr, maps, maptools, RColorBrewer, ggplot2, spdep, geoR, fields, mgcv, lattice, latticeExtra)

## Settings
root <- strsplit(getwd(), "csss554")[[1]][1] %>% paste0(., "csss554/")
root <- paste0(root, "hw/hw5/")

## Data
data(ca20)

#-----------------------------------------------------------

# (0) Plot against covariates
plot(ca20$data ~ ca20$covariate$altitude, ylab="calcium", xlab="Altitude (m)", col="blue")
plot(ca20$data ~ ca20$covariate$area, ylab="calcium", xlab="Area", col="blue")


# (a) Look at cloud and binned semi-variogram

variogram.cloud <- variog(ca20, option="cloud")
plot(variogram.cloud, ylab="Semi-variance", xlab="Distance", col="grey")

variogram.binned <- variog(ca20, option="bin", trend=~ca20$covariate$altitude + ca20$covariate$area)
plot(variogram.binned, ylab="Semi-variance", xlab="Distance", col="blue")

# (b) Examine Monte Carlo intervals of no spatial dependence

soil.env <- variog.mc.env(ca20, obj=variogram.binned)
plot(variogram.binned, env=soil.env, ylab="Semi-variance", xlab="Distance")

# (c) Fit variogram model to the data
ols <- variofit(variogram.binned, c(100,200), weights="equal")
wls <- variofit(variogram.binned, c(100, 200))

# (d) Maximum and restricted maximum likelihood fits
mlfit <- likfit(ca20, ini=c(100, 200), trend=~ca20$covariate$altitude + ca20$covariate$area)
remlfit <- likfit(ca20, ini=c(100, 200), trend=~ca20$covariate$altitude + ca20$covariate$area, lik.method="RML")

# Graph all together
plot(variogram.binned, max.dist=2000, xlab="Distance (m)", ylab="Semi-variance")
lines(ols, max.dist=2000, col="red", lty=1)
lines(wls, max.dist=2000, col="green", lty=2)
lines(mlfit, max.dist=2000, col="blue", lty=3)
lines(remlfit, max.dist=2000, col="black", lty=4)
legend("bottomright", legend=c("OLS", "WLS", "ML", "REML"), lty=c(1, 2, 3, 4), bty="n", col=c("red", "green", "blue", "black"))

# Plots
points(ca20, pt.divid="data.proportional", xlab="x-coordinate", ylab="y-coordinate", col=ca20$covariate$area)

# (f) Carry out kriging and examing the resultant surface

## Detrend
lmfit <- lm(ca20$data ~ ca20$covariate$altitude + ca20$covariate$area)
detrend <- as.geodata(cbind(ca20$coords, lmfit$residuals))
## Plot
points(detrend, pt.divid="rank.prop", xlab="x-coordinate", ylab="y-coordinate", col="green")
## Fit detrend and krig
mlfit2 <- likfit(detrend, ini=c(100, 200))
pred.grid <- expand.grid(seq(5000, 6000, l=50), seq(4800, 5800, l=50))
kc <- krige.conv(detrend, loc=pred.grid, krige=krige.control(obj.m=mlfit2))
## Plot results
image.plot(x=pred.grid[["Var1"]][1:50], y=unique(pred.grid[["Var2"]])[1:50], z=matrix(kc$predict, nrow=50, ncol=50), xlab="x-coordinate", ylab="y-coordinate")
symbols(detrend$coords[,1], detrend$coords[,2], circles=(detrend$data-min(detrend$data))/1,add=T,inches=0.04)
## Plot Std Predictions
image.plot(x=pred.grid[["Var1"]][1:50],y=unique(pred.grid[["Var2"]]),z=matrix(sqrt(kc$krige.var),nrow=50,ncol=50),col=cm.colors(100),xlab="x-coordinate",ylab="y-coordinate")
points(detrend$coords[,1],detrend$coords[,2],pch=16)

# (h) Model with GAM
ca20.df <- data.frame(x=ca20$coords[,1], y=ca20$coords[,2], data=ca20$data, altitude=ca20$covariate$altitude, area=ca20$covariate$area)
gam.mod <- gam(data ~ s(x, y, bs="tp") + altitude + area, data=ca20.df, method="REML")
vis.gam(gam.mod, theta=30, phi=30)
gam.frame <- data.frame(x=pred.grid[, 1], y=pred.grid[,2], altitude=mean(ca20$covariate$altitude))
gam.pred <- predict.gam(gam.mod, gam.frame, type="terms")


# Contour plot
print(contourplot(zinc.pred~pred.grid[,1]*pred.grid[,2],xlab="",  ylab="", main ="", colorkey=T, scales=list(draw=F),
                  pretty=T, region=T, par.settings=custom.theme(region = 
                                                                  brewer.pal(9, "Greys"),bg = "grey80")))
# add the observed points
trellis.focus("panel", 1, 1, highlight=FALSE)
lpoints(zinc.dat[,1], zinc.dat[,2], pch=19, col="red", cex=.4)

