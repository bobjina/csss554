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
root <- paste0(root, "hw/hw4/")

## Load
df <- fread('http://faculty.washington.edu/jonno/SISMIDmaterial/penn-data.txt')
load("USA_adm2.RData")
nb <- read.gal("penn_nb.gal") # File is on canvas site

###################
### Exercises
###################

##################
# (1a) Perform cluster detection using Besag/Newell with k=500
population <- df$pop
cases <- df$cases
E <- population * (sum(cases)/sum(population))
SMR <- cases/E
n <- length(cases)

## Get centroids
getLabelPoint <- function(county) {
  Polygon(county[c("long", "lat")])@labpt
}
df.penn <- map_data("county", "penn") 
cent <- by(df.penn, df.penn$subregion, getLabelPoint) # Returns list
cent <- do.call("rbind.data.frame", cent) 
names(cent) <- c("long", "lat") # Appropriate Header
centroids <- matrix(0, nrow = n, ncol = 2)
for (i in 1:n) centroids[i, ] <- c(cent$lat[i], cent$long[i])
colnames(centroids) <- c("x", "y")
rownames(centroids) <- 1:n

## Run Besag/Newell
k <- 500
alpha.level <- 0.01
geo <- centroids
BNresults <- besag_newell(geo, population, cases, expected.cases = NULL,
                          k, alpha.level)
BNsig <- length(BNresults$p.values[BNresults$p.values <
                                     alpha.level])
cat("No of sig results = ", BNsig, "\n")
## No of sig results = 11
resmat <- matrix(NA, nrow = BNsig, ncol = 100)
reslen <- NULL
for (i in 1:length(BNresults$clusters)) {
  reslen[i] <- length(BNresults$clusters[[i]]$location.IDs.included)
  resmat[i, 1:reslen[i]] <- BNresults$clusters[[i]]$location.IDs.included
}

## Plot results
penn.map <- map("county", "pennsylvania",
              fill = TRUE, plot = FALSE)
names <- substr(penn.map$names, 1 + nchar("pennsylvania,"),
                nchar(penn.map$names))
penn <- map2SpatialPolygons(penn.map, IDs = names,
                          proj4string = CRS("+proj=longlat"))

par(mfrow = c(1,4), mar = c(0.1, 0.1, 0.1, 0.1))
for (i in 1:4) {
plot(penn)
plot(penn[resmat[i, c(1:reslen[i])]], col = "red", add = T)
}


# (1b) Perform cluster detection SatScan alpha = 0.05, max pop 20%
max.pop <- 0.2
n.simulations <- 999
alpha.level <- 0.05
Kpoisson <- kulldorff(geo, cases, population, expected.cases=NULL, max.pop, n.simulations, alpha.level, plot=T)


## Most likely cluster
Kcluster <- Kpoisson$most.likely.cluster$location.IDs.included
plot(penn, axes=TRUE)
plot(penn[Kcluster], add=TRUE, col="red")
title("Most Likely Cluster")

## Secondary clusters
K2cluster <- Kpoisson$secondary.clusters[[1]]$location.IDs.included
plot(penn, axes=TRUE)
plot(penn[K2cluster], add=TRUE, col="red")
title("Second Most Likely Cluster")

##################
# (2a) Fit Poisson-Gamma smoothing model

eb <- eBayes(cases, E)
alpha <- eb$alpha
beta <- exp(eb$beta)
mean(cases/E)
summary(eb$RR)

par(mar=c(1,1,1,1))
mapvariable(eb$RR, penn)

##################
# (2b) Plot against SMRs

eb.results <- cbind(eb=eb$RR,smr=SMR) %>% data.table
p <- ggplot(eb.results) + 
     geom_point(aes(y=eb, x=smr)) +
     geom_abline(intercept=0,slope=1) +
     xlab("SMR") + ylab("Fitted") +
     xlim(0, 1.7) + ylim(0, 1.7)
p.log <- ggplot(eb.results) +
  geom_point(aes(y=log(eb), x=log(smr))) +
  geom_abline(intercept=0,slope=1) +
  xlab("log(SMR)") + ylab("log(Fitted)") +
  xlim(-1, .7) + ylim(-1, .7)

##################
# (2c) Sample distributions using rgamma for i=1:4

apost <- alpha + cases
bpost <- (alpha + E*exp(beta))/exp(beta)

hist.gamma <- function(i) {
  hist(rgamma(100, shape=apost[i], rate=bpost[i]))
}

##################
# (2d) Sample distributions using rgamma for i=1:4

postden.gamma <- function(i) {
  EBpostdens(cases[i], E[i], alpha, beta,
             lower = 0, upper = 2, main = paste0("Area ", i)) 
}

##################
# (2e) Posterior medians vs means
ebmedian <- qgamma(0.5, apost, bpost)
mapvariable(ebmedian, penn)

ggplot(data.table(mean=eb$RR, median=ebmedian))+
  geom_point(aes(x=median, y=mean)) +
  geom_abline(intercept=0, slope=1) +
  xlim(0.5, 2) + ylim(0.5, 2)

##################
# (2f) Posterior medians vs means

ebsd <- (exp(beta)*(alpha + cases)^1/2)/(alpha+E*exp(beta))
mapvariable(ebsd, penn)

se <- sqrt(SMR/E)
mapvariable(se, penn)

##################
# (2g) Post Threshold

par(mfrow = c(1, 1), mar = c(1, 1, 1, 1))

thresh <- EBpostthresh(cases, E, alpha = alpha,
                        beta, rrthresh = 1.2)
mapvariable(thresh, penn)

##################
# (2h) Fit using INLA




