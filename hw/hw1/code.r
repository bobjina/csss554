###########################################################
### Author: Patrick Liu (pyliu@uw.edu)
### Date: 1/22/2017
### Class: CSS&SS554
### Homework 1
###########################################################

###################
### Setting up ####
###################

rm(list=ls())
pacman::p_load(SpatialEpi, sp, data.table, dplyr)

## Settings
root <- strsplit(getwd(), "csss554")[[1]][1] %>% paste0(., "csss554/")
root <- paste0(root, "hw/week1/")

## Load
data(scotland)
df <- scotland$data
map <- scotland$spatial.polygon

###################
### Exercises
###################

##################
# (a) Provide maps of the SMRs, Yi/Ei, and of the proportion in AFF, xi.

## Calculate SMR, x, etc.
SMR <- df$cases/df$expected
x <- df$AFF
Y <- df$cases
E <- df$expected

## Map
pdf(paste0(root, "map_smr.pdf"))
map.smr <- mapvariable(SMR, map, main="Scotland (SMR of Lip Cancer)", xlab="Eastings (km)", ylab="Northings (km)")
dev.off()

pdf(paste0(root, "map_aff.pdf"))
map.x <- mapvariable(x, map, main="Scotland (AFF Indicator)", xlab="Eastings (km)", ylab="Northings (km)")
dev.off()

##################
# (c) Fit model (1) and report the estimates and standard errors for each of alpha and beta.
# Give a 95% confidence interval for exp(beta). Also report the estimate of K.

## Fit model 1
mod.poisson <- glm(Y ~ offset(log(E)) + x, family="poisson")
summary(mod.poisson)

## Estimates and SE for alpha and beta
alpha <- coefficients(mod.poisson)[1]
beta <- coefficients(mod.poisson)[2]
alpha.se <- sqrt(vcov(mod.poisson)[1, 1])
beta.se <- sqrt(vcov(mod.poisson)[2,2])

## 95% CI for exp(beta)
alpha.ci <- exp(confint(mod.poisson)[1,])
beta.ci <- exp(confint(mod.poisson)[2,])

## Estimates of K by fitting quasi-poisson
mod.quasi <- glm(Y ~ offset(log(E)) + x, family = quasipoisson(link = "log"))
summary(mod.quasi)

##################
# (d) Now fit an alternative model that includes latitude and longitude in the log-linear
# model, in order to investigate "confounding by location".

lat <- scotland$geo$x
long <- scotland$geo$y

mod.poisson.geo <- glm(Y ~ offset(log(E)) + x + lat + long, family="poisson")
mod.quasi.geo <- glm(Y ~ offset(log(E)) + x + lat + long, quasipoisson(link = "log"))

summary(mod.qausi.geo)


