###########################################################
### Author: Patrick Liu (pyliu@uw.edu)
### Date: 3/7/2017
### Project: Thesis
### Purpose: Template
###				
###########################################################

###################
### Setting up ####
###################
rm(list=ls())
pacman::p_load(data.table, dplyr, maptools, mgcv, boot, 
               ggplot2, rgdal, survey, woodson, RColorBrewer, INLA, spdep, 
               car, binom)

os <- .Platform$OS.type
if (os == "windows") {
  j <- "J:/"
  h <- "H:/"
} else {
  j <- "/home/j/"
  user <- Sys.info()[["user"]]
  h <- paste0("/snfs2/HOME/", user)
}

## Path locals
root <- c("C:/Users/pyliu/Desktop/data/dhs")

##--FUNCTIONS--------------------------------------------------------

## Fit INLA
fit.inla <- function(df, me)  {
  ## CREATE FRAME FOR INLA
  df <- df[var==me]
  ## MODEL
  graph.file <- "C:/Users/pyliu/Desktop/data/shp/uganb.graph"
  formula <- logit.p ~ 1 + 
    f(year_rw1, model="rw1", scale.model=TRUE, hyper=list(theta=list(prior="pc.prec", param=c(0.5, 0.01)))) +
    f(year_unstruct, model = "iid", param=c(0.5, 0.008)) +
    f(struct, model="besag", adjust.for.con.comp = TRUE, constr=TRUE, graph=graph.file) + 
    f(unstruct, model = "iid", param=c(0.5, 0.008))
  mod <- inla(formula, family = "gaussian", data = df, 
              control.predictor = list(compute = TRUE),
              scale=logit.prec) 
  return(mod)
}

###########################################################################################################################
# PREP
###########################################################################################################################

load("C:/Users/pyliu/Desktop/data/prepped.rdata")

## Model
vars <- c("sba", "ifd", "anc4")
mods <- lapply(vars, function(x) fit.inla(pred.frame, x))
names(mods) <- vars
for (v in vars) {
  pred.frame[var==v, est_mean := mods[[v]]$summary.fitted.values$[["0.50quant"]] %>% inv.logit]
  pred.frame[var==v, est_lower := mods[[v]]$summary.fitted.values[["0.025quant"]] %>% inv.logit]
  pred.frame[var==v, est_upper := mods[[v]]$summary.fitted.values[["0.975quant"]] %>% inv.logit]
} 
pred.frame <- pred.frame[!ID_1 %in% c(31, 32)]


save(shp.adm1, pred.frame, file="C:/Users/pyliu/Desktop/data/modeled.rdata")