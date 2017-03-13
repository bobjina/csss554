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



#--FUNCTIONS-----------------------------------------------------------------------------------

## Core function to collapse dataframe
collapse.by <- function(df, var, by_vars) {
  ## Subset dataframe to where not missing variable or survey design variables
  df.c <- df[!is.na(get(var)) & !is.na(strata) & !is.na(psu) & !is.na(pweight)] %>% copy
  ## Setup design
  design <- setup.design(df, var)
  ## Setup by the by call as a formula
  by_formula <- as.formula(paste0("~", paste(by_vars, collapse = "+")))
  ## Calculate number of observations, number of clusters, strata
  meta <- df[, list(sample_size = length(which(!is.na(get(var)))), 
                    nclust = length(unique(psu)), 
                    nstrata= length(unique(strata)),
                    var = var
  ), by = by_vars]
  ## Calculate mean and standard error by by_var(s).  Design effect is dependent on the scaling of the sampling weights
  est <- svyby(~get(var), by_formula, svymean, design = design, deff = "replace", na.rm = TRUE, drop.empty.groups = TRUE, keep.names = FALSE, multicore=TRUE) %>% data.table
  old <- c("get(var)", "DEff.get(var)", "se")
  new <- c("mean", "design_effect", "standard_error")
  setnames(est, old, new)
  ## Merge
  out <- merge(meta, est, by=by_vars)
  return(out)                     
}

## Create survey design object
setup.design <- function(df, var) {
  ## Conservative adjustment recommended by Thomas Lumley for single-PSU strata.  Centers the data for the single-PSU stratum around the sample grand mean rather than the stratum mean
  options(survey.lonely.psu = 'adjust')
  ## conservative adjustment recommended by Thomas Lumley for single-PSU within subpopulations.  Need to find out more about what exactly this is doing.
  options(survey.adjust.domain.lonely = TRUE)
  ## Check for survey design vars
  check_list <- c("strata", "psu", "pweight")
  for (i in check_list) {
    ## Assign to *_formula the variable if it exists and nonmissing, else NULL
    assign(paste0(i, "_formula"),
           ifelse(i %in% names(df) & nrow(df[!is.na(i)]) > 0, paste("~", i), NULL) %>% as.formula
    ) 
  }
  ## Set svydesign
  return(svydesign(id = psu_formula, weight = pweight_formula, strat = strata_formula, data = df[!is.na(var)], nest = TRUE))
}

## Prep df
prep.df <- function(id) {
 cb <- fread("C:/Users/pyliu/Desktop/git/csss554/proj/code/extraction/codebook.csv") 
 df <- fread(cb[id]$data)
 shp <- readOGR(cb[id]$shp)
 if (is.na(proj4string(shp))) proj4string(shp) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
 o <- over(shp, shp.adm1)
 shp@data <- shp@data %>% data.table
 shp@data$ID_1 <- o$ID_1
 if ("DHSCLUST" %in% names(shp@data)) clust <- "DHSCLUST" else clust <- "CLUSTER"
 shp@data <- shp@data[, (clust) := as.numeric(get(clust))]
 df <- merge(df, shp@data[, c(clust, "ID_1"), with=F], by.x="psu", by.y=clust, all.x=TRUE)
 return(df)
}



###########################################################################################################################
# PREP
###########################################################################################################################

## Load data
shp.adm1 <- readRDS("C:/Users/pyliu/Desktop/data/shp/UGA_adm1.rds")
#shp.adm1@data$ID_1 <- shp.adm1@data$OBJECTID
df <- lapply(c(1, 2, 3), prep.df) %>% rbindlist(., use.names=TRUE, fill=TRUE)
df <- df[!is.na(pweight)]

## Define indicator
df <- df[!is.na(birth_attendant_mapped), sba := ifelse(grepl("doctor|nurse|midwife", birth_attendant_mapped), 1, 0)]
df <- df[!is.na(delivery_location_mapped), ifd := ifelse(grepl("hospital|private", delivery_location_mapped), 1, 0) ]
df <- df[!is.na(anc_times), anc1 := ifelse(anc_times >= 1, 1, 0)]
df <- df[!is.na(anc_times), anc4 := ifelse(anc_times >= 4, 1, 0)]
df <- df[!is.na(anc_times), anc_drop := ifelse(anc_times >= 1 & anc_times < 4, 1, 0)]

## Tabulate estimates
vars.meta <- c("nid", "ihme_loc_id", "year_start", "year_end", "survey_module", "file_path")
vars.geo <- c("ID_1")
vars <- c("sba", "ifd", "anc1", "anc4", "anc_drop")
cf <- lapply(vars, function(x) collapse.by(df, x, by_vars=c(vars.meta, vars.geo))) %>% rbindlist

## Wilson Interval
cf.w <- cf[mean %in% c(0,1) | standard_error == 0]
sample_size <- cf.w$sample_size
n <- ifelse(cf.w$mean==0, 0, sample_size)
ci <- binom.confint(n, sample_size, conf.level = 0.95, methods = "wilson")
se <- (ci$upper - ci$lower)/3.92
cf[mean %in% c(0,1)| standard_error == 0]$standard_error <- se

## Clean df
cf <- cf[mean == 1, mean := 0.99]
cf <- cf[mean == 0, mean := 0.01]
cf <- cf[, logit.p := logit(mean)]
cf <- cf[, logit.prec := standard_error^2/(mean^2 * (1-mean)^2)]
cf$struct <- cf$ID_1
cf$unstruct <- cf$ID_1
cf <- cf[order(ID_1)]

## Prediction frame
locs <- shp.adm1@data$ID_1
vars <- c("anc1", "anc4", "anc_dropout", "sba", "ifd")
pred.frame <- expand.grid(ID_1=locs, year_id=c(2000, 2005, 2010, 2015), var=vars)
pred.frame <- pred.frame %>% as.data.table
pred.frame$ID_1 <- pred.frame$ID_1 %>% as.numeric
pred.frame$year_start <- pred.frame$year_start%>% as.numeric
cf$ID_1 <- as.numeric(cf$ID_1)

## Bin DHS years into 2000, 2005, 2010
cf <- cf[year_start==2000, year_id := 2000]
cf <- cf[year_start==2006, year_id := 2005]
cf <- cf[year_start==2011, year_id := 2010]

## Merge on data
pred.frame <- merge(pred.frame, cf, by=c("ID_1", "year_id", "var"), all.x=TRUE)
pred.frame$struct <- pred.frame$ID_1
pred.frame$unstruct <- pred.frame$ID_1
pred.frame$year_rw1 <- pred.frame$year_id
pred.frame$year_unstruct <- pred.frame$year_id

save(shp.adm1, cf, df, pred.frame,  file="C:/Users/pyliu/Desktop/data/prepped.rdata")








