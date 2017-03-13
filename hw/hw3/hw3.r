###########################################################
### Author: Patrick Liu (pyliu@uw.edu)
### Date: 2/7/2017
### Class: CSS&SS554
### Homework 3
###########################################################

rm(list=ls())
pacman::p_load(SpatialEpi, sp, data.table, dplyr, maps, maptools, RColorBrewer, ggplot2, spdep)

## Settings
root <- strsplit(getwd(), "csss554")[[1]][1] %>% paste0(., "csss554/")
root <- paste0(root, "proj/")

## Source
source("C:/Users/pyliu/Desktop/san/code/graph/woodson_mapping_suite.r")

## Data
df <- fread("C:/Users/pyliu/Desktop/UGA_MACRO_DHS_2011_2011_4780.csv")

## Maps
  ## DHS
  map.dhs <- readShapeSpatial("J:/DATA/MACRO_DHS/UGA/2011/UGA_DHS6_2011_GPS_Y2012M10D11.SHP")
  names(map.dhs) <- tolower(names(map.dhs))
  map.dhs@data <- map.dhs@data %>% data.table
  ## Admin1
  map.admin1 <- readRDS("C:/Users/pyliu/Downloads/UGA_adm1.rds")

## Plot
plot(map.admin1)
plot(map.dhs, add=TRUE)
title(main="Map of PSUs from Uganda 2011 DHS")