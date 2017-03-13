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
               car, binom, grid, gridExtra)

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

#----------------------------------------------

g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}



load("C:/Users/pyliu/Desktop/data/modeled.rdata")

## Map
years <- c(2000, 2005, 2010, 2015)
p.sba <- lapply(years, function(x) {
     wmap(chloropleth_map = shp.adm1,
     geog_id = "ID_1",
     variable = "est_mean",
     data=pred.frame[var=="sba" & year_id == x],
     override_scale=c(0,1),
     color_ramp=rev(brewer.pal(7,"RdYlGn")),
     legend_position="right",
     legend_bar_width=1,
     return_map_object_only=TRUE)
})
p.ifd <- lapply(years, function(x) {
  wmap(chloropleth_map = shp.adm1,
       geog_id = "ID_1",
       variable = "est_mean",
       data=pred.frame[var=="ifd" & year_id == x],
       override_scale=c(0,1),
       color_ramp=rev(brewer.pal(7,"RdYlGn")),
       legend_position="bottom",
       return_map_object_only=TRUE)
})
p.anc4 <- lapply(years, function(x) {
  wmap(chloropleth_map = shp.adm1,
       geog_id = "ID_1",
       variable = "est_mean",
       data=pred.frame[var=="anc4" & year_id == x],
       override_scale=c(0,1),
       color_ramp=rev(brewer.pal(7,"RdYlGn")),
       legend_position="bottom",
       return_map_object_only=TRUE)
})

legend <- g_legend(p.sba[[1]])
## Arrange

## ifd
sba1 <- arrangeGrob(p.sba[[1]] + theme(legend.position="none") + ggtitle("2000") + theme(plot.title = element_text(size = 20)))
sba2<- arrangeGrob(p.sba[[2]] + theme(legend.position="none") + ggtitle("2005") + theme(plot.title = element_text(size = 20)))
sba3 <- arrangeGrob(p.sba[[3]] + theme(legend.position="none") + ggtitle("2010") + theme(plot.title = element_text(size = 20)))
sba4 <- arrangeGrob(p.sba[[4]] + theme(legend.position="none") + ggtitle("2015") + theme(plot.title = element_text(size = 20)))
sba.t <- arrangeGrob(sba1, sba2, sba3,sba4, nrow=1)
sba.r <- arrangeGrob(sba.t, legend, ncol=2, widths=c(8, 1))
g.sba <- arrangeGrob(textGrob("SBA", gp=gpar(fontsize=20)), sba.r, widths=c(1, 10), nrow=1)

## IFD
ifd1 <- arrangeGrob(p.ifd[[1]] + theme(legend.position="none") + ggtitle("2000") + theme(plot.title = element_text(size = 20)))
ifd2<- arrangeGrob(p.ifd[[2]] + theme(legend.position="none") + ggtitle("2005") + theme(plot.title = element_text(size = 20)))
ifd3 <- arrangeGrob(p.ifd[[3]] + theme(legend.position="none") + ggtitle("2010") + theme(plot.title = element_text(size = 20)))
ifd4 <- arrangeGrob(p.ifd[[4]] + theme(legend.position="none") + ggtitle("2015") + theme(plot.title = element_text(size = 20)))
ifd.t <- arrangeGrob(ifd1, ifd2, ifd3,ifd4, nrow=1)
ifd.r <- arrangeGrob(ifd.t, legend, ncol=2, widths=c(8, 1))
g.ifd <- arrangeGrob(textGrob("IFD", gp=gpar(fontsize=20)), ifd.r, widths=c(1, 10), nrow=1)

## ANC4
anc41 <- arrangeGrob(p.anc4[[1]] + theme(legend.position="none") + ggtitle("2000") + theme(plot.title = element_text(size = 20)))
anc42<- arrangeGrob(p.anc4[[2]] + theme(legend.position="none") + ggtitle("2005") + theme(plot.title = element_text(size = 20)))
anc43 <- arrangeGrob(p.anc4[[3]] + theme(legend.position="none") + ggtitle("2010") + theme(plot.title = element_text(size = 20)))
anc44 <- arrangeGrob(p.anc4[[4]] + theme(legend.position="none") + ggtitle("2015") + theme(plot.title = element_text(size = 20)))
anc4.t <- arrangeGrob(anc41, anc42, anc43,anc44, nrow=1)
anc4.r <- arrangeGrob(anc4.t, legend, ncol=2, widths=c(8, 1))
g.anc4 <- arrangeGrob(textGrob("ANC4", gp=gpar(fontsize=20)), anc4.r, widths=c(1, 10), nrow=1)



fig <- arrangeGrob(g.sba, g.ifd, g.anc4, nrow=3) 


png("C:/Users/pyliu/Desktop/data/figure1.png", w=20, h=21, units="in", res=150)
grid.arrange(fig)
dev.off()


ggplot(pred.frame[var=="sba"]) + 
  geom_line(aes(y=est_mean, x=year_id, group=ID_1, color=ID_1))



## Table
shp.adm1@data <- shp.adm1@data %>% data.table

df <- pred.frame[var %in% c("sba", "ifd", "anc4"), .(ID_1, year_id, var, est_mean, est_lower, est_upper)]
df <- merge(df, shp.adm1@data[,.(ID_1, NAME_1)], by="ID_1", all.x=TRUE)


ggplot(df[year_id==2015]) + 
  geom_histogram(aes(x=est_mean, binwidth=0.1)) +
  geom_density(aes(x=est_mean)) + 
  xlab("Coverage") +
  facet_wrap(~var)