##############################
# This function plots maps and distribution plots to a PDF.
# Author: Rebecca Woodson Stubbs
# Written 2016
##############################

library(data.table)
library(ggplot2)
library(grid)
library(ggthemes)
library(maptools)
library(rje)

# For more documentation and examples, see: https://rpubs.com/BeccaStubbs/woodson_examples

##############################################################################
# SERIES MAP FUNCTION
##############################################################################

wmap<-function(chloropleth_map,
               geog_id,
               variable,
               
               # Optional data/geometry
               outline_map=NULL, # 
               data=NULL,
               
               # What elements of the map do you want the function to return?
               histogram=TRUE,
               return_map_object_only=FALSE,
               destination_folder=NULL,
               
               # Inputs for the color scheme of the maps
               color_ramp=wpal("easter_to_earth"),
               outline_size=.1,
               outline_color="white",
               override_scale=NULL,
               color_value_breaks=NULL,
               
               # Inputs for map titles
               map_title=" ",
               additional_variable_name_string=NULL,
               title_font_size=NULL,
               title_font_face="plain",
               
               # Inputs for generating series-maps
               series_dimension=NULL,
               series_sequence=NULL,
               
               # Inputs for map Legend
               legend_position="bottom", 
               legend_font_size=NULL,
               legend_font_face="plain",
               legend_bar_width=.4,
               legend_bar_length=20,
               legend_breaks=NULL,
               legend_labels=NULL,
               
               # Do you want print statements?
               verbose=F){      
  
  ##############################################################################################################################
  # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ Information about this Function~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
  #
  # This is an all-purpose mapping function that plots a chloropleth map in R. This function is designed to make generating
  # simple, beautiful maps in R a quick and easy process. The aesthetic of these maps are designed to be simple, easy-to-read, 
  # and have minimal visual distractions. 
  # 
  # You can use this function to create a single map of 1 variable, or you can use this function to create a series of maps
  # that loops through dimensions of a variable (say, years, or types of vaccine coverage). 
  #
  # MANDATORY INPUTS:
  # * chloropleth_map
  #      A SpatialPolygons object with data as a data.table rather than a data.frame.
  # * geog_id
  #     A string-- the name of the column that serves as the geographic ID that specifies the unit of analyisis for your data
  # * variable
  #     A string-- the name of the column that will serve as the values you want to plot by your geog_id
  #      
  # Optional data/geometry
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  # * data 
  #     A data.table that contains the data you want to map (must contain geog_id, and the variable of 
  #     interest, if specified. If a series dimension and/or series sequence is defined, those must also exist in this data set)
  # * outline_map
  #      Another SpatialPolygons object that you want to use the outlines from.
  #
  # What elements do you want the plot to contain?
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  # * histogram
  #     TRUE/FALSE. If "TRUE", the plot will contain a histogram of the values at the bottom.
  # * return_ map_ object_ only
  #     If "TRUE", you can assign the function to a variable, and store the map plot portion of this ggplot object so that you can combine it with other graphics at will. Default value is FALSE. This will never return the histogram. 
  # * destination_folder
  #     A string file path to a folder you want a PDF created in that will hold your map(s). The pdf will be have a title that is the variable name, plus any additional_ variable_ name_string you specify. If this ps specified, a pdf with the map(s) will be created.
  #
  # Inputs for the color scheme of the maps
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  # * color_ramp
  #     A list of colors that will serve as the colors you "stretch" through based on your data values. This will default to a color scheme described in woodson pallettes called "Easter to Earth" that displays variation well when there are many geographic units. 
  #     The fewer geographic units, the simpler you probably want your color ramp to be. See woodson palletes for more options, or create your own.
  # * outline_color
  #     What color you want the outline of the additional geography to be (if provided). This can be any color r recognizes--suggestions might be "black","yellow", or "white". Default is white.
  # * outline_size
  #     A numeric value that specifies how large you want your white outlines to be if you have specified an outline you want shown on your map. Default value is .1. 
  # * override_scale
  #     A vector with two items that will be used to stretch the color ramp instead of the min/max values present in the data set. should be structured "c(min,max)".
  # * color_ value_ breaks
  #     How you want the colors "stretched" across the range of minimum/maximum values. 
  #     Default is NULL/ uniform distribution stretched across the color ramp from the minimum and maximum data values provided. 
  #
  # Inputs for map titles
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  # * map_title
  #     A string that serves as the basis for your map title (if no dimensions are specified, the title will be as it is specified. If a dimension is specified, a phrase constructed using the series 
  #     dimension and what you are mapping will be added to the plot title [ex="year:1990"]. 
  # * additional_ variable_ name_ string
  #     This is an additonal string that you want to add to the name of the PDF to describe any other breakdowns you might be using. For example, if you had to 
  #     map something by year, age, sex, you would first need to subset your data to be one age/sex group 
  #    before plotting it out by year. If you subset your data in a loop, you could use this string to specify something along the lines of paste0("age_ ",a," _ sex _",s). NOTE: You need to put in a similar paste0 statement in your map title if you also want this sub-breakdown 
  #     described in the title of your map, not just the file path to the PDF.
  # * title_ font_ size
  #     How large you want the title font to be. No default; default values based on ggthemes tufte()'s default
  # * title_ font_ face
  #     Special properties of the title font. Options include "plain", "bold", "italic". Default is plain.
  #
  #  Inputs for generating series-maps
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  # * series_dimension
  #   A string-- the name of the column that will serve as the variable 
  #   you loop through to create a series map. For example, year. 
  # * series_sequence
  #   A vector c(x,y,z...) that specifies a subset of the series dimensions you want to map. For example, if you have a data set that contains all years between 1980-2014, you can specify that you only want to plot out every other year by setting series sequence to be seq(1980,2014,2). This function will make sure all of the items you speficy actually exist within your series_dimension. 
  # 
  # Inputs for map legend
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # * legend_position
  #    Where you want the legend to go. Options are "top","bottom","right","left", and "none", which will create a map with no legend 
  #   (useful if you want to return the map only and add a custom legend yourself). Default is "bottom".
  # * legend_ font_ size
  #     How large you want the legend font to be. No default; default values based on ggthemes tufte()'s default.
  # * legend_ font_ face
  #     Special properties of the legend font. Options include "plain", "bold", "italic". Default is plain.
  # * legend_ bar_ width
  #     How fat you want the color bar that serves as the legend to be. Default value is 0.4.   
  # * legend_ bar_ length
  #     How long you want the color bar that serves as the legend to be. Default value is 20.
  # * legend_ breaks
  #     An optional vector of the values you want to label in your legend's color scale.
  # * legend_ labels
  #     An optional vector of the character strings you want to use to label your legend's color scale (must be same length as legend_breaks)
  #
  # Inputs for print statements
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~           
  # * verbose          
  #            Whether you want print statements from the function (default=F)
  #
  # If you have a wishlist for functionalities, and/or would like to contribute to this effort, feel free to contact the author,
  # Rebecca Stubbs, at stubbsrw@uw.edu.
  ##############################################################################################################################
  
  # Copying objects such that the original names of the variables are unaltered
    chloropleth_map<-copy(chloropleth_map)
    outline_map<-copy(outline_map)
    data<-copy(data)
  # Defensive Checks on Input Data:
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check that the specified geog_id exists, check for the variable's existence if no external data is provided
    try(if((geog_id %in% names(chloropleth_map@data)==F)&("geog_id" %in% names(chloropleth_map@data)==F)) stop("That geographic ID does not appear to exist within your chloropleth map object.")) # Check to see if the "geog_id" field is in your raw data
    if (length(data)==0){try(if((variable %in% names(chloropleth_map@data)==F)) stop("That variable does not appear to exist within your data set [the spatial polygons data frame you say contains the data, too]."))}# Check to see if the variable is in the external data provided:
  # If external data is specified, check to see if geog_id and the varaible is within the data
    if (length(data)>0){try(if((geog_id %in% names(data)==F)) stop("That geographic ID does not appear to exist within your data set."))} 
    if (length(data)>0){try(if((variable %in% names(data)==F)) stop("That variable does not appear to exist within your data set."))}# Check to see if the variable is in the external data provided:
  # Preparing Data For Mapping
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Setting the geographic ID variables as "geog_id" in both the data and geographic rdata objects
    setnames(chloropleth_map@data,geog_id,"geog_id")
  # Defining the data you want to map: If it's in addition to the chloropleth map object, change the geog_id column name to "geog_id" to facilitate the merge
    if (length(data)>0){ # If external data was provided...
      setnames(data,geog_id,"geog_id") #If an external data source is provided, set that name to geog_id as well
    }else{data<-copy(chloropleth_map@data)} # if no external data was provided, "data" is now just the data that was in the map object in the first place.
  # Renaming the variable to be "variable" 
    setnames(data, variable, "variable") # Renaming the variable of interest to "variable" within the dataset
  # If there is a series dimension specified, check to make sure it is in the data set and, set that name to be "series_dimension". 
    if (length(series_dimension)>0){ # If you plan to loop through miltiple dimensions...
      try(if((series_dimension %in% names(data)==F)) stop("That series dimension (what you want to iterate through) does not appear to exist within your data set.")) # Check to make sure it exists in your data
      setnames(data,series_dimension,"series_dimension")}else{data[,series_dimension:="*&^! no dimensions"]} # If a series dimension is specified, rename it "series_dimension". If there is no series dimension, add a column and call it "no dimensions"; we'll only loop through once to plot whatever variable you have with no series.
  # Sub-setting the data such that only the variables that matter are kept
    data<-data[, list(geog_id=as.character(geog_id), variable, series_dimension)] # Sub-setting your data c keep only the relevant columns
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Fortifying the Map and Joining on the Data
    # "fortifying" the Rdata shapefiles
      chloropleth_map <-data.table(suppressWarnings(fortify(chloropleth_map, region="geog_id")))
      if (length(outline_map)>0){outline_map<-data.table(suppressWarnings(fortify(outline_map)))} # If an outline map is specified, fortify the outline map as well.
    # Renaming the chloropleth map "ID" field as geog_id
      setnames(chloropleth_map,"id","geog_id")
    # creating one long, huge object that you can subset by merging together the data and the map, if the data isn't already in the map.
      chloropleth_map<-merge(data, chloropleth_map, by="geog_id", allow.cartesian=T)
    # If no series sequence is defined, but a series dimension is defined, loop through every option or layer of the dimensions
    if (length(series_sequence)==0){map_dims<-unique(chloropleth_map$series_dimension)}
    # If a series sequence is defined (the function was passed a vector of particular values within the series dimension): 
    if (length(series_sequence)>0){
      if(verbose) print("Note: The color ramp will stil be set based on ALL dimensions of your variable, unless you override it otherwise.") # Printing a warning 
      # If you have specified a series you want to loop through (for example, only SOME years), 
      # we will loop through that instead of every unique option. First, though, we check to make sure each of those options actually exists within the column.
      for (select_dimension in series_sequence){ 
        try(if((select_dimension %in% unique(chloropleth_map$series_dimension)==F)) stop(paste0("The dimension ",select_dimension," does not appear to exist in the column you have specified to hold your dimensions.")))}
      # If the above checks all pass...:                          
      map_dims<-series_sequence} #Set the "dimensions" as the series_sequence specified; the map will iterate through that sequence.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Setting the range of values the colors will scale between
  ## The default:
  #Discovering what the min and max are of the variable across the whole data series (all values of that variable)
  maximum<-max(chloropleth_map[["variable"]]); minimum<-min(chloropleth_map[["variable"]])
  ##  Overriding the min/max scale with input values, if desired:
  # If an override was provided, setting minimum to the first in the list, and maximum to the second in the list provided.
  if (length(override_scale)>0){minimum<-override_scale[1];maximum<-override_scale[2]}
  
  ###########################################
  ## LOOPING ACROSS DIMENSIONS
  ########################################### 
  # If an output folder is specified, it means that you have decided you want your maps to be written to a PDF. 
  # this line starts a PDF, since we want the PDF to contain each of the maps we make in series , so we need to open it before we start looping through our variable values. 
  if (length(destination_folder)>0){pdf(paste0(destination_folder,variable,additional_variable_name_string,".pdf"))} #If you want it written to a pdf, (because you specified a destination folder) open it!
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Starting the Loop
  for (select_dimension in map_dims){ #for each dimension you want to plot...
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Determining map title, and subsetting the data
    if (select_dimension=="*&^! no dimensions"){main_map_title<-map_title}else{main_map_title<-paste0(map_title,": ",select_dimension)} # Determining the map title
    if(verbose) print(main_map_title) # print out on screen what variable and dimension the script is on!
    subset<-chloropleth_map[series_dimension==select_dimension]  # Sub-setting the fortified object to map out 1 layer/dimension (ex: year) of the variable of interest  
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Creating the Map Plot in GGPlot2
    map_plot<-ggplot(subset)  + #starting the ggplot object based on the subset, fortified data  
      geom_polygon(aes(x=long, y=lat, group=group, fill=variable), color=NA, size=0.0)  # Telling the ggplot that you want to be "filling" the polygons with the values from variables
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Defining Color Ramp
    if(!is.null(legend_breaks)&!is.null(legend_labels)){
      map_plot<-map_plot+scale_fill_gradientn(colours=rev(color_ramp), limits=c(minimum, maximum), values=color_value_breaks, breaks=legend_breaks, labels=legend_labels) + #Defining the colors (based on the mapcolors from cubehelix above) for each value
        #more map formatting: Keeping the image clean by avoiding unecessary scales, keeping the background white, etc.
        scale_x_continuous("", breaks=NULL) + # Getting rid of the x-scale
        scale_y_continuous("", breaks=NULL) + # Getting rid of the y-scale
        coord_fixed(ratio=1) # Making sure that 1 unit on the x axis is the same as 1 unit on the y axis
    }else{
      map_plot<-map_plot+scale_fill_gradientn(colours=rev(color_ramp), limits=c(minimum, maximum), values=color_value_breaks) + #Defining the colors (based on the mapcolors from cubehelix above) for each value
        #more map formatting: Keeping the image clean by avoiding unecessary scales, keeping the background white, etc.
        scale_x_continuous("", breaks=NULL) + # Getting rid of the x-scale
        scale_y_continuous("", breaks=NULL) + # Getting rid of the y-scale
        coord_fixed(ratio=1) # Making sure that 1 unit on the x axis is the same as 1 unit on the y axis
    } # Why have this if-clause? Turns out, if the value of legend_breaks is NULL, then you end up not getting a legend at all. Lame!
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Adding a legend
    if (legend_position %in% c("bottom","top")){
      map_plot<-map_plot+guides(fill=guide_colourbar(title=" ", barheight=legend_bar_width, barwidth=legend_bar_length, label=TRUE, ticks=FALSE )) + # Legend for the colors
        labs(title = main_map_title) + # Setting the title as the map title specified in the function
        theme_tufte() + # Defining a "tufte" theme for the map (this eliminates unecessary things like background grey matrices, etc)
        theme(legend.position=legend_position)} # Specifying that you want the legend at the bottom of the map. }
    if (legend_position %in% c("right","left")){
      map_plot<-map_plot+guides(fill=guide_colourbar(title=" ", barheight=legend_bar_length, barwidth=legend_bar_width, label=TRUE, ticks=FALSE )) + # Legend for the colors
        labs(title = main_map_title) + # Setting the title as the map title specified in the function
        theme_tufte() + # Defining a "tufte" theme for the map (this eliminates unecessary things like background grey matrices, etc)
        theme(legend.position=legend_position)} # Specifying that you want the legend at the bottom of the map. 
    if (legend_position %in% c("none")){
      if(verbose) print("You don't want a legend!")
      map_plot<-map_plot+ # 
        labs(title = main_map_title) + # Setting the title as the map title specified in the function
        theme_tufte() + # Defining a "tufte" theme for the map (this eliminates unecessary things like background grey matrices, etc)
        theme(legend.position="none")
    }
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Adding Titles
      map_plot<-map_plot + theme(plot.title = element_text(size = title_font_size,  face=title_font_face))+
        theme(legend.text = element_text(size = legend_font_size, face=legend_font_face))
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Adding Outline Maps
      if (length(outline_map)>0){    ## If there was an outline specified, add outline geometry in white
        if (length(outline_color)>0){outline_map_color<-outline_color}
        map_plot<-map_plot+geom_path(data = outline_map, 
                                     aes(x = long, y = lat, group = group),
                                     color = outline_map_color, size = outline_size)} 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## If you just want the map plot as an object you can pass to other things...
    if (return_map_object_only==TRUE){return(map_plot)}else{
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Making a histogram of the distribution of that year's values
      if (histogram==TRUE){ # If you have specified that you do want the histogram at the bottom:
        histo<-ggplot(subset, aes(x=variable)) + geom_histogram(aes(fill = ..count..), bins=30, na.rm = TRUE)+xlim(minimum, maximum)+
          scale_fill_gradient("Count", low =  color_ramp[3], high = color_ramp[length(color_ramp)-2])+theme_tufte()+
          guides(fill=guide_colourbar(title=" ", barheight=0, barwidth=0, label=FALSE, ticks=FALSE))+
          labs(title="Distribution",x= "",y="")+theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank())}
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Printing the Plot:
      if (histogram==TRUE){# Combining Histogram and Map to plot into a single image.
        grid.newpage() # Starting a new page
        pushViewport(viewport(layout = grid.layout(5, 1))) # Defining the ratio of the histogram to map to be 5 sections vertically, 1 horizontally
        vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y) # Defining a function that allows setting the layout of the viewport 
        print(map_plot, vp = vplayout(1:4, 1)) # Printing the map plot to the viewport in vertical slots 1-4, covering all the x-space
        print(histo, vp = vplayout(5, 1)) # Printing the histogram to the bottom of the map: 
      }else{print(map_plot)} #If you didn't want the histogram, just print out the map!
    } # Closing the "if return map object=TRUE" clause
  } # Closing the loop of dimensions
  if (length(destination_folder)>0){dev.off();print("PDF ready to view.")} #If you were writing this to a PDF, you can close it, and check it out!
} # Closing Function!


##############################################
## Plotting a color pallette
##############################################

# Adapted from:
#https://learnr.wordpress.com/2009/04/15/ggplot2-qualitative-colour-palettes/
plot_colors<-function(color_list,color_list_name=NULL){
  # Author: Rebecca Stubbs
  # color_list: list of colors
  # color_list_name: The title of the plot
  # requires: data.table,ggplot2,ggthemes
  
  data_table<-data.table(data.frame(x=letters[1:length(unique(color_list))]))
  data_table[, colors:=unique(color_list)]
  data_table[, bar_height:=1]
  p<-ggplot(data_table,aes(x=colors))+geom_bar(aes(fill=colors, stat="identity"))+theme_tufte()+labs(x=paste(unique(color_list), collapse=', '))+
    theme(
      legend.position = "none",
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank())
  p<-p+ scale_fill_manual(values=color_list) + ggtitle(color_list_name)
  return(p)
}

################################################
## Generating a cubehelix intensity pallette
#################################################
intensity_pallette<-function(start,r,gamma,hue_list=c(.5,1,3)){
  # This function creates a color pallette from a cubehelix that increases in Intensity/hue as it gets darker.
  # Start: what color you want to start with
  # hue: how bright you want the colors to be (1: normal, higher: brighter, lower: more demure)
  # R: How many "rotations" through color space you want it to do (how complicated do you want your color ramp to be?)
  # gamma: How light or dark you want it to be (1: normal, higher:darker, lower: lighter)
  # requires: library(rje) for the cubehelix function. 
  # Author: Rebecca Stubbs on 2/18/2016
  #low intensity pallette
  mellow<-cubeHelix(11, start = start, r = r, hue = hue_list[1], gamma = gamma)
  #middling intensity
  mid<-cubeHelix(11, start = start, r = r, hue = hue_list[2], gamma = gamma) 
  #strong intensity
  strong<-cubeHelix(11, start = start, r = r, hue = hue_list[3], gamma = gamma) #hues higher than 2 get weird
  # binding together the colors such that they are flipped (light to dark)
  colors<-(rbind(rev(mellow[2:8]),rev(mid[2:8]),rev(strong[2:8])))
  mellow_to_strong<-append(colors[1,1:2],colors[2,3:5]) #appending together the mellow and middling colors
  mellow_to_strong<-append(mellow_to_strong,colors[3,6:7]) # appending together the strong colors onto the mellow and middling
  return(mellow_to_strong)}

################################################
## Creating a function (wpal) that 
#################################################
#http://www.mrao.cam.ac.uk/~dag/CUBEHELIX/cubewedges.html
wpal<-function(color=NULL){
  wpal<-list()
  wpal[["easter_to_earth"]]<-c("#000000", "#5b2c44", "#826737", "#67af6d", "#90c6de", "#ffff4c")
  wpal[["cool_toned"]]<-c("#000000", "#3E350C", "#9D4B60" , "#AB86D0" ,"#97E4DF","#ffff4c")
  wpal[["bright_greens"]]<-(cubeHelix(8, start = -0.5, r = -.3, hue = 3, gamma = 1.5)[2:7]) #bright_greens
  wpal[["bright_fire"]]<-(cubeHelix(8, start = .5, r = .4, hue = 3, gamma = 1.3)[2:7]) # Bright fire
  wpal[["eggplant_to_teal"]]<-(cubeHelix(8, start = .95, r = -.7, hue = 3, gamma = 1.3)[2:7]) # eggplant_to_teal
   
  ##############################
  # Half-Rotation Colors
  ##############################
  wpal[["purple_to_sea_green"]]<-cubeHelix(11, start = 0.5, r = -.5, hue = 1.5, gamma = 1)[1:10] 
  wpal[["thanksgiving"]]<-cubeHelix(11, start = 0.5, r = .5, hue = 1.5, gamma = 1)[1:10] 
  wpal[["purple_to_lavender"]]<-cubeHelix(11, start = 1, r = -.5, hue = 1.5, gamma = 1)[1:10] 
  wpal[["brown_to_sea_green"]]<-cubeHelix(11, start = 1, r = .5, hue = 1.5, gamma = 1)[1:10] 
  wpal[["brown_to_pink"]]<-cubeHelix(11, start = 1.5, r = -.5, hue = 1.5, gamma = 1)[1:10] 
  wpal[["green_to_lavender"]]<-cubeHelix(11, start = 1.5, r = .5, hue = 1.5, gamma = 1)[1:10] 
  wpal[["green_to_salmon"]]<-cubeHelix(11, start = 2, r = -.5, hue = 1.5, gamma = 1)[1:10] 
  wpal[["sea_green_to_pink"]]<-cubeHelix(11, start = 2, r = .5, hue = 1.5, gamma = 1)[1:10] 
  wpal[["deep_blue_to_pink"]]<-cubeHelix(11, start = 2.5, r = -.5, hue = 1.5, gamma = 1)[1:10] 
  wpal[["deep_blue_to_pink"]]<-cubeHelix(11, start = 2.5, r = .5, hue = 1.5, gamma = 1)[1:10] 
  
  ##############  .8 rotation colors
  wpal[["brown_green_blue_pink"]]<-cubeHelix(11, start = 1, r = .8, hue = 1.5, gamma = 1)[1:10] 
  wpal[["red_green_blue"]]<-cubeHelix(11, start = .5, r = .8, hue = 1.5, gamma = 1)[1:10] 
  wpal[["purple_red_green"]]<-cubeHelix(11, start = 0, r = .8, hue = 1.5, gamma = 1)[1:10] 
  wpal[["sea_green_purple_tan"]]<-cubeHelix(11, start = 2, r = .8, hue = 1.5, gamma = 1)[1:10] 
  wpal[["classy_earth"]]<-cubeHelix(11, start = .5, r = .8, hue = .5, gamma = 1)[1:10]
  wpal[["stormy_seas"]]<-cubeHelix(11, start = 1, r = .8, hue = .5, gamma = 1)[1:10]
  
  ## A bunch of different, single-rotation color helix patterns:
  wpal[["black_to_light_1"]]<-cubeHelix(11, start = 0, r = -1, hue = 1, gamma = 1)[1:10]
  wpal[["black_to_light_2"]]<-cubeHelix(11, start = .5, r = -1, hue = 1, gamma = 1)[1:10]
  wpal[["black_to_light_3"]]<-cubeHelix(11, start = 1, r = -1, hue = 1, gamma = 1)[1:10]
  wpal[["black_to_light_4"]]<-cubeHelix(11, start = 1.5, r = -1, hue = 1, gamma = 1)[1:10]  
  wpal[["black_to_light_5"]]<-cubeHelix(11, start = 2, r = -1, hue = 1, gamma = 1)[1:10]  
  wpal[["black_to_light_6"]]<-cubeHelix(11, start = 0, r = 1, hue = 1.5, gamma = 1)[1:10]  
  wpal[["black_to_light_7"]]<-cubeHelix(11, start = .5, r = 1, hue = 1.5, gamma = 1)[1:10]  
  wpal[["black_to_light_8"]]<-cubeHelix(11, start = 1, r = 1, hue = 1.5, gamma = 1)[1:10]  
  wpal[["black_to_light_9"]]<-cubeHelix(11, start = 1.5, r = 1, hue = 1.5, gamma = 1)[1:10]  
  wpal[["black_to_light_10"]]<-cubeHelix(11, start = 2, r = 1, hue = 1.5, gamma = 1)[1:10]  
  wpal[["black_to_light_11"]]<-cubeHelix(11, start = 0, r = 1, hue = .5, gamma = 1)[1:10]  
  wpal[["black_to_light_12"]]<-cubeHelix(11, start = .5, r = 1, hue = .5, gamma = 1)[1:10]  
  wpal[["black_to_light_13"]]<-cubeHelix(11, start = 1, r = 1, hue = .5, gamma = 1)[1:10]  
  wpal[["black_to_light_14"]]<-cubeHelix(11, start = 1.5, r = 1, hue = .5, gamma = 1)[1:10]
  wpal[["black_to_light_15"]]<-cubeHelix(11, start = 2, r = 1, hue = .5, gamma = 1)[1:10]
  
  
  
  ########################
  # Diverging Schemes
  ########################
  ### Diverging Pallettes Based on Intensity Pallettes
  wpal[["purple_to_green_diverging_intensity"]]<-append(rev(intensity_pallette(start=.2,r=.4,gamma=1)), intensity_pallette(start=1.75,r=.5,gamma=1))
  wpal[["blue_to_red_intensity"]]<-append(rev(intensity_pallette(start=.5,r=-.5,gamma=1)), intensity_pallette(start=0.5,r=.5,gamma=1))
  
  ### Diverging from black
  wpal[["pink_blue_multi_diverging_from_black"]]<-append(rev(cubeHelix(11, start = 1.5, r = -.4, hue = 1.75, gamma = 1)[1:9]), cubeHelix(11, start = .5, r = -.4, hue = 1.75, gamma = 1)[1:10])
  wpal[["orange_blue_diverging_from_black"]]<-append(rev(cubeHelix(11, start = .5, r = -.4, hue = 1.75, gamma = 1)[1:9]), cubeHelix(11, start = .5, r = .4, hue = 1.75, gamma = 1)[1:10])
  wpal[["green_purple_diverging_from_black"]]<-append(rev(cubeHelix(11, start = 0, r = -.4, hue = 1.75, gamma = 1)[1:9]), cubeHelix(11, start = 1, r = -.4, hue = 1.75, gamma = 1)[1:10])
  wpal[["tan_green_multi_diverging_from_black"]]<-append(rev(cubeHelix(11, start = 2, r = -.4, hue = 1.75, gamma = 1)[1:9]), cubeHelix(11, start = .5, r = -.4, hue = 1.75, gamma = 1)[1:10])
  ### Diverging from white
  wpal[["pink_blue_multi_diverging_from_white"]]<-append(cubeHelix(11, start = 1.5, r = -.4, hue = 1.75, gamma = 1)[3:11], rev(cubeHelix(11, start = .5, r = -.4, hue = 1.75, gamma = 1)[3:10]))
  wpal[["purple_blue_diverging_from_white"]]<-append(cubeHelix(11, start = 1, r = -.4, hue = 1.75, gamma = 1)[3:11], rev(cubeHelix(11, start = 0, r = -.4, hue = 1.75, gamma = 1)[3:10]))
  wpal[["tan_green_multi_diverging_from_white"]]<-append(cubeHelix(11, start = 2, r = -.4, hue = 1.75, gamma = 1)[3:11], rev(cubeHelix(11, start = .5, r = -.4, hue = 1.75, gamma = 1)[3:10]))
  ## Diverging from Colors 
  wpal[["green_pink_diverging_from_purple"]]<-append(rev(cubeHelix(11, start = 3, r = -.4, hue = 1.75, gamma = 1)[3:10]), (cubeHelix(11, start = 3, r = .4, hue = 1.75, gamma = 1)[3:10]))
  wpal[["orange_blue_diverging_from_purple"]]<-append(rev(cubeHelix(11, start = .5, r = -.4, hue = 1.75, gamma = 1)[3:10]), (cubeHelix(11, start = .5, r = .4, hue = 1.75, gamma = 1)[3:10]))
  wpal[["tan_blue_multi_diverging_from_green"]]<-append(rev(cubeHelix(11, start = 2, r = -.4, hue = 1.75, gamma = 1)[3:10]), (cubeHelix(11, start = 2, r = .4, hue = 1.75, gamma = 1)[3:10]))
  
  ##############################
  # Making Intensity Pallettes
  ##############################
  ## Testing out some different color pallettes  
  wpal[["tan_to_red_intensity"]]<-intensity_pallette(start=0.5,r=.5,gamma=1)
  wpal[["lavender_to_deep_green_intensity"]]<-intensity_pallette(start=1.75,r=.5,gamma=1)
  wpal[["pink_to_purple_intensity"]]<-intensity_pallette(start=3,r=.5,gamma=1)
  wpal[["mild_green_to_dark_brown_intensity"]]<-intensity_pallette(start=1,r=.5,gamma=1)
  wpal[["lavender_green_dark_brown_intensity"]]<-intensity_pallette(start=1,r=.75,gamma=1)
  wpal[["sea_green_to_purple_intensity"]]<-intensity_pallette(start=.5,r=-.5,gamma=1)
  wpal[["pastel_to_purple_intensity"]]<-intensity_pallette(start=.2,r=.4,gamma=1)
  wpal[["sea_green_to_blue_intensity"]]<-intensity_pallette(start=3,r=-.4,gamma=1)
  
  if (length(color)>0){return(wpal[[color]])}else{return(wpal)}
}

################ 
# Writing function to view plots
view_wpal<-function(color=NULL){
  
  if (length(color)<1){
    index<-1
    for (color in names(wpal())){
      if (index==1){grid.newpage(); pushViewport(viewport(layout = grid.layout(5, 15)))}
      if (index<=5){print((plot_colors(wpal(color),color_list_name=color)), vp = vplayout(index, 1:15))}
      index <- index+1
      if (index>5){index<-1}
    }}else{print((plot_colors(wpal(color),color_list_name=color)))}
} # Closing function
