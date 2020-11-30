#'---------------------------------------------------------
#'title: R Mapping Tutorial
#'author: Kathryn Campbell
#'date: 19/10/2020
#'---------------------------------------------------------

rm(list=ls())

# This clears anything currently in the environment

#############################################
#            INSTALL PACKAGES               #
#############################################
library(rgdal)
library(ggplot2)
library(rworldmap)
library(cleangeo)
library(dplyr)
library(gridExtra)
library(ggpubr)
library(mapproj)


#############################################
#              LOAD FUNCTIONS               #
#############################################
source("R/shp_to_df.R")
source("R/create_discrete_scale_for_GLUE.R")
# These scripts were created by Rachel Steenson 31/01/2020

#############################################
#                IMPORT DATA                #
#############################################
systematic_data <- read.csv(file = "Data/Gurdeep_data.csv")
# This data is from Gurdeep's systematic review 19/10/20
# Lists the title, country and year for each paper
# Edited for easy import into R

# Download world map
world_map <- getMap()
plot(world_map)

#############################################
#              CLEANING DATA                #
#############################################
# Find which country names do not match between data and map file
map_countries <- as.character(world_map$ADMIN)
systematic_countries <- unique(systematic_data$Country)
no_match <- setdiff(systematic_countries, map_countries); message(length(no_match), " countries are mis-matched: \n", paste0(no_match, collapse="\n"))

# Fix names
systematic_data$Country[grepl("Uganda", systematic_data$Country)] <- "uganda"
systematic_data$Country[grepl("Nigeria", systematic_data$Country)] <- "Nigeria"
systematic_data$Country[grepl("Ethiopia", systematic_data$Country)] <- "Ethiopia"
systematic_data$Country[grepl("Tanzania", systematic_data$Country)] <- "Republic of Tanzania"
systematic_data$Country[grepl("Zambai", systematic_data$Country)] <- "Zambia"
systematic_data$Country[grepl("South Africa", systematic_data$Country)] <- "South Africa"
systematic_data$Country[grepl("chad", systematic_data$Country)] <- "chad"
systematic_data$Country[grepl("Central African Republic", systematic_data$Country)] <- "Central African Republic"
# How many entries are there for the remaining mis-matches?
table(systematic_data$Country[which(systematic_data$Country %in% no_match)])

# Have a quick look at the number of papers per country, and the total number of countries
country_table<-table(systematic_data$Country); country_table
length(country_table)

#############################################
#           PROCESS THE DATA                #
#############################################
systematic_countries <- systematic_data %>%
  group_by(Country) %>%
  summarise(n=n()) ; systematic_countries
# Make a quick table of each country and the number of papers associated with it

world_map <- clgeo_Clean(world_map)
# Clean shapefile - if you run the shp_to_df function without this, you get an error (not normally required!)

world_map_data <- merge(world_map, systematic_countries, by.x="ADMIN", by.y="Country")
# Merge summary data with world map

world_map_data[is.na(world_map_data$n)] <- 0
table(world_map_data$n, useNA="always")
# Set number of sequences==NA as 0

world_map_data_df <- shp_to_df(world_map_data)
# Transform the spatialPolygonsDataframe to a dataframe, with coordinates stored in the df

#############################################
#               MAP THE DATA                #
#############################################
summary(world_map_data_df$n)
# Quick summary of the data - this will give the max number of papers - necessary for setting the breaks!

br <- c(0:12)
world_map_data_df_catscale1 <- create_discrete_scale_for_GLUE(dataframe=world_map_data_df, n_col="n",
                                                                  breaks=br)
cols = c("white", colorRampPalette(c("#ffe5e5", "#b30000"))(length(br)-1))
systematic_map<-ggplot() +
  geom_polygon(data=world_map_data_df_catscale1, aes(x=long, y=lat, group=group, fill=cat_scale), col="black") +
  scale_fill_manual(name="Number of \nPapers", values=cols) +
  theme_void() +
  coord_equal() +
  ggtitle("Systematic Review Papers")
systematic_map
# Make the map! 
# Set the breaks appropriately and use the function to create a colour scale
# Plot the map including the data and a colour scale indicating the number of papers, and give it a title

png("figures/191020_SystematicPapers.png", width = 1100, height = 600)
systematic_map
dev.off()
# Save the figure!

#############################################
#             STACKED BARPLOT               #
#############################################
# Make a plot to show how many papers were published each year, split by country

stacked_plot <- ggplot(systematic_data, aes(x=factor(Year_of_publication), fill=Country))+
  geom_histogram(stat="count", position="stack") +
  ggtitle("Papers per Year"); stacked_plot

png("figures/201020_PapersPerYear.png", width = 1100, height = 600)
stacked_plot
dev.off()
# Save the figure!
