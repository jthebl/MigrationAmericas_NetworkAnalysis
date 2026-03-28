
rm(list=ls())

#### Code for creating Immigration Matrices based off of UN data #####
#### Relevant links: https://www.un.org/development/desa/pd/content/international-migrant-stock #####

#### Notes ####

# Data was collected from the UN website. Countries were filtered by those who have a minimum population of 
# 2 million. Parameter for edges is binary with threshold of 10,000 immigrants (i.e. if equal to or greater than
# 10,000 immigrants, qualifies for edge). The edges also represent net immigration, i.e. subtracting the 
# opposing migration pathways between two countries, with the direction of greater size dictating the edge direction,
# though again, edges are only established if there is a minimum of 10,000 immigrants passing from one country to the 
# other after considering the net migration. In this particular code, I added the edge attribute for thickness which is
# associated with this value of net migration divided by the receiving country's population divided by 1000
# (i.e. the units are x immigrants for every 1000 residents).

#####################################################################################################################
#####################################################################################################################

#setting things up

setwd("C:/Users/heblj/OneDrive - Middlebury College/MIIS_Fall 2024/Intro Network Analysis/Final Project/Country Immigration Data Excels")

library(igraph)

load("g_EL_90.rda")
load("g_EL_00.rda")
load("g_EL_10.rda")
load("g_EL_20.rda")


# Set layout for subsequent networks
layout_preferred <- layout_with_fr(g_EL_20)

########################################################
### WITH THE UNITED STATES INCLUDED ###
########################################################




##########################
##### Data from 2020 #####
##########################

# Network from EdgeList
#EL_Network_20 <- read.csv("2020_edgelist_Per1000.csv", header = T)


# Create a graph from the edgelist
#g_EL_20 <- graph_from_data_frame(EL_Network_20, directed = T)

# Scale the edge thickness based on the values
# You can adjust the scaling factor as needed
#E(g_EL_20)$width <- EL_Network_20$Weight

# Plot the graph with edge thickness
plot(g_EL_20, 
     edge.width = E(g_EL_20)$width/4, 
     vertex.size = degree(g_EL_20),
     edge.arrow.size = 0.5,
     vertex.label.cex = 0.8,
     layout = layout_preferred,
     main = "2020",
     sub = "vertex size = degree score \n edge width = immigrants per 1000 residents")








##########################
##### Data from 2010 #####
##########################

# Network from EdgeList
#EL_Network_10 <- read.csv("2010_edgelist_Per1000.csv", header = T)


# Create a graph from the edgelist
#g_EL_10 <- graph_from_data_frame(EL_Network_10, directed = T)

# Scale the edge thickness based on the values
# You can adjust the scaling factor as needed
#E(g_EL_10)$width <- EL_Network_10$Weight


# Plot the graph with edge thickness
plot(g_EL_10, 
     edge.width = E(g_EL_10)$width/4, 
     vertex.size = degree(g_EL_10),
     edge.arrow.size = 0.5,
     vertex.label.cex = 0.8,
     layout = layout_preferred,
     main = "2010",
     sub = "vertex size = degree score \n edge width = immigrants per 1000 residents")














##########################
##### Data from 2000 #####
##########################

# Network from EdgeList
#EL_Network_00 <- read.csv("2000_edgelist_Per1000.csv", header = T)



# Create a graph from the edgelist
#g_EL_00 <- graph_from_data_frame(EL_Network_00, directed = T)

# Scale the edge thickness based on the values
# You can adjust the scaling factor as needed
#E(g_EL_00)$width <- EL_Network_00$Weight


# Plot the graph with edge thickness
plot(g_EL_00, 
     edge.width = E(g_EL_00)$width/4, 
     vertex.size = degree(g_EL_00),
     edge.arrow.size = 0.5,
     vertex.label.cex = 0.8,
     layout = layout_preferred,
     main = "2000",
     sub = "vertex size = degree score \n edge width = immigrants per 1000 residents")













##########################
##### Data from 1990 #####
##########################

# Network from EdgeList
#EL_Network_90 <- read.csv("1990_edgelist_Per1000.csv", header = T)

# Create a graph from the edgelist
#g_EL_90 <- graph_from_data_frame(EL_Network_90, directed = T)

# Scale the edge thickness based on the values
# You can adjust the scaling factor as needed
#E(g_EL_90)$width <- EL_Network_90$Weight


# Plot the graph with edge thickness
plot(g_EL_90, 
     edge.width = E(g_EL_90)$width/4, 
     vertex.size = degree(g_EL_90),
     edge.arrow.size = 0.5,
     vertex.label.cex = 0.8,
     layout = layout_preferred,
     main = "1990",
     sub = "vertex size = degree score \n edge width = immigrants per 1000 residents")










### Saving the Data for easy loading later ###
# save(g_EL_90, file = "g_EL_90.rda")
# save(g_EL_00, file = "g_EL_00.rda")
# save(g_EL_10, file = "g_EL_10.rda")
# save(g_EL_20, file = "g_EL_20.rda")

################################################
############## PLOTTING ########################
################################################

# Load the data

load("g_EL_90.rda")
load("g_EL_00.rda")
load("g_EL_10.rda")
load("g_EL_20.rda")

#Adjusting vertex names for US and Venezuela so not taking up so much space in middle of network

# V(g_EL_90)$name[19] <- "US"
# V(g_EL_90)$name[18] <- "VZ"
# V(g_EL_00)$name[19] <- "US"
# V(g_EL_00)$name[18] <- "VZ"
# V(g_EL_10)$name[19] <- "US"
# V(g_EL_10)$name[18] <- "VZ"
# V(g_EL_20)$name[19] <- "US"
# V(g_EL_20)$name[18] <- "VZ"

# Plotting graphs together: 1990 and 2020 ----

#define layout so easier to compare

layout_preferred <- layout_with_fr(g_EL_20)


#Sizing by Degree
par(mfrow=c(1,4))
plot(g_EL_90,
     main = "1990_SizeDegree",
     edge.width = E(g_EL_90)$width^.5,
     edge.arrow.size = 0.5,
     sub = "Vertex by Degree",
     layout = layout_preferred,
     vertex.size = degree(g_EL_90, mode = "in"))
plot(g_EL_00,
     main = "2000_SizeDegree",
     edge.width = E(g_EL_00)$width^.5,
     edge.arrow.size = 0.5,
     sub = "Vertex by Degree",
     layout = layout_preferred,
     vertex.size = degree(g_EL_00, mode = "in"))
plot(g_EL_10,
     main = "2010_SizeDegree",
     edge.width = E(g_EL_10)$width^.5,
     edge.arrow.size = 0.5,
     sub = "Vertex by Degree",
     layout = layout_preferred,
     vertex.size = degree(g_EL_10, mode = "in"))
plot(g_EL_20,
     main = "2020_SizeDegree",
     edge.width = E(g_EL_20)$width^.5,
     edge.arrow.size = 0.5,
     sub = "Vertex by Degree",
     layout = layout_preferred,
     vertex.size = degree(g_EL_20, mode = "in"))



# #Sizing by Betweenness
# par(mfrow=c(1,4))
# plot(g_EL_90,
#      main = "2020_SizeBtwn",
#      sub = "Vertex by Btwn",
#      edge.arrow.size = 0.1,
#      layout = layout_preferred,
#      vertex.size = betweenness(g_EL_90))
# plot(g_EL_00,
#      edge.arrow.size = 0.1,
#      main = "2020_SizeBtwn",
#      sub = "Vertex by Btwn",
#      layout = layout_preferred,
#      vertex.size = betweenness(g_EL_00))
# plot(g_EL_10,
#      edge.arrow.size = 0.1,
#      main = "2020_SizeBtwn",
#      sub = "Vertex by Btwn",
#      layout = layout_preferred,
#      vertex.size = betweenness(g_EL_10))
# plot(g_EL_20,
#      main = "2020_SizeBtwn",
#      sub = "Vertex by Btwn",
#      edge.arrow.size = 0.1,
#      layout = layout_preferred,
#      vertex.size = betweenness(g_EL_20))
# par(mfrow=c(1,1))







#### Saving WithUSA Plots to JPEG ####


# SizeDegree
# Open a JPEG device
jpeg("Plot_USA_SizeDegree_per1000.jpg", 
     width = 1500, height = 1500, res = 150)

# Set up a 2x2 plotting area
par(mfrow = c(2, 2))

# Plot each graph
plot(g_EL_90,
     main = "1990_SizeDegree",
     edge.width = E(g_EL_90)$width^.4,
     edge.arrow.size = 0.3,
     sub = "Migrants per 1000: 16.2 \n # edges: 43",
     layout = layout_preferred,
     vertex.size = degree(g_EL_90, mode = "in"))
plot(g_EL_00,
     main = "2000_SizeDegree",
     edge.width = E(g_EL_00)$width^.4,
     edge.arrow.size = 0.3,
     sub = "Migrants per 1000: 22.6 \n # edges: 43",
     layout = layout_preferred,
     vertex.size = degree(g_EL_00, mode = "in"))
plot(g_EL_10,
     main = "2010_SizeDegree",
     edge.width = E(g_EL_10)$width^.4,
     edge.arrow.size = 0.3,
     sub = "Migrants per 1000: 27.6 \n # edges: 51",
     layout = layout_preferred,
     vertex.size = degree(g_EL_10, mode = "in"))
plot(g_EL_20,
     main = "2020_SizeDegree",
     edge.width = E(g_EL_20)$width^.4,
     edge.arrow.size = 0.3,
     sub = "Migrants per 1000: 30.8 \n # edges: 64",
     layout = layout_preferred,
     vertex.size = degree(g_EL_20, mode = "in"))

# Close the JPEG device
dev.off()


par(mfrow=c(1,1))

# 
# #SizeBtwn
# # Open a JPEG device
# jpeg("Plot_USA_SizeBtwn.jpg", 
#      width = 1500, height = 1500, res = 150)
# 
# # Set up a 2x2 plotting area
# par(mfrow = c(2, 2))
# 
# # Plot each graph
# plot(g_EL_90,
#      main = "2020_SizeBtwn",
#      sub = "Vertex by Btwn",
#      edge.arrow.size = 0.1,
#      layout = layout_preferred,
#      vertex.size = betweenness(g_EL_90))
# plot(g_EL_00,
#      edge.arrow.size = 0.1,
#      main = "2020_SizeBtwn",
#      sub = "Vertex by Btwn",
#      layout = layout_preferred,
#      vertex.size = betweenness(g_EL_00))
# plot(g_EL_10,
#      edge.arrow.size = 0.1,
#      main = "2020_SizeBtwn",
#      sub = "Vertex by Btwn",
#      layout = layout_preferred,
#      vertex.size = betweenness(g_EL_10))
# plot(g_EL_20,
#      main = "2020_SizeBtwn",
#      sub = "Vertex by Btwn",
#      edge.arrow.size = 0.1,
#      layout = layout_preferred,
#      vertex.size = betweenness(g_EL_20))
# 
# # Close the JPEG device
# dev.off()
# 
# 
# par(mfrow=c(1,1))




##### Hubs and Authorities ####

# # Run the HITS algorithm
# hits_result <- hits_scores(g_EL_20)
# 
# # Extract and view the hub and authority scores
# hub_scores <- hits_result$hub
# authority_scores <- hits_result$authority
# 
# hub_scores
# authority_scores
# 
# plot(g_EL_20,
#      vertex.size = hub_scores)

########################################################
### Excluding THE UNITED STATES ###
########################################################

# g_EL_20_NUSA <- delete_vertices(g_EL_20, 19)
# g_EL_10_NUSA <- delete_vertices(g_EL_10, 19)
# g_EL_00_NUSA <- delete_vertices(g_EL_00, 19)
# g_EL_90_NUSA <- delete_vertices(g_EL_90, 19)

# Save Data
# save(g_EL_00_NUSA, file = "g_EL_00_NUSA.rda")
# save(g_EL_90_NUSA, file = "g_EL_90_NUSA.rda")
# save(g_EL_10_NUSA, file = "g_EL_10_NUSA.rda")
# save(g_EL_20_NUSA, file = "g_EL_20_NUSA.rda")

#Load Data

load("g_EL_90_NUSA.rda")
load("g_EL_00_NUSA.rda")
load("g_EL_10_NUSA.rda")
load("g_EL_20_NUSA.rda")

#Adjusting vertex names for US and Venezuela so not taking up so much space in middle of network

# V(g_EL_90_NUSA)$name[18] <- "VZ"
# V(g_EL_00_NUSA)$name[18] <- "VZ"
# V(g_EL_10_NUSA)$name[18] <- "VZ"
# V(g_EL_20_NUSA)$name[18] <- "VZ"


### PLOTTING ###
################

layout_preferred_1 <- layout_with_fr(g_EL_20_NUSA)

# Vertex size based on degree
par(mfrow=c(1,4))
plot(g_EL_90_NUSA,
     main = "1990_SizeDegree",
     edge.width = E(g_EL_90_NUSA)$wdith^0.5,
     edge.arrow.size = 0.5,
     sub = "Migrants per 1000: 16.2 \n # edges: 26",
     layout = layout_preferred_1,
     vertex.size = degree(g_EL_90_NUSA, mode = "in")*2)
plot(g_EL_00_NUSA,
     main = "2000_SizeDegree",
     edge.width = E(g_EL_00_NUSA)$width^0.5,
     edge.arrow.size = 0.5,
     sub = "Migrants per 1000: 22.6 \n # edges: 26",
     layout = layout_preferred_1,
     vertex.size = degree(g_EL_00_NUSA, mode = "in")*2)
plot(g_EL_10_NUSA,
     main = "2010_SizeDegree",
     edge.width = E(g_EL_10_NUSA)$width^0.5,
     edge.arrow.size = 0.5,
     sub = "Migrants per 1000: 27.6 \n # edges: 33",
     layout = layout_preferred_1,
     vertex.size = degree(g_EL_10_NUSA, mode = "in")*2)
plot(g_EL_20_NUSA,
     main = "2020_SizeDegree",
     edge.width = E(g_EL_20_NUSA)$width^0.5,
     vertex.label.dist = 1,
     edge.arrow.size = 0.5,
     sub = "Migrants per 1000: 30.8 \n # edges: 46",
     layout = layout_preferred_1,
     vertex.size = degree(g_EL_20_NUSA, mode = "in")*2)

par(mfrow=c(1,1))

# 
# # Vertex size based on betweenness
# 
# par(mfrow=c(1,4))
# plot(g_EL_90_NUSA,
#      main = "1990_SizeDegree",
#      edge.width = E(g_EL_90_NUSA)$wdith^0.5,
#      edge.arrow.size = 0.2,
#      sub = "Vertex by Degree",
#      layout = layout_preferred_1,
#      vertex.size = betweenness(g_EL_90_NUSA))
# plot(g_EL_00_NUSA,
#      main = "2000_SizeDegree",
#      edge.width = E(g_EL_00_NUSA)$width^0.5,
#      edge.arrow.size = 0.2,
#      sub = "Vertex by Degree",
#      layout = layout_preferred_1,
#      vertex.size = betweenness(g_EL_00_NUSA))
# plot(g_EL_10_NUSA,
#      main = "2010_SizeDegree",
#      edge.width = E(g_EL_10_NUSA)$width^0.5,
#      edge.arrow.size = 0.2,
#      sub = "Vertex by Degree",
#      layout = layout_preferred_1,
#      vertex.size = betweenness(g_EL_10_NUSA))
# plot(g_EL_20_NUSA,
#      main = "2020_SizeDegree",
#      edge.width = E(g_EL_20_NUSA)$width^0.5,
#      edge.arrow.size = 0.2,
#      sub = "Vertex by Degree",
#      layout = layout_preferred_1,
#      vertex.size = betweenness(g_EL_20_NUSA))
# par(mfrow=c(1,1))



#####Saving NOUSA plots####

#SizeDegree
# Open a JPEG device
jpeg("Plot_NOUSA_SizeDegree_Per1000.jpg", 
     width = 1500, height = 1500, res = 150)

# Set up a 2x2 plotting area
par(mfrow = c(2, 2))

# Plot each graph
plot(g_EL_90_NUSA,
     main = "1990_SizeDegree",
     edge.width = E(g_EL_90_NUSA)$wdith^0.5,
     edge.arrow.size = 0.5,
     sub = "Migrants per 1000: 16.2 \n # edges: 26",
     layout = layout_preferred_1,
     vertex.size = degree(g_EL_90_NUSA, mode = "in")*2)
plot(g_EL_00_NUSA,
     main = "2000_SizeDegree",
     edge.width = E(g_EL_00_NUSA)$width^0.5,
     edge.arrow.size = 0.5,
     sub = "Migrants per 1000: 22.6 \n # edges: 26",
     layout = layout_preferred_1,
     vertex.size = degree(g_EL_00_NUSA, mode = "in")*2)
plot(g_EL_10_NUSA,
     main = "2010_SizeDegree",
     edge.width = E(g_EL_10_NUSA)$width^0.5,
     edge.arrow.size = 0.5,
     sub = "Migrants per 1000: 27.6 \n # edges: 33",
     layout = layout_preferred_1,
     vertex.size = degree(g_EL_10_NUSA, mode = "in")*2)
plot(g_EL_20_NUSA,
     main = "2020_SizeDegree",
     edge.width = E(g_EL_20_NUSA)$width^0.5,
     vertex.label.dist = 1,
     edge.arrow.size = 0.5,
     sub = "Migrants per 1000: 30.8 \n # edges: 46",
     layout = layout_preferred_1,
     vertex.size = degree(g_EL_20_NUSA, mode = "in")*2)
# Close the JPEG device
dev.off()


par(mfrow=c(1,1))


# #SizeBtwn
# # Open a JPEG device
# jpeg("Plot_NOUSA_SizeBtwn.jpg", 
#      width = 1500, height = 1500, res = 150)
# 
# # Set up a 2x2 plotting area
# par(mfrow = c(2, 2))
# 
# # Plot each graph
# plot(g_90_NUSA,
#      main = "1990_SizeBtwn",
#      edge.arrow.size = 0.5,
#      layout = layout_preferred_1,
#      vertex.size = betweenness(g_90_NUSA))
# plot(g_00_NUSA,
#      edge.arrow.size = 0.5,
#      main = "2000_SizeBtwn",
#      layout = layout_preferred_1,
#      vertex.size = betweenness(g_00_NUSA))
# plot(g_10_NUSA,
#      edge.arrow.size = 0.5,
#      main = "2010_SizeBtwn",
#      layout = layout_preferred_1,
#      vertex.size = betweenness(g_10_NUSA))
# plot(g_20_NUSA,
#      main = "2020_SizeBtwn",
#      edge.arrow.size = 0.5,
#      layout = layout_preferred_1,
#      vertex.size = betweenness(g_20_NUSA))
# 
# # Close the JPEG device
# dev.off()


par(mfrow=c(1,1))

