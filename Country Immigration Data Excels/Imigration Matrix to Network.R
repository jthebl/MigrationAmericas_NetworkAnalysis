
rm(list=ls())

#### Code for creating Immigration Matrices based off of UN data #####
#### Relevant links: https://www.un.org/development/desa/pd/content/international-migrant-stock #####

#### Notes ####

# Data was collected from the UN website. Countries were filtered by those who have a minimum population of 
# 2 million. Parameter for edges is binary with threshold of 10,000 immigrants (i.e. if equal to or greater than
# 10,000 immigrants, assigned a value of 1). The edges also represent net immigration, i.e. subtracting the 
# opposing migration pathways between two countries, with the direction of greater size dictating the edge direction,
# though again, edges are only established if there is a minimum of 10,000 immigrants passing from one country to the 
# other after considering the net migration. Additionally, I added the edge attribute for thickness which is
# associated with this value of net migration divided by the receiving country's population divided by 1000
# (i.e. the units are x immigrants for every 1000 residents).

#####################################################################################################################
#####################################################################################################################








#setting things up####

setwd("C:/Users/heblj/OneDrive - Middlebury College/MIIS_Fall 2024/Intro Network Analysis/Final Project/Country Immigration Data Excels")

library(igraph)

load("m_1990.rda")
load("m_2000.rda")
load("m_2010.rda")
load("m_2020.rda")
load("g_90.rda")
load("g_00.rda")
load("g_10.rda")
load("g_20.rda")
load("g_90_NUSA.rda")
load("g_00_NUSA.rda")
load("g_10_NUSA.rda")
load("g_20_NUSA.rda")

layout_preferred_2 <- layout_with_fr(g_20_NUSA)
layout_preferred <- layout_with_fr(g_20)

################################
#######Descriptive Stats########
################################

#With US

##Density

edge_density(g_90) #0.126
edge_density(g_00) #0.126
edge_density(g_10) #0.149
edge_density(g_20) #0.187


##Degree centralization

centr_degree(g_90)$centralization #0.366
centr_degree(g_00)$centralization #0.366
centr_degree(g_10)$centralization #0.370
centr_degree(g_20)$centralization #0.330


##Betweeness Centralization

centr_betw(g_90)$centralization #0.008
centr_betw(g_00)$centralization #0.016
centr_betw(g_10)$centralization #0.029
centr_betw(g_20)$centralization #0.031



#WithOut US#Wg_90ithOut US

##Density

edge_density(g_90_NUSA) #0.085
edge_density(g_00_NUSA) #0.085
edge_density(g_10_NUSA) #0.108
edge_density(g_20_NUSA) #0.150



##Degree centralization

centr_degree(g_90_NUSA)$centralization #0.066
centr_degree(g_00_NUSA)$centralization #0.159
centr_degree(g_10_NUSA)$centralization #0.166
centr_degree(g_20_NUSA)$centralization #0.183


##Betweenness Centralization

centr_betw(g_90_NUSA)$centralization #0.009
centr_betw(g_00_NUSA)$centralization #0.017
centr_betw(g_10_NUSA)$centralization #0.032
centr_betw(g_20_NUSA)$centralization #0.034





###################################################
##### Interpreting/analyzing hub vs degree OUT ####
###################################################

#With US Data

# Calculate hub scores
hub_scores <- hub_score(g_90)$vector

# Order the hub scores from highest to lowest
ordered_indices <- order(hub_scores, decreasing = TRUE)
ordered_hub_scores <- hub_scores[ordered_indices]

# Print the ordered hub scores
print(ordered_hub_scores)


# Degree OUT
degree_scores <- degree(g_90, mode = "out")

ordered_indices <- order(degree_scores, decreasing = T)
ordered_degree_scores <- degree_scores[ordered_indices]

print(ordered_degree_scores)






#WithOut US

# Calculate hub scores
hub_scores <- hub_score(g_90_NUSA)$vector

# Order the hub scores from highest to lowest
ordered_indices <- order(hub_scores, decreasing = TRUE)
ordered_hub_scores <- hub_scores[ordered_indices]

# Print the ordered hub scores
print(ordered_hub_scores)


# Degree OUT
degree_scores <- degree(g_90_NUSA, mode = "out")

ordered_indices <- order(degree_scores, decreasing = T)
ordered_degree_scores <- degree_scores[ordered_indices]

print(ordered_degree_scores)



########################################################
########################################################
### WITH THE UNITED STATES INCLUDED ####################
########################################################
########################################################



##########################
##### Data from 2020 #####
##########################

# Network from matrix
network_binary_20 <- read.csv("2020_Immigration Matrix_Binary_simplified.csv",
                    header=T,
                    row.names=1,
                    check.names = F)


#Graphing from Matrix
m_20 <- as.matrix(network_binary_20)

g_20 <- graph_from_adjacency_matrix(m_20,
                                 mode = "directed",
                                 weighted = NULL)


#Plots to assess
plot(g_20,
     main = "2020_SizeDegree",
     sub = "Vertex by Degree",
     layout = layout_preferred,
     edge.arrow.size = 0.2,
     vertex.size = degree(g_20))


plot(g_20,
     main = "2020_SizeBtwn",
     sub = "Vertex by Btwn",
     edge.arrow.size = 0.2,
     layout = layout_preferred,
     vertex.size = betweenness(g_20))





##########################
##### Data from 2010 #####
##########################

network_binary_10 <- read.csv("2010_Immigration Matrix_Binary_simplified.csv",
                       header=T,
                       row.names=1,
                       check.names = F)



#Graphing from matrix
m_10 <- as.matrix(network_binary_10)


g_10 <- graph_from_adjacency_matrix(m_10,
                                    mode = "directed",
                                    weighted = NULL)



plot(g_10,
     main = "2010_SizeDegree",
     edge.arrow.size = 0.2,
     sub = "Vertex by Degree",
     layout = layout_preferred,
     vertex.size = degree(g_10))


plot(g_10,
     edge.arrow.size = 0.2,
     main = "2010_SizeBtwn",
     sub = "Vertex by Btwn",
     layout = layout_preferred,
     vertex.size = betweenness(g_10))







##########################
##### Data from 2000 #####
##########################

# Network from matrix
network_binary_00 <- read.csv("2000_Immigration Matrix_Binary_simplified.csv",
                       header=T,
                       row.names=1,
                       check.names = F)



#Graphing from matrix
m_00 <- as.matrix(network_binary_00)


g_00 <- graph_from_adjacency_matrix(m_00,
                                    mode = "directed",
                                    weighted = NULL)


plot(g_00,
     main = "2000_SizeDegree",
     edge.arrow.size = 0.2,
     sub = "Vertex by Degree",
     layout = layout_preferred,
     vertex.size = degree(g_00))


plot(g_00,
     edge.arrow.size = 0.2,
     main = "2000_SizeBtwn",
     sub = "Vertex by Btwn",
     layout = layout_preferred,
     vertex.size = betweenness(g_00))











##########################
##### Data from 1990 #####
##########################

#Network from Matrix
network_binary_90 <- read.csv("1990_Immigration Matrix_Binary_simplified.csv",
                     header=T,
                     row.names=1,
                    check.names = F)


# #Graph from matrix
 m_90 <- as.matrix(network_binary_90)

 
 g_90 <- graph_from_adjacency_matrix(m_90,
                                 mode = "directed",
                                 weighted = NULL)


plot(g_90,
     main = "1990_SizeDegree",
     edge.arrow.size = 0.2,
     sub = "Vertex by Degree",
     layout = layout_preferred,
     vertex.size = degree(g_90, mode = "in"))


plot(g_90,
     main = "2020_SizeBtwn",
     sub = "Vertex by Btwn",
     edge.arrow.size = 0.2,
     layout = layout_preferred,
     vertex.size = betweenness(g_90))










################################################
#######Adding Attributes and modifications #####
################################################

#Adjusting Country names ####


#Adjusting vertex names so not taking up so much space in network

#for 1990
V(g_90)$name[1] <- "Arg"
V(g_90)$name[2] <- "Bol"
V(g_90)$name[3] <- "Braz"
V(g_90)$name[4] <- "Can"
V(g_90)$name[6] <- "Col"
V(g_90)$name[7] <- "CR"
V(g_90)$name[8] <- "Ecu"
V(g_90)$name[9] <- "ElSal"
V(g_90)$name[10] <- "Guat"
V(g_90)$name[11] <- "Hond"
V(g_90)$name[12] <- "Mex"
V(g_90)$name[13] <- "NG"
V(g_90)$name[14] <- "Pan"
V(g_90)$name[15] <- "PG"
V(g_90)$name[17] <- "US"
V(g_90)$name[18] <- "UG"
V(g_90)$name[19] <- "VZ"

#for 2000
V(g_00)$name[1] <- "Arg"
V(g_00)$name[2] <- "Bol"
V(g_00)$name[3] <- "Braz"
V(g_00)$name[4] <- "Can"
V(g_00)$name[6] <- "Col"
V(g_00)$name[7] <- "CR"
V(g_00)$name[8] <- "Ecu"
V(g_00)$name[9] <- "ElSal"
V(g_00)$name[10] <- "Guat"
V(g_00)$name[11] <- "Hond"
V(g_00)$name[12] <- "Mex"
V(g_00)$name[13] <- "NG"
V(g_00)$name[14] <- "Pan"
V(g_00)$name[15] <- "PG"
V(g_00)$name[17] <- "US"
V(g_00)$name[18] <- "UG"
V(g_00)$name[19] <- "VZ"

#for 2010
V(g_10)$name[1] <- "Arg"
V(g_10)$name[2] <- "Bol"
V(g_10)$name[3] <- "Braz"
V(g_10)$name[4] <- "Can"
V(g_10)$name[6] <- "Col"
V(g_10)$name[7] <- "CR"
V(g_10)$name[8] <- "Ecu"
V(g_10)$name[9] <- "ElSal"
V(g_10)$name[10] <- "Guat"
V(g_10)$name[11] <- "Hond"
V(g_10)$name[12] <- "Mex"
V(g_10)$name[13] <- "NG"
V(g_10)$name[14] <- "Pan"
V(g_10)$name[15] <- "PG"
V(g_10)$name[17] <- "US"
V(g_10)$name[18] <- "UG"
V(g_10)$name[19] <- "VZ"

#for 2020
V(g_20)$name[1] <- "Arg"
V(g_20)$name[2] <- "Bol"
V(g_20)$name[3] <- "Braz"
V(g_20)$name[4] <- "Can"
V(g_20)$name[6] <- "Col"
V(g_20)$name[7] <- "CR"
V(g_20)$name[8] <- "Ecu"
V(g_20)$name[9] <- "ElSal"
V(g_20)$name[10] <- "Guat"
V(g_20)$name[11] <- "Hond"
V(g_20)$name[12] <- "Mex"
V(g_20)$name[13] <- "NG"
V(g_20)$name[14] <- "Pan"
V(g_20)$name[15] <- "PG"
V(g_20)$name[17] <- "US"
V(g_20)$name[18] <- "UG"
V(g_20)$name[19] <- "VZ"




#Adding Degree as vertex attribute####

###Degree Total ####
# Assuming you have four graphs g1, g2, g3, g4
graphs_td <- list(g_90, g_00, g_10, g_20)
#
## Calculate degree scores for each graph
degree_scores_td <- lapply(graphs_td, degree)


V(g_90)$DegT <- degree_scores_td[[1]]
V(g_00)$DegT <- degree_scores_td[[2]]
V(g_10)$DegT <- degree_scores_td[[3]]
V(g_20)$DegT <- degree_scores_td[[4]]


###Degree IN ####
# Assuming you have four graphs g1, g2, g3, g4
graphs_di <- list(g_90, g_00, g_10, g_20)
#
## Calculate degree scores for each graph
degree_scores_di <- lapply(graphs_di, degree, mode = "in")


V(g_90)$DegI <- degree_scores_di[[1]]
V(g_00)$DegI <- degree_scores_di[[2]]
V(g_10)$DegI <- degree_scores_di[[3]]
V(g_20)$DegI <- degree_scores_di[[4]]



###Degree OUT ####
# Assuming you have four graphs g1, g2, g3, g4
graphs_do <- list(g_90, g_00, g_10, g_20)
#
## Calculate degree scores for each graph
degree_scores_do <- lapply(graphs_do, degree, mode = "out")


V(g_90)$DegO <- degree_scores_do[[1]]
V(g_00)$DegO <- degree_scores_do[[2]]
V(g_10)$DegO <- degree_scores_do[[3]]
V(g_20)$DegO <- degree_scores_do[[4]]



#Creating Hub and Auth Scores####

######Hubs and Auth for 1990####

#Extract and view the hub and authority scores
hub_g_90 <- hits_scores(g_90)$hub
auth_g_90 <- hits_scores(g_90)$authority

 
######Hubs and Auth for 2000####
hub_g_00 <- hits_scores(g_00)$hub
auth_g_00 <- hits_scores(g_00)$authority
 
 
######Hubs and Auth for 2010####
hub_g_10 <- hits_scores(g_10)$hub
auth_g_10 <- hits_scores(g_10)$authority
 
 
######Hubs and Auth for 2020####
hub_g_20 <- hits_scores(g_20)$hub
auth_g_20 <- hits_scores(g_20)$authority

###Add Hubs and Auth as Vertex Attributes ####

#Add hubs as vertex attribute
##since scores from 0 to 1, need to times 10 to get reasonable value for attribute use
##and add 1 so that the lowest hub and auth score is not 1, which would affect atrribute use
V(g_90)$Hub <- round(hub_g_90*10+1, 0)
V(g_00)$Hub <- round(hub_g_00*10+1, 0)
V(g_10)$Hub <- round(hub_g_10*10+1, 0)
V(g_20)$Hub <- round(hub_g_20*10+1, 0)

#Add auth as vertex attribute
V(g_90)$Auth <- round(auth_g_90*10+1, 0)
V(g_00)$Auth <- round(auth_g_00*10+1, 0)
V(g_10)$Auth <- round(auth_g_10*10+1, 0)
V(g_20)$Auth <- round(auth_g_20*10+1, 0)


#Create color attributes based on DegT, DegI, DEGO, Hub, Auth####


######Color for DegTotal####
# # Find the maximum degree score for each vertex across all networks
max_degree_value_td <- max(unlist(degree_scores_td))
#
# # Create a color palette from yellow to red
color_palette_degree_td <- colorRampPalette(c("yellow", "red"))(max_degree_value_td + 1)

V(g_90)$ColorDT <- color_palette_degree_td[degree(g_90)]
V(g_00)$ColorDT <- color_palette_degree_td[degree(g_00)]
V(g_10)$ColorDT <- color_palette_degree_td[degree(g_10)]
V(g_20)$ColorDT <- color_palette_degree_td[degree(g_20)]


######Color for DegIn####
# # Find the maximum degree score for each vertex across all networks
max_degree_value_di <- max(unlist(degree_scores_di))
#
# # Create a color palette from yellow to red
color_palette_degree_di <- colorRampPalette(c("yellow", "red"))(max_degree_value_di + 1)

V(g_90)$ColorDI <- color_palette_degree_di[degree(g_90)]
V(g_00)$ColorDI <- color_palette_degree_di[degree(g_00)]
V(g_10)$ColorDI <- color_palette_degree_di[degree(g_10)]
V(g_20)$ColorDI <- color_palette_degree_di[degree(g_20)]


######Color for DegOut####
# # Find the maximum degree score for each vertex across all networks
max_degree_value_do <- max(unlist(degree_scores_do))
#
# # Create a color palette from yellow to red
color_palette_degree_do <- colorRampPalette(c("yellow", "red"))(max_degree_value_do + 1)

V(g_90)$ColorDO <- color_palette_degree_do[degree(g_90)]
V(g_00)$ColorDO <- color_palette_degree_do[degree(g_00)]
V(g_10)$ColorDO <- color_palette_degree_do[degree(g_10)]
V(g_20)$ColorDO <- color_palette_degree_do[degree(g_20)]



######Color for Hub####

# Create a color palette from yellow to red; using 121 as max value and squaring all
# the hub and auth scores to great greater gradation in the coloring, i.e. make more
# obvious differences in scores via the associated vertex colors. 

color_palette_hub <- colorRampPalette(c("yellow", "red"))(121)

V(g_90)$ColorHub <- color_palette_hub[V(g_90)$Hub^2]
V(g_00)$ColorHub <- color_palette_hub[V(g_00)$Hub^2]
V(g_10)$ColorHub <- color_palette_hub[V(g_10)$Hub^2]
V(g_20)$ColorHub <- color_palette_hub[V(g_20)$Hub^2]

######Color for Auth####

# Create a color palette from yellow to red; using 121 as max value and squaring all
# the hub and auth scores to great greater gradation in the coloring, i.e. make more
# obvious differences in scores via the associated vertex colors. 

color_palette_auth <- colorRampPalette(c("yellow", "red"))(121)

V(g_90)$ColorAuth <- color_palette_auth[V(g_90)$Auth^2]
V(g_00)$ColorAuth <- color_palette_auth[V(g_00)$Auth^2]
V(g_10)$ColorAuth <- color_palette_auth[V(g_10)$Auth^2]
V(g_20)$ColorAuth <- color_palette_auth[V(g_20)$Auth^2]

#Adding edge attribute, i.e. thickness based on weight as of migration flow####

######For 1990####
# # Network from EdgeList
EL_Network_90 <- read.csv("1990_edgelist_Per1000.csv", header = T)
#
# # Create a graph from the edgelist
g_EL_90 <- graph_from_data_frame(EL_Network_90, directed = T)
#
# # Scale the edge thickness based on the values
# # You can adjust the scaling factor as needed
E(g_90)$width <- EL_Network_90$Weight


######For 2000####
# Network from EdgeList
EL_Network_00 <- read.csv("2000_edgelist_Per1000.csv", header = T)



# Create a graph from the edgelist
g_EL_00 <- graph_from_data_frame(EL_Network_00, directed = T)

# Scale the edge thickness based on the values
# You can adjust the scaling factor as needed
E(g_00)$width <- EL_Network_00$Weight



######For 2010####

# Network from EdgeList
EL_Network_10 <- read.csv("2010_edgelist_Per1000.csv", header = T)


# Create a graph from the edgelist
g_EL_10 <- graph_from_data_frame(EL_Network_10, directed = T)

# Scale the edge thickness based on the values
# You can adjust the scaling factor as needed
E(g_10)$width <- EL_Network_10$Weight



######For 2020####
EL_Network_20 <- read.csv("2020_edgelist_Per1000.csv", header = T)


# Create a graph from the edgelist
g_EL_20 <- graph_from_data_frame(EL_Network_20, directed = T)


# Scale the edge thickness based on the values
E(g_20)$width <- EL_Network_20$Weight


##############################
#Plotting and Saving Jpegs####
##############################

#Set layout for all networks/jpegs for easy comparison

layout_preferred <- layout_with_fr(g_20)

###Degree Total####

jpeg("Plot_USA_SizeDegreeTotal.jpg", 
     width = 1500, height = 1500, res = 150)

# Set up a 2x2 plotting area
par(mfrow = c(2, 2), mar = c(1, 0, 2, 0))


# Plot each graph
plot(g_90,
     main = "1990 \n Size & Color = Degree Total",
     edge.arrow.size = 0.5,
     edge.width = 1,
     vertex.label.cex = .9,
     layout = layout_preferred,
     vertex.size = V(g_90)$DegT*1.5,
     vertex.color = V(g_90)$ColorDT,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_00,
     main = "2000 \n Size & Color = Degree Total",
     edge.arrow.size = 0.5,
     edge.width = 1,
     vertex.label.cex = .9,
     layout = layout_preferred,
     vertex.size = V(g_00)$DegT*1.5,
     vertex.color = V(g_00)$ColorDT,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_10,
     main = "2010 \n Size & Color = Degree Total",
     edge.arrow.size = 0.5,
     edge.width = 1,
     vertex.label.cex = .9,
     layout = layout_preferred,
     vertex.size = V(g_10)$DegT*1.5,
     vertex.color = V(g_10)$ColorDT,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_20,
     main = "2020 \n Size & Color = Degree Total",
     edge.arrow.size = 0.5,
     edge.width = 1,
     vertex.label.cex = .9,
     layout = layout_preferred,
     vertex.size = V(g_20)$DegT*1.5,
     vertex.color = V(g_20)$ColorDT,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

# Close the JPEG device
dev.off()


par(mfrow=c(1,1))



######With Edge Attribute####


jpeg("Plot_USA_SizeDegreeTotal_Thick.jpg", 
     width = 1500, height = 1500, res = 150)

# Set up a 2x2 plotting area
par(mfrow = c(2, 2), mar = c(1, 0, 2, 0))

# Plot each graph
plot(g_90,
     main = "1990 \n Size & Color = Degree Total",
     edge.arrow.size = 1,
     edge.width = E(g_90)$width/4,
     vertex.label.cex = .9,
     layout = layout_preferred,
     vertex.size = V(g_90)$DegT,
     vertex.color = V(g_90)$ColorDT,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_00,
     main = "2000 \n Size & Color = Degree Total",
     edge.arrow.size = 1,
     edge.width = E(g_00)$width/4,
     vertex.label.cex = .9,
     layout = layout_preferred,
     vertex.size = V(g_00)$DegT,
     vertex.color = V(g_00)$ColorDT,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_10,
     main = "2010 \n Size & Color = Degree Total",
     edge.arrow.size = 1,
     edge.width = E(g_10)$width/4,
     vertex.label.cex = .9,
     layout = layout_preferred,
     vertex.size = V(g_10)$DegT,
     vertex.color = V(g_10)$ColorDT,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_20,
     main = "2020 \n Size & Color = Degree Total",
     edge.arrow.size = 1,
     edge.width = E(g_20)$width/4,
     vertex.label.cex = .9,
     layout = layout_preferred,
     vertex.size = V(g_20)$DegT,
     vertex.color = V(g_20)$ColorDT,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

# Close the JPEG device
dev.off()


par(mfrow=c(1,1))


###Degree IN####

jpeg("Plot_USA_SizeDegreeIN.jpg", 
     width = 1500, height = 1500, res = 150)

# Set up a 2x2 plotting area
par(mfrow = c(2, 2), mar = c(1, 0, 2, 0))


# Plot each graph
plot(g_90,
     main = "1990 \n Size & Color = Degree IN",
     edge.arrow.size = 0.5,
     edge.width = 1,
     vertex.label.cex = .9,
     layout = layout_preferred,
     vertex.size = V(g_90)$DegI*1.5,
     vertex.color = V(g_90)$ColorDI,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_00,
     main = "2000 \n Size & Color = Degree IN",
     edge.arrow.size = 0.5,
     edge.width = 1,
     vertex.label.cex = .9,
     layout = layout_preferred,
     vertex.size = V(g_00)$DegI*1.5,
     vertex.color = V(g_00)$ColorDI,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_10,
     main = "2010 \n Size & Color = Degree IN",
     edge.arrow.size = 0.5,
     edge.width = 1,
     vertex.label.cex = .9,
     layout = layout_preferred,
     vertex.size = V(g_10)$DegI*1.5,
     vertex.color = V(g_10)$ColorDI,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_20,
     main = "2020 \n Size & Color = Degree IN",
     edge.arrow.size = 0.5,
     edge.width = 1,
     vertex.label.cex = .9,
     layout = layout_preferred,
     vertex.size = V(g_20)$DegI*1.5,
     vertex.color = V(g_20)$ColorDI,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

# Close the JPEG device
dev.off()


par(mfrow=c(1,1))


######With Edge Attribute####

jpeg("Plot_USA_SizeDegreeIN_Thick.jpg", 
     width = 1500, height = 1500, res = 150)

# Set up a 2x2 plotting area
par(mfrow = c(2, 2), mar = c(1, 0, 2, 0))

# Plot each graph
plot(g_90,
     main = "1990 \n Size & Color = Degree IN",
     edge.arrow.size = 1,
     edge.width = E(g_90)$width/4,
     vertex.label.cex = .9,
     layout = layout_preferred,
     vertex.size = V(g_90)$DegI,
     vertex.color = V(g_90)$ColorDI,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_00,
     main = "2000 \n Size & Color = Degree IN",
     edge.arrow.size = 1,
     edge.width = E(g_00)$width/4,
     vertex.label.cex = .9,
     layout = layout_preferred,
     vertex.size = V(g_00)$DegI,
     vertex.color = V(g_00)$ColorDI,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_10,
     main = "2010 \n Size & Color = Degree IN",
     edge.arrow.size = 1,
     edge.width = E(g_10)$width/4,
     vertex.label.cex = .9,
     layout = layout_preferred,
     vertex.size = V(g_10)$DegI,
     vertex.color = V(g_10)$ColorDI,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_20,
     main = "2020 \n Size & Color = Degree IN",
     edge.arrow.size = 1,
     edge.width = E(g_20)$width/4,
     vertex.label.cex = .9,
     layout = layout_preferred,
     vertex.size = V(g_20)$DegI,
     vertex.color = V(g_20)$ColorDI,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

# Close the JPEG device
dev.off()


par(mfrow=c(1,1))


###Degree Out####

jpeg("Plot_USA_SizeDegreeOUT.jpg", 
     width = 1500, height = 1500, res = 150)

# Set up a 2x2 plotting area
par(mfrow = c(2, 2), mar = c(1, 0, 2, 0))


# Plot each graph
plot(g_90,
     main = "1990 \n Size & Color = Degree OUT",
     edge.arrow.size = 0.5,
     edge.width = 1,
     vertex.label.cex = .9,
     layout = layout_preferred,
     vertex.size = V(g_90)$DegO*2,
     vertex.color = V(g_90)$ColorDO,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_00,
     main = "2000 \n Size & Color = Degree OUT",
     edge.arrow.size = 0.5,
     edge.width = 1,
     vertex.label.cex = .9,
     layout = layout_preferred,
     vertex.size = V(g_00)$DegO*2,
     vertex.color = V(g_00)$ColorDO,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_10,
     main = "2010 \n Size & Color = Degree OUT",
     edge.arrow.size = 0.5,
     edge.width = 1,
     vertex.label.cex = .9,
     layout = layout_preferred,
     vertex.size = V(g_10)$DegO*2,
     vertex.color = V(g_10)$ColorDO,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_20,
     main = "2020 \n Size & Color = Degree OUT",
     edge.arrow.size = 0.5,
     edge.width = 1,
     vertex.label.cex = .9,
     layout = layout_preferred,
     vertex.size = V(g_20)$DegO*2,
     vertex.color = V(g_20)$ColorDO,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

# Close the JPEG device
dev.off()


par(mfrow=c(1,1))


######With Edge Attribute####

jpeg("Plot_USA_SizeDegreeOUT_Thick.jpg", 
     width = 1500, height = 1500, res = 150)

# Set up a 2x2 plotting area
par(mfrow = c(2, 2), mar = c(1, 0, 2, 0))

# Plot each graph
plot(g_90,
     main = "1990 \n Size & Color = Degree OUT",
     edge.arrow.size = 1,
     edge.width = E(g_90)$width/4,
     vertex.label.cex = .9,
     layout = layout_preferred,
     vertex.size = V(g_90)$DegO,
     vertex.color = V(g_90)$ColorDO,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_00,
     main = "2000 \n Size & Color = Degree OUT",
     edge.arrow.size = 1,
     edge.width = E(g_00)$width/4,
     vertex.label.cex = .9,
     layout = layout_preferred,
     vertex.size = V(g_00)$DegO,
     vertex.color = V(g_00)$ColorDO,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_10,
     main = "2010 \n Size & Color = Degree OUT",
     edge.arrow.size = 1,
     edge.width = E(g_10)$width/4,
     vertex.label.cex = .9,
     layout = layout_preferred,
     vertex.size = V(g_10)$DegO,
     vertex.color = V(g_10)$ColorDO,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_20,
     main = "2020 \n Size & Color = Degree OUT",
     edge.arrow.size = 1,
     edge.width = E(g_20)$width/4,
     vertex.label.cex = .9,
     layout = layout_preferred,
     vertex.size = V(g_20)$DegO,
     vertex.color = V(g_20)$ColorDO,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

# Close the JPEG device
dev.off()


par(mfrow=c(1,1))


###Hub####

jpeg("Plot_USA_SizeHub.jpg", 
     width = 1500, height = 1500, res = 150)

# Set up a 2x2 plotting area
par(mfrow = c(2, 2), mar = c(1, 0, 2, 0))


# Plot each graph
plot(g_90,
     main = "1990 \n Size & Color = HUB",
     edge.arrow.size = 0.5,
     edge.width = 1,
     vertex.label.cex = .9,
     layout = layout_preferred,
     vertex.size = V(g_90)$Hub*2,
     vertex.color = V(g_90)$ColorHub,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_00,
     main = "2000 \n Size & Color = HUB",
     edge.arrow.size = 0.5,
     edge.width = 1,
     vertex.label.cex = .9,
     layout = layout_preferred,
     vertex.size = V(g_00)$Hub*2,
     vertex.color = V(g_00)$ColorHub,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_10,
     main = "2010 \n Size & Color = HUB",
     edge.arrow.size = 0.5,
     edge.width = 1,
     vertex.label.cex = .9,
     layout = layout_preferred,
     vertex.size = V(g_10)$Hub*2,
     vertex.color = V(g_10)$ColorHub,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_20,
     main = "2020 \n Size & Color = HUB",
     edge.arrow.size = 0.5,
     edge.width = 1,
     vertex.label.cex = .9,
     layout = layout_preferred,
     vertex.size = V(g_20)$Hub*2,
     vertex.color = V(g_20)$ColorHub,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

# Close the JPEG device
dev.off()


par(mfrow=c(1,1))

######With Edge Attribute####

jpeg("Plot_USA_SizeHub_Thick.jpg", 
     width = 1500, height = 1500, res = 150)

# Set up a 2x2 plotting area
par(mfrow = c(2, 2), mar = c(1, 0, 2, 0))


# Plot each graph
plot(g_90,
     main = "1990 \n Size & Color = HUB",
     edge.arrow.size = 1,
     edge.width = E(g_90)$width/4,
     vertex.label.cex = .9,
     layout = layout_preferred,
     vertex.size = V(g_90)$Hub*2,
     vertex.color = V(g_90)$ColorHub,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_00,
     main = "2000 \n Size & Color = HUB",
     edge.arrow.size = 1,
     edge.width = E(g_00)$width/4,
     vertex.label.cex = .9,
     layout = layout_preferred,
     vertex.size = V(g_00)$Hub*2,
     vertex.color = V(g_00)$ColorHub,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_10,
     main = "2010 \n Size & Color = HUB",
     edge.arrow.size = 1,
     edge.width = E(g_10)$width/4,
     vertex.label.cex = .9,
     layout = layout_preferred,
     vertex.size = V(g_10)$Hub*2,
     vertex.color = V(g_10)$ColorHub,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_20,
     main = "2020 \n Size & Color = HUB",
     edge.arrow.size = 1,
     edge.width = E(g_20)$width/4,
     vertex.label.cex = .9,
     layout = layout_preferred,
     vertex.size = V(g_20)$Hub*2,
     vertex.color = V(g_20)$ColorHub,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

# Close the JPEG device
dev.off()


par(mfrow=c(1,1))


###Authority####

jpeg("Plot_USA_SizeAuth.jpg", 
     width = 1500, height = 1500, res = 150)

# Set up a 2x2 plotting area
par(mfrow = c(2, 2), mar = c(1, 0, 2, 0))


# Plot each graph
plot(g_90,
     main = "1990 \n Size & Color = Auth",
     edge.arrow.size = 0.5,
     edge.width = 1,
     vertex.label.cex = .9,
     layout = layout_preferred,
     vertex.size = V(g_90)$Auth*2,
     vertex.color = V(g_90)$ColorAuth,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_00,
     main = "2000 \n Size & Color = Auth",
     edge.arrow.size = 0.5,
     edge.width = 1,
     vertex.label.cex = .9,
     layout = layout_preferred,
     vertex.size = V(g_00)$Auth*2,
     vertex.color = V(g_00)$ColorAuth,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_10,
     main = "2010 \n Size & Color = Auth",
     edge.arrow.size = 0.5,
     edge.width = 1,
     vertex.label.cex = .9,
     layout = layout_preferred,
     vertex.size = V(g_10)$Auth*2,
     vertex.color = V(g_10)$ColorAuth,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_20,
     main = "2020 \n Size & Color = Auth",
     edge.arrow.size = 0.5,
     edge.width = 1,
     vertex.label.cex = .9,
     layout = layout_preferred,
     vertex.size = V(g_20)$Auth*2,
     vertex.color = V(g_20)$ColorAuth,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

# Close the JPEG device
dev.off()


par(mfrow=c(1,1))


######With Edge Attribute####


jpeg("Plot_USA_SizeAuth_Thick.jpg", 
     width = 1500, height = 1500, res = 150)

# Set up a 2x2 plotting area
par(mfrow = c(2, 2), mar = c(1, 0, 2, 0))


# Plot each graph
plot(g_90,
     main = "1990 \n Size & Color = Auth",
     edge.arrow.size = 1,
     edge.width = E(g_90)$width/4,
     vertex.label.cex = .9,
     layout = layout_preferred,
     vertex.size = V(g_90)$Auth*2,
     vertex.color = V(g_90)$ColorAuth,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_00,
     main = "2000 \n Size & Color = Auth",
     edge.arrow.size = 1,
     edge.width = E(g_00)$width/4,
     vertex.label.cex = .9,
     layout = layout_preferred,
     vertex.size = V(g_00)$Auth*2,
     vertex.color = V(g_00)$ColorAuth,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_10,
     main = "2010 \n Size & Color = Auth",
     edge.arrow.size = 1,
     edge.width = E(g_10)$width/4,
     vertex.label.cex = .9,
     layout = layout_preferred,
     vertex.size = V(g_10)$Auth*2,
     vertex.color = V(g_10)$ColorAuth,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_20,
     main = "2020 \n Size & Color = Auth",
     edge.arrow.size = 1,
     edge.width = E(g_20)$width/4,
     vertex.label.cex = .9,
     layout = layout_preferred,
     vertex.size = V(g_20)$Auth*2,
     vertex.color = V(g_20)$ColorAuth,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

# Close the JPEG device
dev.off()


par(mfrow=c(1,1))




########################################################
########################################################
### WITHOUT THE UNITED STATES INCLUDED #################
########################################################
########################################################


#Remove the US from all four networks####

# g_20_NUSA <- delete_vertices(g_20, 17)
# g_10_NUSA <- delete_vertices(g_10, 17)
# g_00_NUSA <- delete_vertices(g_00, 17)
# g_90_NUSA <- delete_vertices(g_90, 17)


#Adding Degree as vertex attribute####

###Degree Total ####
# Assuming you have four graphs g1, g2, g3, g4
graphs_td_NUSA <- list(g_90_NUSA, g_00_NUSA, g_10_NUSA, g_20_NUSA)
#
## Calculate degree scores for each graph
degree_scores_td_NUSA <- lapply(graphs_td_NUSA, degree)


V(g_90_NUSA)$DegT  <- degree_scores_td_NUSA[[1]]
V(g_00_NUSA)$DegT  <- degree_scores_td_NUSA[[2]]
V(g_10_NUSA)$DegT  <- degree_scores_td_NUSA[[3]]
V(g_20_NUSA)$DegT  <- degree_scores_td_NUSA[[4]]


###Degree IN ####
# Assuming you have four graphs g1, g2, g3, g4
graphs_di_NUSA <- list(g_90_NUSA, g_00_NUSA, g_10_NUSA, g_20_NUSA)
#
## Calculate degree scores for each graph
degree_scores_di_NUSA <- lapply(graphs_di_NUSA, degree, mode = "in")


V(g_90_NUSA)$DegI  <- degree_scores_di_NUSA[[1]]
V(g_00_NUSA)$DegI  <- degree_scores_di_NUSA[[2]]
V(g_10_NUSA)$DegI  <- degree_scores_di_NUSA[[3]]
V(g_20_NUSA)$DegI  <- degree_scores_di_NUSA[[4]]



###Degree OUT ####
# Assuming you have four graphs g1, g2, g3, g4
graphs_do_NUSA <- list(g_90_NUSA, g_00_NUSA, g_10_NUSA, g_20_NUSA)
#
## Calculate degree scores for each graph
degree_scores_do_NUSA <- lapply(graphs_do_NUSA, degree, mode = "out")


V(g_90_NUSA)$DegO  <- degree_scores_do_NUSA[[1]]
V(g_00_NUSA)$DegO  <- degree_scores_do_NUSA[[2]]
V(g_10_NUSA)$DegO  <- degree_scores_do_NUSA[[3]]
V(g_20_NUSA)$DegO  <- degree_scores_do_NUSA[[4]]



#Creating Hub and Auth Scores####

######Hubs and Auth for 1990####

#Extract and view the hub and authority scores
hub_g_90_NUSA <- hits_scores(g_90_NUSA)$hub
auth_g_90_NUSA <- hits_scores(g_90_NUSA)$authority


######Hubs and Auth for 2000####
hub_g_00_NUSA <- hits_scores(g_00_NUSA)$hub
auth_g_00_NUSA <- hits_scores(g_00_NUSA)$authority


######Hubs and Auth for 2010####
hub_g_10_NUSA <- hits_scores(g_10_NUSA)$hub
auth_g_10_NUSA <- hits_scores(g_10_NUSA)$authority


######Hubs and Auth for 2020####
hub_g_20_NUSA <- hits_scores(g_20_NUSA)$hub
auth_g_20_NUSA <- hits_scores(g_20_NUSA)$authority

###Add Hubs and Auth as Vertex Attributes ####

#Add hubs as vertex attribute
##since scores from 0 to 1, need to times 10 to get reasonable value for attribute use
##and add 1 so that the lowest hub and auth score is not 1, which would affect atrribute use
V(g_90_NUSA)$Hub  <- round(hub_g_90_NUSA*10+1, 0)
V(g_00_NUSA)$Hub  <- round(hub_g_00_NUSA*10+1, 0)
V(g_10_NUSA)$Hub  <- round(hub_g_10_NUSA*10+1, 0)
V(g_20_NUSA)$Hub  <- round(hub_g_20_NUSA*10+1, 0)

#Add auth as vertex attribute
V(g_90_NUSA)$Auth  <- round(auth_g_90_NUSA*10+1, 0)
V(g_00_NUSA)$Auth  <- round(auth_g_00_NUSA*10+1, 0)
V(g_10_NUSA)$Auth  <- round(auth_g_10_NUSA*10+1, 0)
V(g_20_NUSA)$Auth  <- round(auth_g_20_NUSA*10+1, 0)


#Create color attributes based on DegT, DegI, DEGO, Hub, Auth####


######Color for DegTotal####
# # Find the maximum degree score for each vertex across all networks
max_degree_value_td_NUSA <- max(unlist(degree_scores_td_NUSA))
#
# # Create a color palette from yellow to red
color_palette_degree_td_NUSA <- colorRampPalette(c("yellow", "red"))(max_degree_value_td_NUSA + 1)

V(g_90_NUSA)$ColorDT  <- color_palette_degree_td_NUSA[degree(g_90_NUSA)+1] #Add 1 since Honduras has no edges for 2000 and 2010 and throws an error
V(g_00_NUSA)$ColorDT  <- color_palette_degree_td_NUSA[degree(g_00_NUSA)+1]
V(g_10_NUSA)$ColorDT  <- color_palette_degree_td_NUSA[degree(g_10_NUSA)+1]
V(g_20_NUSA)$ColorDT  <- color_palette_degree_td_NUSA[degree(g_20_NUSA)+1]


######Color for DegIn####
# # Find the maximum degree score for each vertex across all networks
max_degree_value_di_NUSA <- max(unlist(degree_scores_di_NUSA))
#
# # Create a color palette from yellow to red
color_palette_degree_di_NUSA <- colorRampPalette(c("yellow", "red"))(max_degree_value_di_NUSA + 1)

V(g_90_NUSA)$ColorDI  <- color_palette_degree_di_NUSA[degree(g_90_NUSA)+1]
V(g_00_NUSA)$ColorDI  <- color_palette_degree_di_NUSA[degree(g_00_NUSA)+1]
V(g_10_NUSA)$ColorDI  <- color_palette_degree_di_NUSA[degree(g_10_NUSA)+1]
V(g_20_NUSA)$ColorDI  <- color_palette_degree_di_NUSA[degree(g_20_NUSA)+1]


######Color for DegOut####
# # Find the maximum degree score for each vertex across all networks
max_degree_value_do_NUSA <- max(unlist(degree_scores_do_NUSA))
#
# # Create a color palette from yellow to red
color_palette_degree_do_NUSA <- colorRampPalette(c("yellow", "red"))(max_degree_value_do_NUSA + 1)

V(g_90_NUSA)$ColorDO  <- color_palette_degree_do[degree(g_90_NUSA)+1]
V(g_00_NUSA)$ColorDO  <- color_palette_degree_do[degree(g_00_NUSA)+1]
V(g_10_NUSA)$ColorDO  <- color_palette_degree_do[degree(g_10_NUSA)+1]
V(g_20_NUSA)$ColorDO  <- color_palette_degree_do[degree(g_20_NUSA)+1]



######Color for Hub####

# Create a color palette from yellow to red; using 121 as max value and squaring all
# the hub and auth scores to great greater gradation in the coloring, i.e. make more
# obvious differences in scores via the associated vertex colors. 

color_palette_hub_NUSA <- colorRampPalette(c("yellow", "red"))(121)

V(g_90_NUSA)$ColorHub  <- color_palette_hub_NUSA[V(g_90_NUSA)$Hub ^2]
V(g_00_NUSA)$ColorHub  <- color_palette_hub_NUSA[V(g_00_NUSA)$Hub ^2]
V(g_10_NUSA)$ColorHub  <- color_palette_hub_NUSA[V(g_10_NUSA)$Hub ^2]
V(g_20_NUSA)$ColorHub  <- color_palette_hub_NUSA[V(g_20_NUSA)$Hub ^2]

######Color for Auth####

# Create a color palette from yellow to red; using 121 as max value and squaring all
# the hub and auth scores to great greater gradation in the coloring, i.e. make more
# obvious differences in scores via the associated vertex colors. 

color_palette_auth_NUSA <- colorRampPalette(c("yellow", "red"))(121)

V(g_90_NUSA)$ColorAuth  <- color_palette_auth[V(g_90_NUSA)$Auth^2]
V(g_00_NUSA)$ColorAuth  <- color_palette_auth[V(g_00_NUSA)$Auth^2]
V(g_10_NUSA)$ColorAuth  <- color_palette_auth[V(g_10_NUSA)$Auth^2]
V(g_20_NUSA)$ColorAuth  <- color_palette_auth[V(g_20_NUSA)$Auth^2]

##############################
#Plotting and Saving Jpegs####
##############################

#Set layout for all networks/jpegs for easy comparison

layout_preferred_2 <- layout_with_fr(g_20_NUSA)

###Degree Total####

jpeg("Plot_NUSA_SizeDegreeTotal.jpg", 
     width = 1500, height = 1500, res = 150)

# Set up a 2x2 plotting area
par(mfrow = c(2, 2), mar = c(1, 0, 2, 0))


# Plot each graph
plot(g_90_NUSA,
     main = "1990 No USA \n Size & Color = Degree Total",
     edge.arrow.size = 0.5,
     edge.width = 1,
     vertex.label.cex = .9,
     layout = layout_preferred_2,
     vertex.size = V(g_90_NUSA)$DegT *1.5,
     vertex.color = V(g_90_NUSA)$ColorDT ,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_00_NUSA,
     main = "2000 No USA \n Size & Color = Degree Total",
     edge.arrow.size = 0.5,
     edge.width = 1,
     vertex.label.cex = .9,
     layout = layout_preferred_2,
     vertex.size = V(g_00_NUSA)$DegT *1.5,
     vertex.color = V(g_00_NUSA)$ColorDT ,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_10_NUSA,
     main = "2010 No USA \n Size & Color = Degree Total",
     edge.arrow.size = 0.5,
     edge.width = 1,
     vertex.label.cex = .9,
     layout = layout_preferred_2,
     vertex.size = V(g_10_NUSA)$DegT *1.5,
     vertex.color = V(g_10_NUSA)$ColorDT ,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_20_NUSA,
     main = "2020 No USA \n Size & Color = Degree Total",
     edge.arrow.size = 0.5,
     edge.width = 1,
     vertex.label.cex = .9,
     layout = layout_preferred_2,
     vertex.size = V(g_20_NUSA)$DegT *1.5,
     vertex.color = V(g_20_NUSA)$ColorDT ,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

# Close the JPEG device
dev.off()


par(mfrow=c(1,1))



######With Edge Attribute####


jpeg("Plot_NUSA_SizeDegreeTotal_Thick.jpg", 
     width = 1500, height = 1500, res = 150)

# Set up a 2x2 plotting area
par(mfrow = c(2, 2), mar = c(1, 0, 2, 0))

# Plot each graph
plot(g_90_NUSA,
     main = "1990 No USA \n Size & Color = Degree Total",
     edge.arrow.size = 1,
     edge.width = E(g_90_NUSA)$width/4,
     vertex.label.cex = .9,
     layout = layout_preferred_2,
     vertex.size = V(g_90_NUSA)$DegT *1.5,
     vertex.color = V(g_90_NUSA)$ColorDT ,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_00_NUSA,
     main = "2000 No USA \n Size & Color = Degree Total",
     edge.arrow.size = 1,
     edge.width = E(g_00_NUSA)$width/4,
     vertex.label.cex = .9,
     layout = layout_preferred_2,
     vertex.size = V(g_00_NUSA)$DegT *1.5,
     vertex.color = V(g_00_NUSA)$ColorDT ,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_10_NUSA,
     main = "2010 No USA\n Size & Color = Degree Total",
     edge.arrow.size = 1,
     edge.width = E(g_10_NUSA)$width/4,
     vertex.label.cex = .9,
     layout = layout_preferred_2,
     vertex.size = V(g_10_NUSA)$DegT *1.5,
     vertex.color = V(g_10_NUSA)$ColorDT ,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_20_NUSA,
     main = "2020 No USA \n Size & Color = Degree Total",
     edge.arrow.size = 1,
     edge.width = E(g_20_NUSA)$width/4,
     vertex.label.cex = .9,
     layout = layout_preferred_2,
     vertex.size = V(g_20_NUSA)$DegT *1.5,
     vertex.color = V(g_20_NUSA)$ColorDT ,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

# Close the JPEG device
dev.off()


par(mfrow=c(1,1))


###Degree IN####

jpeg("Plot_NUSA_SizeDegreeIN.jpg", 
     width = 1500, height = 1500, res = 150)

# Set up a 2x2 plotting area
par(mfrow = c(2, 2), mar = c(1, 0, 2, 0))


# Plot each graph
plot(g_90_NUSA,
     main = "1990 No USA \n Size & Color = Degree IN",
     edge.arrow.size = 0.5,
     edge.width = 1,
     vertex.label.cex = .9,
     layout = layout_preferred_2,
     vertex.size = V(g_90_NUSA)$DegI *1.5,
     vertex.color = V(g_90_NUSA)$ColorDI ,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_00_NUSA,
     main = "2000 No USA \n Size & Color = Degree IN",
     edge.arrow.size = 0.5,
     edge.width = 1,
     vertex.label.cex = .9,
     layout = layout_preferred_2,
     vertex.size = V(g_00_NUSA)$DegI *1.5,
     vertex.color = V(g_00_NUSA)$ColorDI ,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_10_NUSA,
     main = "2010 No USA \n Size & Color = Degree IN",
     edge.arrow.size = 0.5,
     edge.width = 1,
     vertex.label.cex = .9,
     layout = layout_preferred_2,
     vertex.size = V(g_10_NUSA)$DegI *1.5,
     vertex.color = V(g_10_NUSA)$ColorDI ,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_20_NUSA,
     main = "2020 No USA \n Size & Color = Degree IN",
     edge.arrow.size = 0.5,
     edge.width = 1,
     vertex.label.cex = .9,
     layout = layout_preferred_2,
     vertex.size = V(g_20_NUSA)$DegI *1.5,
     vertex.color = V(g_20_NUSA)$ColorDI ,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

# Close the JPEG device
dev.off()


par(mfrow=c(1,1))


######With Edge Attribute####

jpeg("Plot_NUSA_SizeDegreeIN_Thick.jpg", 
     width = 1500, height = 1500, res = 150)

# Set up a 2x2 plotting area
par(mfrow = c(2, 2), mar = c(1, 0, 2, 0))

# Plot each graph
plot(g_90_NUSA,
     main = "1990 No USA \n Size & Color = Degree IN",
     edge.arrow.size = 1,
     edge.width = E(g_90_NUSA)$width/4,
     vertex.label.cex = .9,
     layout = layout_preferred_2,
     vertex.size = V(g_90_NUSA)$DegI *1.5,
     vertex.color = V(g_90_NUSA)$ColorDI ,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_00_NUSA,
     main = "2000 No USA \n Size & Color = Degree IN",
     edge.arrow.size = 1,
     edge.width = E(g_00_NUSA)$width/4,
     vertex.label.cex = .9,
     layout = layout_preferred_2,
     vertex.size = V(g_00_NUSA)$DegI *1.5,
     vertex.color = V(g_00_NUSA)$ColorDI ,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_10_NUSA,
     main = "2010 No USA \n Size & Color = Degree IN",
     edge.arrow.size = 1,
     edge.width = E(g_10_NUSA)$width/4,
     vertex.label.cex = .9,
     layout = layout_preferred_2,
     vertex.size = V(g_10_NUSA)$DegI *1.5,
     vertex.color = V(g_10_NUSA)$ColorDI ,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_20_NUSA,
     main = "2020 No USA \n Size & Color = Degree IN",
     edge.arrow.size = 1,
     edge.width = E(g_20_NUSA)$width/4,
     vertex.label.cex = .9,
     layout = layout_preferred_2,
     vertex.size = V(g_20_NUSA)$DegI *1.5,
     vertex.color = V(g_20_NUSA)$ColorDI ,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

# Close the JPEG device
dev.off()


par(mfrow=c(1,1))


###Degree Out####

jpeg("Plot_NUSA_SizeDegreeOUT.jpg", 
     width = 1500, height = 1500, res = 150)

# Set up a 2x2 plotting area
par(mfrow = c(2, 2), mar = c(1, 0, 2, 0))


# Plot each graph
plot(g_90_NUSA,
     main = "1990 No USA \n Size & Color = Degree OUT",
     edge.arrow.size = 0.5,
     edge.width = 1,
     vertex.label.cex = .9,
     layout = layout_preferred_2,
     vertex.size = V(g_90_NUSA)$DegO *2,
     vertex.color = V(g_90_NUSA)$ColorDO ,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_00_NUSA,
     main = "2000 No USA \n Size & Color = Degree OUT",
     edge.arrow.size = 0.5,
     edge.width = 1,
     vertex.label.cex = .9,
     layout = layout_preferred_2,
     vertex.size = V(g_00_NUSA)$DegO *2,
     vertex.color = V(g_00_NUSA)$ColorDO ,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_10_NUSA,
     main = "2010 No USA \n Size & Color = Degree OUT",
     edge.arrow.size = 0.5,
     edge.width = 1,
     vertex.label.cex = .9,
     layout = layout_preferred_2,
     vertex.size = V(g_10_NUSA)$DegO *2,
     vertex.color = V(g_10_NUSA)$ColorDO ,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_20_NUSA,
     main = "2020 No USA \n Size & Color = Degree OUT",
     edge.arrow.size = 0.5,
     edge.width = 1,
     vertex.label.cex = .9,
     layout = layout_preferred_2,
     vertex.size = V(g_20_NUSA)$DegO *2,
     vertex.color = V(g_20_NUSA)$ColorDO ,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

# Close the JPEG device
dev.off()


# par(mfrow=c(1,1))


######With Edge Attribute####

jpeg("Plot_NUSA_SizeDegreeOUT_Thick.jpg", 
     width = 1500, height = 1500, res = 150)

# Set up a 2x2 plotting area
par(mfrow = c(2, 2), mar = c(1, 0, 2, 0))

# Plot each graph
plot(g_90_NUSA,
     main = "1990 No USA \n Size & Color = Degree OUT",
     edge.arrow.size = 1,
     edge.width = E(g_90_NUSA)$width/4,
     vertex.label.cex = .9,
     layout = layout_preferred_2,
     vertex.size = V(g_90_NUSA)$DegO *2,
     vertex.color = V(g_90_NUSA)$ColorDO ,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_00_NUSA,
     main = "2000 No USA \n Size & Color = Degree OUT",
     edge.arrow.size = 1,
     edge.width = E(g_00_NUSA)$width/4,
     vertex.label.cex = .9,
     layout = layout_preferred_2,
     vertex.size = V(g_00_NUSA)$DegO *2,
     vertex.color = V(g_00_NUSA)$ColorDO ,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_10_NUSA,
     main = "2010 No USA \n Size & Color = Degree OUT",
     edge.arrow.size = 1,
     edge.width = E(g_10_NUSA)$width/4,
     vertex.label.cex = .9,
     layout = layout_preferred_2,
     vertex.size = V(g_10_NUSA)$DegO *2,
     vertex.color = V(g_10_NUSA)$ColorDO ,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_20_NUSA,
     main = "2020 No USA \n Size & Color = Degree OUT",
     edge.arrow.size = 1,
     edge.width = E(g_20_NUSA)$width/4,
     vertex.label.cex = .9,
     layout = layout_preferred_2,
     vertex.size = V(g_20_NUSA)$DegO *2,
     vertex.color = V(g_20_NUSA)$ColorDO ,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

# Close the JPEG device
dev.off()


par(mfrow=c(1,1))


###Hub####

jpeg("Plot_NUSA_SizeHub.jpg", 
     width = 1500, height = 1500, res = 150)

# Set up a 2x2 plotting area
par(mfrow = c(2, 2), mar = c(1, 0, 2, 0))


# Plot each graph
plot(g_90_NUSA,
     main = "1990 No USA \n Size & Color = HUB",
     edge.arrow.size = 0.5,
     edge.width = 1,
     vertex.label.cex = .9,
     layout = layout_preferred_2,
     vertex.size = V(g_90_NUSA)$Hub*2,
     vertex.color = V(g_90_NUSA)$ColorHub,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_00_NUSA,
     main = "2000 No USA \n Size & Color = HUB",
     edge.arrow.size = 0.5,
     edge.width = 1,
     vertex.label.cex = .9,
     layout = layout_preferred_2,
     vertex.size = V(g_00_NUSA)$Hub*2,
     vertex.color = V(g_00_NUSA)$ColorHub,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_10_NUSA,
     main = "2010 No USA \n Size & Color = HUB",
     edge.arrow.size = 0.5,
     edge.width = 1,
     vertex.label.cex = .9,
     layout = layout_preferred_2,
     vertex.size = V(g_10_NUSA)$Hub*2,
     vertex.color = V(g_10_NUSA)$ColorHub,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_20_NUSA,
     main = "2020 No USA \n Size & Color = HUB",
     edge.arrow.size = 0.5,
     edge.width = 1,
     vertex.label.cex = .9,
     layout = layout_preferred_2,
     vertex.size = V(g_20_NUSA)$Hub*2,
     vertex.color = V(g_20_NUSA)$ColorHub,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

# Close the JPEG device
dev.off()


par(mfrow=c(1,1))

######With Edge Attribute####

jpeg("Plot_NUSA_SizeHub_Thick.jpg", 
     width = 1500, height = 1500, res = 150)

# Set up a 2x2 plotting area
par(mfrow = c(2, 2), mar = c(1, 0, 2, 0))


# Plot each graph
plot(g_90_NUSA,
     main = "1990 No USA \n Size & Color = HUB",
     edge.arrow.size = 1,
     edge.width = E(g_90_NUSA)$width/4,
     vertex.label.cex = .9,
     layout = layout_preferred_2,
     vertex.size = V(g_90_NUSA)$Hub*2,
     vertex.color = V(g_90_NUSA)$ColorHub,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_00_NUSA,
     main = "2000 No USA \n Size & Color = HUB",
     edge.arrow.size = 1,
     edge.width = E(g_00_NUSA)$width/4,
     vertex.label.cex = .9,
     layout = layout_preferred_2,
     vertex.size = V(g_00_NUSA)$Hub*2,
     vertex.color = V(g_00_NUSA)$ColorHub,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_10_NUSA,
     main = "2010 No USA \n Size & Color = HUB",
     edge.arrow.size = 1,
     edge.width = E(g_10_NUSA)$width/4,
     vertex.label.cex = .9,
     layout = layout_preferred_2,
     vertex.size = V(g_10_NUSA)$Hub*2,
     vertex.color = V(g_10_NUSA)$ColorHub,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_20_NUSA,
     main = "2020 No USA \n Size & Color = HUB",
     edge.arrow.size = 1,
     edge.width = E(g_20_NUSA)$width/4,
     vertex.label.cex = .9,
     layout = layout_preferred_2,
     vertex.size = V(g_20_NUSA)$Hub*2,
     vertex.color = V(g_20_NUSA)$ColorHub,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

# Close the JPEG device
dev.off()


par(mfrow=c(1,1))


###Authority####

jpeg("Plot_NUSA_SizeAuth.jpg", 
     width = 1500, height = 1500, res = 150)

# Set up a 2x2 plotting area
par(mfrow = c(2, 2), mar = c(1, 0, 2, 0))


# Plot each graph
plot(g_90_NUSA,
     main = "1990 No USA \n Size & Color = Auth",
     edge.arrow.size = 0.5,
     edge.width = 1,
     vertex.label.cex = .9,
     layout = layout_preferred_2,
     vertex.size = V(g_90_NUSA)$Auth *2,
     vertex.color = V(g_90_NUSA)$ColorAuth ,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_00_NUSA,
     main = "2000 No USA \n Size & Color = Auth",
     edge.arrow.size = 0.5,
     edge.width = 1,
     vertex.label.cex = .9,
     layout = layout_preferred_2,
     vertex.size = V(g_00_NUSA)$Auth *2,
     vertex.color = V(g_00_NUSA)$ColorAuth ,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_10_NUSA,
     main = "2010 No USA \n Size & Color = Auth",
     edge.arrow.size = 0.5,
     edge.width = 1,
     vertex.label.cex = .9,
     layout = layout_preferred_2,
     vertex.size = V(g_10_NUSA)$Auth *2,
     vertex.color = V(g_10_NUSA)$ColorAuth ,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_20_NUSA,
     main = "2020 No USA \n Size & Color = Auth",
     edge.arrow.size = 0.5,
     edge.width = 1,
     vertex.label.cex = .9,
     layout = layout_preferred_2,
     vertex.size = V(g_20_NUSA)$Auth *2,
     vertex.color = V(g_20_NUSA)$ColorAuth ,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

# Close the JPEG device
dev.off()


par(mfrow=c(1,1))


######With Edge Attribute####


jpeg("Plot_NUSA_SizeAuth_Thick.jpg", 
     width = 1500, height = 1500, res = 150)

# Set up a 2x2 plotting area
par(mfrow = c(2, 2), mar = c(1, 0, 2, 0))


# Plot each graph
plot(g_90_NUSA,
     main = "1990 No USA \n Size & Color = Auth",
     edge.arrow.size = 1,
     edge.width = E(g_90_NUSA)$width/4,
     vertex.label.cex = .9,
     layout = layout_preferred_2,
     vertex.size = V(g_90_NUSA)$Auth *2,
     vertex.color = V(g_90_NUSA)$ColorAuth ,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_00_NUSA,
     main = "2000 No USA \n Size & Color = Auth",
     edge.arrow.size = 1,
     edge.width = E(g_00_NUSA)$width/4,
     vertex.label.cex = .9,
     layout = layout_preferred_2,
     vertex.size = V(g_00_NUSA)$Auth *2,
     vertex.color = V(g_00_NUSA)$ColorAuth ,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_10_NUSA,
     main = "2010 No USA \n Size & Color = Auth",
     edge.arrow.size = 1,
     edge.width = E(g_10_NUSA)$width/4,
     vertex.label.cex = .9,
     layout = layout_preferred_2,
     vertex.size = V(g_10_NUSA)$Auth *2,
     vertex.color = V(g_10_NUSA)$ColorAuth ,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

plot(g_20_NUSA,
     main = "2020 No USA \n Size & Color = Auth",
     edge.arrow.size = 1,
     edge.width = E(g_20_NUSA)$width/4,
     vertex.label.cex = .9,
     layout = layout_preferred_2,
     vertex.size = V(g_20_NUSA)$Auth *2,
     vertex.color = V(g_20_NUSA)$ColorAuth ,
     vertex.frame.color = "lightblue",
     vertex.label.font = 2)

# Close the JPEG device
dev.off()


par(mfrow=c(1,1))






################################
#######Descriptive Stats########
################################

#With US

##Density

edge_density(g_90) #0.126
edge_density(g_00) #0.126
edge_density(g_10) #0.149
edge_density(g_20) #0.187


##Degree centralization

centr_degree(g_90)$centralization #0.366
centr_degree(g_00)$centralization #0.366
centr_degree(g_10)$centralization #0.370
centr_degree(g_20)$centralization #0.330


##Betweeness Centralization

centr_betw(g_90)$centralization #0.008
centr_betw(g_00)$centralization #0.016
centr_betw(g_10)$centralization #0.029
centr_betw(g_20)$centralization #0.031



#WithOut US#Wg_90ithOut US

##Density

edge_density(g_90_NUSA) #0.085
edge_density(g_00_NUSA) #0.085
edge_density(g_10_NUSA) #0.108
edge_density(g_20_NUSA) #0.150



##Degree centralization

centr_degree(g_90_NUSA)$centralization #0.066
centr_degree(g_00_NUSA)$centralization #0.159
centr_degree(g_10_NUSA)$centralization #0.166
centr_degree(g_20_NUSA)$centralization #0.183


##Betweenness Centralization

centr_betw(g_90_NUSA)$centralization #0.009
centr_betw(g_00_NUSA)$centralization #0.017
centr_betw(g_10_NUSA)$centralization #0.032
centr_betw(g_20_NUSA)$centralization #0.034





###################################################
##### Interpreting/analyzing hub vs degree OUT ####
###################################################

#With US Data

# Calculate hub scores
hub_scores <- hub_score(g_90)$vector

# Order the hub scores from highest to lowest
ordered_indices <- order(hub_scores, decreasing = TRUE)
ordered_hub_scores <- hub_scores[ordered_indices]

# Print the ordered hub scores
print(ordered_hub_scores)


# Degree OUT
degree_scores <- degree(g_90, mode = "out")

ordered_indices <- order(degree_scores, decreasing = T)
ordered_degree_scores <- degree_scores[ordered_indices]

print(ordered_degree_scores)






#WithOut US

# Calculate hub scores
hub_scores <- hub_score(g_90_NUSA)$vector

# Order the hub scores from highest to lowest
ordered_indices <- order(hub_scores, decreasing = TRUE)
ordered_hub_scores <- hub_scores[ordered_indices]

# Print the ordered hub scores
print(ordered_hub_scores)


# Degree OUT
degree_scores <- degree(g_90_NUSA, mode = "out")

ordered_indices <- order(degree_scores, decreasing = T)
ordered_degree_scores <- degree_scores[ordered_indices]

print(ordered_degree_scores)







###############################################################################
#################### Saving Data Shortcut #####################################
###############################################################################


# save(g_90, file="g_90.rda")
# save(g_00, file="g_00.rda")
# save(g_10, file="g_10.rda")
# save(g_20, file="g_20.rda")
# 
# save(g_90_NUSA, file="g_90_NUSA.rda")
# save(g_00_NUSA, file="g_00_NUSA.rda")
# save(g_10_NUSA, file="g_10_NUSA.rda")
# save(g_20_NUSA, file="g_20_NUSA.rda")





#BROKERAGE????###################


install.packages("statnet", dependencies = TRUE)
library(statnet)

install.packages("intergraph")
library(intergraph)

net <- asNetwork(g_20)


brokerage(g_20, cl=network::get.vertex.attribute(net, "name"))




#####################################################################
######### Layout in orientation of Geography; for ArcGIS???##########
#####################################################################




# Define the countries and their coordinates in the specified order
countries <- c("Arg", "Bol", "Braz", "Can", "Chile", "Col", "CR", "Ecu", 
               "ElSal", "Guat", "Hond", "Mex", "NG", "Pan", "PG", "Peru", 
               "UG", "US", "VZ")

latitudes <- c(-38.4161, -16.2902, -14.2350, 56.1304, -35.6751, 4.5709, 
               9.7489, -1.8312, 13.7942, 15.7835, 15.2000, 23.6345, 
               9.0820, 8.5379, -6.314993, -9.1900, 1.3733, 37.0902, 6.4238)

longitudes <- c(-63.6167, -63.5887, -51.9253, -106.3468, -71.5429, -74.2973, 
                -83.7534, -78.1834, -88.8965, -90.2308, -86.2419, -102.5528, 
                8.6753, -80.7821, 143.95555, -75.0152, 32.2903, -95.7129, -66.5897)

# Combine into a data frame
coordinates <- data.frame(country = countries, latitude = latitudes, longitude = longitudes)


#Add as vertex attributes

g_90 <- set_vertex_attr(g_90, name = "latitude", value = coordinates$latitude)
g_90 <- set_vertex_attr(g_90, name = "longitude", value = coordinates$longitude)

# Create a layout matrix from the latitude and longitude attributes
layout <- cbind(V(g_90)$longitude^2, V(g_90)$latitude^2)

# Plot the graph with the custom layout
plot(g_90, layout = layout)













install.packages("ggplot2")
install.packages("ggmap")
install.packages("dplyr")


library(ggplot2)
library(ggmap)
library(dplyr)


# Get the map of the western hemisphere
map <- get_map(location = c(lon = -90, lat = 0), zoom = 2, maptype = "terrain")


# Define the countries and their coordinates in the specified order
countries <- c("Arg", "Bol", "Braz", "Can", "Chile", "Col", "CR", "Ecu", 
               "ElSal", "Guat", "Hond", "Mex", "NG", "Pan", "PG", "Peru", 
               "UG", "US", "VZ")

latitudes <- c(-38.4161, -16.2902, -14.2350, 56.1304, -35.6751, 4.5709, 
               9.7489, -1.8312, 13.7942, 15.7835, 15.2000, 23.6345, 
               9.0820, 8.5379, -6.314993, -9.1900, 1.3733, 37.0902, 6.4238)

longitudes <- c(-63.6167, -63.5887, -51.9253, -106.3468, -71.5429, -74.2973, 
                -83.7534, -78.1834, -88.8965, -90.2308, -86.2419, -102.5528, 
                8.6753, -80.7821, 143.95555, -75.0152, 32.2903, -95.7129, -66.5897)

# Combine into a data frame
coordinates <- data.frame(country = countries, latitude = latitudes, longitude = longitudes)

# Create a graph (example)
g_90 <- make_empty_graph(n = length(countries), directed = FALSE) %>%
  set_vertex_attr(name = "name", value = countries) %>%
  set_vertex_attr(name = "latitude", value = latitudes) %>%
  set_vertex_attr(name = "longitude", value = longitudes)


# Create a layout matrix from the latitude and longitude attributes
layout <- cbind(V(g_90)$longitude, V(g_90)$latitude)



# Convert the igraph object to a data frame for ggplot2
edges <- as_data_frame(g_90, what = "edges")
vertices <- as_data_frame(g_90, what = "vertices")

# Plot the map with the network
ggmap(map) +
  geom_segment(data = edges, aes(x = longitudes[from], y = latitudes[from], 
                                 xend = longitudes[to], yend = latitudes[to]), 
               color = "blue", size = 0.5) +
  geom_point(data = vertices, aes(x = longitude, y = latitude), 
             color = "red", size = 3) +
  geom_text(data = vertices, aes(x = longitude, y = latitude, label = name), 
            vjust = -1, hjust = 0.5, size = 3)
