### Constructing Network Visualizations




# Set-up Space ------------------------------------------------------------

# clean
rm(list=ls())
gc()

# Load necessary packages
library(readxl)
library(tidyverse)
library(openxlsx)
library(igraph)

# Set Working Directory
setwd("C:/Users/heblj/OneDrive/Career/Professional_Portfolio/
      Network_Analysis/MigrationAmericas_NetworkAnalysis/Immigration_Networks/
      Migration_Data")



# Load data ---------------------------------------------------------------


### Edgelists ---------------------------------------------------------------


## 1990
el_1990 <- read.csv("Edgelists/EdgeList_1990.csv") # load the csv edgelist 

## 2000
el_2000 <- read.csv("Edgelists/EdgeList_2000.csv") # load the csv edgelist

## 2010
el_2010 <- read.csv("Edgelists/EdgeList_2010.csv") # load the csv edgelist

## 2020
el_2020 <- read.csv("Edgelists/EdgeList_2020.csv") # load the csv edgelist


### Matrices ----------------------------------------------------------------




# Creating Network Objects ------------------------------------------------

  #1990
  g_1990 <- select(el_1990, Origin, Destination) %>% # select only the first two columns for graphing purposes
    as.matrix() %>% # need to convert dataframe to a matrix
    graph_from_edgelist(directed = T) # creates the network graph object
  
  # Visualize
  # plot(g_1990)
  
  
  #2000
  g_2000 <- select(el_2000, Origin, Destination) %>% # select only the first two columns for graphing purposes
    as.matrix() %>% # need to convert dataframe to a matrix
    graph_from_edgelist(directed = T) # creates the network graph object
  
  # Visualize
  # plot(g_2000)
  
  
  #2010
  g_2010 <- select(el_2010, Origin, Destination) %>% # select only the first two columns for graphing purposes
    as.matrix() %>% # need to convert dataframe to a matrix
    graph_from_edgelist(directed = T) # creates the network graph object
  
  # Visualize
  # plot(g_2010)
  
  
  #2020
  g_2020 <- select(el_2020, Origin, Destination) %>% # select only the first two columns for graphing purposes
    as.matrix() %>% # need to convert dataframe to a matrix
    graph_from_edgelist(directed = T) # creates the network graph object
  
  # Visualize
  # plot(g_2020)
  
  

# Cleaning graph and adding attributes ------------------------------------


  ### Updating Vertex Labels --------------------------------------------------
  
    # Update vertex names according to Official ISO 3166-1 Alpha-3 codes
    country_codes <- list(
      "Argentina"      = "ARG",
      "Bolivia"        = "BOL",
      "Brazil"         = "BRA",
      "Canada"         = "CAN",
      "Chile"          = "CHL",
      "Colombia"       = "COL",
      "Costa Rica"     = "CRI",
      "Ecuador"        = "ECU",
      "El Salvador"    = "SLV",
      "Guatemala"      = "GTM",
      "Honduras"       = "HND",
      "Mexico"         = "MEX",
      "Nicaragua"      = "NIC",
      "Panama"         = "PAN",
      "Paraguay"       = "PRY",
      "Peru"           = "PER",
      "United States"  = "USA",
      "Uruguay"        = "URY",
      "Venezuela"      = "VEN"
    )
  
    # Convert country list to a named character vector
    codes <- unlist(country_codes)
    
    # Add country code to each graph object
    #1990
      V(g_1990)$code <- codes[V(g_1990)$name]
    
    #2000
      V(g_2000)$code <- codes[V(g_2000)$name]
      
    #2010
      V(g_2010)$code <- codes[V(g_2010)$name]
      
    #2020
      V(g_2020)$code <- codes[V(g_2020)$name]
    
  
  
  ### Adding Degree-related Attributes ------------------------------------------------
  
      ###### Degree Total ------------------------------------------------------------
      
      # Combine graphis into list object for efficiency
      graphs_td <- list(g_1990, g_2000, g_2010, g_2020)
      
      # Calculate degree-total scores for each graph
      degree_scores_td <- lapply(graphs_td, degree)
      
      
      V(g_1990)$DegT <- degree_scores_td[[1]]
      V(g_2000)$DegT <- degree_scores_td[[2]]
      V(g_2010)$DegT <- degree_scores_td[[3]]
      V(g_2020)$DegT <- degree_scores_td[[4]]
      
      
      ###### Degree IN ------------------------------------------------------------
      
      # Calculate degree-in scores for each graph
      degree_scores_di <- lapply(graphs_td, degree, mode = "in")
      
      
      V(g_1990)$DegI <- degree_scores_di[[1]]
      V(g_2000)$DegI <- degree_scores_di[[2]]
      V(g_2010)$DegI <- degree_scores_di[[3]]
      V(g_2020)$DegI <- degree_scores_di[[4]]
      
      
      ###### Degree OUT ------------------------------------------------------------
      
      # Calculate degree-out scores for each graph
      degree_scores_do <- lapply(graphs_td, degree, mode = "out")
      
      
      V(g_1990)$DegO <- degree_scores_do[[1]]
      V(g_2000)$DegO <- degree_scores_do[[2]]
      V(g_2010)$DegO <- degree_scores_do[[3]]
      V(g_2020)$DegO <- degree_scores_do[[4]]
      
    

  ### Adding Centralization Measures ------------------------------------------
      ###### Betweeness --------------------------------------------------------------
      
      #1990
      V(g_1990)$betweenness <- betweenness(
        g_1990,
        directed = T,
        weights = NULL
      )
      
      #1990
      V(g_2000)$betweenness <- betweenness(
        g_2000,
        directed = T,
        weights = NULL
      )
      
      #2010
      V(g_2010)$betweenness <- betweenness(
        g_2010,
        directed = T,
        weights = NULL
      )
      
      #2020
      V(g_2020)$betweenness <- betweenness(
        g_2020,
        directed = T,
        weights = NULL
      )

  
      