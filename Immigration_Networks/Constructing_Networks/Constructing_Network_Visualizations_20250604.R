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
setwd("C:/Users/heblj/OneDrive/Career/Professional_Portfolio/Network_Analysis/MigrationAmericas_NetworkAnalysis/Immigration_Networks")



# Load data ---------------------------------------------------------------


### adjacency Matrices ---------------------------------------------------------------

# Should use adjacency matrices as this will ensure later vertex-order of the countries is the same

## 1990
AM_1990 <- read.xlsx("Migration_Data/Matrices/DifferenceMatrices_1990.xlsx", 
                     sheet = "DM_10000_binary",
                     rowNames = TRUE)

## 2000
AM_2000 <- read.xlsx("Migration_Data/Matrices/DifferenceMatrices_2000.xlsx", 
                    sheet = "DM_10000_binary",
                    rowNames = TRUE)

## 2010
AM_2010 <- read.xlsx("Migration_Data/Matrices/DifferenceMatrices_2010.xlsx", 
                    sheet = "DM_10000_binary",
                    rowNames = TRUE)

## 2020
AM_2020 <- read.xlsx("Migration_Data/Matrices/DifferenceMatrices_2020.xlsx", 
                    sheet = "DM_10000_binary",
                    rowNames = TRUE)


### Matrices ----------------------------------------------------------------
# Creating Network Objects ------------------------------------------------

  #1990
    m_1990 <- as.matrix(AM_1990) # Create Matrix object first
    g_1990 <- graph_from_adjacency_matrix(m_1990,
                                          mode = "directed",
                                          weighted = NULL)
  
    #Save network
    save(g_1990, file = "Constructing_Networks/igraph Objects/g_1990.R")

  
  
  #2000
    m_2000 <- as.matrix(AM_2000) # Create Matrix object first
    g_2000 <- graph_from_adjacency_matrix(m_2000,
                                          mode = "directed",
                                          weighted = NULL)
    
    #Save network
    save(g_2000, file = "Constructing_Networks/igraph Objects/g_2000.R")
  
  
  #2010
    m_2010 <- as.matrix(AM_2010) # Create Matrix object first
    g_2010 <- graph_from_adjacency_matrix(m_2010,
                                          mode = "directed",
                                          weighted = NULL)
    
    #Save network
    save(g_2010, file = "Constructing_Networks/igraph Objects/g_2010.R")
  
  
  #2020
    m_2020 <- as.matrix(AM_2020) # Create Matrix object first
    g_2020 <- graph_from_adjacency_matrix(m_2020,
                                          mode = "directed",
                                          weighted = NULL)
    
    #Save network
    save(g_2020, file = "Constructing_Networks/igraph Objects/g_2020.R")
  

# Adding attributes ------------------------------------
  
  ### Vertices Attributes ----
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
        "Venezuela"      = "VEN",
        "United.States"  = "USA",
        "El.Salvador"    = "SLV"
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
        
      
  
    ### Adding Centralization Measures ----
        ###### Betweeness -------------------------------------------------------
        
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

    ### Adding GDP-per-Cap to each Vertex per year ----
        
        # Load the GDP dataframe
        gdpPerCap <- read.xlsx("Constructing_Networks/IMF_GDPperCap_PPP.xlsx", sheet = "10yr Avgs")
        
        # 1990
        idx <- match(V(g_1990)$name, gdpPerCap$COUNTRY)
        V(g_1990)$GDPperCap <- gdpPerCap$`1990`[idx]
        
        # 2000
        idx <- match(V(g_2000)$name, gdpPerCap$COUNTRY)
        V(g_2000)$GDPperCap <- gdpPerCap$`2000`[idx]
        
        # 2010
        idx <- match(V(g_2010)$name, gdpPerCap$COUNTRY)
        V(g_2010)$GDPperCap <- gdpPerCap$`2010`[idx]
        
        # 2020
        idx <- match(V(g_2020)$name, gdpPerCap$COUNTRY)
        V(g_2020)$GDPperCap <- gdpPerCap$`2020`[idx]
        
        
        
    ### Saving Updates to igraph objects ----
        #1990
        save(g_1990, file = "Constructing_Networks/igraph Objects/g_1990.R")
        
        
        #2000
        save(g_2000, file = "Constructing_Networks/igraph Objects/g_2000.R")
        
        
        #2010
        save(g_2010, file = "Constructing_Networks/igraph Objects/g_2010.R")

        
        #2020
        save(g_2020, file = "Constructing_Networks/igraph Objects/g_2020.R")
        
        
        
        
        
        ### Adding Vertex Colors ----
        ###### DegTotal ----
        # Find the maximum degree score for each vertex across all networks
        max_degree_value_td <- max(unlist(degree_scores_td))
        #
        # Create a color palette from yellow to red
        color_palette_degree_td <- colorRampPalette(c("yellow", "red"))(max_degree_value_td + 1)
        
        V(g_1990)$ColorDT <- color_palette_degree_td[degree(g_1990)]
        V(g_2000)$ColorDT <- color_palette_degree_td[degree(g_2000)]
        V(g_2010)$ColorDT <- color_palette_degree_td[degree(g_2010)]
        V(g_2020)$ColorDT <- color_palette_degree_td[degree(g_2020)]
        ###### DegIn ----
        # Find the maximum degree score for each vertex across all networks
        max_degree_value_di <- max(unlist(degree_scores_di))
        
        # Create a color palette from yellow to red
        color_palette_degree_di <- colorRampPalette(c("yellow", "red"))(max_degree_value_di + 1)
        
        V(g_1990)$ColorDI <- color_palette_degree_di[degree(g_1990)]
        V(g_2000)$ColorDI <- color_palette_degree_di[degree(g_2000)]
        V(g_2010)$ColorDI <- color_palette_degree_di[degree(g_2010)]
        V(g_2020)$ColorDI <- color_palette_degree_di[degree(g_2020)]
        
        
        ###### DegOut ----
        # # Find the maximum degree score for each vertex across all networks
        max_degree_value_do <- max(unlist(degree_scores_do))
        #
        # # Create a color palette from yellow to red
        color_palette_degree_do <- colorRampPalette(c("yellow", "red"))(max_degree_value_do + 1)
        
        V(g_1990)$ColorDO <- color_palette_degree_do[degree(g_1990)]
        V(g_2000)$ColorDO <- color_palette_degree_do[degree(g_2000)]
        V(g_2010)$ColorDO <- color_palette_degree_do[degree(g_2010)]
        V(g_2020)$ColorDO <- color_palette_degree_do[degree(g_2020)]
        
        
        
        
        ###### GDP per Cap ----
        
        # Extract GDP values
        gdp_vals_1990 <- V(g_1990)$GDPperCap
        gdp_vals_2020 <- V(g_2020)$GDPperCap
        
        # Create palette
        color_palette_GDP <- colorRampPalette(c("red", "yellow"))(5)
        
        # Scale GDP values to 1–100
        gdp_scaled_1990 <- scales::rescale(gdp_vals_1990, to = c(1, 5))
        gdp_scaled_1990 <- round(gdp_scaled_1990)
        
        gdp_scaled_2020 <- scales::rescale(gdp_vals_2020, to = c(1, 5))
        gdp_scaled_2020 <- round(gdp_scaled_2020)
        
        # Assign colors
        V(g_1990)$ColorGDP <- color_palette_GDP[gdp_scaled_1990]
        
        V(g_2020)$ColorGDP <- color_palette_GDP[gdp_scaled_2020]
        

  ### Edge Attributes ----
    ### migration flow "weight(s)" ----
        
        E(g_1990)$MigrationFlowWeight <- el_1990$Immigration_absolute
        E(g_1990)$MigrationFlowWeight_PropOrg <- el_1990$per1000_org
        E(g_1990)$MigrationFlowWeight_PropDest <- el_1990$per1000_dest
        
        E(g_2000)$MigrationFlowWeight <- el_2000$Immigration_absolute
        E(g_2000)$MigrationFlowWeight_PropOrg <- el_2000$per1000_org
        E(g_2000)$MigrationFlowWeight_PropDest <- el_2000$per1000_dest
        
        E(g_2010)$MigrationFlowWeight <- el_2010$Immigration_absolute
        E(g_2010)$MigrationFlowWeight_PropOrg <- el_2010$per1000_org
        E(g_2010)$MigrationFlowWeight_PropDest <- el_2010$per1000_dest
        
        E(g_2020)$MigrationFlowWeight <- el_2020$Immigration_absolute
        E(g_2020)$MigrationFlowWeight_PropOrg <- el_2020$per1000_org
        E(g_2020)$MigrationFlowWeight_PropDest <- el_2020$per1000_dest

  ### Saving Graphs ----
      
      save(g_1990, file = "Constructing_Networks/g_1990.R")
      save(g_2000, file = "Constructing_Networks/g_2000.R")
      save(g_2010, file = "Constructing_Networks/g_2010.R")
      save(g_2020, file = "Constructing_Networks/g_2020.R")
      
# Creating Visualizations ----
      
  ### Load the graphs ----
      load("Constructing_Networks/g_1990.R")
      load("Constructing_Networks/g_2000.R")
      load("Constructing_Networks/g_2010.R")
      load("Constructing_Networks/g_2020.R")
      
      # Defined the preferred layout to use in subsequent networks
      layout_preferred <- layout_with_fr(g_1990)
      
  ### Graph based on Degree ----
      ### Degree Total ----
      
      plot(g_1990,
           main = "Migration 1990",
           sub = "Vertex size by Degree",
           layout = layout_preferred,
           edge.arrow.size = 0.5,
           vertex.size = 10,
           vertex.color = V(g_1990)$ColorGDP,
           vertex.label.font = 2,
           vertex.label = V(g_1990)$code)
      
      plot(g_2020,
           main = "Migration 2020",
           sub = "Vertex size by Degree",
           edge.arrow.size = 0.5,
           vertex.size = 10,
           vertex.color = V(g_2020)$ColorGDP,
           vertex.label.font = 2,
           vertex.label = V(g_2020)$code)
      