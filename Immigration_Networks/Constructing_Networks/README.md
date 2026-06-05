---
title: "Step 3: Constructing Network Objects"
author: "Joey"
date: "2026-06-05"
output: 
  html_document:
    keep_md: TRUE
---

# Creating graph objects
If you have been following in the project narrative - [Sourcing Raw Migration Data](https://github.com/jthebl/MigrationAmericas_NetworkAnalysis/tree/50146dfec499754fff72d8809971d308dfe1675b/Immigration_Networks/MigrationData#getting-started-sourcing-the-raw-migration-data), [Step 1: Constructing the Matrix](https://github.com/jthebl/MigrationAmericas_NetworkAnalysis/tree/50146dfec499754fff72d8809971d308dfe1675b/Immigration_Networks/MigrationData#step-1-constructing-the-matrix), and [Step 2: Constructing Edgelists](https://github.com/jthebl/MigrationAmericas_NetworkAnalysis/tree/50146dfec499754fff72d8809971d308dfe1675b/Immigration_Networks/MigrationData#step-2-constructing-edgelists) - we have successfully aggregated our data and created edgelist dataframes. In order to make visualizations using the igraph package, we need to first create network objects.




## Creating the network objects
Next, we need to create network objects with the igraph package so that we can not only create our visualization from the migration data we have thus far aggregated, but also so we can add additional nuances and information to the visualizations themselves. We will first create the network objects by using graph_from_adjacency_matrix() function. We need to load the proper adjacency matrices that we created in [Step 1: Constructing the Matrix.](https://github.com/jthebl/MigrationAmericas_NetworkAnalysis/tree/db793aef067e82282f2215df82572fa7029ec04c/Immigration_Networks/Migration_Data#step-1-constructing-the-matrix) We will then create network objects from these adjacency matrices by using the graph_from_adjacency_matrix() function.

``` r
### Matrices ----------------------------------------------------------------
  # Of note, you do need to know the relevant path or file directory for the files you plan to load, in this case the adjacency matrices.

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
  
  # Creating Network Objects ------------------------------------------------

  #1990
    m_1990 <- as.matrix(AM_1990) # Create Matrix object first
    g_1990 <- graph_from_adjacency_matrix(m_1990,
                                          mode = "directed",
                                          weighted = NULL)
  
  
  #2000
    m_2000 <- as.matrix(AM_2000) # Create Matrix object first
    g_2000 <- graph_from_adjacency_matrix(m_2000,
                                          mode = "directed",
                                          weighted = NULL)

  
  #2010
    m_2010 <- as.matrix(AM_2010) # Create Matrix object first
    g_2010 <- graph_from_adjacency_matrix(m_2010,
                                          mode = "directed",
                                          weighted = NULL)
    
  
  #2020
    m_2020 <- as.matrix(AM_2020) # Create Matrix object first
    g_2020 <- graph_from_adjacency_matrix(m_2020,
                                          mode = "directed",
                                          weighted = NULL)
```

## Updating Country Codes
Given that our data included the full country names, which will make reading our network visualizations extremely challenging, we need to simply country names into the internationally recognized three letter codes. Note that we are doing this not by changing the names of the vertices in the graph objects, but rather by including an additional vertex attribute called "code".


``` r
# Updating Vertex Labels --------------------------------------------------
    
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
    
    # Some additional naming variants in our data
    "United.States"  = "USA",
    "El.Salvador"    = "SLV",
    "Costa.Rica"     = "CRI"
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
```


## Adding Additional Vertex Attributes
In the following code chunks we will add additional vertex attributes. These will be related to network analysis metrics that are useful for understanding the dynamics of a network. 

### Degree
Degree represents the number of edges that a vertex possesses. These edges can be qualified as "In", that is edges coming toward the vertex (in the case of migration, a net migration toward the country), or "Out", that is edges going away from the vertex (net migration out of the country toward the country at the other end of the edge). The code below reflects the addition of these attributes to the vertices.


``` r
# Adding Degree-related Attributes ------------------------------------------------

    # Degree Total ------------------------------------------------------------
    
    # Combine graphis into list object for efficiency
    graphs_td <- list(g_1990, g_2000, g_2010, g_2020)
    
    # Calculate degree-total scores for each graph
    degree_scores_td <- lapply(graphs_td, degree)
    
    
    V(g_1990)$DegT <- degree_scores_td[[1]]
    V(g_2000)$DegT <- degree_scores_td[[2]]
    V(g_2010)$DegT <- degree_scores_td[[3]]
    V(g_2020)$DegT <- degree_scores_td[[4]]
    
    
    # Degree IN ------------------------------------------------------------
    
    # Calculate degree-in scores for each graph
    degree_scores_di <- lapply(graphs_td, degree, mode = "in")
    
    
    V(g_1990)$DegI <- degree_scores_di[[1]]
    V(g_2000)$DegI <- degree_scores_di[[2]]
    V(g_2010)$DegI <- degree_scores_di[[3]]
    V(g_2020)$DegI <- degree_scores_di[[4]]
    
    
    # Degree OUT ------------------------------------------------------------
    
    # Calculate degree-out scores for each graph
    degree_scores_do <- lapply(graphs_td, degree, mode = "out")
    
    
    V(g_1990)$DegO <- degree_scores_do[[1]]
    V(g_2000)$DegO <- degree_scores_do[[2]]
    V(g_2010)$DegO <- degree_scores_do[[3]]
    V(g_2020)$DegO <- degree_scores_do[[4]]
```

### Centrality Measure: Betweeness
The next attribute added to the vertices is betweeness. Betweeness is a measure that tells how often a vertex sits on the shortest path(s) between other vertices. In other words, it measures how much a vertex acts as an essential bridge between other vertices.


``` r
# Adding Centralization Measures ----
  # Betweeness -------------------------------------------------------
  
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
```

### GDP-per-Capita Hypothesis
To assess (at this point only qualitatively via visualizations) the extent to which GDP-per-capita might explain
migration patterns, we will next add GDP-per-capita as an additional attribute to the vertices. The underlying hypothesis is that countries with lower GDP-per-capita scores will experience _greater migration_, that is, greater nubmer of outflows or edges that are directed away from the low-GDP-per-capita country to others with greater GDP-per-capita scores.

Given that the periods assessed in this analysis invovle migration at 10yr increments (i.e. 1990, 2000, 2010, 2020), GDP was calculated by taking the average GDP-per-capita over the previous 10years (e.g. for 1990, GDP-per-capita was calulated by taking the mean of GDP-per-capita for each year from 1980 to 1990).


``` r
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
```

### Adding attributes to the Edges - Need to start with edglists
If we want to add attributes to the edges in our network visualizations, such as the number of migrants an edge represents (either in absolute or relative terms), we need to load the edgelists we created in [Step 2: Constructing Edgelists](https://github.com/jthebl/MigrationAmericas_NetworkAnalysis/tree/50146dfec499754fff72d8809971d308dfe1675b/Immigration_Networks/MigrationData#step-2-constructing-edgelists) and then add the relevant information these dataframes contain (specifically number of migrants each edge represents) to our network objects. 


``` r
# Edgelists ---------------------------------------------------------------

  # 1990
   el_1990 <- read.csv("Constructing_Networks/Edgelists/EdgeList_1990.csv") # load the csv edgelist
  
  # 2000
   el_2000 <- read.csv("Constructing_Networks/Edgelists/EdgeList_2000.csv") # load the csv edgelist
  
  # 2010
   el_2010 <- read.csv("Constructing_Networks/Edgelists/EdgeList_2010.csv") # load the csv edgelist
  
  # 2020
   el_2020 <- read.csv("Constructing_Networks/Edgelists/EdgeList_2020.csv") # load the csv edgelist
 
  
   
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
```

### On to Visualizations
Now that we have consolidated and updates our igraph objects, we are ready to begin making network visualizations. It was decided that this would be best demonstrated in and R pub, which can be seen here - [Step 4: Visualizing Networks](https://rpubs.com/jtheblj/Step4_VisualizingNetworks)
