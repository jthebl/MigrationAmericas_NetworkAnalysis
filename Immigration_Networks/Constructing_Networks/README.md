---
title: "Step 3: Constructing Networks"
author: "Joey"
date: "2026-06-01"
output: 
  html_document:
    keep_md: TRUE
---

# Creating graph objects
If you have been following in the project narrative, we have successfully aggregated our data, created edgelist dataframes. In order to make visualizations using the igraph package, we need to first create graph objects.



## Loading the edgelists
We must first load the edgelists. For those familiar with R, it is important that you have, before this point, indicated the working directory and are aware of the location of the saved edgelists from Step 2. 


``` r
# Load data ---------------------------------------------------------------


# Edgelists ---------------------------------------------------------------

# 1990
 el_1990 <- read.csv("Edgelists/EdgeList_1990.csv") # load the csv edgelist

# 2000
 el_2000 <- read.csv("Edgelists/EdgeList_2000.csv") # load the csv edgelist

# 2010
 el_2010 <- read.csv("Edgelists/EdgeList_2010.csv") # load the csv edgelist

# 2020
 el_2020 <- read.csv("Edgelists/EdgeList_2020.csv") # load the csv edgelist
```


## Creating the graph objects
Next, we need to create graph objects in the igraph package by using graph_from_edgelist() function

``` r
### Matrices ----------------------------------------------------------------
# Creating Network Objects ------------------------------------------------

  #1990
  g_1990 <- select(el_1990, Origin, Destination) %>% # select only the first two columns for graphing purposes
    as.matrix() %>% # need to convert dataframe to a matrix
    graph_from_edgelist(directed = T) # creates the network graph object

  
  
  #2000
  g_2000 <- select(el_2000, Origin, Destination) %>% # select only the first two columns for graphing purposes
    as.matrix() %>% # need to convert dataframe to a matrix
    graph_from_edgelist(directed = T) # creates the network graph object
  
  
  
  #2010
  g_2010 <- select(el_2010, Origin, Destination) %>% # select only the first two columns for graphing purposes
    as.matrix() %>% # need to convert dataframe to a matrix
    graph_from_edgelist(directed = T) # creates the network graph object
  
  
  #2020
  g_2020 <- select(el_2020, Origin, Destination) %>% # select only the first two columns for graphing purposes
    as.matrix() %>% # need to convert dataframe to a matrix
    graph_from_edgelist(directed = T) # creates the network graph object
```

## Updating Country Codes
Given that our data included the full country names, which is will make reading our network visualizations extremely challenging, we next need to simply country names into the internationally recognized three letter codes. Note that we are doing this not by changing the names of the vertices in the graph objects, but rather by including an additional vertex attribute called "code".


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
```

## Adding Additional Vertex Attributes
In the following code chunks we will add additional vertex attributes. These will be related to network analysis metrics that are useful for understanding the dynamics of a network. 

### Degree
Degree represents the number of edges that a vertex possesses. These edges can be qualified as "In", that is edges coming toward the vertex (in the case of migration, a net migration toward the country), or "Out", that is edges going away from the vertex (net migration out of the country toward the country at the other end of the edge). The code below reflects the addition of these attributes to the vertices


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
The next attribute added to the vertices is betweeness. Betweeness is a measure that tells you how often a vertex sits on the shortest paths between other vertices. In other words, it measures how much a vertex acts as an essential bridge between other vertices.


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

