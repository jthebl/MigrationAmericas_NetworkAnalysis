---
title: "Matrices"
author: "Joey"
date: "2026-03-28"
output: 
  html_document: 
    keep_md: true
---


# Getting Started: Sourcing the Raw Migration data

There are many sources of migration data available publicly, but for the present
project I sourced my raw data from the United Nations, specifically from the 
Population Division's "International Migrant Stock" [report](https://www.un.org/development/desa/pd/content/international-migrant-stock). 
Data is presented in 5-year intervals starting from 1990 and continuing until 2020 (at the
time of, and that which is considered in the current version of this project). Relevant country
migration data can be selected according to either Destination, Origin, or Both. 

This excel can be filtered for the countries of interest, both from sending and receiving perspectives.
Though an obvious point, it is worth mentioning here that on a small enough timescale, migration is, at it's core and as will be constructed in the present analysis, 
a two-party phenomena (i.e. it involves a country of origin 
and country of destination). A more nuanced, qualitative discussion of this and other aspects of this analysis
can be reviewed in the Further Thoughts and Considerations file. 

For the present project, which is focused on migration in the Americas, the following
criteria was used for selecting countries:
  (1) Minimum population of 2 million (this effectively eliminates the Caribbean-Island 
      nations though in a future analysis I hope to include these states as well).
  (2) Within the "American" Region (the continued debate regarding geographic nomenclature--and the inherent
      effects this has on socioeconomic factors, as well as cultural perceptions--,though 
      important, is not relevant to the present analysis given the complete inclusion of both the North
      and South American regions).
      
From this filtering-protocol, the following nations, and their respective migration data, were selected:
  Argentina,
  Bolivia,
  Brazil,
  Canada,
  Chile,
  Colombia,
  Costa Rica,
  Ecuador,
  El Salvador,
  Guatemala,
  Honduras,
  Mexico,
  Nicaragua,
  Panama,
  Paraguay,
  Peru,
  United States,
  Uruguay,
  Venezuela.
  
The following section will review how this raw data of migration between nation-pairs was converted into a matrix.



# Step 1: Constructing the Matrix

This step requires some relatively straightforward logic as it pertains to the calculations for determining the flow 
of migrants between country-pairs. See the code below for specifics on how the associated matrices were constructed:


### R-script: Calculate differences in migration between nation-pairs
This code generates an excel containing the migration matrix. Of note, the column of the matrix
represents the "recipient" country while the rows are the "sending" country. For example, taking
any value 'x' within the matrix, the corresponding country-column is the country that recieves
'x' number of migrants from the corresponding country-row. If 'x' is a positive number, then it
indicates net-flow "to" the column-country, while a negative number indicates net-flow "out" of the 
column-country (you can confirm this by noting that every value has a corresponding "opposite" value
in the matrix position that represents the inverse of the country-pair order. E.g. for country
A and B, if column-A and row-B have a value of 'x', then the corresponding column-B and row-A matrix
position will have a '-x' value).


``` r
GenerateDiffMatrix <- function(matrix_dir, Raw_matrix, year) {
  
  setwd(matrix_dir)
  
  
  # Load Data (i.e. the matrix)
  df_matrix_xl <- read_xlsx(Raw_matrix)
  countries <- df_matrix_xl[, 1]           # extract row names
  df_matrix  <- df_matrix_xl[, -1]         # drop first column (which has country names)
  df_matrix <- as.matrix(df_matrix)        # Convert tibble to a matrix
  rownames(df_matrix) <- countries[[1]]    # add back in rownames (i.e. country names) to matrix
  
  
  # The Code ----------------------------------------------------------------
  
  
  # Create the countries matrix; square matrix (i.e. name and order of rows/columns the same)
  diff_m <- matrix(0, 
                   nrow = length(countries), #Sending countries
                   ncol = length(countries), #Receiving countries
                   dimnames = list(countries, countries)) 
  
  diff_m <- df_matrix - t(df_matrix) #calculates a matrix that has the difference in migration between country pairs. 
  
  
  # Save outputs ------------------------------------------------------------
  
  write.xlsx(df_matrix, 
             file=paste0("DifferenceMatrices_",year,".xlsx"), 
             sheetName="RawMatrix", 
             row.names=TRUE)
}
```


### Additional script that generates additional matrices 
The code below generates matrices that reflect only positive values (i.e. the net flows),
with respective negative values filled in with the flag-code "999". 

``` r
# Simplified Matrix: Positive Only (i.e. Net-flow of immigrants)
  diff_m_simp <- diff_m
  for (i in 1:nrow(diff_m_simp)) {
    for (j in 1:ncol(diff_m_simp)) {
      if (diff_m_simp[i,j] < 0) {
        diff_m_simp[i,j] <-  999
      }
    }
  }
```

The code below takes a different approach in dividing the migrant flows by the population
of the 'receiving' country (i.e. column-country) for the given year represented by the matrix.
This therefore reflects a "density" related metric of immigration. 


``` r
# Difference relative to Population size of Receiving country
  population_df <- read.csv("Population_1990.csv") 
  pop_vec <- population_df[,2] #Convert population column into vector for below calculations
  
  # Scale migration flows by receiving-country population
  scaled_m <- sweep(diff_m,
                    MARGIN = 2,
                    STATS = pop_vec,
                    FUN = "/")
  
  # Scale per 1000 citizens of receiving country (makes values a bit more manageable as it pertains to interpretation)
  scaled_m_per_1000 <- sweep(scaled_m,
                             MARGIN = 2,
                             STATS = 1000,
                             FUN = "*") 
```


Lastly, for the sake of the subsequent analysis with igraph, the code below generates a binary
matrix in which a value of "1" is given for any nation-pair in which the receiving country
received >=10,000 migrants. This will both minimize the complexity of the resulting network figures, 
while also filtering for "major flows" of migrants as it pertains to the region at large. 

``` r
# Simple Binary for igraph networks ---------------------------------------

  diff_m_binary <- diff_m
  for (i in 1:nrow(diff_m_binary)) {
    for (j in 1:ncol(diff_m_binary)) {
      if (diff_m_binary[i,j] < 9999) {
        diff_m_binary[i,j] <-  0
      }
    }
  }  
```


