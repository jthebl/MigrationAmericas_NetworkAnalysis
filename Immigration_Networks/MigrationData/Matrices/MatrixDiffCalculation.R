
# Title: Calculate Differences in migration between nation-pairs # --------

# General Steps Guide -----------------------------------------------------

# Steps: 
## 1. **Load the Matrix**
## 2. **Extract Countries**: Extract the list of countries from the first column and first row of the matrix.
## 3. **Initialize a New DataFrame**: Create a new DataFrame to store the differences between immigration between country pairs.
## 4. **Calculate Differences**: Calculate the net number of immigrants moving from each origin country to each destination country; store values in new DataFrame.
## 5. **Save the Differences Matrix**


# Set-up Space ------------------------------------------------------------

# clean
rm(list=ls())
gc()

# Load necessary packages
library(readxl)
library(tidyverse)
library(openxlsx)

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
  
  
  
  
  
  # Simplified Matrix: Positive Only (i.e. Net-flow of immigrants) ----------
  diff_m_simp <- diff_m
  for (i in 1:nrow(diff_m_simp)) {
    for (j in 1:ncol(diff_m_simp)) {
      if (diff_m_simp[i,j] < 0) {
        diff_m_simp[i,j] <-  999
      }
    }
  }
  

# Difference relative to Population size of Receiving country (i.e --------

  population_df <- read.xlsx("Population_1990")  
  
  

# Save outputs ------------------------------------------------------------
  
  write.xlsx(df_matrix, 
             file=paste0("DifferenceMatrices_",year,".xlsx"), 
             sheetName="RawMatrix", 
             row.names=TRUE)
  
  write.xlsx(diff_m, 
             file=paste0("DifferenceMatrices_",year,".xlsx"), 
             sheetName="DiffMatrix", 
             append=TRUE, 
             row.names=TRUE)
  
  write.xlsx(diff_m_simp, 
             file=paste0("DifferenceMatrices_",year,".xlsx"), 
             sheetName="DiffMatrix_simp", 
             append=TRUE, 
             row.names=TRUE)
  
  
}


# Run Function ------------------------------------------------------------

GenerateDiffMatrix(matrix_dir = "C:/Users/heblj/OneDrive/Career/Professional_Portfolio/Network_Analysis/MigrationAmericas_NetworkAnalysis/Immigration_Networks/MigrationData/Matrices",
                   Raw_matrix = "MigrationMatrix_1990.xlsx",
                   year = "1990")
