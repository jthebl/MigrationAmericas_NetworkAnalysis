
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

GenerateDiffMatrix <- function(matrix_dir, Raw_matrix, year, populationCSV) {
  
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
        diff_m_simp[i,j] <-  0
      }
    }
  }
  

  
  
  # Difference relative to Population size of Receiving country (i.e --------

  pop_file <- paste0("Population_Files/",populationCSV)
  
  population_df <- read.csv(pop_file) 
  pop_vec <- population_df[,2]
  
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
  
  # Scale per 1000 citizens of receiving country, but only considering the net-flow directions (i.e. only positive values)
  scaled_m_net <- sweep(diff_m_simp,
                        MARGIN = 2,
                        STATS = pop_vec,
                        FUN = "/")
  
  scaled_m_per_1000_net <- sweep(scaled_m_net,
                                 MARGIN = 2,
                                 STATS = 1000,
                                 FUN = "*")
  
  

# Simple Binary for igraph networks ---------------------------------------

  diff_m_binary <- diff_m
  for (i in 1:nrow(diff_m_binary)) {
    for (j in 1:ncol(diff_m_binary)) {
      if (diff_m_binary[i,j] < 9999) {
        diff_m_binary[i,j] <-  0
      } else {
        diff_m_binary[i,j] <- 1
      }
    }
  }  
  
  
  

# Save outputs ------------------------------------------------------------
  
  wb <- createWorkbook()
  
  addWorksheet(wb, "RawMatrix")
  writeData(wb, "RawMatrix", df_matrix, rowNames = TRUE)
  
  addWorksheet(wb, "DiffMat")
  writeData(wb, "DiffMat", diff_m, rowNames = TRUE)
  
  addWorksheet(wb, "DiffMat_Pos")
  writeData(wb, "DiffMat_Pos", diff_m_simp, rowNames = TRUE)
  
  addWorksheet(wb, "DiffMat_PopScaled")
  writeData(wb, "DiffMat_PopScaled", scaled_m, rowNames = TRUE)
  
  addWorksheet(wb, "DiffMat_PopScaled_Per1000")
  writeData(wb, "DiffMat_PopScaled_Per1000", scaled_m_per_1000, rowNames = TRUE)
  
  addWorksheet(wb, "DiffMat_PopScaled_Pos")
  writeData(wb, "DiffMat_PopScaled_Pos", scaled_m_net, rowNames = TRUE)
  
  addWorksheet(wb, "DiffMat_PopScaled_Per1000_Pos")
  writeData(wb, "DiffMat_PopScaled_Per1000_Pos", scaled_m_per_1000_net, rowNames = TRUE)
  
  addWorksheet(wb, "DM_10000_binary")
  writeData(wb, "DM_10000_binary", diff_m_binary, rowNames = TRUE)
  
  saveWorkbook(wb, paste0("DifferenceMatrices_", year, ".xlsx"), overwrite = TRUE)
  
}


# Run Function ------------------------------------------------------------

#1990
GenerateDiffMatrix(matrix_dir = "C:/Users/heblj/OneDrive/Career/Professional_Portfolio/Network_Analysis/MigrationAmericas_NetworkAnalysis/Immigration_Networks/MigrationData",
                   Raw_matrix = "Matrices/MigrationMatrix_1990.xlsx",
                   year = "1990",
                   populationCSV = "Population_1990.csv")


#2000
GenerateDiffMatrix(matrix_dir = "C:/Users/heblj/OneDrive/Career/Professional_Portfolio/Network_Analysis/MigrationAmericas_NetworkAnalysis/Immigration_Networks/MigrationData",
                   Raw_matrix = "Matrices/MigrationMatrix_2000.xlsx",
                   year = "2000",
                   populationCSV = "Population_2000.csv")


#2010
GenerateDiffMatrix(matrix_dir = "C:/Users/heblj/OneDrive/Career/Professional_Portfolio/Network_Analysis/MigrationAmericas_NetworkAnalysis/Immigration_Networks/MigrationData",
                   Raw_matrix = "Matrices/MigrationMatrix_2010.xlsx",
                   year = "2010",
                   populationCSV = "Population_2010.csv")


#2020
GenerateDiffMatrix(matrix_dir = "C:/Users/heblj/OneDrive/Career/Professional_Portfolio/Network_Analysis/MigrationAmericas_NetworkAnalysis/Immigration_Networks/MigrationData",
                   Raw_matrix = "Matrices/MigrationMatrix_2020.xlsx",
                   year = "2020",
                   populationCSV = "Population_2020.csv")













# Running Histograms to assess distribution via density -------------------

# Not totally clear to me what of this will be useful...

# based off net-migration (i.e. only the positive migration flows)
vals <- as.vector(scaled_m_per_1000_net)
vals_clean <- vals[vals != 0.00000]

hist(vals_clean,
     breaks = 1000,          # increase or decrease this number
     main = "Histogram of immigration density - 1990",
     xlab = "Density \n(immigrants/recieving-population)",
     col = "steelblue",
     border = "white",
     xlim = c(-5,5)
)

qs <- quantile(vals, probs = c(0.5, 0.75, 0.9))

abline(v = qs,
       col = c("darkgreen", "pink", "red"),
       lwd = 2,
       lty = 2)

# factor everything so that the 90% is 1.0 
#90% = 0.6377
#1/0.6377 = 1.568135

vals_fact <- vals_clean*1.568135

hist(vals_fact,
     breaks = 1000,          # increase or decrease this number
     main = "Histogram of immigration density - 1990",
     xlab = "Density \n(immigrants/recieving-population factored)",
     col = "steelblue",
     border = "white",
     xlim = c(-5,5)
)

qs_fact <- quantile(vals_fact, probs = c(0.5, 0.75, 0.9))
abline(v = qs_fact,
       col = c("darkgreen", "pink", "red"),
       lwd = 2,
       lty = 2)



