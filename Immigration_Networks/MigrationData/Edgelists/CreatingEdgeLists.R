### Constructing an EdgeList from an adjacency matrix


# Load Space ----
library(readxl)
library(tidyverse)
library(openxlsx)

# Set working directory
setwd("C:/Users/heblj/OneDrive/Career/Professional_Portfolio/Network_Analysis/MigrationAmericas_NetworkAnalysis/Immigration_Networks/MigrationData")

# Load Data ----

## First binary matrix

### Read in matrix (which technically is a data.frame)
m_binary <- read.xlsx(xlsxFile = "Matrices/DifferenceMatrices_1990.xlsx",
                      sheet = "DM_10000_binary",
                      colNames = TRUE,
                      rowNames = TRUE)

m_binary <- as.matrix(m_binary) # converts to a matrix

## Next, population-scaled matrix (i.e. migrants per 1000 receiving-country citizens)
m_scale <- read.xlsx(xlsxFile = "Matrices/DifferenceMatrices_1990.xlsx",
                     sheet = "DiffMat_PopScaled_Per1000_Pos",
                     colNames = TRUE,
                     rowNames = TRUE)

m_scake <- as.matrix(m_scale)



# Construct Edgelist
EL_0 <- as.data.frame(as.table(m_binary)) %>% # Convert matrix to a long table
  rename(
    Origin = Var1,
    Destination = Var2,
    Binary = Freq
  )
    

EL_1 <- EL_0[EL_0$Binary==1,] %>% # Filter for only binary values equal to 1, i.e. a true "edge"
  select(Origin, Destination) # Remove the binary column. 
  


# Rename columns as requested
colnames(df_ones) <- c("ColumnID", "RowID")
