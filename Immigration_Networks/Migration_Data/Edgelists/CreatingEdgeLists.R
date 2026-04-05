### Constructing an EdgeList from an adjacency matrix


#Clean-up First
rm(list = ls())
gc()



# Load Space ----
library(readxl)
library(tidyverse)
library(openxlsx)


GenerateEdgeListFx <- function(parent_dir, 
                               DifferenceMatrices_xlsx, 
                               EdgeList_dir, 
                               year) {
  
  # Set working directory
  setwd(parent_dir)
  
  
  # Load Data ----
  
  ## First binary matrix
  
  ### Read in matrix (which technically is a data.frame)
  m_binary <- read.xlsx(xlsxFile = paste0("Matrices/",DifferenceMatrices_xlsx),
                        sheet = "DM_10000_binary",
                        colNames = TRUE,
                        rowNames = TRUE)
  
  m_binary <- as.matrix(m_binary) # converts to a matrix
  
  
  ## Next, absolute-immigration-amount matrix
  m_abs <- read.xlsx(xlsxFile = paste0("Matrices/",DifferenceMatrices_xlsx),
                     sheet = "DiffMat_Pos",
                     colNames = TRUE,
                     rowNames = TRUE)
  
  m_abs <- as.matrix(m_abs)
  
  
  ## Next, population-scaled matrix (i.e. migrants per 1000 DESTINATION-country citizens)
  m_scale_dest <- read.xlsx(xlsxFile = paste0("Matrices/",DifferenceMatrices_xlsx),
                            sheet = "DiffMat_PopScaled_Per1000_dest",
                            colNames = TRUE,
                            rowNames = TRUE)
  
  m_scale_dest <- as.matrix(m_scale_dest)
  
  
  ## Next, population-scaled matrix (i.e. migrants per 1000 ORIGIN-country citizens)
  m_scale_org <- read.xlsx(xlsxFile = paste0("Matrices/",DifferenceMatrices_xlsx),
                           sheet = "DiffMat_PopScaled_Per1000_org",
                           colNames = TRUE,
                           rowNames = TRUE)
  
  m_scale_org <- as.matrix(m_scale_org)
  
  
  
  # Construct Edgelist ----
  
  ## First the base edgelist: binary
  EL_0 <- as.data.frame(as.table(m_binary)) %>% # Convert matrix to a long table
    rename(
      Origin = Var1,
      Destination = Var2,
      Binary = Freq
    )
  
  ## Edgelist with absolution migration
  EL_abs <- as.data.frame(as.table(m_abs)) %>% # Convert matrix to a long table
    rename(
      Origin_2 = Var1,
      Destination_2 = Var2,
      Immigration_absolute = Freq
    )
  
  ## Edgelist with immigration relative to 1000-citizens of DESTINATION
  EL_dest <- as.data.frame(as.table(m_scale_dest)) %>% # Convert matrix to a long table
    rename(
      Origin_3 = Var1,
      Destination_3 = Var2,
      per1000_dest = Freq
    )
  
  ## Edgelist with absolution migration
  EL_pop <- as.data.frame(as.table(m_scale_org)) %>% # Convert matrix to a long table
    rename(
      Origin_4 = Var1,
      Destination_4 = Var2,
      per1000_org = Freq
    )
  
  
  
  ## Now bind the two edgelists to add the immigration-density to the base edgelist
  EL_1 <- EL_0 %>%
    cbind(EL_abs, EL_dest, EL_pop) %>%
    select(Origin, Destination, Binary, Immigration_absolute, per1000_dest, per1000_org)
  
  
  EL_2 <- EL_1[EL_1$Binary==1,] %>% # Filter for only binary values equal to 1, i.e. a true "edge"
    select(Origin, Destination, Immigration_absolute, per1000_dest, per1000_org) # Remove the binary column. 
  
  
  # Save EdgeList ----
  
  write.csv(EL_2,
            file = paste0(EdgeList_dir,"/EdgeList_",year,".csv"),
            row.names = FALSE
  )
  
  
}




# Running Code ------------------------------------------------------------

#1990
GenerateEdgeListFx(parent_dir = r"(C:\Users\heblj\OneDrive\Career\Professional_Portfolio\Network_Analysis\MigrationAmericas_NetworkAnalysis\Immigration_Networks\MigrationData\)",
                   DifferenceMatrices_xlsx = "DifferenceMatrices_1990.xlsx",
                   EdgeList_dir = r"(C:\Users\heblj\OneDrive\Career\Professional_Portfolio\Network_Analysis\MigrationAmericas_NetworkAnalysis\Immigration_Networks\MigrationData\Edgelists)",
                   year = 1990)

#2000
GenerateEdgeListFx(parent_dir = r"(C:\Users\heblj\OneDrive\Career\Professional_Portfolio\Network_Analysis\MigrationAmericas_NetworkAnalysis\Immigration_Networks\MigrationData\)",
                   DifferenceMatrices_xlsx = "DifferenceMatrices_2000.xlsx",
                   EdgeList_dir = r"(C:\Users\heblj\OneDrive\Career\Professional_Portfolio\Network_Analysis\MigrationAmericas_NetworkAnalysis\Immigration_Networks\MigrationData\Edgelists)",
                   year = 2000)

#2010
GenerateEdgeListFx(parent_dir = r"(C:\Users\heblj\OneDrive\Career\Professional_Portfolio\Network_Analysis\MigrationAmericas_NetworkAnalysis\Immigration_Networks\MigrationData\)",
                   DifferenceMatrices_xlsx = "DifferenceMatrices_2010.xlsx",
                   EdgeList_dir = r"(C:\Users\heblj\OneDrive\Career\Professional_Portfolio\Network_Analysis\MigrationAmericas_NetworkAnalysis\Immigration_Networks\MigrationData\Edgelists)",
                   year = 2010)

#2020
GenerateEdgeListFx(parent_dir = r"(C:\Users\heblj\OneDrive\Career\Professional_Portfolio\Network_Analysis\MigrationAmericas_NetworkAnalysis\Immigration_Networks\MigrationData\)",
                   DifferenceMatrices_xlsx = "DifferenceMatrices_2020.xlsx",
                   EdgeList_dir = r"(C:\Users\heblj\OneDrive\Career\Professional_Portfolio\Network_Analysis\MigrationAmericas_NetworkAnalysis\Immigration_Networks\MigrationData\Edgelists)",
                   year = 2020)
