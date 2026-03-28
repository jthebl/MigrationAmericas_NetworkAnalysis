# Load necessary libraries
install.packages("readxl")
install.packages("dplyr")
library(readxl)
library(dplyr)

# Creating Edgelist with Weights ####
# For 2000 ####
# Load the Excel file
file_path <- "edgelist_2000.csv"
df <- read.csv(file_path)

# Convert the data frame to a matrix and set row names
matrix_data <- as.matrix(df[-1])
rownames(matrix_data) <- df[[1]]

# Initialize an empty data frame to store edges
edges <- data.frame(Source = character(), Target = character(), stringsAsFactors = FALSE)

# Iterate over the rows and columns of the matrix
for (i in 1:nrow(matrix_data)) {
  for (j in 1:ncol(matrix_data)) {
    if (is.na(matrix_data[i, j])) {
      next  # Skip to the next iteration of the inner loop if the value is NA
    }
    if (matrix_data[i, j] != 0) {
      edges <- rbind(edges, data.frame(Source = rownames(matrix_data)[i], Target = colnames(matrix_data)[j], Weight = matrix_data[i, j]))
    }
  }
}

# Replace "United.States" with "United States" in the edges data frame
edges$Source <- gsub("United\\.States", "United States", edges$Source)
edges$Target <- gsub("United\\.States", "United States", edges$Target)
# Save the edges data frame to a CSV file
write.csv(edges, file = "EL_00.csv", row.names = FALSE)

print("Edgelist has been successfully created and saved to 'edgelist.csv'.")




# For 1990 ####
# Load the Excel file
file_path <- "edgelist_90.csv"
df <- read.csv(file_path)

# Convert the data frame to a matrix and set row names
matrix_data <- as.matrix(df[-1])
rownames(matrix_data) <- df[[1]]

# Initialize an empty data frame to store edges
edges <- data.frame(Source = character(), Target = character(), stringsAsFactors = FALSE)

# Iterate over the rows and columns of the matrix
for (i in 1:nrow(matrix_data)) {
  for (j in 1:ncol(matrix_data)) {
    if (is.na(matrix_data[i, j])) {
      next  # Skip to the next iteration of the inner loop if the value is NA
    }
    if (matrix_data[i, j] != 0) {
      edges <- rbind(edges, data.frame(Source = rownames(matrix_data)[i], Target = colnames(matrix_data)[j]))
    }
  }
}

# Replace "United.States" with "United States" in the edges data frame
edges$Source <- gsub("United\\.States", "United States", edges$Source)
edges$Target <- gsub("United\\.States", "United States", edges$Target)
# Save the edges data frame to a CSV file
write.csv(edges, file = "EL_90.csv", row.names = FALSE)

print("Edgelist has been successfully created and saved to 'edgelist.csv'.")




# For 2010 ####
# Load the Excel file
file_path <- "edgelist_10.csv"
df <- read.csv(file_path)

# Convert the data frame to a matrix and set row names
matrix_data <- as.matrix(df[-1])
rownames(matrix_data) <- df[[1]]

# Initialize an empty data frame to store edges
edges <- data.frame(Source = character(), Target = character(), stringsAsFactors = FALSE)

# Iterate over the rows and columns of the matrix
for (i in 1:nrow(matrix_data)) {
  for (j in 1:ncol(matrix_data)) {
    if (is.na(matrix_data[i, j])) {
      next  # Skip to the next iteration of the inner loop if the value is NA
    }
    if (matrix_data[i, j] != 0) {
      edges <- rbind(edges, data.frame(Source = rownames(matrix_data)[i], Target = colnames(matrix_data)[j]))
    }
  }
}

# Replace "United.States" with "United States" in the edges data frame
edges$Source <- gsub("United\\.States", "United States", edges$Source)
edges$Target <- gsub("United\\.States", "United States", edges$Target)
# Save the edges data frame to a CSV file
write.csv(edges, file = "EL_10.csv", row.names = FALSE)

print("Edgelist has been successfully created and saved to 'edgelist.csv'.")




# For 2020 ####
# Load the Excel file
file_path <- "edgelist_20.csv"
df <- read.csv(file_path)

# Convert the data frame to a matrix and set row names
matrix_data <- as.matrix(df[-1])
rownames(matrix_data) <- df[[1]]

# Initialize an empty data frame to store edges
edges <- data.frame(Source = character(), Target = character(), stringsAsFactors = FALSE)

# Iterate over the rows and columns of the matrix
for (i in 1:nrow(matrix_data)) {
  for (j in 1:ncol(matrix_data)) {
    if (is.na(matrix_data[i, j])) {
      next  # Skip to the next iteration of the inner loop if the value is NA
    }
    if (matrix_data[i, j] != 0) {
      edges <- rbind(edges, data.frame(Source = rownames(matrix_data)[i], Target = colnames(matrix_data)[j]))
    }
  }
}

# Replace "United.States" with "United States" in the edges data frame
edges$Source <- gsub("United\\.States", "United States", edges$Source)
edges$Target <- gsub("United\\.States", "United States", edges$Target)
# Save the edges data frame to a CSV file
write.csv(edges, file = "EL_20.csv", row.names = FALSE)

print("Edgelist has been successfully created and saved to 'edgelist.csv'.")


# Creating graphs from matrix via edgelist

# For 1990 ####
network_binary_90 <- read.csv("1990_Immigration Matrix_Binary_simplified.csv",
                    header=T,
                    row.names=1,
                    check.names = F)

# Convert the data frame to a matrix and set row names
matrix_data_90 <- as.matrix(network_binary_90[-1])
rownames(matrix_data) <- network_binary_90[[1]]

# Initialize an empty data frame to store edges
edges_90 <- data.frame(Source = character(), Target = character(), stringsAsFactors = FALSE)

# Iterate over the rows and columns of the matrix
for (i in 1:nrow(matrix_data_90)) {
  for (j in 1:ncol(matrix_data_90)) {
    if (is.na(matrix_data_90[i, j])) {
      next  # Skip to the next iteration of the inner loop if the value is NA
    }
    if (matrix_data_90[i, j] != 0) {
      edges <- rbind(edges, data.frame(Source = rownames(matrix_data_90)[i], Target = colnames(matrix_data_90)[j], Weight = matrix_data_90[i, j]))
    }
  }
}

# Replace "United.States" with "United States" in the edges data frame
edges_90$Source <- gsub("United\\.States", "United States", edges_90$Source)
edges_90$Target <- gsub("United\\.States", "United States", edges_90$Target)
# Save the edges data frame to a CSV file
write.csv(edges_90, file = "EL_90.csv", row.names = FALSE)

