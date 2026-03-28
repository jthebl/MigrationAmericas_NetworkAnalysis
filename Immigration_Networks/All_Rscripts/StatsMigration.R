rm(list=ls())

#### Code for running additional stats on Immigration Data #####
#### Relevant links: https://www.un.org/development/desa/pd/content/international-migrant-stock #####

#### Notes ####


# Setting up
setwd("C:/Users/heblj/OneDrive - Middlebury College/MIIS_Fall 2024/Intro Network Analysis/Final Project/Country Immigration Data Excels")

M_Per1000_Stats <- read.csv("StatsMigrationCSV.csv",
                            row.names = 1)

# Paired T-tests
ttr_90_20 <- t.test(M_Per1000_Stats[,3], M_Per1000_Stats[,12], paired = TRUE)
ttr_90_20
#	Paired t-test
# data:  M_Per1000_Stats[, 3] and M_Per1000_Stats[, 12]
# t = 1.7154, df = 19, p-value = 0.1025
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   -12.46246 125.69264
# sample estimates:
#   mean difference 
# 56.61509 

ttr_90_10 <- t.test(M_Per1000_Stats[,3], M_Per1000_Stats[,9], paired = TRUE)
ttr_90_10
#Paired t-test

# data:  M_Per1000_Stats[, 3] and M_Per1000_Stats[, 9]
# t = 1.8578, df = 19, p-value = 0.07877
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   -5.28007 88.66540
# sample estimates:
#   mean difference 
# 41.69267 

ttr_90_00 <- t.test(M_Per1000_Stats[,3], M_Per1000_Stats[,6], paired = TRUE)
ttr_90_00
# Paired t-test
# 
# data:  M_Per1000_Stats[, 3] and M_Per1000_Stats[, 6]
# t = 1.5676, df = 19, p-value = 0.1335
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   -5.42636 37.80505
# sample estimates:
#   mean difference 
# 16.18934 

ttr_00_20 <- t.test(M_Per1000_Stats[,6], M_Per1000_Stats[,12], paired = TRUE)
ttr_00_20
# Paired t-test
# 
# data:  M_Per1000_Stats[, 6] and M_Per1000_Stats[, 12]
# t = 1.6013, df = 19, p-value = 0.1258
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   -12.41253  93.26402
# sample estimates:
#   mean difference 
# 40.42575 
