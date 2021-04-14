#Set working directory (MAC)
setwd("/Users/isabellachittumuri/Desktop/Project")

#Packages
library(dplyr)
library(tidyverse)
library(readxl)
library(readr)
library(qualityTools)

#Import excel file
df <- read.csv("PM2.5.csv", header = T)

#Only keep rows for year 2017, month 1
df2 <- df[c(33649:34392),]

#Only keep columns: hour, PM2.5
df3 <- df2[c(5, 6)]

#Mean of PM2.5 for every 24 hours
n.colmeans = function(df3, n = 24){
  aggregate(x = df3,
            by = list(gl(ceiling(nrow(df3)/n), n)[1:nrow(df3)]),
            FUN = mean)
}

t <- n.colmeans(df3, 24)

#Put those means into data frame
df4 = data.frame(t)

#Keep only 2 columns
df5 <- df4[c(1,3)]

#Target Population: Rename columns
names(df5)[1] <- "Day"
names(df5)[2] <- "24-hr PM2.5"

#Mean, removing the one NA
mean(df5$`24-hr PM2.5`, na.rm = T)

#Median, removing the one NA
median(df5$`24-hr PM2.5`, na.rm = T)

#median is actaully 74.20833
print(74.20833)

#Mode
getmode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}
v <- df5$`24-hr PM2.5`
result <- getmode(v)
print(result)

#Mode is actually between 20-40
print(30)

#Range, removing the one NA
range(df5$`24-hr PM2.5`, na.rm = T)

#Unajusted Variance (sigma squared), removing the one NA
a <- c(df5$`24-hr PM2.5`)
n = length(a)
var(a,na.rm = T)*(n-1)/n

#Unajusted Standard Deviation (sigma)
sqrt(17016.67)

#Adjusted Variance (S squared), removing the one NA
var(df5$`24-hr PM2.5`, na.rm = T)

#Adjusted Standard Deviation (S), removing the one NA
sd(df5$`24-hr PM2.5`, na.rm = T)

#Frequency Histogram Plot 
hist(df5$'24-hr PM2.5', breaks = 31,
     main="24-hr PM2.5 Concentration Levels in Beijing (Jan, 2017)", 
     xlab="24-hr PM2.5 Concentration Level (ug/m3)", 
     col="lightblue")

#Data summary
summary(df5$`24-hr PM2.5`) 

#Reduced Target Population of size N=8
df6 = data.frame(c(13.08, 36.67, 38.22, 50.5, 74.21, 178.25, 185.31, 268.92))

#Rename column CO Reduced
names(df6)[1] <- "Reduced 24-hr PM2.5"

#Mean for Reduced
mean(df6$`Reduced 24-hr PM2.5`)

#Unajusted Variance (sigma squared) for Reduced
b <- c(df6$`Reduced 24-hr PM2.5`)
m = length(b)
var(b)*(m-1)/m

#Unajusted Standard Deviation (sigma) for Reduced
sqrt(7522.227)

#Adjusted Variance (S squared) for Reduced
var(df6$`Reduced 24-hr PM2.5`)

#Adjusted Standard Deviation (S) for Reduced
sd(df6$`Reduced 24-hr PM2.5`)

#Frequency Histogram Plot for Reduced
hist(df6$`Reduced 24-hr PM2.5`, breaks = 37,
     main="Reduced 24-hr PM2.5 Concentration Levels in Beijing (Jan, 2017)", 
     xlab="24-hr PM2.5 Concentration Level (ug/m3)", 
     col="lightgreen") 

#Sampling distribution of size n=3
all_combos <- t(combn(df6$`Reduced 24-hr PM2.5`,3)); all_combos

#Convert to a data frame
sampl_distr = data.frame(all_combos)

#Calculate sample means for each sample in sampling distribution
sampl_distr$mean = rowMeans(sampl_distr, na.rm = TRUE)

#Frequency Histogram Plot of Sampling Distribution of Sample Means
hist(sampl_distr$mean, breaks = 64,
     main="Sample Distribution of the Sample Means", 
     xlab="Sample Mean of 24-hr PM2.5 Concentration (ug/m3)", col="lightpink")

#Mean of Sample Means E(ybar)
mean(sampl_distr$mean)

#compare previous mean to mean of reduced pop
mean(df6$`Reduced 24-hr PM2.5`)

#Median of Sample Means
median(sampl_distr$mean)

#Mode of Sample Means
getmode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}
v <- sampl_distr$mean
result <- getmode(v)
print(result)

#Adjusted Variance of Reduced Target Popluation (S^2)
var(df6$`Reduced 24-hr PM2.5`)

#Ajusted Variance of Sample Means (equals unajusted variance of target pop) 
(1-(3/8))*(8596.831/3)

#compare previous answer to unajusted variance of Sample Means
d <- c(sampl_distr$mean)
p = length(d)
var(d)*(p-1)/p

#Select 3 Samples from Sample Distribution
q <- sample_n(sampl_distr, 3, replace = F)

#Selection without Mean
q_2 <- dplyr::select(q,X1,X2,X3)

#Tranpose to have X values turned into Y values
q_2_transpose <- as.data.frame(t(as.matrix(q_2)))

#Variance of the Selected 3 Samples
var(q_2_transpose$V1)
var(q_2_transpose$V2)
var(q_2_transpose$V3)

#Ajusted variance of 3 samples using their s^2 (compare these #s to variance of sample means)
(1-(3/8))*(var(q_2_transpose$V1)/3)
(1-(3/8))*(var(q_2_transpose$V2)/3)
(1-(3/8))*(var(q_2_transpose$V3)/3)


