
# Import required libraries

list.of.packages <- c("ggplot2", "gridExtra", "reshape2", "cowplot", "cluster", "mclust")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(ggplot2)
library(gridExtra)
library(reshape2)
library(cowplot)
library(cluster)
library(mclust)

# Function to split a rainfall timeseries vector into events (separated by a minimum of 'n' zeros)

stormSplitting <- function(rainvec, conseqZeros) {
  
  ################################################
  #Initialize list and number of zeros to define a storm
  stormList = list()
  listIndex = 1
  lastIndex = length(rainvec)
  #lastIndex = 16
  #conseqZeros = 3
  ################################################
  
  
  #Eliminate leading zeros before assigning storm start index
  #Sanity check to make sure that vector is not all zeros
  
  firstNonZero = min(which(rainvec!=0))
  if (!is.infinite(firstNonZero)) {   
    stormStartIndex = firstNonZero
    stormEndIndex = length(rainvec)
    
    #First check if there are any zeros in the vector (if not, a single storm)
    if (any(rainvec[stormStartIndex:stormEndIndex]==0)) {
      #initalize runlength
      runlength = 0  
      
      ###Loop over each element of the vector
      for (j in firstNonZero:lastIndex) {
        #print(j)
        
        if (stormStartIndex > j) next
        #if the current element is not zero, reset runlength counter
        if (rainvec[j] != 0) {
          runlength = 0
          next
        } 
        
        #if the current element is zero, increment the runlength counter
        if (rainvec[j] == 0) {   
          runlength = runlength + 1
        }
        
        #check if we have reached the number of zeros needed to define a separate storm
        if (runlength < conseqZeros) {
          next 
        } else {
          
          stormEndIndex = j
          storm = rainvec[stormStartIndex:stormEndIndex]
          
          #Trim trailing zeros (always conseqZeros?) and add storm vector to list
          storm = storm[1:max(which(storm!=0))]
          if (sum(storm) > 0.01) {
            stormList[[listIndex]] = storm
            listIndex = listIndex + 1
          }
          runlength = 0
          
          ##Need to jump to next non-zero element rest of the vector if not already at en
          if (j == length(rainvec)) {
            break
          }
          temp = rainvec[(j+1): lastIndex]
          
          sanity_check = min(which(temp!=0)) ##check if all only all zeros left
            if (is.infinite(sanity_check)) {
              break
            } else {
            index = min(which(temp!=0))
            stormStartIndex = j+index
            }
        }
      }
    
      ###Need to add the last storm into the list
      #print(stormStartIndex)
      #print(stormEndIndex)
      if ((stormStartIndex >= stormEndIndex) && (stormStartIndex <= length(rainvec))) {
        storm = rainvec[stormStartIndex:length(rainvec)]
        storm = storm[1:max(which(storm!=0))] #Trim trailing zeros
        if (sum(storm) > 0.01) {
        stormList[[listIndex]] = storm
        }
      }
    
    
    } else {
        stormList[[listIndex]] = rainvec
    }
  
  }
  return(stormList) 

}

# Additional helper functions to make rainfall characteristic variables
countZeros = function(vec) {
  return(length(which(vec==0)))
}

getMeanOverRainyPeriod = function(vec) {
  total = sum(vec)
  duration = length(vec)
  dryDuration = countZeros(vec)
  wetDuration = duration - dryDuration
  return(total/wetDuration)
}

getPeakPosition = function(vec) {
  index = which.max(vec)
  position = index/length(vec)
  return(position)
}

# Function representing convective strength of a storm    
getBeta = function(vec) {
  L = 5
  total = sum(vec)
  numerator = 0
  for (i in 1:length(vec)) {
    if (vec[i] >= L) {
      numerator = numerator + (vec[i])
    }
  }
  return(numerator/total)
}
         
pop.var <- function(x) var(x) * (length(x)-1) / length(x)
pop.sd <- function(x) sqrt(pop.var(x))

# Read in rainfall data and split into events
setwd("C:/Users/jordanna/Documents")
forcing_data = read.table("TrinidadCLM_2004to2010_RepeatTwo.dat", header=FALSE,
                          sep="\t", skip = 0)
rainfallVec = forcing_data$V3*3600
nConsecutive = 8

Storms <- stormSplitting(rainfallVec, nConsecutive)

# Define rainfall variables
durations <- mapply(length, Storms)
durationsQuantile <- quantile(durations,c(0.25,0.5,0.75,0.9,0.95,0.99,0.999))

totals <- mapply(sum, Storms)
meanRainRate = totals/durations
maxHourlyIntensity = mapply(max, Storms)
intradryDuration = mapply(countZeros, Storms)
rainyDuration = durations - intradryDuration
meanRainRateOverRainyPeriod = mapply(getMeanOverRainyPeriod, Storms)
positionPeak = mapply(getPeakPosition, Storms)
convectiveStrength = mapply(getBeta, Storms)
dryPercentageInEvent = mapply(countZeros, Storms)/durations
standardDev = mapply(pop.sd, Storms)


#Make sure features have the same magnitude (normalize)
rainfalldata = data.frame(totals, durations, meanRainRate, maxHourlyIntensity, dryPercentageInEvent, intradryDuration, 
                          meanRainRateOverRainyPeriod, positionPeak, convectiveStrength, rainyDuration)
rainfalldata.scaled = scale(rainfalldata)


# Basic clustering with kmeans and PAM

set.seed(25)
k = 3
# Using all data
clusters = kmeans(rainfalldata.scaled[,c(1:10)], k)
rainfalldata$cluster = as.factor(clusters$cluster)
plot1 = ggplot(data = rainfalldata) + 
    geom_point(aes(y = totals, x = durations, color = as.factor(cluster))) +
    ggtitle("Rainfall Data using kmeans") + ylim(0,180)
clusplot(rainfalldata[,c(1:9)], rainfalldata$cluster, color = T, shade = T)

# Using only rainfall totals and durations
rainfalldata_2 = rainfalldata.scaled[,1:2]
clusters_2 = kmeans(rainfalldata_2, k)
clusplot(rainfalldata[,1:2], clusters_2$cluster, color = T, shade = T)

# Partitioning around mediods with totals, durations, intensity variables
clusters_pam = pam(rainfalldata.scaled[,c(1:4)], k)
plot(clusters_pam)

rainfalldata$pamcluster = pam(rainfalldata.scaled[,c(1:4,7)], k, cluster.only = T)
clusplot(rainfalldata[,c(1:4,7)], rainfalldata$pamcluster, color = T, shade = T)


