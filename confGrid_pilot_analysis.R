############### Confidence and Categorisation: ConfGrid ########################
#Study 4 - Data Analysis Script 

#Sarah Ashcroft-Jones 
#sarahashjones@gmail.com
#GitHub: sarahajones

############### 0.1 Script Set-Up ##############################################
#LOAD IN PACKAGES
library(tidyverse)
library(readr) 
library(plyr)
library(dplyr)
library(ggplot2)

library(reshape)

#DATA LOADING
mydir = setwd("C:/Users/A-J/Desktop/confGrid/raw_data")
myfile = list.files(path=mydir, pattern="zapGrid*", full.names=TRUE) #pull file
dat_csv = ldply(myfile, read_csv) #load in data
numParticipants <- 58

#create a PID code to identify participants more clearly. 
user_ids <- unique(dat_csv$user_id) # cache this to save recalculating 
dat_csv$PID <- NA_real_ # initalise PIDs as NA
dat_csv$PID <- sapply(
  dat_csv$user_id,  # vector to iterate over
  function(id) which(user_ids == id)  # function to apply to each element
)
#remove identifiable information - user_id column
dat_csv <- subset(dat_csv, select = -(user_id))

rm(mydir, myfile, user_ids)

############### 1.0 Quick data quality checks ################################
#PARTICIPANT COMMENTS
#check on the recorded comments
feedback_strat <- subset(dat_csv, feedback_strategy != 'NA')
feedback_strat <- feedback_strat$feedback_strategy;feedback_strat
#seem to follow memory largely? some differentiation between color and size  

feedback_tech <- subset(dat_csv, feedback_technical != 'NA')
feedback_tech <- feedback_tech$feedback_technical;feedback_tech
#no tech issues reported 

feedback_thought <- subset(dat_csv, feedback_comments != 'NA')
feedback_thought <- feedback_thought$feedback_comments; feedback_thought
# enjoyed/positive x1 says too difficult

rm(feedback_thought,feedback_strat,feedback_tech)

#DEMOGRAPHICS
#what is the age, mean, range?
ageData <- subset(dat_csv, participantAge != "NA")
summary(ageData$participantAge)#mean 33.78
range(ageData$participantAge) #19-57
sd(ageData$participantAge) #10.3

#what is the gender split?
genderData <- subset(dat_csv, participantGender != 'NA')
gender <- genderData$participantGender #30 female 28male
female <- sum(gender == 'female')
male <- sum(gender == 'male')
nonbinary <- sum(gender == 'non-binary') #0

#what handedness do we have?
handData <- subset(dat_csv, participantHandedness != 'NA')
handedness <- handData$participantHandedness
right <- sum(handedness == "right") #54 right, 2 left
ambi <- sum(handedness == 'ambidextrous') #2ambi

rm(gender, female, male, nonbinary, handedness, right, ambi, ageData, genderData, handData)

#lets have a look at confidence across the space 
dat_csv$confidence <- as.numeric(dat_csv$confidence)
confidence <- subset(dat_csv,is.na(confidence) == FALSE)
confidence <- subset(dat_csv, confidence != "null") #we have just over 9000 trials

confidence$fish_color <- 1- confidence$fish_color #flip color

ggplot(data=confidence, aes(fish_size)) + #the trials worked - labelling was correct
  geom_point(mapping = aes(x = fish_size, y = fish_color, color = distribution_name))

ggplot(data=confidence, aes(fish_size)) + 
  geom_point(mapping = aes(x = fish_size, y = fish_color, shape = distribution_name, color = confidence)) +
  scale_color_gradient(low = "blue", high = "red") +
  facet_wrap( ~ PID)

#plotting confidence scores normalised, (z-scored within participant)
meanConf <- confidence %>%
  group_by(PID) %>%
  summarise(mean(confidence))

sdConf <- confidence %>%
  group_by(PID) %>%
  summarise(sd(confidence))

zConf <- matrix(data = NA, nrow = 160, ncol = numParticipants)
pps <- 1:numParticipants

for(i in pps){
  PIDConf <- subset(confidence, PID == i)
  #z = (x-μ)/σ
  trials <- 1:length(PIDConf$trial_index)
  for(j in trials){
  x <- (PIDConf$confidence[j] - meanConf[i,2])/(sdConf[i,2])
  zConf[j,i] <- as.numeric(x)
  }
}

#wrangle zConf back into a single column -remove NA to merge with confidence dataframe
zConfCol <- as.data.frame(zConf[,1])
names(zConfCol)[1] <- 'conf'
pp <- 2:length(pps)
for(i in pp){
  z <-as.data.frame(zConf[,i])
  names(z)[1] <- 'conf'
  zConfCol <- rbind(zConfCol, z)
}

zConf <- subset(zConfCol, is.na(conf)!= TRUE) #remove the 17 missing trials to match up the oclumns with confidence

confidence$zConf <- zConf$conf

#which might bring out some variability that's hard to see in these full-scale plots
ggplot(data=confidence, aes(fish_size)) + 
  geom_point(mapping = aes(x = fish_size, y = fish_color, shape = distribution_name, color = zConf)) +
  scale_color_gradient(low = "blue", high = "red") +
  facet_wrap( ~ PID)

#and perhaps to 'valence' the color scale to indicate the actual judgment made (native/invasive).
ggplot(data=confidence, aes(fish_size)) + 
  geom_point(mapping = aes(x = fish_size, y = fish_color, shape = distribution_name, color = confidence, alpha = button_label)) +
  scale_color_gradient(low = "blue", high = "red") +
  scale_alpha_manual(values=c(0.2,0.5)) +
  facet_wrap( ~ PID)

############### 2.0 Comparing models of confidence #############################
#start generating predictions for different possible models of confidence in the task, 
#and we can ask (e.g., via a simple measure like mean-squared error or correlation, 
#or perhaps something more sophisticated [tbd]) which of these models best fits the data -- 
#of individual subjects, and/or in aggregate. 
#Some simple models we could consider to get us started are:
#• Summed distance to bound (along the two dimensions)
#• Minimum distance to bound (out of the two dimensions)
#• Maximum distance to bound (out of the two dimensions)
#The idea would be to calculate these for each point in the 2D space to compare with people's actual reported confidence. 
#Simple correlation with these might be a useful first step, and then we could make things more complex 
#(e.g., allowing confidence to be some non-linear function of the measured distance).

modelData <- confidence[c("PID", "distribution_name","grid_box", "confidence", "fish_color", "fish_size")]

#lets put size in the same space as color 0-1 space
modelData$fish_size <- ((modelData$fish_size-200)/600)

# check which are the most relevant dimensions for which area of the categroy space
modelData <- modelData %>%
  mutate(relevantBoundary = case_when(
    grid_box == "box3_1" ~ 1, #"size",
    grid_box == "box3_2" ~ 1, #"size",
    grid_box == "box2_1" ~ 1, #"size",
    grid_box == "box1_3" ~ 2, #"color",
    grid_box == "box2_3" ~ 2, #"color",
    grid_box == "box1_2" ~ 2, #"color",
    TRUE ~ 3 #both potentially or trial dependent ~ will have to check trial by trial
  )) 

#Let's first consider
#• Minimum distance to bound (out of the two dimensions)
#if confidence is scaled relative to the minimum distance to the bound out of the two dimension
#is the minimum distance reduces  to 0 then confidence is 0, if it is maximal then confidence is 1

modelData <- modelData %>% 
  mutate(minimumDistance = case_when (
    relevantBoundary == 1 ~ abs((1/3) - fish_size),
    relevantBoundary == 2 ~ abs((1/3) - fish_color),
    )) %>% #now let's look at the border cases 
  mutate(minDistCol = case_when (
    grid_box == "box1_1" ~ abs((1/3) - fish_color),
    grid_box == "box2_2" ~ abs((1/3) - fish_color),
    grid_box == "box3_3" ~ abs((1/3) - fish_color)
  )) %>%
  mutate(minDistSize = case_when (
    grid_box == "box1_1" ~ abs((1/3) - fish_size),
    grid_box == "box2_2" ~ abs((1/3) - fish_size),
    grid_box == "box3_3" ~ abs((1/3) - fish_size)
  )) %>% 
  mutate(minimumDistance = case_when (
    relevantBoundary == 3 & (minDistCol-minDistSize)> 0 ~ minDistSize,
    relevantBoundary == 3 & (minDistCol-minDistSize)< 0 ~ minDistCol,
    TRUE ~ minimumDistance #minimum distance is scaled from 0-0.66
  )) %>%
  mutate(minimumDistance = ((minimumDistance/(2/3))*100))
  
modelData <- subset(modelData, select = -c(minDistCol, minDistSize))

#• Maximum distance to bound (out of the two dimensions)
modelData <- modelData %>% 
  mutate(maximumDistance = case_when (
    relevantBoundary == 1 ~ abs((1/3) - fish_size),
    relevantBoundary == 2 ~ abs((1/3) - fish_color),
  )) %>% #now let's look at the border cases 
  mutate(maxDistCol = case_when (
    grid_box == "box1_1" ~ abs((1/3) - fish_color),
    grid_box == "box2_2" ~ abs((1/3) - fish_color),
    grid_box == "box3_3" ~ abs((1/3) - fish_color)
  )) %>%
  mutate(maxDistSize = case_when (
    grid_box == "box1_1" ~ abs((1/3) - fish_size),
    grid_box == "box2_2" ~ abs((1/3) - fish_size),
    grid_box == "box3_3" ~ abs((1/3) - fish_size)
  )) %>% 
  mutate(maximumDistance = case_when (
    relevantBoundary == 3 & (maxDistCol-maxDistSize)> 0 ~ maxDistCol,
    relevantBoundary == 3 & (maxDistCol-maxDistSize)< 0 ~ maxDistSize,
    TRUE ~ maximumDistance 
  )) %>%
  mutate(maximumDistance = ((maximumDistance/(2/3))*100))

modelData <- subset(modelData, select = -c(maxDistCol, maxDistSize))

#• Summed distance to bound (along the two dimensions)
#• Maximum distance to bound (out of the two dimensions)
modelData <- modelData %>% 
  mutate(summedDistance = case_when (
    relevantBoundary == 1 ~ abs((1/3) - fish_size),
    relevantBoundary == 2 ~ abs((1/3) - fish_color),
  )) %>% #now let's look at the border cases 
  mutate(sumDistCol = case_when (
    grid_box == "box1_1" ~ abs((1/3) - fish_color),
    grid_box == "box2_2" ~ abs((1/3) - fish_color),
    grid_box == "box3_3" ~ abs((1/3) - fish_color)
  )) %>%
  mutate(sumDistSize = case_when (
    grid_box == "box1_1" ~ abs((1/3) - fish_size),
    grid_box == "box2_2" ~ abs((1/3) - fish_size),
    grid_box == "box3_3" ~ abs((1/3) - fish_size)
  )) %>% 
  mutate(summedDistance = case_when (
    relevantBoundary == 3  ~ sumDistCol + sumDistSize,
    TRUE ~ summedDistance 
  )) 
modelData <- subset(modelData, select = -c(sumDistCol, sumDistSize))

############### 3.0 Grid simulations ###########################################
#Simulate under these models 
#The idea would be to calculate these for each point in the 2D space 
#to compare with people's actual reported confidence. 
#this means a 601*601 space with each grid point having a distance to bound calculated under each model

x <- matrix(data = NA, nrow = 601, ncol = 601)
gridX_row <- c(200:800)
x[1:601,] <- gridX_row
x <-t(x) #size matrix
x <- ((x-200)/600) #put the size into 0-1 space

y <- matrix(data = NA, nrow = 601, ncol = 601)
gridY_col <- seq(1, 0, length.out = 601)
y[1:601,] <- gridY_col #color matrix

#set the boundary - and calculate distance to that boundary 
x1 <- matrix(data = NA, nrow = 601, ncol = 601) #make a copy of the NA version matrix to overwrite

#from size 200-400, when color above 0.33 distance based on size (for min and max)
#this is because size holds constant at the boundary at 400 on the vertical but color
#is continually varied in a way that makes it irrelevant in those spaces
x1[1:400,1:200] <- abs((1/3) - x[1:400,1:200]) #based on size

#from size 400-600, when color is above 0.66 distance based on size (as above)
x1[1:200, 201:400] <- abs((1/3) - x[1:200, 201:400]) #based on size

#from size 600-800, when color is less than 0.33 distance based on color
x1[401:601, 201:601] <- abs((1/3) - y[401:601, 201:601]) #based on color

#from size 600-800, when color is less than 0.66 distance based on color
x1[201:400, 401:601] <- abs((1/3) - y[201:400, 401:601]) #based on color

#we now have 3 grid "areas" that need adapting for distance 
#these distances will vary depending on what model of distance/confidence we have
############### 3.1 Minimum Distance ###########################################
#• Minimum distance to bound (out of the two dimensions)
x2 <- x1 #most are the same or defined by a single dimension (i.e. the other is irrelevant)

#the weird diagonal grid boxes are defined by either, 
#grid1_1 - this is the odd one out we need to find distance to a single point (.33, .33)
minDistCol <- abs((1/3) - y[401:601, 1:200]) #pick color here
minDistSize <- abs((1/3) - x[401:601, 1:200]) #pick size here
#now we need to find the hypotenuse between them - 
#it's length will be the same as the distance to the (.33,.33) point
pythagorean <- function(a, b) {
  hypotenuse <- sqrt(a^2 + b^2)
  return(hypotenuse)
}
x2[401:601, 1:200] <- pythagorean(minDistCol,minDistSize)


#grid2_2
minDistCol <- abs((1/3) - y[201:400, 201:400]) #pick color here
minDistSize <- abs((1/3) - x[201:400, 201:400]) #pick size here
minimumDistance2_2 <- ifelse(((minDistCol-minDistSize)>0),  minDistSize, minDistCol) 
x2[201:400, 201:400] <- minimumDistance2_2 

#grid3_3 
minDistCol <- abs((1/3) - y[1:200, 401:601]) #pick color here
minDistSize <- abs((1/3) - x[1:200, 401:601]) #pick size here
minimumDistance3_3 <- ifelse(((minDistCol-minDistSize)>0),  minDistSize, minDistCol) 
x2[1:200, 401:601] <- minimumDistance3_3 
#x2 is now confidence proxy based on minimum distance to boundary in space
x2 <- ((x2/(2/3))*100) #set it into 0-100 space

#x2 is now a matrix of confidence values under the minimum distance model 

# Data 
colnames(x2) <- paste("Col", 1:601)
rownames(x2) <- paste("Row", 1:601)

# Transform the matrix in long format
dfX <- melt(x2)
colnames(dfX) <- c("x", "y", "value")
dfX$x <- rev(dfX$x) #THIS ORDER ALSO NEEDS REVERSING - WHY?
ggplot(dfX, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  coord_fixed() +
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank() ) #remove y axis ticks

############### 3.2 Maximum Distance ###########################################
#• Maximum distance to bound (out of the two dimensions)
x3 <- x1

#the weird diagonal grid boxes are defined by either, 
#grid1_1
maxDistCol <- abs((1/3) - y[401:601, 1:200]) #pick color here
maxDistSize <- abs((1/3) - x[401:601, 1:200]) #pick size here
maximumDistance1_1 <- ifelse(((maxDistCol-maxDistSize)<0),  maxDistSize, maxDistCol) 
x3[401:601, 1:200] <- maximumDistance1_1 

#grid2_2
maxDistCol <- abs((1/3) - y[201:400, 201:400]) #pick color here
maxDistSize <- abs((1/3) - x[201:400, 201:400]) #pick size here
maximumDistance2_2 <- ifelse(((maxDistCol-maxDistSize)<0),  maxDistSize, maxDistCol) 
x3[201:400, 201:400] <- maximumDistance2_2 

#grid3_3 
maxDistCol <- abs((1/3) - y[1:200, 401:601]) #pick color here
maxDistSize <- abs((1/3) - x[1:200, 401:601]) #pick size here
maximumDistance3_3 <- ifelse(((maxDistCol-maxDistSize)<0),  maxDistSize, maxDistCol) 
x3[1:200, 401:601] <- maximumDistance3_3 
#x3 is now confidence proxy based on minimum distance to boundary in space
x3 <- ((x3/(2/3))*100) #set it into 0-100 space

# Data 
colnames(x3) <- paste("Col", 1:601)
rownames(x3) <- paste("Row", 1:601)

# Transform the matrix in long format
dfX2 <- melt(x3)
colnames(dfX2) <- c("x", "y", "value")
dfX2$x <- rev(dfX2$x) #THIS ORDER ALSO NEEDS REVERSING 
ggplot(dfX2, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  coord_fixed() +
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank() ) #remove y axis ticks

############### 3.3 Summed Distance ###########################################
#• Summed distance to bound (along the two dimensions)
x4 <- x1

#the weird diagonal grid boxes are defined by either, 
#grid1_1
sumDistCol <- abs((1/3) - y[401:601, 1:200]) #pick color here
sumDistSize <- abs((1/3) - x[401:601, 1:200]) #pick size here
sumDistance1_1 <- sumDistCol + sumDistSize 
x4[401:601, 1:200] <- sumDistance1_1 

#grid2_2
sumDistCol <- abs((1/3) - y[201:400, 201:400]) #pick color here
sumDistSize <- abs((1/3) - x[201:400, 201:400]) #pick size here
sumDistance2_2 <- sumDistCol + sumDistSize
x4[201:400, 201:400] <- sumDistance2_2 

#grid3_3 
sumDistCol <- abs((1/3) - y[1:200, 401:601]) #pick color here
sumDistSize <- abs((1/3) - x[1:200, 401:601]) #pick size here
sumDistance3_3 <- sumDistCol + sumDistSize 
x4[1:200, 401:601] <- sumDistance3_3 
#x4 is now confidence proxy based on minimum distance to boundary in space
x4 <- ((x4/(2/3))*100) #set it into 0-100 space

# Data 
colnames(x4) <- paste("Col", 1:601)
rownames(x4) <- paste("Row", 1:601)

# Transform the matrix in long format
dfX3 <- melt(x4)
colnames(dfX3) <- c("x", "y", "value")
dfX3$x <- rev(dfX3$x) #THIS ORDER ALSO NEEDS REVERSING 
ggplot(dfX3, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  coord_fixed() +
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank() ) #remove y axis ticks
