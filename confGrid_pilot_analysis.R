############### Confidence and Categorisation: ConfGrid ########################
#Study 4 - Data Analysis Script 

#Sarah Ashcroft-Jones 
#sarahashjones@gmail.com
#GitHub: sarahajones

############### 0.1 Script Set-Up ##############################################
# Set the working directory to the folder that this file is in:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#LOAD IN PACKAGES
# Create a list of the required packages:
list.of.packages <- c("tidyverse",
                      "ggplot2",
                      "patchwork",
                      "reshape",
                      "readr",
                      "plyr",
                      "dplyr"
                      )
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] #check for any uninstalled packages
if(length(new.packages)) install.packages(new.packages) #install any missing packages (requires internet access)
lapply(list.of.packages, require, character.only = TRUE) #library all required packages
rm(list.of.packages, new.packages)

#DATA LOADING
dat_csv <- read_csv(file = "ConfGrid1_Study_CleanData.csv")#load in data
numParticipants <- 58

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

############### 1.1 Confidence checks and calculations #########################
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
  dplyr::summarise(mean(confidence))

sdConf <- confidence %>%
  group_by(PID) %>%
  dplyr::summarise(sd(confidence))

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

zConf <- subset(zConfCol, is.na(conf)!= TRUE) #remove the 17 missing trials to match up the columns with confidence

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

#okay - we can see some variability across pp - 
#some pp highly confident always e.g. pp4, some low confidence always e.g. pp 12
#some showing nice lowered confidence along the category bound e.g. pp 21

rm(meanConf, PIDConf, sdConf, x, z, zConf, zConfCol, i, j, 
   pp, pps, trials)

############### 2.0 Grid simulations ###########################################
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

rm(gridX_row, gridY_col)
#we now have 3 grid "areas" that need adapting for distance 
#(in some models, some other gir areas will also need adapting too)
#these distances will vary depending on what model of distance/confidence we have
############### 2.1 Minimum Distance ###########################################
#• Minimum distance to bound (out of the two dimensions)
x2 <- x1 #most are the same or defined by a single dimension (i.e. the other is irrelevant)

#the weird diagonal grid boxes are defined by either, 
#grid1_1 - this is the odd one out we need to find distance to a single point (.33, .33)
DistCol <- abs((1/3) - y[401:601, 1:200]) #pick color here
DistSize <- abs((1/3) - x[401:601, 1:200]) #pick size here
#now we need to find the hypotenuse between them - 
#it's length will be the same as the distance to the (.33,.33) point
pythagorean <- function(a, b) {
  hypotenuse <- sqrt(a^2 + b^2)
  return(hypotenuse)
}
x2[401:601, 1:200] <- pythagorean(DistCol,DistSize)

#grid2_2
DistCol <- abs((1/3) - y[201:400, 201:400]) #pick color here
DistSize <- abs((1/3) - x[201:400, 201:400]) #pick size here
x2[201:400, 201:400] <- ifelse(((DistCol-DistSize)>0),  DistSize, DistCol) 

#grid3_3 
DistCol <- abs((1/3) - y[1:200, 401:601]) #pick color here
DistSize <- abs((1/3) - x[1:200, 401:601]) #pick size here
x2[1:200, 401:601] <- ifelse(((DistCol-DistSize)>0),  DistSize, DistCol) 
#x2 is now confidence proxy based on minimum distance to boundary in space
x2 <- ((x2/(2/3))*100) #set it into 0-100 space

# Data 
colnames(x2) <- paste("Col", 1:601)
rownames(x2) <- paste("Row", 1:601)

# Transform the matrix in long format
dfX1 <- melt(x2)
colnames(dfX1) <- c("x", "y", "value")
dfX1$x <- rev(dfX1$x) #THIS ORDER ALSO NEEDS REVERSING - WHY?
minmodel <- ggplot(dfX1, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  coord_fixed() +
  ggtitle("Minimum Distance Model") +
  labs(x='Stimulus Size', y = 'Color Gradient') +
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank() ); minmodel #remove y axis ticks

############### 2.2 Maximum Distance ###########################################
#• Maximum distance to bound (out of the two dimensions)
x3 <- x1

#we have to overwrite a few of the grid areas from x1 here:namely the 2 grid boxes as follows:  
#from size 400-600, when color is above 0.66 (here the max distance is going to flip)
DistCol <- abs((1/3) - y[1:200, 201:400]) #pick color here
DistSize <- abs((1/3) - x[1:200, 201:400]) #pick size here
x3[1:200, 201:400] <- ifelse(((DistCol-DistSize)<0), DistSize, DistCol)  

#from size 600-800, when color is less than 0.66 distance based on color
DistCol <- abs((1/3) - y[201:400, 401:601]) #pick color here
DistSize <- abs((1/3) - x[201:400, 401:601]) #pick size here
x3[201:400, 401:601] <- ifelse(((DistCol-DistSize)<0), DistSize, DistCol) 

#the weird diagonal grid boxes are defined by either, 
#grid1_1
DistCol <- abs((1/3) - y[401:601, 1:200]) #pick color here
DistSize <- abs((1/3) - x[401:601, 1:200]) #pick size here
x3[401:601, 1:200] <- pythagorean(DistCol,DistSize)

#grid2_2
DistCol <- abs((1/3) - y[201:400, 201:400]) #pick color here
DistSize <- abs((1/3) - x[201:400, 201:400]) #pick size here
x3[201:400, 201:400] <- ifelse(((DistCol-DistSize)<0),  DistSize, DistCol)  

#grid3_3 
DistCol <- abs((1/3) - y[1:200, 401:601]) #pick color here
DistSize <- abs((1/3) - x[1:200, 401:601]) #pick size here
x3[1:200, 401:601] <- ifelse(((DistCol-DistSize)<0), DistSize, DistCol)
#x3 is now confidence proxy based on minimum distance to boundary in space
x3 <- ((x3/(2/3))*100) #set it into 0-100 space

# Data 
colnames(x3) <- paste("Col", 1:601)
rownames(x3) <- paste("Row", 1:601)

# Transform the matrix in long format
dfX2 <- melt(x3)
colnames(dfX2) <- c("x", "y", "value")
dfX2$x <- rev(dfX2$x) #THIS ORDER ALSO NEEDS REVERSING 
maxmodel <- ggplot(dfX2, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  coord_fixed() +
  ggtitle("Maximum Distance Model") +
  labs(x='Stimulus Size', y = 'Color Gradient') +
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank() ); maxmodel #remove y axis ticks

############### 2.3 Summed Distance ###########################################
#• Summed distance to bound (along the two dimensions)
x4 <- x1
x4 <- ((x4/(4/3))*100) #set it into 0-100 space ###2/3?

#we have to overwrite a few of the grid areas from x1 here: namely the 2 grid boxes as follows:  
#from size 400-600, when color is above 0.66 (here the max distance is going to flip)
DistCol <- abs((1/3) - y[1:200, 201:400]) #pick color here
DistSize <- abs((1/3) - x[1:200, 201:400]) #pick size here
x4[1:200, 201:400] <- (((DistCol + DistSize)/(4/3))*100) #set it into 0-100 space

#from size 600-800, when color is less than 0.66 distance based on color
DistCol <- abs((1/3) - y[201:400, 401:601]) #pick color here
DistSize <- abs((1/3) - x[201:400, 401:601]) #pick size here
x4[201:400, 401:601] <- (((DistCol + DistSize)/(4/3))*100)

#the weird diagonal grid boxes are defined by either, 
#grid1_1
DistCol <- abs((1/3) - y[401:601, 1:200]) #pick color here
DistSize <- abs((1/3) - x[401:601, 1:200]) #pick size here
x4[401:601, 1:200] <- (((pythagorean(DistCol,DistSize))/(4/3))*100) ##2/3??

#grid2_2
DistCol <- abs((1/3) - y[201:400, 201:400]) #pick color here
DistSize <- abs((1/3) - x[201:400, 201:400]) #pick size here
x4[201:400, 201:400] <- (((DistCol + DistSize)/(4/3))*100)

#grid3_3 
DistCol <- abs((1/3) - y[1:200, 401:601]) #pick color here
DistSize <- abs((1/3) - x[1:200, 401:601]) #pick size here
x4[1:200, 401:601] <- (((DistCol + DistSize)/(4/3))*100)
#x4 is now confidence proxy based on minimum distance to boundary in space

# Data 
colnames(x4) <- paste("Col", 1:601)
rownames(x4) <- paste("Row", 1:601)

# Transform the matrix in long format
dfX3 <- melt(x4)
colnames(dfX3) <- c("x", "y", "value")
dfX3$x <- rev(dfX3$x) #THIS ORDER ALSO NEEDS REVERSING 
summodel <- ggplot(dfX3, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  coord_fixed() +
  ggtitle("Summed Distance Model") +
  labs(x='Stimulus Size', y = 'Color Gradient') +
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank() ); summodel #remove y axis ticks

patch <- minmodel + maxmodel + summodel; patch 

rm(dfX1, dfX2, dfX3, DistCol, DistSize, x, y, x1, minmodel, maxmodel, summodel)

############### 3.0 Comparing confidence to the models ########################
modelData <- confidence[c("PID", "distribution_name","grid_box", "confidence", "zConf","fish_color", "fish_size")]

#lets put size in the same space as color 0-1 space
modelData$fish_size <- ((modelData$fish_size-200)/600)

# check which are the most relevant dimensions for which area of the category space
# this is not true for every model but worth starting here
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
  )) %>% 
  #now let's look at the border cases 
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
  mutate(minimumDistance = case_when ( #overwrite for grid_box1_1 
    relevantBoundary == 3 & grid_box == "box1_1" ~ pythagorean(minDistCol,minDistSize),
    TRUE ~ minimumDistance
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
    grid_box == "box3_3" ~ abs((1/3) - fish_color),
    #overwrite 2 grid boxes that need reconstrual here
    grid_box == "box2_3" ~ abs((1/3) - fish_color),
    grid_box == "box3_2" ~ abs((1/3) - fish_color)
  )) %>%
  mutate(maxDistSize = case_when (
    grid_box == "box1_1" ~ abs((1/3) - fish_size),
    grid_box == "box2_2" ~ abs((1/3) - fish_size),
    grid_box == "box3_3" ~ abs((1/3) - fish_size),
    #overwrite 2 grid boxes that need reconstrual here
    grid_box == "box2_3" ~ abs((1/3) - fish_size),
    grid_box == "box3_2" ~ abs((1/3) - fish_size)
  )) %>% 
  mutate(maximumDistance = case_when (
    relevantBoundary == 3 & (maxDistCol-maxDistSize)> 0 ~ maxDistCol,
    relevantBoundary == 3 & (maxDistCol-maxDistSize)< 0 ~ maxDistSize,
    #overwrite those 2 grid boxes explictly here
    grid_box == "box2_3" & (maxDistCol-maxDistSize)> 0 ~ maxDistCol,
    grid_box == "box3_2" & (maxDistCol-maxDistSize)> 0 ~ maxDistCol,
    grid_box == "box2_3" & (maxDistCol-maxDistSize)< 0 ~ maxDistSize,
    grid_box == "box3_2" & (maxDistCol-maxDistSize)< 0 ~ maxDistSize,
    TRUE ~ maximumDistance 
  ))  %>% 
  mutate(maximumDistance = case_when (#overwrite grid_box1_1 in a different way 
    relevantBoundary == 3 & grid_box == "box1_1" ~ pythagorean(maxDistCol,maxDistSize),
    TRUE ~ maximumDistance
  ))%>%
  mutate(maximumDistance = ((maximumDistance/(2/3))*100))

modelData <- subset(modelData, select = -c(maxDistCol, maxDistSize))

#• Summed distance to bound (along the two dimensions)
modelData <- modelData %>% 
  mutate(summedDistance = case_when (
    relevantBoundary == 1 ~ abs((1/3) - fish_size),
    relevantBoundary == 2 ~ abs((1/3) - fish_color),
  )) %>% #now let's look at the border cases 
  mutate(sumDistCol = case_when (
    grid_box == "box1_1" ~ abs((1/3) - fish_color),
    grid_box == "box2_2" ~ abs((1/3) - fish_color),
    grid_box == "box3_3" ~ abs((1/3) - fish_color),
    #overwrite 2 grid boxes that need reconstrual here
    grid_box == "box2_3" ~ abs((1/3) - fish_color),
    grid_box == "box3_2" ~ abs((1/3) - fish_color)
  )) %>%
  mutate(sumDistSize = case_when (
    grid_box == "box1_1" ~ abs((1/3) - fish_size),
    grid_box == "box2_2" ~ abs((1/3) - fish_size),
    grid_box == "box3_3" ~ abs((1/3) - fish_size),
    #overwrite 2 grid boxes that need reconstrual here
    grid_box == "box2_3" ~ abs((1/3) - fish_size),
    grid_box == "box3_2" ~ abs((1/3) - fish_size)
  )) %>% 
  mutate(summedDistance = case_when (
    relevantBoundary == 3  ~ sumDistCol + sumDistSize,
    #overwrite those 2 grid boxes explicitly here
    grid_box == "box2_3" ~ sumDistCol + sumDistSize,
    grid_box == "box3_2" ~ sumDistCol + sumDistSize,
    TRUE ~ summedDistance 
  )) %>%
  mutate(summedDistance = case_when (#overwrite grid_box1_1 in a different way 
    grid_box == "box1_1" ~ pythagorean(sumDistCol,sumDistSize),
    TRUE ~ summedDistance
  ))%>%
  #mutate(summedDistance = case_when(
    #grid_box == "box2_3" ~ (summedDistance/(4/3))*100,
    #grid_box == "box2_2" ~ (summedDistance/(4/3))*100,
    #grid_box == "box3_3" ~ (summedDistance/(4/3))*100,
    #grid_box == "box3_2" ~ (summedDistance/(4/3))*100,
    #TRUE ~ (summedDistance/(2/3))*100)) #NO WAIT MAYBE THIS SHOULD ALL BE 4/3 ratiod? 
  mutate(summedDistance = (summedDistance/(4/3))*100) #put into the 4/3 space again

modelData <- subset(modelData, select = -c(sumDistCol, sumDistSize))


cor.test(modelData$confidence, modelData$minimumDistance)
cor.test(modelData$confidence, modelData$maximumDistance)
cor.test(modelData$confidence, modelData$summedDistance)

###############################################################################