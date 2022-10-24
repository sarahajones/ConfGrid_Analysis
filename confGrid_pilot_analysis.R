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
                      "dplyr",
                      "corrgram",
                      "Hmisc", 
                      "afex", #running ANOVA
                      "emmeans" #pairwise ttests
                      )
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] #check for any uninstalled packages
if(length(new.packages)) install.packages(new.packages) #install any missing packages (requires internet access)
lapply(list.of.packages, require, character.only = TRUE) #library all required packages
rm(list.of.packages, new.packages)

#DATA LOADING
dat_csv <- read_csv(file = "ConfGrid1_Study_CleanData.csv")#load in data
numParticipants <- 58

#LOCAL FUNCTIONS 
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

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

#RUN TIME 
#how long did it take for the partipants to complete the study (rouhgly?)
lastTrial <- subset(dat_csv, trial_index == 363)
lastTrial$minutes <- (lastTrial$time_elapsed/1000)/60
summary(lastTrial$minutes)
sd(lastTrial$minutes)

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

rm(gender, female, male, nonbinary, handedness, right, ambi, ageData, genderData, handData, lastTrial)

############### 1.1 Confidence checks and calculations #########################
#lets have a look at confidence across the space 
dat_csv$confidence <- as.numeric(dat_csv$confidence)
confidence <- subset(dat_csv,is.na(confidence) == FALSE)
confidence <- subset(dat_csv, confidence != "null") #we have just over 9000 trials

confidence$fish_color <- 1- confidence$fish_color #flip color

accuracy <- confidence %>% 
  group_by(PID) %>%
  dplyr::summarise(mean = mean(as.numeric(correct)))

PIDwiseAccuracy <- ggplot(data = accuracy) +
  geom_point(mapping = aes(x = PID, y = mean)) +
  labs(title="Participant Accuracy", x="PID", y="Accuracy") + 
  xlim(1, numParticipants)+ ylim(0, 1); PIDwiseAccuracy

accuracy <- confidence %>% 
  group_by(PID, block) %>%
  dplyr::summarise(mean = mean(as.numeric(correct)))

PIDwiseAccuracy <- ggplot(data = accuracy) +
  geom_point(mapping = aes(x = PID, y = mean, color = block)) +
  labs(title="Participant Accuracy", x="PID", y="Accuracy") + 
  xlim(1, numParticipants)+ ylim(0, 1); PIDwiseAccuracy

mean(accuracy$mean)

#how are people using the confidence scale? 
ggplot(data = confidence, aes(confidence))+
  geom_bar() +
  facet_wrap(~PID)

ggplot(data=confidence, aes(fish_size)) + #the trials worked - labelling was correct
  geom_point(mapping = aes(x = fish_size, y = fish_color, color = distribution_name))

ggplot(data=confidence, aes(fish_size)) + 
  geom_point(mapping = aes(x = fish_size, y = fish_color, shape = distribution_name, color = confidence)) +
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

#plotting confidence scores normalised, (z-scored within participant)
#which might bring out some variability that's hard to see in these full-scale plots
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

ggplot(data=confidence, aes(fish_size)) + 
  geom_point(mapping = aes(x = fish_size, y = fish_color, shape = distribution_name, color = zConf)) +
  scale_color_gradient(low = "white", high = "black") +
  facet_wrap( ~ PID) #very hard to see here pulled by very low and very high?

high <- subset(confidence, zConf < -2) # 327 trials have a z score of less than -5
low <- subset(confidence, zConf > 2) #78 trials here have z score of more that 5
rm(high, low)

#remove those 14 trials just to improve the color gradient in the viz
confSubset <- subset(confidence, zConf < 2 & zConf > -2) #still have 8778 trials here

ggplot(data=confSubset, aes(fish_size)) + 
  geom_point(mapping = aes(x = fish_size, y = fish_color, shape = distribution_name, color = zConf)) +
  scale_color_gradient(low = "blue", high = "red") +
  facet_wrap( ~ PID) #a little easire to see here

#and perhaps to 'valence' the color scale to indicate the actual judgment made (native/invasive).
ggplot(data=confSubset, aes(fish_size)) + 
  geom_point(mapping = aes(x = fish_size, y = fish_color, shape = distribution_name, color = zConf, alpha = button_label)) +
  scale_color_gradient(low = "blue", high = "red") +
  scale_alpha_manual(values=c(0.2,0.5)) +
  facet_wrap( ~ PID)

rm(meanConf, PIDConf, sdConf, x, z, zConf, zConfCol, i, j, 
   pp, pps, trials, accuracy, confSubset, PIDwiseAccuracy)

############### 1.2 Smoothed confidence visualisations ########################

# split each grid space into 4 - so space now has 36 grid boxes,
# take average confidence in each space 
# apply a color and a smoothing function as appropriate to match red plots

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

rm(gridX_row, gridY_col)

############### 2.1 Minimum Distance ###########################################
#Minimum distance to bound (out of the two dimensions), pull boundary to the full .33 level
x2 <- x1 #most are the same or defined by a single dimension (i.e. the other is irrelevant)

DistCol <- abs((1/3) - y) #pick color here
DistSize <- abs((1/3) - x) #pick size here

x2 <- ifelse(((DistCol-DistSize)>0),  DistSize, DistCol) 

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
# Maximum distance to bound (out of the two dimensions)
x3 <- x1
x3 <- ifelse(((DistCol-DistSize)<0), DistSize, DistCol)  

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
#Summed distance to bound (along the two dimensions)
x4 <- x1

x4 <- (((DistCol + DistSize)/(4/3))*100) #set it into 0-100 space
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

rm(dfX1, dfX2, dfX3, DistCol, DistSize, x2, x3, x4, minmodel, maxmodel, summodel)

############### 2.4 Asymmetrical Models ########################################
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
#(in some models, some other gir areas will also need adapting too)
#these distances will vary depending on what model of distance/confidence we have

############### 2.4.1 Minimum Distance Asymmetry #################################
#• Minimum distance to bound (out of the two dimensions)
x5 <- x1 #most are the same or defined by a single dimension (i.e. the other is irrelevant)

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
x5[401:601, 1:200] <- pythagorean(DistCol,DistSize)

#grid2_2
DistCol <- abs((1/3) - y[201:400, 201:400]) #pick color here
DistSize <- abs((1/3) - x[201:400, 201:400]) #pick size here
x5[201:400, 201:400] <- ifelse(((DistCol-DistSize)>0),  DistSize, DistCol) 

#grid3_3 
DistCol <- abs((1/3) - y[1:200, 401:601]) #pick color here
DistSize <- abs((1/3) - x[1:200, 401:601]) #pick size here
x5[1:200, 401:601] <- ifelse(((DistCol-DistSize)>0),  DistSize, DistCol) 
#x5 is now confidence proxy based on minimum distance to boundary in space
x5 <- ((x5/(2/3))*100) #set it into 0-100 space

# Data 
colnames(x5) <- paste("Col", 1:601)
rownames(x5) <- paste("Row", 1:601)

# Transform the matrix in long format
dfX4 <- melt(x5)
colnames(dfX4) <- c("x", "y", "value")
dfX4$x <- rev(dfX4$x) #THIS ORDER ALSO NEEDS REVERSING - WHY?
asym_minmodel <- ggplot(dfX4, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  coord_fixed() +
  ggtitle("Asym_Minimum Distance Model") +
  labs(x='Stimulus Size', y = 'Color Gradient') +
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank() ); asym_minmodel #remove y axis ticks

############### 2.4.2 Maximum Distance Asymmetry #########################
#• Maximum distance to bound (out of the two dimensions)
x6 <- x1

#we have to overwrite a few of the grid areas from x1 here:namely the 2 grid boxes as follows:  
#from size 400-600, when color is above 0.66 (here the max distance is going to flip)
DistCol <- abs((1/3) - y[1:200, 201:400]) #pick color here
DistSize <- abs((1/3) - x[1:200, 201:400]) #pick size here
x6[1:200, 201:400] <- ifelse(((DistCol-DistSize)<0), DistSize, DistCol)  

#from size 600-800, when color is less than 0.66 distance based on color
DistCol <- abs((1/3) - y[201:400, 401:601]) #pick color here
DistSize <- abs((1/3) - x[201:400, 401:601]) #pick size here
x6[201:400, 401:601] <- ifelse(((DistCol-DistSize)<0), DistSize, DistCol) 

#the weird diagonal grid boxes are defined by either, 
#grid1_1
DistCol <- abs((1/3) - y[401:601, 1:200]) #pick color here
DistSize <- abs((1/3) - x[401:601, 1:200]) #pick size here
x6[401:601, 1:200] <- pythagorean(DistCol,DistSize)

#grid2_2
DistCol <- abs((1/3) - y[201:400, 201:400]) #pick color here
DistSize <- abs((1/3) - x[201:400, 201:400]) #pick size here
x6[201:400, 201:400] <- ifelse(((DistCol-DistSize)<0),  DistSize, DistCol)  

#grid3_3 
DistCol <- abs((1/3) - y[1:200, 401:601]) #pick color here
DistSize <- abs((1/3) - x[1:200, 401:601]) #pick size here
x6[1:200, 401:601] <- ifelse(((DistCol-DistSize)<0), DistSize, DistCol)
#x3 is now confidence proxy based on minimum distance to boundary in space
x6 <- ((x6/(2/3))*100) #set it into 0-100 space

# Data 
colnames(x6) <- paste("Col", 1:601)
rownames(x6) <- paste("Row", 1:601)

# Transform the matrix in long format
dfX5 <- melt(x6)
colnames(dfX5) <- c("x", "y", "value")
dfX5$x <- rev(dfX5$x) #THIS ORDER ALSO NEEDS REVERSING 
asym_maxmodel <- ggplot(dfX5, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  coord_fixed() +
  ggtitle("Asym_Maximum Distance Model") +
  labs(x='Stimulus Size', y = 'Color Gradient') +
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank() ); asym_maxmodel #remove y axis ticks

############### 2.4.3 Summed Distance Asymmetry #########################
#• Summed distance to bound (along the two dimensions)
x7 <- x1
x7 <- ((x7/(2/3))*100) #set it into 0-100 space ###2/3?

#we have to overwrite a few of the grid areas from x1 here: namely the 2 grid boxes as follows:  
#from size 400-600, when color is above 0.66 (here the max distance is going to flip)
DistCol <- abs((1/3) - y[1:200, 201:400]) #pick color here
DistSize <- abs((1/3) - x[1:200, 201:400]) #pick size here
x7[1:200, 201:400] <- (((DistCol + DistSize)/(4/3))*100) #set it into 0-100 space

#from size 600-800, when color is less than 0.66 distance based on color
DistCol <- abs((1/3) - y[201:400, 401:601]) #pick color here
DistSize <- abs((1/3) - x[201:400, 401:601]) #pick size here
x7[201:400, 401:601] <- (((DistCol + DistSize)/(4/3))*100)

#the weird diagonal grid boxes are defined by either, 
#grid1_1
DistCol <- abs((1/3) - y[401:601, 1:200]) #pick color here
DistSize <- abs((1/3) - x[401:601, 1:200]) #pick size here
x7[401:601, 1:200] <- (((pythagorean(DistCol,DistSize))/(2/3))*100) ##2/3??

#grid2_2
DistCol <- abs((1/3) - y[201:400, 201:400]) #pick color here
DistSize <- abs((1/3) - x[201:400, 201:400]) #pick size here
x7[201:400, 201:400] <- (((DistCol + DistSize)/(4/3))*100)

#grid3_3 
DistCol <- abs((1/3) - y[1:200, 401:601]) #pick color here
DistSize <- abs((1/3) - x[1:200, 401:601]) #pick size here
x7[1:200, 401:601] <- (((DistCol + DistSize)/(4/3))*100)
#x7 is now confidence proxy based on minimum distance to boundary in space

# Data 
colnames(x7) <- paste("Col", 1:601)
rownames(x7) <- paste("Row", 1:601)

# Transform the matrix in long format
dfX6 <- melt(x7)
colnames(dfX6) <- c("x", "y", "value")
dfX6$x <- rev(dfX6$x) #THIS ORDER ALSO NEEDS REVERSING 
asym_summodel <- ggplot(dfX6, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  coord_fixed() +
  ggtitle("Asym_Summed Distance Model") +
  labs(x='Stimulus Size', y = 'Color Gradient') +
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank() ); asym_summodel #remove y axis ticks

asym_patch <- asym_minmodel + asym_maxmodel + asym_summodel; asym_patch 

combi_patch <- patch/asym_patch; combi_patch

rm(dfX4, dfX5, dfX6, DistCol, DistSize, x, y, x1, x5, x6, x7, asym_minmodel, asym_maxmodel, asym_summodel, asym_patch, patch)
############### 2.5 Scaled Model of Stimulus Space ################################
# a scaled model that better describes the L category. 
# this will be some kind of function of the stimulus space, not the bound per say. 
# The idea would be to calculate these for each point in the 2D space 

x <- matrix(data = NA, nrow = 601, ncol = 601)
gridX_row <- c(200:800)
x[1:601,] <- gridX_row
x <-t(x) #size matrix
x <- ((x-200)/600) #put the size into 0-1 space

y <- matrix(data = NA, nrow = 601, ncol = 601)
gridY_col <- seq(1, 0, length.out = 601)
y[1:601,] <- gridY_col #color matrix

probBx <- 1-x
probBy <- 1-y
confB <- probBx + probBy

colnames(confB) <- paste("Col", 1:601)
rownames(confB) <- paste("Row", 1:601)

# Transform the matrix in long format
confPlot <- melt(confB)
colnames(confPlot) <- c("x", "y", "value")
confPlot$x <- rev(confPlot$x)
confPlot$value <- (confPlot$value/2)*100
plotB <- ggplot(confPlot, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  coord_fixed() +
  ggtitle("Scaled Model (Probability B)") +
  labs(x='Stimulus Size', y = 'Color Gradient') +
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank()); plotB #remove y axis ticks

#switch to look at the O category and the related stimulus likelihood
confA <- abs(1-confB) #hold this matrix in memory for later use
confPlotA <- confPlot
confPlotA$value <- 100- confPlotA$value

plotA <- ggplot(confPlotA, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  coord_fixed() +
  ggtitle("Scaled Model (Probability A)") +
  labs(x='Stimulus Size', y = 'Color Gradient') +
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank()); plotA #remove y axis ticks

plotAB <- plotA + plotB; plotAB

#now combine the two scaled stimulus based 
confC <- confA #set it all to the O space
confC <- 1-confA
#need to overwrite the L with the confB values 
#grid 1_1 
confC[401:601, 1:200] <- confB[401:601, 1:200]
#grid 1_3 and 1_2 - where size less than 400 and color more than .33
confC[1:400, 1:200] <- confB[1:400, 1:200]
#grid boxes 2_1 and £_1 where size is 400-800 and color less than .33
confC[401:601, 201:601] <- confB[401:601, 201:601]

confPlotC <- melt(confC)
colnames(confPlotC) <- c("x", "y", "value")
confPlotC$x <- rev(confPlotC$x)
confPlotC$value <- (confPlotC$value/2)*100
plotC <- ggplot(confPlotC, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  coord_fixed() +
  ggtitle("Scaled Model") +
  labs(x='Stimulus Size', y = 'Color Gradient') +
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank()); plotC #remove y axis ticks

plotABC <- plotA + plotB + plotC; plotABC

combiScaled <- combi_patch/plotABC; combiScaled
#scaled model simulation for L is now in confB data
#scaled model simulation for O is now in confA data 

############### 2.5.1 Scaled Minimum Model ######################################
#the L stands as a stimulus space model, O category as min
#we want the L to be the scaled model and the O to be the min model
x8 <- confB #everything set to stimulus space, need to overwrite O now
x8 <- (x8/2) #set into the 0-1 space

#grid2_2
DistCol <- abs((1/3) - y[201:400, 201:400]) #pick color here
DistSize <- abs((1/3) - x[201:400, 201:400]) #pick size here
x8[201:400, 201:400] <- ifelse(((DistCol-DistSize)>0),  DistSize, DistCol) 

#grid2_3
DistCol <- abs((1/3) - y[1:200, 201:400]) #pick color here
DistSize <- abs((1/3) - x[1:200, 201:400]) #pick size here
x8[1:200, 201:400] <- ifelse(((DistCol-DistSize)>0),  DistSize, DistCol) 

#grid3_2 
DistCol <- abs((1/3) - y[201:400, 401:601]) #pick color here
DistSize <- abs((1/3) - x[201:400, 401:601]) #pick size here
x8[201:400, 401:601] <- ifelse(((DistCol-DistSize)>0),  DistSize, DistCol) 

#grid3_3 
DistCol <- abs((1/3) - y[1:200, 401:601]) #pick color here
DistSize <- abs((1/3) - x[1:200, 401:601]) #pick size here
x8[1:200, 401:601] <- ifelse(((DistCol-DistSize)>0),  DistSize, DistCol) 

x8 <- x8*100

# Data 
colnames(x8) <- paste("Col", 1:601)
rownames(x8) <- paste("Row", 1:601)

# Transform the matrix in long format
dfX8 <- melt(x8)
colnames(dfX8) <- c("x", "y", "value")
dfX8$x <- rev(dfX8$x) #THIS ORDER ALSO NEEDS REVERSING - WHY?
scale_minmodel <- ggplot(dfX8, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  coord_fixed() +
  ggtitle("Scale_Minimum Distance Model") +
  labs(x='Stimulus Size', y = 'Color Gradient') +
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank() ); scale_minmodel #remove y axis ticks

############### 2.5.2 Scaled Maximum Model ######################################
#the L stands as a stimulus space model, O category as max
#we want the L to be the scaled model and the O to be the max model
x9 <- confB #everything set to stimulus space, need to overwrite O now
x9 <- (x9/2) #set into the 0-1 space

#grid2_2
DistCol <- abs((1/3) - y[201:400, 201:400]) #pick color here
DistSize <- abs((1/3) - x[201:400, 201:400]) #pick size here
x9[201:400, 201:400] <- ifelse(((DistCol-DistSize)<0),  DistSize, DistCol) 

#grid2_3
DistCol <- abs((1/3) - y[1:200, 201:400]) #pick color here
DistSize <- abs((1/3) - x[1:200, 201:400]) #pick size here
x9[1:200, 201:400] <- ifelse(((DistCol-DistSize)<0),  DistSize, DistCol) 

#grid3_2 
DistCol <- abs((1/3) - y[201:400, 401:601]) #pick color here
DistSize <- abs((1/3) - x[201:400, 401:601]) #pick size here
x9[201:400, 401:601] <- ifelse(((DistCol-DistSize)<0),  DistSize, DistCol) 

#grid3_3 
DistCol <- abs((1/3) - y[1:200, 401:601]) #pick color here
DistSize <- abs((1/3) - x[1:200, 401:601]) #pick size here
x9[1:200, 401:601] <- ifelse(((DistCol-DistSize)<0),  DistSize, DistCol) 

x9 <- x9*100

# Data 
colnames(x9) <- paste("Col", 1:601)
rownames(x9) <- paste("Row", 1:601)

# Transform the matrix in long format
dfX9 <- melt(x9)
colnames(dfX9) <- c("x", "y", "value")
dfX9$x <- rev(dfX9$x) #THIS ORDER ALSO NEEDS REVERSING - WHY?
scale_maxmodel <- ggplot(dfX9, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  coord_fixed() +
  ggtitle("Scale_Maximum Distance Model") +
  labs(x='Stimulus Size', y = 'Color Gradient') +
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank() ); scale_maxmodel #remove y axis ticks

############### 2.5.3 Scaled Summed Model #####################################
#the L stands as a stimulus space model, O category as sum
#we want the L to be the scaled model and the O to be the max model
x10 <- confB #everything set to stimulus space, need to overwrite O now
x10 <- (x10/2)*100 #set into the 0-1 space

DistCol <- abs((1/3) - y) #pick color here
DistSize <- abs((1/3) - x) #pick size here

#grid2_2
x10[201:400, 201:400] <- (((DistCol[201:400, 201:400] + DistSize[201:400, 201:400])/(4/3))*100)
#grid2_3
x10[1:200, 201:400] <- (((DistCol[1:200, 201:400] + DistSize[1:200, 201:400])/(4/3))*100) 
#grid3_2 
x10[201:400, 401:601] <- (((DistCol[201:400, 401:601] + DistSize[201:400, 401:601])/(4/3))*100) 
#grid3_3 
x10[1:200, 401:601] <- (((DistCol[1:200, 401:601] + DistSize[1:200, 401:601])/(4/3))*100)

# Data 
colnames(x10) <- paste("Col", 1:601)
rownames(x10) <- paste("Row", 1:601)

# Transform the matrix in long format
dfX10 <- melt(x10)
colnames(dfX10) <- c("x", "y", "value")
dfX10$x <- rev(dfX10$x) #THIS ORDER ALSO NEEDS REVERSING - WHY?
scale_summodel <- ggplot(dfX10, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  coord_fixed() +
  ggtitle("Scale_Summed Distance Model") +
  labs(x='Stimulus Size', y = 'Color Gradient') +
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank() ); scale_summodel #remove y axis ticks

scaleMixPlot <- scale_minmodel + scale_maxmodel + scale_summodel; scaleMixPlot

scaledMixed <- plotABC / scaleMixPlot; scaledMixed

combiScaleMix <- combiScaled / scaleMixPlot; combiScaleMix


rm(confA, confB, confC, confPlot, confPlotA, confPlotC, dfX10, dfX8, dfX9, plotA, plotB, plotC, plotAB, plotABC, probBx, probBy,
   x10, x8, x9, x, y, scale_maxmodel, scale_minmodel, scale_summodel, scaleMixPlot, gridY_col, gridX_row, DistCol, DistSize)
############### 3.0 Comparing confidence to the models ########################
modelData <- confidence[c("PID", "distribution_name", "confidence","fish_color", "fish_size", "grid_box")]

#lets put size in the same space as color 0-1 space
modelData$fish_size <- ((modelData$fish_size-200)/600)

#find distances to color and size boundary again
DistCol <- abs((1/3) - modelData$fish_color)#pick color here
DistSize <- abs((1/3) - modelData$fish_size) #pick size here

#Let's consider:
#Minimum distance to bound (out of the two dimensions)
modelData <- modelData %>% 
  mutate(minimumDistance = ifelse(((DistCol-DistSize)>0),  DistSize, DistCol)
  ) %>% 
  mutate(minimumDistance = ((minimumDistance/(2/3))*100)) %>%
#Maximum distance to bound (out of the two dimensions)
  mutate(maximumDistance = ifelse(((DistCol-DistSize)<0), DistSize, DistCol) 
  ) %>% 
  mutate(maximumDistance = ((maximumDistance/(2/3))*100)) %>%
# Summed distance to bound (along the two dimensions)
  mutate(summedDistance = (((DistCol + DistSize)/(4/3))*100)
  )

# now for the asymmetrical cases: 
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
  mutate(asym_minimumDistance = case_when (
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
  mutate(asym_minimumDistance = case_when (
    relevantBoundary == 3 & (minDistCol-minDistSize)> 0 ~ minDistSize,
    relevantBoundary == 3 & (minDistCol-minDistSize)< 0 ~ minDistCol,
    TRUE ~ asym_minimumDistance #minimum distance is scaled from 0-0.66
  )) %>% 
  mutate(asym_minimumDistance = case_when ( #overwrite for grid_box1_1 
    relevantBoundary == 3 & grid_box == "box1_1" ~ pythagorean(minDistCol,minDistSize),
    TRUE ~ asym_minimumDistance
  )) %>%
  mutate(asym_minimumDistance = ((asym_minimumDistance/(2/3))*100))

modelData <- subset(modelData, select = -c(minDistCol, minDistSize))

#• Maximum distance to bound (out of the two dimensions)
modelData <- modelData %>% 
  mutate(asym_maximumDistance = case_when (
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
  mutate(asym_maximumDistance = case_when (
    relevantBoundary == 3 & (maxDistCol-maxDistSize)> 0 ~ maxDistCol,
    relevantBoundary == 3 & (maxDistCol-maxDistSize)< 0 ~ maxDistSize,
    #overwrite those 2 grid boxes explictly here
    grid_box == "box2_3" & (maxDistCol-maxDistSize)> 0 ~ maxDistCol,
    grid_box == "box3_2" & (maxDistCol-maxDistSize)> 0 ~ maxDistCol,
    grid_box == "box2_3" & (maxDistCol-maxDistSize)< 0 ~ maxDistSize,
    grid_box == "box3_2" & (maxDistCol-maxDistSize)< 0 ~ maxDistSize,
    TRUE ~ asym_maximumDistance 
  ))  %>% 
  mutate(asym_maximumDistance = case_when (#overwrite grid_box1_1 in a different way 
    relevantBoundary == 3 & grid_box == "box1_1" ~ pythagorean(maxDistCol,maxDistSize),
    TRUE ~ asym_maximumDistance
  ))%>%
  mutate(asym_maximumDistance = ((asym_maximumDistance/(2/3))*100))

modelData <- subset(modelData, select = -c(maxDistCol, maxDistSize))

#• Summed distance to bound (along the two dimensions)
modelData <- modelData %>% 
  mutate(asym_summedDistance = case_when (
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
  mutate(asym_summedDistance = case_when (
    relevantBoundary == 3  ~ sumDistCol + sumDistSize,
    #overwrite those 2 grid boxes explicitly here
    grid_box == "box2_3" ~ sumDistCol + sumDistSize,
    grid_box == "box3_2" ~ sumDistCol + sumDistSize,
    TRUE ~ asym_summedDistance 
  )) %>%
  mutate(asym_summedDistance = case_when (#overwrite grid_box1_1 in a different way 
    grid_box == "box1_1" ~ pythagorean(sumDistCol,sumDistSize),
    TRUE ~ asym_summedDistance
  ))%>%
  mutate(asym_summedDistance = case_when(
    grid_box == "box2_3" ~ (asym_summedDistance/(4/3))*100,
    grid_box == "box2_2" ~ (asym_summedDistance/(4/3))*100,
    grid_box == "box3_3" ~ (asym_summedDistance/(4/3))*100,
    grid_box == "box3_2" ~ (asym_summedDistance/(4/3))*100,
    TRUE ~ (asym_summedDistance/(2/3))*100)) 

modelData <- subset(modelData, select = -c(sumDistCol, sumDistSize, relevantBoundary))

#now consider the stimulus scaled models
modelData <- modelData %>% 
  mutate(scaled_minDistance = (((1- fish_size) + (1-fish_color))/2)
  ) %>%
  mutate(scaled_minDistance = case_when(
    grid_box == "box2_2" ~ ifelse(((DistCol-DistSize)>0),  DistSize, DistCol),
    grid_box == "box2_3" ~ ifelse(((DistCol-DistSize)>0),  DistSize, DistCol),
    grid_box == "box3_2" ~ ifelse(((DistCol-DistSize)>0),  DistSize, DistCol),
    grid_box == "box3_3" ~ ifelse(((DistCol-DistSize)>0),  DistSize, DistCol),
    TRUE ~ scaled_minDistance
  )) %>%
  mutate(scaled_minDistance = scaled_minDistance*100) %>%
  mutate(scaled_maxDistance = (((1- fish_size) + (1-fish_color))/2)
  ) %>%
  mutate(scaled_maxDistance = case_when(
    grid_box == "box2_2" ~ ifelse(((DistCol-DistSize)<0),  DistSize, DistCol),
    grid_box == "box2_3" ~ ifelse(((DistCol-DistSize)<0),  DistSize, DistCol),
    grid_box == "box3_2" ~ ifelse(((DistCol-DistSize)<0),  DistSize, DistCol),
    grid_box == "box3_3" ~ ifelse(((DistCol-DistSize)<0),  DistSize, DistCol),
    TRUE ~ scaled_maxDistance
  )) %>%
  mutate(scaled_maxDistance = scaled_maxDistance*100) %>%
  mutate(scaled_sumDistance = (((1- fish_size) + (1-fish_color))/2)
  ) %>%
  mutate(scaled_sumDistance = case_when(
    grid_box == "box2_2" ~ (DistCol + DistSize)/(4/3),
    grid_box == "box2_3" ~ (DistCol + DistSize)/(4/3),
    grid_box == "box3_2" ~ (DistCol + DistSize)/(4/3),
    grid_box == "box3_3" ~ (DistCol + DistSize)/(4/3),
    TRUE ~ scaled_sumDistance
  )) %>%
  mutate(scaled_sumDistance = scaled_sumDistance*100) %>%
  mutate(scaled_scaled = (((1- fish_size) + (1-fish_color))/2)
  ) %>%
  mutate(scaled_scaled = case_when(
    grid_box == "box2_2" ~ 1-scaled_scaled,
    grid_box == "box2_3" ~ 1-scaled_scaled,
    grid_box == "box3_2" ~ 1-scaled_scaled,
    grid_box == "box3_3" ~ 1-scaled_scaled,
    TRUE ~ scaled_scaled
  )) %>%
  mutate(scaled_scaled = scaled_scaled*100)
  
  
############### 3.1 Running Anova on model values ##############################
# run correlation within each individual for the 10 models 
pValue <- matrix(data = NA, nrow = numParticipants, ncol = 10)
rValue <- matrix(data = NA, nrow = numParticipants, ncol = 10)
PID <- c(1:numParticipants)
for(i in PID){
  dataX <- subset(modelData, PID == i)
  dataX <- dataX[c("confidence", 
                   "minimumDistance","maximumDistance", "summedDistance", 
                   "asym_minimumDistance", "asym_maximumDistance", "asym_summedDistance",
                   "scaled_minDistance", "scaled_maxDistance", "scaled_sumDistance",
                   "scaled_scaled")]
  correlations <- rcorr(as.matrix(dataX))
  pValue[i,] <- correlations$P[1,2:11]
  rValue[i,] <- correlations$r[1,2:11]
}

#check which of these correlations are significant
sigCorrs <- ifelse((pValue<0.05),  rValue, NA) #just highlights what corrs are significant

minSig <- numParticipants - sum(is.na(sigCorrs[,1])) #26/58 sig corr for min model --- marginally highest num here
maxSig <- numParticipants - sum(is.na(sigCorrs[,2])) #16/58 sig corr for max model
sumSig <- numParticipants - sum(is.na(sigCorrs[,3])) #24/58 sig for summed model 

asym_minSig <- numParticipants - sum(is.na(sigCorrs[,4])) #12/58 sig corr for asym_min model
asym_maxSig <- numParticipants - sum(is.na(sigCorrs[,5])) #16/58 sig corr for asym_max model
asym_sumSig <- numParticipants - sum(is.na(sigCorrs[,6])) #19/58 sig for asym_summed model 

scale_minSig <- numParticipants - sum(is.na(sigCorrs[,7])) #24/58
scale_maxSig <- numParticipants - sum(is.na(sigCorrs[,8])) #26/58
scale_sumSig <- numParticipants - sum(is.na(sigCorrs[,9])) #28/58
scale_scaleSig<- numParticipants - sum(is.na(sigCorrs[,10])) #24/58

#identify the largest correlation for each pp
highest <- matrix(data = NA, nrow = numParticipants, ncol = 10)
for(i in PID){
  dataX <-rValue[i,]
  index <- which.max(dataX)
  highest[i,index] <- max(dataX)
  } #these are the strongest within individual correlations (not necessarily sig corrs)

minModel <- numParticipants - sum(is.na(highest[,1])) #6/58 show strongest corr for min model
maxModel <- numParticipants - sum(is.na(highest[,2])) #2/58 show strongest corr for max model
sumModel <- numParticipants - sum(is.na(highest[,3])) #7/58 show strongest corr for summed model

asym_minModel <- numParticipants - sum(is.na(highest[,4])) #1/58 show strongest corr for min model
asym_maxModel <- numParticipants - sum(is.na(highest[,5])) #2/58 show strongest corr for max model
asym_sumModel <- numParticipants - sum(is.na(highest[,6])) #6/58 show strongest corr for summed model

scale_minModel <- numParticipants - sum(is.na(highest[,7])) #13/58
scale_maxModel <- numParticipants - sum(is.na(highest[,8])) #4/58
scale_sumModel <- numParticipants - sum(is.na(highest[,9])) #8/58
scale_scaleModel <- numParticipants - sum(is.na(highest[,10])) #9/58

#second order stats on the r-values of the model correlations to confidence
#running anova  R values from the correlations with confidence
#minimumDistance
rValues <- as.data.frame(rValue)
anovaData <- as.data.frame(PID)
anovaData$Rvalue <- rValues$V1
anovaData$model <- "minimumDistance"

#maximumDistance
model2 <- as.data.frame(PID)
model2$Rvalue <- rValues$V2
model2$model <- "maximumDistance"

anovaData <- rbind(anovaData, model2)

#summedDistance
model3 <- as.data.frame(PID)
model3$Rvalue <- rValues$V3
model3$model <- "summedDistance"

anovaData <- rbind(anovaData, model3)

#asym_minimumDistance
model4 <- as.data.frame(PID)
model4$Rvalue <- rValues$V4
model4$model <- "asym_minimumDistance"

anovaData <- rbind(anovaData, model4)
 
#asym_maximumDistance
model5 <- as.data.frame(PID)
model5$Rvalue <- rValues$V5
model5$model <- "asym_maximumDistance"

anovaData <- rbind(anovaData, model5)

#asym_summedDistance
model6 <- as.data.frame(PID)
model6$Rvalue <- rValues$V6
model6$model <- "asym_summedDistance"

anovaData <- rbind(anovaData, model6)

#scaled_minDistance
model7 <- as.data.frame(PID)
model7$Rvalue <- rValues$V7
model7$model <- "scaled_minDistance"

anovaData <- rbind(anovaData, model7)

#scaled_maxDistance
model8 <- as.data.frame(PID)
model8$Rvalue <- rValues$V8
model8$model <- "scaled_maxDistance"

anovaData <- rbind(anovaData, model8)

#scaled_sumDistance
model9 <- as.data.frame(PID)
model9$Rvalue <- rValues$V9
model9$model <- "scaled_sumDistance"

anovaData <- rbind(anovaData, model9)

#scaled_scaled
model10 <- as.data.frame(PID)
model10$Rvalue <- rValues$V10
model10$model <- "scaled_scaled"

anovaData <- rbind(anovaData, model10)
rm(model2, model3, model4, model5, model6, model7, model8, model9, model10)

anovaData <- anovaData %>% 
  mutate(combiModel = case_when(
    model == "minimumDistance" ~ 0,
    model == "maximumDistance" ~ 0,
    model == "summedDistance" ~ 0,
    model == "scaled_scaled" ~ 0,
    TRUE ~ 1
  ))

anovaData$model <- as.factor(anovaData$model)

#oneway ANOVA
rValueAOV <- aov(Rvalue ~ model, data = anovaData)
summary(rValueAOV)
#postHoc <- TukeyHSD(rValueAOV); postHoc
#check the pairwise comparisons
int_comp <- emmeans(rValueAOV, ~ model)
pairs(int_comp,adjust="none")

#withinPID?
rValueAOV <- aov_ez(id="PID", dv="Rvalue", data=anovaData, within = c("model"))
rValueAOV
#check the pairwise comparisons
int_comp <- emmeans(rValueAOV, ~ model)
pairs(int_comp,adjust="none")

#tidy up the environment
rm(minSig, maxSig, sumSig, minModel, maxModel, sumModel, i, dataX, index, correlations, 
  DistCol, DistSize, sigCorrs, pValue, rValue, highest,
    asym_maxModel, asym_maxSig, asym_minModel,
   asym_minSig, asym_sumModel,  asym_sumSig,  scale_maxModel,
  scale_minModel,  scale_sumModel, scale_minSig, scale_maxSig,
  scale_sumSig, scale_scaleModel, scale_scaleSig, rValues)

############### 3.1  Using Likelihoods to assess fit ##########################

#t tests are getting out of hand - likelihood comparisons might be the way to go. 
#the likelihood of it being from model X given the data Y 


############### 4.0 Response bound work #######################################
#looking at response based bound as a fixed individual bound in the space
#this bound within individual will become the basis to rerun the above models
responseData <- confidence[c("PID", "distribution_name", "button", "correct", "confidence","fish_color", "fish_size")]

responseData$fish_size <- ((responseData$fish_size-200)/600) #put size in 0-1 space

#find one person as an example
pp1 <- subset(responseData, PID ==1)
ggplot(data=pp1, aes(fish_size)) + 
  geom_point(mapping = aes(x = fish_size, y = fish_color, color = button)) 
#can see the vaguely follow the vertical and horizontal - but how will we define this? 

#now lets find the response based boundary per participant
colorVector <- seq(0, 1, length.out = 100)
sizeVector <- seq(0, 1, length.out = 100)
countVerticalBlue <- matrix(data = NA, nrow = 100, ncol = 100)
countHorizontalRed <- matrix(data = NA, nrow = 100, ncol = 100)
countHorizontalBlue <- matrix(data = NA, nrow = 100, ncol = 100)
responseBound <- matrix(data = NA, nrow = length(PID), ncol = 2)

for(i in PID){ #for each pp
  data <- subset(responseData, PID == i)
  for(val in colorVector){
    j <- match(c(val), colorVector) #set the horizontal value and index
    for(value in sizeVector){
      k <- match(c(value),sizeVector) #set the vertical value and index

      #set size as the rows and color as the column - so each column holds a size constant
      countVerticalBlue[k,j] <- sum(data$fish_size < value & data$button == 1) 
      countHorizontalRed[k,j] <- sum(data$fish_color > val & data$fish_size > value & data$button == 0)
      countHorizontalBlue [k,j] <- sum(data$fish_color < val & data$fish_size > value & data$button == 1)
      }
  }
  totalWrong <- countVerticalBlue + countHorizontalRed + countHorizontalBlue
  indices <- which(totalWrong == min(totalWrong), arr.ind = TRUE) #find the minimum value index
  
  colors <- unique(indices[,2])
  sizes <- unique(indices[,1])
  
  if (length(colors) == 1){
    responseBound[i, 1] <- colorVector[colors[1]]
  } else if(length(colors) %% 2 != 0){
    responseBound[i, 1] <- colorVector[median(colors)]
  } else if(length(colors)%% 2 == 0 & length(colors) < length(indices[,2])){
    responseBound[i,1] <- colorVector[getmode(indices[,2])]
  } else {responseBound[i,1] <- colorVector[mean(colors)]} ####PROBLEM HERE
  
  if (length(sizes) == 1){
    responseBound[i, 2] <- sizeVector[sizes[1]]
  } else if(length(sizes) %% 2 != 0){
    responseBound[i, 2] <- sizeVector[median(sizes)]
  } else if(length(sizes)%% 2 == 0 & length(sizes) < length(indices[,1])){
    responseBound[i,2] <- sizeVector[getmode(indices[,1])]}
  else {responseBound[i,2] <- sizeVector[mean(sizes)]} ###PROBLEM HERE
  
}
responseBound <- as.data.frame(responseBound)
responseBound$PID <- PID
responseBound$VerticalSize <- responseBound$V2
responseBound$HorizontalColor <- responseBound$V1
responseBound <- responseBound %>% select(-V1, -V2)
confidence <- merge(confidence, responseBound, by = c("PID"))

#plot these boundaries to sanity check them 
confidence$fish_size <- ((confidence$fish_size-200)/600) #put size in 0-1 space

ggplot(data=confidence, aes(fish_size)) + 
  geom_point(mapping = aes(x = fish_size, y = fish_color, shape = button_label, color = confidence, alpha = button_label)) +
  scale_color_gradient(low = "blue", high = "red") +
  scale_alpha_manual(values=c(0.2,0.5)) +
  geom_vline(aes(xintercept = VerticalSize)) +
  geom_hline(aes(yintercept = HorizontalColor)) +
  facet_wrap( ~ PID)

############### 4.1 Adjust response bounds - constrained spaced ######################

# we want to make sure that each "quadrant" of the subject bound 
# contains some data = so that the don't fit to an empty quadrant space 
# (will constrain locations for response bound itself)


###############################################################################