#Confidence and Grid Categorisation
#Study 1 - Data Cleaning Script 

#Sarah Ashcroft-Jones 
#sarahashjones@gmail.com
#GitHub: sarahajones 

################################################################################################

#load in required packages
library(plyr) 
library(readr) 

mydir = setwd("C:/Users/A-J/Desktop/confGrid/raw_data")
myfile = list.files(path=mydir, pattern="zapGrid*", full.names=TRUE) #pull file
dat_csv = ldply(myfile, read_csv) #load in data

#DATA ANONYMISATION
#create a PID code to identify participants more clearly. 
user_ids <- unique(dat_csv$user_id) # cache this to save recalculating 
dat_csv$PID <- NA_real_ # initalise PIDs as NA
dat_csv$PID <- sapply(
  dat_csv$user_id,  # vector to iterate over
  function(id) which(user_ids == id)  # function to apply to each element
)
#remove identifiable information - user_id column
dat_csv <- subset(dat_csv, select = -(user_id))


#DATA CLEANING/CHECKS
#check we have every participant
completeParticipants <- unique(dat_csv$PID)
numParticipants <- length(completeParticipants)
#answer should match up the number of imported files

if (length(myfile) != numParticipants){
  errorCondition('PARTICIPANT LOADING ERROR ') #error if all files did not load
} #numParticipants should be 76 

#check we recorded PIS and consent, record needed for our ethics,
if (nrow(PIS<- subset(dat_csv, PIS == 'TRUE')) != numParticipants){
  errorCondition('PIS ERROR')
}
if (nrow(consent <- subset(dat_csv, consent == 'TRUE')) != numParticipants){
  errorCondition('CONSENT ERROR')
}


#if no errors thrown - save data frame as clean csv for analysis - exclusion criteria will be applied dynamically in anlysis script
write.csv(dat_csv,"C:/Users/A-J/Desktop/ConfGrid/ConfGrid1_Study_CleanData.csv", row.names = FALSE)

