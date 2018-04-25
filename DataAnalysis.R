#Load raw data
test <- read.csv("test.csv", header = TRUE)
train <-read.csv("train.csv", header=TRUE)

#Add a "Survived" variable to the test data set to allow the combination of data sets
test.survived <- data.frame(Survived = rep("None", nrow(test)), test[,])

#Combine data sets
data.combined <- rbind(train, test.survived)

#A bit about R data types (eg: factors)
str(data.combined)

#Represent a data set as a factor
data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)

#Gross survival rates 
table(data.combined$Survived)

#Distribution across classes
table(data.combined$Pclass)

#Load up ggplot2 package to use for visualization 
library(ggplot2)

str(train)

#Drawing a histogram using aes that is for a-statistics which describes how the data looks like
#Hypothesis- rich folks survived at a higher rate 
train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x =Pclass, fill=factor(Survived))) + 
  geom_bar(width = 0.5) + 
  xlab("Pclass")+
  ylab("Total count")+
  labs(fill = "Survived")
#Examine the first few names from the training data set
head(as.character(train$Name))

#How many unique names are there accross the train and test data sets? expected 1309
length(unique(as.character(data.combined$Name)))

#1307 is the number of names therefore there are two duplicate names accross the data sets
#First get the duplicate names and store them as a vector
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])
str(dup.names)


#Look into the combined data set and find the duplicate names 
data.combined[which(data.combined$Name %in% dup.names), ]

#Any correlation with the variables(eg: sibsp)
misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")), ]
misses[1:5, ]


#Check out females if the pattern continues
females <- data.combined[which(train$Sex=="female"), ]
females[1:5, ]

#Expand upon the relationship between the 'Survivied' and 'Pclass' by creating and adding a new variable(feature) called 'Title'
#onto the data.combined data set and explore a potential 3-dimensional relationship.
#Create a utility function to help with the extraction
extractitle <- function(Name){
  Name <- as.character(Name)
  if(length(grep("Miss.", Name))>0){
    return ("Miss.")
  }else if(length(grep("Master.", Name))>0){
    return ("Master.")
  }else if(length(grep("Mrs.",Name))>0){
    return ("Mrs.")
  }else if(length(grep("Mr.", Name))>0){
    return ("Mr.")
  }else{
    return ("Others")
  }
}

titles <- NULL
for(i in 1:nrow(data.combined)){
  titles <- c(titles, extractitle(data.combined[i, "Name"]))
  
}
data.combined$Title <- as.factor(titles)

#Use only 891 instead of 1309 rows as we only have the survived table
#for the train set which is 891 rows.
ggplot(data.combined[1:891, ], aes(x=Title, fill = Survived))+
  geom_bar(width = 0.5)+
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("Title")+
  ylab("Total count")+
  labs(fill = "Survived")
