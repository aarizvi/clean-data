library(plyr)
library(reshape2)
library(dplyr)

setwd("/Users/aarizvi/Desktop/R/")

train.dir <- c("UCI HAR Dataset/train/") 

train.files <- list.files(train.dir, full.names=T) #collect training files directory
train.files <- train.files[-1] #remove interial signals folder

test.dir <- c("UCI HAR Dataset/test/") #collect test files directory
test.files <- list.files(test.dir, full.names=T)
test.files <- test.files[-1]

df <- as.data.frame(lapply(train.files, read.table, sep="\t", header=F)) #compile x, y, and subject files into one data frame
colnames(df) <- c("subject", "train_set", "train_label") #name columns according to proper subject and experiment type columns
df.test <- as.data.frame(lapply(test.files,read.table,sep="\t", header=F)) 
colnames(df.test) <- c("subject", "test_set", "test_label")

df.compiled <- merge(df, df.test, all=TRUE) #merge the training and test data frames
df.compiled[ order(df.compiled$subject),]

#melt compiled data so that the train and test labels are on the same column
df.compiled <- melt(df.compiled, id= c("subject", "train_set", "test_set"), measure.vars=c("train_label", "test_label"))
colnames(df.compiled) <- c("subject", "train_set", "test_set", "train_test", "label" )

df.compiled <- melt(df.compiled, id= c("subject", "train_test", "label"), measure.vars=c("train_set", "test_set"))
levels(df.compiled$variable) <- c("training", "test")

#rename the variable to experiment type - which is whether the activity was training or test
df.compiled <- rename(df.compiled, exp_type = variable)
df.compiled <- df.compiled[,-2]


labels <- read.table("UCI HAR Dataset/activity_labels.txt") #read activity label file
df.compiled <- mutate(df.compiled, label = as.factor(label)) #mutate column "label" as a factor
levels(df.compiled$label) <- labels$V2 #change the number id of activity into a descripitive label that was in activitylabels.txt

meas_mean <- function(x) {mean(as.numeric(strsplit(x, " ")[[1]]), na.rm=T)} #write function to calculate mean of measurements

measurement_mean <- sapply(df.compiled$value, meas_mean) #add mean column 

measurements <- NULL
#for loop to store all measurement means in a vector
for(i in 1:length(measurement_mean)) {
    measurements[i] <- measurement_mean[[i]][1]
}

#add measurements to compiled data frame
df.compiled <- cbind(df.compiled[,-4],measurements) 

#condense average duplicates and calcualte standard deviation of duplicates
df.c2 <- ddply(df.compiled, .(subject,label,exp_type), summarize,
               mean=round(mean(measurements),2),
               sd = round(sd(measurements),2))

df.final <- df.c2[complete.cases(df.c2),] #remove NAs
rownames(df.final) <- NULL #remove rownames

write.table(df.final, file="tidy_data.txt", row.names=F, sep="\t")


