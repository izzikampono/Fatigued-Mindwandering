##
## SART model analysis
## Cognitive Science Practical 2022
##


##-----------------------------------##
## Section 2: Running an experiment  ##
##-----------------------------------##


# Read in the data
data_path_base <- "~/documents/uni/BASE-FINAL" # Change to the location of your output folder
data_path_fatigued <- "~/documents/uni/FATIGUE-FINAL"
#data_path_fatigued <- "~/documents/uni/fatigued-model" # Change to the location of your output folder
data_path2 <- "~/documents/uni/" # Change to the location of your output folder

# List of the data files

#for base model
behfiles1 <- list.files(path = data_path_base, pattern=".csv", full.names = TRUE)
behfiles1

#for fatigued model
behfiles2 <- list.files(path = data_path_fatigued, pattern=".csv", full.names = TRUE)
behfiles2


# Combine all data files into a single data frame (behdat)
base_model <- data.frame()
for (i in 1:length(behfiles1)) {
  base_model <- rbind(base_model, read.csv(behfiles1[i], sep = ",", strip.white = TRUE))
}

fatigued_model <- data.frame()
for (i in 1:length(behfiles2)) {
  fatigued_model <- rbind(fatigued_model, read.csv(behfiles2[i], sep = ",", strip.white = TRUE))
}



### mean reaction times
## note : means of all reaction times (not only accurate responses)


#base model
base_rt_clean <- base_model[base_model$rt!="NIL",]$rt
base_rt_clean <- as.numeric(base_rt_clean)
mean(base_rt_clean)

#fatigued model
f_rt_clean <-fatigued_model[fatigued_model$rt!="NIL",]$rt
f_rt_clean <-as.numeric((f_rt_clean))
mean(f_rt_clean)

base_model

###--------------------------------###
### checking accuracy of responses ###
### -------------------------------###

## base model ##

#correct vector : stores count of 1/0s that signify a correct/false response
correct = c()
for(i in 1:nrow(base_model)){
  if ((base_model[i,4] == "f") && (base_model[i,3]=="O")){
    print("pressed correctly")
    print(base_model[i,4])
    correct = append(correct,1)
  }
  else if ((base_model[i,4] == "NIL") && (base_model[i,3]=="Q")){
    print("withheld correctly")
    correct <-append(correct,1)
  }
  else{
    print("mind wandering")
    correct<- append(correct,0)
  }
}
correct
#put accuracy as a column in the dataset
base_model$correct = correct
length(correct)
length(base_model$participant)

### Do the same for the fatigued model ###
correct2 = c()
for(i in 1:nrow(fatigued_model)){
  if ((fatigued_model[i,4] == "f") && (fatigued_model[i,3]=="O")){
    print(fatigued_model[i,4])
    correct2 = append(correct2,1)
  }
  else if ((fatigued_model[i,4] == "NIL") && (fatigued_model[i,3]=="Q")){
    print(fatigued_model[i,4])
    correct2 <-append(correct2,1)
  }
  else{
    correct2<- append(correct2,0)
  }
}

length(correct2)

#put correct as a column in the dataset
fatigued_model$correct = correct2
head(fatigued_model)

## calculate accuracy for each participant across models#

acc<- function(vec){
  sum(vec)/200
}

acc2<- function(vec,v){
  sum(vec)/length(v)
}

base_model[base_model$correct==0,]

length(base_model[base_model$participant==3,]$correct)



non_target_base <- base_model[base_model$stimulus=="Q",]
non_target_fatigue <- fatigued_model[fatigued_model$stimulus=="Q",]

t.test(non_target_base,non_target_fatigue)

group_mean_base    <- setNames(aggregate(base_model$correct, by = list(base_model$participant),
                                FUN = function(z) acc(z)),c("participant", "mean_accuracy"))
group_mean_base
group_mean_fatigue <- setNames(aggregate(fatigued_model$correct, by = list(fatigued_model$participant),
                                FUN = function(z) acc(z)),c("participant", "mean_accuracy"))


group_mean_base_nonT <-setNames(aggregate(non_target_base$correct, by = list(non_target_base$participant),
                                          FUN = function(z) mean(z)),c("participant", "mean_accuracy"))

group_mean_fatigue_nonT <-setNames(aggregate(non_target_fatigue$correct, by = list(non_target_fatigue$participant),
                                             FUN = function(z) mean(z)),c("participant", "mean_accuracy"))
group_mean_base_nonT
group_mean_fatigue_nonT


group_mean_base
group_mean_fatigue

#make a dataframe to hold accuracy across participants
grouped_means = data.frame(group_mean_base$mean_accuracy,group_mean_fatigue$mean_accuracy, cols =c("base","fatigued"))
colnames(grouped_means) = c("base","fatigued")


grouped_means

library(ggplot2)

group_mean_base
group_mean_fatigue

ggplot() + geom_point(data = group_mean_base,aes(x=participant,y=mean_accuracy, fill= "red"))

ggplot(group_mean_fatigue, aes(x= factor(participant), y = mean_accuracy)) + 
  geom_bar(stat="identity", position ="dodge",, fill = "lightblue")+
  coord_cartesian(ylim=c(0.7,1)) +
  xlab("participant") +
  ylab("accuracy across trials")  + ggtitle("accuracy of fatigued model") +
  geom_hline(yintercept=mean(group_mean_fatigue$mean_accuracy),linetype="dashed", 
             color = "blue", size=1)


 ggplot(group_mean_base, aes(x= factor(participant), y = mean_accuracy)) + 
  geom_bar(stat="identity", position ="dodge",, fill = "pink")+
  coord_cartesian(ylim=c(0.7,1)) +
  xlab("participant") +
  ylab("accuracy across trials")  + ggtitle("accuracy of base model") +
  geom_hline(yintercept=mean(group_mean_base$mean_accuracy),linetype="dashed", 
             color = "red", size=1)

group_means = rbind(group_mean_base,group_mean_fatigue)
group_means$type = c(rep("base",25),rep("fatigue",25))

group_means

#plot of accuracies of participants across models
ggplot(group_mean_base, aes(x= factor(participant), y = mean_accuracy)) + 
  geom_bar(stat="identity", position ="dodge")+
  coord_cartesian(ylim=c(0,1)) +
  xlab("participant") +
  ylab("accuracy across trials") +
  geom_hline(yintercept=mean(group_mean_fatigue$mean_accuracy),linetype="dashed", 
             color = "blue", size=1) +
  geom_hline(yintercept=mean(group_mean_base$mean_accuracy), linetype="dashed", 
                             color = "red", size=1)

ggplot(group_means, aes(x= factor(participant), y = mean_accuracy, fill = type)) + 
  geom_bar(stat="identity", position ="dodge")+
  coord_cartesian(ylim=c(0,1)) +
  xlab("participant") +
  ylab("accuracy across trials")
  
# means and sd
group_mean_fatigue

mean(group_mean_fatigue$mean_accuracy)
sd(group_mean_fatigue$mean_accuracy)

mean(group_mean_base$mean_accuracy)
sd(group_mean_base$mean_accuracy)

mean(group_mean_base_nonT$mean_accuracy)
sd(group_mean_base_nonT$mean_accuracy)

mean(group_mean_fatigue_nonT$mean_accuracy)
sd(group_mean_fatigue_nonT$mean_accuracy)


##Normality tests 
shapiro.test(group_mean_fatigue$mean_accuracy)
shapiro.test(group_mean_base$mean_accuracy)

shapiro.test(group_mean_base_nonT$mean_accuracy)



#### non target trials analysis ####

group_means_nonT = rbind(group_mean_base_nonT,group_mean_fatigue_nonT)
group_means_nonT$type = c(rep("base",length(group_mean_base_nonT$participant)),rep("fatigue",length(group_mean_fatigue_nonT$participant)))
colnames(group_means_nonT) = c("participant","mean_accuracy","type")
head(group_means_nonT)

group_means

ggplot(group_means_nonT, aes(x= factor(participant), y = mean_accuracy, fill = type)) + 
  geom_bar(stat="identity", position ="dodge")+
  coord_cartesian(ylim=c(0,0.3)) +
  xlab("participant") +
  ylab("accuracy across non-target trials") +
  geom_hline(yintercept=mean(group_mean_base_nonT$mean_accuracy),linetype="dashed", 
             color = "red", size=1) +
  geom_hline(yintercept=mean(group_mean_fatigue_nonT$mean_accuracy), linetype="dashed", 
             color = "blue", size=1)

group_means$type = as.factor(group_means$type)

group_means

p <- ggplot(group_means, aes(x=type, y=mean_accuracy)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=4) +
  stat_summary(fun=mean, geom="point", shape=23, size=4) + ylim(0.9,1)
p
p




## t-test on the grouped accuracies
?t.test
t.test(grouped_means$base,grouped_means$fatigued, alternative = "greater")

## t-test on non target stimulus

t.test(group_mean_base_nonT$mean_accuracy,group_mean_fatigue_nonT$mean_accuracy, alternative = "greater")

wilcox.test(grouped_means$base
          , grouped_means$fatigued)


## Analysis

# What is the mean response accuracy, and what is the standard error of the mean? Aggregate within and then across participants.
?data.frame()
mean(base_model$correct)
mean(fatigued_model$correct)


#### analysis of non-target stimulus ####


















############## ANALYSIS OF REACTION TIMES #############


###-----------------------------------------###
### checking Reaction times of participants ###
### ----------------------------------------###




#make function to get mean reaction times of each participant, 
#disincluding the nil reactions where they dont press a button
mean_rts_clean<- function(r){
  r = as.numeric(r)
  r = r[!is.na(r)]
  mean(r)
}

all = rbind(base_model,fatigued_model)
all$model = c(rep("base", length(base_model$participant)),rep("fatigue",length(base_model$participant)))
all_correct = all[all$correct==1,]
base_correct =as.numeric(all_correct[all_correct$model=="base",]$rt)
fatigue_correct =as.numeric(all_correct[all_correct$model=="fatigue",]$rt)

group_mean_rt_base   <- setNames(aggregate(base_model$rt, by = list(base_model$participant),
                                         FUN = function(z) mean_rts_clean(z)),c("participant", "mean_rt"))

group_mean_rt_fatigue <- setNames(aggregate(fatigued_model$rt, by = list(fatigued_model$participant),
                                         FUN = function(z) mean_rts_clean(z)),c("participant", "mean_rt"))

group_mean_rt_base
group_mean_rt_fatigue

# means and sds #

mean(group_mean_rt_fatigue$mean_rt)
sd(group_mean_rt_fatigue$mean_rt)

mean(group_mean_rt_base$mean_rt)
sd(group_mean_rt_base$mean_rt)

# plots # 

group_rts = rbind(group_mean_rt_base,group_mean_rt_fatigue)
group_rts$type = c(rep("base",25),rep("fatigue",25))

ggplot(group_rts, aes(factor(participant), mean_rt, fill = type)) + 
  geom_bar(stat="identity", position = "dodge")+
  coord_cartesian(ylim=c(0,0.3)) +
  xlab("participant") +
  ylab("average reaction times across participants (s)") +
  geom_hline(yintercept=mean(group_mean_rt_fatigue$mean_rt),linetype="dashed", 
             color = "blue", size=0.5) +
  geom_hline(yintercept=mean(group_mean_rt_base$mean_rt), linetype="dashed", 
             color = "red", size=0.5)


ggplot(group_mean_rt_fatigue, aes(factor(participant), mean_rt)) + 
  geom_bar(stat="identity", position = "dodge", fill = "lightblue")+
  coord_cartesian(ylim=c(0,0.3)) +
  xlab("participant") +
  ylab("average reaction times across participants (s)") +
  geom_hline(yintercept=mean(group_mean_rt_fatigue$mean_rt),linetype="dashed", 
             color = "blue", size=0.5) + ggtitle("Reaction times of fatigued model")
  
ggplot(group_mean_rt_base, aes(factor(participant), mean_rt)) + 
  geom_bar(stat="identity", position = "dodge", fill = "pink")+
  coord_cartesian(ylim=c(0,0.3)) +
  xlab("participant") +
  ylab("average reaction times across participants (s)") +
  geom_hline(yintercept=mean(group_mean_rt_base$mean_rt),linetype="dashed", 
             color = "red", size=0.5) + ggtitle("Reaction times of base model")




#t-test between the models #
t.test(group_mean_rt_base$mean_rt,group_mean_rt_fatigue$mean_rt,alternative="greater")

mean(group_mean_rt_base$mean_rt)
sd(group_mean_rt_base$mean_rt)

mean(group_mean_rt_fatigue$mean_rt)
sd(group_mean_rt_fatigue$mean_rt)







# What is the mean response time and standard error? Aggregate within and then across participants.




##---------------------------------------##
## Section 3: Analysing the model trace ##
##---------------------------------------##

                    ## PART 1 :: Analysis of goal chunk activations ##

## The easiest way to do this is to read the trace file line by line wihle keeping track of the state of the model

library(stringr)
library(data.table)
library(dplyr)
library(tidyverse)

# Read the trace file into memory
base_model_trace <- file(paste0(data_path_base, "/sart-trace.txt"), "r")
fatigued_model_trace <- file(paste0(data_path_fatigued, "/sart-trace.txt"), "r")

b_lines <- readLines(base_model_trace)
f_lines <- readLines(fatigued_model_trace)

participant <- 0L
time <- 1
n <- length(b_lines)
activations <- data.table(participant = rep(0L, n), time = rep(0, n), activation = rep(0, n), chunk_type = rep("",n)) 
idx <- 1L 
activations
for(i in 1:length(b_lines)) {

  # Read a single line
  line <- b_lines[i]
  
  # Detect the start of a new model run
  if(str_detect(line, "Run \\d+")) {
    participant <- participant + 1L
    print(participant)
  }
  
  # Check whether the line contains the activation of the ATTEND chunk, and then store the value
  # Hints:
  #   - you can use str_detect() and str_extract(). See http://stringr.tidyverse.org/
  #   - use regular expressions to describe string patterns. A good resource is https://regexr.com/ 
  #   - you will also need to keep track of the time, which is given at the start of many (but not all!) lines in the trace file. 

  if (str_detect(line, "Chunk ATTEND with activation")) {
    activation = as.numeric(str_extract(line, "\\d\\.\\d+"))
    line <- b_lines[i + 1L]
    time = as.numeric(str_extract(line, "\\d+\\.\\d+"))
    set(activations, idx, j = 1:4, value = list(participant, time, activation,"attend"))
    idx <- idx + 1L
  }
  else if (str_detect(line, "Chunk WANDER with activation")) {
    activation = as.numeric(str_extract(line, "\\d\\.\\d+"))
    line <- b_lines[i + 1L]
    time = as.numeric(str_extract(line, "\\d+\\.\\d+"))
    set(activations, idx, j = 1:4, value = list(participant, time, activation,"wander"))
    idx <- idx + 1L
  }
}

head(activations)

### make same table for the fatigued model ###

activations_fatigue <- data.table(participant = rep(0L, n), time = rep(0, n), activation = rep(0, n), chunk_type = rep("",n)) 
idx <- 1L 
participant <- 0L

for(i in 1:length(f_lines)) {
  
  # Read a single line
  line <- f_lines[i]
  
  # Detect the start of a new model run
  if(str_detect(line, "Run \\d+")) {
    participant <- participant + 1L
    print(participant)
  }
  
  # Check whether the line contains the activation of the ATTEND chunk, and then store the value
  # Hints:
  #   - you can use str_detect() and str_extract(). See http://stringr.tidyverse.org/
  #   - use regular expressions to describe string patterns. A good resource is https://regexr.com/ 
  #   - you will also need to keep track of the time, which is given at the start of many (but not all!) lines in the trace file. 
  #   - you can add a line to the activations data.table using set(activations, idx, j = 1:3, value = list(participant, time, activation)). See ?set for more information.
  
  if (str_detect(line, "Chunk ATTEND with activation")) {
    activation = as.numeric(str_extract(line, "\\d\\.\\d+"))
    line <- f_lines[i + 1L]
    time = as.numeric(str_extract(line, "\\d+\\.\\d+"))
    set(activations_fatigue, idx, j = 1:4, value = list(participant, time, activation,"attend"))
    idx <- idx + 1L
  }
  else if (str_detect(line, "Chunk WANDER with activation")) {
    activation = as.numeric(str_extract(line, "\\d\\.\\d+"))
    line <- f_lines[i + 1L]
    time = as.numeric(str_extract(line, "\\d+\\.\\d+"))
    set(activations_fatigue, idx, j = 1:4, value = list(participant, time, activation,"wander"))
    idx <- idx + 1L
  }
}

head(activations_fatigue)



############ filtering and binning data ########################


#filter out the empty rows
activations = activations[activations$activation>0]
activations_fatigue = activations_fatigue[activations_fatigue$activation>0]



## for base model ##
binned_activations <- mutate(activations, time_bin = cut(time, breaks=500, labels=seq(1,500))) #500 is the time limit
averaged_binned <- aggregate(binned_activations$activation, by = list(binned_activations$time_bin,binned_activations$chunk_type), FUN = mean)
colnames(averaged_binned) = c("time","chunk_type","activation")
averaged_binned$time = as.numeric(averaged_binned$time)

## for fatigued model ##

binned_activations_f <- mutate(activations_fatigue, time_bin = cut(time, breaks=500, labels=seq(1,500))) #500 is the time limit
averaged_binned_f <- aggregate(binned_activations_f$activation, by = list(binned_activations_f$time_bin,binned_activations_f$chunk_type), FUN = mean)
colnames(averaged_binned_f) = c("time","chunk_type","activation")
averaged_binned_f$time = as.numeric(averaged_binned_f$time)

## check tables ##

head(averaged_binned)
head(averaged_binned_f)

## Create a plot of mean activation of the ATTEND chunk over time, averaged across participants. ##

#base model
ggplot() + geom_point(data = averaged_binned, aes(x = time, y = activation, color = chunk_type),size=2) +
  ggtitle(" Chunk activation plot of base model ")


# fatigued model
ggplot() + geom_point(data = averaged_binned_f, aes(x = time, y = activation, color = chunk_type),size=2) +
  ggtitle(" Chunk activation plot of fatigued model ")


attend_activation_base = averaged_binned[averaged_binned$chunk_type=="attend",]$activation
wander_activation_base = averaged_binned[averaged_binned$chunk_type=="wander",]$activation

attend_activation_f = averaged_binned_f[averaged_binned_f$chunk_type=="attend",]$activation
wander_activation_f = averaged_binned_f[averaged_binned_f$chunk_type=="wander",]$activation



### t-test comparing activation levels of goal chunks across models ##
t.test(attend_activation_f,attend_activation_base, alternative="less")

t.test(attend_activation_base,wander_activation_base, alternative = "less")
t.test(attend_activation_base,wander_activation_base, alternative = "greater")
t.test(attend_activation_base,wander_activation_base)

#t-test on differnece of activations
t.test(wander_activation_f-attend_activation_f,wander_activation_base-attend_activation_base, alternative ="greater")

t.test(wander_activation_f,wander_activation_base,alternative = "less")


mean(wander_activation_f-attend_activation_f)
sd(wander_activation_base)

mean(wander_activation_base-attend_activation_base)
sd(wander_activation_f)



?t.test()



####################################################################################

### counting mw chunk vs attend chunk retrievals ###

head(activations)

attend_base = length(activations[activations$chunk_type=="attend",]$chunk_type)
wander_base = length(activations[activations$chunk_type=="wander",]$chunk_type)

attend_f = length(activations_fatigue[activations_fatigue$chunk_type=="attend",]$chunk_type)
wander_f = length(activations_fatigue[activations_fatigue$chunk_type=="wander",]$chunk_type)

#percentage of goal chunk retrievals
attend_base/(attend_base+wander_base)
attend_base

attend_f/(attend_f+wander_f)
attend_f

wander_f
wander_base


### ------------------------------------------ ###
### analysis of activation of random memories  ###
### ------------------------------------------ ###

# Read the trace file into memory
base_model_trace <- file(paste0(data_path_base, "/sart-trace.txt"), "r")
fatigued_model_trace <- file(paste0(data_path_fatigued, "/sart-trace.txt"), "r")

b_lines <- readLines(base_model_trace)
f_lines <- readLines(fatigued_model_trace)


## extract activations from trace ##
# base model #
participant <- 0L
time <- 0
n <- length(b_lines)
memory_activations_b <- data.table(participant = rep(0L, n), time = rep(0, n), activation = rep(0, n), chunk_type = rep("",n)) 
idx <- 1L 

for(i in 1:length(b_lines)) {
  
  # Read a single line
  line <- b_lines[i]
  
  # Detect the start of a new model run
  if(str_detect(line, "Run \\d+")) {
    participant <- participant + 1L
    print(participant)
  }
  
  if (str_detect(line, "Chunk ATTEND-MEMORY with activation")) {
    activation = as.numeric(str_extract(line, "\\d\\.\\d+"))
    line <- b_lines[i + 1L]
    time = as.numeric(str_extract(line, "\\d+\\.\\d+"))
    set(memory_activations_b, idx, j = 1:4, value = list(participant, time, activation,"Attend-memory"))
    idx <- idx + 1L
  }
  else if (str_detect(line, "Chunk RANDOM-MEMORY with activation")) {
    activation = as.numeric(str_extract(line, "\\d\\.\\d+"))
    line <- b_lines[i + 1L]
    time = as.numeric(str_extract(line, "\\d+\\.\\d+"))
    set(memory_activations_b, idx, j = 1:4, value = list(participant, time, activation,"Random-memory"))
    idx <- idx + 1L
  }
 
}
memory_activations_b
memory_activations_b[memory_activations_b$chunk_type=="wander"]

### make same table for the fatigued model ###

participant <- 0L
time <- 1
n <- length(f_lines)
memory_activations_f <- data.table(participant = rep(0L, n), time = rep(0, n), activation = rep(0, n), chunk_type = rep("",n)) 
idx <- 1L 

for(i in 1:length(f_lines)) {
  
  # Read a single line
  line <- f_lines[i]
  
  # Detect the start of a new model run
  if(str_detect(line, "Run \\d+")) {
    participant <- participant + 1L
    print(participant)
  }
  
  if (str_detect(line, "Chunk ATTEND-MEMORY with activation")) {
    activation = as.numeric(str_extract(line, "\\d\\.\\d+"))
    line <- f_lines[i + 1L]
    time = as.numeric(str_extract(line, "\\d+\\.\\d+"))
    set(memory_activations_f, idx, j = 1:4, value = list(participant, time, activation,"Attend-memory"))
    idx <- idx + 1L
  }
  else if (str_detect(line, "Chunk RANDOM-MEMORY with activation")) {
    activation = as.numeric(str_extract(line, "\\d\\.\\d+"))
    line <- f_lines[i + 1L]
    time = as.numeric(str_extract(line, "\\d+\\.\\d+"))
    set(memory_activations_f, idx, j = 1:4, value = list(participant, time, activation,"Random-memory"))
    idx <- idx + 1L
  }
  
}

#### filter out empty rows from data tabele

memory_activations_b = memory_activations_b[memory_activations_b$activation>0]
memory_activations_f = memory_activations_f[memory_activations_f$activation>0]

memory_activations_b

### binning data by seconds

memory_activation_binned_b
binned_memory_activations_f <- mutate(memory_activations_f, time_bin = cut(time, breaks=500, labels=seq(1,500))) #500 is the time limit
binned_memory_activations_f
memory_activation_binned_f <- aggregate(binned_memory_activations_f$activation, by = list(binned_memory_activations_f$time_bin,binned_memory_activations_f$chunk_type), FUN = mean)
activation_participant_f <-aggregate(binned_memory_activations_f$activation, by = list(binned_memory_activations_f$participant,binned_memory_activations_f$chunk_type), FUN = mean)

colnames(memory_activation_binned_f) = c("time","chunk_type","activation")
memory_activation_binned_f$time = as.numeric(averaged_binned_f$time)

head(memory_activation_binned_f)



binned_memory_activations_b <- mutate(memory_activations_b, time_bin = cut(time, breaks=500, labels=seq(1,500))) #500 is the time limit
binned_memory_activations_b
memory_activation_binned_b <- aggregate(binned_memory_activations_b$activation, by = list(binned_memory_activations_b$time_bin,binned_memory_activations_b$chunk_type), FUN = mean)
colnames(memory_activation_binned_b) = c("time","chunk_type","activation")
memory_activation_binned_b$time = as.numeric(memory_activation_binned_b$time)
activation_participant_b <-aggregate(binned_memory_activations_b$activation, by = list(binned_memory_activations_b$participant,binned_memory_activations_b$chunk_type), FUN = mean)

tail(memory_activation_binned_b)

## putting activations by participant into one dataframe ##
x = rbind(activation_participant_b,activation_participant_f)
setNa

activation_participant_b


###### Plotting #######

#base model
ggplot() + geom_point(data = memory_activation_binned_b, aes(x = time, y = activation, color = chunk_type),size=2) +
  ggtitle("Plot of memory activations of the base model ") +ylim(5,6)


# fatigued model
ggplot() + geom_point(data = memory_activation_binned_f, aes(x = time, y = activation, color = chunk_type),size=2) +
  ggtitle(" Plot of memory activations of the fatigued model ") + ylim(5,6)

#### T-TEST on random memory activations ####

random_activation_b = memory_activation_binned_b[memory_activation_binned_b$chunk_type=="Random-memory",]$activation
random_activation_f = memory_activation_binned_f[memory_activation_binned_f$chunk_type=="Random-memory",]$activation

attend_memory_activation_b = memory_activation_binned_b[memory_activation_binned_b$chunk_type=="Attend-memory",]$activation
attend_memory_activation_f = memory_activation_binned_f[memory_activation_binned_f$chunk_type=="Attend-memory",]$activation

t.test(random_activation_b,random_activation_f)
t.test(attend_memory_activation_b,attend_memory_activation_f)

### -------------------------------------------------###
###         PART 3:  DUration of mind wandering      ###
### -------------------------------------------------###


## Read the trace file into memory
base_model_trace <- file(paste0(data_path_base, "/sart-trace.txt"), "r")
fatigued_model_trace <- file(paste0(data_path_fatigued, "/sart-trace.txt"), "r")

b_lines <- readLines(base_model_trace)
f_lines <- readLines(fatigued_model_trace)


## extract activations from trace ##

# fatigue model #
participant <- 0L
time <- 0
n <- length(b_lines)
MW_duration <- data.table(participant = rep(0L, n), start_time = rep(0, n), end_time = rep(0, n)) 
idx <- 1L 
flag = 0

for(i in 1:length(f_lines)) {
  
  # Read a single line
  line <- f_lines[i]
  
  # Detect the start of a new model run
  if(str_detect(line, "Run \\d+")) {
    
    if (flag==1) {
      set(MW_duration,idx, j = 1:3, value = list(participant, time_start, 500))
      idx = idx +1L
      print("IN IF STATEMENT")
      
    }
    flag=0
    participant <- participant + 1L
    print(participant)
  }
  
  if (str_detect(line, "Chunk WANDER with activation") && flag==0) {
    line <- f_lines[i + 1L]
    time_start = as.numeric(str_extract(line, "\\d+\\.\\d+"))
    flag = 1
  }
  if (str_detect(line, "Chunk ATTEND with activation") && flag==1){
    line <- f_lines[i + 1L]
    time_end = as.numeric(str_extract(line, "\\d+\\.\\d+"))
    flag = 0
    set(MW_duration, idx, j = 1:3, value = list(participant, time_start, time_end))
    idx= idx+1L
  }
}



MW_duration = MW_duration[MW_duration$participant>0,]

MW_duration$duration = MW_duration$end_time - MW_duration$start_time

## Base Model

participant <- 0L
time <- 0
n <- length(b_lines)
MW_duration_base <- data.table(participant = rep(0L, n), start_time = rep(0, n), end_time = rep(0, n)) 
idx <- 1L 
flag = 0

for(i in 1:length(b_lines)) {
  
  # Read a single line
  line <- b_lines[i]
  
  # Detect the start of a new model run
  if(str_detect(line, "Run \\d+")) {
    if (flag==1) {
      set(MW_duration,idx, j = 1:3, value = list(participant, time_start, 500))
      idx = idx +1L
      print("IN IF STATEMENT")
      
    }
    flag=0
    participant <- participant + 1L
    print(participant)
  }
  
  if (str_detect(line, "Chunk WANDER with activation") && flag==0) {
    line <- b_lines[i + 1L]
    time_start = as.numeric(str_extract(line, "\\d+\\.\\d+"))
    flag = 1
  }
  if (str_detect(line, "Chunk ATTEND with activation") && flag==1){
    line <- b_lines[i + 1L]
    time_end = as.numeric(str_extract(line, "\\d+\\.\\d+"))
    flag = 0
    set(MW_duration_base, idx, j = 1:3, value = list(participant, time_start, time_end))
    idx= idx+1L
  }

}

MW_duration_base= MW_duration_base[MW_duration_base$participant>0,]

MW_duration_base$duration = MW_duration_base$end_time - MW_duration_base$start_time
MW_duration_base


#check if there are any negative values in the dataframe 
MW_duration_base[MW_duration_base$duration<0,]
MW_duration[MW_duration$duration<0,]



## look at means and sd 

mean(MW_duration_base$duration)


mean(MW_duration$duration)



## join the two dataframes together

MW_duration_join = rbind(MW_duration,MW_duration_base)
MW_duration_join$model = c(rep("fatigue",length(MW_duration$participant)),rep("base",length(MW_duration_base$participant)))

hist(MW_duration_base$duration)

ggplot(MW_duration, aes(x=duration)) + geom_histogram(color="red")


MW_duration[MW_duration$participant==1,]

#mind wandering durations per participant

ggplot(MW_duration_join, aes(x=factor(participant), y=duration)) +
  geom_boxplot()
ggplot(MW_duration, aes(x=factor(participant), y=duration)) +
  geom_boxplot()

ggplot() + geom_point(data = MW_duration, aes(x = participant, y = duration, color = model),size=2) +
  ggtitle("Mind Wandering Durations") + ylim(0,0.5)
ggplot() + geom_point(data = MW_duration_base, aes(x = factor(participant), y = duration),size=2) +
  ggtitle("Mind Wandering Durations") + ylim(0,0.5)

MW_duration_base

#Mind wadnering durations over time
ggplot() + geom_point(data = MW_duration, aes(x = start_time, y = duration, color = participant),size=2) +
  ggtitle("Mind Wandering Durations") + ylim(0,0.5)

ggplot() + geom_point(data = MW_duration[MW_duration$participant==1,], aes(x = start_time, y = duration, color = participant),size=2) +
  ggtitle("Mind Wandering Durations") + ylim(0,0.5)



#### TRY BINNING THE DATA PER TRIAL ###

binned_MW_durations <- mutate(MW_duration_join, trial_num = cut(start_time, breaks=250, labels=seq(1,250))) #500 is the time limit
binned_MW_durations
binned_MW <- aggregate(binned_MW_durations$duration, by = list(binned_MW_durations$trial_num,binned_MW_durations$model), FUN = max)
binned_MW$Group.1 = as.numeric(binned_MW$Group.1)
binned_MW
#plot binned data
ggplot() + geom_point(data=binned_MW, aes(x=Group.1 , y = x , color = Group.2)) + 
  ylim(0,2) + ylab("MW durations (s)")


binned_memory_activations_b <- mutate(memory_activations_b, time_bin = cut(time, breaks=500, labels=seq(1,500))) #500 is the time limit
binned_memory_activations_b
memory_activation_binned_b <- aggregate(binned_memory_activations_b$activation, by = list(binned_memory_activations_b$time_bin,binned_memory_activations_b$chunk_type), FUN = mean)
colnames(memory_activation_binned_b) = c("time","chunk_type","activation")
memory_activation_binned_b$time = as.numeric(memory_activation_binned_b$time)
activation_participant_b <-aggregate(binned_memory_activations_b$activation, by = list(binned_memory_activations_b$participant,binned_memory_activations_b$chunk_type), FUN = mean)



t.test(binned_MW[binned_MW$Group.2=="base",]$x,binned_MW[binned_MW$Group.2=="fatigue",]$x)


