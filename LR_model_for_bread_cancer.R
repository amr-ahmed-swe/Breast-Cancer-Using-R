library(ggplot2)
library(cowplot)

# Import the data and set up header names 
dataset <- read.table("breast cancer.csv", header=FALSE,
                      sep=",")

#1 Clump Thickness = CT
#2 Uniformity of Cell Size = UCSize
#3 Uniformity of Cell Shape = UCShape
#4 Marginal Adhesion = MA
#5 Single Epithelial Cell Size = SECS
#6 Bare Nuclei = BN
#7 Bland Chromatin = BC
#8 Normal Nucleoli = NN

names(dataset) <- c('id','CT','UCSize','UCShape','MA','SECS','BN','BC','NN','Mitoses','Target')


#To see dataset structure 
str(dataset)

# cleaning Data 

#Drop id col 
dataset <- dataset[ , 2:11]


# dealing with Bn Col and transform target class to 0 / 1 col 
# for (value in 1:length(dataset$BN)) {
#   if(dataset$BN[value] == "?")
#     dataset$BN[value] = "0"
#   
#   # if the person don't have cancer "changing the value from 2 to 0 "
#   if(dataset$`Target Class`[value] == 2)
#     dataset$`Target Class`[value] = 0
#   
#   #person have cancer "from 4 to 1"
#   if(dataset$`Target Class`[value] == 4)
#     dataset$`Target Class`[value] = 1
# }

dataset[dataset == "?"] <- 0

dataset$Target <- ifelse(test=dataset$Target == 4, yes=1, no=0)

# dataset$Target <- ifelse(test=dataset$Target == 4, yes="Cancer", no="No Cancer")
# dataset$Target <- as.factor(dataset$Target)

dataset$BN <- as.integer(dataset$BN)


#to check 
#unique(dataset$BN)
#str(dataset$BN)


#split the data into training and test set 
#install.packages("caTools")
library(caTools)
set.seed(123)
split <- sample.split(dataset$Target , SplitRatio = 0.7)
training_set <- subset(dataset , split == TRUE)
test_set <- subset(dataset , split == FALSE)

#implement LR model 
model <- glm(formula = Target ~ . ,
             family = binomial,
             data = training_set)

summary(model)

predictions_prop <- predict(model , type = "response" , newdata = test_set[-10])

predictions <- ifelse(predictions_prop >0.5 , 1 , 0)

# Confusion matrix and measuring accuracy 
cm <- table(test_set[,10] , predictions )
accuracy <- sum(diag(cm)) / sum(cm) *100
accuracy

# Visualization
# predicted.data <- data.frame(
#   probability.of.cancer=model$fitted.values,
#   cancer=training_set$Target)

predicted.data <- data.frame(
  prediction.of.cancer=predictions,
  cancer=test_set$Target)

# predicted.data <- predicted.data[
#   order(predicted.data$probability.of.cancer, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)

# Plotting on the graph
ggplot(data=predicted.data, aes(x=rank, y=prediction.of.cancer)) +
  geom_point(aes(color=cancer), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of getting cancer")
