#install.packages("rpart.plot")
#install.packages("rpart")
#install.packages("rpart.plot")
# libraries
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)
library(popbio)


###K-means Clustering###

#importing the dataset into R
my_data <- read.table(file = "clipboard", 
                      sep = "\t", header=TRUE)

#convert the data frame into a numeric matrix
data <- as.matrix(sapply(my_data, as.numeric))

head(data)

#plot the data
plot(data)

data.3means <- kmeans (data , centers=3) 

#show the centers
data.3means$centers
#show the clusters
data.3means$cluster

#plot the groups
plot(data[data.3means$cluster ==1, ],
     col="red",
     xlim=c(min( data[,1] ), max( data[,1] ) ),
     ylim=c(min( data[,2] ), max( data[,2] ) )
)
points( data[ data.3means$cluster ==2, ],
        col="blue")

points( data[ data.3means$cluster ==3, ],
        col="seagreen")

#plot the centers on the plot
points(data.3means$centers , pch=2, col="green")

results<- kmeans(data , 3)
results

####################################
###Naive Bayes###

# data
data<- read.csv(file.choose(), header = T)
str(data)
xtabs(~diabetesMed+readmitted,data = data)
data$diabetesMed<-as.factor(data$diabetesMed)
data$readmitted<- as.factor(data$readmitted)

# visualization
data %>%
  ggplot(aes(x= readmitted,y= diabetesMed,fill= readmitted)) +
  geom_boxplot() +
  ggtitle("Box Plot")

#data partition
set.seed(1234)
ind <-sample(2,nrow(data),replace = T, prob = c(0.6, 0.4))
train <-data[ind == 1,]
test  <-data[ind ==2,]

#Naive bayes model
model<- naive_bayes(readmitted~ .,data = train, usekernel = T)
model

train%>%
  filter(readmitted == "YES" ) %>%
  summarise(mean(diabetesMed),sd(diabetesMed))
plot(model)

model<- naive_bayes(readmitted~ .,data = test)
model

test%>%
  filter(time_in_hospital == "0" ) %>%
  summarise(mean(readmitted),sd(readmitted))
plot(model)

# Predict
p <- predict(model,train, type = 'prob')
head(cbind(p, train))

# Confusion matrix - train data
p1 <- predict(model,train)
(tab1 <- table(p1 ,train$readmitted))
1 - sum(diag(tab1)) / sum(tab1)

# Confusion matrix - test data
p2 <- predict(model,test)
(tab2 <- table(p2 ,test$readmitted))
2 - sum(diag(tab2)) / sum(tab2)


##################################################

###Decision Tree###
data<- diabetic_data
str(data)
data$readmittedF<-factor(data$readmitted)
set.seed(1234)
pd<-sample(2,nrow(data),replace = TRUE,prob = c(0.8,0.2))
train<-data[pd==1,]
validate<-data[pd==2,]
library(party)
tree<-ctree(readmittedF~race+gender+age+num_lab_procedures+diabetesMed+num_medications,data=train)controls = ctree_control(mincriterion = 0.9,minsplit = 200)
tree
plot(tree)
predict(tree,validate)

#using rpart
library(rpart)
tree2 <-rpart(readmittedF~race+gender+age+num_lab_procedures+diabetesMed+num_medications,train)
library(rpart.plot)
rpart.plot(tree2)
predict(tree2,validate)

#misclassification error(train data)
tab<-table(predict(tree),train$readmittedF)
print(tab)
1-sum(diag(tab))/sum(tab)

#misclassification error(validate data)
testPred<-predict(tree,newdata=validate)
tab<-table(testPred,validate$readmittedF)
print(tab)
1-sum(diag(tab))/sum(tab)

##################################################

###Logistic Regression###


data<- read.csv(file.choose(),header = T)

diabetesMed <- data$diabetesMed

readmitted <- data$readmitted
readmittedCode <- ifelse(readmitted == "YES",1,0)
diabetesMedCode <- ifelse(diabetesMed == "Yes",1,0)
mymodel <- glm(readmittedCode~diabetesMed, binomial, data)
summary(mymodel)
prediction <- predict(mymodel, newdata = data, type ="response")
logi.hist.plot(diabetesMedCode,readmittedCode, boxp = FALSE, type='count', col= 'gray', xlabel = "Diabetes",
               ylabel = "Readmission probability")









