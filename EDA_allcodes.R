#LINEAR REGRESSION

rm(list=ls()) 	
library(readr)
data <- read_csv("/Users/nachiketgg/Documents/datasets/lab5/dataset.csv")
View(data)

data1=data[c(1:5),]
data1

data2=data[c(6:15),]
data2

data3=data[c(16:25),]
data3

rbind(data1,data2)

str(data)
library(caret)
library(ggplot2)
library(lattice)

indexes = createDataPartition(data$Effectiveness , p=.7 , list = F)
train = data[indexes, ]
test = data[-indexes, ]

nrow(train)
ncol(train)

cor(data$Effectiveness, data$Pyranometer)

model<-lm(Effectiveness~., data = data)

predicted<-predict(model,test)
predicted


#2. Store Temperature column : LCZ(TT=0), LCZ(TT=3), LCZ(TT=5), in variables V1, V2 and V3 respectively. Cbind all these three columns.
V1 <- data$LCZ0
V1

V2 <- data$LCZ3
V2

V3 <- data$LCZ5
V3

cbind(V1,V2,V3)

#3. Plot Time Vs Pyranometer reading of 'data' for the month May # data2 has been used in all the graphs since FN was allotted

library(ggplot2)
ggplot(data2,aes(x=Time,y=Pyranometer))+geom_point() 

#4. Plot Pyranometer reading Vs Two

ggplot(data2,aes(x=Pyranometer,y=Two_5))+geom_point() 

#5. Plot Pyranometer reading Vs Qact
ggplot(data2,aes(x=Pyranometer,y=Qact_5))+geom_point() 

#7. Determine the best linear fit equation between Pyranometer reading and Efficiency(acc to task split up)
mod=lm(Efficiency~Pyranometer, data2)
summary(mod)

coef(mod)

#8. Plot the best linear fit

ggplot(data2,aes(x=Efficiency,y=Pyranometer))+geom_point()

data_mod <- data.frame(Predicted = predict(mod), Observed = data2$Pyranometer)
ggplot(data_mod,aes(x = Predicted,y = Observed)) + geom_point() + geom_smooth(method=lm, se=FALSE)

#9. Determine the Residual Sum of Squares (RSS)
sum((resid(mod))^2)

#10. Plot the residuals
plot(resid(mod))


#LOGISTIC REGRESSION
rm(list=ls()) 	
library(readr)
data <- read_csv("/Users/nachiketgg/Documents/datasets/lab5/iris23.csv")
View(data)

data$species<-as.factor(data$species)
levels(data$species)

data2<-sample(2,nrow(data),replace=T, prob=c(0.75,0.25))
train<-data[data2==1, ] 
test<-data[data2==2, ]

model<-glm(species~sepal_length+sepal_width+petal_length+petal_width, data=train,family='binomial')
summary(model)

pre<-predict(model, train, type='response')
pre






















#ANOVA


#Pre-processing the data-set
data <- read.csv("Anova_Dataset.csv", header = TRUE)
processed_data <- na.omit(data)
head(processed_data)
summary(processed_data)
str(processed_data)
nrow(processed_data)

#Splitting the model**
library(caret)

indexs = createDataPartition(processed_data$yield, times = 1, p = 0.8, list = F) 
#times = no. of times to be split
#p = percentage of data to be used for training, here 80% is used of training and 20% for testing

train = processed_data[indexs, ]
nrow(train)
test = processed_data[-indexs, ]
nrow(test)

#Creating the model - One way Anova**
## ONE-WAY ANOVA
av1 <- aov(train$yield ~ train$density, data = train)
av1
summary(av1)

av2 <- aov(train$yield ~ train$block, data = train)
av2
summary(av2)

av3 <- aov(train$yield ~ train$fertilizer, data = train)
av3
summary(av3)

#Creating the model - Two way Anova**
av12 <- aov(train$yield ~ train$density + train$block + train$fertilizer, data = train)
av12
summary(av1)

#Finding the best fit**
library(AICcmodavg)

one.way <- av3
two.way <- av12
intr <- aov(train$yield ~ train$density*train$fertilizer, data = train)

model.set <- list(one.way, two.way, intr)
model.names <- c('one.way', 'two.way', 'intr')

aictab(model.set, modnames = model.names)

#Creating histogram**
hist(processed_data$yield)
#Conclusion: We found a statistically-significant difference in average crop yield by both fertilizer type (F(2)=9.018, p < 0.001) and by planting density (F(1)=15.316, p < 0.001).***


#SVM
install.packages("caTools")
library(caTools)
library(e1071)

dataset = read.csv('/Users/nachiketgg/Documents/datasets/lab5/social.csv', header=T)
dataset = dataset[3:5]

dataset$Purchased = factor(dataset$Purchased, levels = c(0,1))

set.seed(123)
split = sample.split(dataset$Purchased , SplitRatio = 0.75)

training_set = subset(dataset , split == TRUE)
test_set = subset(dataset , split == FALSE)

training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

classifier = svm(formula = Purchased ~ ., 
                 data=training_set, 
                 type = 'C-classification' , 
                 kernel = 'linear', cost=1)

y_pred = predict(classifier , newdata = test_set[-3])

cm = table(as.factor(test_set[,3]) , as.factor(y_pred))
print(cm)
confusionMatrix(cm)

(OR)

df<-read.csv('D:/suv_data.csv',stringsAsFactors = TRUE)
df = df[3:5]
head(df)
df<-na.omit(df)
sum(is.na(df))
df$Purchased = factor(df$Purchased, levels = c(0, 1))

df %>%ggplot(aes(x=Purchased, y=EstimatedSalary))+geom_line()

df %>%ggplot(aes(x=Purchased, y=Age))+geom_line()

df %>%   
  ggplot(aes(x=Purchased, y=EstimatedSalary,fill=Purchased,group=Purchased)) +  
  geom_boxplot() +theme_bw()+   
  ggtitle("Box Plot")


df %>%   
  ggplot(aes(x=Purchased, y=Age,fill=Purchased,group=Purchased)) +  
  geom_boxplot() +theme_bw()+   
  ggtitle("Box Plot")


#SPLITTING THE DATASET INTO TRAINING AND TESTING DATASET

set.seed(123)
split = sample.split(df$Purchased, SplitRatio = 0.70)

training_set = subset(df,split == TRUE)
test_set = subset(df,split == FALSE)


training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])


class(df$Purchased)
classifier_svm = svm(formula =Purchased~.,
                     data = training_set,
                     type = 'C-classification',
                     kernel = 'linear')

classifier_svm

y_pred = predict(classifier_svm, newdata = test_set[-3])

y_pred

cm = table(test_set[, 3], y_pred)
cm
confusionMatrix(cm)


precision<-28/(28+7)
recall<-28/(28+15)
f1<-(2*precision*recall)/(precision+recall)

precision


recall

f1





#DECISION TREE

install.packages("corrplot")
library(corrplot)
df<-read.csv("D:/glass.csv")
head(df)
names(df)




boxplot(df)


sum(is.na(df))
df<-na.omit(df)

cor(df)
corrplot(cor(df))
colnames(df)



set.seed(123)
train_index <- sample(1:nrow(df), 0.8*nrow(df))
train_data <- df[train_index,]
test_data <- df[-train_index,]
train_data$X1.2 <- as.factor(train_data$X1.2)
test_data$X1.2 <- as.factor(test_data$X1.2)

# Load the rpart package
library(rpart)

# Use Information Gain as the splitting criterion
df_tree_info_gain <- rpart(X1.2 ~ ., data = train_data, 
                           method = "class", parms = list(split = "information"))


# Use Gini Index as the splitting criterion
df_tree_gini_index <- rpart(X1.2 ~ ., data = train_data, 
                            method = "class", parms = list(split = "gini"))


# 2
# Load the rpart.plot package
library(rpart.plot)

# Visualize the decision tree using Information Gain as the splitting criterion
rpart.plot(df_tree_info_gain, 
           main = "Decision Tree - Information Gain", type = 2, extra = 101)


# Visualize the decision tree using Gini Index as the splitting criterion
rpart.plot(df_tree_gini_index, 
           main = "Decision Tree - Gini Index", type = 2, extra = 101)





