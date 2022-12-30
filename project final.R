library(naniar)
library(ggplot2)
library(ggpubr)
library(ggcorrplot)
library(bestglm)
library(pROC) #for ROC curve
library(caret) 
library(glmnet)
library(MASS) 
library(class)
library(knitr)

## Read data into R
set.seed(88)
Stroke_data<-data.frame(read.csv("/Users/nhivutu/Desktop/Math 448/Rproject/healthcare-dataset-stroke-data.csv"))
head(Stroke_data)
summary(Stroke_data)

###################
## Cleaning data.##
###################
# Change to appropriate format
Stroke_data$bmi <- as.numeric(Stroke_data$bmi)
Stroke_data <- replace_with_na(data = Stroke_data, replace = list(gender=c("Other")))

# Handling missing data
sum(is.na(Stroke_data))

#remove all missing value.
data_clean<-na.omit(Stroke_data)

#remove id attribute.
data_clean<- subset(data_clean, select = -id )
summary(data_clean)

###################
## Visualization ##
###################


#### Looking at the attributes of data 

attach(data_clean)
# check unique values of categorical values by using "unique()"
unique(gender)
unique(ever_married)
unique(work_type)
unique(Residence_type)
unique(smoking_status)

##  For classification variables.
table(gender)
df <- data.frame(group = c("Male", "Female"),value = c(2011,2897))
labs <- paste0(df$group, " (", df$value,")")
ggpie(df, "value", label = labs,fill = "group", color = "white",palette = c("#00AFBB", "#E7B800"))

ggplot(data_clean, 
       aes(x = Residence_type, fill = Residence_type)) + 
  geom_bar() +
  labs(title = "Effect of Residence type on getting stroke?") +
  theme(legend.position = "none") 

ggplot(data_clean, 
       aes(x = ever_married, fill = ever_married)) + 
  geom_bar() +
  labs(title = "Effect mariage on getting stroke?") +
  theme(legend.position = "none") 

ggplot(data_clean, 
       aes(x = work_type, fill = work_type)) + 
  geom_bar() +
  labs(title = "Work Type distribution") +
  theme(legend.position = "none") 

addmargins(table( stroke,heart_disease))

slices<-c(4496,169)
pct <- round(slices/sum(slices)*100)
df <- data.frame(stroke = c("No", "Yes"),value = pct)
labs <- paste0( " (", df$value,"%",")")
ggpie(df, "value", label = labs,title="Do not have heart disease with stroke",fill = "stroke", color = "white")

slices<-c(203,40)
pct <- round(slices/sum(slices)*100)
df <- data.frame(stroke = c("No", "Yes"),value = pct)
labs <- paste0( " (", df$value,"%",")")
ggpie(df, "value", label = labs,title="having heart disease with stroke",fill = "stroke", color = "white")

addmargins(table(hypertension, stroke))

slices<-c(149,60)
pct <- round(slices/sum(slices)*100)
df <- data.frame(group = c("No", "Yes"),value = pct)
labs <- paste0( " (", df$value,"%",")")
ggpie(df, "value", label = labs,title=" Hypertension with experience of stroke",fill = "group", color = "white",palette = c("#00AFBB", "#E7B800"))

slices<-c(4308,391)
pct <- round(slices/sum(slices)*100)
df <- data.frame(group = c("No", "Yes"),value = pct)
labs <- paste0( " (", df$value,"%",")")
ggpie(df, "value", label = labs,title="Hypertension with no stroke",fill = "group", color = "white",palette = c("#00AFBB", "#E7B800"))

table(stroke)
slices <- c(4699,209)
pct <- round(slices/sum(slices)*100)
df <- data.frame(group = c("Yes", "No"),value = pct)
labs <- paste0(df$group, " (", df$value,"%",")")
ggpie(df, "value", label = labs,fill = "group", color = "white",palette = c("#00AFBB", "#E7B800")) 

## For numberical values
ggdensity(data_clean, x = "bmi",
          add = "mean", rug = TRUE,
          color = "gender", fill = "gender",
          main="Distribution of BMI by gender")
ggdensity(data_clean, x = "age",
          add = "mean", rug = TRUE,
          color = "gender", fill = "gender",
          main="Distribution of Age by gender")
ggdensity(data_clean, x = "avg_glucose_level",
          add = "mean", rug = TRUE,
          fill = "gender",
          main="Distribution of Average glucose level by gender")

#Correlation for numerical variable.
          
df <-subset(data_clean, select = age,bmi,avg_glucose_level )
r <- cor(df, use="complete.obs")
round(r,2)
ggcorrplot(r, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE)
#############
## MODELING##
#############

data_clean$heart_disease <- factor(data_clean$heart_disease, levels = c(0,1), labels =c('No','Yes'))
data_clean$hypertension <- factor(data_clean$hypertension,levels = c(0,1), labels = c('No','Yes'))

## Slip data into traing set and test set (70-30 split )
set.seed(1)
subset=sample(c(TRUE ,FALSE), nrow(data_clean ), replace=TRUE, prob=c(0.7, 0.3))
train= data_clean[subset,]
test=data_clean[!subset,]

#create matrix for training set and test set
train.X<-model.matrix(stroke~.,data=train)[,-1]
train.Y<-train$stroke
test.X<-model.matrix(stroke~.,data=test)[,-1]
test.Y<-test$stroke

## LOGISTIC REGRESSION .
set.seed(2)
fit.all=glm(stroke~.,data=train,family=binomial)
summary(fit.all)
probs_predict=predict(fit.all,newdata=test, type="response")
roc.glm = roc(test.Y ~ probs_predict, plot = TRUE, print.auc = TRUE)
c.glm=coords(roc.glm, "best", ret = "threshold")
pred.glm <- ifelse(probs_predict > c.glm$threshold,1,0)


##  Accurate rate, Sensitivity, Specificity table.
tab.glm=table(pred.glm, test.Y)
train_con_mat = confusionMatrix(tab.glm, positive = "1")
c(train_con_mat$overall["Accuracy"], 
  train_con_mat$byClass["Sensitivity"], 
  train_con_mat$byClass["Specificity"])

# Accurate rate table with a threshold =0.5
pred.glm2 <- ifelse(probs_predict > 0.5,1,0)
tab.glm2=table(pred.glm2, test.Y)
accurat.glm2 = confusionMatrix(tab.glm2, positive = "1")
c(accurat.glm2$overall["Accuracy"], 
  accurat.glm2$byClass["Sensitivity"], 
  accurat.glm2$byClass["Specificity"])

## LINEAR DISCRIMINANT ANALYSIS (LDA) 
set.seed(2)
model.lda=lda(stroke~.,data = train)
pred.lda<-predict(model.lda, newdata=test)

plot(model.lda)
# with a threshold =0.5
tab.lda=table(pred.lda$class,test.Y)
Accurate.lda= confusionMatrix(tab.lda, positive = "1")
c(Accurate.lda$overall["Accuracy"], 
  Accurate.lda$byClass["Sensitivity"], 
  Accurate.lda$byClass["Specificity"])


# ROC curve and am optimal threshold for LDA.
roc.lda= roc(test.Y ~ pred.lda$posterior[, 2], plot = TRUE, print.auc = TRUE,main = "LDA ROC curve")
c.lda=coords(roc.lda, "best", ret = "threshold")
pred.lda.adj <- ifelse(pred.lda$posterior[, 2] >c.lda$threshold, 1, 0)
pred.lda2=pred.lda.adj


#Accuracy, Sensitivity, Specificity table.
tab.lda2=table(pred.lda2,test.Y)
Accurate.lda2= confusionMatrix(tab.lda2, positive = "1")
c(Accurate.lda2$overall["Accuracy"], 
  Accurate.lda2$byClass["Sensitivity"], 
  Accurate.lda2$byClass["Specificity"])


## LASSO METHOD VS RIDGE REGRESSION.
set.seed(23)
# Find the best lambda using cross-validation
set.seed(123) 
cv.lasso <- cv.glmnet(train.X, train.Y, alpha = 1, family = "binomial")
cv.ridge <- cv.glmnet(train.X, train.Y, alpha = 0, family = "binomial")

##Tuning parameter selection plot
par(mfrow=c(2,2))
plot(cv.lasso, main = "Lasso penalty\n")
plot(cv.ridge, main = "Ridge penalty\n")

# Fit the final model on the training data
model.lasso <- glmnet(train.X, train.Y, alpha = 1,  family = "binomial",
                      lambda = cv.lasso$lambda.min)
model.ridge<- glmnet(train.X, train.Y, alpha = 0,  family = "binomial",
                     lambda = cv.ridge$lambda.min)

# Make prediction in test data.
prob.lasso <-model.lasso %>% predict(newx = test.X, type = "response" )
prob.ridge<- model.ridge %>% predict(newx = test.X, type = "response" )
set.seed(24)
#Find the best threshold for both models
par(mfrow=c(2,2))
roc.lasso= roc(test$stroke ~ prob.lasso, plot = TRUE, print.auc = TRUE,main = "Lasso ROC curve")
roc.ridge=roc(test$stroke ~ prob.ridge, plot = TRUE, print.auc = TRUE,main = "Ridge Regression ROC curve")

# Comparison 2 thresholds.
c.lasso=coords(roc.lasso, "best", ret = "threshold")
c.ridge=coords(roc.ridge, "best", ret = "threshold")

# Make predictions as class.
pred.lasso<-ifelse(prob.lasso > c.lasso$threshold,1,0)
pred.ridge<-ifelse(prob.ridge > c.ridge$threshold,1,0)

##  Accurate rate, Sensitivity, Specificity table.
tab.lasso=table(pred.lasso, test$stroke)
tab.ridge=table(pred.ridge, test$stroke)

## For lasso method
Accurate.lasso= confusionMatrix(tab.lasso, positive = "1")
c(Accurate.lasso$overall["Accuracy"], 
  Accurate.lasso$byClass["Sensitivity"], 
  Accurate.lasso$byClass["Specificity"])

## For ridge regression method
Accurate.ridge= confusionMatrix(tab.ridge, positive = "1")
c(Accurate.ridge$overall["Accuracy"], 
  Accurate.ridge$byClass["Sensitivity"], 
  Accurate.ridge$byClass["Specificity"])


## KNN
#Using CV to find best K to get the maximum accuracy
k.vec=1:15
test.classer=rep(0,length(k.vec))
for (i in 1:length(k.vec))
{
  k=k.vec[i]
  knn.pred=knn(train.X,test.X,train.Y,k=k)
  test.classer[i]=mean(test.Y==knn.pred)
}
plot(1/k.vec,test.classer,type="o",main="Test Classification Accuracy") 
best.k=which.max(test.classer)  

#model with the best k.
knn.pred=knn(train.X,test.X,train.Y,k=best.k)
mean(test.Y==knn.pred) # accurate rate

#model with k=10
knn.pred1=knn(train.X,test.X,train.Y,k=10)
mean(test.Y==knn.pred1) # accurate rate

#model with k=15
knn.pred2=knn(train.X,test.X,train.Y,k=10)
mean(test.Y==knn.pred2) # accurate rate


#Accuracy, Sensitivity, Specificity table.
tab.knn=table(knn.pred,test.Y)
Accurate.knn= confusionMatrix(tab.knn, positive = "1")
c(Accurate.knn$overall["Accuracy"], 
  Accurate.knn$byClass["Sensitivity"], 
  Accurate.knn$byClass["Specificity"])

# Cost of Misclassification

acc <- c()
sen <- c()
spc <- c()
k.vec=1:50
for (i in 1:50) {
  k=k.vec[i]
  knn.pred=knn(train.X,test.X,train.Y,k=k)
  acc <- c(acc,length(which(test.Y==knn.pred)==TRUE)/length(test.Y))
  sen <- c(sen,length(which((test.Y==knn.pred) & (test.Y==1))) / length(which(test.Y==1)))
  spc <- c(spc,length(which((test.Y==knn.pred) & (test.Y==0))) / length(which(test.Y==0)))
}
costdf <- data.frame(k=1:50,Accuracy=acc,Sensitivity=sen,Specificity=spc)
cost=3*(1-costdf$Sensitivity)+(1-costdf$Specificity)
costdf <- cbind(costdf,"Cost"=cost)

kable(costdf[c(1:15,seq(20,50,by=5)),],row.names=FALSE)
plot(k.vec,sen,type="o",main="Sensitivity of Classification") 
plot(k.vec,spc,type="o",main="Specificity of Classification")
plot(k.vec,spc,type="o",main="Accuracy of Classification")
