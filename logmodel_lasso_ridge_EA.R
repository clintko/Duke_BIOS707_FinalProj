###This code is for BIOS 707 Final Project
###Group: Angel, Clint, Emily
###The code inthis Script File was created by Emily 
### with the purpose of fitting a logistic regression

setwd('C:\\Users\\eja16\\OneDrive\\Desktop\\BIOSTATS\\BIOS707\\Final Project')
data<- read.csv('.\\proj_dat.csv')
##head(data)
data<-data[,1:32]

install.packages('tidyverse')
library(tidyverse)
dat_covar = data %>% select(-id, -diagnosis)
dat_label = cbind(data$diagnosis) %>% as.data.frame

#set seed for reproducibility 
set.seed(707)
#split into training and test sets
data[,'train'] <- ifelse(runif(nrow(data))<0.8,1,0)
#separate training and test sets
trainset <- data[data$train==1,]
testset <- data[data$train==0,]

trainColNum <- grep('train',names(trainset))
#remove train flag column from train and test sets
trainset <- trainset[,-trainColNum]
testset <- testset[,-trainColNum]
#get column index of predicted variable in dataset
typeColNum <- grep('diagnosis',names(data))

#build model on training set
colnames(trainset)
log_model <- glm(diagnosis ~  . ,data = trainset, family = binomial,control = list(maxit = 50))

#Create confusion matrix
prob <- predict.glm(log_model,testset[,-typeColNum],type='response')

##contrasts(data$diagnosis) #0=B 1=M

##.first create vector to hold predictions (we know 0 refers to neg now)
predict <- rep('B',nrow(testset))
predict[prob>.5] <- 'M'
#confusion matrix
table(pred=predict,true=testset$diagnosis)
#accuracy
log_accuracy <- mean(predict==testset$diagnosis)
log_error<- 6/109



library(glmnet)
#convert training data to matrix format
x <- model.matrix(diagnosis~.,trainset)
#convert class to numerical variable
y <- ifelse(trainset$diagnosis=='M',1,0)
#perform grid search to find optimal value of lambda
#family= binomial => logistic regression, alpha=1 => lasso
# check docs to explore other type.measure options
cv.out <- cv.glmnet(x,y,alpha=1,family='binomial',type.measure = 'mse' )
#plot result
plot(cv.out, main= 'LASSO Model')
#Select lambda
#min value of lambda
lambda <- cv.out$lambda.min
#best value of lambda
lambda_1se <- cv.out$lambda.1se

coef(cv.out,s=lambda_1se) #only 7 covariates not set to 0 by lasso

#Run the lasso reduced model on the test data
#get test data
x_test <- model.matrix(diagnosis~.,testset)
#predict class, type="class"
lasso_prob <- predict(cv.out,newx = x_test,s=lambda_1se,type='response')
#translate probabilities to predictions
lasso_predict <- rep('B',nrow(testset))
lasso_predict[lasso_prob>.5] <- 'M'
#confusion matrix
table(pred=lasso_predict,true=testset$diagnosis)
#accuracy
lasso_accuracy <- mean(lasso_predict==testset$diagnosis) #lasso accuracy higher thanthe full logistic model 
error.lasso<- 4/109



###logistic regression with ridge penalty
y_tr<-as.factor(trainset$diagnosis)
x_tr<-as.matrix(trainset[,-2])
#crossvalidation 
cv.out.r<-cv.glmnet(x_tr,y_tr,type.measure = 'class', nfolds=10, family='binomial')
plot(cv.out.r, main='Ridge Fi (Training)')
lambda.r<-cv.out.r$lambda.min
#fit the model with alpha=0 for the ridge penalty
ridge.log_model <- glmnet (x_tr, y_tr, family = 'binomial', alpha=0)
ridge_predict <- predict(ridge.log_model, x_tr, type='class', s=lambda.r)
#create training set confusion matrix
table(y_tr,ridge_predict)
###calculate the error 
error.ridge_tr <- (1+10)/460

#perform on the test data
y_test<-as.factor(testset$diagnosis)
x_test<-as.matrix(testset[,-2])
#crossvalidation 
cv.out.r2<-cv.glmnet(x_test,y_test,type.measure = 'class', nfolds=10, family='binomial')
plot(cv.out.r2, main='Ridge Fit (Test)')
lambda.r2<-cv.out.r2$lambda.min
#fit the model with alpha=0 for the ridge penalty
ridge.log_model2 <- glmnet (x_test, y_test, family = 'binomial', alpha=0)
ridge_predict2 <- predict(ridge.log_model2, x_test, type='class', s=lambda.r2)
#confusion matrix
table(y_test, ridge_predict2)
#calculate the error
error.ridge <- 2/109

#Table of the error rates of the model fits 
#you cna see that ridge has the smallest error, so we would expect it to be more accurate model
errors<- cbind(log_error , error.lasso , error.ridge )
rownames(errors) <- 'Error'
colnames(errors)<- c('Logistic', 'LASSO' , 'Ridge')
errors