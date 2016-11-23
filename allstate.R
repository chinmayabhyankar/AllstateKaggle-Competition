#import data
install.packages("data.table")
setwd("C:/Users/Chinmay/Desktop/R/R/R-projects/Allstate")

library(data.table)
train =  fread("train.csv", stringsAsFactors = T)
train<-as.data.frame(train)


test =  fread("test.csv", stringsAsFactors = T)
test<-as.data.frame(test)
#rflm = fread("submission7.csv", stringsAsFactors = T)
#write.csv(rflm, file = "submission8.csv", row.names = T)
#analyze data
summary(train)
a = colnames(train)
str(train$cont10)
names(train) %in% names(test)
#1,89,92,96,99,103,106,109,110,111,113,116
#random forest model

train = train[,-c(1,90,93,97,100,104,107,110,111,112,113,114,117)]
i1 <- sapply(train,function(train)is.factor(train)&&length(levels(train))>31)
#cat112 has level >31
trainfactor31 = train[,colnames(train[,i1])
train = train   
#linear model
set.seed(111)
trainmodel = lm(log(loss)~.,data = train)
predictlm = predict(trainmodel,test)
submission.df = data.frame(id = test$id, loss = exp(predictlm))

#randomforest model
install.packages("randomForest")
library(randomForest)
trainrf = randomForest(log(loss)~.,data = train)
write.csv(submission.df, file = "submission3.csv", row.names = T)

#naivebayes
trainnb = naiveBayes(log(loss)~.,data = train)
predictnb = predict(trainnb,test)
submission.df = data.frame(id = test$id, loss = exp(predictnb))
write.csv(submission.df, file = "submission3.csv", row.names = T)

#decision tree
library(tree)
#set.seed(123)
traintree = tree(log(loss)~.,data = train)
predicttree = predict(traintree,test)
submission.df = data.frame(id = test$id, loss = exp(predicttree))
write.csv(submission.df, file = "submission4.csv", row.names = T)


# exploratory analysis
require(GGally)
require(ggplot2)
require(ggpairs)

ggpairs(train, columns= c('cont1','cont2','cont3','cont4','loss'),
        diag=list(continuous="density",   discrete="bar"), axisLabels="show")

ggplot(aes(x = train$) + geom_histogram()

# H2O
install.packages("h2o")
library(h2o)
train$loss = log(train$loss +1)

localH2O <- h2o.init(nthreads = -1)

train.h2o = as.h2o(train)
test.h2o = as.h2o(test)
y.dep = 132
x.indep <- c(2:89,91,92,94:96,101:103,105,106,108,109,115,116,118:131)

rforest.model <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = 1000, mtries = 3, max_depth = 4, seed = 1122)

#check performance
h2o.performance(rforest.model)

predict.rforest <- as.data.frame(h2o.predict(rforest.model, test.h2o))

submission.df = data.frame(id = test$id, loss = exp(predict.rforest$predict)-1 )
write.csv(submission.df, file = "submission9.csv", row.names = T)
#write.csv(loss, file = "loss.csv", row.names = T)

#ensemble learning
loss1 = predict.rforest$predict
#combination of lm nd rf

loss = exp(predictlm)
ensemble = mean(c(loss,loss1),trim = 0)
ensemble = apply(cbind(loss,loss1),FUN = mean,na.rm = TRUE)
#length(loss)

#deep learning using h2o
dlearning.model <- h2o.deeplearning(y = y.dep,
                                    x = x.indep,
                                    training_frame = train.h2o,
                                    epoch = 60,
                                    hidden = c(100,100),
                                    activation = "Rectifier",
                                    seed = 1122)

predict.dlearning <- as.data.frame(h2o.predict(dlearning.model, test.h2o))
submission.df = data.frame(id = test$id, loss = exp(predict.dlearning$predict)-1 )

write.csv(submission.df, file = "submission11.csv", row.names = T)

