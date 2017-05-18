
# ---
#   title: "Project-DM2017"
# author: "Salman Lashkarara,Hina,Behzad"
# date: "May 5, 2017"
# output: Library for calling svm, decision tree, and logestic regresion
# ---


library(randomForest)
library(e1071)
library(rpart)
library(ggplot2)
library(ROCR)
library(caret)

AUC <-function(prediction,target){
  res<-prediction(prediction, target, label.ordering = NULL)
  auc.tmp <- performance(res,"auc"); 
  auc <- as.numeric(auc.tmp@y.values)
  auc
}

combiner<-function(train,test){
  reg<-logestic.regresion(train,test)
  svm<-sVM(train,test)
  tree<-decision.tree(train,test)
  (reg+svm+tree)/3
}

logestic.regresion<-function(train,test){
  model<-lm(target~. ,train)
  predict(model,test)
}

decision.tree<-function(train,test){
  fit <- rpart(target~. , method="class", data=train)
  tree<-predict(fit, test)
  tree[,2]
}

sVM <- function(train,test){
  model <- svm(target ~ . , train)
  predict(model, test)
}

radial.svm <- function(train,test){
  model <- svm(target ~ . , train, kernel="radial", cost=1, gamma=0.5)
  predict(model, test)
}

rocCurve <-function(prediction,real){  
  res<-prediction(prediction, real, label.ordering = NULL)
  roc.perf = performance(res, measure = "tpr", x.measure = "fpr")
  data.frame(y=as.vector(unlist(roc.perf@y.values)),x=as.vector(unlist(roc.perf@x.values)))
}

