library(dummies)
library(randomForest)
library(e1071)
library(gbm)
library(caret)

##==============================

got=read.csv("gotupdated_1.csv")

got=got[,c(1:17,20,21,24)]
got_name=got[,1]
got_variables=got[,-1]
got_new=dummy.data.frame(got_variables,sep="_")

##==============================

set.seed(8)
smp_size=floor(.6*nrow(got))
train_ind=sample(seq_len(nrow(got)),size=smp_size)
got_train=got_new[train_ind,]
got_test=got_new[-train_ind,]

got_train_name=got_name[train_ind]
got_test_name=got_name[-train_ind]

##==============================

rf=randomForest(x=got_train[,1:(ncol(got_train)-1)],y=as.factor(got_train[,ncol(got_train)]),ntree=1000,probabilities=T)
svm_model=svm(x=got_train[,1:ncol(got_train)-1)],y=as.factor(got_train[,ncol(got_train)]))


pred_svm_train=predict(svm_model,newdata = got_train[, 1:ncol(got_train)-1)])
pred_rf_train=predict(rf,newdata = got_train[,1:ncol(got_train)-1)])


predDF_train=data.frame(pred_svm_train,pred_rf_train,death=got_train$Death)
combModFit.gbm <- train(as.factor(death) ~ ., method = "gbm",data=predDF_train)

combPred.gbm <- predict(combModFit.gbm, predDF_train)


accuracy_train <- rbind(confusionMatrix(combPred.gbm, got_train$Death)$overall[1], 
                  confusionMatrix(pred_svm_train, got_train$Death)$overall[1], 
                  confusionMatrix(pred_rf_train, got_train$Death)$overall[1])
row.names(accuracy_train) <- c("Stack", "SVM", "RF")
accuracy_train

##==============================

pred_svm_test=predict(svm_model,newdata = got_test[, 1:ncol(got_train)-1)])
pred_rf_test=predict(rf,newdata = got_test[,1:ncol(got_train)-1)])


predDF_test=data.frame(pred_svm_test,pred_rf_test,death=got_test$Death)
names(predDF_test)=c("pred_svm_train","pred_rf_train","death")
combPred.gbm_test <- predict(combModFit.gbm, predDF_test)


accuracy_test <- rbind(confusionMatrix(combPred.gbm_test, got_test$Death)$overall[1], 
                  confusionMatrix(pred_svm_test, got_test$Death)$overall[1], 
                  confusionMatrix(pred_rf_test, got_test$Death)$overall[1])
row.names(accuracy_test) <- c("Stack", "SVM", "RF")
accuracy_test

##==============================

##without relevant shit
m=cor(got_new)
m[upper.tri(m)] <- 0
diag(m) <- 0

got_train_tester=got_train[,!apply(m,2,function(x) any(x > 0.99))]
rf_tester=randomForest(x=got_train_tester[,1:(ncol(got_train_tester)-1)],y=as.factor(got_train_tester[,ncol(got_train_tester)]),ntree=1000,probabilities=T)
svm_model_tester=svm(x=got_train_tester[,1:(ncol(got_train_tester)-1)],y=as.factor(got_train_tester[,ncol(got_train_tester)]))
pred_svm_train_tester=predict(svm_model_tester,newdata = got_train_tester[, 1:(ncol(got_train_tester)-1)])
pred_rf_train_tester=predict(rf_tester,newdata = got_train_tester[,1:(ncol(got_train_tester)-1)])

predDF_train_tester=data.frame(pred_svm_train_tester,pred_rf_train_tester,death=got_train_tester$Death)
combModFit.gbm_tester <- train(as.factor(death) ~ ., method = "gbm",data=predDF_train_tester)

combPred.gbm_tester <- predict(combModFit.gbm_tester, predDF_train_tester)


accuracy_train_tester <- rbind(confusionMatrix(combPred.gbm_tester, got_train$Death)$overall[1], 
                  confusionMatrix(pred_svm_train_tester, got_train$Death)$overall[1], 
                  confusionMatrix(pred_rf_train_tester, got_train$Death)$overall[1])
row.names(accuracy_train_tester) <- c("Stack", "SVM", "RF")
accuracy_train_tester

##==============================

got_test_tester=got_test[,!apply(m,2,function(x) any(x > 0.99))]
pred_svm_test_tester=predict(svm_model_tester,newdata = got_test_tester[, 1:(ncol(got_train_tester)-1)])
pred_rf_test_tester=predict(rf_tester,newdata = got_test_tester[,1:(ncol(got_train_tester)-1)])
predDF_test_tester=data.frame(pred_svm_train_tester=pred_svm_test_tester,pred_rf_train_tester=pred_rf_test_tester,death=got_test_tester$Death)
combPred.gbm_tester_test <- predict(combModFit.gbm_tester, predDF_test_tester)
accuracy_test_tester <- rbind(confusionMatrix(combPred.gbm_tester_test, got_test_tester$Death)$overall[1], 
                  confusionMatrix(pred_svm_test_tester, got_test_tester$Death)$overall[1], 
                  confusionMatrix(pred_rf_test_tester, got_test_tester$Death)$overall[1])
row.names(accuracy_test_tester) <- c("Stack", "SVM", "RF")
accuracy_test_tester

##==============================

notdead=got_new[got_new$Death==0,]

notdead_names=got_name[got_new$Death==0]
pred_rf_notdead=predict(rf,newdata = notdead[,1:ncol(got_train)-1)],type="prob")
not_dead_predictions=cbind(as.character(notdead_names),pred_rf_notdead)
not_dead_predictions=as.data.frame(not_dead_predictions)

got_to_merge=got[,c(1,2,3,5)]

names(not_dead_predictions)=c("name","alive_prob","dead_prob")
got_to_merge$name=as.character(got_to_merge$name)

not_dead_predictions=merge(not_dead_predictions,got_to_merge,by="name")

image_urls=read.csv("got_images_characters.csv")

not_dead_predictions=merge(not_dead_predictions,image_urls,by="name")

##write.csv(file="got_predictions_EP2_fixed.csv",row.names=F,not_dead_predictions)

##create a shiny app with probabilities by house 