library(dummies)
library(randomForest)

got=read.csv("gotupdated.csv")

got=got[,c(1:17,20,21,24)]
got_name=got[,1]
got_variables=got[,-1]
got_new=dummy.data.frame(got_variables,sep="_")

set.seed(8)
smp_size=floor(.6*nrow(got))
train_ind=sample(seq_len(nrow(got)),size=smp_size)
got_train=got_new[train_ind,]
got_test=got_new[-train_ind,]

got_train_name=got_name[train_ind]
got_test_name=got_name[-train_ind]

rf=randomForest(x=got_train[,1:123],y=as.factor(got_train[,124]),ntree=1000,probabilities=T)

pred_rf_train=predict(rf,newdata = got_train[,1:123])
confusionMatrix(pred_rf_train, got_train$Death)$overall[1]

pred_rf_test=predict(rf,newdata = got_test[,1:123])
confusionMatrix(pred_rf_test, got_test$Death)$overall[1]


notdead=got_new[got_new$Death==0,]
notdead_names=got_name[got_new$Death==0]

pred_rf_notdead=predict(rf,newdata = notdead[,1:123],type="prob")

not_dead_predictions=as.data.frame(cbind(as.character(notdead_names),pred_rf_notdead))
got_to_merge=got[,c(1,2,3,5)]
names(not_dead_predictions)=c("name","alive_prob","dead_prob")

got_to_merge$name=as.character(got_to_merge$name)
not_dead_predictions=merge(not_dead_predictions,got_to_merge,by="name")

image_urls=read.csv("got_images_characters.csv")

not_dead_predictions=merge(not_dead_predictions,image_urls,by="name")


not_dead_predictions$image=paste0('<img src="got_images/',not_dead_predictions$image,'" height="60"></img>')

