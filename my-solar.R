training.gbm <- function(data){
  gbmCV <- expand.grid(.n.tree=[50,100,150],
                       .n.interactions=[1:4],.shrinkage=[0.5,1])
  trained.gbm <- train(labels ~., method="gbm",
                       distribution="Gaussian",tuneGrid=gbmCV, 
                       data=data,
                       trControl=trainControl(method='cv',
                                              number=5,
                                              returnResamp="all"))
  ntrees <- gbm$perf(trained.gbm$finalModel, method="OOB")
  return trained.gbm
}