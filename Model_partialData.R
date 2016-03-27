load(file = "/Users/zhiyuan/Dropbox/Documents/DMC/Code/DW.RData")

miss.class = function(pred.class, true.class, produceOutput = F) {
  confusion.mat = table(pred.class, true.class)
  if(produceOutput) {
    return(list(confusionMtx = confusion.mat,
                miss_class = 1 - sum(diag(confusion.mat))/sum(confusion.mat)))
  }
  else{
    print('miss-class')
    print(1 - sum(diag(confusion.mat))/sum(confusion.mat))
    print('confusion mat')
    print(confusion.mat)
  }
}

train = dt.sim2$train  
test = dt.sim2$test

colnames(train)
formatDt <- function(dt){
  dt$gapTimeInterval = cut(dt$couponsReceivedGapTime,breaks = c(0,2000,3000,4000,6000,10000,18000,30000,Inf))
  return(dt)
}

train = formatDt(train)
test = formatDt(test)

colIndex = c("couponsReceivedGapTime","price","premiumProduct","basePrice","reward","couponsReceivedDay","orderDay")


X_0 = test[,colIndex]
Y_0 = test$couponUsed
X = train[,colIndex]
Y = train$couponUsed
colnames(X)
which(is.na(X_0))


### Tree
library(tree)
library(gbm)
require(randomForest)
require(foreach)
library(doMC)
require(plyr)

registerDoMC(cores=4)


X_0 = test[,colIndex]
Y_0 = test$couponUsed
models = list()
for(i in seq(1,(max(train$orderDay)-3),by = 3)){
  rowIndex = which(train$orderDay >= i)
  X = train[rowIndex,colIndex]
  Y = train$couponUsed[rowIndex]
  out.tree = foreach(ntree=rep(75, 4), .combine=combine, .packages='randomForest') %dopar% 
    randomForest(X, as.factor(Y), importance = T, ntree = 300)
  models[[i]] = out.tree 
  print(i)
}
models = models[seq(1,(max(train$orderDay)-3),by = 3)]
summary(models)


models.missRate = lapply(models,function(m){
                    tmp = miss.class(predict(m,X_0,n.trees = 200),Y_0,T)
                    tmp$miss_class
                  })

plot(unlist(models.missRate),type = "b")


seq(1,(max(train$orderDay)-3))
rowIndex = which(train$orderDay >= 52)
X = train[rowIndex,colIndex]
Y = train$couponUsed[rowIndex]
out.tree = foreach(ntree=rep(75, 4), .combine=combine, .packages='randomForest') %dopar% 
  randomForest(X, as.factor(Y), importance = T, ntree = 300)
models[[i]] = out.tree 
print(i)
miss.class(predict(out.tree,X_0,n.trees = 200),Y_0,T)
