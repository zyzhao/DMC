
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

evalFunc <- function(pred,true,group = NULL){
  if(is.vector(pred)){
    pr = cbind(pred[which(group == 1)],
               pred[which(group == 2)],
               pred[which(group == 3)])
    tr = cbind(true[which(group == 1)],
               true[which(group == 2)],
               true[which(group == 3)])
  }else{
    pr = pred
    tr = true
  }
  n = nrow(pr)
  E = sum(((pr[,1] - tr[,1]) / mean(tr[,1]))^2 + ((pr[,2] - tr[,2]) / mean(tr[,2]))^2 + ((pr[,3] - tr[,3]) / mean(tr[,3]))^2)
  return(E)
}

train = dt.sim2$train  
test = dt.sim2$test

length(unique(train$userID))
length(unique(c(train$userID,test$userID)))
test$userID[-which(test$userID %in% c(train$userID))]
test[test$userID == "31385fe70de329d70f3c32e39cf93c62",]

### Define Feature Mtx

train = dt.sim2$train
test = dt.sim2$test
colnames(train)
formatDt <- function(dt){
  dt$gapTimeInterval = cut(dt$couponsReceivedGapTime,breaks = c(0,2000,3000,4000,6000,10000,18000,30000,Inf))
  return(dt)
}

train = formatDt(train)
test = formatDt(test)


#test$gapTimeInterval = cut(test$couponsReceivedGapTime,breaks = c(0,2000,3000,4000,6000,10000,18000,30000,Inf))


colIndex = c(7,8,9,10,15,31:61)
colIndex = c("couponsReceivedGapTime","price","premiumProduct","basePrice","reward","couponsReceivedDay","orderDay")


X_0 = test[,colIndex]
Y_0 = test$couponUsed
X = train[,colIndex]
Y = train$couponUsed
colnames(X)
colnames(X_0)
which(is.na(X_0))

X$CouponsReceivedAmount = 1 / X$CouponsReceivedAmount
X_0$CouponsReceivedAmount = 1 / X_0$CouponsReceivedAmount


# X$couponsReceivedWeekday = factor(X$couponsReceivedWeekday )
X$orderWeekday = factor(X$orderWeekday )
# X$couponsReceivedDay = factor(X$couponsReceivedDay)
# X_0$couponsReceivedWeekday = factor(X_0$couponsReceivedWeekday )
X_0$orderWeekday = factor(X_0$orderWeekday )
# X_0$couponsReceivedDay = factor(X_0$couponsReceivedDay)

X_0[is.na(X_0)] = 0

### Tree
library(tree)
library(gbm)
require(randomForest)
require(foreach)
library(doMC)
require(plyr)

registerDoMC(cores=4)

# out.tree = tree(Y~.,data=X)
# out.tree = randomForest(X, as.factor(Y), importance = T, ntree = 1000)

# out.tree = foreach(ntree=rep(50, 4), .combine=combine, .packages='randomForest') %dopar% 
#   randomForest(X, as.factor(Y), importance = T, ntree = 300,mtry = ncol(X))
out.tree = foreach(ntree=rep(50, 4), .combine=combine, .packages='randomForest') %dopar% 
  randomForest(X, as.factor(Y), importance = T, ntree = 300)

# plot(out.tree)
# text(out.tree)
# summary(out.tree)
out.tree$importance

miss.class(predict(out.tree,X,n.trees = 200),Y)
miss.class(predict(out.tree,X_0,n.trees = 200),Y_0)
Y_0.prob =predict(out.tree,X,n.trees = 200,type = "prob") 
Y_0.prob = Y_0.prob[,2]

N = length(Y_0)
evalFunc(Y_0.prob,Y_0,test$couponSlot)
evalFunc(rep(0,N),Y_0,test$couponSlot)
evalFunc(rep(1,N),Y_0,test$couponSlot)






Y_0.hat = as.numeric(predict(out.tree,X_0)) -1
Y_0.err = as.numeric(predict(out.tree,X_0))-1 - Y_0
table(Y_0.err,test$predStep)
Y_0.err.byLastOrderInTrain = table(Y_0.err,test$lastOrderInTrain)
plot(round(Y_0.err.byLastOrderInTrain[1,] / apply(Y_0.err.byLastOrderInTrain,2,sum),2)
     ,type = "l")

colnames(test)
plot(test$orderIndexOfUser,jitter(test$lastOrderInTrain,1),col = Y_0.err+2)
t = table(test$orderIndexOfUser,Y_0.err)
ts = apply(t,1,sum)
plot(round(t[,1] / ts,2),type = "l")

dt = rbind(train,test)
dt = dt[order(dt$orderTime),]
boxplot(dt$basketValue ~ dt$class,type = "l",outline = F)
boxplot(dt$CouponsReceivedAmount ~ dt$class,type = "l",outline = F)
boxplot(dt$couponsReceivedGapTime ~ dt$class,type = "l",outline = F)


t = table(test$couponID,Y_0.err)

t[order(t[,1]),]
aaa = dt[which(dt$couponID == "c064a7f10b0ab4f795fd416fe7818d6d"),]
fix(aaa)
aaa = dt[which(dt$couponID == "fd0df77e24042cf619442fcf685a98a7"),]
fix(aaa)
dt[which(dt$userID == "8a9cb585394ea221ac2d6557341be70d"),]



ind = which(Y_0 == 1)
test[ind,]


### Tree Boosting
B = 1000;M = 4;l= .001
i = 0;outTrees = list();out.summary = c()
for(B in c(100,200,500)){
  for(M in 1:6){
    for(l in c(1,.1,.01,.001)){
      out.tree.boost = gbm(Y ~ .,data = X,
                           n.trees = B,
                           shrinkage = l,
                           interaction.depth = M,
                           distribution = "adaboost",
                           n.cores = 4)
      i =i+1
      print(i)
      outTrees[[i]] = out.tree.boost 
      Y.hat =round(predict(out.tree.boost,  X, n.trees = B, type = "response"))
      Y_0.hat =round(predict(out.tree.boost,  X_0, n.trees = B, type = "response"))
      train.err = miss.class(Y.hat,Y,T)$miss_class
      test.err = miss.class(Y_0.hat,Y_0,T)$miss_class
      out.summary = rbind(out.summary,
                          c(B,M,l,train.err,test.err ))
    }
  }
}

Y.hat =round(predict(out.tree.boost,  X, n.trees = B, type = "response"))
Y_0.hat =round(predict(out.tree.boost,  X_0, n.trees = B, type = "response"))

miss.class(Y.hat,Y)
miss.class(Y_0.hat,Y_0)


Y_0.err = as.numeric(Y_0.hat - Y_0)
table(Y_0.err,test$predStep)
Y_0.err.byLastOrderInTrain = table(Y_0.err,test$lastOrderInTrain)
plot(round(Y_0.err.byLastOrderInTrain[1,] / apply(Y_0.err.byLastOrderInTrain,2,sum),2)
     ,type = "l")

### SVM Model
library(e1071)
dat = data.frame(X=X, Y=as.factor(Y))
fit=svm(Y~., data=dat, kernel="linear", cost=.01,probability = T)

Y.hat = predict(fit) 
Y_0.hat = predict(fit,data.frame(X = X_0)) 
Y_0.prob = predict(fit,data.frame(X = X_0),probability = T) 
Y_0.prob =attributes(Y_0.prob)$probabilities[,2]


evalFunc(Y_0.prob,Y_0,test$couponSlot)


miss.class(Y.hat,Y)
miss.class(Y_0.hat,Y_0)



sum(predict(fit) != Y) / length(Y)
if(pr == T){
  summary(fit)    
}
### Model fitted value
train.misRate = sum(predict(fit) != Y) / length(Y)
train.confusionTable = table(data.frame(Truth = Y,"Est." = predict(fit)))

### Model Prediction
Y_0.hat = predict(svmfit,data.frame(X = X_0)) 
test.confusionTable = table(data.frame(Truth = factor(Y_0),Pred = factor(Y_0.hat)))
test.misRate = sum(Y_0.hat != Y_0) / length(Y_0)

out.svm = list(Model = fit,
               train.misRate = train.misRate,
               train.confusionTable = train.confusionTable,
               test.misRate = test.misRate,




### GLM Model 
colnames(train)
rowIndex = 1:nrow(train)
colIndex = c(12,6:9,15)
#colIndex = c(22:30)
#rowIndex = which(train$userID %in% test$userID)

X_0 = test[,colIndex]
Y_0 = test$couponUsed
X = train[rowIndex,colIndex]
Y = train$couponUsed[rowIndex]

X[is.na(X)] = 0
X_0[is.na(X_0)] = 0



dat = data.frame(X=X, Y=as.factor(Y))
glmfit = glm(Y ~ . ,data = dat,family = binomial(link=logit))
sum(ifelse(glmfit$fitted.values >.5,1,0) != Y) / length(Y)
summary(glmfit)
Y_0.prob.hat = predict(glmfit,data.frame(X = X_0),type="response", se.fit=FALSE)
Y_0.hat = ifelse(Y_0.hat >.5,1,0) 
sum(Y_0.hat != Y_0) / length(Y_0)
plot(residuals(glmfit))

table(data.frame(Truth = Y_0,Pred = Y_0.hat))

table(Y)
table(Y_0)


colnames(X)
class(X$categoryIDs)
### Basket Value

hist(dt.train$basketValue,br = 1000)
dim(dt.train[which(dt.train$basketValue > 2000),])
hist(dt.train[which(dt.train$basketValue < 2000),"basketValue"],br = 100)
boxplot(train$price,outline = F)

outlierId = which(dt.train$basketValue > 2000)
dt.nonOutlier = dt.train[which(dt.train$basketValue <= 2000),]
dt.outlier = dt.train[which(dt.train$basketValue > 2000),]
colnames(dt.outlier)
dt.outlier[order(dt.outlier$userID,dt.outlier$orderTime),-(5:28)]
boxplot(dt.train$basketValue ~ factor(dt.train$coupon1Used + dt.train$coupon2Used + dt.train$coupon3Used),outline = F,plot = 2)

basketValue_Var = aggregate(dt.train[-outlierId,]$basketValue,by = list(dt.train[-outlierId,]$userID),function(d){ifelse(length(d)>1,var(d),0)})
basketValue_mean =  aggregate(dt.train[-outlierId,]$basketValue,by = list(dt.train[-outlierId,]$userID),mean)
        
boxplot(basketValue_Var$x,outline = F)
boxplot(basketValue_mean$x,outline = F)

plot(basketValue_mean[-which(basketValue_Var$x == 0),]$x,sqrt(basketValue_Var[-which(basketValue_Var$x == 0),]$x))
plot(basketValue_mean[-(basketValue_Var$x == 0),]$x,log(basketValue_Var[-(basketValue_Var$x == 0),]$x))


### Tmp


tt = table(data.frame(count = train$categoryIDs,used = factor(train$couponUsed)))
rownames(tt) = c()
tt[order(tt[,1]),]
dim(tt)


library(cape)
myImagePlot()

table(train$couponUsed,train$orderDay)
table(train$couponUsed,train$orderWeekday)

table(train$couponUsed,train$couponsReceivedDay)
table(train$couponUsed,train$couponsReceivedWeekday)

table(test$couponUsed,test$orderDay)




t = table(train$couponUsed,train$couponsReceivedDay)
y = t[2,]/(t[1,] + t[2,])
plot((t[1,] + t[2,]),log(y))
plot((t[1,] + t[2,]),y)
plot(y,type = "b")
acf(y)
plot(diff(y),type = "b")


t = table(train$couponUsed,train$orderDay)
y = t[2,]/(t[1,] + t[2,])
plot(y,type = "b")
acf(y)



t = as.numeric(dt$orderTime - dt$couponsReceived)
ind  = which(dt$couponUsed == 1 & t < 24*3600)
ind2  = which(dt$couponUsed == 0 & t < 24*3600)
hist(t[ind],br = 100)
hist(t[ind2],br = 100)


##