library(utils)

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
colnames(dt.train)



obs = train[10000,]

hisRate <- function(obs){
  var = c("userID","couponID","brand","productGroup","categoryIDs")
  value = obs[,var]
  cutoff = obs$orderDay
  dt = train
# 
#   
#   levels(factor(train$couponID))
#   levels(factor(train$brand))
#   levels(factor(train$productGroup))
#   levels(factor(train$categoryIDs))
#   
#   c = 2
  
  ind0 = (dt$orderDay < cutoff)
  ind = c()
  # "userID","couponID"
  for(i in 1:5){
    ind = cbind(ind,dt[,var[i]] == unlist(value[i]))
  }
  
  # Contain category
  ind6_7 = t(apply(data.frame(dt[,var[5]]),1,function(x,v){
#                 x = "796c5815752a170021838a78bf7fab34,ac6d9e2b42dff101d8e6e5582d7a45cf,4b3e2dbd4d59c43bf6982405a5f7fce7"    
#                 v = "796c5815752a170021838a78bf7fab34,ac6d9e2b42dff101d8e6e5582d7a451f"
                x.sp = strsplit(x,",")[[1]]
                v.sp = strsplit(v,",")[[1]]
                tmp = apply(t(v.sp),1,function(v){v %in% x.sp})
                id1 = all(tmp)
                id2 = any(tmp)
                return(c(id1,id2))
              },
              v = unlist(value[5])))
  
  ind = cbind(ind,ind6_7)
  
  summary(ind)
  
  
  ### 1-7 one feature subsets
  dats = list()
  for(i in 1:ncol(ind)){
    p = length(dats) + 1
    dats[[p]] = train[which(ind[,i] == T & ind0 == T),]
  }
  
  
  ###  one feature subsets
  for(i in 2:ncol(ind)){
    p = length(dats) + 1
    dats[[p]] = dats[[i]][which(ind[,1] == T & ind[,i] == T & ind0 == T),]
  
  }
  
  rate = unlist(lapply(dats,function(dt){
                  sum(dt$couponUsed) / length(dt$couponUsed )
                }))
  return(rate)
}


system.time(hisRate(train[10000,]))
var = c("userID","couponID","brand","productGroup","categoryIDs")

newDt = c()
for(i in 1:nrow(train)){
  newDt = rbind(newDt,hisRate(train[i,]))
  print(i)
}


newDt.test = c()
for(i in 1:nrow(test)){
  newDt.test = rbind(newDt.test,hisRate(test[i,]))
  print(i)
}

save(newDt.test,file = "/Users/zhiyuan/Dropbox/Documents/DMC/Code/hisDt_test.RData")
load(file = "/Users/zhiyuan/Dropbox/Documents/DMC/Code/hisDt_test.RData")



# save(newDt,file = "/Users/zhiyuan/Dropbox/Documents/DMC/Code/hisDt.RData")
load(file = "/Users/zhiyuan/Dropbox/Documents/DMC/Code/hisDt.RData")


colIndex = c("couponsReceivedGapTime","price","premiumProduct","basePrice","reward","orderWeekday")
# colIndex = c("couponsReceivedGapTime","price","premiumProduct","basePrice" )

slot = test$couponSlot
X_0 = test[,colIndex]
Y_0 = test$couponUsed
X = train[,colIndex]
Y = train$couponUsed
X_all = rbind(X,X_0)
Y_all = c(Y,Y_0)

X_new = cbind(X,newDt[,2:7])
comId = complete.cases(X_new)
X_new = X_new[comId,]
Y_new = Y[comId]

X_0_new = cbind(X_0,newDt.test[,2:7])
Y_0_new = Y_0


registerDoMC(cores=2)
out.tree.new = foreach(ntree=rep(100, 3), .combine=combine, .packages='randomForest') %dopar% 
  randomForest(X_new, as.factor(Y_new), importance = T, ntree = 300)

comId.new  = complete.cases(X_0_new)
X_0_new_sub = X_0_new[comId.new,]
Y_0_new_sub = Y_0_new[comId.new]

Y_0_new_sub.prob =predict(out.tree.new,X_0_new_sub,n.trees = 300,type = "prob") 
Y_0_new_sub.prob = Y_0_new_sub.prob[,2]


Y_0_ex.prob = Y_0.prob
Y_0_ex.prob[comId.new] = Y_0_new_sub.prob 

evalFunc(Y_0_ex.prob,Y_0,slot)
evalFunc(Y_0.prob,Y_0,slot)

###

X_new = cbind(X,newDt[,2:7])
comId = complete.cases(X_new)
X_new = X_new[comId,]
Y_new = Y[comId]

X_0_new = cbind(X_0,newDt.test[,2:7])
X_0_new = cbind(X_0,newDt.test[,2])


Y_0_new = Y_0


registerDoMC(cores=3)
out.tree.new = foreach(ntree=rep(100, 3), .combine=combine, .packages='randomForest') %dopar% 
  randomForest(X_new, as.factor(Y_new), importance = T, ntree = 300)

comId.new  = complete.cases(X_0_new)
X_0_new_sub = X_0_new[comId.new,]
Y_0_new_sub = Y_0_new[comId.new]

Y_0_new_sub.prob =predict(out.tree.new,X_0_new_sub,n.trees = 300,type = "prob") 
Y_0_new_sub.prob = Y_0_new_sub.prob[,2]


Y_0_ex.prob = Y_0.prob
Y_0_ex.prob[comId.new] = Y_0_new_sub.prob 

evalFunc(Y_0_ex.prob,Y_0,slot)
evalFunc(Y_0.prob,Y_0,slot)



###=============


dt.new = cbind(dt.new,train$couponsReceivedGapTime[(nrow(newDt)/2):nrow(newDt)],train$couponUsed[(nrow(newDt)/2):nrow(newDt)])
dt.new = dt.new[complete.cases(dt.new),]
X = dt.new[,1:(ncol(dt.new)-1)]
Y = dt.new[,ncol(dt.new)]

pca = princomp(X,cor = T)
pca
plot(pca$scores[,1],pca$scores[,2],col =Y + 1)
plot(pca$scores[,1],pca$scores[,3],col =Y + 1)
plot(pca$scores[,2],pca$scores[,3],col =Y + 1)

library(tree)
library(gbm)
require(randomForest)
require(foreach)
library(doMC)
require(plyr)

out.tree = foreach(ntree=rep(50, 4), .combine=combine, .packages='randomForest') %dopar% 
  randomForest(X, as.factor(Y), importance = T, ntree = 300)

out.tree$importance


miss.class(predict(out.tree,X,n.trees = 200),Y)
miss.class(predict(out.tree,X_0,n.trees = 200),Y_0)



library(foreach)
usedData = train[which(train$couponUsed == 1),][2000:2010,]
newDt <- foreach(i=1:nrow(usedData)) %do% hisRate(usedData[i,])
newDt

usedData = train[which(train$couponUsed == 0),][5000:5010,]
newDt <- foreach(i=1:nrow(usedData)) %do% hisRate(usedData[i,])
newDt

dats[[3]]$couponUsed

summary(dats)
length(dats[[2]])
lapply(dats,nrow)
### ======


# save(newDt,file = "/Users/zhiyuan/Dropbox/Documents/DMC/Code/hisDt.RData")
load(file = "/Users/zhiyuan/Dropbox/Documents/DMC/Code/hisDt.RData")

dim(newDt)
dim(train)

row.id =   (nrow(newDt)/2):nrow(newDt)

d = cbind(newDt[,c(1:7)],
           train$couponsReceivedGapTime,
           train$couponUsed)
d = d[row.id,]
d = d[complete.cases(d),]
dim(d)

train.id = 1:4000


X = d[train.id ,1:(ncol(d)-1)]
Y = d[train.id ,ncol(d)]
X_0 = d[-train.id ,1:(ncol(d)-1)]
Y_0 = d[-train.id ,ncol(d)]


library(tree)
library(gbm)
require(randomForest)
require(foreach)
library(doMC)
require(plyr)

out.tree = foreach(ntree=rep(50, 4), .combine=combine, .packages='randomForest') %dopar% 
  randomForest(X, as.factor(Y), importance = T, ntree = 300)

out.tree$importance


miss.class(predict(out.tree,X,n.trees = 200),Y)
miss.class(predict(out.tree,X_0,n.trees = 200),Y_0)


library(e1071)
dat = data.frame(X=X, Y=as.factor(Y))
fit== tune(svm,Y~., data=dat, kernel="radial", 
           ranges = list(gamma=c(1,2),
                         cost=c(0.01, 0.1,1,10)))



Y.hat = predict(fit) 
Y_0.hat = predict(fit,data.frame(X = X_0)) 

miss.class(Y.hat,Y)
miss.class(Y_0.hat,Y_0)

### polynomial

fit==tune(svm,Y~., data=dat, kernel="polynomial", 
          ranges = list(degree = c(3,5,10),
                        cost=c(0.01, 0.1, 1,10)))
Y.hat = predict(fit) 
Y_0.hat = predict(fit,data.frame(X = X_0)) 

miss.class(Y.hat,Y)
miss.class(Y_0.hat,Y_0)


### NN
library(neuralnet)

row.id =   (nrow(newDt)/2):nrow(newDt)

d = cbind(newDt[,c(1:7)],
          train$couponsReceivedGapTime,
          train$couponUsed)
d = d[row.id,]
d = d[complete.cases(d),]
dim(d)

train.id = 1:4000


X = d[train.id ,1:(ncol(d)-1)]
Y = d[train.id ,ncol(d)]
X_0 = d[-train.id ,1:(ncol(d)-1)]
Y_0 = d[-train.id ,ncol(d)]


dat = data.frame(X=X, Y=Y)
model.out = as.formula(paste("Y ~ ",paste(names(dat[,3:7]),collapse='+')))

nRep = 3
nn.out = neuralnet(model.out, data = dat, hidden=c(3), threshold=0.01,rep=nRep,err.fct='ce',
                   linear.output=F)

nn.out$net.result

Y_0.prob = compute(nn.out,X_0[,3:7],rep=1)$net.result

hist(Y_0.prob)
Y_0.hat = round(Y_0.prob)
miss.class(Y_0.hat,Y_0)



