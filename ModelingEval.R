library(tree)
library(gbm)
require(randomForest)
require(foreach)
library(doMC)
require(plyr)
library(e1071)

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


evalFunc <- function(pred,true,group = NULL,cutoff = .5,itr = 0){
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
  if(itr == 0){
    err.cut = c()
    for(i in (1:5)){
      pr.cut = pr
      pr.cut[ which(pr<i/10)]=0
      err.cut[i] = evalFunc(pr.cut,tr,group,cutoff = i/10,itr = 1)
    }
    errPlot = plot((1:5)/10,err.cut,type = "b")
  }
  if(itr == 0){
    err.cut2 = c()
    for(i in (1:9)){
      pr.cut2 = pr
      pr.cut2[ which(pr>i/10)]=1
      err.cut2[i] = evalFunc(pr.cut2,tr,group,cutoff = i/10,itr = 1)
    }
    errPlot = plot((1:9)/10,err.cut2,type = "b")
  }
  n = nrow(pr)
  E = sum(((pr[,1] - tr[,1]) / mean(tr[,1]))^2 + ((pr[,2] - tr[,2]) / mean(tr[,2]))^2 + ((pr[,3] - tr[,3]) / mean(tr[,3]))^2)
  # print(E)
  if(itr == 0){
    return(list(E,err.cut,err.cut2))
  }else{
    return(E)
  }
}

evalFunc(Y_0.prob,Y_0,slot)

train = dt.sim2$train  
test = dt.sim2$test


### Define Feature Mtx

# train = dt.sim2$train
# test = dt.sim2$test
# colnames(train)
# formatDt <- function(dt){
#   dt$gapTimeInterval = cut(dt$couponsReceivedGapTime,breaks = c(0,2000,3000,4000,6000,10000,18000,30000,Inf))
#   return(dt)
# }
# 
# train = formatDt(train)
# test = formatDt(test)


#test$gapTimeInterval = cut(test$couponsReceivedGapTime,breaks = c(0,2000,3000,4000,6000,10000,18000,30000,Inf))


names(train)
train$orderWeekday = factor(train$orderWeekday )
test$orderWeekday = factor(test$orderWeekday )

train$productGroup = factor(train$productGroup )
test$productGroup = factor(test$productGroup )


colIndex = c("couponsReceivedGapTime","price","premiumProduct","basePrice","reward","orderWeekday")
# colIndex = c("couponsReceivedGapTime","price","premiumProduct","basePrice" )



slot = train$couponSlot
slot_0 = test$couponSlot
slot_all = c(train$couponSlot,test$couponSlot)

X_0 = test[,colIndex]
Y_0 = test$couponUsed
X = train[,colIndex]
Y = train$couponUsed
X_all = rbind(X,X_0)
Y_all = c(Y,Y_0)
colnames(X)
colnames(X_0)
which(is.na(X_0))


### Tree
### Random Forest
out.tree1 = foreach(ntree=rep(100, 3), .combine=combine, .packages='randomForest') %dopar% 
  randomForest(X, as.factor(Y), importance = T, ntree = 300)

Y_0.prob =predict(out.tree1,X_0,n.trees = 300,type = "prob") 
Y_0.prob = Y_0.prob[,2]
evalFunc(Y_0.prob,Y_0,slot_0)
evalFunc(rep(mean(Y),length(Y_0)),Y_0,slot_0)
evalFunc(rep(0,length(Y_0)),Y_0,slot_0)


### Bagging
out.tree2 = foreach(ntree=rep(50, 4), .combine=combine, .packages='randomForest') %dopar% 
  randomForest(X, as.factor(Y), importance = T, ntree = 200,mtry = ncol(X))

Y_0.prob =predict(out.tree2,X_0,n.trees = 300,type = "prob") 
Y_0.prob = Y_0.prob[,2]
evalFunc(Y_0.prob,Y_0,slot_0)



### Boosting
#B = 1000;M = 4;l= .001
i = 0;boostTrees = list();out.summary = c()
for(B in c(100,200,500)){
  for(M in 1:4){
    for(l in c(1,.1,.01,.001)){
      out.tree.boost = gbm(Y ~ .,data = X,
                           n.trees = B,
                           shrinkage = l,
                           interaction.depth = M,
                           distribution = "adaboost",
                           n.cores = 4)
      i =i+1
      print(i)
      boostTrees[[i]] = out.tree.boost 
      Y.hat =predict(out.tree.boost,  X, n.trees = B, type = "response")
      Y_0.hat =predict(out.tree.boost,  X_0, n.trees = B, type = "response")
      Y_all.hat =predict(out.tree.boost,  X_all, n.trees = B, type = "response")
      train.err = evalFunc(Y.hat,Y,slot)
      test.err = evalFunc(Y_0.hat,Y_0,slot_0)
      all.err = evalFunc(Y_all.hat,Y_all,slot_all)
      out.summary = rbind(out.summary,
                          c(B,M,l,train.err[[1]],test.err[[1]],all.err[[1]]))
    }
  }
}
out.summary[,4:6] = round(out.summary[,4:6] ,1)
which.min(min(out.summary[,4]))
which.min(out.summary[,5])  ## 6075.0
which.min(min(out.summary[,6]))


# Boosting model fit

# ======== Level-1 ========== #

Y_0.prob = BOOSTING(X,Y,X_0,B = 200,M = 3,l= .01)

test.err1 = evalFunc(Y_0.prob,Y_0,slot_0)


# ======== Level-2 ========== #
colIndex = 3:7
X_new = cbind(X,newDt[,colIndex])
comId = which(complete.cases(X_new))
#comId = comId[floor(length(comId)/2) : length(comId)]
X_new = X_new[comId,]
Y_new = Y[comId]

X_0_new = cbind(X_0,newDt.test[,colIndex])
Y_0_new = Y_0

comId.new  = complete.cases(X_0_new)
X_0_new_sub = X_0_new[comId.new,]
Y_0_new_sub = Y_0_new[comId.new]
# sum(comId.new )

Y_0_new_sub.prob = BOOSTING(X_new,Y_new,X_0_new_sub,B = 200,M = 3,l= .01)

Y_0_L2.prob = Y_0.prob
Y_0_L2.prob[comId.new] = Y_0_new_sub.prob 

test.err2 = evalFunc(Y_0_L2.prob,Y_0,slot_0)

# ======== Level-3 ========== #
colIndex = 2:7
X_new = cbind(X,newDt[,colIndex])
comId = complete.cases(X_new)
X_new = X_new[comId,]
Y_new = Y[comId]

X_0_new = cbind(X_0,newDt.test[,colIndex])
Y_0_new = Y_0

comId.new  = complete.cases(X_0_new)
X_0_new_sub = X_0_new[comId.new,]
Y_0_new_sub = Y_0_new[comId.new]
# sum(comId.new )

Y_0_new_sub.prob = BOOSTING(X_new,Y_new,X_0_new_sub,B = 200,M = 3,l= .01)

Y_0_L3.prob = Y_0_L2.prob
Y_0_L3.prob[comId.new] = Y_0_new_sub.prob 

test.err3 = evalFunc(Y_0_L3.prob,Y_0,slot_0)


# ======== Level-4 ========== #
colIndex = 1:7
X_new = cbind(X,newDt[,colIndex])
comId = which(complete.cases(X_new))
# comId = comId[floor(length(comId)/5) : length(comId)]
X_new = X_new[comId,]
Y_new = Y[comId]

X_0_new = cbind(X_0,newDt.test[,colIndex])
Y_0_new = Y_0

comId.new  = complete.cases(X_0_new)
X_0_new_sub = X_0_new[comId.new,]
Y_0_new_sub = Y_0_new[comId.new]
# sum(comId.new )

Y_0_new_sub.prob = BOOSTING(X_new,Y_new,X_0_new_sub,B = 200,M = 3,l= .01)

Y_0_L4.prob = Y_0_L3.prob
Y_0_L4.prob[comId.new] = Y_0_new_sub.prob 

test.err4 = evalFunc(Y_0_L4.prob,Y_0,slot_0)


# ======== Level-ex ========== #
colIndex = 1:2
X_new = cbind(X,newDt[,colIndex])
comId = complete.cases(X_new)
X_new = X_new[comId,]
Y_new = Y[comId]

X_0_new = cbind(X_0,newDt.test[,colIndex])
Y_0_new = Y_0

comId.new  = complete.cases(X_0_new)
X_0_new_sub = X_0_new[comId.new,]
Y_0_new_sub = Y_0_new[comId.new]
# sum(comId.new )

Y_0_new_sub.prob = BOOSTING(X_new,Y_new,X_0_new_sub,B = 200,M = 3,l= .01)

Y_0_Lex.prob = Y_0_L4.prob
w = .5
Y_0_Lex.prob[comId.new] = (1 - w)*Y_0_Lex.prob[comId.new] + w* Y_0_Lex.prob[comId.new]

test.err5 = evalFunc(Y_0_Lex.prob,Y_0,slot_0)
# ======== BOOSTING SUMMARY ========== #

test.err1[[1]]
test.err2[[1]]
test.err3[[1]]
test.err4[[1]]
test.err5[[1]]

for(w in (1:10/10)){
  Y_0_Lex.prob = Y_0_L4.prob
  Y_0_Lex.prob[comId.new] = (1 - w)*Y_0_Lex.prob[comId.new] + w * Y_0_new_sub.prob
  print(evalFunc(Y_0_Lex.prob,Y_0,slot_0)[[1]])
}





### SVM
colIndex = c("couponsReceivedGapTime","price","orderWeekday")

slot = test$couponSlot
X_0 = test[,colIndex]
Y_0 = test$couponUsed
X = train[,colIndex]
Y = train$couponUsed
X_all = rbind(X,X_0)
Y_all = c(Y,Y_0)
colnames(X)
colnames(X_0)
which(is.na(X_0))


dat = data.frame(X=X, Y=as.factor(Y))
svmfit0 = svm(Y~., data=dat, kernel="linear", cost=.01,probability = T)

svmfit1=tune(svm,Y~., data=dat, kernel="linear", 
             ranges = list(cost=c(0.01, 0.1,1,10)))

svmfit2 = tune(svm,Y~., data=dat, kernel="radial", 
               ranges = list(gamma=c(1,2),
                             cost=c(0.01, 0.1,1,10)))


svmfit3 = tune(svm,Y~., data=dat, kernel="polynomial", 
               ranges = list(degree=c(2,3,4),
                             cost=c(0.01, 0.1,1,10)))


out.summary.svm = c()

svmErr <- function(model){
  Y.hat = predict(model,data.frame(X = X),probability = T) 
  Y_0.hat = predict(model,data.frame(X = X_0),probability = T) 
  Y_all.hat = predict(model,data.frame(X = X_all),probability = T) 
  Y.prob =attributes(Y.hat)$probabilities[,2]
  Y_0.prob =attributes(Y_0.hat)$probabilities[,2]
  Y_all.prob =attributes(Y_all.hat)$probabilities[,2]
  return(c(evalFunc(Y.prob,Y,slot),
           evalFunc(Y_0.prob,Y_0,slot),
           evalFunc(Y_all.prob,Y_all,slot)))
}

svmErr(svmfit0)


### NN
library(neuralnet)

dat = data.frame(X = X[1:5], Y=Y)
model.out = as.formula(paste("Y ~ ",paste(names(dat[,-ncol(dat)]),collapse='+')))

nRep = 3
nn.out = neuralnet(model.out, data = dat, hidden=c(3), threshold=0.01,rep=nRep,err.fct='ce',
                   linear.output=F)

nn.out$net.result

Y_0.prob =( as.vector(compute(nn.out,X_0[1:5],rep=1)$net.result) + 
              as.vector(compute(nn.out,X_0[1:5],rep=2)$net.result) ) / 2
evalFunc(Y_0.prob,Y_0,slot_0)





