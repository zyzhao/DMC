library(tree)
library(gbm)
require(randomForest)
require(foreach)
library(doMC)
require(plyr)
library(e1071)




registerDoMC(cores=4)




### Tree
### Random Forest
RF <- function(X,Y,X_0,B,bagging=F){
  b = floor(B/4)
  if(baggeing == F){
    out.tree1 = foreach(ntree=rep(b, 4), .combine=combine, .packages='randomForest') %dopar% 
      randomForest(X, as.factor(Y), importance = T, ntree = B)
  }else{
    out.tree1 = foreach(ntree=rep(b, 4), .combine=combine, .packages='randomForest') %dopar% 
      randomForest(X, as.factor(Y), importance = T, ntree = B,mtry = ncol(X))
  }
  Y_0.prob =predict(out.tree1,X_0,n.trees = B,type = "prob") 
  Y_0.prob = Y_0.prob[,2]
  return(Y_0.prob)
}

out.tree.boost = gbm(Y_new ~ .,data = X_new,
                     n.trees = B,
                     shrinkage = l,
                     interaction.depth = M,
                     distribution = "adaboost",
                     n.cores = 4)
Y_0.prob = predict(out.tree.boost,  X_0_new_sub, n.trees = B, type = "response")
names(X_0_new_sub)

BOOSTING <- function(X,Y,X_0,B,M,l){
  # B = 200;M = 3;l= .01
  out.tree.boost = gbm(Y ~ .,data = X,
                       n.trees = B,
                       shrinkage = l,
                       interaction.depth = M,
                       distribution = "adaboost",
                       n.cores = 4)
  #Y.prob =predict(out.tree.boost,  X, n.trees = B, type = "response")
  Y_0.prob =predict(out.tree.boost,  X_0, n.trees = B, type = "response")
  #Y_all.prob =predict(out.tree.boost,  X_all, n.trees = B, type = "response")

#   test.err = evalFunc(Y_0.prob,Y_0,slot_0)
#   test.err  
  
  return(Y_0.prob)
}


modelSelection <- function(X,Y,X_0){
#   nTree = c(100,200,500)
#   treeDepth = 1:4
#   rate = c(1,.1,.01,.001)
  nTree = c(100,200)
  treeDepth = 1:2
  rate = c(1,.1)
  i = 0;boostTrees = list();out.summary = c()
  for(B in nTree){
    for(M in treeDepth){
      for(l in rate){
        out.tree.boost = gbm(Y ~ .,data = X,
                             n.trees = B,
                             shrinkage = l,
                             interaction.depth = M,
                             distribution = "adaboost",
                             n.cores = 4)
        i =i+1
        print(i)
        boostTrees[[i]] = out.tree.boost 
        # Y.hat =predict(out.tree.boost,  X, n.trees = B, type = "response")
        Y_0.hat =predict(out.tree.boost,  X_0, n.trees = B, type = "response")

        test.err = evalFunc(Y_0.hat,Y_0,slot_0)
        out.summary = rbind(out.summary,
                            c(B,M,l,test.err[[1]]))
      }
    }
  }
  out.summary[,-c(1:3)] = round(out.summary[,-c(1:3)] ,1)
  bestId = which.min(min(out.summary[,4]))
  bestModel = boostTrees[[bestId]]
  Y_0.hat = predict(bestModel,  X_0, n.trees = bestModel$n.trees, type = "response")
  out = list(err = out.summary,
             bestModel = bestModel,
             pred = Y_0.hat)
  return(out)
}





# Boosting model fit

# ======== Set up ==========#

nTree = c(100,200)
treeDepth = 1:3
rate = c(1,.1,.01)

# ======== Level-1 ========== #

i = 0;boostTrees = list();out.summary = c()
for(B in nTree){
  for(M in treeDepth){
    for(l in rate){
      out.tree.boost = gbm(Y ~ .,data = X,
                           n.trees = B,
                           shrinkage = l,
                           interaction.depth = M,
                           distribution = "adaboost",
                           n.cores = 4)
      i =i+1
      print(i)
      boostTrees[[i]] = out.tree.boost 
      # Y.hat =predict(out.tree.boost,  X, n.trees = B, type = "response")
      Y_0.hat =predict(out.tree.boost,  X_0, n.trees = B, type = "response")
      
      test.err = evalFunc(Y_0.hat,Y_0,slot_0)
      out.summary = rbind(out.summary,
                          c(B,M,l,test.err[[1]]))
    }
  }
}
out.summary[,-c(1:3)] = round(out.summary[,-c(1:3)] ,1)
bestId = which.min(min(out.summary[,4]))
bestModel = boostTrees[[bestId]]
Y_0.hat = predict(bestModel,  X_0, n.trees = bestModel$n.trees, type = "response")


model1 = list(err = out.summary,
               bestModel = bestModel,
               pred = Y_0.hat)
Y_0.prob = model1$pred

test.err1 = evalFunc(Y_0.prob,Y_0,slot_0)


# ======== Level-2 ========== #
colIndex = 3:5
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


i = 0;boostTrees = list();out.summary = c()
for(B in nTree){
  for(M in treeDepth){
    for(l in rate){
      out.tree.boost = gbm(Y_new ~ .,data = X_new,
                           n.trees = B,
                           shrinkage = l,
                           interaction.depth = M,
                           distribution = "adaboost",
                           n.cores = 4)
      i =i+1
      print(i)
      boostTrees[[i]] = out.tree.boost 
      # Y.hat =predict(out.tree.boost,  X, n.trees = B, type = "response")
      Y_0_new_sub.prob =predict(out.tree.boost,  X_0_new_sub, n.trees = B, type = "response")

      Y_0.tmp= model1$pred
      Y_0.tmp[comId.new] = Y_0_new_sub.prob 
      
      test.err = evalFunc(Y_0.tmp,Y_0,slot_0)
      out.summary = rbind(out.summary,
                          c(B,M,l,test.err[[1]]))
    }
  }
}
out.summary[,-c(1:3)] = round(out.summary[,-c(1:3)] ,1)
bestId = which.min(min(out.summary[,4]))
bestModel = boostTrees[[bestId]]

Y_0_new_sub.prob =predict(bestModel,  X_0_new_sub, n.trees = bestModel$n.trees, type = "response")
Y_0.tmp= Y_0.prob
Y_0.tmp[comId.new] = Y_0_new_sub.prob 


model2 = list(err = out.summary,
              bestModel = bestModel,
              pred = Y_0.tmp)

Y_0_new_sub.prob =  model2$pred

Y_0_L2.prob = model2$pred
test.err2 = evalFunc(Y_0_L2.prob,Y_0,slot_0)

# ======== Level-3 ========== #
colIndex = 2:5
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


i = 0;boostTrees = list();out.summary = c()
for(B in nTree){
  for(M in treeDepth){
    for(l in rate){
      out.tree.boost = gbm(Y_new ~ .,data = X_new,
                           n.trees = B,
                           shrinkage = l,
                           interaction.depth = M,
                           distribution = "adaboost",
                           n.cores = 4)
      i =i+1
      print(i)
      boostTrees[[i]] = out.tree.boost 
      # Y.hat =predict(out.tree.boost,  X, n.trees = B, type = "response")
      Y_0_new_sub.prob =predict(out.tree.boost,  X_0_new_sub, n.trees = B, type = "response")
      
      Y_0.tmp= model2$pred
      Y_0.tmp[comId.new] = Y_0_new_sub.prob 
      
      test.err = evalFunc(Y_0.tmp,Y_0,slot_0)
      out.summary = rbind(out.summary,
                          c(B,M,l,test.err[[1]]))
    }
  }
}
out.summary[,-c(1:3)] = round(out.summary[,-c(1:3)] ,1)
bestId = which.min(min(out.summary[,4]))
bestModel = boostTrees[[bestId]]


Y_0_new_sub.prob =predict(bestModel,  X_0_new_sub, n.trees = bestModel$n.trees, type = "response")
Y_0.tmp= model2$pred
Y_0.tmp[comId.new] = Y_0_new_sub.prob 



model3 = list(err = out.summary,
              bestModel = bestModel,
              pred = Y_0.tmp)

Y_0_L3.prob = model3$pred
test.err3 = evalFunc(Y_0_L3.prob,Y_0,slot_0)

# ======== Level-4 ========== #
colIndex = 1:5
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


i = 0;boostTrees = list();out.summary = c()
for(B in nTree){
  for(M in treeDepth){
    for(l in rate){
      out.tree.boost = gbm(Y_new ~ .,data = X_new,
                           n.trees = B,
                           shrinkage = l,
                           interaction.depth = M,
                           distribution = "adaboost",
                           n.cores = 4)
      i =i+1
      print(i)
      boostTrees[[i]] = out.tree.boost 
      # Y.hat =predict(out.tree.boost,  X, n.trees = B, type = "response")
      Y_0_new_sub.prob =predict(out.tree.boost,  X_0_new_sub, n.trees = B, type = "response")
      
      Y_0.tmp= model3$pred
      Y_0.tmp[comId.new] = Y_0_new_sub.prob 
      
      test.err = evalFunc(Y_0.tmp,Y_0,slot_0)
      out.summary = rbind(out.summary,
                          c(B,M,l,test.err[[1]]))
    }
  }
}
out.summary[,-c(1:3)] = round(out.summary[,-c(1:3)] ,1)
bestId = which.min(min(out.summary[,4]))
bestModel = boostTrees[[bestId]]


Y_0_new_sub.prob =predict(bestModel,  X_0_new_sub, n.trees = bestModel$n.trees, type = "response")
Y_0.tmp= model3$pred
Y_0.tmp[comId.new] = Y_0_new_sub.prob 



model4 = list(err = out.summary,
              bestModel = bestModel,
              pred = Y_0.tmp)


Y_0_L4.prob = model4$pred
test.err4 = evalFunc(Y_0_L4.prob,Y_0,slot_0)


test.err1[[1]]
test.err2[[1]]
test.err3[[1]]
test.err4[[1]]



### BasketValue
dt = rbind(dt.sim$train,dt.sim$test)
ts.ind = sample(1:nrow(dt),500)
ts.ind = (nrow(dt) - 500):nrow(dt)
tr = dt[-ts.ind,]
ts = dt[ts.ind,]

dt2 = rbind(dt.sim2$train,dt.sim2$test)
dt2 = dt2[order(dt2$orderID),]
ts.ind2 = sample(1:nrow(dt),500*3)
ts.ind2 = (nrow(dt2) - 500*3 + 1):nrow(dt2)
tr2 = dt2[-ts.ind2,]
ts2 = dt2[ts.ind2,]



tr$couponsReceived = as.numeric(tr$couponsReceived)
ts$couponsReceived = as.numeric(ts$couponsReceived)

basketCal <- function(v,p = .99){
  cutoff = quantile(v,p)
  mean(v[which(v<cutoff)])
}


uBasketValue = aggregate(tr$basketValue,by = list(userID = tr$userID),mean)
testBasketValue = merge(ts[,c("userID","basketValue","couponsReceived")],uBasketValue,by = "userID",all.x = T)

uBasketValueByCoupon = aggregate(tr2$basketValue,by = list(couponID = tr2$couponID),mean)
testBasketValueByCoupon = merge(ts2[,c("userID","orderID","basketValue","couponID")],uBasketValueByCoupon,by = "couponID",all.x = T)

testBasketValueByCoupon2 = aggregate(testBasketValueByCoupon$x,by = list(userID = testBasketValueByCoupon$userID,
                                                                         orderID =testBasketValueByCoupon$orderID ), mean)


testBasketValue = merge(testBasketValue,testBasketValueByCoupon2,by = "userID",all.x = T)





testBasketValue[which(is.na(testBasketValue$x)),]
names(ts)
ts[which(is.na(testBasketValue$x)),c("userID","orderTime","basketValue","orderIndexOfUser")]

meanBasketValue=  basketCal(tr$basketValue,1)


testBasketValue$pred =  testBasketValue$x.x
# testBasketValue$pred[which(is.na(testBasketValue$pred))] = testBasketValue$x.y[which(is.na(testBasketValue$pred))] 
testBasketValue$pred[which(is.na(testBasketValue$pred))] = meanBasketValue




#testBasketValue$x[which(is.na(testBasketValue$x))] =  meanBasketValue
sum((testBasketValue$pred - testBasketValue$basketValue)^2) / mean(testBasketValue$basketValue)^2


sum((mean(testBasketValue$basketValue)- testBasketValue$basketValue)^2) / mean(testBasketValue$basketValue)^2

