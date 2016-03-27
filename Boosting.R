library(tree)
library(gbm)
require(randomForest)
require(foreach)
library(doMC)
require(plyr)
library(e1071)

load(file = "/Users/zhiyuan/Dropbox/Documents/DMC/Code/DW.RData")
load(file = "/Users/zhiyuan/Dropbox/Documents/DMC/Code/hisDt_test.RData")
load(file = "/Users/zhiyuan/Dropbox/Documents/DMC/Code/hisDt.RData")

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

dt.train = read.delim("/Users/zhiyuan/Dropbox/Documents/DMC/DMC 2015/DMC_2015_orders_train.txt",sep = "|")
dt.class = read.delim("/Users/zhiyuan/Dropbox/Documents/DMC/DMC 2015/DMC_2015_orders_class.txt",sep = "|")



train = dt.sim2$train  
test = dt.sim2$test

dt.tr = rbind(dt.sim2$train  ,dt.sim2$test)

dt.cl = formatData(dt.class)
dt.cl = combineCouponCols(dt.cl)
names(dt.cl)
var = c("userID","couponID","brand","productGroup","categoryIDs")
UR1 = aggregate(dt.tr$couponUsed,by = list(userID = dt.tr[,var[1]]),mean)
UR2 = aggregate(dt.tr$couponUsed,by = list(couponID = dt.tr[,var[2]]),mean)
UR3 = aggregate(dt.tr$couponUsed,by = list(brand = dt.tr[,var[3]]),mean)
UR4 = aggregate(dt.tr$couponUsed,by = list(productGroup = dt.tr[,var[4]]),mean)
UR5 = aggregate(dt.tr$couponUsed,by = list(categoryIDs = dt.tr[,var[5]]),mean)


dt.cl = merge(dt.cl,UR1,by = "userID",all.x = T)
dt.cl = merge(dt.cl,UR2,by = "couponID",all.x = T)
dt.cl = merge(dt.cl,UR3,by = "brand",all.x = T)
dt.cl = merge(dt.cl,UR4,by = "productGroup",all.x = T)
dt.cl = merge(dt.cl,UR5,by = "categoryIDs",all.x = T)


names(dt.cl)

sum(complete.cases(newDt))
nrow(newDt)

sum(complete.cases(newDt[,2:5]))
nrow(newDt[,3:5])



extractDate <- function(dt){
  trainIndex = 1:nrow(dt.sim$train)
  
  
  d.coupon = as.Date(dt$couponsReceived)
  d.coupon.diff = as.numeric(d.coupon - min(d.coupon))
  d.coupon.wd = weekdays(d.coupon)
  
  d.order = as.Date(dt$orderTime)
  d.order.diff = as.numeric(d.order - min(d.order))
  d.order.wd = weekdays(d.order)
  
  dt$couponsReceivedDay = d.coupon.diff
  dt$couponsReceivedWeekday = d.coupon.wd
  
  dt$orderDay = d.order.diff
  dt$orderWeekday = d.order.wd
  
  tmpCouponsReceivedAmount = aggregate(dt$couponsReceivedDay,
                                       by = list(couponsReceivedDay = dt$couponsReceivedDay),
                                       length)
  colnames(tmpCouponsReceivedAmount)[2] = "CouponsReceivedAmount"
  dt = merge(dt,tmpCouponsReceivedAmount,by = "couponsReceivedDay",all.x = T)
  
  return(dt)
}



train = dt.tr
test = extractDate(dt.cl)
load(file = "/Users/zhiyuan/Dropbox/Documents/DMC/Code/hisDt.RData")
load(file = "/Users/zhiyuan/Dropbox/Documents/DMC/Code/hisDt_test.RData")
newDt = rbind(newDt,newDt.test)[,1:5]
newDt.test = dt.cl[,17:21]
names(newDt.test) =1:5
colnames(newDt) =1:5


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




registerDoMC(cores=4)


# Boosting model fit

# ======== Level-1 ========== #

Y_0.prob = BOOSTING(X,Y,X_0,B = 200,M = 3,l= .01)

# test.err1 = evalFunc(Y_0.prob,Y_0,slot_0)


# ======== Level-2 ========== #
colIndex = 3:5
X_new = cbind(X,newDt[,colIndex])
comId = which(complete.cases(X_new))
#comId = comId[floor(length(comId)/2) : length(comId)]
X_new = X_new[comId,]
Y_new = Y[comId]

X_0_new = cbind(X_0,newDt.test[,colIndex])


comId.new  = complete.cases(X_0_new)
X_0_new_sub = X_0_new[comId.new,]
# sum(comId.new)

Y_0_new_sub.prob = BOOSTING(X_new,Y_new,X_0_new_sub,B = 200,M = 3,l= .01)

Y_0_L2.prob = Y_0.prob
Y_0_L2.prob[comId.new] = Y_0_new_sub.prob 


# ======== Level-3 ========== #
colIndex = 2:5
X_new = cbind(X,newDt[,colIndex])
comId = complete.cases(X_new)
X_new = X_new[comId,]
Y_new = Y[comId]

X_0_new = cbind(X_0,newDt.test[,colIndex])

comId.new  = complete.cases(X_0_new)
X_0_new_sub = X_0_new[comId.new,]
# sum(comId.new )

Y_0_new_sub.prob = BOOSTING(X_new,Y_new,X_0_new_sub,B = 200,M = 3,l= .01)

Y_0_L3.prob = Y_0_L2.prob
Y_0_L3.prob[comId.new] = Y_0_new_sub.prob 



# ======== Level-4 ========== #
colIndex = 1:5
X_new = cbind(X,newDt[,colIndex])
comId = which(complete.cases(X_new))
# comId = comId[floor(length(comId)/5) : length(comId)]
X_new = X_new[comId,]
Y_new = Y[comId]

X_0_new = cbind(X_0,newDt.test[,colIndex])

comId.new  = complete.cases(X_0_new)
X_0_new_sub = X_0_new[comId.new,]
# sum(comId.new )

Y_0_new_sub.prob = BOOSTING(X_new,Y_new,X_0_new_sub,B = 200,M = 3,l= .01)

Y_0_L4.prob = Y_0_L3.prob
Y_0_L4.prob[comId.new] = Y_0_new_sub.prob 



dt.cl$couponUsed = Y_0_L4.prob
head(dt.cl)
pred = dt.cl[,c("orderID","couponUsed","couponSlot")]

pr1 = pred[which(pred$couponSlot == 1),]
pr2 = pred[which(pred$couponSlot == 2),]
pr3 = pred[which(pred$couponSlot == 3),]

out = cbind(pr1[order(pr1$orderID),1:2],
            pr2[order(pr2$orderID),2],
            pr3[order(pr3$orderID),2])
colnames(out) = c("orderID",paste0("coupon",1:3,"Prediction"))
head(out)
sum(complete.cases(out))
nrow(out)
write.csv(out, file = "/Users/zhiyuan/Google Drive/Data Mining Cup/Rcode/couponUsedOut.csv")
