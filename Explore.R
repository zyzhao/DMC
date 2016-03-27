
miss.class = function(pred.class, true.class, produceOutput = F) {
  confusion.mat = table(pred.class, true.class)
  if(produceOutput) {
    return(list(confusionMtx = confusion.mat,
                miss_class = 1 - sum(diag(confusion.mat))/sum(confusion.mat),
                sensitivity = confusion.mat[1,1] / sum(confusion.mat[,1]),
                specificity = confusion.mat[2,2] / sum(confusion.mat[,2])))
  }
  else{
    print('miss-class')
    print(1 - sum(diag(confusion.mat))/sum(confusion.mat))
    print('confusion mat')
    print(confusion.mat)
  }
}


load(file = "/Users/zhiyuan/Dropbox/Documents/DMC/Code/DW.RData")
train = dt.sim$train

train$numCouponUsed = train$coupon1Used + train$coupon2Used + train$coupon3Used
train$CouponUsed = (train$numCouponUsed > 0)
table(train$numCouponUsed )
train$maxPrice = apply(train[,c("price1","price2","price3")],1,max)
train$minPrice = apply(train[,c("price1","price2","price3")],1,min)
train$medPrice = apply(train[,c("price1","price2","price3")],1,median)
train$gapTimeInterval = cut(train$couponsReceivedGapTime,breaks = c(0,2000,3000,4000,6000,10000,18000,30000,Inf))


colnames(train)

table(train[,c("CouponUsed","minPrice")])
table(train[,c("CouponUsed","couponsReceivedGapTime")])
tb = table(train[,c("CouponUsed","gapTimeInterval")])
plot(tb[2,] / (tb[1,] + tb[2,]),type = "b")


M1 = glm(CouponUsed ~ gapTimeInterval + maxPrice + minPrice + medPrice,data = train,family = binomial(link=logit))
summary(M1 )
M2 = glm(CouponUsed ~ couponsReceivedGapTime + maxPrice + minPrice + medPrice,data = train,family = binomial(link=logit))
summary(M2)
M3 = glm(CouponUsed ~ as.numeric(gapTimeInterval) + maxPrice + minPrice + medPrice,data = train,family = binomial(link=logit))
summary(M3 )


MM1 = glm(CouponUsed ~ gapTimeInterval + minPrice,data = train,family = binomial(link=logit))
MM2 = glm(CouponUsed ~ couponsReceivedGapTime + minPrice ,data = train,family = binomial(link=logit))
MM3 = glm(CouponUsed ~ as.numeric(gapTimeInterval)  + minPrice,data = train,family = binomial(link=logit))
summary(MM1 )
summary(MM2)
summary(MM3 )


miss.class(round(MM3$fitted.values),train$CouponUsed)


?range
cut(train$couponsReceivedGapTime,10)
hist(log(train$couponsReceivedGapTime),br = 100)
hist(log(train$couponsReceivedGapTime[which(train$CouponUsed == 1)]),br = 100)
hist(train$couponsReceivedGapTime,br = 100)
hist(train$couponsReceivedGapTime[which(train$CouponUsed == 1)],br = 100)

cf = 8000
par(mfcol = c(1,2))
hist(train$couponsReceivedGapTime[which(train$couponsReceivedGapTime < cf)],br = 100)
hist(train$couponsReceivedGapTime[which(train$couponsReceivedGapTime >= cf)],br = 100)
par(mfcol = c(1,1))



cut(1:100, 3)
