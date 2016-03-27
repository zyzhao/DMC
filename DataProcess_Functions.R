dt.train = read.delim("/Users/zhiyuan/Dropbox/Documents/DMC/DMC 2015/DMC_2015_orders_train.txt",sep = "|")
dt.class = read.delim("/Users/zhiyuan/Dropbox/Documents/DMC/DMC 2015/DMC_2015_orders_class.txt",sep = "|")


### Change all factor columns to character format, and format the time columns as POSIXlt/POSIXt
formatData <- function(dt,p = F){
  dt$couponsReceived = strptime(as.vector(dt$couponsReceived),format='%Y-%m-%d %H:%M:%S')
  dt$orderTime = strptime(as.vector(dt$orderTime),format='%Y-%m-%d %H:%M:%S')
  dt$couponsReceivedGapTime = as.numeric(dt$orderTime - dt$couponsReceived)

  col_names = colnames(dt)
  for(i in 1:ncol(dt)){
    if(class(dt[,i])[1] == "factor"){dt[,i] = as.vector(dt[,i])}
    if(p == T){print(paste0(col_names[i],": ",class(dt[,i])))}
  }
  return(dt)
}

### Use a date cutoff to simulate train and class dataset
### 6 day ahead cutoff makes the simulated train and class datasets have the same proportion as the real case.
### Put two datasets into a list with names "train" and "test"
dataSimulation <- function(dt,ahead = 6,formatFunction = NULL){
  if(is.null(formatFunction) == F){dt = formatFunction(dt)}
  
  dt$orderTime = strptime(as.vector(dt$orderTime),format='%Y-%m-%d %H:%M:%S')
  cutoff = max(dt$orderTime) - ahead*3600*24
  
  ind.train.sim = which(dt$orderTime < cutoff)
  ind.test.sim = which(dt$orderTime >= cutoff)
  
  train = dt[ind.train.sim,]
  test = dt[ind.test.sim,]
  return(list(train = train,
              test = test))
}

### This functions essentially add some columns about the information of the datasets like those in the file "EDA_uID.pdf"
### I think this could be used to examine the result of a model fit in different situations. 
### For Example, we want to know if there is different between the prediction of the users with history and without history.
dataFeature <- function(dt.sim,out = "merge"){
  dt.train = dt.sim$train
  dt.class = dt.sim$test
  
  dt.class$class = rep(1,nrow(dt.class))
  dt.train$class = rep(0,nrow(dt.train))
  dt = rbind(dt.train,dt.class)
  
  dt = dt[order(dt$userID,dt$orderTime),]
  
  uID.pre = 0
  ind = c()
  for(r in 1:nrow(dt)){
    uID.cur = dt$userID[r]
    if(uID.cur != uID.pre){
      ind = c(ind,1)
    }else{
      ind = c(ind,ind[length(ind)] + 1)
    }
    uID.pre  = uID.cur
  }
  dt.orderIndex  = dt[,c("userID","orderID")]
  dt.orderIndex$orderIndexOfUser = ind ### Key  is UserID and OrderID
  
  dt.t = dt.orderIndex[which(dt$class == 0),]
  dt.c = dt.orderIndex[which(dt$class == 1),]
  
  
  dt.user =data.frame(userID = unique(dt$userID))
  lastOrderInTrain = aggregate(dt.t$orderIndexOfUser,by = list(userID = dt.t$userID),max)
  lastOrderInClass = aggregate(dt.c$orderIndexOfUser,by = list(userID = dt.c$userID),max)
  
  
  dt.user = merge(dt.user,lastOrderInTrain,by = "userID",all.x = T)
  dt.user = merge(dt.user,lastOrderInClass,by = "userID",all.x = T)
  colnames(dt.user) = c("userID","lastOrderInTrain","lastOrderInClass")
  
  dt.user[is.na(dt.user)] = 0
  dt.user$predStep = ifelse(dt.user$lastOrderInClass > 0,dt.user[,3] - dt.user[,2],0)
  
  if(tolower(out) == "merge"){
    dt = merge(dt,dt.orderIndex,by = c("userID","orderID"),all.x = T)
    dt = merge(dt,dt.user,by = c("userID"),all.x = T)
    output = list(train = dt[which(dt$class == 0),],
                  test = dt[which(dt$class == 1),])
  }else{
    output = merge(dt.orderIndex,dt.user,by = c("userID"),all.x = T)
  }
  return(output)
}

### Combine 3 groups of coupon columns to 1 group, and create a new column "couponSlot" to record the original index of that coupon
combineCouponCols <- function(dt){
  col_names = colnames(dt)
  nCol = ncol(dt)
  colStart = which(col_names == "couponID1")
  colEnd = which(col_names == "categoryIDs3")
  step = (colEnd - colStart +1) / 3
  
  tmp1 = (dt[,c(1:4,c(colStart + 0:(step - 1)),colEnd + 1,(colEnd + 4):nCol)])
  tmp1$couponSlot = 1
  tmp2 = (dt[,c(1:4,c(colStart + (step) + 0:(step - 1)),colEnd + 2,(colEnd + 4):nCol)])
  tmp2$couponSlot = 2
  tmp3 = (dt[,c(1:4,c(colStart + 2 * (step) + 0:(step - 1)),colEnd + 3,(colEnd + 4):nCol)])
  tmp3$couponSlot = 3
  colnames(tmp1) = colnames(tmp2) = colnames(tmp3) = c(colnames(dt)[1:colStart - 1],
                                                       c("couponID","price",
                                                         "basePrice","reward",
                                                         "premiumProduct","brand",
                                                         "productGroup","categoryIDs",
                                                         "couponUsed")
                                                       ,colnames(dt)[(colEnd + 4):nCol]
                                                       ,"couponSlot")
  out = rbind(tmp1,tmp2,tmp3)
  return(out)
}

expandFeatureMtx <- function(dt.sim){
  train = dt.sim$train
  test = dt.sim$test
  
  
  C_count = aggregate(userID ~ couponID,data = train,length)
  C_used = aggregate(couponUsed ~ couponID,data = train,sum)
  C_usedRate = merge(C_count,C_used,by = "couponID" ,all.x = T)
  colnames(C_usedRate) = c("couponID","C_count","C_used")
  C_usedRate$C_usedRate = C_usedRate$C_used / C_usedRate$C_used
  train = merge(train,C_usedRate,by = "couponID",all.x = T)
  test = merge(test,C_usedRate,by = "couponID",all.x = T)
  
  
  UC_count = aggregate(couponUsed ~ userID + couponID,data = train,length)
  UC_used = aggregate(couponUsed ~ userID + couponID,data = train,sum)
  UC_usedRate = merge(UC_count,UC_used,by = c("userID","couponID") ,all.x = T)
  colnames(UC_usedRate)[3:4] = c("UC_count","UC_used")
  UC_usedRate$UC_usedRate = UC_usedRate$UC_used / UC_usedRate$UC_count
  train = merge(train,UC_usedRate,by = c("userID","couponID"),all.x = T)
  test = merge(test,UC_usedRate,by = c("userID","couponID"),all.x = T)
  
  
  tmp = aggregate(UC_usedRate ~ userID + couponID,data = train,sum)
  C_userCount =aggregate(UC_usedRate ~ couponID,data = tmp,length)
  C_userUsed =aggregate(UC_usedRate ~ couponID,data = tmp,function(d){sum(d>0)})
  C_userUsedRate = merge(C_userCount,C_userUsed,by = c("couponID") ,all.x = T)
  colnames(C_userUsedRate)[2:3] = c("C_userCount","C_userUsed")
  C_userUsedRate$C_userUsedRate = C_userUsedRate$C_userUsed / C_userUsedRate$C_userCount
  train = merge(train,C_userUsedRate,by = c("couponID"),all.x = T)
  test = merge(test,C_userUsedRate,by = c("couponID"),all.x = T)
  
  #dt[is.na(dt)] = 0
  
  return(list(train = train,
              test = test))
}

extractDate <- function(dt.sim){
  dt = rbind(dt.sim$train,dt.sim$test)
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
  
  return(list(train = dt[which(dt$class==0),],
              test = dt[which(dt$class==1),]))
}





dt.sim = dataSimulation(dt.train,formatFunction = formatData)
dt.sim = dataFeature(dt.sim)
dt.sim2 = lapply(dt.sim,combineCouponCols)
dt.sim2 = expandFeatureMtx(dt.sim2)
dt.sim2 = extractDate(dt.sim2)


# train = dt.sim2$train  
# test = dt.sim2$test
# colnames(train)

save(dt.sim,dt.sim2,file = "/Users/zhiyuan/Dropbox/Documents/DMC/Code/DW.RData")
