rm(list=ls())

installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}

perform_stat <- function(t_table){
  
  TN <- t_table["0","0"] 
  FP <- t_table["0","1"]
  FN <- t_table["1","0"]
  TP <- t_table["1","1"]
  
  stat <- list(c = (TP+TN)/(TN+FP+FN+TP),er = (FP + FN)/(TN+FP+FN+TP), t.1 = FP/(TN+FP), t.2 = 1-(TP/(FN+TP)), pr = TP/(FN+TP), pc = TP/(FP+TP) )
  
  return(stat)
}

needed <-  c('leaps','MASS','ISLR','class','boot','glmnet','pls') 
installIfAbsentAndLoad(needed)

gt <- read.csv("Global Terroism .csv",header = TRUE)

summary(gt)
str(gt)
head(gt,6)

gt_new <- gt[gt$natlty1 != 'NULL' & gt$nkill != 'NULL' & gt$nwound != 'NULL', ]

str(gt_new)

gt_new$natlty1 <- as.numeric(as.character(gt_new$natlty1))
gt_new$nkill <- as.numeric(as.character(gt_new$nkill))
gt_new$nwound <- as.numeric(as.character(gt_new$nwound))

str(gt_new)

################################################################
################## Subset Decision #############################
################################################################

sum(gt_new$gname > 0)

set.seed(5072)

train.prop <- .75
train.indexes <- sample(1:nrow(gt_new),size = train.prop*nrow(gt_new))
train.set <- gt_new[train.indexes,]
test.indexes <- setdiff(1:nrow(gt_new),train.indexes)
test.set <- gt_new[test.indexes,]

test <- prcomp(train.set[-15])
summary(test)

c <-  regsubsets(gname ~ ., data = train.set, nvmax = ncol(gt_new),method = "exhaustive") 
d <- summary(c)
plot(c, scale = "Cp", main = "Validaiton Set")

test.mat <- model.matrix(gname~.,data=test.set)
num.predictors <- ncol(gt_new)-1

#a vector to store the MSEs for the model for each number of predictors
val.errors=rep(NA,num.predictors)

for(i in 1:num.predictors){                              
  #gets the predictors and their coefficients from the model with i predictors
  coefi=coef(c, id=i)
  #matrix multiplication of only relevant predictors to get predicted value
  pred= test.mat[,names(coefi)]%*%coefi
  #calculate MSE
  val.errors[i] =mean((test.set$gname-pred)^2)
}

#Now we can plot the MSEs just like we did with indirect approaches
val.errors
best.val.errors <- which.min(val.errors)
plot(x = 1:length(val.errors), y = val.errors, type = 'b', xlab = 'Num Predictors', ylab = 'MSE', main = "Terrorist Valdiation MSE")
points(x = c(best.val.errors), y = c(val.errors[best.val.errors]), pch = 19, col = 'red', cex = 1)

best.val.errors.sd <- which(val.errors < (sd(val.errors) + min(val.errors)))[1]
points(x = c(best.val.errors.sd), y = c(val.errors[best.val.errors.sd]), pch = 19, col = 'blue', cex = 1)

set.seed(5072)

#randomly shuffle rows of Boston data before we create the folds
mydf <- gt_new
n <- nrow(gt_new)
mydf <- mydf[sample(1:n, n),]
num.folds <- 10
fold.indices <- cut(1:n, breaks=num.folds, labels=FALSE)

#create an empty matrix which will soon store MSEs per model per fold
#each row represents the one of the 13 models. Each column represents MSE from one of the folds
MSE.matrix <- matrix(nrow = num.predictors, ncol = num.folds)


for (k in 1:num.folds){
  #assign a tenth of rows to test set
  test.set <- mydf[which(fold.indices == k),]
  #rest of the rows go to train set
  train.set <- setdiff(test.set, mydf)
  #get the models for each number of predictors using only the train set
  k.fold.reg <- regsubsets(gname ~ ., data = train.set, nvmax = ncol(gt_new),method = "exhaustive")
  test.mat <- model.matrix(gname~.,data=test.set)
  
  #iterate through all models created on kth fold
  for (i in 1:num.predictors){
    coefi <- coef(k.fold.reg, id=i)
    pred <- test.mat[,names(coefi)]%*%coefi
    MSE.matrix[i,k] <- mean((test.set$gname-pred)^2)
  }
}

#need to get the mean of MSEs of all the folds associated with each model
MSE.mean <- rep(0,num.predictors)
for (x in 1:num.predictors){
  MSE.mean[x] <- mean(MSE.matrix[x,]) 
}

#now we can graph this like the previous examples
MSE.mean
best.k.fold <- which.min(MSE.mean)

plot(x = 1:length(MSE.mean), y = MSE.mean, type = 'b', xlab = 'Num Predictors', ylab = 'MSE', main = " Cross Valdiation MSE")
points(x = c(best.k.fold), y = c(MSE.mean[best.k.fold]), pch = 19, col = 'red', cex = 1)

best.k.fold.sd <- which(MSE.mean < (sd(MSE.mean) + min(MSE.mean)))[1]
points(x = c(best.k.fold.sd), y = c(MSE.mean[best.k.fold.sd]), pch = 19, col = 'blue', cex = 1)

d

# We are going to start by running our models off of three variables and build to seven.

set.seed(5072)

oa_c <- rep(0,9)
oa_t.1 <- rep(0,9)
oa_t.2 <- rep(0,9)

head(gt_new, 4)
n <- length(gt_new$gname)
gt_new <- gt_new[sample(1:n, n),]
numfolds <- 10
fold.indices <- cut(1:n, breaks=numfolds, labels=FALSE)

#####################
#### Three ##########
#####################

variables <- c("country","INT_LOG","INT_IDEO")
plot_var <- c("c","t.1","t.2")

##############################
#### Logistic Regression #####
##############################

c <- rep(0,9)
t.1 <- rep(0,9)
t.2 <- rep(0,9)


dial <- c(.3,.35,.4,.45,.5,.55,.6,.65,.7)

a <- 1

while (a < 10){
    c_num <- rep(0,10)
    t.1_num <- rep(0,10)
    t.2_num <-rep(0,10)
  
  for(i in 1:numfolds){
    
    test <- which(fold.indices == i)
    train <- which(fold.indices != i)
    test.gname <- gt_new$gname[setdiff(1:n,train)]
    
    glm.fit <- glm(gname ~ country + INT_LOG + INT_IDEO, data=gt_new, family=binomial, subset = train)

    glm.probs <- predict(glm.fit, gt_new[test,][variables], type="response")

    glm.pred <- rep("0", length(test)) 
    glm.pred[glm.probs>dial[a]] <- "1"

    tab <- table(test.gname, glm.pred)
  
    stat_LR <- perform_stat(tab)
    
    c_num[i] <- stat_LR$c
    t.1_num[i] <- stat_LR$t.1
    t.2_num[i] <- stat_LR$t.2
  }
    
  c[a] <- mean(c_num)
  t.1[a] <- mean(t.1_num)
  t.2[a] <- mean(t.2_num)

  a <- a + 1
}

Log_Reg_3 <- data.frame(cbind(c,t.1,t.2),row.names = dial)
matplot(Log_Reg_3[plot_var], type = c("b"),pch=1,col = 1:4 ,ylab = "Percentage", xlab = "Dial", sub = "Black: Correct, Red: Type I, Green: Type II", main = "Impact of different dial settings on Log Reg Pred")

oa_c[1] <- c[which.min(t.2)]
oa_t.1[1] <- t.1[which.min(t.2)]
oa_t.2[1] <- t.2[which.min(t.2)]

##############################
#### LDA #####################
##############################

c_num <- rep(0,10)
t.1_num <- rep(0,10)
t.2_num <-rep(0,10)

for(i in 1:numfolds){
  
  test <- which(fold.indices == i)
  train <- which(fold.indices != i)
  test.gname <- gt_new$gname[setdiff(1:n,train)]
  
  lda.fit <- lda(gname ~ country + INT_LOG + INT_IDEO, data=gt_new, family=binomial, subset = train)

  lda.pred <- predict(lda.fit, gt_new[test,][variables])

  tab <- table(test.gname,lda.pred$class)

  stat_LDA <- perform_stat(tab)
  
  c_num[i] <- stat_LDA$c
  t.1_num[i] <- stat_LDA$t.1
  t.2_num[i] <- stat_LDA$t.2
  
}

oa_c[2] <- mean(c_num)
oa_t.1[2] <- mean(t.1_num)
oa_t.2[2] <- mean(t.2_num)

##############################
#### QDA #####################
##############################

c_num <- rep(0,10)
t.1_num <- rep(0,10)
t.2_num <-rep(0,10)

for(i in 1:numfolds){
  
  test <- which(fold.indices == i)
  train <- which(fold.indices != i)
  test.gname <- gt_new$gname[setdiff(1:n,train)]
  
  qda.fit <- qda(gname ~ country + INT_LOG + INT_IDEO, data=gt_new, family=binomial, subset = train)

  qda.pred <- predict(qda.fit, gt_new[test,][variables])$class

  tab <- table(test.gname, qda.pred)

  stat_QDA <- perform_stat(tab)

  c_num[i] <- stat_QDA$c
  t.1_num[i] <- stat_QDA$t.1
  t.2_num[i] <- stat_QDA$t.2

}

oa_c[3] <- mean(c_num)
oa_t.1[3] <- mean(t.1_num)
oa_t.2[3] <- mean(t.2_num)

#####################
#### Four ###########
#####################

variables <- c("country","INT_LOG","INT_IDEO","natlty1")
plot_var <- c("c","t.1","t.2")

##############################
#### Logistic Regression #####
##############################

c <- rep(0,9)
t.1 <- rep(0,9)
t.2 <- rep(0,9)


dial <- c(.3,.35,.4,.45,.5,.55,.6,.65,.7)

a <- 1

while (a < 10){
  c_num <- rep(0,10)
  t.1_num <- rep(0,10)
  t.2_num <-rep(0,10)
  
  for(i in 1:numfolds){
    
    test <- which(fold.indices == i)
    train <- which(fold.indices != i)
    test.gname <- gt_new$gname[setdiff(1:n,train)]
    
    glm.fit <- glm(gname ~ country + INT_LOG + INT_IDEO + natlty1, data=gt_new, family=binomial, subset = train)
    
    glm.probs <- predict(glm.fit, gt_new[test,][variables], type="response")
    
    glm.pred <- rep("0", length(test)) 
    glm.pred[glm.probs>dial[a]] <- "1"
    
    tab <- table(test.gname, glm.pred)
    
    stat_LR <- perform_stat(tab)
    
    c_num[i] <- stat_LR$c
    t.1_num[i] <- stat_LR$t.1
    t.2_num[i] <- stat_LR$t.2
  }
  
  c[a] <- mean(c_num)
  t.1[a] <- mean(t.1_num)
  t.2[a] <- mean(t.2_num)
  
  a <- a + 1
}

Log_Reg_3 <- data.frame(cbind(c,t.1,t.2),row.names = dial)
matplot(Log_Reg_3[plot_var], type = c("b"),pch=1,col = 1:4 ,ylab = "Percentage", xlab = "Dial", sub = "Black: Correct, Red: Type I, Green: Type II", main = "Impact of different dial settings on Log Reg Pred")

oa_c[4] <- c[which.min(t.2)]
oa_t.1[4] <- t.1[which.min(t.2)]
oa_t.2[4] <- t.2[which.min(t.2)]

##############################
#### LDA #####################
##############################

c_num <- rep(0,10)
t.1_num <- rep(0,10)
t.2_num <-rep(0,10)

for(i in 1:numfolds){
  
  test <- which(fold.indices == i)
  train <- which(fold.indices != i)
  test.gname <- gt_new$gname[setdiff(1:n,train)]
  
  lda.fit <- lda(gname ~ country + INT_LOG + INT_IDEO + natlty1, data=gt_new, family=binomial, subset = train)
  
  lda.pred <- predict(lda.fit, gt_new[test,][variables])
  
  tab <- table(test.gname,lda.pred$class)
  
  stat_LDA <- perform_stat(tab)
  
  c_num[i] <- stat_LDA$c
  t.1_num[i] <- stat_LDA$t.1
  t.2_num[i] <- stat_LDA$t.2
  
}

oa_c[5] <- mean(c_num)
oa_t.1[5] <- mean(t.1_num)
oa_t.2[5] <- mean(t.2_num)

##############################
#### QDA #####################
##############################

c_num <- rep(0,10)
t.1_num <- rep(0,10)
t.2_num <-rep(0,10)

for(i in 1:numfolds){
  
  test <- which(fold.indices == i)
  train <- which(fold.indices != i)
  test.gname <- gt_new$gname[setdiff(1:n,train)]
  
  qda.fit <- qda(gname ~ country + INT_LOG + INT_IDEO + natlty1, data=gt_new, family=binomial, subset = train)
  
  qda.pred <- predict(qda.fit, gt_new[test,][variables])$class
  
  tab <- table(test.gname, qda.pred)
  
  stat_QDA <- perform_stat(tab)
  
  c_num[i] <- stat_QDA$c
  t.1_num[i] <- stat_QDA$t.1
  t.2_num[i] <- stat_QDA$t.2
  
}

oa_c[6] <- mean(c_num)
oa_t.1[6] <- mean(t.1_num)
oa_t.2[6] <- mean(t.2_num)

#####################
#### Five ###########
#####################

variables <- c("country","INT_LOG","INT_IDEO","natlty1", "suicide")
plot_var <- c("c","t.1","t.2")

##############################
#### Logistic Regression #####
##############################

c <- rep(0,9)
t.1 <- rep(0,9)
t.2 <- rep(0,9)


dial <- c(.3,.35,.4,.45,.5,.55,.6,.65,.7)

a <- 1

while (a < 10){
  c_num <- rep(0,10)
  t.1_num <- rep(0,10)
  t.2_num <-rep(0,10)
  
  for(i in 1:numfolds){
    
    test <- which(fold.indices == i)
    train <- which(fold.indices != i)
    test.gname <- gt_new$gname[setdiff(1:n,train)]
    
    glm.fit <- glm(gname ~ country + INT_LOG + INT_IDEO + natlty1 + suicide, data=gt_new, family=binomial, subset = train)
    
    glm.probs <- predict(glm.fit, gt_new[test,][variables], type="response")
    
    glm.pred <- rep("0", length(test)) 
    glm.pred[glm.probs>dial[a]] <- "1"
    
    tab <- table(test.gname, glm.pred)
    
    stat_LR <- perform_stat(tab)
    
    c_num[i] <- stat_LR$c
    t.1_num[i] <- stat_LR$t.1
    t.2_num[i] <- stat_LR$t.2
  }
  
  c[a] <- mean(c_num)
  t.1[a] <- mean(t.1_num)
  t.2[a] <- mean(t.2_num)
  
  a <- a + 1
}

Log_Reg_3 <- data.frame(cbind(c,t.1,t.2),row.names = dial)
matplot(Log_Reg_3[plot_var], type = c("b"),pch=1,col = 1:4 ,ylab = "Percentage", xlab = "Dial", sub = "Black: Correct, Red: Type I, Green: Type II", main = "Impact of different dial settings on Log Reg Pred")

oa_c[7] <- c[which.min(t.2)]
oa_t.1[7] <- t.1[which.min(t.2)]
oa_t.2[7] <- t.2[which.min(t.2)]

##############################
#### LDA #####################
##############################

c_num <- rep(0,10)
t.1_num <- rep(0,10)
t.2_num <-rep(0,10)

for(i in 1:numfolds){
  
  test <- which(fold.indices == i)
  train <- which(fold.indices != i)
  test.gname <- gt_new$gname[setdiff(1:n,train)]
  
  lda.fit <- lda(gname ~ country + INT_LOG + INT_IDEO + natlty1 + suicide, data=gt_new, family=binomial, subset = train)
  
  lda.pred <- predict(lda.fit, gt_new[test,][variables])
  
  tab <- table(test.gname,lda.pred$class)
  
  stat_LDA <- perform_stat(tab)
  
  c_num[i] <- stat_LDA$c
  t.1_num[i] <- stat_LDA$t.1
  t.2_num[i] <- stat_LDA$t.2
  
}

oa_c[8] <- mean(c_num)
oa_t.1[8] <- mean(t.1_num)
oa_t.2[8] <- mean(t.2_num)

##############################
#### QDA #####################
##############################

c_num <- rep(0,10)
t.1_num <- rep(0,10)
t.2_num <-rep(0,10)

for(i in 1:numfolds){
  
  test <- which(fold.indices == i)
  train <- which(fold.indices != i)
  test.gname <- gt_new$gname[setdiff(1:n,train)]
  
  qda.fit <- qda(gname ~ country + INT_LOG + INT_IDEO + natlty1 + suicide, data=gt_new, family=binomial, subset = train)
  
  qda.pred <- predict(qda.fit, gt_new[test,][variables])$class
  
  tab <- table(test.gname, qda.pred)
  
  stat_QDA <- perform_stat(tab)
  
  c_num[i] <- stat_QDA$c
  t.1_num[i] <- stat_QDA$t.1
  t.2_num[i] <- stat_QDA$t.2
  
}

oa_c[9] <- mean(c_num)
oa_t.1[9] <- mean(t.1_num)
oa_t.2[9] <- mean(t.2_num)

##############################
#### Final ###################
##############################

subset_results <- cbind(oa_c, oa_t.1, oa_t.2)
colnames(subset_results) <- c("Correctness","Type I","Type II")
rownames(subset_results) <- c("LogR(3)","LDA(3)","QDA(3)","LogR(4)","LDA(4)","QDA(4)","LogR(5)","LDA(5)","QDA(5)")
subset_results

################################################################
################## Lasso Decision ##############################
################################################################

set.seed(5072)

x <- model.matrix(gname ~ ., data = gt_new)[,-1]
y <- gt_new$gname

grid=10^seq(10,-2,length=100)

set.seed(5072)

train=sample(1:nrow(x), nrow(x)*0.5)
test=(-train)
y.test=y[test]

lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
cv.out.lasso = cv.glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(cv.out.lasso)
bestlam.lasso=cv.out.lasso$lambda.min
lasso.pred <- predict(lasso.mod,newx=x[test,],s=bestlam.lasso)
lasso.coef=predict(lasso.mod,type="coefficients",s=bestlam.lasso)[1:14,]
lasso.coef

######################################
#### Lasso Subset Selection ##########
######################################

set.seed(5072)

oa_c <- rep(0,3)
oa_t.1 <- rep(0,3)
oa_t.2 <- rep(0,3)

n <- length(gt_new$gname)
gt_new <- gt_new[sample(1:n, n),]
numfolds <- 10
fold.indices <- cut(1:n, breaks=numfolds, labels=FALSE)

######################################
#### Lasso Variable Selection ########
######################################

variables <- c("extended","country","region","multiple","suicide","targtype1")
plot_var <- c("c","t.1","t.2")

##############################
#### Logistic Regression #####
##############################

c <- rep(0,9)
t.1 <- rep(0,9)
t.2 <- rep(0,9)


dial <- c(.3,.35,.4,.45,.5,.55,.6,.65,.7)

a <- 1

while (a < 10){
  c_num <- rep(0,10)
  t.1_num <- rep(0,10)
  t.2_num <-rep(0,10)
  
  for(i in 1:numfolds){
    
    test <- which(fold.indices == i)
    train <- which(fold.indices != i)
    test.gname <- gt_new$gname[setdiff(1:n,train)]
    
    glm.fit <- glm(gname ~ extended + country + region + multiple + suicide + targtype1, data = gt_new, family=binomial, subset = train)
    
    glm.probs <- predict(glm.fit, gt_new[test,][variables], type="response")
    
    glm.pred <- rep("0", length(test)) 
    glm.pred[glm.probs>dial[a]] <- "1"
    
    tab <- table(test.gname, glm.pred)
    
    stat_LR <- perform_stat(tab)
    
    c_num[i] <- stat_LR$c
    t.1_num[i] <- stat_LR$t.1
    t.2_num[i] <- stat_LR$t.2
  }
  
  c[a] <- mean(c_num)
  t.1[a] <- mean(t.1_num)
  t.2[a] <- mean(t.2_num)
  
  a <- a + 1
}

Log_Reg_3 <- data.frame(cbind(c,t.1,t.2),row.names = dial)
matplot(Log_Reg_3[plot_var], type = c("b"),pch=1,col = 1:4 ,ylab = "Percentage", xlab = "Dial", sub = "Black: Correct, Red: Type I, Green: Type II", main = "Impact of different dial settings on Log Reg Pred")

oa_c[1] <- c[which.min(t.2)]
oa_t.1[1] <- t.1[which.min(t.2)]
oa_t.2[1] <- t.2[which.min(t.2)]

##############################
#### LDA #####################
##############################

c_num <- rep(0,10)
t.1_num <- rep(0,10)
t.2_num <-rep(0,10)

for(i in 1:numfolds){
  
  test <- which(fold.indices == i)
  train <- which(fold.indices != i)
  test.gname <- gt_new$gname[setdiff(1:n,train)]
  
  lda.fit <- lda(gname ~ extended + country + region + multiple + suicide + targtype1, data = gt_new, family=binomial, subset = train)
  
  lda.pred <- predict(lda.fit, gt_new[test,][variables])
  
  tab <- table(test.gname,lda.pred$class)
  
  stat_LDA <- perform_stat(tab)
  
  c_num[i] <- stat_LDA$c
  t.1_num[i] <- stat_LDA$t.1
  t.2_num[i] <- stat_LDA$t.2
  
}

oa_c[2] <- mean(c_num)
oa_t.1[2] <- mean(t.1_num)
oa_t.2[2] <- mean(t.2_num)

##############################
#### QDA #####################
##############################

c_num <- rep(0,10)
t.1_num <- rep(0,10)
t.2_num <-rep(0,10)

for(i in 1:numfolds){
  
  test <- which(fold.indices == i)
  train <- which(fold.indices != i)
  test.gname <- gt_new$gname[setdiff(1:n,train)]
  
  qda.fit <- qda(gname ~ extended + country + region + multiple + suicide + targtype1, data = gt_new, family=binomial, subset = train)
  
  qda.pred <- predict(qda.fit, gt_new[test,][variables])$class
  
  tab <- table(test.gname, qda.pred)
  
  stat_QDA <- perform_stat(tab)
  
  c_num[i] <- stat_QDA$c
  t.1_num[i] <- stat_QDA$t.1
  t.2_num[i] <- stat_QDA$t.2
  
}

oa_c[3] <- mean(c_num)
oa_t.1[3] <- mean(t.1_num)
oa_t.2[3] <- mean(t.2_num)

##############################
#### Final ###################
##############################

lasso_results <- cbind(oa_c, oa_t.1, oa_t.2)
colnames(lasso_results) <- c("Correctness","Type I","Type II")
rownames(lasso_results) <- c("LogR","LDA","QDA")
lasso_results

################################################################
################## PCR #########################################
################################################################

set.seed(5072)

plot_var <- c("c","t.1","t.2")

n <- length(gt_new$gname)
gt_new <- gt_new[sample(1:n, n),]
numfolds <- 10
fold.indices <- cut(1:n, breaks=numfolds, labels=FALSE)

c <- rep(0,9)
t.1 <- rep(0,9)
t.2 <- rep(0,9)

dial <- c(.3,.35,.4,.45,.5,.55,.6,.65,.7)

a <- 1

while (a < 10){
  c_num <- rep(0,10)
  t.1_num <- rep(0,10)
  t.2_num <-rep(0,10)
  
  for(i in 1:numfolds){
    
    test <- which(fold.indices == i)
    train <- which(fold.indices != i)
    test.gname <- gt_new$gname[setdiff(1:n,train)]
    
    pcr.fit <- pcr(gname ~ ., data=gt_new, subset = train, scale=F, validation="CV") 
    
    n.pcs  <-  3   
    pcr.probs <- predict(pcr.fit, gt_new[test,], ncomp=n.pcs, type='response')
    
    pcr.pred <- rep("0", length(test)) 
    pcr.pred[pcr.probs>dial[a]] <- "1"
    
    tab <- table(test.gname, pcr.pred)
    
    stat_LR <- perform_stat(tab)
    
    c_num[i] <- stat_LR$c
    t.1_num[i] <- stat_LR$t.1
    t.2_num[i] <- stat_LR$t.2
  }
  
  c[a] <- mean(c_num)
  t.1[a] <- mean(t.1_num)
  t.2[a] <- mean(t.2_num)
  
  a <- a + 1
}

Log_Reg_3 <- data.frame(cbind(c,t.1,t.2),row.names = dial)
matplot(Log_Reg_3[plot_var], type = c("b"),pch=1,col = 1:4 ,ylab = "Percentage", xlab = "Dial", sub = "Black: Correct, Red: Type I, Green: Type II", main = "Impact of different dial settings on Log Reg Pred")

oa_c <- c[which.min(t.2)]
oa_t.1 <- t.1[which.min(t.2)]
oa_t.2 <- t.2[which.min(t.2)]

##############################
#### Final ###################
##############################

pcr_results <- cbind(oa_c, oa_t.1, oa_t.2)
colnames(pcr_results) <- c("Correctness","Type I","Type II")
rownames(pcr_results) <- c("LogR")
pcr_results

################################################################
################## Final Understanding #########################
################################################################

subset_results
lasso_results
pcr_results
