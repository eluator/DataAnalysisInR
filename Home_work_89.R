Boston=read.table("~/Загрузки/Книги/Программирование/Теорвер/Восьмое/01_Boston housing/housing1.data", dec = ".", sep = ",", header = T)
Boston[1:5,]
class(Boston[,14])
Boston[1:5,14]
Boston$MEDV=as.factor(ifelse(Boston$MEDV>mean(Boston$MEDV),"богатые","бедные"))
Boston[1:5,14]
set.seed(1234)
test.num <- sample(1:nrow(Boston), 100, replace = FALSE) 
learn=Boston[-test.num,1:13]
test=Boston[test.num,1:13]
cl=Boston[-test.num,14]
library(class)
a=c(1:15)
for(i in 1:15){
  a[i]=sum(knn(learn,test,cl, k=i,prob = T)!=Boston[test.num,14])
}
a
KNN=knn(learn,learn,cl, k=3,prob = T)
t=table(KNN, Boston[-test.num, 14])
t
table(Boston[test.num, 14])
(sum(t)-sum(diag(t)))*100/sum(t)
KNN=knn(learn,test,cl, k=3,prob = T)
t=table(KNN, Boston[test.num, 14])
t
table(Boston[test.num, 14])
(sum(t)-sum(diag(t)))*100/sum(t)

library(rpart)
str(Boston)
Boston.res=rpart(MEDV~.,
                 data = Boston[-test.num,], method="class",
                 control=rpart.control(minsplit=10, minbucket=5, maxdepth=6))
print(Boston.res, digits = 2)
#plot(Boston.res)
#text(Boston.res, use.n=T)
library(rpart.plot)
rpart.plot(Boston.res, type=2, extra = 1)

head(predict(Boston.res, Boston[ , -14], type="class"))
head(predict(Boston.res, Boston[ , -14])[ , 2])
t=table(Boston[-test.num , 14], predict(Boston.res, Boston[-test.num,-14], type="class"))
table(Boston[test.num , 14])
(sum(t)-sum(diag(t)))*100/sum(t)
t=table(Boston[test.num , 14], predict(Boston.res, Boston[test.num,-14], type="class"))
table(Boston[test.num , 14])
(sum(t)-sum(diag(t)))*100/sum(t)


predict(Boston.res, Boston[ , -15], type="class")
predict(Wine.res, Wine[ , -15])[ , 2]
# ??????? ?????????? ?? learn
table(Boston[ , 15], predict(Boston.res, Boston[ , -15], type="class"))




Boston=read.table("~/Загрузки/Книги/Программирование/Теорвер/Восьмое/01_Boston housing/housing1.data", dec = ".", sep = ",", header = T)
Boston[1:5,]
class(Boston[,14])
Boston[1:5,14]
Boston$MEDV=as.factor(ifelse(Boston$MEDV>mean(Boston$MEDV),"богатые","бедные"))

names(Boston)=c("преступность","разреженность_населения","торговля", "река","загазованность","комнаты","старые","расстояние_1","расстояние_2","налог", "ученики","афроамериканцы", "соц_статус","цена")
library(randomForest)

test.num <- sample(1:nrow(Boston), 100, replace = FALSE) 
test <- Boston[test.num, 1:14]
train <- Boston[-test.num, 1:14]
x <- train[, 1:13]
x.test <- test[ ,1:13]
y <- train[, 14]
y.test <- test[, 14]
y.1 <- as.factor(y)
n=30
nodesize1=35
max=10
rf <- randomForest(x, y=y.1, ntree=n, mtry=floor(sqrt(ncol(train))),
                   replace=TRUE, nodesize = nodesize1, maxnodes = max,
                   importance=TRUE, localImp=FALSE,
                   proximity=FALSE, norm.votes=TRUE, do.trace=n/10,
                   keep.forest=TRUE, corr.bias=FALSE, keep.inbag=FALSE)
train.predict=predict(rf,train)
t=table(y,train.predict)
t
(sum(t)-sum(diag(t)))*100/sum(t)
test.predict=predict(rf,test)
t=table(y.test,test.predict)
t
(sum(t)-sum(diag(t)))*100/sum(t)
varImpPlot(rf, sort=F)
library(gbm)
set.seed(3217)
table(Boston$цена)
ntree.1 <- 80
nodesize.1 <-10

gbm.res <- gbm(цена~. , data=train, 
               distribution="gaussian", 
               n.trees=ntree.1,
               shrinkage=0.05, 
               interaction.depth=5,
               bag.fraction = 0.66,
               n.minobsinnode = nodesize.1,
               cv.folds = 0, 
               keep.data=TRUE, 
               verbose=TRUE) 
data.predict <- predict(gbm.res, newdata = train[,1:13], n.trees = ntree.1) 
data.predict[1:11] 

data.pr.2 <- rep(0, nrow(train))
data.pr.2[(data.predict > 0.5)& (data.predict < 1.5)] <- 1 
data.pr.2[data.predict > 1.5] <- 2
t=table(train$цена, data.pr.2)
t
(sum(t)-sum(diag(t)))*100/sum(t)
data.predict <- predict(gbm.res, newdata = test[,1:13], n.trees = ntree.1) 
data.predict[1:11] 

data.pr.2 <- rep(0, nrow(test))
data.pr.2[(data.predict > 0.5)& (data.predict < 1.5)] <- 1 
data.pr.2[data.predict > 1.5] <- 2
t=table(test$цена, data.pr.2)
t
(sum(t)-sum(diag(t)))*100/sum(t)