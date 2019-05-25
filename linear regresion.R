library(ISLR)
ISLR::Hitters
A = data.frame(iris)
sf = sample(2,nrow(A),replace=TRUE,prob= c(0.7,0.3))
trd = A [sf == 1,]
table(trd$Species)
tsd = A[sf == 2,]
model1 = lm(Sepal.Length ~ .,data = trd)
model2 = lm(Sepal.Length~Petal.Length + Petal.Width + Species,data = trd)
predict(model1,tsd)
str(A)
tbp = A[1:2,2:5]
predict = (model1,tsd)
pred = predict(model1,trd)

cbind(pred,tsd$Sepal.Length)




