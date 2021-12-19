#1 
library(scatterplot3d)
x = c(3.0, 6.0, 3.0, 6.0, 7.5, 7.5, 15.0)
u = c(10.0, 10.0, 20.0, 20.0, 5.0, 10.0, 12.0)
y = c(4.56, 5.9, 6.7, 8.02, 7.7, 8.1, 6.1)
s = scatterplot3d(x, u, y, xlim = 2:16, ylim = 8:22, zlim = 0:10, pch = 16, type = 'h')

m = lm(y ~ x+u)
coef(m) #y = 0.06137287x + 0.04610154u + 5.73189545

s$plane3d(m)
nx = c(7.5, 5.0)
nu = c(15.0, 12.0)
test1 = data.frame(x = nx, u = nu)
ny = predict(m, test1)
ny
s = scatterplot3d(nx, nu, ny, xlim = 2:16, ylim = 8:22, zlim = 0:10, pch = 16, type = 'h', color = 'red', angle = 60)
s$plane3d(m)


#5 
ucla = read.csv('https://stats.idre.ucla.edu/stat/data/binary.csv')
str(ucla)
ucla$admit = factor(ucla$admit) #factor형으로 변환 (0,1 분류)

n = nrow(ucla) #ucla 데이터 개수
i = 1:n
train_list = sample(i, n*0.6) #60%를 훈련집합으로 함
test_list = setdiff(i, train_list)#나머지(40%)는 테스트 집합
ucla_train = ucla[train_list, ]
ucla_test = ucla[test_list, ]

#학습은 훈련집합(ucla_train) 사용하고 
#predict는 테스트 집합(ucla_test) 사용

#결정트리
library(rpart)
r = rpart(admit~., data = ucla_train)
printcp(r)

par(mfcol = c(1,1), xpd = NA)
plot(r)
text(r, use.n = TRUE)

#결정 트리 
predict(r, newdata = ucla_test)
predicted_r = predict(r, newdata = ucla_test, type = 'class')
predicted_r

table(predicted_r, ucla_test$admit)

library(caret)
confusionMatrix(data = predicted_r, reference = ucla_test$admit)


#랜덤포레스트 (트리 50)
library(randomForest)
f_50 = randomForest(admit~., data = ucla_train, ntree = 50)
f_50

#랜덤포레스트 (트리 1,000)
f_1000 = randomForest(admit~., data = ucla_train, ntree = 1000)
f_1000


#랜덤포레스트 (트리 50)
p_50 = predict(f_50, newdata = ucla_test)
p_50
table(p_50, ucla_test$admit)

#랜덤포레스트 (트리 1000)
p_1000 = predict(f_1000, newdata = ucla_test)
p_1000
table(p_1000, ucla_test$admit)


confusionMatrix(data = p_50, reference = ucla_test$admit)
confusionMatrix(data = p_1000, reference = ucla_test$admit)

#k-NN
library(class)
k = knn(ucla_train, ucla_test, ucla_train$admit,k = 9)
k

#k-NN
table(k, ucla_test$admit)

#k-NN
confusionMatrix(data = k, reference = ucla_test$admit)


k = train(admit~., data = ucla_train, method = 'knn')
k

#SVM (radial basis)
library(e1071)
sr = svm(admit~., data=ucla_train) #radial basis
print(sr)

#SVM (polynomial)
sp = svm(admit~., data=ucla_train, kernel = 'polynomial')
print(sp)

#SVM (radial basis)
predicted_sr = predict(sr, newdata = ucla_test)
table(predicted_sr, ucla_test$admit)

#SVM (polynomial)
predicted_sp = predict(sp, ucla_test)
table(predicted_sp, ucla_test$admit)

confusionMatrix(data = predicted_sr, reference = ucla_test$admit)
confusionMatrix(data = predicted_sp, reference = ucla_test$admit)
