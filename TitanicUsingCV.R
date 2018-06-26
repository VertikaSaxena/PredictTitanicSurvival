require(ISLR)
require(boot)
train<-read.csv(file.choose())
View(train)
cv.glm
plot(Sex~Embarked, data=train)
plot(Pclass~Embarked, data=train)
glm.fit=glm(PassengerId~Survived,data=train)
cv.glm(train,glm.fit)$delta  


#TEST
require(ISLR)
require(boot)
?cv.glm
test<-read.csv(file.choose())
View(test)
plot(Pclass~Sex,data=test)
plot(Fare~Sex,data=test)
plot(Age~Sex,data=test)
plot(Sex~Embarked,data=test)
glm.fit=glm(PassengerId~Sex,data=test)
cv.glm(test,glm.fit)$delta 
loocv=function(fit)
{
  h=lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
}
loocv(glm.fit)

