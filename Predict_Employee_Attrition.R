getwd()
setwd("C:/Texas A&M Semester/613")
IBM=read.csv("IBM_HR_attrition.csv", header=T, na.strings = "na")
#View(IBM)
row.has.na <- apply(IBM, 1, function(x){any(is.na(x))}) 
sum(row.has.na)

#####Check factor####
IBM$EnvironmentSatisfaction=factor(IBM$EnvironmentSatisfaction, levels = c("1", "2","3","4"))
IBM$JobSatisfaction <-  factor(IBM$JobSatisfaction, levels = c("1", "2","3","4"))
IBM$WorkLifeBalance <-  factor(IBM$WorkLifeBalance, levels = c("1", "2","3","4"))
IBM$StockOptionLevel <-  factor(IBM$StockOptionLevel, levels = c("0","1", "2","3"))
IBM$JobInvolvement=factor(IBM$JobInvolvement, levels=c("1", "2","3","4"))
IBM$Education <-  factor(IBM$Education, levels = c("1", "2","3","4","5"))
IBM$RelationshipSatisfaction <-  factor(IBM$RelationshipSatisfaction, levels = c("1", "2","3","4"))
IBM$PerformanceRating=factor(IBM$PerformanceRating, levels=c("1","2","3","4"))
IBM$JobLevel <-  factor(IBM$JobLevel, levels = c("1", "2","3","4","5"))

####SMOTE####
set.seed(3)
require(DMwR)
library(DMwR)
IBM=SMOTE(Attrition~., data=IBM, perc.over = 200, perc.under = 200, k=3)
#fix(IBM)
#View(IBM)
table(IBM$Attrition)
dim(IBM)

row.has.na <- apply(IBM, 1, function(x){any(is.na(x))}) 
sum(row.has.na)

####Data Cleaning####
#Factorizing string columns 
IBM$Attrition <-  factor(IBM$Attrition, levels= c("No","Yes"),labels = c("0", "1"))
IBM$BusinessTravel <-  factor(IBM$BusinessTravel, levels = c("Non-Travel", "Travel_Rarely","Travel_Frequently"), labels=c("1","2","3"),ordered="TRUE")
IBM$Department <-  factor(IBM$Department, levels = c("Human Resources", "Research & Development","Sales"), labels=c("1","2","3"),ordered="FALSE")
IBM$EducationField <-  factor(IBM$EducationField, levels = c("Human Resources", "Life Sciences","Marketing","Medical","Other","Technical Degree"), labels=c("1","2","3","4","5","6"),ordered="FALSE")
IBM$JobRole <-  factor(IBM$JobRole, levels = c("Healthcare Representative", "Human Resources","Laboratory Technician","Manager","Manufacturing Director","Research Director","Research Scientist","Sales Executive","Sales Representative"), labels=c("1","2","3","4","5","6","7","8","9"),ordered="FALSE")
IBM$MaritalStatus <-  factor(IBM$MaritalStatus, levels = c("Divorced", "Married","Single"), labels=c("1","2","3"),ordered="FALSE")
IBM$OverTime <-  factor(IBM$OverTime, levels = c("No", "Yes"), labels=c("0","1") ,ordered="FALSE")
IBM$Gender<- factor(IBM$Gender, levels=  c("Male","Female"), labels=c("0","1"), ordered="FALSE")

##Removing redundant rows
IBM$EmployeeCount <- NULL
IBM$Over18 <- NULL
IBM$StandardHours <- NULL
IBM$EmployeeNumber<-NULL

row.has.na <- apply(IBM, 1, function(x){any(is.na(x))}) 
sum(row.has.na)

str(IBM)

##Converting Numerical Values into factors for MBA and NN
IBM$�..Age=cut(IBM$�..Age,breaks = c(18, 30, 36, 40, 61), labels = c("0", "1", "2", "3"),right = FALSE, ordered_result = "TRUE")
is.factor(IBM$�..Age)

summary(IBM$DistanceFromHome)
IBM$DistanceFromHome=cut(IBM$DistanceFromHome,breaks = c(1, 3, 8, 14, 30), labels = c("0", "1", "2", "3"),right = FALSE, ordered_result = "TRUE")
is.factor(IBM$DistanceFromHome)

summary(IBM$DailyRate)
IBM$DailyRate=cut(IBM$DailyRate,breaks = c(102, 452, 811.6, 1158.1, 1500), labels = c("0", "1", "2", "3"),right = FALSE, ordered_result = "TRUE")
is.factor(IBM$DailyRate)

summary(IBM$HourlyRate)
IBM$HourlyRate=cut(IBM$HourlyRate,breaks = c(30, 51.19, 67.33, 83.6, 101), labels = c("0", "1", "2", "3"),right = FALSE, ordered_result = "TRUE")

summary(IBM$MonthlyIncome)
IBM$MonthlyIncome=cut(IBM$MonthlyIncome,breaks = c(1009, 2598, 4014 ,6634 , 20000), labels = c("0", "1", "2", "3"),right = FALSE, ordered_result = "TRUE")

summary(IBM$MonthlyRate)
IBM$MonthlyRate=cut(IBM$MonthlyRate,breaks = c(2094, 7739,12858 , 19179 , 27000), labels = c("0", "1", "2", "3"),right = FALSE, ordered_result = "TRUE")

summary(IBM$NumCompaniesWorked)
IBM$NumCompaniesWorked=cut(IBM$NumCompaniesWorked,breaks = c(0,1,2.9 ,5 ,10), labels = c("0", "1", "2", "3"),right = FALSE, ordered_result = "TRUE")

summary(IBM$PercentSalaryHike)
IBM$PercentSalaryHike=cut(IBM$PercentSalaryHike,breaks = c(11, 13,14.27 ,18 ,26), labels = c("0", "1", "2", "3"),right = FALSE, ordered_result = "TRUE")

summary(IBM$YearsWithCurrManager)
IBM$YearsWithCurrManager=cut(IBM$YearsWithCurrManager,breaks = c(0, 1.1,2.7 ,6.9 ,18), labels = c("0", "1", "2", "3"),right = FALSE, ordered_result = "TRUE")

summary(IBM$YearsSinceLastPromotion)
IBM$YearsSinceLastPromotion=cut(IBM$YearsSinceLastPromotion,breaks = c(0, 1,3,16), labels = c("0", "1", "2"),right = FALSE, ordered_result = "TRUE")

summary(IBM$YearsInCurrentRole)
IBM$YearsInCurrentRole=cut(IBM$YearsInCurrentRole,breaks = c(0, 1.45,2.25 ,7 ,19), labels = c("0", "1", "2", "3"),right = FALSE, ordered_result = "TRUE")

summary(IBM$YearsAtCompany)
IBM$YearsAtCompany=cut(IBM$YearsAtCompany,breaks = c(0, 2,5 ,8.27 ,41), labels = c("0", "1", "2", "3"),right = FALSE, ordered_result = "TRUE")

summary(IBM$TotalWorkingYears)
IBM$TotalWorkingYears=cut(IBM$TotalWorkingYears,breaks = c(0, 6,8.565 ,12.145,41), labels = c("0", "1", "2", "3"),right = FALSE, ordered_result = "TRUE")

summary(IBM$TrainingTimesLastYear)
IBM$TrainingTimesLastYear=cut(IBM$TrainingTimesLastYear,breaks = c(0, 2,3.42,7), labels = c("0", "1", "2"),right = FALSE, ordered_result = "TRUE")

row.has.na <- apply(IBM, 1, function(x){any(is.na(x))}) 
sum(row.has.na)

str(IBM)



####Train and test####
set.seed(1)
train = sample (1: nrow(IBM), nrow(IBM)*.80)
IBM.train=IBM[train,]
IBM.test=IBM[-train,]

####randomForest####
library(randomForest)
set.seed(3)
#for(ntree in seq(100,500,50)){
#  for(j in c(4,5,6,7,8)){
#    rf.IBM=randomForest(Attrition~., data=IBM, subset=train, ntree=ntree, mtry=j, importance=TRUE )
#    print(rf.IBM)
#    yhat.rf=predict(rf.IBM, IBM.test)
#    yhat.rf
#    print(mean(yhat.rf==IBM.test$Attrition))
#  }
#}


#The best accuracy was found for ntree=200 & mtry=4  
set.seed(3)
rf.IBM=randomForest(Attrition~., data=IBM, subset=train,ntree=200, mtry=7, importance=TRUE)
rf.IBM
importance(rf.IBM)
varImpPlot(rf.IBM)
yhat=predict(rf.IBM, IBM.test)
mean(yhat!=IBM.test$Attrition)

#Results of Random Forest suggest that JobRole, OverTime, StockOptionLevel, MaritalStatus are important predictors
#####ROC for RF####
install.packages("pROC")
library(pROC)
yhat.rf=predict(rf.IBM, IBM.test)
a= as.numeric(yhat.rf)
roc(IBM.test$Attrition, a, plot = TRUE)

#####GBM#####
library(gbm)
par(mfrow =c(1,1))
set.seed (2)
boost.IBM=gbm(Attrition~., data=IBM.train, n.trees=1000, shrinkage = .2, interaction.depth =1, distribution = "multinomial" )
boost.IBM
summary(boost.IBM)

#Results of boosting suggest JobRole, EducationField, StockOptionLevel & Overtime are the most influencing predictors


####Logistic####
set.seed(1)
glm.fit=glm(Attrition~JobRole+StockOptionLevel+OverTime+MaritalStatus ,data=IBM ,family=binomial, subset = train)
glm.probs=predict(glm.fit ,IBM.test, type ="response")
a=nrow(IBM.test)
glm.pred=rep(0 , a)
glm.pred[glm.probs>.5]="1"
table(glm.pred ,IBM.test$Attrition)
mean(glm.pred!= IBM.test$Attrition )
#####CV for Logistic####
library(boot)
set.seed(15)
cv.error=rep(0 ,10)
for (i in 1:10){
  glm.fit=glm(Attrition~JobRole+StockOptionLevel+OverTime+MaritalStatus  ,data=IBM ,family=binomial)
  cv.error[i]=cv.glm(IBM ,glm.fit ,K=10)$delta[1]
}
cv.error
#####ROC for Logistic#####
library(pROC)
glm.fit=glm(Attrition~JobRole+StockOptionLevel+OverTime+MaritalStatus  ,data=IBM ,family=binomial, subset = train)
glm.probs=predict(glm.fit,IBM.test, type ="response")
q = roc(IBM.test$Attrition, glm.probs)
auc(q)
plot.roc(q)

####LDA####
library(MASS)
set.seed(6)
lda.IBM=lda(Attrition~JobRole+StockOptionLevel+OverTime+MaritalStatus  ,data=IBM , subset=train)
#lda.IBM
lda.pred=predict(lda.IBM , IBM.test )
lda.class=lda.pred$class
table(lda.class , IBM.test$Attrition)
mean(lda.class!=IBM.test$Attrition)

#####CV for LDA#####
cv.lda=lda(Attrition~JobRole+StockOptionLevel+OverTime+MaritalStatus  ,data=IBM , CV=TRUE,method="moment")
table(cv.lda$class, IBM$Attrition)
mean(cv.lda$class!=IBM$Attrition)

####ROC for LDA####
a=as.numeric(lda.class)
roccurve.lda = roc(IBM.test$Attrition,a)
plot(roccurve.lda, colorize = TRUE)
auc(roccurve.lda)


####QDA####
qda.IBM=qda(Attrition~JobRole+StockOptionLevel+OverTime+MaritalStatus ,data=IBM ,subset=train)
#qda.IBM
qda.pred=predict(qda.IBM , IBM.test )
qda.class=qda.pred$class
table(qda.class , IBM.test$Attrition)
mean(qda.class!=IBM.test$Attrition)

#####CV for QDA#####
cv.qda=qda(Attrition~JobRole+StockOptionLevel+OverTime+MaritalStatus  ,data=IBM , CV=TRUE,method="moment")
table(cv.qda$class, IBM$Attrition)#Calculate this manually
mean(cv.qda$class!=IBM$Attrition)
####ROC for QDA####
library(pROC)
a=as.numeric(qda.class)
b=as.numeric(IBM.test$Attrition)
roccurve.qda=roc(a,b)
plot(roccurve.qda, colorize = TRUE)
auc(roccurve.qda)

####KNN####
library(class)
library(knncat)
train.IBM=cbind(IBM.train$Overtime,IBM.train$MaritalStatus,IBM.train$JobRole,IBM.train$StockOptionLevel)
test.IBM=cbind(IBM.test$Overtime,IBM.test$MaritalStatus,IBM.test$JobRole,IBM.test$StockOptionLevel)
train.Attrition=IBM.train$Attrition
set.seed(8)
for(k in 1:10){
  knn.pred=knn(IBM.train, IBM.test, train.Attrition ,k=k)
  table(knn.pred, IBM.test$Attrition)
  print(k)
  print(mean(knn.pred==IBM.test$Attrition))
}
#Best model is at k=1 with 92.16%% accuracy
#ROC FOR KNN####
knn.pred=knn(IBM.train, IBM.test, train.Attrition, k=1)
x=as.numeric(knn.pred)
roc(IBM.test$Attrition,x,plot=TRUE)


####Classification Tree####
library(tree)
set.seed(3)
tree.IBM=tree(Attrition~JobRole+StockOptionLevel+OverTime+MaritalStatus, IBM, subset = train)
summary(tree.IBM)
plot(tree.IBM)
text(tree.IBM ,pretty =0)
tree.pred=predict(tree.IBM ,IBM.test ,type ="class")
table(tree.pred ,IBM.test$Attrition)
mean(tree.pred!=IBM.test$Attrition)

####CV for classification tree####
set.seed (3)
cv.IBM=cv.tree(tree.IBM ,FUN=prune.misclass )
cv.IBM
#THe lowest classification error rate(296) occurs for tree of size 10 & 7
par(mfrow =c(1,2))
plot(cv.IBM$size ,cv.IBM$dev ,type="b")
plot(cv.IBM$k ,cv.IBM$dev ,type="b")
prune.IBM=prune.misclass(tree.IBM ,best =7)
tree.pred=predict(prune.IBM , IBM.test ,type="class")
table(tree.pred ,IBM.test$Attrition)
mean(tree.pred!=IBM.test$Attrition)

#####ROC for CART####
u = as.numeric(tree.pred)
roc(IBM.test$Attrition, u, plot = TRUE)

####SVC####
library(e1071)
svmfit =svm(Attrition~JobRole+StockOptionLevel+OverTime+MaritalStatus, data=IBM.train , kernel="linear", cost =10,scale =TRUE )
tune.out=tune(svm ,Attrition~., data=IBM.train ,kernel ="linear", ranges =list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100) ))
bestmod =tune.out$best.model
summary(bestmod )
ypred=predict(bestmod, IBM.test)
table(predict=ypred, truth=IBM.test$Attrition)
mean(ypred!=IBM.test$Attrition)
#Best model has cost=10

####CV for SVC####
tuned=tune.svm(Attrition~JobRole+StockOptionLevel+OverTime+MaritalStatus, data=IBM , kernel="linear", cost = 10^(-2:2), scale=TRUE)
summary(tuned)
bestmod=tuned$best.model
summary(bestmod)


#####ROC for SVC#####
svm=svm(Attrition~JobRole+StockOptionLevel+OverTime+MaritalStatus, data=IBM.train , kernel="linear", cost =1, scale=TRUE)
svm.pred=predict(svm,IBM.test, decision.values = TRUE)
a=as.numeric(svm.pred)
q = roc(IBM.test$Attrition, a)
auc(q)
plot.roc(q)

#####SVM#####
library(e1071)
svmfit=svm(Attrition~JobRole+StockOptionLevel+OverTime+MaritalStatus, data=IBM.train, kernel ="radial", gamma =1, cost=1, scale=TRUE)
#plot(svmfit , IBM.train)
summary(svmfit)

#####CV for SVM####

tuned=tune.svm(Attrition~JobRole+StockOptionLevel+OverTime+MaritalStatus, data=IBM.train, kernel ="radial", gamma = 10^(-6:-1), cost = 10^(-2:2), scale=TRUE)
summary(tuned)
bestmod=tuned$best.model
summary(bestmod)
#best model at cost=100 and gamma=0.1
ypred=predict(bestmod, IBM.test)
table(predict=ypred, truth=IBM.test$Attrition)
mean(ypred!=IBM.test$Attrition)

####ROC for SVM####
svm=svm(Attrition~JobRole+StockOptionLevel+OverTime+MaritalStatus, data=IBM.train, kernel ="radial", gamma=0.1, cost =1, scale=TRUE)
svm.pred=predict(svm,IBM.test, decision.values = TRUE)
a=as.numeric(svm.pred)
q = roc(IBM.test$Attrition, a)
auc(q)
plot.roc(q)

####Deep Learning####
install.packages("h2o")
library(h2o)
h2o.init(nthread=-1)

data <- h2o.importFile(path = normalizePath("C:/Users/Neehar/Desktop/TAMU/Sem 1/613/Project/IBM_New.csv"))
summary(data)
str(data)
data$Attrition = h2o.asfactor(data$Attrition)
split <- h2o.splitFrame( data, c(0.80), seed = 1 )
train <- h2o.assign( split[[1]], "train" ) # 80%
test  <- h2o.assign( split[[2]], "test" )  # 20%
dim(test)
model <- h2o.deeplearning(   x=2:31,   y=1,   train, distribution = "bernoulli",
                             activation="Rectifier", nfolds=5, seed = 3,
                             hidden=c(60,30),   input_dropout_ratio=0.15, 
                             categorical_encoding = "OneHotInternal",
                             epochs=100,  variable_importances = TRUE )

dim(data)
model
head( h2o.varimp(model)) 
predictions <- h2o.predict(model, newdata=test)
pred=as.data.frame(predictions)
mean(predictions$predict != test$Attrition)
test_h2o = as.data.frame(test)

roc_auc <- function(probabilities,dataset)
{ #Command - roc_auc(probabilities,dataset) 
  #probabilities are those obtained from predict function #dataset is the actual data (0s and 1s) 
  library(ROCR) #Install ROCR library before running 
  pr=prediction(probabilities, dataset) 
  prf=performance(pr,measure = "tpr", x.measure = "fpr") 
  auc=performance(pr,measure = "auc") 
  auc=auc@y.values[[1]] 
  plot(prf,colorize=TRUE,main=paste("ROC curve with AUC=",auc)) }

roc_auc(pred[,3],test_h2o[,1])




####Market Basket Analysis####
##To find Association rules
library(arules)
?apriori
##

rules <- apriori(IBM,
                 parameter = list(minlen=5, supp=0.01, conf=0.2),
                 appearance = list(rhs=c("Attrition=1"),
                                   default="none",lhs=c("EnvironmentSatisfaction=1","EnvironmentSatisfaction=2","EnvironmentSatisfaction=3","EnvironmentSatisfaction=4","JobLevel=1","JobLevel=2","JobLevel=3","JobLevel=4","JobLevel=5","BusinessTravel=1","BusinessTravel=2","BusinessTravel=3", "JobSatisfaction=1","JobSatisfaction=2","JobSatisfaction=3","JobSatisfaction=4","YearsInCurrentRole=0","YearsInCurrentRole=1","YearsInCurrentRole=2","YearsInCurrentRole=3", "DailyRate=0","DailyRate=1","DailyRate=2","DailyRate=3")),control = list(verbose=F))
##Upto three decimal digits
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)
#Find_redundant_rules

superset.matrix <- is.subset(rules.sorted@lhs,y=NULL,sparse=FALSE)
superset <- rowSums(superset.matrix, na.rm=T) ==1
which(superset)
rules.pruned <- rules.sorted[superset]
inspect(rules.pruned)


##Plottingofgraph
library(arulesViz)
??arulesViz
##Explanation in the report has been provided with the graphs
plot(rules.pruned,method="graph")
plot(rules.pruned, method="paracoord", control=list(reorder=TRUE))
sel <- plot(rules.pruned, measure=c("support", "lift"), shading="lift", interactive=TRUE)
