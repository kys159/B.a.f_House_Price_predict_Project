rm(list=ls())
setwd("C:/Users/dudtj/Desktop/비어플")
final_1<-read.csv("real_final.csv",header=T)
final_1<-final_1[,-1]
str(final_1)

###factor로 변경
names(final_1)
final_1[,c(-1,-4,-5,-13,-17,-24,-28)]<-lapply(final_1[,c(-1,-4,-5,-13,-17,-24,-28)],factor)

###은태형부분
final_2<-read.csv("et.csv",header=T)
final_2<-final_2[,-1]
final_2$Fireplaces <- as.factor(final_2$Fireplaces)
final_2$Pool <- as.factor(final_2$Pool)
final_2$Fence <- as.factor(final_2$Fence)
final_2$GarageType<-as.factor(final_2$GarageType)
str(final_2)

###NA결측치 Unf으로 변경 
table(final_2$GarageFinish)
final_2$GarageFinish<-as.character(final_2$GarageFinish)
final_2$GarageFinish[is.na(final_2$GarageFinish)]<-"Unf"
final_2$GarageFinish<-as.factor(final_2$GarageFinish)

###chisq.test 결과 pool만 나왔었음 생략하겠음

###nearZeroVar
library(caret)
nearZeroVar(final_2,names=T)

###분산 0인 변수 제거		#KitchenAbvGr은 일단 냅뒀음
names(final_2)
final_2<-final_2[,c(-4,-6,-8,-10,-17)]

###합치기
data<-cbind(final_1,final_2)
str(data)

write.csv(data,"final.csv")

###stepwise
data<-data[,-1]
full <- lm(SalePrice~. , data=data)
slm <- step(full, direction='both')
par(mfrow=c(2,2))
plot(slm)

###다중공선성
library(car)
vif(slm)
#BsmtFinType1 제거
names(data)
data<-data[,-22]

###log변환
log_data<-data
log_data$SalePrice<-log(log_data$SalePrice)
str(log_data)
log_full <- lm(SalePrice~. , data=log_data)
log_slm <- step(log_full, direction='both')
par(mfrow=c(2,2))
plot(log_slm)

log_slm
summary(log_slm)

###524,1299,969 제거
log_data1<-log_data[-c(524,1299,969),]
log_full1<-lm(SalePrice~.,data=log_data1)
log_slm1<-step(log_full1,direction='both')
par(mfrow=c(2,2))
plot(log_slm1)
summary(log_slm1)
vif(log_slm1)

###fix_data는 스텝와이스 한 데이터
fix_data1<-log_slm1$model
str(fix_data1)

###HeatingQC,BsmtExposure, Foundation 모든 범주 p-value 높아 anova 분석 실시
###다중공선성 BsmtQual 9.88이라 애매 일단 놔둠
names(fix_data1) # 17, 15, 13
anova(lm(SalePrice~.,data=fix_data1),lm(SalePrice~.,data=fix_data1[,-13]))
anova(lm(SalePrice~.,data=fix_data1),lm(SalePrice~.,data=fix_data1[,-15]))
anova(lm(SalePrice~.,data=fix_data1),lm(SalePrice~.,data=fix_data1[,-17]))

###anova분석결과 foundation만 지움
###496,633,1325 제거
names(fix_data1)
fix_data2<-fix_data1[rownames(fix_data1)!="1325",-13]
fix_data2<-fix_data2[rownames(fix_data2)!="633",]
fix_data2<-fix_data2[rownames(fix_data2)!="496",]
str(fix_data2)

###바뀐 데이터 회귀분석 실시
log_full2<-lm(SalePrice~., data=fix_data2)
log_slm2<-step(log_full2,direction='both')
par(mfrow=c(2,2))
plot(log_slm2)
summary(log_slm2)
vif(log_slm2)

###다중공선성 없음
###관측치는 더이상 지우지 않겠음
###범주중에 한개만 p-value가 높은 변수 : Condition , HouseStyle , RoofStyle, PavedDrive
###모두 p-value가 높은 변수 : BsmtQual , BsmtExposure , HeatingQC
###Condition1 ~ 6 , 7 , 11 , 22
###BsmtQual ~ 13, 14 , 16
fix_data3<-log_slm2$model
names(fix_data3)

###anova 분석 결과 HouseStyle , RoofStyle 제거
anova(lm(SalePrice~.,data=fix_data3),lm(SalePrice~.,data=fix_data3[,-6]))
anova(lm(SalePrice~.,data=fix_data3),lm(SalePrice~.,data=fix_data3[,-7])) #제거
anova(lm(SalePrice~.,data=fix_data3),lm(SalePrice~.,data=fix_data3[,-11]))#제거
anova(lm(SalePrice~.,data=fix_data3),lm(SalePrice~.,data=fix_data3[,-22]))

anova(lm(SalePrice~.,data=fix_data3),lm(SalePrice~.,data=fix_data3[,-13]))
anova(lm(SalePrice~.,data=fix_data3),lm(SalePrice~.,data=fix_data3[,-14]))
anova(lm(SalePrice~.,data=fix_data3),lm(SalePrice~.,data=fix_data3[,-16]))

###변수 제거한 데이터 다시 회귀
fix_data4<-fix_data3[,-c(7,11)]
str(fix_data4)

log_full4<-lm(SalePrice~., data=fix_data4)
log_slm4<-step(log_full4,direction='both')
par(mfrow=c(2,2))
plot(log_slm4)
summary(log_slm4)
vif(log_slm4)

###다중공선성 없음
###찐막으로 한개 혹은 2개 범주 높음 PavedDrive , BsmtExposure , BsmtQual , condition1
###	모든 범주 높음 HeatingQC
### 위에서 부터 순서대로 20, 12, 11, 6 , 14
fix_data5<-log_slm4$model
str(fix_data5)
names(fix_data5)

###anova분석
anova(lm(SalePrice~.,data=fix_data5),lm(SalePrice~.,data=fix_data5[,-20]))
anova(lm(SalePrice~.,data=fix_data5),lm(SalePrice~.,data=fix_data5[,-12]))
anova(lm(SalePrice~.,data=fix_data5),lm(SalePrice~.,data=fix_data5[,-11]))
anova(lm(SalePrice~.,data=fix_data5),lm(SalePrice~.,data=fix_data5[,-6]))
anova(lm(SalePrice~.,data=fix_data5),lm(SalePrice~.,data=fix_data5[,-14]))

###최종
###다중공선성만족
###모든 변수 p-value 낮거나 anova 분석결과 유의한 변수임
###선형성, 등분산성, 정규성 비교적 만족

###최종 모델
final_model<-lm(SalePrice~.,data=fix_data5)
summary(final_model)

###아직 해결 안된점. 로그변환한 것을 어떻게 해석 해야하는가 및 회귀식 해석
###변수중에 p-value가 높지만 anova 분석은 귀무가설을 기각해서 놔뒀는데 어떻게 해야할까


qqPlot(log_slm3,labels=row.names(states),id.method="identify",simulate=TRUE,main="Q-Q_ plot")


#######################예측################################
final_test<-read.csv("테스트셋.csv",header=T)
str(final_test)
final_test<-final_test[,-1]
final_test[,-c(1,4,5,13,17,24,28,29:46)]<-lapply(final_test[,-c(1,4,5,13,17,24,28,29:46)],factor)
dim(final_test)

final_test$Fireplaces <- as.factor(final_test$Fireplaces)
final_test$Pool <- as.factor(final_test$Pool)
final_test$Fence <- as.factor(final_test$Fence)
final_test$GarageType<-as.factor(final_test$GarageType)

final_test$GarageFinish<-as.character(final_test$GarageFinish)
final_test$GarageFinish[is.na(final_test$GarageFinish)]<-"Unf"
final_test$GarageFinish<-as.factor(final_test$GarageFinish)
apply(final_test,2,is.na)

###결측치 대체
final_test$SaleType[is.na(final_test$SaleType)]<-"TYPE2"
final_test$KitchenQual[is.na(final_test$KitchenQual)]<-"TA"
final_test$Functional[is.na(final_test$Functional)]<-"Typ"
final_test$TotalBsmtSF[is.na(final_test$TotalBsmtSF)]<-988
final_test$Exterior1st[is.na(final_test$Exterior1st)]<-2
final_test$MSZoning[is.na(final_test$MSZoning)]<-2
final_test$Bath[is.na(final_test$Bath)]<-2
final_test$GarageCars[is.na(final_test$GarageCars)]<-2
summary(final_test)

table(is.na(final_test))

###회귀모형 적용###
pred<-predict(final_model,final_test)
SalePrice<-exp(pred)

price<-cbind(final_test$Id,SalePrice)
str(price)
head(price)

table(is.na(price))

write.csv(price,"prediction_price.csv")