rm(list=ls())
setwd("C:/Users/dudtj/Desktop/�����")
final_1<-read.csv("real_final.csv",header=T)
final_1<-final_1[,-1]
str(final_1)

###factor�� ����
names(final_1)
final_1[,c(-1,-4,-5,-13,-17,-24,-28)]<-lapply(final_1[,c(-1,-4,-5,-13,-17,-24,-28)],factor)

###�������κ�
final_2<-read.csv("et.csv",header=T)
final_2<-final_2[,-1]
final_2$Fireplaces <- as.factor(final_2$Fireplaces)
final_2$Pool <- as.factor(final_2$Pool)
final_2$Fence <- as.factor(final_2$Fence)
final_2$GarageType<-as.factor(final_2$GarageType)
str(final_2)

###NA����ġ Unf���� ���� 
table(final_2$GarageFinish)
final_2$GarageFinish<-as.character(final_2$GarageFinish)
final_2$GarageFinish[is.na(final_2$GarageFinish)]<-"Unf"
final_2$GarageFinish<-as.factor(final_2$GarageFinish)

###chisq.test ��� pool�� ���Ծ��� �����ϰ���

###nearZeroVar
library(caret)
nearZeroVar(final_2,names=T)

###�л� 0�� ���� ����		#KitchenAbvGr�� �ϴ� ������
names(final_2)
final_2<-final_2[,c(-4,-6,-8,-10,-17)]

###��ġ��
data<-cbind(final_1,final_2)
str(data)

write.csv(data,"final.csv")

###stepwise
data<-data[,-1]
full <- lm(SalePrice~. , data=data)
slm <- step(full, direction='both')
par(mfrow=c(2,2))
plot(slm)

###���߰�����
library(car)
vif(slm)
#BsmtFinType1 ����
names(data)
data<-data[,-22]

###log��ȯ
log_data<-data
log_data$SalePrice<-log(log_data$SalePrice)
str(log_data)
log_full <- lm(SalePrice~. , data=log_data)
log_slm <- step(log_full, direction='both')
par(mfrow=c(2,2))
plot(log_slm)

log_slm
summary(log_slm)

###524,1299,969 ����
log_data1<-log_data[-c(524,1299,969),]
log_full1<-lm(SalePrice~.,data=log_data1)
log_slm1<-step(log_full1,direction='both')
par(mfrow=c(2,2))
plot(log_slm1)
summary(log_slm1)
vif(log_slm1)

###fix_data�� ���ܿ��̽� �� ������
fix_data1<-log_slm1$model
str(fix_data1)

###HeatingQC,BsmtExposure, Foundation ��� ���� p-value ���� anova �м� �ǽ�
###���߰����� BsmtQual 9.88�̶� �ָ� �ϴ� ����
names(fix_data1) # 17, 15, 13
anova(lm(SalePrice~.,data=fix_data1),lm(SalePrice~.,data=fix_data1[,-13]))
anova(lm(SalePrice~.,data=fix_data1),lm(SalePrice~.,data=fix_data1[,-15]))
anova(lm(SalePrice~.,data=fix_data1),lm(SalePrice~.,data=fix_data1[,-17]))

###anova�м���� foundation�� ����
###496,633,1325 ����
names(fix_data1)
fix_data2<-fix_data1[rownames(fix_data1)!="1325",-13]
fix_data2<-fix_data2[rownames(fix_data2)!="633",]
fix_data2<-fix_data2[rownames(fix_data2)!="496",]
str(fix_data2)

###�ٲ� ������ ȸ�ͺм� �ǽ�
log_full2<-lm(SalePrice~., data=fix_data2)
log_slm2<-step(log_full2,direction='both')
par(mfrow=c(2,2))
plot(log_slm2)
summary(log_slm2)
vif(log_slm2)

###���߰����� ����
###����ġ�� ���̻� ������ �ʰ���
###�����߿� �Ѱ��� p-value�� ���� ���� : Condition , HouseStyle , RoofStyle, PavedDrive
###��� p-value�� ���� ���� : BsmtQual , BsmtExposure , HeatingQC
###Condition1 ~ 6 , 7 , 11 , 22
###BsmtQual ~ 13, 14 , 16
fix_data3<-log_slm2$model
names(fix_data3)

###anova �м� ��� HouseStyle , RoofStyle ����
anova(lm(SalePrice~.,data=fix_data3),lm(SalePrice~.,data=fix_data3[,-6]))
anova(lm(SalePrice~.,data=fix_data3),lm(SalePrice~.,data=fix_data3[,-7])) #����
anova(lm(SalePrice~.,data=fix_data3),lm(SalePrice~.,data=fix_data3[,-11]))#����
anova(lm(SalePrice~.,data=fix_data3),lm(SalePrice~.,data=fix_data3[,-22]))

anova(lm(SalePrice~.,data=fix_data3),lm(SalePrice~.,data=fix_data3[,-13]))
anova(lm(SalePrice~.,data=fix_data3),lm(SalePrice~.,data=fix_data3[,-14]))
anova(lm(SalePrice~.,data=fix_data3),lm(SalePrice~.,data=fix_data3[,-16]))

###���� ������ ������ �ٽ� ȸ��
fix_data4<-fix_data3[,-c(7,11)]
str(fix_data4)

log_full4<-lm(SalePrice~., data=fix_data4)
log_slm4<-step(log_full4,direction='both')
par(mfrow=c(2,2))
plot(log_slm4)
summary(log_slm4)
vif(log_slm4)

###���߰����� ����
###������ �Ѱ� Ȥ�� 2�� ���� ���� PavedDrive , BsmtExposure , BsmtQual , condition1
###	��� ���� ���� HeatingQC
### ������ ���� ������� 20, 12, 11, 6 , 14
fix_data5<-log_slm4$model
str(fix_data5)
names(fix_data5)

###anova�м�
anova(lm(SalePrice~.,data=fix_data5),lm(SalePrice~.,data=fix_data5[,-20]))
anova(lm(SalePrice~.,data=fix_data5),lm(SalePrice~.,data=fix_data5[,-12]))
anova(lm(SalePrice~.,data=fix_data5),lm(SalePrice~.,data=fix_data5[,-11]))
anova(lm(SalePrice~.,data=fix_data5),lm(SalePrice~.,data=fix_data5[,-6]))
anova(lm(SalePrice~.,data=fix_data5),lm(SalePrice~.,data=fix_data5[,-14]))

###����
###���߰���������
###��� ���� p-value ���ų� anova �м���� ������ ������
###������, ��л꼺, ���Լ� ���� ����

###���� ��
final_model<-lm(SalePrice~.,data=fix_data5)
summary(final_model)

###���� �ذ� �ȵ���. �α׺�ȯ�� ���� ��� �ؼ� �ؾ��ϴ°� �� ȸ�ͽ� �ؼ�
###�����߿� p-value�� ������ anova �м��� �͹������� �Ⱒ�ؼ� ���״µ� ��� �ؾ��ұ�


qqPlot(log_slm3,labels=row.names(states),id.method="identify",simulate=TRUE,main="Q-Q_ plot")


#######################����################################
final_test<-read.csv("�׽�Ʈ��.csv",header=T)
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

###����ġ ��ü
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

###ȸ�͸��� ����###
pred<-predict(final_model,final_test)
SalePrice<-exp(pred)

price<-cbind(final_test$Id,SalePrice)
str(price)
head(price)

table(is.na(price))

write.csv(price,"prediction_price.csv")