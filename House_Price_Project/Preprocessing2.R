install.packages("caret")
library(caret)
setwd("C:/Users/kimot/Desktop/��������/��ȸ ������Ʈ")
data <- read.csv("����_train.csv", header=T,stringsAsFactor=F)
head(data)
summary(data)
str(data)
names(data)
summary(data)

#near-zero variation ������� ���� �����ϱ�(�������� �����ִ� Ư¡�� ���ϴ� ������)
#�л��� ���� 0�� ����Ȯ��
nearZeroVar(data, names=TRUE) 
#�� �������� ȸ�ͽ��� p-value�� Ȯ���ؼ� ��������� �Ǵ�
#pvalue �� 0.05���� ������ �͹������� �Ⱒ,�븳������ ä��>������ �����ִ�
#pvalue �� 0.05���� ũ�� �͹������� ä��>������ ���� ������� 
tmp1 <- nearZeroVar(data)
for (i in 1:length(tmp1)){
  print(summary(lm(data$SalePrice~data[,tmp1[i]], data=data)))}
#1 ũ�� , 2 �۴� �̹Ƿ� 1�ΰ͸� ������
a <- c(1,2,1,1,2,2,2,2,1,2,1,2,2,2,2,2,1,2,2,1,1)
b <- nearZeroVar(data, names=TRUE)
tmp2 <- data.frame(a,b)
trash <- as.character(subset(tmp2, a==1)$b)
summary(data[,trash])
#Street, Utilities, LandSlope, MiscFeature, MiscVal ������ ������ ����
data <- data[,-c(6,10,12,75,76)]
names(data)

#��Ÿ BsmtFinSF2, LowQualFinSF, X3SsnPorch ������ �ٸ� ������ ���踦 ���� ó���ϰ���
#������ MiscFeature�� ���ų� �ְų� �������̴� ���� �ȳ�!!
par(mfrow=c(1,2))
boxplot(subset(data, is.na(MiscFeature), SalePrice))
boxplot(subset(data, !is.na(MiscFeature), SalePrice))

# �ֹ��� ���µ� ǰ���� �������� (1�� ��ü)
subset(data, KitchenAbvGr==0, select=c("KitchenAbvGr","KitchenQual"))

##kitchen ���� (score)
#��κ��� �ֹ� ������ 1�ε� 2�̻��� ������ BldgType(�� ���� ����ִ� ���� ��)������ 
#�����Ǿ������� �˾Ҵ�. ���� BldgType������ KitchenAbvGr���� ��ü�ϱ�� ����
#���� kitchenQu�� ���ݿ� ������ ��ġ�� ������ �� ũ��
table(data$KitchenAbvGr)
kitchen <- data[,c("KitchenAbvGr","KitchenQual","SalePrice")]
boxplot(kitchen$SalePrice~kitchen$KitchenAbvGr)#�ֹ氳���� ���ٰ��ؼ� ������ ������ �ƴϴ� ������ ����
boxplot(data$SalePrice~data$KitchenQual)
boxplot(data$SalePrice~data$BldgType)
table(kitchen$KitchenAbvGr)
table(subset(data, KitchenAbvGr>=2, BldgType))
table(data$BldgType)
subset(data, select=c("BldgType","KitchenAbvGr"))
subset(data, BldgType=="TwnhsE"|BldgType=="Twnhs" ,select=c("BldgType","KitchenAbvGr"))
subset(data, KitchenAbvGr>=2, select=c("KitchenQual","KitchenAbvGr"))
table(data$KitchenQual)
#��������� BldgType������ KitchenAbvGr���� ��ü�ϴµ� KitchenAbvGr�� SalePrice�� ���ǻ�����踦 ���´�
#����Ƽ�� 1,2,3,4 ���� �ο��ϰ� �ֹ��� 1���� ���� 0, 1�� ������ ������ -0.5�� ���ִ� ������� �� ���� ��ġ����.
data$KitchenAbvGr[data$KitchenAbvGr==0 | data$KitchenAbvGr==1] <- 0
data$KitchenAbvGr[data$KitchenAbvGr==2] <- -0.5
data$KitchenAbvGr[data$KitchenAbvGr==3] <- -1
data$KitchenQual <- as.character(data$KitchenQual)
data$KitchenQual[data$KitchenQual=='Ex'] <- 4
data$KitchenQual[data$KitchenQual=='Gd'] <- 3
data$KitchenQual[data$KitchenQual=='TA'] <- 2
data$KitchenQual[data$KitchenQual=='Fa'] <- 1
data$KitchenQual <- as.numeric(data$KitchenQual)
data$Kitchen <- data$KitchenQual+data$KitchenAbvGr
data <- data[,-c(13,50,51)]
names(data)
cor(data$Kitchen, data$SalePrice)
plot(data$Kitchen, data$SalePrice)


##Fireplaces ����ó�� (score)
fireplaces <- data[,c(51,52,73)]
summary(fireplaces)
boxplot(fireplaces$SalePrice~fireplaces$Fireplaces) # �������� ������ ������ ���� ������踦 ���δ�
boxplot(fireplaces$SalePrice~fireplaces$FireplaceQu) # ����Ƽ ���� ���� ������踦 ����
tmp <-subset(fireplaces, Fireplaces>1)
boxplot(tmp$Fireplaces~tmp$FireplaceQu)
#�ڽ��ö��� �׷������� ����Ƽ�� ���ݿ� ��ġ�� ������ �� Ŀ���δ� ���� 1�����̷� ������ �ξ �� ������
#��ġ�� ������� fireplace ������ ó���Ѵ�
names(fireplaces)
tmp1 <- subset(fireplaces, is.na(FireplaceQu))
boxplot(tmp1$SalePrice)
#�����ΰ� ���� ���� ����Ƽ�� Po�� ���� ������ ���̰� �ȳ��� ���� na,po�� 0���� ���� 
par(mfrow=c(1,2))
boxplot(tmp1$SalePrice)
boxplot(fireplaces$SalePrice~fireplaces$FireplaceQu) 
#������ġ��
fireplaces[,2] <- as.numeric(fireplaces[,2])
table(fireplaces[,2])
table(data[,52])
fireplaces[,2][is.na(fireplaces[,2])] <- 0
fireplaces[,2][fireplaces[,2]==1] <- 44
fireplaces[,2][fireplaces[,2]==2] <- 11
fireplaces[,2][fireplaces[,2]==3] <- 33
fireplaces[,2][fireplaces[,2]==4] <- 00
fireplaces[,2][fireplaces[,2]==5] <- 22
fireplaces[,2][fireplaces[,2]==44] <- 4
fireplaces[,2][fireplaces[,2]==11] <- 1
fireplaces[,2][fireplaces[,2]==33] <- 3
fireplaces[,2][fireplaces[,2]==00] <- 0
fireplaces[,2][fireplaces[,2]==22] <- 2
table(fireplaces[,2])
fireplace.sum <- fireplaces[,1]+fireplaces[,2]
data[,75] <- fireplace.sum
names(data)[75] <- "Fireplace"
data <- data[,-c(51,52)]
names(data)


## OOOObath ����
bath <- data[,c(44:47,71)]
names(bath)
cor(bath) # bath���������� ������谡 ���� ����
table(bath$BsmtFullBath)
table(bath$BsmtHalfBath)
table(bath$FullBath)
table(bath$HalfBath)
boxplot(bath$SalePrice~bath$BsmtHalfBath) # BsmtHalfBath�� ���� ���ݿ�������x�����Ѵ�
data <- data[,-45]

dim(data)
names(data)

## condition2 ���� ����
data <- data[,-12]
names(data)


## garage ���� ��ġ�� (score)
garage <- data[,c(49:55,69)]
head(garage)
#garage�� ���� ���� �ִ� ���� ������ ���� garage�� �ִ� ���� ��ü�� �� ������ ����.
par(mfrow=c(1,2))
boxplot(subset(garage, is.na(GarageType), SalePrice),ylim=c(0,700000))
boxplot(subset(garage, !is.na(GarageType), SalePrice),ylim=c(0,700000))

#garage�� ���� ������� ������ ���� ������踦 �����Ƿ� garage���������� ���ش�
cor(garage$GarageCars,garage$GarageArea)
table(garage$GarageCar)

par(mfrow=c(2,4))
boxplot(garage$SalePrice~garage$GarageType) #2Types Attchd Basment�� 1��
#Builtln 2��
#CarPort, Detchd �� 0���� 
garage$GarageType<-as.character(garage$GarageType)
garage$GarageType[garage$GarageType=='CarPort' | garage$GarageType=='Detchd'] <- 0
garage$GarageType[garage$GarageType=='2Types' | garage$GarageType=='Attchd'|garage$GarageType=='Basment'] <- 1
garage$GarageType[garage$GarageType=='BuiltIn'] <- 2
garage$GarageType[is.na(garage$GarageType)] <- 0
garage$GarageType<-as.numeric(garage$GarageType)


#GarageYrBlt�� 2000�� ���ķ� 0�� 1�� �ο�
plot(garage$SalePrice~garage$GarageYrBlt)#
garage$GarageYrBlt[garage$GarageYrBlt>2000] <- 1
garage$GarageYrBlt[garage$GarageYrBlt<=2000] <- 0


#GarageFinish
boxplot(garage$SalePrice~garage$GarageFinish) #
table(garage$GarageFinish)
garage$GarageFinish <- as.character(garage$GarageFinish)
garage$GarageFinish[garage$GarageFinish=='Fin'] <- 3
garage$GarageFinish[garage$GarageFinish=='RFn'] <- 2
garage$GarageFinish[garage$GarageFinish=='Unf'] <- 1
garage$GarageFinish<- as.numeric(garage$GarageFinish)
garage$GarageFinish[is.na(garage$GarageFinish)] <- 0

#GarageCars
boxplot(garage$SalePrice~garage$GarageCars) #
garage$GarageCars<-as.numeric(garage$GarageCars)

#GarageQual
boxplot(garage$SalePrice~garage$GarageQual) # 
garage$GarageQual <- as.character(garage$GarageQual)
table(garage$GarageQual)
garage$GarageQual[garage$GarageQual=='Po'|garage$GarageQual=='Fa'] <- 0
garage$GarageQual[garage$GarageQual=='TA'|garage$GarageQual=='Gd'|garage$GarageQual=='Ex'] <- 1
garage$GarageQual <- as.numeric(garage$GarageQual)
garage$GarageQual[is.na(garage$GarageQual)] <- 0

head(garage)
garage<- garage[,-c(5,7,8)]

garage.sum <- apply(garage, 1, sum)
garage.sum[is.na(garage.sum)] <- 0

data <- data[,-c(49:55)]
data$Garage <- garage.sum




## Pool �������� �׳� �������� ������ 1 ������ 0 ���� ����
data[data$PoolArea!=0,]$PoolArea <- 1
subset(data,select=c("PoolArea","PoolQC"))
names(data)
data <- data[,-56]
names(data)[names(data)=="PoolArea"] <- "Pool" 

##Fence "NA GdPrv"�� 1 "GdWo MnPrv MnWw" �� 0
data$Fence<-as.character(data$Fence)
data$Fence[is.na(data$Fence)] <- 'NA'
table(data$Fence)
boxplot(data$SalePrice~data$Fence)
summary(lm(data$SalePrice~data$Fence, data=data)) # p���� ���� ����������
data$Fence[data$Fence=='NA' | data$Fence=='GdPrv'] <- 1
data$Fence[data$Fence=='GdWo' | data$Fence=='MnPrv'|data$Fence=='MnWw'] <- 0
data$Fence <-as.numeric(data$Fence) 


#PavedDrive (������°� �������� ������ ����)
table(data$PavedDrive)
boxplot(data$SalePrice~data$PavedDrive)

##deck�̳� porch�� �ִپ��� ������ ������ �ִ�1 ����0
names(deck)
deck <- data[,c(50:54,61)]
summary(data)
data$WoodDeckSF[data$WoodDeckSF!=0] <- 1
data$OpenPorchSF[data$OpenPorchSF!=0] <- 1
data$X3SsnPorch[data$X3SsnPorch!=0] <- 1
data$EnclosedPorch[data$EnclosedPorch!=0] <- 1
data$ScreenPorch[data$ScreenPorch!=0] <- 1

##Bedroom�� ������ ��ü �� ������ ������谡 ����, ���ݰ��� ������谡 ���� ��ü�氹�� ������ ��ü
str(data)
names(data)
check <- c()
cor(data$BedroomAbvGr, data$TotRmsAbvGrd)
for(i in dim(data)[1]) {
  if(data$BedroomAbvGr[i]>data$TotRmsAbvGrd[i]){
    check[i] <- 1}}
cor(data$BedroomAbvGr, data$SalePrice)
cor(data$TotRmsAbvGrd, data$SalePrice)
data <- data[,-46]

##MoSold,YrSold ���ݿ� ������ �� �� ���� �ʰ�, ������ Ȯ���� ���� ���� �������̰� ����. ����
data$MoSold
data$YrSold
table(data$MoSold)
table(data$YrSold)
boxplot(data$SalePrice~data$YrSold)
boxplot(data$SalePrice~data$MoSold)
data <- data[,-c(56,57)]

##SaleType �� boxplot���� Ȯ�� �� 3���� ���ַ� ���´�
table(data$SaleType)
boxplot(data$SalePrice~data$SaleType)
data$SaleType<-as.character(data$SaleType)
data$SaleType[data$SaleType=='COD'] <- 'TYPE1'
data$SaleType[data$SaleType=='ConLD'] <- 'TYPE1'
data$SaleType[data$SaleType=='ConLI'] <- 'TYPE1'
data$SaleType[data$SaleType=='ConLw'] <- 'TYPE1'
data$SaleType[data$SaleType=='Oth'] <- 'TYPE1'
data$SaleType[data$SaleType=='Con'] <- 'TYPE2'
data$SaleType[data$SaleType=='CWD'] <- 'TYPE2'
data$SaleType[data$SaleType=='New'] <- 'TYPE2'
data$SaleType[data$SaleType=='WD'] <- 'TYPE3'
data$SaleType<-as.factor(data$SaleType)

##SaleCondition�� normal�� partial �� �ΰ��� ����
data$SaleCondition <- as.character(data$SaleCondition)
boxplot(data$SalePrice~data$SaleCondition)
table(data$SaleCondition)
data$SaleCondition[data$SaleCondition!='Partial'] <- 'Normal'
data$SaleCondition <- as.factor(data$SaleCondition)

##Functional Maj2�� 5������ ������ ���� ������ ���̹Ƿ� Maj2�� Typ�� ���´�. 
boxplot(data$SalePrice~data$Functional)
table(data$Functional)
data$Functional <- as.character(data$Functional)
data$Functional[data$Functional!='Maj2'] <- 'Typ'
data$Functional <- as.factor(data$Functional)

##SalePrice ���� �� �ڷ� ����
names(data)
SalePrice <- data$SalePrice
data <- data[,-58]
data$SalePrice <- SalePrice

dim(data)
data$BsmtFinSF1+data$BsmtFinSF2+data$BsmtUnfSF == data$TotalBsmtSF
data$X1stFlrSF+data$X2ndFlrSF+data$LowQualFinSF == data$GrLivArea

head(data[,c(30,32,33,34)], 30)

## factor�� ��ȯ�� ����

data$PavedDrive <- as.factor(data$PavedDrive)
data$Fence <- as.factor(data$Fence)
data$Pool <- as.factor(data$Pool)
data$ScreenPorch <- as.factor(data$ScreenPorch)
data$X3SsnPorch <- as.factor(data$X3SsnPorch)
data$EnclosedPorch <- as.factor(data$EnclosedPorch)
data$OpenPorchSF <- as.factor(data$OpenPorchSF)
data$WoodDeckSF <- as.factor(data$WoodDeckSF)

#
str(data)
summary(data)

names(data)
##�����Ⲩ�� ��ġ��
reduction <- read.csv("C:/Users/kimot/Desktop/��������/��ȸ ������Ʈ/reduction.csv",header=T)

names(reduction)
new1<-reduction[,2:30]
new2<-data[,43:61]
final <- data.frame(new1,new2)
str(final)
summary(final)
final$Electrical[is.na(final$Electrical)] <- 2
names(final)
for(i in 1:28) {
  final[,i] <- as.factor(final[,i])}
final[,c(1,4,5,14,19,25)] <- lapply(final[,c(1,4,5,14,19,25)],as.numeric)