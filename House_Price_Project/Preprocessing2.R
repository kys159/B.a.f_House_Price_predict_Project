install.packages("caret")
library(caret)
setwd("C:/Users/kimot/Desktop/은태파일/학회 프로젝트")
data <- read.csv("집값_train.csv", header=T,stringsAsFactor=F)
head(data)
summary(data)
str(data)
names(data)
summary(data)

#near-zero variation 방법으로 변수 제거하기(한쪽으로 몰려있는 특징을 지니는 변수들)
#분산이 거의 0인 변수확인
nearZeroVar(data, names=TRUE) 
#각 변수별로 회귀식의 p-value를 확인해서 지울것인지 판단
#pvalue 가 0.05보다 작으면 귀무가설을 기각,대립가설을 채택>변수가 쓸모있다
#pvalue 가 0.05보다 크면 귀무가설을 채택>변수가 별로 쓸모없다 
tmp1 <- nearZeroVar(data)
for (i in 1:length(tmp1)){
  print(summary(lm(data$SalePrice~data[,tmp1[i]], data=data)))}
#1 크다 , 2 작다 이므로 1인것만 버린다
a <- c(1,2,1,1,2,2,2,2,1,2,1,2,2,2,2,2,1,2,2,1,1)
b <- nearZeroVar(data, names=TRUE)
tmp2 <- data.frame(a,b)
trash <- as.character(subset(tmp2, a==1)$b)
summary(data[,trash])
#Street, Utilities, LandSlope, MiscFeature, MiscVal 변수는 지우기로 결정
data <- data[,-c(6,10,12,75,76)]
names(data)

#기타 BsmtFinSF2, LowQualFinSF, X3SsnPorch 변수는 다른 변수와 관계를 보고 처리하겠음
#실제로 MiscFeature가 없거나 있거나 가격차이는 별로 안남!!
par(mfrow=c(1,2))
boxplot(subset(data, is.na(MiscFeature), SalePrice))
boxplot(subset(data, !is.na(MiscFeature), SalePrice))

# 주방이 없는데 품질이 나와있음 (1로 대체)
subset(data, KitchenAbvGr==0, select=c("KitchenAbvGr","KitchenQual"))

##kitchen 변수 (score)
#대부분의 주방 갯수는 1인데 2이상인 변수는 BldgType(한 집에 살고있는 가정 수)변수와 
#연관되어있음을 알았다. 따라서 BldgType변수를 KitchenAbvGr으로 대체하기로 결정
#또한 kitchenQu는 가격에 영향을 끼치는 정도가 꽤 크다
table(data$KitchenAbvGr)
kitchen <- data[,c("KitchenAbvGr","KitchenQual","SalePrice")]
boxplot(kitchen$SalePrice~kitchen$KitchenAbvGr)#주방개수가 많다고해서 가격이 높은게 아니다 오히려 낮다
boxplot(data$SalePrice~data$KitchenQual)
boxplot(data$SalePrice~data$BldgType)
table(kitchen$KitchenAbvGr)
table(subset(data, KitchenAbvGr>=2, BldgType))
table(data$BldgType)
subset(data, select=c("BldgType","KitchenAbvGr"))
subset(data, BldgType=="TwnhsE"|BldgType=="Twnhs" ,select=c("BldgType","KitchenAbvGr"))
subset(data, KitchenAbvGr>=2, select=c("KitchenQual","KitchenAbvGr"))
table(data$KitchenQual)
#결론적으로 BldgType변수를 KitchenAbvGr으로 대체하는데 KitchenAbvGr는 SalePrice와 음의상관관계를 갖는다
#퀄리티는 1,2,3,4 값을 부여하고 주방이 1개인 것을 0, 1개 증가할 때마다 -0.5을 해주는 방식으로 그 둘을 합치겠음.
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


##Fireplaces 변수처리 (score)
fireplaces <- data[,c(51,52,73)]
summary(fireplaces)
boxplot(fireplaces$SalePrice~fireplaces$Fireplaces) # 벽난로의 개수와 가격이 양의 상관관계를 보인다
boxplot(fireplaces$SalePrice~fireplaces$FireplaceQu) # 퀄리티 또한 양의 상관관계를 보임
tmp <-subset(fireplaces, Fireplaces>1)
boxplot(tmp$Fireplaces~tmp$FireplaceQu)
#박스플랏을 그려봤을때 퀄리티가 가격에 미치는 영향이 더 커보인다 따라서 1점차이로 점수를 두어서 두 변수를
#합치는 방법으로 fireplace 변수를 처리한다
names(fireplaces)
tmp1 <- subset(fireplaces, is.na(FireplaceQu))
boxplot(tmp1$SalePrice)
#벽난로가 없는 집과 퀄리티가 Po인 집의 가격은 차이가 안난다 따라서 na,po는 0으로 설정 
par(mfrow=c(1,2))
boxplot(tmp1$SalePrice)
boxplot(fireplaces$SalePrice~fireplaces$FireplaceQu) 
#변수합치기
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


## OOOObath 변수
bath <- data[,c(44:47,71)]
names(bath)
cor(bath) # bath변수끼리는 상관관계가 거의 없다
table(bath$BsmtFullBath)
table(bath$BsmtHalfBath)
table(bath$FullBath)
table(bath$HalfBath)
boxplot(bath$SalePrice~bath$BsmtHalfBath) # BsmtHalfBath는 별로 가격에영향을x제거한다
data <- data[,-45]

dim(data)
names(data)

## condition2 변수 버림
data <- data[,-12]
names(data)


## garage 변수 합치기 (score)
garage <- data[,c(49:55,69)]
head(garage)
#garage가 없는 집과 있는 집의 가격을 보면 garage가 있는 집이 대체로 더 가격이 높다.
par(mfrow=c(1,2))
boxplot(subset(garage, is.na(GarageType), SalePrice),ylim=c(0,700000))
boxplot(subset(garage, !is.na(GarageType), SalePrice),ylim=c(0,700000))

#garage에 들어가는 차대수와 면적이 양의 상관관계를 가지므로 garage면적변수를 없앤다
cor(garage$GarageCars,garage$GarageArea)
table(garage$GarageCar)

par(mfrow=c(2,4))
boxplot(garage$SalePrice~garage$GarageType) #2Types Attchd Basment을 1로
#Builtln 2로
#CarPort, Detchd 는 0으로 
garage$GarageType<-as.character(garage$GarageType)
garage$GarageType[garage$GarageType=='CarPort' | garage$GarageType=='Detchd'] <- 0
garage$GarageType[garage$GarageType=='2Types' | garage$GarageType=='Attchd'|garage$GarageType=='Basment'] <- 1
garage$GarageType[garage$GarageType=='BuiltIn'] <- 2
garage$GarageType[is.na(garage$GarageType)] <- 0
garage$GarageType<-as.numeric(garage$GarageType)


#GarageYrBlt는 2000년 전후로 0과 1값 부여
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




## Pool 수영장은 그냥 수영장이 있으면 1 없으면 0 으로 설정
data[data$PoolArea!=0,]$PoolArea <- 1
subset(data,select=c("PoolArea","PoolQC"))
names(data)
data <- data[,-56]
names(data)[names(data)=="PoolArea"] <- "Pool" 

##Fence "NA GdPrv"은 1 "GdWo MnPrv MnWw" 은 0
data$Fence<-as.character(data$Fence)
data$Fence[is.na(data$Fence)] <- 'NA'
table(data$Fence)
boxplot(data$SalePrice~data$Fence)
summary(lm(data$SalePrice~data$Fence, data=data)) # p값을 보니 버릴수없다
data$Fence[data$Fence=='NA' | data$Fence=='GdPrv'] <- 1
data$Fence[data$Fence=='GdWo' | data$Fence=='MnPrv'|data$Fence=='MnWw'] <- 0
data$Fence <-as.numeric(data$Fence) 


#PavedDrive (포장상태가 좋을수록 가격이 높다)
table(data$PavedDrive)
boxplot(data$SalePrice~data$PavedDrive)

##deck이나 porch는 있다없다 정도로 나눈다 있다1 없다0
names(deck)
deck <- data[,c(50:54,61)]
summary(data)
data$WoodDeckSF[data$WoodDeckSF!=0] <- 1
data$OpenPorchSF[data$OpenPorchSF!=0] <- 1
data$X3SsnPorch[data$X3SsnPorch!=0] <- 1
data$EnclosedPorch[data$EnclosedPorch!=0] <- 1
data$ScreenPorch[data$ScreenPorch!=0] <- 1

##Bedroom의 갯수는 전체 방 갯수와 상관관계가 높고, 가격과는 상관관계가 낮아 전체방갯수 변수로 대체
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

##MoSold,YrSold 가격에 영향을 줄 것 같지 않고, 실제로 확인해 봐도 별로 가격차이가 없다. 버림
data$MoSold
data$YrSold
table(data$MoSold)
table(data$YrSold)
boxplot(data$SalePrice~data$YrSold)
boxplot(data$SalePrice~data$MoSold)
data <- data[,-c(56,57)]

##SaleType 은 boxplot으로 확인 후 3가지 범주로 묶는다
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

##SaleCondition은 normal과 partial 로 두개로 묶기
data$SaleCondition <- as.character(data$SaleCondition)
boxplot(data$SalePrice~data$SaleCondition)
table(data$SaleCondition)
data$SaleCondition[data$SaleCondition!='Partial'] <- 'Normal'
data$SaleCondition <- as.factor(data$SaleCondition)

##Functional Maj2가 5개지만 가격이 낮은 경향을 보이므로 Maj2와 Typ로 묶는다. 
boxplot(data$SalePrice~data$Functional)
table(data$Functional)
data$Functional <- as.character(data$Functional)
data$Functional[data$Functional!='Maj2'] <- 'Typ'
data$Functional <- as.factor(data$Functional)

##SalePrice 변수 맨 뒤로 변경
names(data)
SalePrice <- data$SalePrice
data <- data[,-58]
data$SalePrice <- SalePrice

dim(data)
data$BsmtFinSF1+data$BsmtFinSF2+data$BsmtUnfSF == data$TotalBsmtSF
data$X1stFlrSF+data$X2ndFlrSF+data$LowQualFinSF == data$GrLivArea

head(data[,c(30,32,33,34)], 30)

## factor로 변환할 변수

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
##영스기꺼랑 합치기
reduction <- read.csv("C:/Users/kimot/Desktop/은태파일/학회 프로젝트/reduction.csv",header=T)

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