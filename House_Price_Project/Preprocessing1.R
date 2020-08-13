rm(list=ls())
data<-read.csv("C:/Users/dudtj/Desktop/비어플/집값_train.csv",header=T,stringsAsFactor=F)
str(data)

###NA 0 혹은 None로 변경
data$LotFrontage[is.na(data$LotFrontage)]<-0
data$Alley[is.na(data$Alley)]<-"None"
data$MasVnrType[is.na(data$MasVnrType)]<-"None"
data$MasVnrArea[is.na(data$MasVnrArea)]<-0
data$BsmtQual[is.na(data$BsmtQual)]<-"None"
data$BsmtExposure[is.na(data$BsmtExposure)]<-"None"
data$BsmtFinType1[is.na(data$BsmtFinType1)]<-"None"
names(data)

###Electrical,BsmtCond,BsmtFinType2 NA 최빈값으로 대체
data$Electrical[is.na(data$Electrical)]<-"SBrkr"
data$BsmtCond[is.na(data$BsmtCond)]<-"TA"
data$BsmtFinType2[is.na(data$BsmtFinType2)]<-"Unf"

###일단 내가 하는 부분
mydata<-data[,c(1:47,81)]
str(mydata)
table(apply(mydata,2,is.na))

###mydata에 문자형 변수 factor로 변경
select<-sapply(mydata,function(x){is.character(x)})
mydata[,select]<-lapply(mydata[,select],factor)
str(mydata)
#MSSubClass 범주형으로 변경
mydata$MSSubClass<-as.factor(mydata$MSSubClass)


# SalePrice를 나눠서 행으로 넣고 열에 범주형 변수를 넣어서 확인
summary(mydata$SalePrice)
hist(mydata$SalePrice)
# SalePrice 1st quantile 부터 순서대로 0,1,2,3 으로 나눔
mydata$SalePrice<-with(mydata,ifelse(0<=SalePrice & SalePrice<=129975, 0 ,
                                     ifelse(129975<SalePrice & SalePrice<=163000 , 1,
                                            ifelse(163000<SalePrice & SalePrice<=214000, 2 , 3))))

mydata$SalePrice<-as.factor(mydata$SalePrice)
str(mydata)
# 범주형 SalePrice를 행에 넣고 열에 범주형 변수를 넣어서 chisq 검정을 통해 상관성 확인
# for문 이용해서 p-value 0.05 넘는 변수 뽑아냄
chi.output<-matrix(rep(NA,50),ncol=1)
for (i in 2:48){	
  if(class(mydata[,i])=="factor"){
    chi.mat<-as.matrix(with(mydata,table(SalePrice,data[,i])))
    chi.output[i,1]<-chisq.test(chi.mat)$p.value
  }
}
chi.output>=0.05	# 6, 10, 12, 15 , 36, 
names(mydata)	# Street , Utilities, LandSlope, Condition2, BsmtFinType2

# nearzerovar
library(caret)
nearZeroVar(mydata, names=TRUE) 

#Street , Utilities, LandSlope, Condition2, BsmtFinType2 분산이 0이고 chisq.test 결과 지움
#Alley , LandContour, RoofMat1, BsmtCond, BsmtFinSF2, Heating, LowQualFinSF 분산이 0 지움

#################SalePrice를 범주형으로 묶은상태로 보는것이 더 직관적이여서 SalePrice를 묶은상태로 봄

###MSSubClass (60,80,120) 2 , (20,40,50,70,75,85,90,160) 1 , (30,45,180,190) 0 boxplot기준
with(mydata,boxplot(SalePrice~MSSubClass,las=2))
mydata$MSSubClass<-with(mydata,ifelse(MSSubClass==60 | MSSubClass==120 | MSSubClass==80, 2,
                                      ifelse(MSSubClass==30 | MSSubClass==45 | MSSubClass==180 | MSSubClass==190, 0,1)))

###MSZoning	(FV,RL) 2 , (RH) 1 , (RM,C) 0 로 합쳐줌
with(mydata,boxplot(SalePrice~MSZoning))
mydata$MSZoning<-with(mydata,ifelse(MSZoning=="FV" | MSZoning=="RL", 2,
                                    ifelse(MSZoning=="RH" , 1,0)))

###LotFrontage, LotArea 상관계수 0.1 이여서 묶지 않고 위에 결측치만 0으로 바꿔줌 LotArea는 결측치없음

###Street P-VALUE 높음 제거

###Alley	분산이 0 제거함

###LotShape	IR2 2 , (IR1,IR3) 1 , Reg 0 
with(mydata,boxplot(SalePrice~LotShape))
mydata$LotShape<-with(mydata,ifelse(LotShape=="IR2" , 2,
                                    ifelse(LotShape=="IR1" | LotShape=="IR3" , 1, 0)))

###LandContour 분산이 0 제거함

###Utilities p-value 높음 제거

###LotConfig	(CulDSac,FR3,FR2) 1 , (Corner,Inside) 0 으로 묶음
with(mydata,boxplot(SalePrice~LotConfig))
mydata$LotConfig<-with(mydata,ifelse(LotConfig=="CulDSac" | LotConfig=="FR3" | LotConfig=="FR2" , 1, 0))

###LandSlope p-value 높음 제거

###Neighborhood  박스플랏 평균기준 4단계로 나눴음 0 , 1, 2 ,3 부여
with(mydata,boxplot(SalePrice~Neighborhood,las=2))
mydata$Neighborhood<-with(mydata,ifelse(Neighborhood=="NoRidge" | Neighborhood=="NridgHt" | Neighborhood=="StoneBr" |
                                          Neighborhood=="Somerst" | Neighborhood=="Timber" | Neighborhood=="Veenker"	, 3,
                                        ifelse(Neighborhood=="Blueste" | Neighborhood=="BrDale" | Neighborhood=="BrkSide" |
                                                 Neighborhood=="Edwards" | Neighborhood=="IDOTRR" | Neighborhood=="MeadowV" |
                                                 Neighborhood=="OldTown" , 0 ,
                                               ifelse(Neighborhood=="Mitchel" | Neighborhood=="NAmes" | Neighborhood=="NPkVill" | Neighborhood=="Sawyer" | 
                                                        Neighborhood=="SWISU", 1, 2 ))))

###Condition1	박스플랏 평균 기준 3단계로 나눔 2, 1, 0 부여
with(mydata,boxplot(SalePrice~Condition1,las=2))
mydata$Condition1<-with(mydata,ifelse(Condition1=="PosA" | Condition1=="PosN" | Condition1=="RRNe" |
                                        Condition1=="RRNn" | Condition1=="Norm" | Condition1=="RRAn" , 2,
                                      ifelse( Condition1=="Feedr" | Condition1=="RRAe", 1 , 0)))

###Condition2 p-value 높음 제거

###BldgType 은태형 부분 변수랑 합침

###HouseStyle	박스플랏 평균 기준 3단계로 나눔 2,1,0 부여
with(mydata,boxplot(SalePrice~HouseStyle,las=2))
mydata$HouseStyle<-with(mydata,ifelse(HouseStyle=="SLvl" | HouseStyle=="2.5Fin" | HouseStyle=="2Story" , 2, 
                                      ifelse(HouseStyle=="1.5Unf", 0 ,1)))

###OverallQual	박스플랏 평균 기준 4단계로 나눔 0,1,2,3
with(mydata,boxplot(SalePrice~OverallQual,las=2))
mydata$OverallQual<-with(mydata,ifelse(OverallQual==1 | OverallQual==2 | OverallQual==3 | OverallQual==4 , 0 ,
                                       ifelse(OverallQual==5 | OverallQual==6  , 1 ,
                                              ifelse(OverallQual==7 , 2,3))))

###OverallCond	박스플랏 평균 기준 3단계로 나눔 0,1,2
with(mydata,boxplot(SalePrice~OverallCond,las=2))
mydata$OverallCond<-with(mydata,ifelse(OverallCond==5 | OverallCond==9 , 2 ,
                                       ifelse(OverallCond==6 | OverallCond==7 | OverallCond==8 , 1 ,0 )))

###YearBuilt	YearRemodAdd와의 상관계수가 0.6으로 높고 YearRemodAdd가 YearBuilt까지 설명하므로
#YearBuilt는 제거함

###YearRemodAdd 그냥 놔둠

###RoofStyle	박스플랏 평균 기준 2개로 나누고 0,1 부여
with(mydata,boxplot(SalePrice~RoofStyle,las=2))
mydata$RoofStyle<-with(mydata,ifelse(RoofStyle=="Gable" | RoofStyle=="Gambrel" , 0, 1))

###RoofMatl 분산이 0 제거함

###Exterior1st	박스플랏 평균 기준 4개로 나누고 0,1,2,3 부여
with(mydata,boxplot(SalePrice~Exterior1st,las=2))
mydata$Exterior1st<-with(mydata,ifelse(Exterior1st=="CemntBd" | Exterior1st=="lmStucc" , 3,
                                       ifelse(Exterior1st=="AsbShng" | Exterior1st=="AsphShn" | Exterior1st=="BrkComm" |Exterior1st=="CBlock" |
                                                Exterior1st=="WdShing" ,0,
                                              ifelse(Exterior1st=="HdBoard" | Exterior1st=="MetalSd" | Exterior1st=="Sutcco" |
                                                       Exterior1st=="WdSdng" , 1 ,2))))

###Exterior2nd 	Exterior1st 와 테이블을 봤을때 매우 비슷해서 Exterior2nd를 버림
table(mydata$Exterior1st,mydata$Exterior2nd)

###MasVnrType	박스플랏 평균 기준 3개로 나눔 0,1,2 부여
with(mydata,boxplot(SalePrice~MasVnrType,las=2))
mydata$MasVnrType<-with(mydata,ifelse(MasVnrType=="Stone" , 2 ,
                                      ifelse(MasVnrType=="BrkFace" , 1 , 0 )))

###MasVnrArea 놔둠

###ExterQual  	박스플랏 평균 기준 3개로 나눔 0,1,2, 부여
with(mydata,boxplot(SalePrice~ExterQual,las=2))
mydata$ExterQual<-with(data,ifelse(ExterQual=="Ex" | ExterQual=="Gd" ,2 ,
                                   ifelse(ExterQual=="Fa" , 0 , 1)))

###ExterCond	박스플랏 평균 기준 3개로 나눔 0,1,2 부여
with(mydata,boxplot(SalePrice~ExterCond,las=2))
mydata$ExterCond<-with(data,ifelse(ExterQual=="Ex" | ExterQual=="Gd" ,1 ,
                                   ifelse(ExterQual=="Fa" | ExterQual=="Po", 0 , 2)))

###Foundation	박스플랏 평균 기준 3개로 나눔 0,1,2 부여
with(mydata,boxplot(SalePrice~Foundation,las=2))
mydata$Foundation<-with(mydata,ifelse(Foundation=="PConc" | Foundation=="Wood" ,2 ,
                                      ifelse(Foundation=="CBlock" , 1 , 0))) 

###BsmtQual 	박스플랏 평균 기준 4개로 나눔 0,1,2,3부여
with(mydata,boxplot(SalePrice~BsmtQual,las=2))
mydata$BsmtQual<-with(mydata,ifelse(BsmtQual=="Ex" , 3,
                                    ifelse(BsmtQual=="Gd" , 2 ,
                                           ifelse(BsmtQual=="TA" , 1, 0))))

###BsmtCond 	분산이 0 제거

###BsmtExposure	박스플랏 평균 기준 4개로 나눔 0,1,2,3 부여
with(mydata,boxplot(SalePrice~BsmtExposure,las=2))
mydata$BsmtExposure<-with(mydata,ifelse(BsmtExposure=="Gd" , 3,
                                        ifelse(BsmtExposure=="None" , 0 ,
                                               ifelse(BsmtExposure=="No",1,2))))

###BsmtFinType1 	박스플랏 평균 기준 3개로 나눔 0,1,2 부여
with(mydata,boxplot(SalePrice~BsmtFinType1,las=2))
mydata$BsmtFinType1<-with(mydata,ifelse(BsmtFinType1=="GLQ" , 2 ,
                                        ifelse(BsmtFinType1=="None",0,1)))

###BsmtFinType2 p-value 큼 제거

###BsmtFinSF1,BsmtFinSF2,BsmtUnfSF 합친게 TotalBsmtSF

###TotalBsmtSF	수치형이라 그대로 놔둠

###Heating 		분산이 0 제거

###HeatingQC	박스플랏 평균 기준 3개로 나눔 0,1,2 부여
with(mydata,boxplot(SalePrice~HeatingQC))
mydata$HeatingQC<-with(mydata,ifelse(HeatingQC=="Ex" ,2,
                                     ifelse(HeatingQC=="Gd" | HeatingQC=="TA" ,1,0 )))

###CentralAir	박스플랏 기준 Y는 1 N는 0 줌 factor일때 1,2 로 바뀌어서 나오기때문에 케릭터로 바꿧다가
#다시 바꿔좀
mydata$CentralAir<-as.character(mydata$CentralAir)
table(mydata$CentralAir)
with(mydata,boxplot(SalePrice~CentralAir))
mydata$CentralAir<-with(mydata,ifelse(CentralAir=="Y" , 1, 0))
mydata$CentralAir<-as.factor(mydata$CentralAir)
str(mydata)

###Electrical	박스플랏 기준 2개로 나눔
with(mydata,boxplot(SalePrice~Electrical))
mydata$Electrical<-with(mydata,ifelse(Electrical=="SBrkr" , 1, 0))

###GrLivArea 가 X1stFlrSF+X2ndFlrSF+LowQualFinSF 이므로 나머지 제거

###제거할 변수들
#BldgType
#YearBuilt
#Exterior2nd
#BsmtFinSF1,BsmtFinSF2,BsmtUnfSF
#X1stFlrSF+X2ndFlrSF+LowQualFinSF
#Street , Utilities, LandSlope, Condition2, BsmtFinType2 분산이 0이고 chisq.test 결과 지움
#Alley , LandContour, RoofMat1, BsmtCond, BsmtFinSF2, Heating, LowQualFinSF 분산이 0 지움

names(mydata)
str(mydata)
###다시 factor로 변경
mydata[,select]<-lapply(mydata[,select],factor)

###제거할 변수 제거 (은태형꺼와 합치기 위해 SalePrice도 지웠음)
myfinal<-mydata[,c(-6,-7,-9,-10,-12,-15,-16,-20,-23,-25,-32,-35,-36,-37,-38,-40,-44,-45,-46,-48)]

###factor로 안바뀐 범주형 변수 factor로 변경
myfinal$MSSubClass<-as.factor(myfinal$MSSubClass)
myfinal$OverallQual<-as.factor(myfinal$OverallQual)
myfinal$OverallCond<-as.factor(myfinal$OverallCond)
str(myfinal)

setwd("C:/Users/dudtj/Desktop/비어플")
write.csv(myfinal,"real_final.csv")
