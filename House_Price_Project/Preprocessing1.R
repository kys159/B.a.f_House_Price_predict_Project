rm(list=ls())
data<-read.csv("C:/Users/dudtj/Desktop/�����/����_train.csv",header=T,stringsAsFactor=F)
str(data)

###NA 0 Ȥ�� None�� ����
data$LotFrontage[is.na(data$LotFrontage)]<-0
data$Alley[is.na(data$Alley)]<-"None"
data$MasVnrType[is.na(data$MasVnrType)]<-"None"
data$MasVnrArea[is.na(data$MasVnrArea)]<-0
data$BsmtQual[is.na(data$BsmtQual)]<-"None"
data$BsmtExposure[is.na(data$BsmtExposure)]<-"None"
data$BsmtFinType1[is.na(data$BsmtFinType1)]<-"None"
names(data)

###Electrical,BsmtCond,BsmtFinType2 NA �ֺ����� ��ü
data$Electrical[is.na(data$Electrical)]<-"SBrkr"
data$BsmtCond[is.na(data$BsmtCond)]<-"TA"
data$BsmtFinType2[is.na(data$BsmtFinType2)]<-"Unf"

###�ϴ� ���� �ϴ� �κ�
mydata<-data[,c(1:47,81)]
str(mydata)
table(apply(mydata,2,is.na))

###mydata�� ������ ���� factor�� ����
select<-sapply(mydata,function(x){is.character(x)})
mydata[,select]<-lapply(mydata[,select],factor)
str(mydata)
#MSSubClass ���������� ����
mydata$MSSubClass<-as.factor(mydata$MSSubClass)


# SalePrice�� ������ ������ �ְ� ���� ������ ������ �־ Ȯ��
summary(mydata$SalePrice)
hist(mydata$SalePrice)
# SalePrice 1st quantile ���� ������� 0,1,2,3 ���� ����
mydata$SalePrice<-with(mydata,ifelse(0<=SalePrice & SalePrice<=129975, 0 ,
                                     ifelse(129975<SalePrice & SalePrice<=163000 , 1,
                                            ifelse(163000<SalePrice & SalePrice<=214000, 2 , 3))))

mydata$SalePrice<-as.factor(mydata$SalePrice)
str(mydata)
# ������ SalePrice�� �࿡ �ְ� ���� ������ ������ �־ chisq ������ ���� ����� Ȯ��
# for�� �̿��ؼ� p-value 0.05 �Ѵ� ���� �̾Ƴ�
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

#Street , Utilities, LandSlope, Condition2, BsmtFinType2 �л��� 0�̰� chisq.test ��� ����
#Alley , LandContour, RoofMat1, BsmtCond, BsmtFinSF2, Heating, LowQualFinSF �л��� 0 ����

#################SalePrice�� ���������� �������·� ���°��� �� �������̿��� SalePrice�� �������·� ��

###MSSubClass (60,80,120) 2 , (20,40,50,70,75,85,90,160) 1 , (30,45,180,190) 0 boxplot����
with(mydata,boxplot(SalePrice~MSSubClass,las=2))
mydata$MSSubClass<-with(mydata,ifelse(MSSubClass==60 | MSSubClass==120 | MSSubClass==80, 2,
                                      ifelse(MSSubClass==30 | MSSubClass==45 | MSSubClass==180 | MSSubClass==190, 0,1)))

###MSZoning	(FV,RL) 2 , (RH) 1 , (RM,C) 0 �� ������
with(mydata,boxplot(SalePrice~MSZoning))
mydata$MSZoning<-with(mydata,ifelse(MSZoning=="FV" | MSZoning=="RL", 2,
                                    ifelse(MSZoning=="RH" , 1,0)))

###LotFrontage, LotArea ������ 0.1 �̿��� ���� �ʰ� ���� ����ġ�� 0���� �ٲ��� LotArea�� ����ġ����

###Street P-VALUE ���� ����

###Alley	�л��� 0 ������

###LotShape	IR2 2 , (IR1,IR3) 1 , Reg 0 
with(mydata,boxplot(SalePrice~LotShape))
mydata$LotShape<-with(mydata,ifelse(LotShape=="IR2" , 2,
                                    ifelse(LotShape=="IR1" | LotShape=="IR3" , 1, 0)))

###LandContour �л��� 0 ������

###Utilities p-value ���� ����

###LotConfig	(CulDSac,FR3,FR2) 1 , (Corner,Inside) 0 ���� ����
with(mydata,boxplot(SalePrice~LotConfig))
mydata$LotConfig<-with(mydata,ifelse(LotConfig=="CulDSac" | LotConfig=="FR3" | LotConfig=="FR2" , 1, 0))

###LandSlope p-value ���� ����

###Neighborhood  �ڽ��ö� ��ձ��� 4�ܰ�� ������ 0 , 1, 2 ,3 �ο�
with(mydata,boxplot(SalePrice~Neighborhood,las=2))
mydata$Neighborhood<-with(mydata,ifelse(Neighborhood=="NoRidge" | Neighborhood=="NridgHt" | Neighborhood=="StoneBr" |
                                          Neighborhood=="Somerst" | Neighborhood=="Timber" | Neighborhood=="Veenker"	, 3,
                                        ifelse(Neighborhood=="Blueste" | Neighborhood=="BrDale" | Neighborhood=="BrkSide" |
                                                 Neighborhood=="Edwards" | Neighborhood=="IDOTRR" | Neighborhood=="MeadowV" |
                                                 Neighborhood=="OldTown" , 0 ,
                                               ifelse(Neighborhood=="Mitchel" | Neighborhood=="NAmes" | Neighborhood=="NPkVill" | Neighborhood=="Sawyer" | 
                                                        Neighborhood=="SWISU", 1, 2 ))))

###Condition1	�ڽ��ö� ��� ���� 3�ܰ�� ���� 2, 1, 0 �ο�
with(mydata,boxplot(SalePrice~Condition1,las=2))
mydata$Condition1<-with(mydata,ifelse(Condition1=="PosA" | Condition1=="PosN" | Condition1=="RRNe" |
                                        Condition1=="RRNn" | Condition1=="Norm" | Condition1=="RRAn" , 2,
                                      ifelse( Condition1=="Feedr" | Condition1=="RRAe", 1 , 0)))

###Condition2 p-value ���� ����

###BldgType ������ �κ� ������ ��ħ

###HouseStyle	�ڽ��ö� ��� ���� 3�ܰ�� ���� 2,1,0 �ο�
with(mydata,boxplot(SalePrice~HouseStyle,las=2))
mydata$HouseStyle<-with(mydata,ifelse(HouseStyle=="SLvl" | HouseStyle=="2.5Fin" | HouseStyle=="2Story" , 2, 
                                      ifelse(HouseStyle=="1.5Unf", 0 ,1)))

###OverallQual	�ڽ��ö� ��� ���� 4�ܰ�� ���� 0,1,2,3
with(mydata,boxplot(SalePrice~OverallQual,las=2))
mydata$OverallQual<-with(mydata,ifelse(OverallQual==1 | OverallQual==2 | OverallQual==3 | OverallQual==4 , 0 ,
                                       ifelse(OverallQual==5 | OverallQual==6  , 1 ,
                                              ifelse(OverallQual==7 , 2,3))))

###OverallCond	�ڽ��ö� ��� ���� 3�ܰ�� ���� 0,1,2
with(mydata,boxplot(SalePrice~OverallCond,las=2))
mydata$OverallCond<-with(mydata,ifelse(OverallCond==5 | OverallCond==9 , 2 ,
                                       ifelse(OverallCond==6 | OverallCond==7 | OverallCond==8 , 1 ,0 )))

###YearBuilt	YearRemodAdd���� �������� 0.6���� ���� YearRemodAdd�� YearBuilt���� �����ϹǷ�
#YearBuilt�� ������

###YearRemodAdd �׳� ����

###RoofStyle	�ڽ��ö� ��� ���� 2���� ������ 0,1 �ο�
with(mydata,boxplot(SalePrice~RoofStyle,las=2))
mydata$RoofStyle<-with(mydata,ifelse(RoofStyle=="Gable" | RoofStyle=="Gambrel" , 0, 1))

###RoofMatl �л��� 0 ������

###Exterior1st	�ڽ��ö� ��� ���� 4���� ������ 0,1,2,3 �ο�
with(mydata,boxplot(SalePrice~Exterior1st,las=2))
mydata$Exterior1st<-with(mydata,ifelse(Exterior1st=="CemntBd" | Exterior1st=="lmStucc" , 3,
                                       ifelse(Exterior1st=="AsbShng" | Exterior1st=="AsphShn" | Exterior1st=="BrkComm" |Exterior1st=="CBlock" |
                                                Exterior1st=="WdShing" ,0,
                                              ifelse(Exterior1st=="HdBoard" | Exterior1st=="MetalSd" | Exterior1st=="Sutcco" |
                                                       Exterior1st=="WdSdng" , 1 ,2))))

###Exterior2nd 	Exterior1st �� ���̺��� ������ �ſ� ����ؼ� Exterior2nd�� ����
table(mydata$Exterior1st,mydata$Exterior2nd)

###MasVnrType	�ڽ��ö� ��� ���� 3���� ���� 0,1,2 �ο�
with(mydata,boxplot(SalePrice~MasVnrType,las=2))
mydata$MasVnrType<-with(mydata,ifelse(MasVnrType=="Stone" , 2 ,
                                      ifelse(MasVnrType=="BrkFace" , 1 , 0 )))

###MasVnrArea ����

###ExterQual  	�ڽ��ö� ��� ���� 3���� ���� 0,1,2, �ο�
with(mydata,boxplot(SalePrice~ExterQual,las=2))
mydata$ExterQual<-with(data,ifelse(ExterQual=="Ex" | ExterQual=="Gd" ,2 ,
                                   ifelse(ExterQual=="Fa" , 0 , 1)))

###ExterCond	�ڽ��ö� ��� ���� 3���� ���� 0,1,2 �ο�
with(mydata,boxplot(SalePrice~ExterCond,las=2))
mydata$ExterCond<-with(data,ifelse(ExterQual=="Ex" | ExterQual=="Gd" ,1 ,
                                   ifelse(ExterQual=="Fa" | ExterQual=="Po", 0 , 2)))

###Foundation	�ڽ��ö� ��� ���� 3���� ���� 0,1,2 �ο�
with(mydata,boxplot(SalePrice~Foundation,las=2))
mydata$Foundation<-with(mydata,ifelse(Foundation=="PConc" | Foundation=="Wood" ,2 ,
                                      ifelse(Foundation=="CBlock" , 1 , 0))) 

###BsmtQual 	�ڽ��ö� ��� ���� 4���� ���� 0,1,2,3�ο�
with(mydata,boxplot(SalePrice~BsmtQual,las=2))
mydata$BsmtQual<-with(mydata,ifelse(BsmtQual=="Ex" , 3,
                                    ifelse(BsmtQual=="Gd" , 2 ,
                                           ifelse(BsmtQual=="TA" , 1, 0))))

###BsmtCond 	�л��� 0 ����

###BsmtExposure	�ڽ��ö� ��� ���� 4���� ���� 0,1,2,3 �ο�
with(mydata,boxplot(SalePrice~BsmtExposure,las=2))
mydata$BsmtExposure<-with(mydata,ifelse(BsmtExposure=="Gd" , 3,
                                        ifelse(BsmtExposure=="None" , 0 ,
                                               ifelse(BsmtExposure=="No",1,2))))

###BsmtFinType1 	�ڽ��ö� ��� ���� 3���� ���� 0,1,2 �ο�
with(mydata,boxplot(SalePrice~BsmtFinType1,las=2))
mydata$BsmtFinType1<-with(mydata,ifelse(BsmtFinType1=="GLQ" , 2 ,
                                        ifelse(BsmtFinType1=="None",0,1)))

###BsmtFinType2 p-value ŭ ����

###BsmtFinSF1,BsmtFinSF2,BsmtUnfSF ��ģ�� TotalBsmtSF

###TotalBsmtSF	��ġ���̶� �״�� ����

###Heating 		�л��� 0 ����

###HeatingQC	�ڽ��ö� ��� ���� 3���� ���� 0,1,2 �ο�
with(mydata,boxplot(SalePrice~HeatingQC))
mydata$HeatingQC<-with(mydata,ifelse(HeatingQC=="Ex" ,2,
                                     ifelse(HeatingQC=="Gd" | HeatingQC=="TA" ,1,0 )))

###CentralAir	�ڽ��ö� ���� Y�� 1 N�� 0 �� factor�϶� 1,2 �� �ٲ� �����⶧���� �ɸ��ͷ� �مf�ٰ�
#�ٽ� �ٲ���
mydata$CentralAir<-as.character(mydata$CentralAir)
table(mydata$CentralAir)
with(mydata,boxplot(SalePrice~CentralAir))
mydata$CentralAir<-with(mydata,ifelse(CentralAir=="Y" , 1, 0))
mydata$CentralAir<-as.factor(mydata$CentralAir)
str(mydata)

###Electrical	�ڽ��ö� ���� 2���� ����
with(mydata,boxplot(SalePrice~Electrical))
mydata$Electrical<-with(mydata,ifelse(Electrical=="SBrkr" , 1, 0))

###GrLivArea �� X1stFlrSF+X2ndFlrSF+LowQualFinSF �̹Ƿ� ������ ����

###������ ������
#BldgType
#YearBuilt
#Exterior2nd
#BsmtFinSF1,BsmtFinSF2,BsmtUnfSF
#X1stFlrSF+X2ndFlrSF+LowQualFinSF
#Street , Utilities, LandSlope, Condition2, BsmtFinType2 �л��� 0�̰� chisq.test ��� ����
#Alley , LandContour, RoofMat1, BsmtCond, BsmtFinSF2, Heating, LowQualFinSF �л��� 0 ����

names(mydata)
str(mydata)
###�ٽ� factor�� ����
mydata[,select]<-lapply(mydata[,select],factor)

###������ ���� ���� (���������� ��ġ�� ���� SalePrice�� ������)
myfinal<-mydata[,c(-6,-7,-9,-10,-12,-15,-16,-20,-23,-25,-32,-35,-36,-37,-38,-40,-44,-45,-46,-48)]

###factor�� �ȹٲ� ������ ���� factor�� ����
myfinal$MSSubClass<-as.factor(myfinal$MSSubClass)
myfinal$OverallQual<-as.factor(myfinal$OverallQual)
myfinal$OverallCond<-as.factor(myfinal$OverallCond)
str(myfinal)

setwd("C:/Users/dudtj/Desktop/�����")
write.csv(myfinal,"real_final.csv")