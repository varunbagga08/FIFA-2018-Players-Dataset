
##model for striker position

##importing fifa data

getwd()
setwd("C:/predictive modelling assignment")
data1<-read.csv("CompleteDataset-Final-ST.csv")

str(data1)

levels(data1$Nationality)

miss_data<-apply(is.na(data1),2,sum)
miss_data<-data.frame(miss_data)

##no missing data

##creating flag for player value

#understanding the distribution
quantile(data1$Player.Value,c(0.90,0.95,0.99))
sum(ifelse(data1$Player.Value>7500,1,0))
224/2277


##10% data has value above 7500..we set this as the threshold value

data1$PLayer_Value_Flag<-ifelse(data1$Player.Value>7500,1,0)

##subsetting data

#10-71

data1_sub<-data1[,c(10:71)]
data1_sub<-data1_sub[-51]
data1_sub<-data1_sub[-1]


##checking the corelation 

(cor(data1_sub))
#pairs(data1_sub[,c(1:20)])

##running a pca

pca1<-prcomp(data1_sub,scale. = T)

##mean of each variable that was used for normalization
pca1$center

##standard dev of each variable that was used for normalization 
pca1$scale

##rotation gives the loading factor for each pca across all the predictors
pca1$rotation
pca1$sdev
pca1$rotation[1:5,1:5]
dim(pca1$x)

##plotting the principal components 

biplot(pca1,scale=0)

###we can infer which variables are the major components of the pca1 and pca2

##pca1 and pca2 are top two principal components basically the eigen vector explaining
##the maximum variances in the data

##now we try to understand the variances explained by these principal components

pca_var<-(pca1$sdev)*2
##proportion of variance

pca_var_t<-(pca_var/sum(pca_var))*100
cum_tot<-cumsum(pca_var_t)
round(pca_var_t)

##scree plot

plot(cum_tot,type="b")


##top 35 principal components explain most of the model

##making it part of the dataframe
data_n<-data.frame(data1[,1:10],pca1$x[,1:35])
data_n<-data.frame(data_n,data1[,75])

##preparing the dataset to feed into the model

##fixing Value
data_n$Value_n<-gsub("M","",as.character(data_n$Value))
data_n$Value_n<-gsub("???","",as.character(data_n$Value_n))
data_n$Value_n<-gsub("K","",as.character(data_n$Value_n))
data_n$Value_n<-as.numeric(data_n$Value_n)

##replacing na values

data_n$Value_n<-ifelse(is.na(data_n$Value_n)>0,0,data_n$Value_n)

##fixing wage

data_n$Wage_n<-gsub("M","",as.character(data_n$Wage))
data_n$Wage_n<-gsub("???","",as.character(data_n$Wage_n))
data_n$Wage_n<-gsub("K","",as.character(data_n$Wage_n))
data_n$Wage_n<-as.numeric(data_n$Wage_n)

##replacing na values

data_n$Wage_n<-ifelse(is.na(data_n$Wage_n)>0,0,data_n$Wage_n)

##data for modelling

data_m<-data_n[,(10:48)]

data_m<-data.frame(data_n[,3],data_m)
names(data_m)[1]<-"Age"
names(data_m)[38]<-"flag"


#splitting into train and test

require(caTools)
sample = sample.split(data_m$flag, SplitRatio = .70)
train_f = subset(data_m, sample == TRUE)
test_f  = subset(data_m, sample == FALSE)

##trying gbm

library(gbm)
tree_gbm=gbm(flag~.
             ,data=train_f,distribution="gaussian",n.trees = 500,
             interaction.depth=4)

summary(tree_gbm)
tree_gbm$train.error

tree_gbm$var.levels

tree_gbm_pred=predict(tree_gbm,newdata=test_f,n.trees = 500)
flag<-ifelse(tree_gbm_pred>0.10,1,0)
sum(ifelse(tree_gbm_pred>0.10,1,0))



library(pROC)
auc <- roc(test_f$flag,tree_gbm_pred)


auc

##checking the misclassification rate

prop.table(table(test_f$flag,flag))

#prop.table(table(test_f$flag,flag))
#flag
#0          1
#0 0.81698389 0.08491947
#1 0.01903367 0.07906296
