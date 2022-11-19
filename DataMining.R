# In order to create a variable to have the upper 70% of "MajmoeDarmad1",
#We create a variable named "Rastei_MajmoeDarmad1"

data<-read.csv(file="data10.csv", header = TRUE)
Quantile = quantile(data$MajmoeDarmad1, probs = seq(0, 1, 0.1))
Dbala = which(data$MajmoeDarmad1>Quantile[7])
data[,93] = rep(0,length(data$TedadAza))
data[,93] = replace(data[,93],Dbala,1)
table(data[,93])
names(data)[93] <- paste("Rastei_MajmoeDarmad1")
View(data)
newdata <- na.omit(data)
View(newdata)

############
##############
############### VISUALIZATION:
####Heatmaps:

library(gplots)
data2<-read.csv(file="data10-heatmap.csv", header = TRUE)
Quantile = quantile(data2$MajmoeDarmad1, probs = seq(0, 1, 0.1))
Dbala = which(data2$MajmoeDarmad1>Quantile[7])
data2[,26] = rep(0,length(data2$TedadAza))
data2[,26] = replace(data2[,26],Dbala,1)
table(data2[,26])
names(data2)[26] <- paste("Rastei_MajmoeDarmad1")
View(data2)
newdata <- na.omit(data2)
View(newdata)s
heatmap.2(cor(data2), Rowv = FALSE, Colv = FALSE, dendrogram = "none",
          cellnote = round(cor(data2),2),
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10)) + title("binary variables 
heatmap")


#BarCHart (nemudarhaye payei)

### Barchart  of NahveTasarof vs. Rastei_MajmoeDarmad1###

library(ggplot2)


data.for.plot <- aggregate(newdata$Rastei_MajmoeDarmad1, by = list(newdata$NahveTasarof), FUN = mean)
names(data.for.plot) <- c("NahveTasarof", "Rastei_MajmoeDarmad1")
ggplot(data.for.plot) + geom_bar(aes(x = NahveTasarof, y = Rastei_MajmoeDarmad1),colour="navy", stat = "identity")

####Barchart  of  SarparstJensiat vs. Rastei_MajmoeDarmad1###
data.for.plot <- aggregate(newdata$Rastei_MajmoeDarmad1, by = list(newdata$SarparstJensiat), FUN = mean)
names(data.for.plot) <- c("SarparstJensiat", "Rastei_MajmoeDarmad1")
ggplot(data.for.plot) + geom_bar(aes(x = SarparstJensiat, y = Rastei_MajmoeDarmad1),colour="pink2", stat = "identity")

####Barchart  of   SarparstMadrak vs. Rastei_MajmoeDarmad1###
data.for.plot <- aggregate(newdata$Rastei_MajmoeDarmad1, by = list(newdata$SarparstMadrak), FUN = mean)
names(data.for.plot) <- c("SarparstMadrak", "Rastei_MajmoeDarmad1")
ggplot(data.for.plot) + geom_bar(aes(x = SarparstMadrak, y = Rastei_MajmoeDarmad1),colour="skyblue3", stat = "identity")

####Barchart  of   SarparstFaaliat vs. Rastei_MajmoeDarmad1###
data.for.plot <- aggregate(newdata$Rastei_MajmoeDarmad1, by = list(newdata$SarparstFaaliat), FUN = mean)
names(data.for.plot) <- c("SarparstFaaliat", "Rastei_MajmoeDarmad1")
ggplot(data.for.plot) + geom_bar(aes(x = SarparstFaaliat, y = Rastei_MajmoeDarmad1),colour="navy", stat = "identity")

####Barchart  of  TedadAza vs. Rastei_MajmoeDarmad1###
data.for.plot <- aggregate(newdata$Rastei_MajmoeDarmad1, by = list(newdata$TedadAza), FUN = mean)
names(data.for.plot) <- c("TedadAza", "Rastei_MajmoeDarmad1")
ggplot(data.for.plot) + geom_bar(aes(x = TedadAza, y = Rastei_MajmoeDarmad1),colour="pink2", stat = "identity")

####Barchart  of  SarparstSen vs. Rastei_MajmoeDarmad1###
data.for.plot <- aggregate(newdata$Rastei_MajmoeDarmad1, by = list(newdata$SarparstSen), FUN = mean)
names(data.for.plot) <- c("SarparstSen", "Rastei_MajmoeDarmad1")
ggplot(data.for.plot) + geom_bar(aes(x = SarparstSen, y = Rastei_MajmoeDarmad1),colour="darkblue", stat = "identity")


####Barchart  of  SarparstZanashoyi vs. Rastei_MajmoeDarmad1###
data.for.plot <- aggregate(newdata$Rastei_MajmoeDarmad1, by = list(newdata$SarparstZanashoyi), FUN = mean)
names(data.for.plot) <- c("SarparstZanashoyi", "Rastei_MajmoeDarmad1")
ggplot(data.for.plot) + geom_bar(aes(x = SarparstZanashoyi, y = Rastei_MajmoeDarmad1),colour="yellow", stat = "identity")


#######ggplot-histogram: (nemudare tozi)########

library(ggplot2)
ggplot() + geom_histogram(aes(x=newdata$SarparstSen), color = "black" , fill="pink2", bins = 40) + 
  labs(x="SarparstSen")

ggplot() + geom_histogram(aes(x=newdata$SarparstMadrak), color = "black" , fill="navy", bins = 40) + 
  labs(x="SarparstMadrak")
ggplot() + geom_histogram(aes(x=newdata$SarparstFaaliat), color = "black" , fill="green", bins = 40) + 
  labs(x="SarparstFaaliat")

######Box plot#########
### Boxplot of Rastei_MajmoeDarmad1 for different values of SarparstFaaliat/SarparstMadrak/SarparstJensiat/NahveTasarof###
Faaliyat_Hboxplot= ggplot(newdata) + geom_boxplot(aes(x = as.factor(SarparstFaaliat), y = Rastei_MajmoeDarmad1)) + xlab("SarparstFaaliat")
Faaliyat_Hboxplot + coord_flip()

ggplot(newdata) + geom_boxplot(aes(x = as.factor(SarparstMadrak), y = Rastei_MajmoeDarmad1)) + xlab("SarparstMadrak")
SarparstMadrak_Hboxplot + coord_flip()

######################################

######Donut plots:

library(dplyr)
rastei_majmoeDarmad1 = arrange(newdata, Rastei_MajmoeDarmad1)
RM = rastei_majmoeDarmad1 %>% group_by(Rastei_MajmoeDarmad1)
RM0nts = data.frame(table(RM[RM$Rastei_MajmoeDarmad1 == 0, 9]))
RM1nts = data.frame(table(RM[RM$Rastei_MajmoeDarmad1 == 1, 9]))

######percentages

RM0nts$fraction <- RM0nts$Freq / sum(RM0nts$Freq)
RM1nts$fraction <- RM0nts$Freq / sum(RM1nts$Freq)

######the cumulative percentages (top of each rectangle)

RM0nts$ymax <- cumsum(RM0nts$fraction)
RM1nts$ymax <- cumsum(RM1nts$fraction)

######the bottom of each rectangle

RM0nts$ymin <- c(0, head(RM0nts$ymax, n=-1))
RM1nts$ymin <- c(0, head(RM1nts$ymax, n=-1))

######label position

RM0nts$labelPosition <- (RM0nts$ymax + RM0nts$ymin) / 2
RM1nts$labelPosition <- (RM1nts$ymax + RM1nts$ymin) / 2

###### good label

RM0nts$label <- paste0(RM0nts$Var1, "\n value: ", RM0nts$Freq)
RM1nts$label <- paste0(RM1nts$Var1, "\n value: ", RM1nts$Freq)

###### showing the plot

ggplot(RM0nts, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=4) +
  scale_fill_brewer(palette=9) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none") + labs(title="nahve tasarrof manzel maskoni khanevarhaye kam 
daramad")
ggplot(RM1nts, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=4) +
  scale_fill_brewer(palette=11) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none") + labs(title="nahve tasarof manzel maskoni khanevar por 
daramad")



####barplot multivar

RM0NS = data.frame(table(RM[RM$Rastei_MajmoeDarmad1 == 1, 2]),rastei_majmoeDarmad1 = 0)
RM1NS = data.frame(table(RM[RM$Rastei_MajmoeDarmad1 == 1, 2]),rastei_majmoeDarmad1 = 1)
RMNS = rbind(RM0NS,RM1NS)
ggplot(RMNS, aes(fill=rastei_majmoeDarmad1, y=Freq, x=Var1)) + 
  geom_bar(position="stack", stat="identity") + 
  ggtitle("SarparstMadrak be tafkik khanevar por daramad va kam daramad") +
  xlab("SarparstMadrak") + ylab("Faravani")

# variables(HAZINE)
library(dplyr)
library(MASS)
H <- ifelse(newdata$Rastei_MajmoeDarmad1 == 0, "skyblue3", "pink4")
parcoord(newdata[ , c(46:56) ], col= H, main = "Hazine be tafkike khanevarha")
##################################################################################################################
#DIMENSION REDUCTION         PCA         :


#Data:
data<-read.csv(file="data10PC.csv", header = TRUE)
Quantile = quantile(data$MajmoeDarmad1, probs = seq(0, 1, 0.1))
Dbala = which(data$MajmoeDarmad1>Quantile[7])
data[,79] = rep(0,length(data$TedadAza))
data[,79] = replace(data[,79],Dbala,1)
table(data[,79])
names(data)[79] <- paste("Rastei_MajmoeDarmad1")
newdata <- na.omit(data)

#### 
summary(newdata)

#Heatmap for finding the correlations:
library(gplots)
data2<-read.csv(file="data10-heatmap.csv", header = TRUE)
heatmap.2(cor(data2), Rowv = FALSE, Colv = FALSE, dendrogram = "none",
          cellnote = round(cor(data2),2),
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10)) + title("binary variables 
heatmap")

########COR :

(cor(newdata))

##################################################
#####Distribution plot
library(ggmap)
th.df= newdata
tbl= table(th.df$Rastei_MajmoeDarmad1, th.df$TedadAza)
prop.tbl=prop.table(tbl,margin=2)
barplot(prop.tbl,xlab="TedadAza",ylab ="",yaxt="n", main="Distribution of Rastei_MajmoeDarmad1 by TedadAza")
axis(2, at=(seq(0,1,.2)), paste(seq(0,100,20),"%"))

####################################
PCAdata = prcomp(newdata,)
summary.pca = summary(PCAdata)

summary.pca

#####################################
#PCA
#for all variables:
pca=prcomp(data.frame(newdata))
summary.pca=summary(pcs)
(rot.pca = pca$rot)
s=pca$x
head(s,6)
###################################

pca.var2 = matrix(rep(0),14,14)
for(i in 1:14){
  p2 = row.names(rot.pca[abs(rot.pca[,i])>quantile(abs(rot.pca[,i]))[4],])
  for(j in 1:14){
    pca.var2[j,i] = p2[j]
  }
}
pca.var2 = data.frame(pca.var2)
View(pca.var2)
pca.var2
##############################################################
#Logistic Regression
##############################################################
# In order to create a variable to have the upper 70% of "MajmoeDarmad1",
#We create a variable named "Rastei_MajmoeDarmad1"

data<-read.csv(file="data10.csv", header = TRUE)
Quantile = quantile(data$MajmoeDarmad1, probs = seq(0, 1, 0.1))
Dbala = which(data$MajmoeDarmad1>Quantile[7])
data[,93] = rep(0,length(data$TedadAza))
data[,93] = replace(data[,93],Dbala,1)
table(data[,93])
names(data)[93] <- paste("Rastei_MajmoeDarmad1")
View(data)
newdata <- na.omit(data)
View(newdata)

library(nnet)
data_nnet<-newdata[,c(2,4,7,8,13,14,18,21,22,31,34,41,46,48,53,55,56,59,60,93)]
df1<-data.frame(class.ind(data_nnet$SarparstMadrak))
df2<-data.frame(class.ind(data_nnet$SarparstFaaliat))
df3<-data.frame(class.ind(data_nnet$NoeEskelet))
data_nnet$TedadAza=(data_nnet$TedadAza-min(data_nnet$TedadAza))/(max(data_nnet$TedadAza)-min(data_nnet$TedadAza))
data_nnet$SarparstSen=(data_nnet$SarparstSen-min(data_nnet$SarparstSen))/(max(data_nnet$SarparstSen)-min(data_nnet$SarparstSen))
data_nnet$HazineKhoraki=(data_nnet$HazineKhoraki-min(data_nnet$HazineKhoraki))/(max(data_nnet$HazineKhoraki)-min(data_nnet$HazineKhoraki))
data_nnet$HazinePoshak=(data_nnet$HazinePoshak-min(data_nnet$HazinePoshak))/(max(data_nnet$HazinePoshak)-min(data_nnet$HazinePoshak))
data_nnet$HazineMaskan=(data_nnet$HazineMaskan-min(data_nnet$HazineMaskan))/(max(data_nnet$HazineMaskan)-min(data_nnet$HazineMaskan))
data_nnet$HazineHamlonaghl=(data_nnet$HazineHamlonaghl-min(data_nnet$HazineHamlonaghl))/(max(data_nnet$HazineHamlonaghl)-min(data_nnet$HazineHamlonaghl))
data_nnet$HazineErtebatat=(data_nnet$HazineErtebatat-min(data_nnet$HazineErtebatat))/(max(data_nnet$HazineErtebatat)-min(data_nnet$HazineErtebatat))
data_nnet1<-cbind(df1,df2,df3,data_nnet)
names(data_nnet1)[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)]=c("M1","M2","M3","M4","M5","M6","M7",
                                                                     "M8", "M9","F1","F2","F3","F4","F5","F6","NE1", "NE2","NE3")

set.seed(123)
train.index<-sample(c(1:dim(data_nnet1)[1]),dim(data_nnet1)[1]*0.6)
train.df<-data_nnet1[train.index,]
valid.df<-data_nnet1[-train.index,]


library(neuralnet)
library(ggplot2)
library(lattice)
library(caret)

options(scipen = 999)
nn<-neuralnet(as.factor(Rastei_MajmoeDarmad1)~TedadAza+ SarparstSen+ HazineKhoraki+ HazinePoshak+
                HazineMaskan+HazineHamlonaghl+HazineErtebatat+radiozabt+video+computer+panke+mashin+zarfshoyi+internet+hararatmarkazi+coolergazisabet+ M1+M2+M3+M4+M5+M6+M7+M8+M9+F1+F2+F3+F4+F5+F6+NE1+NE2+NE3 ,data = train.df, linear.output = F, hidden = 3)
nn$weights
prediction(nn)
plot(nn, rep="best")
train.pred=compute(nn,train.df[-38])
train.class=apply(train.pred$net.result,1,which.max)-1
confusionMatrix(as.factor(train.class),as.factor(train.df$Rastei_MajmoeDarmad1))
valid.pred=compute(nn,valid.df)
valid.class=apply(valid.pred$net.result,1,which.max)-1
confusionMatrix(as.factor(valid.class),as.factor(valid.df$Rastei_MajmoeDarmad1))
####################################DASTI
#TREE:
library(rpart)
library(rpart.plot)


data<-read.csv(file="data10Tree.csv", header = TRUE)
Quantile = quantile(data$MajmoeDarmad1, probs = seq(0, 1, 0.1))
Dbala = which(data$MajmoeDarmad1>Quantile[7])
data[,24] = rep(0,length(data$TedadAza))
data[,24] = replace(data[,24],Dbala,1)
table(data[,24])
names(data)[24] <- paste("Rastei_MajmoeDarmad1")
newdata <- na.omit(data)

nrow(newdata$Rastei_MajmoeDarmad1)

newdata$Rastei_MajmoeDarmad1

#fixing:
set.seed(1)

#Training and Validation Data sets:
train.index <- sample(c(1:dim(newdata)[1]), dim(newdata)[1]*0.6)
train.index
train.df <- newdata[train.index, ]
nrow(train.df)
valid.df <- newdata[-train.index, ]
dim(valid.df)
ClassifiedTraindata <- rpart(Rastei_MajmoeDarmad1 ~ ., data = train.df, method = "class",model=TRUE)
ClassifiedValiddata <- rpart(Rastei_MajmoeDarmad1 ~ ., data = valid.df, method = "class")
prp(ClassifiedTraindata, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)
text(title("train data"))
# confusion matrix
ClassifiedTraindata.point.pred.train <- predict(ClassifiedTraindata,train.df,type = "class")
# generate confusion matrix for training data
confusionMatrix(ClassifiedTraindata.point.pred.train, as.factor(train.df$Rastei_MajmoeDarmad1))

#LOADING DATA:

data<-read.csv(file="data10PC.csv", header = TRUE)
Quantile = quantile(data$MajmoeDarmad1, probs = seq(0, 1, 0.1))
Dbala = which(data$MajmoeDarmad1>Quantile[7])
data[,79] = rep(0,length(data$TedadAza))
data[,79] = replace(data[,79],Dbala,1)
table(data[,79])
names(data)[79] <- paste("Rastei_MajmoeDarmad1")
newdata <- na.omit(data)

#PCA that we need for next steps:
PCAdata = prcomp(newdata,)
summary.pca = summary(PCAdata)
summary.pca

#for all variables:
pca=prcomp(data.frame(newdata))
summary.pca=summary(pca)
(rot.pca = pca$rot)
s=pca$x
head(s,6)
pca
#
pca.var2 = matrix(rep(0),14,14)
for(i in 1:14){
  p2 = row.names(rot.pca[abs(rot.pca[,i])>quantile(abs(rot.pca[,i]))[4],])
  for(j in 1:14){
    pca.var2[j,i] = p2[j]
  }
}
pca.var2 = data.frame(pca.var2)
View(pca.var2)
pca.var2

#New data set that we have from the PCA method

data2 = data.frame(PCAdata$x[,1:14],newdata$Rastei_MajmoeDarmad1)
data2

#Tree:

library(rpart)
library(rpart.plot)

#fixing:
set.seed(1)

dim(data2)

#Training and Validation Data sets:
train.index <- sample(c(1:dim(data2)[1]), dim(data2)[1]*0.6)
train.index
train.df <- data2[train.index, ]
valid.df <- data2[-train.index, ]

train.df$newdata.Rastei_MajmoeDarmad1 

ClassifiedTrain <- rpart(newdata.Rastei_MajmoeDarmad1 ~ ., data = train.df, method = "class",model=TRUE)
ClassifiedValid <- rpart(valid.df$newdata.Rastei_MajmoeDarmad1~ ., data = valid.df, method = "class")
prp(ClassifiedTrain, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)

#text(title("train data"))
#Confusion matrix:
#ClassifiedTrain.point.pred.train <- predict(ClassifiedTrain,train.df,type = "class")
#Generating the confusion matrix for training data
#confusionMatrix(ClassifiedTrain.point.pred.train, as.factor(train.df$Rastei_MajmoeDarmad1))