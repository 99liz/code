rm(list=ls())

#Import the data
url <- "https://raw.githubusercontent.com/99liz/code/main/BahamasFish.csv"
mydata <- read.csv(url)
View(mydata)

#data file with ONLY the dependent variables
dependents<-mydata[,2:5]
View(dependents)

#convert to z-scores
scaled.data<-scale(dependents, scale=TRUE, center=TRUE)


#Run the PCA
PCAmodel<-princomp(scaled.data, cor=FALSE)

summary(PCAmodel) #shows amount of variance explained by each axis
loadings(PCAmodel) #shows loadings of each fish species on each PC
plot(PCAmodel, type="lines") #scree plot
library(factoextra)
fviz_eig(PCAmodel)

#to do ANOVA on PC1 scores
PC1<-PCAmodel$scores[,1] #asks for the first column of the table 
PC1
model1<-lm(PC1~mydata$Site)
anova(model1)

#test
install.packages("agricolae")
library(agricolae)
HSD.test(model1, "mydata$Site", console=TRUE)


#factor
mydata$Site <- factor(mydata$Site)

#graph
library(ggbiplot)
ggbiplot(PCAmodel, obs.scale=1, var.scale=1, groups=mydata$Site, ellipse=TRUE, varname.size=3, varname.adjust=1.2, circle=FALSE) + 
  scale_color_discrete(name='') +
  geom_point(aes(colour=factor(mydata$Site)), size = 1) +
  theme(legend.direction = 'vertical', legend.position='right', legend.text=element_text(size=4))

