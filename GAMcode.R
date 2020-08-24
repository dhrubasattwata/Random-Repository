# install.packages("mgcv")
# install.packages("nlme")
library(nlme)
library(mgcv)
library(ggplot2)
library(Metrics)

## Input Dataset
dat <- read.csv("C:\\Users\\dhchoudhury\\Desktop\\Citi ICG\\FMV\\133283\\GAM Model\\Italy Roll Rate\\RR_Italy_Gam.csv")
# View(dat)

## Dataset to be used for Prediction
test <- read.csv("C:\\Users\\dhchoudhury\\Desktop\\Citi ICG\\FMV\\133283\\GAM Model\\Italy Roll Rate\\Italy_Roll_Rate.csv")
# View(test)

### Prediction
bdg <- gam(RR~s(UR,k=4)+s(CUR,k=4)+s(HPA,k=4)+factor(LTV)+factor(DTI), data=dat)
prdct <- predict(bdg, newdata=data.frame(test))
# View(prdct)


## Graph Generation
projection <- read.csv("C:\\Users\\dhchoudhury\\Desktop\\Citi ICG\\FMV\\133283\\GAM Model\\Italy Roll Rate\\Comparison.csv")
projection$GAM_Model <- prdct

prjct <- cbind(test,projection)
# View(prjct)

prjct1 <- prjct[106:158,3:9]
View(prjct1)


b1 <- as.Date(prjct1$Date, format = "%m/%d/%Y")
b2 <- prjct1$UR
b3 <- prjct1$CUR
b4 <- prjct1$HPA
b5 <- prjct1$Projection
b6 <- prjct1$Actual
b7 <- prjct1$GAM_Model

a <-  data.frame(b1 = as.Date(b1, format = "%m/%d/%Y"),b2,b3,b4,b5,b6,b7)
colnames(a) <- c("Date", "UR","CUR", "HPA","Projection","Actual","GAM_Model")
View(a)

ggplot(data=a, aes(x=CUR)) +
  geom_line(aes(y=Projection,col="Bezier Model")) +
  geom_line(aes(y=Actual,col="Actual")) +
  geom_line((aes(y=GAM_Model,col="GAM_Model")))+
  xlab("Change of Unemployment Rate QoQ") +
  ylab("Values") +
  ggtitle("Comparison of Roll Rate by Unemp. Rate Change")+ 
  theme(
    plot.title = element_text(color="red", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=10, face="bold"),
    axis.title.y = element_text(color="black", size=10, face="bold")
  )
