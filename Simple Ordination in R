##### Simple ordination of community data in R
rm(list=ls())
##### Package installation
install.packages('vegan') # CCA, rda function 
install.packages('readxl') 
install.packages('ggplot2') 
install.packages('ggrepel') 
install.packages('FactoMineR') # CA function
install.packages('factoextra') 
install.packages('devtools') 
library(devtools)
install_github("vqv/ggbiplot") # ggbiplot function

##### Load packages
library(vegan)
library(readxl) # for read excel file
library(ggbiplot)
library(ggplot2)
library(ggrepel)
library("FactoMineR")
library("factoextra")


#### Set directory
base <- "C:/" # Set your directory where your csv or excel file is located
setwd(base)


#####################################################################
################################### 1.Load data
##### Warning: Data must not contain rows or columns which total sum is zero

tot.data <- as.data.frame(read.csv(paste(base, "Sample data.csv", sep = "/")))

##### Remove rows which contain NA
tot.data <- na.omit(tot.data)

##### Extract time series from dataframe
date <- tot.data[,2] # Allocate time column of dataframe
date<- as.Date(date, origin="1970-01-01")

##### Extract dependent var(community data) from dataframe 
dep.var <- tot.data[,3:10]

##### Extract independent var(environment data) from dataframe 
indep.var <- tot.data[,11:18]

##### Scaling community data (Hellinger-transformation), perform if data is quantitative(abundance, density), recommended in ecology
dep.var.hel<-decostand(dep.var, "hellinger")

#####################################################################
################################### 2. Execute ordination
##### Warning: Generally CA, CCA is for occurence data!, if your data is quantitiative perfrom PCA, RDA
##### 2.1 PCA
##### Create labels for PCA visualization

##### Create month and year label
month <- as.numeric(strftime(date, "%m"))
year <- as.numeric(strftime(date, "%y"))

##### Create season label
getSeason <- function(DATE) {
  WS <- as.Date("2012-07-01", format = "%Y-%m-%d")
  SE <- as.Date("2012-10-01",  format = "%Y-%m-%d")
  d <- as.Date(strftime(DATE, format="2012-%m-%d"))
  ifelse (d < WS | d >= SE, "Non Summer", 'Summer')
}
date.season<-getSeason(date)

##### Execute PCA for independent var
indep.var.pca <- prcomp(indep.var, center = TRUE,scale. = TRUE) # 표준화가 필요하지 않은 경우 scale. = FALSE로 설정

##### Plot PCA(indep)
indep.var.pca.plot<-ggbiplot(indep.var.pca, choices=c(1,2),ellipse = TRUE, obs.scale =1, var.scale =1,groups = date.season , varname.adjust = 2)+
  ggtitle('PCA of environmental variables')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
indep.var.pca.plot

##### Execute PCA for dependent var
dep.var.pca <- prcomp(dep.var.hel, center = TRUE,scale. = FALSE)

##### Plot PCA(dep)
dep.var.pca.plot <- ggbiplot(dep.var.pca, choices=c(1,2),ellipse = TRUE, groups = date.season, varname.adjust = 1,legend = 'top')+
  ggtitle('PCA of community data')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dep.var.pca.plot

##### 2.2 Execute RDA
fauna.rda <- rda(dep.var.hel ~., indep.var)

##### Test RDA model significance, model is significant if p<0.05  
anova(fauna.rda, step = 10000, per.max = 10000)

##### Plot RDA
plot(fauna.rda, scaling = 1, display = c('sp','bp'))

##### 2.3 CA
##### CA(dep)
CA(dep.var.hel, ncp = 5, graph = TRUE)

##### CA(indep)
CA(indep.var, ncp = 5, graph = TRUE)

##### 2.4 Exectue CCA
CCA<-cca(dep.var.hel, indep.var)

##### Test CCA model significance, model is significant if p<0.05
anova.cca(CCA)

##### Plot CCA
plot(CCA)
