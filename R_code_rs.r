#R code for remote sensing
install.packages("raster")
install.packages("RStoolbox")
install.packages("rgdal")
install.packages("ggplot2")
#I am using Windows
setwd("C:/lab")
getwd()
library(raster)
b2011<-brick("p224r63_2011_masked.grd"); View(b2011)
plot(b2011)
#changing the legend
# B1: blue
# B2: green
# B3: red
# B4: near infrared (nir) 
# B5: medium infrared
# B6: thermal infrared
# B7: medium infrared
#to use black and white:
cl <- colorRampPalette(c('black','grey','light grey'))(100) 
plot(b2011, col=cl)
#using different colors with the par function)
par(mfrow=c(2,2))
clb<-colorRampPalette(c("dark blue","blue","light blue"))(100)
plot(b2011$B1_sre, col=clb)
clg<-colorRampPalette(c("dark green","green","light green"))(100)
plot(b2011$B2_sre, col=clg)
clr<-colorRampPalette(c("dark red","red","pink"))(100)
plot(b2011$B3_sre, col=clr)
cln<-colorRampPalette(c("red","orange","yellow"))(100)
plot(b2011$B4_sre, col=cln)
#to see also the near infrared, mount the nir on the red band
plotRGB(b2011,r=4,g=3,b=2,stretch="Lin")
#comparing the picture to an older one of the same area
b1988<-brick("p224r63_1988_masked.grd"); View(b1988)
plot(b1988)
par(mfrow=c(2,1))
plotRGB(b1988,r=4,g=3,b=2,stretch="Lin")
plotRGB(b2011,r=4,g=3,b=2,stretch="Lin")
#stretching the queues of the colour disistribution, to enhance the noise
plotRGB(b1988,r=4,g=3,b=2,stretch="hist")
plotRGB(b2011,r=4,g=3,b=2,stretch="hist")
#calculating the spectral indices DVI for the 2 years
#NDVI=(NIR1-RED1)/(NIR2-RED2)
dvi1988<-b1988$B4_sre-b1988$B3_sre
dvi2011<-b2011$B4_sre-b2011$B3_sre
par(mfrow=c(2,1))
cldvi<-colorRampPalette(c("red","orange","yellow"))(100)
plot(dvi1988,col=cldvi)
plot(dvi2011,col=cldvi)
difdvi<-dvi2011-dvi1988
cldif<-colorRampPalette(c("blue","white","red"))(100)
plot(difdvi,col=cldif)
library(RStoolbox)
#PCA on raster data
#increasing pixels dimention
res2011<-aggregate(b2011,fact=10)
par(mfrow=c(2,1))
plotRGB(b2011,r=4,g=3,b=2,stretch="Lin")
plotRGB(res2011,r=4,g=3,b=2,stretch="Lin")
b2011
res2011
#PCA
b11PCA<-rasterPCA(res2011);summary(b11PCA$model)
plotRGB(b11PCA$map,r=4,g=3,b=2,stretch="Lin")
plot(b11PCA$map)
#evaluating land cover
#unsupervised classification, based on Maximum likelihood
b2011c<-unsuperClass(b2011,nClasses=5)
clclass<-colorRampPalette(c("red","green","yellow","blue","black"))(100)
plot(b2011c$map,col=clclass)
