#Please setwd() and path to the location where the folder T=1s is saved

library(gstat)
library(sp)
library(viridis)
library(RColorBrewer)

Data=read.csv("Data_UTM.csv")
plot(Data$longitude,Data$latitude)

coordinates(Data)= ~ longitude+latitude

Variogram=variogram(dS2S~1, data=Data,width=3300,cutoff=100000)
plot(Variogram)
Variogram_Model <-vgm(model="Sph")
Fitted_Model <- fit.variogram(Variogram, model=Variogram_Model,fit.method = 2)
plot(Variogram, model=Fitted_Model,pch=20,col="black",lwd=2,xlab="",ylab="",ylim=c(0,0.65),cex=0.6)
Fitted_Model

Gstat <-gstat(id="dS2S",formula=dS2S~1,data=Data)
Gstat <-gstat(Gstat,id="dS2S",model=Fitted_Model)

Data_grid=read.csv("Grid_UTM.csv")
cols=Data_grid$longitude
rows=Data_grid$latitude
Grid<-data.frame(x=cols,y=rows)
coordinates(Grid)<- ~x+y
plot(Grid,cex=0.5)
points(Data,pch=1,col="red",cex=0.7)

# Ordinary Kriging Global Neighborhood
Kriging_g <- krige(dS2S~1, Data, Grid, model = Fitted_Model)
spplot(Kriging_g["var1.pred"], main = "global ordinary kriging predictions")

lat<-coordinates(Grid)[,2]
lon<-coordinates(Grid)[,1]
dS2S_T1<-Kriging_g$var1.pred
AF_T1<-exp(Kriging_g$var1.pred)
var_T1<-Kriging_g$var1.var
sd_T1<-sqrt(Kriging_g$var1.var)
results <- cbind(lat,lon,dS2S_T1,AF_T1,var_T1,sd_T1)
write.csv(results, file.path(path,"OK_g_T1.csv"))





