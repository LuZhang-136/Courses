library(drc)
rm(list=ls())
dat_csv<-read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv",header=TRUE)
dat_csv<-dat_csv[-c(174:dim(dat_csv)[1]),]
days<-dim(dat_csv)[1]
dat_csv$t<-1:days
###keep the entire db
db<-dat_csv

fit<- drm(deceduti~ t, data = db[c(1:140),], fct = LL.5(c(NA,0,NA,NA,NA)), 
          type='Poisson', control=drmc(maxIt=1000))
db$data<-as.Date(substr(db$data,1,10),"%Y-%m-%d")

db_plot<-data.frame(deceduti=db$deceduti[1:140],curve=predict(fit),data=db$data[1:140])
library(ggplot2)
ggplot(data=db_plot, 
       aes(x=data, y=curve))+geom_line()+ylab("Decessi per COVID-19")+xlab("Data")+
  geom_segment(aes(y =0,x=db_plot$data[40],yend = predict(fit,newdata=data.frame(t=fit$coefficients[3]))-1000,xend =db_plot$data[40]),arrow = arrow(length = unit(0.3, "cm")))+
  geom_hline(yintercept=fit$coefficients[2], linetype="dashed", color = "red", size=1)+
  geom_hline(yintercept=predict(fit,newdata=data.frame(t=fit$coefficients[3])), linetype="dashed", color = "red", size=1)+
  annotate("text", x = db_plot$data[8], y = 25000, label = "Parte logaritmica",col="red")+
  annotate("text", x = db_plot$data[8], y = 10000, label = "Parte esponenziale",col="red")+
  annotate("text", x = db_plot$data[47], y = -500, label = "Punto di flesso",col="red")+
  annotate("text", x = db_plot$data[55], y = 37500, label = "Asintoto superiore",col="red")+
  geom_segment(aes(x=db_plot$data[23], xend=db_plot$data[33], y=10000, yend=10000), 
               arrow = arrow(length = unit(0.3, "cm")))+
  geom_segment(aes(x=db_plot$data[23], xend=db_plot$data[53], y=25000, yend=25000), 
               arrow = arrow(length = unit(0.3, "cm")))+
  geom_segment(aes(x=db_plot$data[70], xend=db_plot$data[100], y=36500, yend=36000), 
               arrow = arrow(length = unit(0.3, "cm")))+
  geom_point(aes(x=data, y=deceduti))
