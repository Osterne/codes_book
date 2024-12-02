# Subject: EDA
# Lecture: Graphs
# Author: Vinicius Osterne (www.osterne.com)



# all color in R
# https://r-charts.com/colors/


# boxplot
ozone <- airquality$Ozone
temp <- airquality$Temp


boxplot(Temp~Month,
        data=airquality,
        main="",
        xlab="Mês",
        ylab="Temperatura (em Fahrenheit)",
        col="gray",
        border="black"
)


# gráfico de barras
data(mtcars)
attach(mtcars)
table_aux = table(cyl)
table_aux
barplot(table_aux, 
        xlab = "Categoria", ylab = "Frequência absoluta", 
        col = c("grey", "grey", "grey"))



# gráfico de pizza
data(mtcars)
attach(mtcars)
table_aux = table(cyl)
table_aux
pie(table_aux,
    col = c("snow2", "snow3", "snow4"))
















#https://rpubs.com/EstatBasica/Cap12

x<-seq(125,167,0.1)  # eixo X do gráfico
regiao1=seq(148.42,167,0.01) # Regiao a ser sombreada na figura
cord.x <- c(min(regiao1),regiao1,max(regiao1))  # coordenadas X da regiao
cord.y <- c(0,dnorm(regiao1,mean=145,sd=5.76),0)  # coordenadas Y da regiao
curve(dnorm(x,mean=145,sd=5.76),xlim=c(125,167),ylim=c(0,0.12),xlab="",ylab="",xaxs="i",yaxs="i",col="black",lwd=2, xaxt='n')  # Curva da distribuicao normal
polygon(cord.x,cord.y,col='orange2') # plotando a regiao
lines(x=c(145,145),y=c(0,dnorm(145,145,5.76)),col="gray",lty=2) # linha vertical da media  

par(new=TRUE) # inserindo a segunda curva 
regiao2=seq(125,148.42,0.01) # Regiao a ser sombreada na figura
cord.x <- c(min(regiao2),regiao2,max(regiao2))
cord.y <- c(0,dnorm(regiao2,mean=155,sd=4),0) 
curve(dnorm(x,mean=155,sd=4),xlim=c(125,167),ylim=c(0,0.12),xlab="",ylab="",xaxs="i",yaxs="i",col="black",lwd=2, xaxt='n') 
polygon(cord.x,cord.y,col='lightblue3')
lines(x=c(155,155),y=c(0,dnorm(155,155,4)),col="darkgray",lty=2)
axis(side=1,at = c(145,148.42,155), labels = c("145","148.42","155")) # 'labels' do eixo X
legend("topleft", legend=c("RC=5%", "Poder=7.93%"), pch=c(15,15), col=c("lightblue3","orange2"), cex=2, bty = "n") # legenda




x<-seq(125,167,0.1)
regiao1=seq(125,148.42,0.01) # Regiao a ser sombreada na figura
cord.x <- c(min(regiao1),regiao1,max(regiao1))
cord.y <- c(0,dnorm(regiao1,mean=155,sd=4),0) 
curve(dnorm(x,mean=155,sd=4),xlim=c(125,167),ylim=c(0,0.15),xlab="",ylab="",xaxs="i",yaxs="i",col="black",lwd=2, xaxt='n') 

polygon(cord.x,cord.y,col='lightblue3')
lines(x=c(155,155),y=c(0,dnorm(155,155,4)),col="darkgray",lty=2)
axis(side=1,at = c(148.42,155), labels = c("148.42","mu=155"))

par(new=TRUE)

# Curva tracejada
curve(dnorm(x,mean=148.42,sd=3),xlim=c(125,167),ylim=c(0,0.15),xlab="",ylab="",xaxs="i",yaxs="i",col="gray",lty=2, lwd=2, xaxt='n') 

par(new=TRUE)
# Curva tracejada
curve(dnorm(x,mean=145,sd=3),xlim=c(125,167),ylim=c(0,0.15),xlab="",ylab="",xaxs="i",yaxs="i",col="gray",lty=2, lwd=2, xaxt='n') 




x<-seq(130,190,0.1)
regiao2=seq(130,147.16,0.01)
regiao1=seq(162.84,190,0.01)
cord.x1 <- c(min(regiao1),regiao1,max(regiao1))
cord.x2 <- c(min(regiao2),regiao2,max(regiao2))
cord.y1 <- c(0,dnorm(regiao1,mean=155,sd=4),0) 
cord.y2 <- c(0,dnorm(regiao2,mean=155,sd=4),0) 
curve(dnorm(x,mean=155,sd=4),xlim=c(125,190),ylim=c(0,0.15),xlab="",ylab="",xaxs="i",yaxs="i",lwd=2, xaxt='n') 
polygon(cord.x1,cord.y1,col='lightblue3')
polygon(cord.x2,cord.y2,col='lightblue3')
lines(x=c(155,155),y=c(0,dnorm(155,155,4)),col="darkgray",lty=2)
axis(side=1,at = c(147.16,155,162.84), 
     labels = c("X_c1","155","X_c2"))

par(new=TRUE)
# Curva tracejada
curve(dnorm(x,mean=145,sd=4.5),xlim=c(130,190),ylim=c(0,0.15),xlab="",ylab="",xaxs="i",yaxs="i",col="gray",lty=2, lwd=2, xaxt='n') 
par(new=TRUE)
# Curva tracejada
curve(dnorm(x,mean=140,sd=3),xlim=c(130,190),ylim=c(0,0.15),xlab="",ylab="",xaxs="i",yaxs="i",col="gray",lty=2, lwd=2, xaxt='n') 
par(new=TRUE)
# Curva tracejada
curve(dnorm(x,mean=170,sd=3),xlim=c(130,190),ylim=c(0,0.15),xlab="",ylab="",xaxs="i",yaxs="i",col="gray",lty=2, lwd=2, xaxt='n') 
par(new=TRUE)
# Curva tracejada
curve(dnorm(x,mean=175,sd=4.5),xlim=c(130,190),ylim=c(0,0.15),xlab="",ylab="",xaxs="i",yaxs="i",col="gray",lty=2, lwd=2, xaxt='n') 

