

suppressMessages(library(ggtern))
set.seed(1)
plot <- ggtern(data = data.frame(x = runif(100),
                                 y = runif(100),
                                 z = runif(100)),
               aes(x, y, z))
plot + stat_density_tern(geom='polygon',
                         n         = 200,
                         aes(fill  = ..level..,
                             alpha = ..level..)) +
  geom_point() +
  theme_rgbw() +
  labs(title = "Example Density/Contour Plot")    +
  scale_fill_gradient(low = "blue",high = "red")  +
  guides(color = "none", fill = "none", alpha = "none")


#Datos simplex aleatorios
set.seed(1906)
x<-runif(100)
temp<-runif(100)
y<-(1-x)*temp
z<-(1-x)*(1-temp)

plot <- ggtern(data = data.frame(x = x,
                                 y = y,
                                 z = z),
               aes(x, y, z))
plot + stat_density_tern(geom='polygon',
                         n         = 200,
                         aes(fill  = ..level..,
                             alpha = ..level..)) +
  geom_point() +
  theme_rgbw() +
  labs(title = "Example Density/Contour Plot")    +
  scale_fill_gradient(low = "blue",high = "red")  +
  guides(color = "none", fill = "none", alpha = "none")

#####Datos simple puntos#####

nombre<-c("A")

x<-c(1/5)
y<-c(2/5)
z<-c(2/5)
plot <- ggtern(data = data.frame(x = x,
                                 y = y,
                                 z = z),
               aes(x, y, z))
plot+geom_point()+
  geom_text(aes(label=nombre),hjust=1, vjust=0) +
  theme_light()

#####Datos simple sumas#####

suppressMessages(library(compositions))
x<-c(0.5,0.1,0.4)
y<-c(.1,.3,.6)#c(1/3,1/3,1/3)
z<-c(.4,.6,0)#c(0.125,0.125,0.75)
nombre<-c("A","B","C")

x<-c(5/8,1/9,1/10)
y<-c(1/8,6/9,1/10)#c(1/3,1/3,1/3)
z<-c(2/8,2/9,8/10)#c(0.125,0.125,0.75)
plot <- ggtern(data = data.frame(x = x,
                                 y = y,
                                 z = z),
               aes(x, y, z))
plot+geom_point()+
  geom_text(aes(label=nombre),hjust=1, vjust=0) +
  theme_light()

datos<-data.frame(x = x, y = y, z = z,row.names = nombre)

AmasB<-perturbe(as.matrix(datos[1,]),as.matrix(datos[2,]))
BmasC<-perturbe(as.matrix(datos[2,]),as.matrix(datos[3,]))
AmasC<-perturbe(as.matrix(datos[1,]),as.matrix(datos[3,]))

oper<-rbind(AmasB,BmasC,AmasC)

nombre2<-c(nombre,"A+B","B+C","A+C")
datos2<-data.frame(rbind(as.matrix(datos), oper),row.names = nombre2)
plot <- ggtern(data = datos2,
               aes(x, y, z))
plot+geom_point()+
  geom_text(aes(label=nombre2),hjust=1, vjust=0) +
  theme_gray()
#####Datos simple potencias#####

A<-c(36/100,33/100,31/100)
pot<-seq(1,30)
n<-length(pot)
predat<-matrix(0,n,3)
#predat[1,]<-A
for (i in pot) {
  predat[i,] <-power.acomp(A,pot[i])
}

datpot<-data.frame(predat)
plot<-ggtern(data=datpot, aes(X1,X2,X3))
plot+geom_point()#+geom_text(aes(label=row.names(datpot)),hjust=1,vjust=-2,size=2)

#####Datos simple perturbacion sucesiva#####

A<-c(54/100,22/100,24/100)
B<-c(23/100,23/100,54/100)
pot<-seq(1,10)
n<-length(pot)
predat<-matrix(0,n+2,3)
predat[1,]<-A
predat[n+2,]<-B
for (i in pot+1) {
  if (i %% 2==0) {
    predat[i,] <-perturbe(predat[i-1,],B)
  }else{
    predat[i,] <-perturbe(predat[i-1,],A)
  }
  
}
datnper<-data.frame(predat)
plot<-ggtern(data=datnper, aes(X1,X2,X3))
plot+geom_point()+geom_text(aes(label=c("A",pot,"B")),hjust=1,vjust=-2,size=2)

