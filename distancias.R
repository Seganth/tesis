####Paquetes a usar#####


if (!require("ggtern")) install.packages("ggtern")
suppressMessages(library(ggtern))
if (!require("compositions")) install.packages("compositions")
suppressMessages(library(compositions))
if (!require("Ternary")) install.packages("Ternary")
suppressMessages(library(Ternary))


n=3
#definir centro
cero<-acomp(c(1,1,1))
#probar distancias
u<-acomp(c(.4,.3,.3))
plot(cero)
TernaryPlot()
TernaryPoints(cero)
TernaryPoints(u)

disait<-function(a,b){
  n<-length(a)
  suma=0
  dena<-geometricmean(a)
  denb<-geometricmean(b)
  for (i in 1:n) {
    suma=suma+ (log(a[i]/dena)-log(b[i]/denb))^2
  }
  return(sqrt(suma))
}

disait(cero,u)

cerot<-clr(cero)
ut<-clr(u)

norm(cerot-ut) # validación norma euclidiana de trans. vs norma aitchison

u<-acomp(c(.9999998,.0000001,.0000001))
disait(cero,u)


###### Centrado en cero ###########

#definir distancia
d=.5
# intervalos de x 
a=-sqrt(2/3)*d
b=+sqrt(2/3)*d

#partición para x
n=50
x=seq(from=a, to=b, length.out = n)
#calculo y, z
y1=-x/2+0.5*sqrt(2*d^2-3*x^2)
y2=-x/2-0.5*sqrt(2*d^2-3*x^2)
z1=y2
z2=y1
#concentrar en una tabla
datU=data.frame(x=c(x,x),y=c(y1,y2),z=c(z1,z2))
plot(datU)
matU=as.matrix(datU)

round(x+y1+z1,2)
dtest=rep(0,2*n)
for (i in 1:(2*n)){dtest[i]=norm(matU[i,])}
dtest

#graficar en U
#install.packages("plot3D")
#library(plot3D)
scatter3D(datU$x,datU$y,datU$z)

#transformar a simplex
matS=clrInv(datU)
datS=data.frame(matS)

#corroborar distancia aitchison
dtest=rep(0,2*n)
for (i in 1:(2*n)){dtest[i]=disait(cero,matS[i,])}
dtest


#graficar en simplex
TernaryPlot()
TernaryPoints(cero)
TernaryLines(datS[c(1:50),])
TernaryLines(datS[c(51:100),])

bol0<-function(n,d){
  # intervalos de x 
  a=-sqrt(2/3)*d
  b=+sqrt(2/3)*d
  #partición para x
  x=seq(from=a, to=b, length.out = n/2)
  #calculo y, z
  y1=-x/2+0.5*sqrt(2*d^2-3*x^2)
  y2=-x/2-0.5*sqrt(2*d^2-3*x^2)
  z1=y2
  z2=y1
  datU=data.frame(x=c(x,x),y=c(y1,y2),z=c(z1,z2))
  matS=clrInv(datU)
  datS=data.frame(matS)
  TernaryPlot()
  TernaryPoints(cero)
  TernaryLines(datS[c(1:(n/2)),])
  TernaryLines(datS[c((n/2+1):n),])
  return(datS)
}

temp=bol0(100,.8)



###### Centrado en otro punto ###########

#definir centro
x0=c(.1,.2,.7)
u=clr(x0)
#definir distancia
d=.5
# intervalos de x 
a=-sqrt(2/3)*d+u[1]
b=+sqrt(2/3)*d+u[1]

#partición para x
n=50
x=seq(from=a, to=b, length.out = n)
#calculo y, z
y1=-(x-u[1])/2+0.5*sqrt(2*d^2-3*(x-u[1])^2)+u[2]
y2=-(x-u[1])/2-0.5*sqrt(2*d^2-3*(x-u[1])^2)+u[2]
z1=-(x-u[1])/2-0.5*sqrt(2*d^2-3*(x-u[1])^2)+u[3]
z2=-(x-u[1])/2+0.5*sqrt(2*d^2-3*(x-u[1])^2)+u[3]
#concentrar en una tabla
datU=data.frame(x=c(x,x),y=c(y1,y2),z=c(z1,z2))
plot(datU)
matU=as.matrix(datU)

round(x+y1+z1,2)
dtest=rep(0,2*n)
for (i in 1:(2*n)){dtest[i]=norm(matU[i,]-u)}
dtest

#graficar en U
#install.packages("plot3D")
#library(plot3D)
scatter3D(datU$x,datU$y,datU$z)

#transformar a simplex
matS=clrInv(datU)
datS=data.frame(matS)

#corroborar distancia aitchison
dtest=rep(0,2*n)
for (i in 1:(2*n)){dtest[i]=disait(x0,matS[i,])}
dtest


#graficar en simplex
TernaryPlot()
TernaryPoints(x0)
TernaryLines(datS[c(1:50),])
TernaryLines(datS[c(51:100),])



bolx0<-function(x0,n,d){
  #transformar centro
  u=clr(x0)
  # intervalos de x 
  a=-sqrt(2/3)*d+u[1]
  b=+sqrt(2/3)*d+u[1]
  #partición para x
  x=seq(from=a, to=b, length.out = n/2)
  #calculo y, z
  y1=-(x-u[1])/2+0.5*sqrt(2*d^2-3*(x-u[1])^2)+u[2]
  y2=-(x-u[1])/2-0.5*sqrt(2*d^2-3*(x-u[1])^2)+u[2]
  z1=-(x-u[1])/2-0.5*sqrt(2*d^2-3*(x-u[1])^2)+u[3]
  z2=-(x-u[1])/2+0.5*sqrt(2*d^2-3*(x-u[1])^2)+u[3]
  datU=data.frame(x=c(x,x),y=c(y1,y2),z=c(z1,z2))
  matS=clrInv(datU)
  datS=data.frame(matS)
  TernaryPlot()
  TernaryPoints(x0)
  TernaryLines(datS[c(1:(n/2)),])
  TernaryLines(datS[c((n/2+1):n),])
  return(datS)
}

temp=bolx0(c(.2,.4,.4),100,.5)
