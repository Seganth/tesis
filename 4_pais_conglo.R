# paquetes ####
library(tidyverse)
library(ggrepel)
library(ggsci) # paletas de colores de journals
library(scales) # para break formatting functions, show_col
library(ggplot2)
library(countrycode)
library(compositions)
library(xtable)

# Funciones
nivtrans<-function(x,nivel="Nivel"){# para ordenar niveles de ingreso
  x %>% mutate(Nivel=factor(get(nivel),levels = c("Bajo","Medio-bajo","Medio-alto","Alto","México")))
}
huecos<-function(x){ #resumen países con huecos en su serie
  x %>% #pivot_longer(!c(code,year,Nivel),names_to="serie") %>% 
    group_by(code) %>% 
    summarise(n=n_distinct(year),miny=min(year),maxy=max(year),huecos=maxy-miny-n+1) %>% 
    filter(huecos>0)
}

obsres<-function(x){# evolución de total de observaciones tras añadir cada serie
  obs<-rep(0,length(names(x)))
  names(obs)<-names(x)
  
  for (i in names(x)) {
    temp<-x %>% select(code:as.name(i))
    obs[i]<-dim(temp %>% na.omit())[1]
  }
  res<-as.data.frame(obs)
  res$delta<-c(0,diff(res$obs))
  return(res)
}

codeobsres<-function(x){# resumen de países ordenado por número de años obs (menor a mayor)
  x %>% group_by(code) %>% summarise(n=n_distinct(year)) %>% arrange(n)
}
codemed<-function(codigo,metodo=TRUE){ #media composicional por país
  dat<-amalgA %>%as.data.frame() %>% 
    rownames_to_column("id") %>%
    separate(col="id",into = c("code","year")) %>% select(-year) %>% 
    filter(code==codigo) %>% select(-code) %>% 
    acomp()
  resultado<-mean(dat,robust=metodo) %>% as.matrix()
  return(resultado)
}


# Directorio
setwd("C:/Users/sgome/Dropbox/#tesis")

# Lectura de archivos new ####

consolidado=read.csv(file = "./Data/consolidado2b.csv",header = TRUE) %>% #sin filtrar NAs
  nivtrans()


minval<-consolidado %>% filter(across(def:soc,~.>0)) %>% summarise(across(def:soc,~min(.)/10,na.rm=TRUE)) %>% 
  pivot_longer(cols = def:soc,names_to="fun",values_to="min")


# Logratio Analysis ####
funciones <- consolidado %>%
  mutate(Nivel=ifelse(code=="TWN","Alto",as.character(Nivel))) %>% #completar Taiwan
  mutate(cont=countrycode(code,"genc3c","continent")) %>% #completar continente
  mutate(cont=ifelse(code=="XKS","Europe",cont)) %>% #completar continete para Kosovo
  #mutate(Nivel=ifelse(code=="MEX","México",Nivel)) %>% #México en categoría separada
  mutate(across(c(def:soc),~ifelse(.==0,min(minval$min),.))) %>% #imputación valores pequeños
  select(code:soc,gto2,cre) %>% filter(across(code:gto2,~!is.na(.))) %>% nivtrans()
obsres(funciones)
codeobsres(funciones)
levels(funciones$Nivel)

# 1	Descriptive ####
gfun_comp<-funciones %>% unite("id",code:year,sep = "'") %>% column_to_rownames("id") %>% 
  select(def:soc) %>% acomp()

gfun_sub<-funciones %>% unite("id",code:year,sep = "'") %>% column_to_rownames("id") %>% 
  mutate(otros=seg+viv+cul+amb+def) %>% 
  #select(soc,gob,edu,sal,eco,otros) %>% # con amalgamación otros
  select(soc,gob,edu,sal,eco) %>% # subcomposición
  acomp()

# 2.2	Variable Selection ####

orden<-mean(gfun_comp) %>% sort(decreasing = T) %>% names()

varD<-variation(gfun_comp[,orden])
varD

tabla<-varD %>% as.data.frame() %>% bind_rows(mean(gfun_comp)) %>%
  bind_rows(cumsum(mean(gfun_comp[,orden])))
print(xtable(tabla,digits=2))
#min(varD);max(varD)
#summary(gfun_comp)

vardend<-varD %>% as.dist() %>% hclust(method = "ward.D") %>% 
  as.dendrogram()

pdf("./TeX/Fig/4.3.2_1_vardend.pdf",width = 7,height = 3.5)
par(mar=c(2,3,1,1))
plot(vardend)
dev.off() 


# correlaciones con crecimiento pib
head(amalgA)
head(amalgA[,1:3])
indice<-!is.na(funciones$cre)

partes<-names(gfun_comp)
sapply(partes,function(x) cor(gfun_comp[indice,x],funciones[indice,]$cre)) %>% sort(decreasing = T)
head(alr(gfun_comp[indice,c(1:4,6:10,5)])$"soc")
cor(gfun_comp[indice,"def"],funciones[indice,]$cre)
partes[-5]

temp<-alr(gfun_comp[indice,c(1:4,6:10,5)]) %>% as.data.frame()
sapply(partes[-5],function(x) cor(temp[,x],
                                  funciones[indice,]$cre)) %>% sort(decreasing = T)


# amalgama para simplificar ####

amalgA<-groupparts(gfun_comp,"resto"=c("seg","def","amb","cul","viv"))
orden<-(mean(amalgA) %>% sort(decreasing = T) %>% names())[c(1,3:6,2)]
amalgA<-amalgA[,orden]
mean(amalgA)
# resumen variables seleccionadas ####

summary(alr(amalgA))
library(compositions)



png("./TeX/Fig/4.3.2_2_compscatter.png",width = 1080,height = 1080)

plot(amalgA,margin = "gob",cex=0.5)
r = sqrt( qchisq(p=0.95,df=2) )
mn = mean(amalgA,robust = T)
vr = var(amalgA,robust = T)
ellipses(mean=mean(amalgA), var=var(amalgA), r=r, lwd=2 ,col="green")
ellipses(mean=mn, var=vr, r=r, lwd=2 ,col="red")
dev.off()

library(psych)
tabla<-describe(alr(amalgA))[,c(3:5,8:12)]
tabla
print(xtable(tabla))

# normalidad preliminar ####
library(MVN)
mvn(alr(amalgA),univariateTest = "Lillie",mvnTest = "dh",desc = T)
mvn(alr(amalgA)[,2:4],univariateTest = "Lillie",mvnTest = "dh",desc = T)

qchisq(.95,2)

temp<-mvn(alr(amalgA)[,c(2,1)],univariateTest = "Lillie",mvnTest = "dh",desc = F,multivariatePlot = "contour")
mvn(alr(amalgA)[,c(1,2)],univariateTest = "Lillie",mvnTest = "dh",desc = F,multivariatePlot = "contour")
mvn(alr(amalgA)[,c(2,3)],univariateTest = "Lillie",mvnTest = "dh",desc = F,multivariatePlot = "contour")
mvn(alr(amalgA)[,c(2,4)],univariateTest = "Lillie",mvnTest = "dh",desc = F,multivariatePlot = "contour")
mvn(alr(amalgA)[,c(3,4)],univariateTest = "Lillie",mvnTest = "dh",desc = F,multivariatePlot = "contour")

X<-amalgA[,c(2:4,6)]
head(amalgA)
head(alr(X))
mvn(alr(X),univariateTest = "Lillie",mvnTest = "hz",desc = F)
library(robCompositions)
out<-outCoDa(X)$outlierIndex
mvn(alr(X)[!out,c(2,3)],univariateTest = "Lillie",mvnTest = "hz",desc = F,multivariatePlot = "contour")

# media de paises ####
lpaises<-unique(funciones$code)
nobs<-funciones %>% group_by(code) %>% summarise(n=n())
lpaisesA<-nobs$code[nobs$n>=2*(ncol(amalgA)-1)] # con 5 o más datos
lpaisesB<-nobs$code[nobs$n<2*(ncol(amalgA)-1)] # con menos de 5 datos
media<-sapply(lpaises, function(x) if (x%in%lpaisesA) codemed(x,metodo = T) else codemed(x,metodo=F)) %>% t() %>% 
  as.data.frame()


paises<-read.csv("./Data/nomb_paises.csv",encoding = "UTF-8") %>% 
  merge( funciones %>%group_by(code) %>%  mutate(Nivel=last(Nivel)) %>% select(code,Nivel) %>% unique()) %>% 
  mutate(ni=Nivel)
levels(paises$ni)<-list(B="Bajo",MB="Medio-bajo",MA="Medio-alto",A="Alto")

rownames(media)<-paises$esp
names(media)<-names(amalgA)
head(media)
mean(media %>% acomp)

write.csv(media,"./Data/FMI/mediapaises.csv")


# PCA
pcagf<-princomp(media,cor = T)
plot(pcagf)
loadings(pcagf)
plot(pcagf,type="lines")
biplot(pcagf,scale = 1)

library(robCompositions)
pcagf<-pcaCoDa(media,method = "classical")
summary(pcagf)
set.seed(1906)
pcagf<-pcaCoDa(media,method = "robust")
summary(pcagf) 
biplot(pcagf,xlabs=rownames(media))

n=4
set.seed(1906)
res.km <- kmeans(scale(alr(media)), centers=n)

# alr PCA ####
n=6
res.pca <- prcomp(alr(media),  scale = TRUE)
#biplot(res.pca)

# Coordinates of individuals
library(factoextra) # para get_pca_ind
ind.coord <- as.data.frame(get_pca_ind(res.pca)$coord)
# Add clusters obtained using the dendrogram cutree 

dendpai<-media %>%  alr() %>% dist() %>% hclust(method = "ward.D") 
ind.coord$Grupo<-factor(cutree(dendpai,n))
# Add Species groups from the original data sett
ind.coord$code=paises$code; ind.coord$pais=paises$esp
ind.coord<-ind.coord  %>% 
  merge( funciones %>%group_by(code) %>%  mutate(Nivel=last(Nivel)) %>% select(code,Nivel) %>% unique())
# Percentage of variance explained by dimensions
eigenvalue <- round(get_eigenvalue(res.pca), 1)
variance.percent <- eigenvalue$variance.percent
head(eigenvalue)

# Coeficientes de PCA ####
# Helper function 
#::::::::::::::::::::::::::::::::::::::::
var_coord_func <- function(loadings, comp.sdev){
  loadings*comp.sdev
}
# Compute Coordinates
#::::::::::::::::::::::::::::::::::::::::
loadings <- res.pca$rotation
sdev <- res.pca$sdev
var.coord <- t(apply(loadings, 1, var_coord_func, sdev)) 
head(var.coord[, 1:4])
print(xtable(var.coord))

#datos transformados completos vs Grupo
fgrupo<-alr(amalgA) %>% as.data.frame() %>%mutate(code=funciones$code,year=funciones$year) %>% 
  merge(ind.coord %>% select(code,Grupo))

#Paises por grupo
temp<-ind.coord %>% group_by(Grupo) %>% summarise(N=n())
ndato<-temp %>% merge(fgrupo %>% group_by(Grupo) %>% summarise(Ti=n())) %>% 
  merge(ind.coord %>% group_by(Grupo) %>% summarise(across(Dim.1:Dim.2,mean))) %>% 
  mutate(etiq=paste(N,"paises"))
#Ajustes para visualizar etiquetas de grupo
ndato$Dim.1_adj<-ndato$Dim.1+c(0.5,-0.25,-0.5,-1,0.5,-0.5)
ndato$Dim.2_adj<-ndato$Dim.2+c(-0.5,0.95,0.75,1,0.5,-0.5)

write.csv(ind.coord,"./Data/FMI/mediaspaisesPCA.csv",row.names = F)

library(ggpubr)
pcagrupo<-ggscatter(
  ind.coord, x = "Dim.1", y = "Dim.2", 
  color = "Grupo", palette = "npg", ellipse = TRUE, ellipse.type = "convex",
  shape = "Nivel", size = 2, rug=F, legend = "right", ggtheme = theme_bw(),
  show.legend.text = TRUE,
  xlab = paste0("Componente 1 (", variance.percent[1], "% )" ),
  ylab = paste0("Componente 2 (", variance.percent[2], "% )" )
) +
  stat_mean(aes(col=Grupo), size = 3)+
  coord_fixed(xlim = c(-6,3),ylim = c(-4,2))+#default xlim = c(-5.1,2.46),ylim = c(-3.88,1.65)
  #geom_text(aes(Dim.1,Dim.2,label=paises$esp))
  geom_text(data = subset(ind.coord,code=="MEX"),aes(Dim.1,Dim.2,label=pais),nudge_x = -0.2,nudge_y = -0.2)+
  geom_text(data=ndato, aes(Dim.1_adj,Dim.2_adj,label=etiq))
pcagrupo
ggsave(filename ="./TeX/Fig/4.3.2_4_PCAmedia.pdf",width = 15,height = 10,units = "cm" )


# Dendrograma circular ####

#https://cran.r-project.org/web/packages/dendextend/vignettes/dendextend.html
library(circlize)
library(dendextend)

ordend<-dendextend::get_nodes_attr(dend,"label")
ordend<-ordend[!is.na(ordend)] %>% as.data.frame()
names(ordend)<-"esp"
ordend$pos<-c(rep("r",17),rep("l",33),rep("r",17))

paises <- paises %>% merge(ordend) %>% 
  mutate(paisN=ifelse (pos=="r", paste(esp,ni,sep=" ") ,paste(ni, esp,sep=" "))) %>% 
  arrange(code)


paleta<-pal_npg()(n)[c(1,3,4,2,6,5)]

dend <- dendpai %>% as.dendrogram %>%
  set("branches_lwd", 2) %>%
  set("branches_lty", 1) %>%
  set("labels_cex", 1) %>% 
  color_branches(k=n,col =paleta) %>% 
  color_labels(k=n,col =paleta)

par(mar= rep(0,4))
pdf("./TeX/Fig/4.3.2_3_dendro.pdf",width=7, height=5)
circlize_dendrogram(dend,facing = "outside",
                    labels_track_height = .4,
                    dend_track_height = 0.5)
dev.off() 


# Datos completos en PCA ####
#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/


ind.sup.coord <- predict(res.pca, newdata = fgrupo) %>% 
  as.data.frame()%>%
  mutate(code=funciones$code,year=funciones$year,Nivel=funciones$Nivel) %>% 
  merge(ind.coord %>% select(code,Grupo))
names(ind.sup.coord)[2:6]<-paste("Dim",1:5,sep = ".")

head(ind.sup.coord)


pcafullgrupo<-ggscatter(
  ind.sup.coord, x = "Dim.1", y = "Dim.2", 
  color = "Grupo", palette = "npg", #ellipse = TRUE, ellipse.type = "convex",
  shape = "Nivel", size = 2, rug=F, legend = "right", ggtheme = theme_bw(),
  show.legend.text = TRUE,
  xlab = paste0("Componente 1 (", variance.percent[1], "% )" ),
  ylab = paste0("Componente 2 (", variance.percent[2], "% )" )
) +
  #coord_cartesian(xlim = c(-5.5,3))+
  #stat_mean(aes(col=Grupo), size = 3)#+
  coord_fixed(xlim = c(-6,3),ylim = c(-4,2))#+#default xlim = c(-5.1,2.46),ylim = c(-3.88,1.65)
pcafullgrupo

temp<-ggplot_build(pcagrupo)
# Normalidad datos agrupados ####

temp<-ind.sup.coord %>% select(Dim.1,Dim.2,Grupo)
mvn(data = temp, subset = "Grupo",
    univariateTest = "Lillie",mvnTest = "dh",desc = F)#$multivariateNormality

temp<-fgrupo %>% select(soc:resto,Grupo)
mvn(data = temp, subset = "Grupo",
    univariateTest = "Lillie",mvnTest = "dh",desc = F)

temp<-fgrupo %>% select(edu:sal,Grupo)
mvn(data = temp, subset = "Grupo",
    univariateTest = "Lillie",mvnTest = "dh",desc = F)


# Distribuciones marginales
fgrupo %>% #select(soc:resto,Grupo) %>% 
  pivot_longer(cols = soc:resto,names_to="fun",values_to="val") %>% 
  mutate(fun=factor(fun,levels = c("soc","edu","eco","sal","resto"))) %>% 
  ggplot(aes(x=val))+
  geom_density(size=1)+
  facet_wrap(~fun)+
  coord_cartesian(xlim = c(-4,2))

library(GGally)
fgrupo %>%
  ggpairs(columns = 2:6, #ggplot2::aes(color=Grupo),
        diag = list(discrete="barDiag", 
                    continuous = wrap("densityDiag", alpha=0.5 ))) +
  scale_color_npg()+
  scale_fill_npg()
# Curvas Densidad en alr o ternaria
plot(amalgA[,c(1:4,6)],margin = "gob",pca = T,col.pca = "blue",robust = T)
plot(amalgA[,c(1:4,6)],margin = "gob",pca = T,col.pca = "blue",robust = F)


#normality ###
#library(MVN)
library(robCompositions)

subn<-names(fgrupo)[c(2:6,8)] #todas las variables
subn<-names(fgrupo)[c(3:5,8)] # sin soc o el resto
mvn(data = fgrupo[,c(2:6,8)], subset = "Grupo",
    univariateTest = "Lillie",mvnTest = "dh",desc = F)$multivariateNormality

set.seed(1511)
sapply(1:6,function(x) norm_dendgrupo2(fgrupo,grupo = x,out = T,colu = 1:5))

set.seed(1906)
sapply(1:6,function(x) norm_dendgrupo2(fgrupo,grupo = x,out = T,colu = 2:4))


X<-amalgA[fgrupo$Grupo==4,c(2:4,6)]
plot(X,margin = "gob")
out<-outCoDa(X);out
outlier<-out$outlierIndex
rownames(X)[out$outlierIndex]
plot(out)
head(X)
plot(X,margin="gob",col=1+as.numeric(out$outlierIndex))


pruebas<-c("mardia","hz","royston","dh")
temp<-sapply(pruebas,function(x) mvn(data = alr(X[!outlier,]),
                               univariateTest = "Lillie",mvnTest = x,desc = T)$multivariateNormality)
temp<-mvn(data = alr(X[!outlier,]),
          univariateTest = "Lillie",mvnTest = "royston",desc = T)
temp %>% as.data.frame()


#1-pchisq(estadístico,df)
# grupo, prueba, estadístico, p value, MVN

pcagrupo
pruebas<-c("mardia","hz","royston","dh")

X<-amalgA[fgrupo$Grupo==4,c(2:4,6)]
set.seed(1906)
out<-outCoDa(X)
rownames(X)[out$outlierIndex]

mvnpruebas<-sapply(1:6, function(k){
  X<-amalgA[fgrupo$Grupo==k,c(2:4,6)]
  set.seed(1906)
  out<-outCoDa(X)
  #if(k==4) out<-outCoDa(X[,c(2:4,6)])
  outlier<-out$outlierIndex
  res<-sapply(pruebas,
              function(x){
                prueba<-mvn(data =alr(X[!outlier,]),
                            univariateTest = "Lillie",
                            mvnTest = x,desc = T)$multivariateNormality
                if (x=="mardia") mvn<- prueba$Result[3] else mvn<-prueba$MVN
                return(mvn)
                
              })
  return(res)
})
mvnpruebas

mvnpruebas<-sapply(1:6, function(k){
X<-amalgA[fgrupo$Grupo==k,]
set.seed(1115)
out<-outCoDa(X)
if(k==3|k==4) out<-outCoDa(X[,c(2:4,6)])
outlier<-out$outlierIndex
nout<-c(sum(outlier),sum(outlier)/nrow(X)*100)
names(nout)<-c("atipicos","porcen")
res<-sapply(pruebas,
            function(x){
              prueba<-mvn(data =alr(X[!outlier,c(2:4,6)]),
                  univariateTest = "Lillie",
                  mvnTest = x,desc = T)$multivariateNormality
              if (x=="mardia") mvn<- prueba$Result[3] else mvn<-prueba$MVN
              return(mvn)
  
  })
#res<-c(nout,res)
return(res)
})
mvnpruebas

nout<-c(sum(outlier),sum(outlier)/nrow(X)*100)
names(nout)<-c("atipicos","porcen")
nout
# grupo, prueba (dh), estadístico, p value, MVNs
# subcomposición edu, eco, sal resumen pruebas ####
library(robCompositions)
library(MVN)
k=1
for(k in 1:6){
X<-amalgA[fgrupo$Grupo==k,]
set.seed(1115)
out<-outCoDa(X)
#out
#plot(out$mahalDist) # distancia mahalanobis
#text(1:nrow(X),out$mahalDist,rownames(X))
#text(1:nrow(X),out$mahalDist*out$outlierIndex,rownames(X),col="red")
if(k==4) out<-outCoDa(X[,c(2:4,6)])
outlier<-out$outlierIndex
prueba<-mvn(data =alr(X[!outlier,c(2:4,6)]),
            univariateTest = "Lillie",
            mvnTest = "dh",desc = T)

multi<-prueba$multivariateNormality
multi$observaciones<-nrow(X)
multi$atipico<-sum(outlier)
multi$per<-multi$atipico/multi$observaciones*100

marginal<-prueba$univariateNormality
# extrae p-values
pvalues<-trimws(marginal[,4])

# si rechaza H0 pone asterísco junto a p-value
nrechaza<-trimws(t(marginal[,5]))
nrechaza[nrechaza=="YES"]<-" "
nrechaza[nrechaza=="NO"]<-"*"
univ<- paste(pvalues,nrechaza,sep = "")

multi<-cbind(multi,t(univ))
# guardar: mahalDist, indicador atípico
temp<-data.frame(id=rownames(X),mD=out$mahalDist,outlier=out$outlierIndex)

if(k==1){
  res<- multi  
  atipicos<-temp
}
else {
  res<-rbind(res,multi)
  atipicos<-rbind(atipicos,temp)
  }
}
rownames(res)<-1:6
names(res)[9:11]<-c("edu","eco","sal")

res
print(xtable(res[,c(2:4,6:11)],digits = 2))
#incluir resumen de outliers en base transformada alr
atipicos<-atipicos %>% separate(id,c("code","year"))
fgrupo<-fgrupo %>% merge(atipicos)
write.csv(fgrupo,"./Data/gto_conglo.csv",row.names = F)
#resumen mD
fgrupo %>% group_by(Grupo) %>% summarise(across(mD:outlier,list("m"=mean,"sd"=sd,"max"=max,"min"=min)))

# No normalidad de soc y resto ####
#pruebas
for(k in 1:6){
  X<-amalgA[fgrupo$Grupo==k,]
  set.seed(1115)
  out<-outCoDa(X)
  outlier<-out$outlierIndex
  prueba<-mvn(data =alr(X[!outlier,]),
              univariateTest = "Lillie",
              mvnTest = "dh",desc = T)
  
  multi<-prueba$multivariateNormality
  multi$observaciones<-nrow(X)
  multi$atipico<-sum(outlier)
  multi$per<-multi$atipico/multi$observaciones*100
  
  marginal<-prueba$univariateNormality
  # extrae p-values
  pvalues<-trimws(marginal[,4])
  
  # si rechaza H0 pone asterísco junto a p-value
  nrechaza<-trimws(t(marginal[,5]))
  nrechaza[nrechaza=="YES"]<-" "
  nrechaza[nrechaza=="NO"]<-"*"
  univ<- paste(pvalues,nrechaza,sep = "")
  
  multi<-cbind(multi,t(univ))
  
  if(k==1) {
    res<-multi
    }
  else{
    res<-rbind(res,multi)
    }
}

names(res)[9:13]<-c("soc","edu","eco","sal","resto")

res
print(xtable(res[,c(2:4,6:9,13)],digits = 2))
#sesgo / kurtosis
library(psych)
describeBy(fgrupo[,2:6],fgrupo$Grupo)

