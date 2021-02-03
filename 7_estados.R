
# paquetes
library(tidyverse)
library(ggsci) # paletas de colores de journals
library(ggplot2)
library(compositions)
library(e1071) #para sesgo y kurtosis
library(xtable)
library(readxl)
library(janitor)
library(plm)
library(lmtest)
library(stargazer)
library(ggrepel)

# datos ####
setwd("C:/Users/sgome/Dropbox/#tesis")

#2015
pef15<-read_excel(path = "./Data/PEF/PEF2015_AC01.xlsx",sheet = 2)
#2016
pef16<-read_excel(path = "./Data/PEF/PEF2016_AC01.xlsx",sheet = 2)
#2017
pef17<-read_excel(path = "./Data/PEF/pef_ac01_2017.xlsx",sheet = 2)
#2018
pef18<-read_excel(path = "./Data/PEF/PEF_2018.xlsx",sheet = 1)
#2019
pef19<-read_excel(path = "./Data/PEF/PEF_2019.xlsx",sheet = 1)
#2020
pef20<-read_excel(path = "./Data/PEF/PEF_2020.xlsx",sheet = 1)
#2021
pef21<-read_excel(path = "./Data/PEF/PPEF_2021.xlsx",sheet = 1)

#selección de variables ####
names(pef15) #c(1,9,24,25,27)
names(pef16) #c(1,9,26,27,29)
names(pef17) #c(1,9,26,27,29)
names(pef18) #c(1,9,30,31,33)
names(pef19) #c(1,9,30,31,33)
names(pef20) #c(1,9,30,31,33)
names(pef21) #c(1,9,30,31,33)


clmns<-c(c(1,3,7,9,24,25,27),c(1,3,7,9,26,27,29),c(1,3,7,9,26,27,29),
         c(1,3,7,9,30,31,33),c(1,3,7,9,30,31,33),c(1,3,7,9,30,31,33),
         c(1,3,7,9,30,31,33)) %>% 
  matrix(nrow = 7,ncol=7) %>%t() 
clnmb<-c("year","ramo","grf","fnmx","codef","entfed","gto")
colnames(clmns)<-clnmb

pef15 %>% dplyr::select(clmns[1,]) %>% head()
# pef16 %>% dplyr::select(clmns[2,]) %>% head()
# pef17 %>% dplyr::select(clmns[3,]) %>% head()
# pef18 %>% dplyr::select(clmns[4,]) %>% head()
# pef19 %>% dplyr::select(clmns[5,]) %>% head()
# pef20 %>% dplyr::select(clmns[6,]) %>% head()
# pef21 %>% dplyr::select(clmns[7,]) %>% head()

todo<-list(pef15,pef16,pef17,pef18,pef19,pef20,pef21)
cont=0
for(i in todo){
  temp<- i %>% dplyr::select(clmns[cont+1,]) 
  names(temp)<-clnmb
  temp<-temp %>%
    filter(ramo!="Aportaciones a Seguridad Social"|fnmx!="Protección Social") %>%
    group_by(year,ramo,grf,fnmx,codef,entfed) %>% summarise(gto=sum(gto)) %>% ungroup()
  if (cont==0) PEF<-temp else PEF<-PEF %>% bind_rows(temp)
  cont=cont+1
}
head(PEF)
PEF %>% group_by(year) %>% summarise(gto=sum(gto)/1e9)

unique(PEF$year)
unique(PEF$ramo)
unique(PEF$grf)
unique(PEF$entfed)
unique(PEF$fnmx)

# rm(i,todo,pef16,pef17,pef18,pef19,pef20,pef21,todo,clmns,clnmb)
# write.csv(PEF,file = "./Data/PEF/resumenPEF.csv",row.names = F)


PEF<-read.csv("./Data/PEF/resumenPEF.csv")


# catálogos ####
# Entidades Federativas
original<-unique(PEF$entfed)
abrevia<-read.csv("./Data/PEF/edo_codigos.csv",encoding = "UTF-8")
cat_entidad<-data.frame(entfed=original,
                        estado=ifelse(grepl("Distribuible|Extranjero",original),"ND", # no distribuible
                                      ifelse(original=="Distrito Federal","Ciudad de México", # nuevo nombre cdmx
                                             ifelse(original=="Estado de México","México",original)))) %>% # nombre original
  merge(abrevia %>% dplyr::select(Estado,ISO3),by.x = "estado",by.y = "Estado",all.x = T) %>% 
  mutate(ISO3=ifelse(estado=="ND","ND",ISO3))

# Funciones México a FMI
original<-unique(PEF$fnmx)
temp<-unique(PEF %>% dplyr::select(grf,fnmx))
original
cat_funcion<-data.frame(fnmx=original,
                        #corto=original,
                        fmi=c("gob","eco","gob","edu","gob","sal","soc","viv","seg","eco",
                              "eco","seg","eco","eco","def","gob","eco","cul","gob","gob",
                              "soc","gob","amb","gob","gob","gob","eco","eco"))
rm(original,abrevia,temp)
# Traducción base PEF a catálogos
peftrad<-PEF %>%  
  merge(cat_funcion) %>% 
  #group_by(year,entfed,fmi) %>% summarise(gto=sum(gto)) %>% ungroup() %>% 
  merge(cat_entidad) %>% 
  group_by(ISO3,estado,year,fmi) %>% summarise(gto=sum(gto)) %>% ungroup()
head(peftrad)
peftrad %>% group_by(year) %>% summarise(gto=sum(gto)/1e9)

# Gasto x estado ####
gedo<-peftrad %>% #filter(ISO3!="ND") %>% 
  dplyr::select(-estado) %>% #group_by(ISO3,year) %>% summarise(gto=sum(gto)) %>% #suma de funciones
  pivot_wider(names_from = ISO3,values_from=gto) %>% relocate(ND,.after=ZAC) %>% # estados a columnas, separar No distribuible
  #column_to_rownames("year") %>% acomp() %>% unclass() %>% as.data.frame() %>% 
  #rownames_to_column("year") %>% # composición de estados
  mutate(tot=dplyr::select(.,AGU:ZAC) %>% rowSums(na.rm = T),across(AGU:ZAC,~./tot)) %>% dplyr::select(-tot) %>% #%total
  pivot_longer(AGU:ZAC,names_to = "ISO3",values_to="gtop") %>% #gasto en % del total
  mutate(gtop=replace_na(gtop,0)) # ceros en NA
  #group_by(ISO3) %>% summarise(gto=geometricmean(gto)) %>% arrange(desc(gto))
# gedo %>% group_by(year) %>% summarise(gtop=sum(gtop))

# prorratear No distribuible entre estados por Gasto####
pefprorr<-peftrad %>% merge(gedo,all.x = T) %>% #agregar porcentajes 
  mutate(gtoorig=gto,gto=gto+ND*gtop) %>% filter(ISO3!="ND") #agregar gasto ND prorrateado a gasto, guardar orig
total<-pefprorr %>% group_by(year,fmi) %>% summarise(gto=sum(gto),gtoorig=sum(gtoorig)) %>% 
  mutate(estado="Total",ISO3="TOT") # crear tabla con gasto total
total %>% group_by(year) %>% summarise(gto=sum(gto)/1e9,gtoorig=sum(gtoorig)/1e9) #check gastos totales

peftrad<-pefprorr %>% dplyr::select(-c(ND,gtop)) %>% # retirar auxiliares de prorrateo
  bind_rows(total)
rm(pefprorr,gedo) #borrar auxiliares

# Resumenes gasto ####
# Total por años en millones de pesos corrientes
peftrad %>% filter(ISO3!="TOT") %>% group_by(year) %>% summarise(gto=sum(gto)/1e6) %>% 
  ggplot(aes(x=year,y=gto)) + geom_point()+geom_text(aes(x=year,y=gto,label=round(gto/1e6,2)))

# Gasto en pesos constantes ####

# Gasto per cápita ####

# Gasto en % de PIB (sólo totales) ####
# porcentaje de PIB total
pib<-read_excel("./Data/PEF/PIBT_5.xlsx",sheet=3)
inf<-c(0.04,0.03) #inflación esperada 20-21
g<-c(-0.1,0.03) # tasa pib real esperada 20-21
proy<-data.frame(year=2020:2021,PIBcorr=c(NA,NA))
pib<-pib %>% bind_rows(proy)
pib$PIBcorr[6]<-pib$PIBcorr[5]*(1+g[1])*(1+inf[1]) # estimación simple PIB 2020
pib$PIBcorr[7]<-pib$PIBcorr[6]*(1+g[2])*(1+inf[2]) # estimación simple PIB 2021

# Gráfica Total por años en porcentaje de PIB
peftrad %>% filter(ISO3!="TOT") %>%  group_by(year) %>% summarise(gto=sum(gto)/1e6) %>% 
  merge(pib) %>% mutate(gtop=gto/PIBcorr*100) %>% 
  ggplot(aes(x=year,y=gtop)) + geom_point() +geom_text(aes(x=year,y=gtop,label=round(gtop,2)))
rm(inf,g,proy)

# tamaño promedio funciones
peftrad %>% filter(ISO3!="TOT") %>% #eliminar total para no duplicar
  group_by(year,fmi) %>% summarise(gto=sum(gto)/1e6) %>% # suma por año-función
  merge(pib) %>% mutate(gtop=gto/PIBcorr*100) %>% # gasto en % de PIB
  group_by(fmi) %>% summarise(gtop=mean(gtop)) %>% # media aritmetica del gasto por función
  arrange(desc(gtop)) #ordenar

# Tabla gasto (total Mex) por función en %PIB para cada año
peftrad %>% filter(ISO3!="TOT") %>% group_by(year,fmi) %>% summarise(gto=sum(gto)/1e6) %>% 
  merge(pib) %>% mutate(gtop=gto/PIBcorr*100) %>% dplyr::select(fmi,year,gtop) %>% 
  pivot_wider(names_from = year,values_from=gtop) %>% arrange(desc(`2017`)) %>% 
  adorn_totals("row")
# más o menos se parece a lo de SHCP, 3% de diferencia en social
# con el prorrateado se parece mas el nivel x funcion y orden de funciones pero el total no cuadra con neto SHCP

# Gráfica trends funciones
peftrad %>% group_by(year,fmi) %>% summarise(gto=sum(gto)/1e6) %>% 
  merge(pib) %>% mutate(gtop=gto/PIBcorr*100) %>% 
  filter(fmi%in%c("soc","gob","eco","edu","sal")) %>% 
  ggplot(aes(x=year,y=gtop,col=fmi)) + geom_line(size=1)


# Detalle de social que crece rápido
pefsoc<-PEF %>%  
  merge(cat_funcion) %>% 
  group_by(year,entfed,fmi,ramo) %>% summarise(gto=sum(gto)) %>% ungroup() %>% 
  filter(fmi=="soc") %>% 
  merge(cat_entidad %>% dplyr::select(entfed,ISO3)) %>% relocate (ISO3,entfed) %>% 
  group_by(year,ramo) %>% summarise(gto=sum(gto)/1e6) %>% 
  merge(pib) %>% mutate(gtop=gto/PIBcorr*100)
temp<-pefsoc %>% group_by(year) %>% summarise(gto=sum(gtop)) #resumen porcentajes
temp[7,2]-temp[1,2]  #incremento

temp<-pefsoc %>% dplyr::select(year,ramo,gtop) %>% 
  pivot_wider(names_from = ramo,values_from=gtop) 
temp[is.na(temp)]<-0
t(temp[7,2:13]-temp[1,2:13]) %>% round(2) %>% as.data.frame() %>%  arrange(desc(V1))
# IMSS, Bienestar e ISSTE traen aumento
rm(pefsoc)

# subcomp. funciones por estado/año ####
edodat<-peftrad %>% dplyr::select(-gtoorig) %>% pivot_wider(names_from = fmi,values_from=gto) %>% 
  mutate(def=replace_na(def,0)) %>% 
  #dplyr::select(-estado) %>% unite("id",c(ISO3,year),sep = ".") %>% 
  #column_to_rownames("id") %>% 
  mutate(tot= dplyr::select(.,amb:def) %>% rowSums()) %>% # gasto total 
  mutate(across(amb:def, ~./tot)) %>% # convertir a composición suma 1
  #groupparts("resto"=c("seg","def","amb","cul","viv"))
  mutate(resto=seg+def+amb+cul+viv) %>% dplyr::select(-c(tot,seg,def,amb,cul,viv)) %>% # agrupar chicos
  relocate(c(soc,edu,eco,sal,resto,gob),.after=estado) #orden

head(edodat)
apply(edodat[,4:9],1,sum) #check suma 1
# pegar media de estados para cada año
yearmed<-function(anio,metodo=TRUE){ #media composicional por año
  dat<-edodat %>%
    filter(year==anio) %>% dplyr::select(-c(ISO3:year,estado)) %>% 
    acomp()
  resultado<-mean(dat,robust=metodo) %>% as.matrix()
  return(resultado)
}

yearmed(2015)
x<-2015:2021
promedio<-sapply(x,yearmed) %>% t() %>%  as.data.frame()
colnames(promedio)<-names(edodat)[4:9]
promedio$year<-x;promedio$estado<-rep("Media",7);promedio$ISO3<-rep("MED",7)
edodat<-bind_rows(edodat,promedio)

# Media estados ####
x<-unique(edodat$ISO3)
edomed<-function(edo,metodo=TRUE){ #media composicional por estado
  dat<-edodat %>%
    filter(ISO3==edo) %>% dplyr::select(-c(ISO3:estado)) %>% 
    acomp()
  resultado<-mean(dat,robust=metodo) %>% as.matrix()
  return(resultado)
}

media<-sapply(x,function(e) edomed(e,F)) %>% t() %>% as.data.frame()
names(media)<-names(edodat)[4:9]
head(media)
apply(media, 1, sum)# check suma 1

# plot(acomp(media))
plot(acomp(media),margin = "gob")

write.csv(media,"./Data/PEF/mediaedos.csv")
media<-read.csv("./Data/PEF/mediaedos.csv") %>% column_to_rownames("X")


# Pruebas de normalidad del gasto por estado
library(MVN)
library(robCompositions)
temp<-alr(media)
out<-outCoDa(media)
out
rownames(media)[out$outlierIndex]
mvn(temp,mvnTest = "dh")
mvn(temp[!out$outlierIndex,],mvnTest = "dh")
mvn(temp[!out$outlierIndex,],mvnTest = "mardia")
mvn(temp[!out$outlierIndex,],mvnTest = "royston")
mvn(temp[!out$outlierIndex,],mvnTest = "hz")

# Dendrograma de variables
temp<-acomp(edodat %>% filter(!ISO3 %in% c("TOT","MED")) %>% dplyr::select(-c(ISO3:estado)))
orden<-mean(temp) %>% sort(decreasing = T) %>% names()
head(temp[,orden])
varD<-variation(temp[,orden])
# Tabla variación composicional
varD %>% as.data.frame() %>% bind_rows(mean(temp[,orden])) %>%
  bind_rows(cumsum(mean(temp[,orden])))
#dendrograma funciones con distancia: variación composicional
vardend<-varD %>% as.dist() %>% hclust(method = "ward.D") %>% 
  as.dendrogram()
plot(vardend)
# Dendrograma variables en media
temp<-acomp(media)[1:32,c(6,1:5)]
varD<-variation(temp)
varD %>% as.data.frame() %>% bind_rows(mean(temp)) %>%
  bind_rows(cumsum(mean(temp)))
vardend<-varD %>% as.dist() %>% hclust(method = "ward.D") %>% 
  as.dendrogram()
plot(vardend)



# Dendrograma ####
# Dendrograma estados
par(mar=c(0,3,2,1))
dendpai<-media[1:32,] %>%  alr() %>% dist() %>% hclust(method = "ward.D")
pdf("./TeX/Fig/45_1_dendro.pdf",width = 6,height = 4)
plot(dendpai,main = NA,sub=NA,ylab = "Distancia")
dev.off()

# Circular
library(circlize)
library(dendextend)
dend <- dendpai %>% as.dendrogram %>%
  set("branches_lwd", 2) %>%
  set("branches_lty", 1) %>%
  set("labels_cex", 1)

circlize_dendrogram(dend,facing = "outside")#,
                    #labels_track_height = .4,
                    #dend_track_height = 0.5)

# alr PCA ####
n=6
res.pca <- prcomp(alr(media[1:32,]),  scale = TRUE)
#biplot(res.pca)

# Coordinates of individuals
library(factoextra) # para get_pca_ind
ind.coord <- as.data.frame(get_pca_ind(res.pca)$coord)
# Add clusters obtained using the dendrogram cutree 

ind.coord$Grupo<-factor(cutree(dendpai,n))
# Add Species groups from the original data sett
ind.coord$code<-rownames(ind.coord)
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
edogrupo<-alr(edodat[,4:9]) %>% as.data.frame() %>% mutate(code=edodat$ISO3,year=edodat$year) %>% 
  merge(ind.coord %>% dplyr::select(code,Grupo))

#Estados por grupo
temp<-ind.coord %>% group_by(Grupo) %>% summarise(N=n())

ndato<-temp %>% merge(edogrupo %>% group_by(Grupo) %>% summarise(Ti=n())) %>% 
  merge(ind.coord %>% group_by(Grupo) %>% summarise(across(Dim.1:Dim.2,mean))) %>% 
  mutate(etiq=paste(N,"estados"))

#Ajustes para visualizar etiquetas de grupo
ndato$Dim.1_adj<-ndato$Dim.1#+c(0.5,-0.25,-0.5,-1,0.5,-0.5)
ndato$Dim.2_adj<-ndato$Dim.2#+c(-0.5,0.95,0.75,1,0.5,-0.5)

library(ggrepel)
library(ggpubr)
pcagrupo<-ggscatter(
  ind.coord, x = "Dim.1", y = "Dim.2", 
  color = "Grupo", palette = "npg", ellipse = TRUE, ellipse.type = "convex",
  #shape = "Nivel",
  size = 2, rug=F, legend = "right", ggtheme = theme_bw(),
  show.legend.text = TRUE,
  xlab = paste0("Componente 1 (", variance.percent[1], "% )" ),
  ylab = paste0("Componente 2 (", variance.percent[2], "% )" )
) +
  #stat_mean(aes(col=Grupo), size = 3)+
  coord_fixed() + #xlim = c(-6,3),ylim = c(-4,2))+#default xlim = c(-5.1,2.46),ylim = c(-3.88,1.65)
  geom_text_repel(data = ind.coord,aes(x=Dim.1,y=Dim.2,label=code))
  #geom_text(data = subset(ind.coord,code=="TOT"),aes(Dim.1,Dim.2,label="Promedio"),nudge_x = -0.2,nudge_y = -0.2)#+
  #geom_text(data=ndato, aes(Dim.1_adj,Dim.2_adj,label=etiq))
pcagrupo








# PCA para comparar con paises####
#library(factoextra)
mediap<-read.csv("./Data/FMI/mediapaises.csv") %>% column_to_rownames("X")
res.pca <- prcomp(alr(mediap),  scale = TRUE)
eigenvalue <- round(get_eigenvalue(res.pca), 1)
variance.percent <- eigenvalue$variance.percent
nivtrans<-function(x,nivel="Nivel"){# para ordenar niveles de ingreso
  x %>% mutate(Nivel=factor(get(nivel),levels = c("Bajo","Medio-bajo","Medio-alto","Alto","México")))
}
ind.coordp<-read.csv("./Data/FMI/mediaspaisesPCA.csv",
                     colClasses = c("character",rep("numeric",5),"factor","character","factor")) %>% 
  nivtrans()
  
#library(ggpubr)
pcagrupo<-ggscatter(
  ind.coordp, x = "Dim.1", y = "Dim.2", 
  color = "Grupo", palette = "npg", ellipse = TRUE, ellipse.type = "convex",
  shape = "Nivel", size = 2, rug=F, legend = "right", ggtheme = theme_bw(),
  show.legend.text = TRUE,
  xlab = paste0("Componente 1 (", variance.percent[1], "% )" ),
  ylab = paste0("Componente 2 (", variance.percent[2], "% )" )
) +
  stat_mean(aes(col=Grupo), size = 3)+
  coord_fixed(xlim = c(-6,3),ylim = c(-4,2))#+#default xlim = c(-5.1,2.46),ylim = c(-3.88,1.65)
  #geom_text(aes(Dim.1,Dim.2,label=paises$esp))
  #geom_text(data = subset(ind.coord,code=="MEX"),aes(Dim.1,Dim.2,label=pais),nudge_x = -0.2,nudge_y = -0.2)+
  
pcagrupo

mediae<-read.csv("./Data/PEF/mediaedos.csv") %>% column_to_rownames("X")
alr(mediae) %>% head()
dimnames(alr(mediae))
ind.sup.coord <- predict(res.pca, newdata = alr(mediae) %>% as.data.frame()) %>% 
  as.data.frame()%>%
  rownames_to_column("code")
names(ind.sup.coord)[2:6]<-paste("Dim",1:5,sep = ".")

head(ind.sup.coord)
library(ggrepel)
pcagrupo+
  geom_point(data = ind.sup.coord,aes(x=Dim.1,y=Dim.2))+
  geom_text_repel(data = ind.sup.coord,aes(x=Dim.1,y=Dim.2,label=code))

library(gginnards)
pcagrupo
p<-delete_layers(pcagrupo,idx=1L)+
  geom_point(data = ind.sup.coord,aes(x=Dim.1,y=Dim.2))+
  geom_text_repel(data = ind.sup.coord,aes(x=Dim.1,y=Dim.2,label=code))
p
ggsave(plot = p,filename = "./TeX/Fig/45_2_pca.pdf",width = 15,height = 10,units = "cm")


# composición de gasto por estados ####
lsfun<-list("media"=mean,"dev"=sd,
            "p25"=function(x,na.rm=T) quantile(x,probs = 0.25,na.rm),
            "p75"=function(x,na.rm=T) quantile(x,probs = 0.75,na.rm),
            "mín"=min,"máx"=max,
            "sesgo"=skewness,"kurtosis"=kurtosis)
peftrad %>% filter(ISO3!="TOT")%>% mutate(gto=gto/1e9) %>% 
  group_by(year) %>% summarise(across(gto,lsfun))
head(peftrad)

# Total
peftrad %>% filter(ISO3!="TOT")%>% #mutate(gto=gto/1e9) %>% 
  ggplot(aes(gto))+
  geom_density()+
  scale_x_log10()

# distribución gtototal por año
peftrad %>% filter(ISO3!="TOT") %>% mutate(gto=gto/1e9) %>% 
  ggplot(aes(factor(year),gto))+
  geom_violin()+
  scale_y_log10() # no se aprecia gran diferencia

# Top estados
peftrad %>% filter(ISO3!="TOT") %>% group_by(ISO3,year) %>% summarise(gto=sum(gto)/1e9) %>% 
  group_by(ISO3) %>% summarise(gto=mean(gto)) %>% arrange(desc(gto)) #miles de millones
# en términos del total del gasto, percápita o de PIB

# composición partes estado
temp<-peftrad %>% filter(ISO3!="TOT") %>% group_by(ISO3,year) %>% summarise(gto=sum(gto)) %>% #suma de funciones
  pivot_wider(names_from = ISO3,values_from=gto) %>% column_to_rownames("year") %>% 
  acomp() %>% unclass() %>% as.data.frame() %>% rownames_to_column("year") %>% # composición de estados
  pivot_longer(AGU:ZAC,names_to = "ISO3",values_to="gto") %>%
  group_by(ISO3) %>% summarise(gto=mean(gto)) %>% arrange(desc(gto))
temp
temp %>% ggplot(aes(x=reorder(ISO3,gto),label=round(gto*100,1))) + geom_bar(aes(weight=gto*100))+
  geom_text(aes(y=100*gto+2.5),size=3)
sum(temp$gto)

tab<-temp %>% mutate(gtoacu=cumsum(gto))
plot(1:32,tab$gtoacu,type = "b")
# promedio ponderado con gastos

# PIB por estado ####
temp<-cat_entidad %>% filter(ISO3!="ND",entfed!="Distrito Federal") %>% arrange(estado)
pibent<-read_excel("./Data/PIB/PIBE/tabulados_pibent/PIBE_2.xlsx",skip = 4,n_max = 38) %>% #mdp constantes 2013
  na.omit() %>% filter(!grepl("roducto|Unidos",Concepto)) %>% 
  arrange(Concepto) %>% 
  mutate(ISO3=temp$ISO3,estado=temp$estado) %>% dplyr::select(-Concepto) %>% 
  #relocate(Concepto,ISO3,estado) %>% 
  pivot_longer(cols = '2003':'2018R',names_to='year',values_to='pib') %>% 
  mutate(year=as.numeric(substr(year,1,4)))

pibent %>% filter(year==2018) %>% 
  ggplot(aes(reorder(ISO3,-pib),pib/1e6))+geom_bar(stat="identity",position = "dodge")+
  theme_bw()+theme(axis.text.x = element_text(angle=90,vjust = 0.5))

# población CONAPO (NEW) ####
ping<-read_excel("./Data/INEGI/poblacióningresos.xlsx",skip = 4) %>% 
  pivot_longer(cols = !c(Entidad,Periodo),names_to="variable",values_to="pob") %>% 
  filter(grepl("Cuarto",Periodo),!grepl("cv|est",variable)) %>% 
  pivot_wider(names_from = variable,values_from=pob) %>% 
  mutate(year=as.numeric(substr(Periodo,22,25))) %>% 
  mutate(across(Total:s_ne,as.numeric)) %>% arrange(Entidad,year)
cat_ent2<-data.frame(Entidad=sort(unique(ping$Entidad)[-30]), # 30 es el total
                     estado=unique(cat_entidad$estado)[-19]) %>% # 19 es No Dististribuible
  merge(cat_entidad %>% filter(entfed!="Distrito Federal") %>% dplyr::select(estado,ISO3))

conapo<-read.csv("./Data/CONAPO/pob_mit_proyecciones.csv") %>% 
  rename(year=AÑO,estado=ENTIDAD) %>% filter(year>=2003,year<=2021,estado!="República Mexicana") %>% 
  group_by(estado,year) %>% summarise(pob=sum(POBLACION)) %>% ungroup() %>% 
  arrange(estado,year) %>% mutate(ISO3=rep(cat_ent2$ISO3,each=19))

conapo %>% #filter(year>=2010,estado!="República Mexicana") %>%
  group_by(year) %>% summarise(pob=sum(pob)/1E6) %>% 
  ggplot(aes(year,pob))+geom_line()


# Poblacion por estado (OLD)###
pobent<-read_excel("./Data/PIB/PIBE/POBent.xlsx",skip=4) %>% 
  na.omit() %>% rename(entidad='...1') %>% dplyr::select(entidad,T2000,T2005,T2010) %>% 
  mutate(r1=(T2005/T2000)^(1/5)-1,r2=(T2010/T2005)^(1/5)-1,r3=(r1+r2)/2) %>% #tasa c.pob. promedio 
  mutate(across(r1:r3,~.*100))

for (i in 1:8){ #pronóstico de población del 2014 al 2021
  if (i==1) {
    proy=pobent$T2010*(1+pobent$r3/100)^(3+i)
    }
  else {
    proy=cbind(proy,pobent$T2010*(1+pobent$r3/100)^(3+i))
  }
}
colnames(proy)<-2014:2021
apply(proy,2,sum)/1e6 # total población méxico por año
plot(apply(proy,2,sum)/1e6)

#temp<-apply(proy,2,sum)/1e6;temp[2:8]-temp[1:7]

pobent<-cbind(pobent[,1:4],proy)
apply(pobent[,2:12],2,sum)/1e6
pobent<-pobent %>% arrange(entidad) %>% 
  mutate(ISO3=temp$ISO3,estado=temp$estado) %>% 
  relocate(entidad,ISO3,estado) %>% dplyr::select(-c(entidad,T2000,T2005,T2010)) %>% 
  pivot_longer(cols = '2014':'2021',names_to='year',values_to='pob')




# Proyección población 2 ###
pobent<-read_excel("./Data/PIB/PIBE/Poblacion_Hist.xlsx",skip=3) %>% 
  rename(entidad="...1") %>% select(-2) %>% na.omit() %>% 
  pivot_longer(cols = `1990`:`2010`,names_to="year",values_to="pob") %>% 
  mutate(year=paste("y",year,sep="")) %>% pivot_wider(names_from = year,values_from=pob) %>% 
  mutate(across(y1990:y2010,as.numeric)) %>% 
  mutate(r95=100*((y1995/y1990)^0.2-1),r00=100*((y2000/y1995)^0.2-1),
         r05=100*((y2005/y2000)^0.2-1),r10=100*((y2010/y2005)^0.2-1))


pobent %>% 
  mutate(across(y1990:y2010,~./y1990)) %>% pivot_longer(cols = y1990:y2010) %>% 
  mutate(year=rep(seq(1990,2010,5),33)) %>% 
  ggplot(aes(x=year,y=log(value),col=entidad))+geom_line()+theme(legend.position = "none")

ppob<-pobent %>% select(entidad,y1990:y2010) %>%
  pivot_longer(cols = y1990:y2010,names_to="time",values_to="pob") %>% 
  mutate(year=rep(seq(1990,2010,5),33)-1990) %>% #años desde el inicio
  group_by(entidad) %>% 
  mutate(indice=pob/first(pob),r=-100+100*(pob/dplyr::lag(pob))^(1/5)) %>% #indice y tasa
  ungroup() %>% 
  filter(entidad!="Estados Unidos Mexicanos") %>% arrange(entidad) %>% #sin total
  mutate(ISO3=rep(cat_ent2$ISO3,each=5)) %>% # agregar iso
  select(-entidad) %>% #filter(year!=0) %>% 
  pdata.frame(index=c("ISO3","time"))

ppob %>% ggplot(aes(year,r,col=ISO3))+geom_line()+theme(legend.position = "none")

eq<-log(pob)~year
eq2<-r~year

g_mco<-plm(eq2,ppob,model = "pooling") # ols
g_int<-plm(eq2,ppob,model = "within", effect = "individual") # within
g_mcg<-plm(eq2,ppob,model = "random", effect = "individual") # gls


#presencia de efectos
pres<-c(format(pFtest(update(g_int,effect="individual"),g_mco)$p.value,digits = 2),"","")
#Hausmant test, correlación de efectos
gcor<-c("","",format(phtest(g_int,g_mcg)$p.value,digits = 2))
#AR(1)
ar1<-sapply(list(g_mco,g_int,g_mcg),function(x) format(pbgtest(x)$p.value,digits = 2))
#AR(2)
ar2<-sapply(list(g_mco,g_int,g_mcg),function(x) format(pbgtest(x,order=2)$p.value,digits = 2))


stargazer(g_mco,g_int,g_mcg,digits = 2, type = "text",
          title = "Regresión de panel para crecimiento económico",
          label = "tab:gmod0_edos",
          column.labels = c("$\\hat{\\und\\gamma}_{\\text{MCO}}$",
                            "$\\hat{\\und\\gamma}_{\\text{W}}$",
                            "$\\hat{\\und\\gamma}_{\\text{MCG}}$"),
          single.row = T,
          dep.var.caption = "Variable dependiente: $g_t$ de los estados",
          omit.stat = "F",
          dep.var.labels.include = F,
          # covariate.labels=c("periodo","ordenada"),
          add.lines = list(c("F (efectos) p-val",pres %>% fsignif()),
                           c("Hausman p-value",gcor%>% fsignif()),
                           c("AR(1) p-value",ar1%>% fsignif()),
                           c("AR(2) p-value",ar2%>% fsignif()))
)

temp<-data.frame(ISO3=rep(cat_ent2$ISO3,each=10),year=rep(21:30,32)) %>% 
  mutate(pred=predict(g_mcg,newdata = temp),reff=rep(ranef(g_mcg),each=10)) %>% 
  mutate(r=pred+reff)
ppobest<-ppob %>% merge(temp %>% select(ISO3,year,r),all=TRUE) %>% 
  arrange(ISO3,year) %>% mutate(time=year+1990) %>% 
  group_by(ISO3) %>% mutate(factor=1+r/100)

for (i in 21:30) {
  ppobest[ppobest$year==i,"pob"]<-ppobest[ppobest$year==i-1,"pob"]*(ppobest[ppobest$year==i,"factor"])
}

ppobest %>% group_by(time) %>% summarise(pob=sum(pob)/1e6)




# estimación pib por estado 2019 en adelante ####
# PIB total 2019 constantes 2013 
p19<-(read_excel("./Data/PIB/PIBT_constantes2.xlsx",skip=4,n_max = 5) %>% t())[190,4] %>% as.numeric()
g<-c(-0.1,0.03) # tasa pib real esperada 20-21
proy<-data.frame(year=2019:2021,pib=c(p19,p19*(1+g[1]),p19*(1+g[1])*(1+g[2])))

# composición estimada
edopib_comp<-pibent %>%dplyr::select(-estado) %>% pivot_wider(names_from = ISO3,values_from=pib) %>% 
  column_to_rownames("year") %>% acomp() 
head(edopib_comp[,1:5])

nuevos<-data.frame(year=2019:2021)
res<-matrix(0,nrow = dim(nuevos)[1],ncol = 32-1)
estadistica<-matrix(0,1,31)
#proyección composición por estados
for(i in 1:31){
  temp<-data.frame(alr=alr(edopib_comp)[,i],year=2003:2018)
  mod<-lm(formula = "alr~poly(year,3)",data = temp)
  res[,i]<-predict(mod,newdata = nuevos)  
  estadistica[[i]]<-summary(mod)$r.squared
}
summary(t(estadistica))
proye<- res %>% alrInv() %>% as.data.frame()
colnames(proye)<-names(edopib_comp)
proye$year<-2019:2021;proye$pibtot<-proy$pib
proye<-proye %>% pivot_longer(cols = AGU:ZAC, names_to="ISO3",values_to="comp") %>% 
  mutate(pib=pibtot*comp)


pibent2<-pibent %>% 
  merge(proye %>% dplyr::select(year,ISO3,pib),all = T) %>% 
  group_by(ISO3) %>% fill(estado,.direction = "down") %>% ungroup()

pibent %>% group_by(year) %>% summarise(pib=sum(pib)) %>% 
  merge(pibent2 %>% group_by(year) %>% summarise(pib2=sum(pib))) #check
write.csv(pibent,'./Data/temp/pibent.csv',row.names = F)
write.csv(pibent2,'./Data/temp/pibent2.csv',row.names = F)
# Gráfica datos PIB y Población por estado
temp<-pibent2 %>% merge(conapo) %>% 
  mutate(pib=pib/1e6,pob=pob/1e6) #%>% #pib y pob en billones
set.seed(1906)
temp%>% filter(year<=2018) %>% 
  ggplot(aes(x=pob,y=pib,col=ISO3,label=ISO3))+geom_point()+
  geom_text_repel(data = subset(temp,year==2018),col="black")+
  scale_y_log10()+scale_x_log10()+
  theme_bw()+theme(legend.position = "none")+
  xlab("Población en millones")+ylab("PIB bdp constantes 2018")
ggsave("./TeX/Fig/45_7_pob_pib.pdf",width = 15,height = 10,units = "cm")


# PIB percápita por estado ####
pibent <- read.csv('./Data/temp/pibent.csv')
pibent2 <- read.csv('./Data/temp/pibent2.csv')

pibpcent<- pibent2 %>% # PIB más proyecciones '20 y '21
  merge(conapo,all = T) %>% # proyecciones conapo de población por estado
  mutate(pibpc=pib/pob*1e6, cre=100*(pibpc/dplyr::lag(pibpc)-1)) # calculo pibpc y g

# gráfica PIB pc
# pibpcent %>% 
#   filter(year==2018) %>% 
#   ggplot(aes(x=reorder(ISO3,pibpc),y=pibpc/1000,label=round(pibpc/1000,0)))+
#   geom_bar(stat = "identity",position = "dodge",fill=4)+
#   geom_text(hjust=-0.2,vjust=0.5,size=2.5,angle=90)+
#   #scale_y_log10()+
#   coord_cartesian(ylim = c(0,600))+
#   theme_bw()+ theme(axis.text.x = element_text(angle=90,vjust=0.5))+
#   xlab("")+ylab("PIB per cápita mdp")
# ggsave("./TeX/Fig/45_3_pibpcedos.pdf",width = 12,height = 7,units = "cm")

# gráfica tasa crecimiento vs PIB pc
# pibpcent %>% filter(dplyr::between(year,2015,2019)) %>% # últimos 5 datos
#   ggplot(aes(x=pibpc/1000,y=cre,label=ISO3,col=ISO3))+geom_point()+
#   scale_x_log10()+theme_bw()+theme(legend.position = "none")+
#   xlab("PIB per cápita mmdp")+ylab("Crecimiento real PIB per cápita")
# ggsave("./TeX/Fig/45_5_g_pibpc.pdf",width = 12,height = 7,units = "cm")

# Total PIB per cápita
temp<-pibpcent %>% group_by(year) %>% summarise(across(pib:pob,sum)) %>% 
  mutate(pibpc=pib/pob*1e6, cre=100*(pibpc/dplyr::lag(pibpc)-1)) %>% 
  mutate(ISO3="TOT",estado="Total") %>% filter(year>=2015)

pibpcent<-pibpcent %>% filter(year>=2015) %>% bind_rows(temp) #agregar resumen total México


write.csv(edodat,'./Data/PEF/gsubedo.csv',row.names = F)
edodat<-read.csv('./Data/PEF/gsubedo.csv')
edodat<-edodat %>% 
  merge(pibpcent,all.x = T) #composición del gasto más información pib pc

rm(temp)
#promedios
temp<-edodat %>% filter(!grepl('TOT|MED',ISO3)) %>% 
  group_by(year) %>% summarise(across(pib:cre,mean))
edodat[edodat$ISO3=='MED',c('year','pib','pob','pibpc','cre')]<-temp
write.csv(edodat,'./Data/PEF/edodat.csv',row.names = F)
edodat<-read.csv('./Data/PEF/edodat.csv')

edodat %>% filter(ISO3=="CAM") %>% select(ISO3,year,pibpc)

# rm(pibent,pobent,proy,promedio,tab,temp,total)

# Mapas ####
# library("devtools")
# install.packages('digest')
# install.packages('rlang')
# install.packages('backports')
# install.packages('ps')
# 
# devtools::install_github("diegovalle/mxmaps")
library(mxmaps)
temp<-df_mxstate[,c('region','state_name')]
cat_entidad<-cat_entidad %>% merge(temp,all.x = T,by.x = 'estado',by.y = 'state_name')

edodat<-edodat %>% 
  merge(cat_entidad %>% dplyr::select(ISO3,region) %>% unique,all.x = T) %>% 
  relocate(ISO3,region)

edodat %>% filter(year==2015,!is.na(region)) %>% 
  #mutate(value=round(pob/1e6,1)) %>% 
  #mutate(value=round(pib/1e6,1)) %>% 
  #mutate(value=round(eco*100,1)) %>%
  #mutate(value=round(cre,1)) %>% 
  mutate(value=round(pibpc,1)) %>% 
  mxstate_choropleth()
rm(mxstate.map)

write.csv(edodat,"./Data/PEF/edodat.csv",row.names = F)
write.csv(cat_entidad,"./Data/PEF/cat_entidad.csv",row.names = F)
# PCA sin atípicos

# Evolución compo estados

# Evolución gastoxfunción algunos estados

# Cruce contra variables por estado: g, esperanza de vida, educación, desempleo, ingreso promedio etc








# Completar gasto en precios constantes

#PIB total constante
temp<-read_excel("./Data/PIB/PIBE/tabulados_pibent/PIBE_2.xlsx",skip = 4,n_max = 38) %>% #mdp constantes 2013
  na.omit() %>% filter(grepl("Producto",Concepto)) %>%dplyr::select(-Concepto) %>% t() %>% 
  as.data.frame() %>% rename(pib=V1) %>% 
  rownames_to_column("year") %>% mutate(year=as.numeric(substr(year,1,4))) %>% 
  bind_rows(proy) %>% filter(year>=2015) # agregar proyección '20:-10%, '21:2%

#pib total corriente + deflactor
deflac<-pib %>% merge(temp) %>% mutate(factor=pib/PIBcorr) #%>% # factor precios 2013
deflac$f18=deflac$factor/deflac$factor[4] # factor precios 2018

#gasto total en precios constantes (usando deflactor del PIB)
gtoconst<-peftrad %>% group_by(ISO3,year) %>% summarise(gto=sum(gto)) %>% #gasto total precios corrientes
  merge(deflac %>% dplyr::select(year,factor,f18)) %>% #deflactor
  mutate(gtoc=gto*f18) #gasto precios constantes 2018

# Gráficas preliminares y exploratorias ####



# datos
g_dat<-edodat %>% filter(!ISO3%in%c("TOT","MED")) %>% #excluir total y promedio
  merge(gtoconst %>% dplyr::select(ISO3,year,gto,gtoc,factor,f18)) %>% # agregar gasto en precios constantes
  mutate(pib=pib/factor*f18,pibpc=pibpc/factor*f18) %>%# convertir PIB a precios constantes 2018
  mutate(gto2=gtoc/pib/1e6*100) %>% #gto en porcentaje de PIB
  mutate(gtopc=gtoc/pob/1e3) %>% #gasto per cápita
  mutate(across(soc:resto,~log(./gob))) %>% dplyr::select(-gob) %>% #logcocientes
  filter(year<=2019) %>% pdata.frame() #quitar proyección '20 y '21

write.csv(g_dat,'./Data/g_dat_edos.csv',row.names = F)
g_dat=read.csv('./Data/g_dat_edos.csv') %>% pdata.frame()

g_dat %>% filter(ISO3=="CAM") %>% select(ISO3,year,pibpc)
g_dat %>% group_by(year) %>% summarise(pib=sum(pib))

# pib per cápita constantes 2013 caja y brazos (no usado)
# g_dat %>% 
#   ggplot(aes(x=reorder(ISO3,pibpc),y=pibpc/1000,label=round(pibpc/12,0)))+geom_boxplot()+
#   scale_y_log10()+
#   theme_bw()+ theme(axis.text.x = element_text(angle=90,vjust=0.5))+
#   xlab("")+ylab("PIB per cápita mdp")
# ggsave("./TeX/Fig/45_3_pibpcedos.pdf",width = 15,height = 10,units = "cm")

# pib per cápita 2018 constantes 2013
g_dat %>% as.data.frame() %>% 
  filter(year %in% c('2018')) %>% 
  ggplot(aes(x=reorder(ISO3,-pibpc),y=pibpc/1000,label=round(pibpc/1000,0)))+
  geom_bar(stat = "identity",position = "dodge",fill=4)+
  geom_text(size=2.5,angle=90,nudge_y = c(rep(-30,3),-250,rep(-30,28)),col="white")+
  #scale_y_log10()+
  coord_cartesian(ylim = c(0,450))+
  theme_bw()+ theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  xlab("")+ylab("PIB per cápita (miles)")
ggsave("./TeX/Fig/45_3_pibpcedos.pdf",width = 12,height = 7,units = "cm")

# g en caja y brazos
g_dat %>% 
  group_by(ISO3) %>% mutate(m=median(cre)) %>% ungroup() %>% 
  ggplot(aes(x=reorder(ISO3,m),y=cre,label=round(cre,1)))+geom_boxplot()+
  theme_bw()+xlab("")+ylab("Crecimiento real PIB per cápita")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))
ggsave("./TeX/Fig/45_4_g_edos.pdf",width = 12,height = 7,units = "cm")


# Cuadro medias gasto por grupo
edoagrupado<-edodat %>% filter(!ISO3%in%c("TOT","MED")) %>% #excluir total y promedio
  merge(gtoconst %>% dplyr::select(ISO3,year,gto,gtoc,factor,f18)) %>% # agregar gasto en precios constantes
  mutate(pib=pib/factor*f18,pibpc=pibpc/factor*f18) %>%# convertir PIB a precios constantes 2018
  mutate(gto2=gtoc/pib/1e6*100) %>% #gto en porcentaje de PIB
  mutate(gtopc=gtoc/pob/1e3,pob=pob/1e6) %>% #gto per cápita
  #mutate(across(soc:resto,~log(./gob))) %>% dplyr::select(-gob) %>% #logcocientes
  filter(year<=2019) %>%  #quitar proyección '20 y '21
  merge(ind.coord %>% dplyr::select(code,Grupo), by.x = "ISO3", by.y = "code") 
edoagrupado %>% 
  group_by(Grupo) %>% summarise(across(c(soc:gob,pibpc,cre,gtopc,gto2,pob),mean),n=n_distinct(ISO3)) %>% 
  mutate(across(soc:gob,~.*100),pibpc=pibpc/1000) %>% 
  relocate(gtopc,gto2,.after=gob) %>% 
  xtable(caption="Resumen de los estados por grupos",label="tab:edos_resumen",
         digits=c(rep(0,8),1,0,0,1,1,0)) %>% 
  print(include.rownames=F)

# Gasto % de PIB del estado
g_dat %>% as.data.frame() %>% 
  filter(year==2018) %>% 
  ggplot(aes(x=reorder(ISO3,-gto2),y=gto2,label=format(gto2,digits=2)))+
  geom_bar(stat = "identity",position = "dodge",fill=4)+
  geom_text(size=2.5,angle=90,nudge_y = -3,col="white")+
  #coord_cartesian(ylim = c(0,600))+
  theme_bw()+ theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  xlab("")+ylab("Gasto público (% del PIB)")
ggsave("./TeX/Fig/45_8_gtoedos.pdf",width = 12,height = 7,units = "cm")

# gasto per cápita
g_dat %>% as.data.frame() %>% 
  filter(year==2018) %>% 
  ggplot(aes(x=reorder(ISO3,-gtopc),y=gtopc,label=round(gtopc,1)))+
  geom_bar(stat = "identity",position = "dodge",fill=4)+
  geom_text(size=2.5,angle=90,nudge_y = c(rep(-5,3),-95,-5,-5,-140.2,rep(-5,25)),col="white")+
  coord_cartesian(ylim = c(0,60))+
  theme_bw()+ theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  xlab("")+ylab("Gasto per cápita (miles)")
ggsave("./TeX/Fig/45_9_gtopcedos.pdf",width = 12,height = 7,units = "cm")


# pib per cápita vs comp ingreso población
# library(MASS)

rm(temp)
temp<-g_dat %>% 
  merge(ping2 %>% mutate(s_0a3=s_0+s_0a1+s_1a2+s_2a3,
                         year=factor(year)) %>% 
          dplyr::select(ISO3,year,s_0a3,s_3a5,s_5mas)) %>%   #agregar población en ingreso bajo
  group_by(ISO3) %>% summarise(across(c(pibpc,s_0a3:s_5mas),mean))%>% 
  pivot_longer(cols = s_0a3:s_5mas,names_to="sm",values_to="pob")

sm.labs <- c("menos de 3SM", "de 3SM a 5SM", "mas de 5SM")
names(sm.labs) <- c("s_0a3", "s_3a5", "s_5mas")

sm.labs

temp  %>% 
  ggplot(aes(x=pibpc/1000,y=pob*100,label=ISO3))+geom_point()+
  geom_smooth(data=subset(temp,ISO3!="CAM"),method = "lm",formula = 'y~x',se=F)+
  facet_wrap(~sm,scales = "free",nrow = 1,
             labeller = labeller(sm = sm.labs))+
  theme_bw()+theme(legend.position = "none")+
  scale_x_log10()+xlab("PIB per cápita (miles)")+ylab("Población %")
ggsave("./TeX/Fig/45_10_pibpc_ing.pdf",width = 15,height = 10,units = "cm")



# Ternarias # Gasto
edoagrupado %>% dplyr::select(ISO3,year,soc:gob) %>% 
  unite("id",ISO3:year,sep = ".") %>% column_to_rownames("id") %>% 
  acomp() %>% 
  plot(margin="gob")

# Variación composicional  
edoagrupado %>% dplyr::select(ISO3,year,soc:gob) %>% 
  unite("id",ISO3:year,sep = ".") %>% column_to_rownames("id") %>% 
  acomp() %>% variation()

# barras por año
edoagrupado %>% 
  group_by(Grupo,year) %>% 
  #summarise(across(c(pob,pib),sum),pibpc=pib/pob) %>% 
  summarise(g=mean(cre),lg=quantile(cre,0.05),ug=quantile(cre,0.95)) %>% 
  ggplot(aes(x=year,y=g)) + geom_bar(stat = "identity",position = "dodge")+
  geom_errorbar(aes(ymin=lg, ymax=ug), width=.2,
                position=position_dodge(.9)) +
  facet_wrap(~Grupo)+theme_bw()

# boxplot por año
edoagrupado %>% 
  ggplot(aes(x=year,y=pibpc/1e3)) + geom_boxplot()+
  facet_wrap(~Grupo)+
  scale_y_log10()

# Composición media, acumulada (gasto, ingresos, pib)
# PIB per cápita, gasto percápita, Ingresos población
# Cuadro medias gfun por clusters entidades
# Normalidad datos estados

# Modelos x grupos



# modelos panel ####
g_mco<-plm(cre~gto2+soc+edu+eco+sal+resto,g_dat,model = "pooling") # ols
g_int<-plm(cre~gto2+soc+edu+eco+sal+resto,g_dat,model = "within", effect = "individual") # within
g_mcg<-plm(cre~gto2+soc+edu+eco+sal+resto,g_dat,model = "random", effect = "individual") # gls


# summary(g_mco)
# summary(g_int)
# summary(g_mcg)

fsignif=function(x,lvl=c(0.1,0.05,0.01)){
  y<-as.numeric(x)
  codigos = (y<lvl[1]) + (y<lvl[2]) + (y<lvl[3])
  codigos[codigos==3]<-"$^{***}$"
  codigos[codigos==2]<-"$^{**}$"
  codigos[codigos==1]<-"$^{*}$"
  res=paste(x,replace_na(codigos,""),sep="")
  return(res)}

#presencia de efectos
pres<-c(format(pFtest(update(g_int,effect="individual"),g_mco)$p.value,digits = 2),"","")
#Hausmant test, correlación de efectos
gcor<-c("","",format(phtest(g_int,g_mcg)$p.value,digits = 2))
#AR(1)
ar1<-sapply(list(g_mco,g_int,g_mcg),function(x) format(pbgtest(x)$p.value,digits = 2))
#AR(2)
ar2<-sapply(list(g_mco,g_int,g_mcg),function(x) format(pbgtest(x,order=2)$p.value,digits = 2))


stargazer(g_mco,g_int,g_mcg,digits = 2, #type = "text",
          title = "Regresión de panel para crecimiento económico",
          label = "tab:gmod0_edos",
          column.labels = c("$\\hat{\\und\\gamma}_{\\text{MCO}}$",
                            "$\\hat{\\und\\gamma}_{\\text{W}}$",
                            "$\\hat{\\und\\gamma}_{\\text{MCG}}$"),
          single.row = T,
          dep.var.caption = "Variable dependiente: $g_t$ de los estados",
          omit.stat = "F",
          dep.var.labels.include = F,
           covariate.labels=c("gto","$\\log$(soc/gob)","$\\log$(edu/gob)",
                              "$\\log$(eco/gob)","$\\log$(sal/gob)",
                              "$\\log$(resto/gob)","ordenada"),
          add.lines = list(c("F (efectos) p-val",pres %>% fsignif()),
                           c("Hausman p-value",gcor%>% fsignif()),
                           c("AR(1) p-value",ar1%>% fsignif()),
                           c("AR(2) p-value",ar2%>% fsignif()))
)



# # g
# g_dat %>% 
#   group_by(ISO3) %>% mutate(m=median(cre)) %>% ungroup() %>% 
#   ggplot(aes(x=reorder(ISO3,m),y=cre,label=round(cre,1)))+geom_boxplot()+
#   theme_bw()+xlab("")+ylab("Crecimiento real PIB per cápita")+
#   theme(axis.text.x = element_text(angle=90,vjust=0.5))
# ggsave("./TeX/Fig/45_4_g_edos.pdf",width = 12,height = 7,units = "cm")
# 
# g_dat %>% 
#   #group_by(ISO3) %>% summarise(pibpc=mean(pibpc),cre=mean(cre)) %>% 
#   #filter(year==2018) %>% 
#   ggplot(aes(x=pibpc/1000,y=cre,label=ISO3))+geom_point()+#geom_text_repel()+
#   scale_x_log10()+
#   theme_bw()+xlab("PIB per cápita mdp")+ylab("Crecimiento real PIB per cápita")
# ggsave("./TeX/Fig/45_5_g_pibpc.pdf",width = 12,height = 7,units = "cm")


# Efectos fijos
temp<-data.frame(ef=fixef(g_int,type="dmean")) %>% rownames_to_column("ISO3")
q<-sapply(temp$ef,function(x) sign(x)*min(abs(x),6))
elcolor<-ifelse(abs(temp$ef)>6,"white","black")
temp %>% 
  ggplot(aes(x=reorder(ISO3,-ef),y=ef))+
  geom_bar(stat = "identity",position = "dodge",fill=4)+
  geom_text(aes(y=q+2*sign(ef),label=format(ef,digits = 1)),angle=90,size=2.5,col=elcolor)+
  theme_bw()+theme(axis.text.x=element_text(angle=90,vjust = 0.5))+#coord_flip()+
  coord_cartesian(ylim = c(-10,10))+
  xlab("")+ylab("")
ggsave("./TeX/Fig/45_11_g_ef.pdf",width = 12,height = 7,units = "cm" )

# Correlación efectos fijos
fnames=c("soc","edu","eco","sal","resto")
sapply(c(fnames,"gto2","cre"), function (x) cor(fixef(g_int), between(g_dat[,x]))) %>% 
  t() %>% as.data.frame() %>% 
  xtable(caption = "Correlación vs los efectos individuales",label="tab:cor_ef_edo") %>% 
  print(includel.rownames=F)

# Autocorrelaciones
temp<-pibent2 %>% mutate(cre=100*(pib/dplyr::lag(pib)-1)) %>% filter(!is.na(cre),year<=2019)
temp<-g_int$residuals %>% as.data.frame() %>% rownames_to_column("id") %>% 
  separate(id,into = c("ISO3","year")) %>% rename(nus=3)

auto<-by(temp$nus, temp$ISO3, function(i) { acf(i,  plot = FALSE)$acf }) 

temp<-matrix(NA,nrow = 32,ncol = 6)
for (i in 1:32) {
  temp[i,1]<-names(auto)[i]
  m<-length(auto[[i]])
  temp[i,2:(m+1)]<-auto[[i]]
}
colnames(temp)<-c("ISO3",0:4)
auto<-temp %>% as.data.frame() %>%
  pivot_longer(cols = `0`:`4`,names_to="lag",values_to="cor") %>% 
  na.omit() %>% mutate(cor=as.numeric(cor),lag=as.numeric(lag))

auto %>% filter(lag <=15) %>% 
  ggplot(aes(x=lag,y=cor,group=lag)) +
  geom_boxplot()+theme_bw()+
  ylab("")
ggsave("./TeX/Fig/45_12_acfbox.pdf",width = 10,height = 8,units = "cm")





# sys GMM ####
eqsgmm<-cre ~ lag(cre) + gto2+
  soc+edu+eco+sal+resto |
  lag(cre, 2:99) | # exógenas en ef. ind. e idios.
  lag(gto2,2)+lag(soc, 2)+lag(edu,2)+lag(eco,2)+lag(sal,2)+lag(resto,2)

4*5/2 #system
6 # normales
temp<-g_dat %>% filter(!ISO3%in%c("CAM","TAB")) %>%as.data.frame() %>% droplevels() %>% pdata.frame()
class(g_dat)
class(temp)
pdim(temp)
pdim(g_dat)

base <- pgmm(formula=eqsgmm,data=g_dat,
             index=c("ISO3", "year"),model="twosteps",
             effect="twoways",transformation = "ld")
base.ind<-pgmm(formula=eqsgmm,g_dat,
               index=c("ISO3", "year"),model="twosteps",# subset = !ISO3%in%c("CAM","TAB"),
               effect="individual",transformation = "ld")
# basefil<-pgmm(formula=eqsgmm,data=temp,
#               index=c("ISO3", "year"),model="twosteps",# subset = !ISO3%in%c("CAM","TAB"),
#               effect="twoways",transformation = "ld")
# coeftest(basefil,vcov. = vcovHC)

sbase<-summary(base)
sbasei<-summary(base.ind)
# sbasef<-summary(basefil)

lmodel<-list(sbasei,sbase)
vsarg<-format(sapply(lmodel, function(x) x$sargan$p.value),digits = 2) 
ar1<-format(sapply(lmodel, function(x) x$m1$p.value),digits = 1)
ar2<-format(sapply(lmodel, function(x) x$m2$p.value),digits = 2)
waldmu<-c("",format(sbase$wald.td$p.value[[1]],digits = 1))#,format(sbasef$wald.td$p.value[[1]],digits = 1))

stargazer(base.ind,base, type="text",
          digits = 2,
          title = "sys-GMM para el crecimiento económico por estados",
          label = "tab:gsGMM_edos",
          column.labels = c("Sin $\\mu$", "Con $\\mu$", "Con $\\mu$ ex.pet."),
          dep.var.caption = "Variable dependiente: $g_t$ de los estados",
          single.row = T,
          dep.var.labels.include = F,
          covariate.labels=c("$g_{t-1}$","gto","$\\log$(soc/gob)","$\\log$(edu/gob)",
                              "$\\log$(eco/gob)","$\\log$(sal/gob)",
                              "$\\log$(resto/gob)"),
          omit.stat = "n",
          add.lines = list(#c("Observaciones",rep("1,296",3)),
            c("Estados",c(32,32)),
            c("Instrumentos",16,16),
            c("Sargan p-value",vsarg %>% fsignif()),
            c("AR(1) p-value",ar1 %>% fsignif()),
            c("AR(2) p-value",ar2 %>% fsignif()),
            c("Wald $\\mu_t$ p-value",waldmu %>% fsignif())
          ))




# errores ####

lsfun<-list("media"=mean,#"dev"=sd,
            "plo"=function(x,na.rm=T) quantile(x,probs = 0.25,na.rm),
            "pup"=function(x,na.rm=T) quantile(x,probs = 0.75,na.rm))


modelos<-list("(2) Temp"=base,"(1) Indiv"=base.ind)#,"(3) Temp ex.pet."=basefil)

for (i in 1:length(modelos)) {
  errores<-modelos[[i]]$residuals %>% as.data.frame() %>%
    mutate(year=names(base$residuals[[1]]),
           model=c(rep('d',3),rep('l',4))) %>% 
    pivot_longer(!c(year,model),names_to='code',values_to='resid') %>% 
    mutate(resid=na_if(resid,0)) %>% na.omit()
  temp<-errores %>% group_by(year) %>% summarise(across(resid,lsfun)) %>% mutate(modelo=names(modelos)[i])
  if (i==1) todo=temp else todo=rbind(todo,temp)
}

todo %>% ggplot(aes(x=as.numeric(year),y=resid_media))+
  geom_ribbon(aes(ymin=resid_plo,ymax=resid_pup,fill=modelo),alpha=0.2)+
  geom_line(aes(col=modelo),size=1)+theme_bw()+
  xlab("Año")+ylab("")+labs(col="",fill="")+
  theme(legend.position = c(.45,.95),
        legend.background = element_rect(fill = "transparent"))+
  guides(fill=guide_legend(nrow=1))
ggsave("./TeX/Fig/45_12_errores.pdf",width = 12,height = 5,units = "cm",bg="transparent")

# Efectos temporales
base$coefficients[[2]][9:11]
coeftest(base,vcov=vcovHC)
sqrt(diag(vcovHC(base)))
sqrt(diag(vcovHC(base))[9:11])
sqrt(diag(vcov(base))[9:11])
base$coefficients
base$vcov[c("2017","2018","2019"),c("2017","2018","2019")]

# Cuadros efectos sustitución
pibent2[131,'pib']/deflac[5,'factor']/100 #1% del PIB CMX 2019 precios corrientes en millones

pibent2 %>% filter(year==2019) %>% 
  summarise(pib=mean(pib)/deflac[5,"factor"]/100) #1% del PIB del edo prom. 2019 precios corrientes en millones

fnames<-c("soc","edu","eco","sal","resto")
gm<-function(iso,año){
  edoagrupado %>% mutate(across(soc:gob,~.*gto2),lg=dplyr::lag(cre)) %>% 
    filter(ISO3==iso,year==año) %>% 
    select(ISO3,year,lg,gto2,soc:gob,cre) %>% 
    unite("id",ISO3:year,sep=".") %>% column_to_rownames("id")
}
logob<-function(x) x %>% mutate(across(soc:resto,~log(./gob))) %>% 
  dplyr::select(-gob)
gm("CMX",2019)
base.ind$coefficients[[2]]

cambios<-c(0.1,0.5,1)
# inicial<-gm("CMX",2019)
# rebal<-inicial
# rebal[fnames[1]]<-gm("CMX",2019)[fnames[1]]+cambios[1]
# rebal[fnames[2]]<-gm("CMX",2019)[fnames[2]]-cambios[1]
# inicial
# rebal
# sum(logob(inicial)[1:7]*base.ind$coefficients[[2]])-sum(logob(rebal)[1:7]*base.ind$coefficients[[2]])

n<-length(fnames)
reasignaciones<-function(modelo){
  temp<-lapply(cambios, function(x){
    res=matrix(0,n,n)
    for(j in 1:n){
      for(k in 1:n){
        inicial<-gm("MEX",2019)
        rebal<-inicial
        rebal[fnames[j]]<-rebal[fnames[j]]+x
        rebal[fnames[k]]<-rebal[fnames[k]]-x
        res[j,k]<-sum(logob(rebal)[1:7]*modelo$coefficients[[2]])-sum(logob(inicial)[1:7]*modelo$coefficients[[2]])
      }
    } 
    return(res)
  })
  matriz<-rbind(temp[[1]],temp[[2]],temp[[3]])
  return(matriz)
}

tab<-cbind(reasignaciones(base.ind),reasignaciones(base)) %>% as.data.frame()
tab
colnames(tab)<-paste(rep(c("Ind","Temp."),each=n),rep(fnames,2))
tab$fun<-rep(fnames,3)
# tab$delta<-rep(cambios,each=n)

tab<-tab[11:15,] %>% relocate(fun,`Ind soc`)

tab %>% 
  xtable(caption = "Efectos de la reasignación del gasto",
         label="tab:gsGMM_delta_edos",digits=1) %>% 
  print(include.rownames=F)


# Forecast modelos # Predict 2020 - 2021
logob<-function(x) x %>% mutate(across(soc:resto,~log(./gob))) %>% 
  dplyr::select(-gob)
g_datp<-edodat %>% filter(!ISO3%in%c("TOT","MED")) %>% #excluir total y promedio
  merge(gtoconst %>% dplyr::select(ISO3,year,gto,gtoc,factor,f18)) %>% # agregar gasto en precios constantes
  mutate(pib=pib/factor*f18,pibpc=pibpc/factor*f18) %>%# convertir PIB a precios constantes 2018
  mutate(gto2=gtoc/pib/1e6*100) %>% #gto en porcentaje de PIB
  mutate(gtopc=gtoc/pob/1e3) %>% #gasto per cápita
  mutate(across(soc:resto,~log(./gob))) %>% dplyr::select(-gob) #%>% #logcocientes
  #filter(year>=2019)  # proyección '20 y '21


gm<-function(iso,año){
  g_datp %>% 
    group_by(ISO3) %>% 
    mutate(gob=0,across(c(soc:resto,gob),exp),lg=dplyr::lag(cre)) %>% 
    ungroup() %>% 
    mutate(tot= select(.,c(soc:resto,gob)) %>% rowSums(),
           across(c(soc:resto,gob),~./tot)) %>% 
    mutate(across(c(soc:resto,gob),~.*gto2)) %>% 
    filter(ISO3==iso,year==año) %>% 
    select(ISO3,year,lg,gto2,soc:resto,gob,cre) %>% 
    unite("id",ISO3:year,sep=".") %>% column_to_rownames("id")
}

gm("CMX",2020)

iso="MEX";año=2020

temp<-g_datp %>% #filter(year>2019) %>% 
  mutate(year=factor(year))
estima<-rep(0,nrow(temp))
for(i in 1:nrow(temp)){
  estima[i]<-sum(base.ind$coefficients[[2]]*
        logob(gm(temp$ISO3[i],temp$year[i]))[1:7])
}
temp$g_est<-estima


fit<-base.ind$fitted.values %>% 
  as.data.frame() %>% 
  mutate(year=c(2017:2019,2016:2019),modelo=c(rep("d",3),rep("l",4))) %>% 
  pivot_longer(AGU:ZAC,names_to="ISO3",values_to="gfit") %>% 
  filter(modelo=="l") %>% arrange(ISO3,year)
  
res<- base.ind$residuals %>% 
  as.data.frame() %>% 
  mutate(year=c(2017:2019,2016:2019),modelo=c(rep("d",3),rep("l",4))) %>% 
  pivot_longer(AGU:ZAC,names_to="ISO3",values_to="resid") %>% 
  filter(modelo=="l") %>% arrange(ISO3,year)
  
g_dat2<-g_dat %>% merge(temp,all = T) %>% 
  dplyr::select(ISO3,year,estado,cre,pibpc,g_est) %>% 
  merge(ind.coord %>% select(code,Grupo),by.x = "ISO3",by.y = "code",all.x = T) %>% 
  #merge(fit %>% dplyr::select(-modelo),all=T) %>% 
  merge(res %>% dplyr::select(-modelo),all=T) %>% 
  mutate(year=as.numeric(as.character(year))) %>% 
  group_by(ISO3) %>% 
  mutate(pibfit=case_when(year<=2019 ~ pibpc,
                          year==2020 ~ dplyr::lag(pibpc)*(1+g_est/100),
                          year==2021 ~ dplyr::lag(pibpc,2)*(1+dplyr::lag(g_est,1)/100)*(1+g_est/100))) %>% 
  mutate(check=100*(pibfit/dplyr::lag(pibfit)-1))

# Gráfica obs vs fitted g

sm.labs <- paste("Grupo",1:6,
                 c('(centro)','(norte)','(golfo)','(petrolero)','(bajo ing.)','(cmdx)'))
names(sm.labs) <- as.character(1:6)

sm.labs
g_dat2 %>% group_by(Grupo,year) %>% 
  summarise(se=sd(resid,na.rm = T)/n(),across(c(cre,g_est,resid),mean,na.rm=T)) %>% 
  mutate(cre=ifelse(year<=2019,cre,NA)) %>% #quitar proyecciones iniciales
  ggplot(aes(x=year))+
  #geom_bar(aes(y=cre),stat="identity",position = "dodge")+
  #geom_ribbon(aes(ymin=g_est-se,ymax=g_est+se),fill=4)+
  geom_point(aes(y=cre),col=1)+
  geom_line(aes(y=g_est),col=2)+
  facet_wrap(~Grupo,scales = "free",labeller = labeller(Grupo=sm.labs))+
  theme_bw()+xlab(NULL)+ylab('Crecimiento económico real %')
ggsave("./TeX/Fig/45_13_g20y21.pdf",width = 15,height = 10,units = "cm")


# Gráfica obs vs fitted pibpc
g_dat2 %>% group_by(Grupo,year) %>% 
  summarise(across(c(cre,g_est,resid,pibpc,pibfit),mean,na.rm=T)) %>% 
  mutate(pibpc=ifelse(year<=2019,pibpc,NA)) %>% #quitar proyecciones iniciales
  ggplot(aes(x=year))+
  #geom_bar(aes(y=cre),stat="identity",position = "dodge")+
  #geom_ribbon(aes(ymin=g_est-se,ymax=g_est+se),fill=4)+
  geom_point(aes(y=pibpc),col=1)+
  geom_line(aes(y=pibfit),col=2)+
  facet_wrap(~Grupo,scales = "free")+
  theme_bw()


# población por ingresos ####

ping<-read_excel("./Data/INEGI/poblacióningresos.xlsx",skip = 4) %>% 
  pivot_longer(cols = !c(Entidad,Periodo),names_to="variable",values_to="pob") %>% 
  filter(grepl("Cuarto",Periodo),!grepl("cv|est",variable)) %>% 
  pivot_wider(names_from = variable,values_from=pob) %>% 
  mutate(year=as.numeric(substr(Periodo,22,25))) %>% 
  mutate(across(Total:s_ne,as.numeric)) %>% arrange(Entidad,year)
cat_ent2<-data.frame(Entidad=sort(unique(ping$Entidad)[-30]), # 30 es el total
           estado=unique(cat_entidad$estado)[-19]) %>% # 19 es No Dististribuible
  merge(cat_entidad %>% filter(entfed!="Distrito Federal") %>% dplyr::select(estado,ISO3))

# con catalogo ISO3 para estado  
ping2<-ping %>% merge(cat_ent2 %>% dplyr::select(Entidad,ISO3)) %>%
  mutate(across(s_0a1:s_0,~./Total)) %>% #en porcentaje del total
  relocate(ISO3,year) %>% relocate(s_0,.before=s_0a1) %>% 
  dplyr::select(-Entidad,-Periodo) %>% 
  arrange(ISO3,year) #%>% 
  # group_by(ISO3) %>% 
  # mutate(across(Total:s_0,~100*(./dplyr::lag(.)-1),.names ="g_{.col}" ))

pnames <- c('SM=0','SM in (0,1]','SM in (1,2]','SM in (2,3]',
            'SM in (3,5]','SM in (5,inf]')
pnames <- c('0','(0,1]','(1,2]','(2,3]','(3,5]',
            paste('(5,',expression(infinity),')',sep=''))
pnames <- c('0','De 0 a 1','De 1 a 2','De 2 a 3','De 3 a 5','5+')
onames <- names(ping2)[4:9]
names(ping2)[4:9] <- pnames

# PEA composición salario detalle total
peacomp <- ping2 %>% 
  dplyr::select(-c(Total,s_ne)) %>% 
  unite("id",ISO3:year,sep='.') %>% column_to_rownames("id") %>%
  acomp()

# Variation matrix

rbind(mean(peacomp,robust = T),variation(peacomp,robust = T)) %>% 
  xtable(caption = 'Media y Varianza Composicional',
         label='tab:pea_medvarcomp',digits=2) %>% 
  print(include.rownames=T)

# Dendrograma
pdf('./TeX/Fig/45p_1_pea_dendro.pdf',width = 6,height = 4)
variation(peacomp,robust = T) %>% as.dist() %>% hclust(method = "ward.D") %>% 
  plot(main=NA,sub=NA,ylab='Distancia')
dev.off()


# categorías reducidas ###
names(ping2)[4:9] <- onames
ging_dat <- ping2 %>% dplyr::select(ISO3,year,Total:s_ne) %>%
  mutate(s_0a3=s_0+s_0a1+s_1a2,s_3a5=s_2a3+s_2a3) %>% dplyr::select(-c(s_0a1:s_3a5,s_0)) %>% 
  relocate(s_0a2,s_2a5,.before=s_5mas) 


# boxplots tasas categorías reducidas
pnames<-c('De 0 a 2 SM','De 2 a 5 SM','Más de 5 SM')
names(pnames) <- c('s_0a2','s_2a5','s_5mas')
ging_dat %>% dplyr::select(ISO3,year,Total:s_5mas) %>%
  group_by(ISO3) %>% mutate(m1=median(s_0a2),m2=median(s_2a5)) %>% ungroup() %>% 
  pivot_longer(cols = Total:s_5mas,names_to="grupo",values_to="p") %>% na.omit() %>% 
  filter(grupo!="Total") %>% mutate(p=p*100) %>% 
  ggplot(aes(x=reorder(ISO3,-m1),y=p))+geom_boxplot()+
  facet_wrap(~grupo,scales = "free",nrow = 3,labeller = labeller(grupo = pnames))+
  theme_bw()+xlab("")+ylab("Población %")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))
ggsave("./TeX/Fig/45_6_pobingreso.pdf",width = 12,height = 15,units = "cm")

# scatter bottom vs gasto
g_dat %>% dplyr::select(ISO3,year,soc:resto,gto2) %>% 
  pivot_longer(cols = c(soc:resto,gto2),names_to="explic",values_to="valor") %>% 
  merge(ging_dat %>% dplyr::select(ISO3,year,Total:s_5mas) %>% 
          mutate(year=factor(year))) %>% 
  ggplot(aes(x=valor,y=s_0a2))+
  facet_wrap(~explic,scales = "free")+
  geom_point()+xlab('Gasto público')+
  ylab('% de la población < 2 SM')+
  theme(legend.position = "none")
ggsave('./TeX/Fig/45p_2_gto_vs_bajo.pdf',height = 10,width = 12,units = 'cm')

# scatter logc bottom vs gasto
g_dat %>% dplyr::select(ISO3,year,soc:resto,gto2) %>% 
  pivot_longer(cols = c(soc:resto,gto2),names_to="explic",values_to="valor") %>% 
  merge(ging_dat %>% dplyr::select(ISO3,year,Total:s_5mas) %>% 
          mutate(year=factor(year),s_0a2=log(s_0a2/s_5mas))) %>% 
  ggplot(aes(x=valor,y=s_0a2))+
  facet_wrap(~explic,scales = "free")+
  geom_point()+xlab('Gasto público')+
  ylab('% de la población < 2 SM')+
  theme(legend.position = "none")


# Datos
ging_dat <- ging_dat  %>% dplyr::select(ISO3,year,Total:s_5mas) %>%
  merge(g_dat %>% dplyr::select(ISO3,year,soc:resto,gto2)) %>% pdata.frame()

pdim(ging_dat)
# modelos panel población| ingresos ####
g_mco<-plm(s_0a2~gto2+soc+edu+eco+sal+resto,ging_dat,model = "pooling") # ols
g_int<-plm(s_0a2~gto2+soc+edu+eco+sal+resto,ging_dat,model = "within", effect = "individual") # within
g_mcg<-plm(s_0a2~gto2+soc+edu+eco+sal+resto,ging_dat,model = "random", effect = "individual") # gls


# summary(g_mco)
# summary(g_int)
# summary(g_mcg)


#presencia de efectos
pres<-c(format(pFtest(update(g_int,effect="individual"),g_mco)$p.value,digits = 2),"","")
#Hausmant test, correlación de efectos
gcor<-c("","",format(phtest(g_int,g_mcg)$p.value,digits = 2))
#AR(1)
ar1<-sapply(list(g_mco,g_int,g_mcg),function(x) format(pbgtest(x)$p.value,digits = 2))
#AR(2)
ar2<-sapply(list(g_mco,g_int,g_mcg),function(x) format(pbgtest(x,order=2)$p.value,digits = 2))


stargazer(g_mco,g_int,g_mcg,digits = 2, type = "text",
          title = "Regresión de panel para población de ingreso bajo",
          label = "tab:pmod0_edos",
          column.labels = c("$\\hat{\\und\\gamma}_{\\text{MCO}}$",
                            "$\\hat{\\und\\gamma}_{\\text{W}}$",
                            "$\\hat{\\und\\gamma}_{\\text{MCG}}$"),
          single.row = T,
          dep.var.caption = "Variable dependiente: Población|Ingreso < 2SM",
          omit.stat = "F",
          dep.var.labels.include = F,
          # covariate.labels=c("gto","$\\log$(soc/gob)","$\\log$(edu/gob)",
          #                    "$\\log$(eco/gob)","$\\log$(sal/gob)",
          #                    "$\\log$(resto/gob)","ordenada"),
          add.lines = list(c("F (efectos) p-val",pres %>% fsignif()),
                           c("Hausman p-value",gcor%>% fsignif()),
                           c("AR(1) p-value",ar1%>% fsignif()),
                           c("AR(2) p-value",ar2%>% fsignif()))
)

# sys GMM población| ingresos ####

eqsgmm<-s_0a3 ~ lag(s_0a3) + gto2+
  soc+edu+eco+sal+resto |
  lag(s_0a3, 2:99) | # exógenas en ef. ind. e idios.
  lag(gto2,2)+lag(soc, 2)+lag(edu,2)+lag(eco,2)+lag(sal,2)+lag(resto,2)

# 4*5/2 #system
# 6 # normales


base <- pgmm(formula=eqsgmm,data=ging_dat,
             index=c("ISO3", "year"),model="twosteps",# subset = !ISO3%in%c("CAM","TAB"),
             effect="twoways",transformation = "ld")
base.ind<-pgmm(formula=eqsgmm,ging_dat,
               index=c("ISO3", "year"),model="twosteps",# subset = !ISO3%in%c("CAM","TAB"),
               effect="individual",transformation = "ld")

# coeftest(base,vcov. = vcovHC)
# sbase<-summary(base)
# sbasei<-summary(base.ind)


lmodel<-list(sbasei,sbase)
vsarg<-format(sapply(lmodel, function(x) x$sargan$p.value),digits = 2) 
ar1<-format(sapply(lmodel, function(x) x$m1$p.value),digits = 1)
ar2<-format(sapply(lmodel, function(x) x$m2$p.value),digits = 2)
waldmu<-c("",format(sbase$wald.td$p.value[[1]],digits = 1))#,format(sbasef$wald.td$p.value[[1]],digits = 1))

stargazer(base.ind,base,# type="text",
          digits = 2,
          title = "sys-GMM para para población de ingreso bajo",
          label = "tab:psGMM_edos",
          column.labels = c("Sin $\\mu$", "Con $\\mu$", "Con $\\mu$ ex.pet."),
          dep.var.caption = "Variable dependiente: Población|Ingreso < 3SM",
          single.row = T,
          dep.var.labels.include = F,
          # covariate.labels=c("$g_{t-1}$","gto","$\\log$(soc/gob)","$\\log$(edu/gob)",
          #                    "$\\log$(eco/gob)","$\\log$(sal/gob)",
          #                    "$\\log$(resto/gob)"),
          omit.stat = "n",
          add.lines = list(#c("Observaciones",rep("1,296",3)),
            c("Estados",c(32,32)),
            c("Instrumentos",16,16),
            c("Sargan p-value",vsarg %>% fsignif()),
            c("AR(1) p-value",ar1 %>% fsignif()),
            c("AR(2) p-value",ar2 %>% fsignif()),
            c("Wald $\\mu_t$ p-value",waldmu %>% fsignif())
          ))


# categorías reducidas ####
ging_dat <- ping2 %>% dplyr::select(ISO3,year,Total:s_ne) %>%
  mutate(s_0a3=s_0+s_0a1+s_1a2+s_2a3) %>% dplyr::select(-c(s_0a1:s_2a3,s_0)) %>% 
  relocate(s_0a3,.before=s_3a5) %>% select(-s_ne)

ging_dat %>% head()
# boxplots tasas categorías reducidas
pnames<-c('De 0 a 3 SM','De 3 a 5 SM','Más de 5 SM')
#pnames<-c(expression('p'[3]),expression('p'[5]),expression('p'[5+]))
names(pnames) <- c('s_0a3','s_3a5','s_5mas')
ging_dat %>% dplyr::select(ISO3,year,Total:s_5mas) %>%
  group_by(ISO3) %>% mutate(m1=median(s_0a3)) %>% ungroup() %>% 
  pivot_longer(cols = Total:s_5mas,names_to="grupo",values_to="p") %>% na.omit() %>% 
  filter(grupo!="Total") %>% mutate(p=p*100) %>% 
  ggplot(aes(x=reorder(ISO3,-m1),y=p))+geom_boxplot()+
  facet_wrap(~grupo,scales = "free",nrow = 3,labeller = labeller(grupo = pnames))+
  theme_bw()+xlab("")+ylab("Población %")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))
ggsave("./TeX/Fig/45_6_pobingreso.pdf",width = 12,height = 15,units = "cm")

# scatter bottom vs gasto
g_dat %>% dplyr::select(ISO3,year,soc:resto,gto2) %>% 
  pivot_longer(cols = c(soc:resto,gto2),names_to="explic",values_to="valor") %>% 
  merge(ging_dat %>% dplyr::select(ISO3,year,Total:s_5mas) %>% 
          mutate(year=factor(year))) %>% 
  ggplot(aes(x=valor,y=s_0a3))+
  facet_wrap(~explic,scales = "free")+
  geom_point()+xlab('Gasto público')+
  ylab('% de la población < 3 SM')+
  theme(legend.position = "none")
#ggsave('./TeX/Fig/45p_2_gto_vs_bajo.pdf',height = 10,width = 12,units = 'cm')

# scatter logc bottom vs gasto
g_dat %>% dplyr::select(ISO3,year,soc:resto,gto2) %>% 
  pivot_longer(cols = c(soc:resto,gto2),names_to="explic",values_to="valor") %>% 
  merge(ging_dat %>% dplyr::select(ISO3,year,Total:s_5mas) %>% 
          mutate(year=factor(year),s_0a3=log(s_0a3/s_5mas))) %>% 
  ggplot(aes(x=valor,y=s_0a3))+
  facet_wrap(~explic,scales = "free")+
  geom_point()+xlab('Gasto público')+
  ylab('% de la población < 3 SM')+
  theme(legend.position = "none")


# Datos
ging_dat <- ging_dat  %>% dplyr::select(ISO3,year,Total:s_5mas) %>%
  merge(g_dat %>% dplyr::select(ISO3,year,soc:resto,gto2)) %>% pdata.frame()

pdim(ging_dat)
# modelos panel población| ingresos ####
g_mco<-plm(s_0a3~gto2+soc+edu+eco+sal+resto,ging_dat,model = "pooling") # ols
g_int<-plm(s_0a3~gto2+soc+edu+eco+sal+resto,ging_dat,model = "within", effect = "individual") # within
g_mcg<-plm(s_0a3~gto2+soc+edu+eco+sal+resto,ging_dat,model = "random", effect = "individual") # gls


# summary(g_mco)
# summary(g_int)
# summary(g_mcg)


#presencia de efectos
pres<-c(format(pFtest(update(g_int,effect="individual"),g_mco)$p.value,digits = 2),"","")
#Hausmant test, correlación de efectos
gcor<-c("","",format(phtest(g_int,g_mcg)$p.value,digits = 2))
#AR(1)
ar1<-sapply(list(g_mco,g_int,g_mcg),function(x) format(pbgtest(x)$p.value,digits = 2))
#AR(2)
ar2<-sapply(list(g_mco,g_int,g_mcg),function(x) format(pbgtest(x,order=2)$p.value,digits = 2))


stargazer(g_mco,g_int,g_mcg,digits = 2, type = "text",
          title = "Regresión de panel para población de ingreso bajo",
          label = "tab:pmod0_edos",
          column.labels = c("$\\hat{\\und\\gamma}_{\\text{MCO}}$",
                            "$\\hat{\\und\\gamma}_{\\text{W}}$",
                            "$\\hat{\\und\\gamma}_{\\text{MCG}}$"),
          single.row = T,
          dep.var.caption = "Variable dependiente: Población|Ingreso < 2SM",
          omit.stat = "F",
          dep.var.labels.include = F,
          # covariate.labels=c("gto","$\\log$(soc/gob)","$\\log$(edu/gob)",
          #                    "$\\log$(eco/gob)","$\\log$(sal/gob)",
          #                    "$\\log$(resto/gob)","ordenada"),
          add.lines = list(c("F (efectos) p-val",pres %>% fsignif()),
                           c("Hausman p-value",gcor%>% fsignif()),
                           c("AR(1) p-value",ar1%>% fsignif()),
                           c("AR(2) p-value",ar2%>% fsignif()))
)

# Graficas pendientes ####
# Ternarias
# install.packages('crop')
# library(crop)

pnames <- c(expression('p'[3]),expression('p'[5]),expression('p'['5+']))
par(mar=rep(0,4))
pdf('./TeX/Fig/45p_3_pea_tern.pdf',width = 4,height = 4)
ging_dat %>% select(s_0a3:s_5mas) %>% acomp() %>% plot(axes=T,labels=pnames)
dev.off()

pdf('./TeX/Fig/45p_3b_pea_tern_centro.pdf',width = 4,height = 4)
ging_dat %>% select(s_0a3:s_5mas) %>% acomp() %>% plot(center=T,axes = T,labels=pnames)
dev.off()

# Zoom ultimo año
ping2 %>% dplyr::select(-c(Total,s_ne)) %>% 
  filter(year==2018|year==2019) %>% 
  group_by(ISO3) %>% 
  mutate(across(s_0:s_5mas,~.-dplyr::lag(.))) %>% ungroup() %>%
  na.omit() %>% 
  pivot_longer(cols = s_0:s_5mas,names_to='salario',values_to='porcentaje') %>% 
  ggplot(aes(x=ISO3,y=porcentaje,col=salario,group=salario))+geom_line(size=1)+
    theme_bw()+theme(axis.text.x = element_text(angle = 90))+
    xlab(NULL)+ylab('Cambio 2018 a 2019')+scale_color_locuszoom()

pnames <- c('Hasta 1','Hasta 2','Hasta 3','Hasta 5','Más de 5')
ping2 %>% dplyr::select(-c(Total,s_ne)) %>% 
  filter(year%in% c(2016:2019)) %>% 
  group_by(ISO3) %>% 
  mutate(across(s_0:s_5mas,~.-dplyr::lag(.))) %>%
  na.omit() %>% ungroup() %>% 
  group_by(year) %>%  summarise(across(s_0:s_5mas,mean)) %>% 
  pivot_longer(cols = s_0:s_5mas,names_to='salario',values_to='porcentaje') %>% 
  filter(salario!='s_0') %>% 
  mutate(salario=rep(pnames,3)) %>% 
  ggplot(aes(x=salario,y=porcentaje,fill=factor(year)))+
  geom_bar(stat='identity',position='dodge')+
  theme_bw()+xlab('Ingreso en SM')+ylab('Cambio en la composición')+
  labs(fill='Año')
ggsave('./TeX/Fig/45p_4_cambiopea.pdf',height = 7,width = 12,units = 'cm')  


ging_dat %>% filter(year==2018|year==2019) %>% 
  ggplot(aes(x=ISO3,y=s_0a3,col=year))+
  geom_point()+
  theme_bw()+theme(axis.text.x = element_text(angle = 90))+
  xlab(NULL)+ylab('Población con Ingreso < 3SM')

# Efectos fijos
temp<-data.frame(ef=fixef(g_int,type="dmean")*100) %>% rownames_to_column("ISO3")
q<-sapply(temp$ef,function(x) sign(x)*min(abs(x),13))
elcolor<-ifelse(abs(temp$ef)>15,"white","black")
temp %>% 
  ggplot(aes(x=reorder(ISO3,-ef),y=ef))+
  geom_bar(stat = "identity",position = "dodge",fill=4)+
  geom_text(aes(y=q+sign(ef)*2.5,label=format(ef,digits = 1)),angle=90,size=2.5,col=elcolor)+
  theme_bw()+theme(axis.text.x=element_text(angle=90,vjust = 0.5))+#coord_flip()+
  coord_cartesian(ylim = c(-20,20))+
  xlab("")+ylab("")
ggsave("./TeX/Fig/45p_5_p_ef.pdf",width = 12,height = 7,units = "cm" )

# Correlacion con efectos
fnames=c("soc","edu","eco","sal","resto")
sapply(c(fnames,"gto2","cre"), function (x) cor(fixef(g_int), between(g_dat[,x]))) %>% 
  t() %>% as.data.frame() %>% 
  xtable(caption = "Correlación vs los efectos individuales",
         label="tab:cor_p_ef_edo") %>% 
  print(includel.rownames=F)
# AR residuales

#temp<-pibent2 %>% mutate(cre=100*(pib/dplyr::lag(pib)-1)) %>% filter(!is.na(cre),year<=2019)
temp<-g_mcg$residuals %>% as.data.frame() %>% rownames_to_column("id") %>% 
  separate(id,into = c("ISO3","year")) %>% rename(nus=3)

auto<-by(temp$nus, temp$ISO3, function(i) { acf(i,  plot = FALSE)$acf }) 

temp<-matrix(NA,nrow = 32,ncol = 6)
for (i in 1:32) {
  temp[i,1]<-names(auto)[i]
  m<-length(auto[[i]])
  temp[i,2:(m+1)]<-auto[[i]]
}
colnames(temp)<-c("ISO3",0:4)
auto<-temp %>% as.data.frame() %>%
  pivot_longer(cols = `0`:`4`,names_to="lag",values_to="cor") %>% 
  na.omit() %>% mutate(cor=as.numeric(cor),lag=as.numeric(lag))

auto %>% filter(lag <=15) %>% 
  ggplot(aes(x=lag,y=cor,group=lag)) +
  geom_boxplot()+theme_bw()+
  ylab("")
ggsave("./TeX/Fig/45p_6_acfbox.pdf",width = 10,height = 8,units = "cm")



# errores ####

lsfun<-list("media"=mean,#"dev"=sd,
            "plo"=function(x,na.rm=T) quantile(x,probs = 0.25,na.rm),
            "pup"=function(x,na.rm=T) quantile(x,probs = 0.75,na.rm))


modelos<-list("(1) MCO"=g_mco,"(2) E. fijos"=g_int,"(3) E. aleatorios"= g_mcg)

for (i in 1:length(modelos)) {
  errores<-modelos[[i]]$residuals %>% as.data.frame() %>% rename('resid'=1) %>% 
    #mutate(year=names(g_mco$residuals)) 
    rownames_to_column('id') %>% separate('id',into = c('ISO3','year'))
    #mutate(resid=na_if(resid,0)) %>% na.omit()
  temp<-errores %>% group_by(year) %>% summarise(across(resid,lsfun)) %>% 
    mutate(modelo=names(modelos)[i])
  if (i==1) todo=temp else todo=rbind(todo,temp)
}

todo %>% ggplot(aes(x=as.numeric(year),y=resid_media))+
  geom_ribbon(aes(ymin=resid_plo,ymax=resid_pup,fill=modelo),alpha=0.2)+
  geom_line(aes(col=modelo),size=1)+theme_bw()+
  xlab("Año")+ylab("")+labs(col="",fill="")+
  theme(legend.position = c(.45,.95),
        legend.background = element_rect(fill = "transparent"))+
  guides(fill=guide_legend(nrow=1))
ggsave("./TeX/Fig/45p_7_errores.pdf",width = 12,height = 5,units = "cm",bg="transparent")
rm(temp,todo,modelos,lsfun)

# Cuadros efectos sustitución


predict(g_mcg,newdata=logob(gm('CMX',2019)))
g_mcg$residuals['CMX-2019']
g_mcg$df.residual['CMX-2019']
temp <- data.frame(ida=rownames(ging_dat),s_0a3=ging_dat$s_0a3,
                   idb=names(g_mcg$residuals),residu=g_mcg$residuals,
                   #residu2=residuals(g_mcg),
                   #fit=fitted(g_mcg),
                   re=rep(ranef(g_mcg),each=5),
                   pred=predict(g_mcg,newdata = ging_dat),
                   mmult=cbind(1,ging_dat %>% dplyr::select(gto2,soc:resto)) %>%
                     as.matrix %*% g_mcg$coefficients) %>% 
  mutate(fit2=mmult+re,
         res2=s_0a3-fit2,
         res3=s_0a3-mmult,
         dif=residu-res2)



# sigmas estimators basic
sl <- sqrt(g_mco$residuals %*% Between(g_mco$residuals)/32)
sv <- sqrt(g_mco$residuals %*% Within(g_mco$residuals)/(32*4))
si <- sqrt((sl^2-sv^2)/5)
si
sv
sqrt(g_mcg$ercomp$sigma2)
#theta grande: T grande / eta grande / nu chico/ más cerca de Within(FE)
# Swamy Arora (default)
g_mcg$ercomp

temp %>% 
  separate(ida,into = c('ISO3','year')) %>% 
  filter(ISO3=='MEX') %>% 
  dplyr::select(ISO3,year,s_0a3,fit2,mmult,residu,res2) %>% 
  mutate(across(s_0a3:res2,as.numeric)) %>% 
  pivot_longer(!c(ISO3,year),names_to='ser',values_to='value') %>% 
  filter(ser%in% c('s_0a3','fit2','mmult')) %>%
  ggplot(aes(x=year,y=value,group=ser,col=ser))+geom_line(size=1)

temp %>% 
  separate(ida,into = c('ISO3','year')) %>% 
  filter(ISO3=='MEX') %>% 
  dplyr::select(ISO3,year,s_0a3,fit2,mmult,residu,res2) %>% 
  mutate(across(s_0a3:res2,as.numeric)) %>% 
  pivot_longer(!c(ISO3,year),names_to='ser',values_to='value') %>% 
  filter(ser%in% c('residu','res2')) %>%
  ggplot(aes(x=year,y=value,group=ser,col=ser))+geom_line(size=1)
fnames<-c("soc","edu","eco","sal","resto")
gm<-function(iso,año){
  edoagrupado %>% mutate(across(soc:gob,~.*gto2),lg=1) %>% 
    merge(ging_dat %>% dplyr::select(ISO3,year,s_0a3)) %>% 
    mutate(re=rep(ranef(g_mcg),each=5)) %>% 
    filter(ISO3==iso,year==año) %>% 
    select(ISO3,year,lg,gto2,soc:gob,s_0a3,re) %>% 
    unite("id",ISO3:year,sep="-") %>% column_to_rownames("id")
}
logob<-function(x) x %>% mutate(across(soc:resto,~log(./gob))) %>% 
  dplyr::select(-gob)
gm("CMX",2019) %>% logob
g_mcg$coefficients

cambios<-c(0.1,0.5,1)
# inicial<-gm("CMX",2019)
# rebal<-inicial
# rebal[fnames[1]]<-gm("CMX",2019)[fnames[1]]+cambios[1]
# rebal[fnames[2]]<-gm("CMX",2019)[fnames[2]]-cambios[1]
# inicial
# rebal
# sum(logob(inicial)[1:7]*g_mcg$coefficients)
# predict(g_mcg,logob(inicial))-predict(g_mcg,logob(rebal))


n<-length(fnames)
reasignaciones<-function(modelo){
  temp<-lapply(cambios, function(x){
    res=matrix(0,n,n)
    for(j in 1:n){
      for(k in 1:n){
        inicial<-gm("MEX",2019)
        rebal<-inicial
        rebal[fnames[j]]<-rebal[fnames[j]]+x
        rebal[fnames[k]]<-rebal[fnames[k]]-x
        res[j,k]<-predict(modelo,logob(rebal))-predict(modelo,logob(inicial))
      }
    } 
    return(res)
  })
  matriz<-rbind(temp[[1]],temp[[2]],temp[[3]])
  return(matriz)
}

tab<-cbind(reasignaciones(g_mcg)) %>% as.data.frame()
tab
colnames(tab)<-fnames
tab$fun<-rep(fnames,3)
# tab$delta<-rep(cambios,each=n)

tab<-tab[11:15,] %>% relocate(fun,.before=1) %>% mutate(across(soc:resto,~.*100))
tab
tab %>% 
  xtable(caption = "Efectos de la reasignación del gasto en la PEA con ingreso < 3SM",
         label="tab:mcg_pdelta_edos",digits=1) %>% 
  print(include.rownames=F)


# Forecast modelos # Predict 2020 - 2021
g_datp<-edodat %>% filter(!ISO3%in%c("TOT","MED")) %>% #excluir total y promedio
  merge(gtoconst %>% dplyr::select(ISO3,year,gto,gtoc,factor,f18)) %>% # agregar gasto en precios constantes
  mutate(pib=pib/factor*f18,pibpc=pibpc/factor*f18) %>%# convertir PIB a precios constantes 2018
  mutate(gto2=gtoc/pib/1e6*100) %>% #gto en porcentaje de PIB
  mutate(gtopc=gtoc/pob/1e3) %>% #gasto per cápita
  mutate(across(soc:resto,~log(./gob))) %>% dplyr::select(-gob) %>% #logcocientes
  merge(ging_dat %>% dplyr::select(ISO3,year,s_0a3),all.x = T)
#filter(year>=2019)  # proyección '20 y '21

g_datp %>% head()
gm<-function(iso,año){
  g_datp %>% 
    group_by(ISO3) %>% 
    mutate(gob=0,across(c(soc:resto,gob),exp),lg=1) %>% 
    ungroup() %>% 
    mutate(tot= select(.,c(soc:resto,gob)) %>% rowSums(),# total
           across(c(soc:resto,gob),~./tot)) %>% # cerradura composicional
    mutate(across(c(soc:resto,gob),~.*gto2)) %>% # base porc de PIB
    filter(ISO3==iso,year==año) %>% 
    select(ISO3,year,lg,gto2,soc:resto,gob,s_0a3) %>% 
    unite("id",ISO3:year,sep=".") %>% column_to_rownames("id")
}

gm("CMX",2020)

iso="CMX";año=2020

temp<-g_datp %>% #filter(year>2019) %>% 
  mutate(year=factor(year))
estima<-rep(0,nrow(temp))
for(i in 1:nrow(temp)){
  estima[i]<-sum(base.ind$coefficients[[2]]*
                   logob(gm(temp$ISO3[i],temp$year[i]))[1:7])
}
temp$g_est<-estima

  
res <- g_mcg$residuals %>% as.data.frame() %>% rownames_to_column('id') %>% 
  separate(id,into = c('ISO3','year')) %>% rename(resid=3)

g_dat2<-g_datp %>%dplyr::select(-c(pib:f18)) %>% 
  mutate(s_0a3est=predict(g_mcg,g_datp) + rep(ranef(g_mcg),each=7)) %>% 
  dplyr::select(ISO3,year,estado,s_0a3,s_0a3est) %>% 
  merge(ind.coord %>% select(code,Grupo),by.x = "ISO3",by.y = "code",all.x = T) %>% 
  #merge(fit %>% dplyr::select(-modelo),all=T) %>% 
  merge(res ,all=T) %>% 
  mutate(year=as.numeric(as.character(year))) #%>% 
  # group_by(ISO3) %>% 
  # mutate(pibfit=case_when(year<=2019 ~ pibpc,
  #                         year==2020 ~ dplyr::lag(pibpc)*(1+g_est/100),
  #                         year==2021 ~ dplyr::lag(pibpc,2)*(1+dplyr::lag(g_est,1)/100)*(1+g_est/100))) %>% 
  # mutate(check=100*(pibfit/dplyr::lag(pibfit)-1))



# Gráfica obs vs fitted g
sm.labs <- paste("Grupo",1:6)
names(sm.labs) <- as.character(1:6)

sm.labs
g_dat2 %>% group_by(Grupo,year) %>% 
  summarise(se=sd(resid,na.rm = T)/n(),
            across(c(s_0a3,s_0a3est,resid),~mean(.)*100,na.rm=T)) %>% 
  ggplot(aes(x=year))+
  #geom_bar(aes(y=cre),stat="identity",position = "dodge")+
  #geom_ribbon(aes(ymin=g_est-se,ymax=g_est+se),fill=4)+
  geom_point(aes(y=s_0a3),col=1)+
  geom_line(aes(y=s_0a3est),col=2)+
  facet_wrap(~Grupo,scales = "free",labeller = labeller(Grupo=sm.labs))+
  theme_bw()
ggsave("./TeX/Fig/45p_8_p20y21.pdf",width = 15,height = 10,units = "cm")

# Mapas
rm(temp)
temp <- ind.coord %>% group_by(Grupo,code) %>% summarise(n=n())
paste(temp$code[temp$Grupo==1],collapse = T)

library(mxmaps)
temp<-df_mxstate[,c('region','state_name')]
cat_entidad<-cat_entidad %>% merge(temp,all.x = T,by.x = 'estado',by.y = 'state_name')

edomap<-edodat %>% 
  merge(cat_entidad %>% dplyr::select(ISO3,region) %>% unique,all.x = T) %>% 
  relocate(ISO3,region) %>% 
  merge(ind.coord %>% dplyr::select(code,Grupo),by.x = 'ISO3',by.y = 'code')

edo %>% filter(year==2015,!is.na(region)) %>% 
  #mutate(value=round(pob/1e6,1)) %>% 
  #mutate(value=round(pib/1e6,1)) %>% 
  #mutate(value=round(eco*100,1)) %>%
  #mutate(value=round(cre,1)) %>% 
  mutate(value=round(pibpc,1)) %>% 
  mxstate_choropleth()
rm(mxstate.map)


edomap<-ind.coord %>% 
  merge(cat_entidad %>% 
          dplyr::select(ISO3,region) %>% 
          unique,all.x = T,by.x = 'code',by.y = 'ISO3') %>%
  rename(ISO3=code) %>% 
  relocate(ISO3,region)

edomap %>% mutate(value=Grupo) %>% 
  mxstate_choropleth(legend = "Grupo") + scale_fill_npg()
ggsave('./TeX/Fig/45p_9_gtogrupos.pdf',height = 7,width = 12,units = 'cm')

edomap %>% filter(Grupo==5)

edodat %>% filter(!ISO3%in% c('MED','TOT')) %>% 
  #mutate(across(soc:gob,~.*gto2)) %>% 
  dplyr::select(ISO3,year,soc:gob) %>% 
  filter(year%in% c('2019','2020','2021')) %>% 
  group_by(ISO3) %>% 
  summarise(across(soc:gob,~100*(.-dplyr::lag(.)))) %>% 
  na.omit() %>% ungroup() %>% 
  mutate(year=rep(2020:2021,32)) %>% 
  pivot_longer(cols = soc:gob,names_to='funcion',values_to='delta19') %>% 
  ggplot(aes(x=funcion,y=delta19))+geom_boxplot()+
  theme_bw()+xlab(NULL)+ylab('Cambio en % del gasto total')+
  #coord_cartesian(ylim=c(-5,5))+
  facet_wrap(~year)
ggsave('./TeX/Fig/45p_PEF2021.pdf',width = 12,height = 7,units = 'cm')

temp <- edodat %>% filter(!ISO3%in% c('MED','TOT')) %>% 
  #mutate(across(soc:gob,~.*gto2)) %>% 
  dplyr::select(ISO3,year,soc:gob) %>% 
  filter(year%in% c('2019','2020','2021')) %>% 
  group_by(ISO3) %>% 
  summarise(across(soc:gob,~100*(.-dplyr::lag(.)))) %>% 
  na.omit() %>% ungroup() %>% 
  mutate(year=rep(2020:2021,32)) 

quantile(temp$sal[temp$year==2020],.25)
# No usado 
# sys GMM población| ingresos 

eqsgmm<-s_0a3 ~ lag(s_0a3) + gto2+
  soc+edu+eco+sal+resto |
  lag(s_0a3, 2:99) | # exógenas en ef. ind. e idios.
  lag(gto2,2)+lag(soc, 2)+lag(edu,2)+lag(eco,2)+lag(sal,2)+lag(resto,2)

# 4*5/2 #system
# 6 # normales


base <- pgmm(formula=eqsgmm,data=ging_dat,
             index=c("ISO3", "year"),model="twosteps",# subset = !ISO3%in%c("CAM","TAB"),
             effect="twoways",transformation = "ld")
base.ind<-pgmm(formula=eqsgmm,ging_dat,
               index=c("ISO3", "year"),model="twosteps",# subset = !ISO3%in%c("CAM","TAB"),
               effect="individual",transformation = "ld")

# coeftest(base,vcov. = vcovHC)
# sbase<-summary(base)
# sbasei<-summary(base.ind)


lmodel<-list(sbasei,sbase)
vsarg<-format(sapply(lmodel, function(x) x$sargan$p.value),digits = 2) 
ar1<-format(sapply(lmodel, function(x) x$m1$p.value),digits = 1)
ar2<-format(sapply(lmodel, function(x) x$m2$p.value),digits = 2)
waldmu<-c("",format(sbase$wald.td$p.value[[1]],digits = 1))#,format(sbasef$wald.td$p.value[[1]],digits = 1))

stargazer(base.ind,base,# type="text",
          digits = 2,
          title = "sys-GMM para para población de ingreso bajo",
          label = "tab:psGMM_edos",
          column.labels = c("Sin $\\mu$", "Con $\\mu$", "Con $\\mu$ ex.pet."),
          dep.var.caption = "Variable dependiente: Población|Ingreso < 3SM",
          single.row = T,
          dep.var.labels.include = F,
          # covariate.labels=c("$g_{t-1}$","gto","$\\log$(soc/gob)","$\\log$(edu/gob)",
          #                    "$\\log$(eco/gob)","$\\log$(sal/gob)",
          #                    "$\\log$(resto/gob)"),
          omit.stat = "n",
          add.lines = list(#c("Observaciones",rep("1,296",3)),
            c("Estados",c(32,32)),
            c("Instrumentos",16,16),
            c("Sargan p-value",vsarg %>% fsignif()),
            c("AR(1) p-value",ar1 %>% fsignif()),
            c("AR(2) p-value",ar2 %>% fsignif()),
            c("Wald $\\mu_t$ p-value",waldmu %>% fsignif())
          ))