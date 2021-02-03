# Gasto por función de México

#fuente A: Centro de estudios de las finanzas públicas (CEFP)  https://www.cefp.gob.mx/Pub_Gasto_Estadisticas.htm
#fuente B: SHCP: http://www.shcp.gob.mx/POLITICAFINANCIERA/FINANZASPUBLICAS/Estadisticas_Oportunas_Finanzas_Publicas/Paginas/unica2.aspx
#fuente C: Transparencia presupuestaria https://www.transparenciapresupuestaria.gob.mx/es/PTP/Datos_Abiertos

##### Inicialización de Paquetes #####

library(readxl)
library(tidyverse)
library(compositions)
library(ggplot2)
library(xtable)


##### Lectura y organización de datos #####
#Directorio
setwd("C:/Users/sgome/Dropbox/#tesis")

mex90_02<-read_excel(path = "./Data/CEFP/cfb1.xls",sheet = 1,skip=3,n_max = 22,na="-")
mex03_11<-read_excel(path = "./Data/CEFP/cfc1.xls",sheet = 1,skip=3,n_max = 30,na="-")
neto80<-read_excel(path = "./Data/CEFP/ca1.xls",sheet = 1,skip=3,n_max = 53)
neto07_19<-read_excel(path = "./Data/SHCP/gastoneto.xls",sheet = 1,skip = 2,n_max = 26,na="n.d.")
names(neto07_19)[1]<-"Concepto"
mex07_19<-read_excel(path = "./Data/SHCP/gastoprog.xls",sheet = 1,skip = 2,n_max = 38,na="n.d.")
names(mex07_19)[1]<-"Concepto"

# Consolidación Gasto Neto
neto07_19$Concepto
unique(neto07_19$Concepto)
parteA<-neto80 %>% pivot_longer(cols = 2:34,names_to="year",values_to="gto") %>% 
  pivot_wider(names_from = "Ramos",values_from="gto") %>% 
  select(c(1,45,54,48)) %>%  # seleccionar año, gasto programable, gasto neto y cto financiero
  filter(year!="2012 A") %>% 
  mutate(year=as.numeric(year))
parteB<-neto07_19[c(1:10,20),] %>% pivot_longer(cols = 2:31,names_to="year",values_to="gto") %>% 
  pivot_wider(names_from = "Concepto" ,values_from="gto") %>% 
  select(c(1:3,12)) %>%  # seleccionar año, gasto programable, gasto neto y cto financiero
  mutate(year=as.numeric(year))
neto<-merge(parteA,parteB,all = TRUE)
names(neto)<-c("year","gtoprog_CEFP","gtoneto_CEFP","gtofin_CEFP","gtoneto_SHCP","gtoprog_SHCP","gtofin_SHCP")
neto<-neto %>% pivot_longer(cols = starts_with("gto"),names_to="serie",values_to="gto") %>% 
  separate(serie,into = c("concepto","fuente")) %>% 
  pivot_wider(names_from = "concepto",values_from="gto")

ggplot(neto,aes(x=year,y=gtoprog,color=fuente))+geom_point()+
  scale_y_log10()
ggplot(neto,aes(x=year,y=gtoneto,color=fuente))+geom_point()+
  scale_y_log10()
ggplot(neto,aes(x=year,y=gtofin,color=fuente))+geom_point()+
  scale_y_log10()
# son muy similares las series, tomar fuente SHCP (más reciente)
neto<-neto %>% filter(fuente=="CEFP"&year<1990|fuente=="SHCP"&year>=1990)
neto<-neto %>% mutate(noprog=gtoneto-gtoprog)

#Catálogos equivalencias a COFOG FMI ####

cat90<-data.frame(funmex=unique(mex90_02$Concepto),
                  funFMI=c(NA,NA,"edu","sal","soc","eco","soc","viv",NA,"eco","eco","eco","eco",
                           NA,"gob","seg","gob","seg","def","gob","amb","seg"))
cat03<-data.frame(funmex=unique(mex03_11$Concepto),
                  funFMI=c(NA,NA,"edu","sal","soc","viv","viv","soc",NA,"eco","eco","eco","eco","eco","eco","eco",
                           "gob","eco","eco",NA,"gob","gob","def","gob","gob","seg","amb","gob","gob","gob"))
cat07<-data.frame(funmex=unique(mex07_19$Concepto),
                  funFMI=c(NA,NA,"gob","seg","gob","gob","gob","def","seg","gob",NA,"amb","viv","sal","cul","edu",
                           "soc","soc",NA,rep("eco",7),"gob","eco",NA,"eco","gob","gob","soc","gob","gob","eco",
                           "eco","eco"))
# Tabla en el documento: "Comparación entre clasificaciones funcionales del gasto público"
print(xtable(cat07[1:28,] ,digits=1),include.rownames=FALSE)

# Traduccion por catálogo a funciones FMI ####
parteA<-mex90_02 %>% 
  merge(x=.,y=cat90,by.x="Concepto",by.y="funmex")%>% #inclusión catálogo
  filter(!is.na(funFMI)) %>% #eliminación de totales y subtotales
  pivot_longer(cols = 2:14,names_to="year",values_to="gto") %>% #año a columna
  group_by(funFMI,year)%>%
  summarise(gto=sum(gto,na.rm = TRUE))%>%
  pivot_wider(names_from = "funFMI",values_from="gto")%>% 
  mutate(cul=NA)

# Comparación de totales
plot(x=1990:2002,y=mex90_02[1,2:14])
lines(x=1990:2002,y=rowSums(parteA[,2:10])) # se respeta el total
# Gráfica del gasto neto vs total gasto programable
plot(x=1990:2002,y=log(mex90_02[1,2:14]),type="l",col="blue",lwd=2,ylim = c(11.5,14.5))
lines(x=1990:2002,y=log(neto80[53,12:24]),lwd=2,col="gray")


parteB<-mex03_11 %>% 
  merge(x=.,y=cat03,by.x="Concepto",by.y="funmex")%>% #inclusión catálogo
  filter(!is.na(funFMI)) %>% #eliminación de totales y subtotales
  pivot_longer(cols = 2:10,names_to="year",values_to="gto") %>% #año a columna
  group_by(funFMI,year)%>%
  summarise(gto=sum(gto,na.rm = TRUE))%>%
  pivot_wider(names_from = "funFMI",values_from="gto") %>% 
  mutate(cul=NA)

plot(x=2003:2011,y=mex03_11[1,2:10])
lines(x=2003:2011,y=rowSums(parteB[,2:10])) #chequeo de totales

parteC<-mex07_19 %>% 
  merge(x=.,y=cat07,by.x="Concepto",by.y="funmex")%>% #inclusión catálogo
  filter(!is.na(funFMI)) %>% #eliminación de totales y subtotales
  pivot_longer(cols = 2:31,names_to="year",values_to="gto") %>% #año a columna
  group_by(funFMI,year)%>%
  summarise(gto=sum(gto,na.rm = TRUE))%>%
  pivot_wider(names_from = "funFMI",values_from="gto") %>% 
  filter_at(vars(amb:viv),any_vars(.>0))

plot(x=1990:2019,y=mex07_19[1,2:31])
lines(x=2007:2019,y=rowSums(parteC[,2:11]))# chequeo de totales



# Consolidación de partes A-C####
mex<-rbind(parteA,parteB%>% filter(year<=2006),parteC) %>% 
  mutate(year=as.numeric(year),code="MEX")

# Agregar gasto no prog. como parte del gasto gob.

plot(x=mex$year,y=neto$noprog[dplyr::between(neto$year,1990,2019)],type = "b",col="red")
lines(x=mex$year,y=mex$gob,type = "b",col="blue")

neto$gob_exd<-c(rep(NA,10),mex$gob) #gasto en gobierno sin deuda y transf
neto$part<-neto$noprog-neto$gtofin #participaciones y adefas
neto$gob<-neto$noprog+neto$gob_exd #gobierno total
neto<-neto %>% mutate(across(gtofin:part,~./gob*100,.names = "p{col}"))
neto %>% select(year,gtofin:part) %>% #para gráfica en dinero
  pivot_longer(!year,names_to="serie",values_to="porcentaje") %>% 
  ggplot(aes(x=year,y=porcentaje,col=serie))+geom_line(size=1)
medias<-sapply(neto[,10:13],mean,na.rm=T)
neto %>% select(year,pgtofin:ppart) %>% #para gráfica en porcentaje
  pivot_longer(!year,names_to="serie",values_to="porcentaje") %>% 
  ggplot(aes(x=year,y=porcentaje,col=serie))+geom_line(size=1)+
  annotate("text",x=1990,y=medias,label=round(medias,1))
plot(x=mex$year,y=neto$noprog[dplyr::between(neto$year,1990,2019)],type = "b",col="red")

mex$gob<-mex$gob+neto$noprog[dplyr::between(neto$year,1990,2019)]


# Revisión de NAs y ceros ####
ggplot(mex,aes(x=year,y=gob))+geom_point()

# Rellenado ####
temp<-mex #inicializo rellenado
temp[between(mex$year,2003,2006),2]=NA #descartar valores de gto ambiental atípicos
temp<-temp %>% unite(col = "id",code,year,sep="'") %>% column_to_rownames(var="id") # poner en formato base FMI
temp<-rellena_compv2(temp) # función rellenar con media condicional
temp<-temp %>% rownames_to_column(var = "id") %>% 
  separate(col="id",into = c("code","year")) %>%  # regresar variables code, year
  mutate(year=as.numeric(year))


mexrell<-mex
mexrell[between(mexrell$year,2003,2006),]$amb<-temp[between(temp$year,2003,2006),]$amb
mexrell[between(mexrell$year,1990,2006),]$cul<-temp[between(temp$year,1990,2006),]$cul

ggplot(mex,aes(x=year,y=amb))+geom_point()+
  geom_line(data = mexrell,aes(x=year,y=amb))
ggplot(mex,aes(x=year,y=cul))+geom_point()+
  geom_line(data = mexrell,aes(x=year,y=cul))

# Guardar archivo rellenado ####
write.csv(mexrell,file = "./Data/SHCP/mex_compos_r.csv",row.names = FALSE)



# leer datos de PIB ####
pib<-data.frame(t(read_excel(path = "./Data/INEGI/PIBT_5.xlsx",sheet = 1,skip = 4,n_max = 2))[2:197,])
names(pib)<-c("tiempo","pib"); rownames(pib)<-c()
pib<-pib %>% mutate(year=rep(1993:2020,times=1,each=7),pib=as.numeric(pib)) %>% 
  filter(tiempo=="Anual") %>% na.omit()

wbdata<-read.csv(file="./Data/World Bank/WDIData.csv", encoding = "UTF-8")
pibwb<- wbdata %>% filter(Indicator.Name=="GDP (current LCU)") %>% 
  select(Country.Code,c(5:64)) %>%
  pivot_longer(cols = starts_with("X"), names_to = "year",values_to = "pib") %>%
  mutate(year=as.numeric(substr(year,2,5)),pib = pib /1E6) %>%
  rename(code=Country.Code)%>%
  filter(code=="MEX")

plot(pibwb$year,pibwb$pib,type = "l",col="blue")
lines(pib$year,pib$pib,type = "l",col="red")
  
# completar base historica con datos del wb
pib<-rbind(pib %>% select(year,pib),pibwb %>% select(year,pib) %>% filter(year<1993)) %>% arrange(year)
seriepib<-pib$pib[pib$year>=1990]
# Convertir a porcentaje de PIB ####
mexper<-mexrell %>% mutate_at(vars(amb:cul),~./seriepib*100)

write.csv(mexper,file = "./Data/SHCP/mex_compos_rp.csv",row.names = FALSE)

