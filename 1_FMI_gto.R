# FMI Datos de gasto por función versión 2.0
# fuente: https://data.imf.org/  Government Finance Statistics by Function of Government (COFOG)

# lee: GFSCOFOG_07-27-2020 23-41-17-45_timeSeries.csv
# crea: FMI_compos_v2.csv, FMI_compos_f_v2.csv, FMI_composc_v2.csv, FMI_compos_fc_v2.csv


suppressMessages(library(tidyverse))
suppressMessages(library(compositions))
suppressMessages(library(countrycode))
library(stringr) # función string length

##### Lectura y organización de datos #####
#Directorio
setwd("C:/Users/sgome/Dropbox/#tesis")

prueba<-read.csv(file = "./Data/FMI/COFOG/GFSCOFOG_07-27-2020 23-41-17-45_timeSeries.csv",nrows = 500,na.strings = "",
                   #header=TRUE,sep = ",",
                 encoding = "UTF-8",check.names = TRUE)
names(prueba)
sapply(prueba, class)

tablaOrig<-read.csv(file = "./Data/FMI/COFOG/GFSCOFOG_07-27-2020 23-41-17-45_timeSeries.csv",na.strings = "",
                    colClasses = c(rep("factor",9),rep("character",48),rep("NULL",3)),encoding = "UTF-8")
names(tablaOrig)
names(tablaOrig)[1]<-"Country.Name"

##### Preparación de datos #####

detfin<- tablaOrig %>% 
  select(Country.Code,COFOG.Function.Name,COFOG.Function.Code,Unit.Name,Attribute:X2019) %>% # variables a usar
  filter(Attribute=="Value" , Unit.Name=="Percent of GDP") #%>% #tomar sólo los valores en %


# categorías de funciones
funciones<-detfin %>% group_by(COFOG.Function.Code,COFOG.Function.Name) %>% summarise(n=n()) %>%
  mutate(nivel=case_when(
    str_length(COFOG.Function.Code)==2 ~ 1,
    str_length(COFOG.Function.Code)==4 ~ 2,
    TRUE ~ 4
  )) # extraer funciones y sus códigos
funciones<-funciones %>% mutate(nivelctm=nivel)
funciones$nivelctm[2:10]<-c(1,rep(2,8)) # crear categoría personalizada donde gob se detalla en sus subcomponentes

detfin<- merge(detfin,funciones%>% select(COFOG.Function.Code,nivel,nivelctm))

# Nombre Gasto reducido
nombfun<-detfin %>% filter(nivel==2|nivelctm==2)%>% group_by(COFOG.Function.Name) %>% summarise(n=n()) %>%
  arrange(COFOG.Function.Name)
medio<-c("investigación","defensa","economia","educacion","ambiente","gob1","faid","publico",
         "gobo","gobr","gobg","salud","vivienda","gobd","seguridad","cultura","social","gobt")
corto<-c("inv","def","eco","edu","amb","gob1","faid","pub","gobo","gobr","gobg","sal","viv",
         "gobd","seg","cul","soc","gobt")
nomb2<-data.frame(COFOG.Function.Name=nombfun$COFOG.Function.Name,medio=medio,corto=corto)

detfin<-merge(detfin,nomb2) # agregar nombres reducidos de funciones a la base
rm(prueba,funciones,nomb2,nombfun,tablaOrig,corto,medio)

# Base con las 10 categorías primarias del GFSM 
funlv2<-detfin %>%
  filter(nivel==2) %>% #usar 10 categorías de nivel2
  select(-c(COFOG.Function.Code,nivel,nivelctm)) %>% #retirar variables ahora innecesarias
  mutate(Country.Code=as.factor(countrycode(Country.Code,"imf","genc3c"))) %>% #reemplazar codigo país IMF por genc3c (ISO3 no incluye a kosovo)
  mutate(cont=as.factor(countrycode(Country.Code,"genc3c","continent"))) %>% #nueva variable: continente
  mutate(reg=as.factor(countrycode(Country.Code,"genc3c","region"))) %>% #nueva variable: region World Bank
  mutate(cont=ifelse(Country.Code=="XKS","Europe",as.character(cont)))%>% # agregar manualmente continete de Kosovo que no está incluido en la función
  mutate_at(vars(5:52), as.numeric) %>% #conversión a valores
  pivot_longer(cols = starts_with("X"), names_to="year", values_to="gto") %>% # columnas año en una sola variable
  select(-c(Unit.Name,Attribute)) %>% # retirar Unit Name y Value de la base
  mutate(year=as.numeric(substr(year,2,5))) %>%
  select(-c(COFOG.Function.Name,medio)) %>%
  pivot_wider(names_from = corto, values_from = gto) %>% #mover variable Función a columnas
  rename(code=Country.Code) %>%
  #na.omit() #retirar datos incompletos
  filter_at(vars(def:soc),any_vars(!is.na(.))) #%>% #al menos una observación

# examino negativos ####
temp<-funlv2%>% filter(amb<0|viv<0) %>%
  mutate(pais=countrycode(code,"genc3c","country.name"))
listaneg<-temp%>% group_by(code,pais) %>% summarise(n=n()) # lista de países con negativos
lista<-c("EST","ISR")
temp<-funlv2 %>% filter(code %in% listaneg$code)
plot(temp[temp$code=="EST",]$amb)
plot(temp[temp$code=="ISR",]$viv)


# retiro negativos y estimo los valores
# reemplazar negativos con la media condicional
for (i in 1:2){
  dato<-funlv2 %>% filter(code==lista[i]) %>%
    unite(col=id,code,year,sep = "'") %>% 
    column_to_rownames(.,var="id") %>%
    select(def:soc)
    rellenado=rellena_compv2(dato)
  funlv2 [funlv2$code==lista[i],5:14] <- rellenado
}
plot(funlv2[funlv2$code=="EST",]$amb)
plot(funlv2[funlv2$code=="ISR",]$viv)


# examinar ceros y NAs ####
temp<- funlv2 %>% filter_at(vars(def:soc),any_vars(.==0|is.na(.))) %>%
  group_by(code) %>% summarise(N=n())
lista<-as.character(temp$code)
temp<-funlv2 %>% filter(code %in% lista) %>%
  mutate(pais=countrycode(code,"genc3c","country.name")) %>%
  select(-c(cont,reg)) %>%
  mutate(nNA=apply(is.na(.),1,sum),ncero=apply(.==0, 1, sum,na.rm=TRUE)) %>% #conteo de registros afectados
  mutate(ntod=nNA + ncero) %>% # total de partes afectadas
  group_by(code) %>%
  summarise(N=n(),comp=sum(nNA==0&ncero==0),totNA=sum(nNA),totcero=sum(ncero),totamb=sum(ntod))
#lista a completar
lista2<-as.character(temp$code[temp$comp>2])
i<-lista2[9]

funlv2est<-funlv2
for (i in lista2) {
  dato<-funlv2 %>% filter(code==i) %>%
    unite(col=id,code,year,sep = "'") %>% 
    column_to_rownames(.,var="id") %>%
    select(def:soc)
  #print(i)
  if(i=="ZAF"){
    rellenado<-rellena_media(dato)
  }else{
    if(i=="MNG"){
      rellenado1<-(rellena_compv2(dato%>%select(-amb))+rellena_media(dato%>%select(-amb)))/2 # estima seg ignorando amb
      rellenado2<-(rellena_compv2(dato)+rellena_media(dato))/2
      rellenado<-rellenado2
      rellenado$seg<-rellenado1$seg
    }else{
      if(i=="SGP"){
        rellenado1<-rellena_compv2(dato[c(6:23,26:29),])
        dato[c(6:23,26:29),]<-rellenado1
        rellenado<-rellena_compv2(dato)
      }else{
        rellenado<-rellena_compv2(dato)  
      }
    }
  }
  funlv2est [funlv2$code==i,5:14]  <- rellenado
}

# comparación de estimados con serie original
i<-lista2[14]
dato<-funlv2 %>% filter(code==i)
orig<-dato[,apply(dato==0|is.na(dato),2,sum,na.rm=TRUE)>0]
variab<-names(orig)
estim<-funlv2est %>% filter(code==i) %>% select(all_of(variab))
dim(estim)[2]
d<-9
limite<-max(orig[,d],estim[,d],na.rm = TRUE)
plot(x=dato$year,y=t(orig[,d]),ylim = c(0,limite),main = i,xlab = "year",ylab = names(estim)[d])
lines(x=dato$year,y=t(estim[,d]))

# Resumen de NAs y ceros restantes ####
temp<-funlv2est %>% filter_at(vars(def:soc),any_vars(.==0|is.na(.))) %>%
  group_by(code) %>% summarise(N=n())
lista<-as.character(temp$code)
temp<-funlv2 %>% filter(code %in% lista) %>%
  mutate(pais=countrycode(code,"genc3c","country.name")) %>%
  select(-c(cont,reg)) %>%
  mutate(nNA=apply(is.na(.),1,sum),ncero=apply(.==0, 1, sum,na.rm=TRUE)) %>% #conteo de registros afectados
  mutate(ntod=nNA + ncero) %>% # total de partes afectadas
  group_by(code) %>%
  summarise(N=n(),comp=sum(nNA==0&ncero==0),totNA=sum(nNA),totcero=sum(ncero),totamb=sum(ntod))

temp<-funlv2 %>% filter(code %in% lista)
temp[temp==0]<-NA

# guardar archivos ####

write.csv(funlv2est,file = "./Data/FMI/COFOG/funlv2est.csv",row.names = FALSE)
funlv2est<-read.csv(file = "./Data/FMI/COFOG/funlv2est.csv",header = TRUE)

# Resumen partes afectadas
temp<-funlv2est%>% filter_at(vars(def:soc),any_vars(is.na(.)|.==0))%>%
  group_by(code)%>% summarise(n=n())
lista<-as.character(temp$code)
 
res_partes<-funlv2est %>%
  pivot_longer(cols=def:soc,names_to = "funcion",values_to="gto_pib")%>%
  mutate(nNA=is.na(gto_pib),ncero=ifelse(nNA,FALSE,gto_pib==0)) #%>%
# resumen por parte
res_partes %>%  group_by(funcion)%>% summarise(N=n(),totNA=sum(nNA),totcero=sum(ncero)) %>%
  mutate(perNA=totNA/N*100,percero=totcero/N*100)

# resumen por año
resanio<-res_partes %>%  group_by(year)%>% summarise(N=n(),totNA=sum(nNA),totcero=sum(ncero)) %>%
  mutate(perNA=totNA/N*100,percero=totcero/N*100)
rm(temp)

funlv2<-funlv2est
rm(funlv2est)

# Guardar archivos sin filtrar ####
write.csv(funlv2,"./Data/FMI/COFOG/FMI_compos_v2.csv",row.names = FALSE)
write.csv(funcustom,"./Data/FMI/COFOG/FMI_composc_v2.csv",row.names = FALSE)



# Tratamiento de ceros, quitar ceros ####
funlv2 %>% filter_all(any_vars(is.na(.)))#NA check
# buscar ceros y negativos
busq<-matrix(0,3,10)
rownames(busq)<-c("ceros","negativos","NAs")
colnames(busq)<-names(funlv2)[5:14]
for (i in 5:14) {
  busq[1,i-4]<-sum(funlv2[,i]==0,na.rm = TRUE)/dim(funlv2)[1]*100
  busq[2,i-4]<-sum(funlv2[,i]<0,na.rm = TRUE)/dim(funlv2)[1]*100
  busq[3,i-4]<-sum(is.na(funlv2[,i]))/dim(funlv2)[1]*100
};busq

rm(busq)
# resumen faltantes viv
lista<-funlv2 %>% select(code,viv) %>% filter(viv==0|is.na(viv))%>%group_by(code) %>% summarise(n=n()) %>% select(code)
lista<-as.character(lista$code) ; lista
funlv2 %>% select(code,viv) %>% filter(code %in% lista) %>% 
  mutate(conteo=1,faltante=viv==0|is.na(viv)) %>% group_by(code) %>% summarise(tot=sum(conteo),nfal=sum(faltante))
# eliminar países con NAs en viv
funlv2 <- funlv2 %>% filter(!(code %in% lista))

# resumen faltantes cul
lista<-funlv2 %>% select(code,cul) %>% filter(cul==0|is.na(cul))%>%group_by(code) %>% summarise(n=n()) %>% select(code)
lista<-as.character(lista$code) ; lista
funlv2 %>% select(code,cul) %>% filter(code %in% lista) %>% 
  mutate(conteo=1,faltante=cul==0|is.na(cul)) %>% group_by(code) %>% summarise(tot=sum(conteo),nfal=sum(faltante))
# eliminar países con NAs en cul



# resumen faltantes de defensa
lista<-funlv2 %>% select(code,def) %>% filter(def==0|is.na(def))%>%group_by(code) %>% summarise(n=n()) %>% select(code)
lista<-as.character(lista$code);lista
funlv2 %>% select(code,def) %>% filter(code %in% lista) %>% 
  mutate(conteo=1,faltante=def==0|is.na(def)) %>% group_by(code) %>% summarise(tot=sum(conteo),nfal=sum(faltante))
# HKG y MAC no pueden tener gasto de defensa se podrían promediar con CHN (años donde los 3 tengan obs) y eliminar los demás
lista<-lista[c(1,3,4,6,7)];lista
funlv2<-funlv2 %>%filter(!code%in% lista)

# resumen faltantes ambiental
lista<-funlv2 %>% select(code,amb) %>% filter(amb==0|is.na(amb))%>%group_by(code) %>% summarise(n=n()) %>% select(code)
lista<-as.character(lista$code) ; lista
funlv2 %>% select(code,amb) %>% filter(code %in% lista) %>% 
  mutate(conteo=1,faltante=amb==0|is.na(amb)) %>% group_by(code) %>% summarise(tot=sum(conteo),nfal=sum(faltante))
# USA tiene presupuesto de gasto ambiental (~0.20% del gdp) se podría rellenar con datos externos o ignorar el país
lista<-lista[1];lista
funlv2<-funlv2 %>% filter(!code%in%lista)


# Posiblemente trabajaremos sin considerar def y amb, entonces se pueden eliminar los NAs de los demás
# Para análisis que incluya def sumar HKG y MAC con CHN, eliminar otros países con NAs
# Para analisis que incluya amb conseguir serie de USA, eliminar otros países con NAs
funlv2 %>% filter_all(any_vars(is.na(.)))#NA check

# Guardar archivos filtrados ####
fun2fil<-funlv2
write.csv(fun2fil,"./Data/FMI/COFOG/FMI_compos_f_v2.csv",row.names = FALSE)
funcfil<-funcustom %>%na.omit()
write.csv(funcfil,"./Data/FMI/COFOG/FMI_compos_fc_v2.csv",row.names = FALSE)

funlv2<-read.csv(file = "./Data/FMI/COFOG/FMI_compos_f_v2.csv",header = TRUE)
funlv2 %>% filter_all(any_vars(is.na(.)))#NA check

### Tabla resumen de # de observaciones por año para cada continente ####
# funlv2<-read.csv(file = "./Data/FMI/COFOG/FMI_compos_v2.csv",header = TRUE)
# prefun<-read.csv(file = "./Data/FMI/FMI_compos.csv",header = TRUE) # datos anteriores


# Agregar clasificación WB
wbclasif<-read.csv(file = "./Data/wbclasif.csv",header = TRUE) #clasificación WB de países
wbclasif<-wbclasif%>%group_by(code,year,Nivel)%>% summarise(n=n())%>%
  mutate(code=ifelse(code=="XKX","XKS",code))
# funlv2<-funlv2%>% mutate(cont=countrycode(code,"genc3c","continent"))#%>%
#   mutate(cont=ifelse(code=="XKS","Europe",cont)) # agregar manualmente continete de Kosovo que no está incluido en la función

funlv2clas<-merge(x=funlv2,y=wbclasif%>% select(code,year,Nivel),all.x = TRUE) %>%
  mutate(Nivel=ifelse(code=="USA"&is.na(Nivel),"Alto",Nivel)) # corrección de faltantes en la tabla de clasificación
funlv2clas %>% filter_all(any_vars(is.na(.)))#NA check
  
ny<-funlv2 %>% group_by(year,cont) %>%
  summarise(n=n_distinct(code)) %>%
  pivot_wider(names_from = cont,values_from=n) %>%
  replace_na(list(Americas=0,Europe=0,Asia=0,Africa=0,Oceania=0))%>%
  ungroup() %>%
  mutate(glob=select(.,Americas:Oceania)%>%rowSums())

# ggplot(data=ny, aes(x=year,y=glob,color=cont))+
#   geom_line()
ny<-funlv2 %>% group_by(year,cont) %>%
  summarise(n=n_distinct(code))
ggplot(data=ny, aes(x=year,y=n,color=cont))+
  geom_line(size=1)

## Tabla resumen de # de observaciones por año para cada nivel de ingreso ####

# Tabla resumen de # años por país
ny2<-funlv2 %>% group_by(code,cont) %>%
  summarise(n=n_distinct(year),ini=min(year),fin=max(year),span=fin-ini+1,hueco=span-n) #%>%
  #pivot_wider(names_from = cont,values_from=n) %>%
  #replace_na(list(Americas=0,Europe=0,Asia=0,Africa=0,Oceania=0))%>%
  #ungroup() %>%
  #mutate(glob=select(.,Asia:Africa)%>%rowSums())
library(ggplot2)
library(ggsci) # paletas de colores de journals

ggplot(data = ny2,aes(n,fill=cont))+
  geom_histogram(binwidth = 2)+
  theme_bw()+
  scale_color_locuszoom()

sum(ny2$n)

#interpolación de intra-años faltantes ####
#revisar continuidad de series ymax-ymin vs #obs
resanio<-funlv2clas %>% group_by(code) %>% summarise(ti=n_distinct(year),tmax=max(year),tmin=min(year),maxobs=tmax-tmin+1,huecos=maxobs-ti)
funlv2clas %>% group_by(Nivel,code) %>% 
  summarise(ti=n_distinct(year),tmax=max(year),tmin=min(year),maxobs=tmax-tmin+1,huecos=maxobs-ti)%>%
  summarise(sti=sum(ti),ttmax=max(tmax),ttmin=min(tmin),shuecos=sum(huecos))
temp<-resanio %>% filter(huecos>0)%>% select(code) #filtro de países con huecos
lista<-as.character(temp$code);lista # lista de países con huecos
#regresar huecos en serie como NAs
temp<-funlv2clas %>% select(-Nivel) %>% pivot_longer(cols = def:soc,names_to="fun",values_to="gto")%>%
  pivot_wider(names_from = "year",values_from="gto")%>% # esto crea NAs en años vacíos
  pivot_longer(cols=!c(code,cont,reg,fun),names_to = "year",values_to="gto") %>% #regreso año a una sola variable
  pivot_wider(names_from = "fun",values_from="gto") %>% # regresa gasto por función a columnas
  arrange(code,year) %>%
  merge(resanio%>%select(code,tmax,tmin)) %>% #agregar información previa del máximo tamaño de la serie
  mutate(year=as.numeric(year)) %>%#convertir año a números para comparación
  filter(year<=tmax,year>=tmin)

# rellenar con interpolación  
funrell<-temp #inicializar base con huecos interpolados

for (i in lista) {
  temp<-funrell %>% filter(code==i)
  serie<-zoo(temp%>%select(def:soc),temp$year)
  serie<-na.approx(serie)
  funrell[funrell$code==i,5:14]<-serie
}

funrell %>% filter_all(any_vars(is.na(.)))#NA check
resaniocheck<-funrell %>% group_by(code) %>% summarise(ti=n_distinct(year),tmax=max(year),tmin=min(year),maxobs=tmax-tmin+1,huecos=maxobs-ti)

paisi<-funrell%>% filter(code=="ZAF")
serie<-zoo(paisi%>%select(def:soc),paisi$year)
serie<-na.approx(serie); serie
plot(time(serie),serie$edu,type = "b")

write.csv(funrell,file = "./Data/FMI/COFOG/FMI_compos_fr_v2.csv",row.names = FALSE)


