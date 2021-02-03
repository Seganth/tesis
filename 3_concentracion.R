##### Organización de datos y consolidación de base#####

library(tidyverse)
library(countrycode) # funcion con códigos de países
library(ggrepel) #para text repel
library(readxl) #lee excel
library(janitor) #para adorn_totals
library(xtable) #para imprimir tablas formato LaTeX

# Agregar México a datos FMI ####
#Directorio
setwd("C:/Users/sgome/Dropbox/#tesis")
fmi<-read.csv(file = "./Data/FMI/COFOG/FMI_compos_fr_v2.csv", header = TRUE) %>% select(-c(reg,tmin,tmax)) %>% 
  rename(gob=pub)
mex<-read.csv(file = "./Data/SHCP/mex_compos_rp.csv",header = TRUE)
mex<-mex %>% mutate(cont=countrycode(code,"genc3c","continent"))

fmi<-rbind(fmi,mex)
rm(mex)


# Leer variables adicionales ####
## GDP, Miembros FMI, Gasto y COFOG #GDP growth #Debt #Revenues #Interest paid on debt #GDP per capita #WB clasif

# variables del WB ####
wbdata<-read.csv(file="./Data/World Bank/WDIData.csv", encoding = "UTF-8")
lista_variab<-c("Population, total","Population density (people per sq. km of land area)","Total natural resources rents (% of GDP)",
                "GDP, PPP (constant 2011 international $)","GDP per capita, PPP (constant 2011 international $)",
                "GDP growth (annual %)","Expense (% of GDP)","Interest payments (% of expense)","Revenue, excluding grants (% of GDP)",
                "Tax revenue (% of GDP)","Central government debt, total (% of GDP)",
                "New business density (new registrations per 1,000 people ages 15-64)","Real interest rate (%)")

selecWB<-wbdata %>% filter(Indicator.Name%in% lista_variab) %>% 
  select(Country.Code,Indicator.Name,c(5:64)) %>%
  pivot_longer(cols = starts_with("X"), names_to = "year",values_to = "value") %>%
  mutate(year=as.numeric(substr(year,2,5))) %>%
  rename(code=Country.Code) %>% 
  pivot_wider(names_from = Indicator.Name,values_from="value") %>% 
  filter_at(vars(3:15),any_vars(!is.na(.))) #%>% #al menos una observación
income_share<-wbdata %>% filter(str_detect(Indicator.Name,"Income share")) %>% 
  select(Country.Code,Indicator.Name,c(5:64)) %>%
  pivot_longer(cols = starts_with("X"), names_to = "year",values_to = "value") %>%
  mutate(year=as.numeric(substr(year,2,5))) %>%
  rename(code=Country.Code) %>% 
  pivot_wider(names_from = Indicator.Name,values_from="value") %>% na.omit()
  #filter_at(vars(3:9),any_vars(!is.na(.))) #%>% #al menos una observación


# income_share%>% filter_all(any_vars(is.na(.)))#NA check
selecWB<-merge(selecWB,income_share,all=TRUE)
write.csv(selecWB,file = "./Data/World Bank/selecWB.csv",row.names = FALSE)
rm(wbdata,income_share)

# Actualización de variables del WB
wbdatax<-read_excel(path = "./Data/World Bank/WDIEXCEL.xlsx",sheet = "Data")
lista_variabx<-read.csv(file = "./Data/World Bank/milista.csv")


selecWB<-wbdatax %>% 
  rename(Indicator.Name=`Indicator Name`,Country.Code=`Country Code`) %>% 
  filter(Indicator.Name%in% lista_variabx$Indicator.Name) %>%
  merge(lista_variabx %>% select(Indicator.Name,ShortName)) %>% 
  select(Country.Code,ShortName,c(5:64)) %>%
  pivot_longer(cols = 3:62, names_to = "year",values_to = "value") %>%
  mutate(year=as.numeric(year)) %>%
  rename(code=Country.Code) %>% 
  pivot_wider(names_from = ShortName,values_from="value") %>% 
  filter_at(vars(3:15),any_vars(!is.na(.))) #%>% #al menos una observación

write.csv(selecWB,file = "./Data/World Bank/selecWB2.csv",row.names = FALSE)
names(selecWB)


# Variables del FMI ####
#Gasto
gto=read_excel(path = "./Data/FMI/Expenditure.xls",na="no data")
gto = gto %>%
  rename(pais=`Expenditure (% of GDP)`) %>% #homologar nombre de variable
  filter(!grepl('Euro|Advanced|Emerging|Developing',pais))%>%
  mutate(code=countrycode(pais,"country.name","genc3c")) %>% #incluir columna de codigos ISO
  pivot_longer(cols = 2:36,names_to = "year", values_to = "gtof") %>% #convertir a tabla larga
  select(-pais) #%>% 
  #na.omit() # retirar renglones sin dato
gto %>% filter(year==2017) %>% summarise(n=n_distinct(code)) # chequeo número de paises
gdp=read_excel(path = "./Data/FMI/GDP.xls",na="no data") 
gdp = gdp %>% rename(pais=`GDP, current prices (Billions of U.S. dollars)`) %>% #homologar nombre de variable
  filter(!grepl('Euro|Advanced|Emerging|Developing',pais))%>%
  mutate(code=countrycode(pais,"country.name","genc3c")) %>%  #incluir columna de codigos ISO
  pivot_longer(cols = 2:46, names_to = "year",values_to = "pibf") %>% #convertir a tabla larga
  select(-pais) %>% 
  na.omit() # retirar renglones sin dato
#GDP growth
growth<-read_excel(path = "./Data/FMI/RealGDPgrowth.xls",na="no data") 
growth = growth %>%
  rename(pais=`Real GDP growth (Annual percent change)`) %>% #homologar nombre de variable
  filter(!grepl('Euro|Advanced|Emerging|Developing',pais))%>%
  mutate(code=countrycode(pais,"country.name","genc3c")) %>% #incluir columna de codigos ISO
  pivot_longer(cols = 2:46,names_to = "year", values_to = "cref") %>% #convertir a tabla larga
  na.omit() %>%# retirar renglones sin dato
  select(-pais)
growth %>% filter(year==2017) %>% summarise(n=n_distinct(code)) # chequeo número de paises
#Debt
deuda= read_excel(path = "./Data/FMI/GeneralGovGrossDebt.xls",na="no data")
deuda = deuda %>%
  rename(pais=`General government gross debt (Percent of GDP)`) %>% #homologar nombre de variable
  filter(!grepl('Euro|Advanced|Emerging|Developing',pais))%>%
  mutate(code=countrycode(pais,"country.name","genc3c")) %>% #incluir columna de codigos ISO
  pivot_longer(cols = 2:46,names_to = "year", values_to = "deuf") %>% #convertir a tabla larga
  na.omit() %>%# retirar renglones sin dato
  select(-pais)
deuda %>% filter(year==2017) %>% summarise(n=n_distinct(code)) # chequeo número de paises
#Revenues
ingr= read_excel(path = "./Data/FMI/Revenue.xls",na="no data")
ingr = ingr %>%
  rename(pais=`Revenue (% of GDP)`) %>% #homologar nombre de variable
  filter(!grepl('Euro|Advanced|Emerging|Developing',pais))%>%
  mutate(code=countrycode(pais,"country.name","genc3c")) %>% #incluir columna de codigos ISO
  pivot_longer(cols = 2:36,names_to = "year", values_to = "ingf") %>% #convertir a tabla larga
  na.omit() %>%# retirar renglones sin dato
  select(-pais)
ingr %>% filter(year==2017) %>% summarise(n=n_distinct(code)) # chequeo número de paises
#Interest paid on debt
int= read_excel(path = "./Data/FMI/InterestPublicDebt.xls",na="no data")
int = int %>%
  rename(pais=`Interest paid on public debt, percent of GDP (% of GDP)`) %>% #homologar nombre de variable
  filter(!grepl('Euro|Advanced|Emerging|Developing',pais))%>%
  mutate(code=countrycode(pais,"country.name","genc3c")) %>% #incluir columna de codigos ISO
  pivot_longer(cols = 2:213,names_to = "year", values_to = "intf") %>% #convertir a tabla larga
  na.omit() %>%# retirar renglones sin dato
  select(-pais)
int %>% filter(year==2011) %>% summarise(n=n_distinct(code)) # chequeo número de paises

extraFMI<-merge(gto,growth,all = TRUE) %>% 
  merge(gdp,all = TRUE)%>%
  merge(deuda,all = TRUE) %>%
  merge(ingr,all = TRUE) %>%
  merge(int,all = TRUE)
write.csv(extraFMI,file = "./Data/FMI/extraFMI2.csv",row.names = FALSE)
rm(gto,gdp,growth,deuda,ingr,int)

# WB clasificación ingreso
wbclasif<-read.csv(file = "./Data/wbclasif.csv",header = TRUE) #clasificación WB de países
wbclasif<-wbclasif%>%select(code,year,Nivel)%>% 
  mutate(code=ifelse(code=="XKX","XKS",code))

# Combinar todo ####
selecWB<-read.csv(file = "./Data/World Bank/selecWB2.csv",header = TRUE)
extraFMI<-read.csv(file = "./Data/FMI/extraFMI2.csv",header = TRUE)

consolidado<-fmi %>% 
  merge(y=extraFMI,all.x= TRUE) %>%
  merge(y=wbclasif %>% select(code,year,Nivel),all.x = TRUE) %>% 
  merge(y=selecWB,all.x = TRUE) %>% 
  mutate(Nivel=ifelse(code=="USA"&is.na(Nivel),"Alto",Nivel)) # corrección de faltantes en la tabla de clasificación
consolidado %>% filter(is.na(Nivel))
rm(extraFMI,selecWB)


#sin filtrar a base GFS
consolidado<-fmi %>% 
  merge(y=extraFMI %>% filter(between(year,1970,2018)),all= TRUE) %>%
  merge(y=wbclasif %>% select(code,year,Nivel),all = TRUE) %>% 
  merge(y=selecWB,all = TRUE) %>% 
  mutate(Nivel=ifelse(code=="USA"&is.na(Nivel),"Alto",Nivel)) %>% # corrección de faltantes en la tabla de clasificación
  mutate(pais=countrycode(code,"genc3c","country.name")) %>% 
  filter(!is.na(pais)) %>% 
  select(code,year,pais,Nivel,everything()) %>% 
  mutate(gtofuns=select(.,def:soc) %>% rowSums(),
         gto2=ifelse(is.na(gtof),gtofuns,gtof)) %>% 
  group_by(code) %>% 
  fill(c(Nivel,cont),.direction ="updown")
  

rm(extraFMI,selecWB,fmi,wbclasif)

write.csv(consolidado,file = "./Data/consolidado2b.csv",row.names = FALSE)

# comparar suma gastos con total (FMI) ####
gastocomp<-consolidado %>% select(c(code,year,Nivel,pibf,gtof,def:soc)) %>%
  filter_at(vars(gtof:soc),any_vars(!is.na(.))) %>% 
  mutate(gtofun=select(.,def:soc) %>% rowSums(),difgto=gtofun-gtof)

# Gráfica de diferencia a través del tiempo
ggplot(gastocomp %>% filter(!is.na(difgto)),aes(x=year,y=difgto, color=Nivel))+
  geom_point()#+scale_x_continuous(limits = c(1990,2020))

# Diferencia promedio por nivel
prom_nivel<-gastocomp %>% filter(!is.na(difgto)) %>% mutate(difgtod=difgto*pibf/100) %>% group_by(Nivel,year) %>% 
  summarise(sdifgto=sum(difgtod),spib=sum(pibf),N=n_distinct(code),med=median(difgto)) %>% mutate(pdifgto=sdifgto/spib*100)
ggplot(prom_nivel,aes(x=year,y=med,color=Nivel))+geom_line()
prom_nivel %>% group_by(Nivel) %>% 
  summarise(sdifd=sum(sdifgto),spib=sum(spib),mN=mean(N),med=median(med)) %>% 
  mutate(pdifgto=sdifd/spib*100)
gastocomp %>% filter(!is.na(difgto)) %>% mutate(difgtod=difgto*pibf/100, m=mean(difgtod)) %>%  group_by(Nivel) %>% 
  summarise(avg=sum(difgtod)/sum(pibf)*100,stddev=sd(difgtod)/sd(pibf)*100)

# Zoom Nivel ingreso bajo
ggplot(data=gastocomp %>%  filter(!is.na(difgto),Nivel=="Bajo"),aes(x=year,y=difgto,color=code))+
  geom_point()+geom_line()

# Zoom Nivel ingreso Medio-bajo
ggplot(data=gastocomp %>%  filter(!is.na(difgto),Nivel=="Medio-bajo"),aes(x=year,y=difgto,color=code))+
  geom_point()+geom_line()+
  geom_text(aes(label=code))
# LTU 1997: 
#CHN 2007: 
#MNG 2016: la suma por función parece estar baja siempre, 
  #la serie tiene huecos que se rellenaron con medias condicionales e interpolacionse
  #En 2016 el gasto el gasto 

# Resumen media y sd de la diferencia
temp<-gastocomp %>% group_by(code,Nivel) %>% summarise(m=mean(difgto,na.rm = TRUE),sd=sd(difgto,na.rm = TRUE)) %>% na.omit()
# Gráfica media(dif) vs sd(dif) con etiquetas del país
ggplot(temp,aes(m,sd,label=code))+geom_point()+
  geom_text_repel()+
  geom_density2d()
# Gráfica media(dif) vs sd(dif) con colores por nivel de ingreso
ggplot(temp,aes(m,sd))+geom_point(aes(col=Nivel))+
  geom_density2d()

plot(x=gastocomp$year,y=gastocomp$difgto)



# más gráficas de la diferencia por año, por nivel de ingreso, por continente, resumen por país


consolidado=read.csv(file = "./Data/consolidado.csv",header = TRUE)
consolidado=read.csv(file = "./Data/consolidado2.csv",header = TRUE)
######## Resumen Representatividad #######


# Cuadro de representatividad de gasto y gdp de los datos ####
# tratar de representar gráficamente

# Total países
temp<-consolidado %>% group_by(code) %>% summarise(N=n_distinct(year))
summary(temp$N)
ggplot(temp,aes(x=N))+geom_histogram(binwidth = 1)
stem(temp$N)

# por continente
temp<-consolidado %>% group_by(cont) %>% summarise(ni=n_distinct(code),ti=n_distinct(year))
consolidado %>% group_by(cont,code) %>% summarise(ti=n_distinct(year)) %>% 
  summarise(ni=n_distinct(code),mint=min(ti),avgt=mean(ti),mediant=median(ti),maxt=max(ti))
library(janitor)
temp %>% adorn_totals()
temp %>% gather(dim,conteo,-cont) %>% 
ggplot(aes(x=cont,y=conteo,fill=dim))+
  geom_col(position = "dodge")
consolidado %>% filter(cont=="Asia") %>% 
  group_by(code) %>% summarise(t=n_distinct(year)) %>% arrange(t) %>% 
  mutate(pais=countrycode(code,"genc3c","country.name"))
# quitar Timor Leste de la base pues sólo tiene 1 observación
consolidado<- consolidado %>% filter(!code=="TLS")

# por Nivel de ingreso
consolidado %>% group_by(Nivel) %>% summarise(ni=n_distinct(code),ti=n_distinct(year)) %>% 
  gather(dim,conteo,-Nivel) %>% 
  ggplot(aes(x=Nivel,y=conteo,fill=dim))+
  geom_col(position = "dodge")
consolidado %>% group_by(Nivel,code) %>% summarise(ti=n_distinct(year)) %>% 
  summarise(ni=n_distinct(code),mint=min(ti),avgt=mean(ti),mediant=median(ti),maxt=max(ti))
consolidado %>% group_by(Nivel,code) %>% summarise(ti=n_distinct(year)) %>% filter(ti<=3) %>% 
  mutate(pais=countrycode(code,"genc3c","country.name"))

consolidado %>% group_by(code) %>% summarise(ti=n_distinct(year)) %>% filter(ti<=6) %>% 
  mutate(pais=countrycode(code,"genc3c","country.name"))


# completar gto cuando se tiene el desglose ####
consolidado=read.csv(file = "./Data/consolidado2.csv",header = TRUE)

consolidado=consolidado %>% mutate(gtofuns=select(.,def:soc) %>% rowSums(),
                                   gto2=ifelse(is.na(gtof),gtofuns,gtof))
#quitar timor leste
consolidado <- consolidado %>% filter(!code=="TLS")

write.csv(consolidado,file = "./Data/consolidado2.csv",row.names = FALSE)

# Cuadros en documento ####


# 1er Tabla # resumen de observaciones por continente 
tab<-consolidado %>% group_by(cont,code) %>% 
  summarise(nanios=n_distinct(year),Desde=min(year),Hasta=max(year)) %>% 
  summarise(npaises=n_distinct(code),minanios=min(nanios),maxanios=max(nanios),Desde=min(Desde),Hasta=max(Hasta)) %>%
  adorn_totals("row") #%>% 
names(tab)<-c("Continente","Países","Mín. Años","Máx. Años","Desde","Hasta")

tab[6,3:6]<-t(c(min(tab[1:5,3]),max(tab[1:5,4]),min(tab[1:5,5]),max(tab[1:5,6])))
tab
print(xtable(tab ,digits=1),include.rownames=FALSE)

# auxiliar para comentario paises con pocos datos
consolidado %>% group_by(code) %>% 
  summarise(nyear=n_distinct(year)) %>% 
  mutate(pais=countrycode(code,"genc3c","country.name")) %>% 
  arrange(nyear)

# 2da tabla # Resumen de representatividad del Gasto y del PIB
tab<-consolidado %>% select(code,year,pibf,gto2,pib,gtop) %>% group_by(year) %>% 
 summarise(n=n_distinct(code)) # 2017 tiene más datos

selecWB<-read.csv(file = "./Data/World Bank/selecWB2.csv",header = TRUE)
extraFMI<-read.csv(file = "./Data/FMI/extraFMI2.csv",header = TRUE)

temp<-selecWB %>% select(code,year,pib,gtop,pop) %>% # Datos completos para abarcar países fuera del GFS
  merge(extraFMI %>% select(code,year,pibf,gtof),all = TRUE) %>% 
  merge(consolidado %>% select(code,year,gto2,def),all = TRUE)


temp %>% filter(year==2017) %>%  # Chequeo variable población
  mutate(fmi=is.na(pibf),pais=countrycode(code,"genc3c","country.name")) %>%
  filter(!is.na(pais)) %>% 
  group_by(fmi) %>% 
  summarise(spob=sum(pop/1e6,na.rm = TRUE))

tab<-temp %>% group_by(year) %>% # Inclusion variable indicadora dato disponible
  filter(year>=1970) %>% 
  mutate(across(pib:gto2,~!is.na(.),.names = "{col}.n")) %>% 
  summarise(across(pib.n:gto2.n,sum))

temp %>% filter(year==2017) %>% # conteo de observaciones según disponibilidad de gto x función
  mutate(dispf=!is.na(def)) %>% 
  group_by(dispf) %>% 
  summarise(across(pib:gto2,~sum(!is.na(.))))

tab<-temp %>% filter(year==2017,!is.na(pibf)) %>% # resumen pib, gto en $ y población
  mutate(gto2=ifelse(is.na(gto2),gtof,gto2)) %>% #retomar gasto para países que  no tienen detalle
  mutate(dispf=!is.na(def),gtod=gto2*pibf/100) %>%
  group_by(dispf) %>% 
  summarise(spib=sum(pibf,na.rm = TRUE),
            sgto=sum(gtod,na.rm = TRUE),
            spob=sum(pop/1e6,na.rm = TRUE),
            n=n_distinct(code)) 

tabp<-tab %>% adorn_percentages(denominator = "col") %>% # calcular columnas de porcentajes
  set_names(c("dispf","ppib","pgto","ppob","pn")) %>% 
  mutate(across(ppib:pn,~.*100)) %>% 
  select(-dispf)
tab2<-  tab %>% # combinar tablas anteriores
  mutate(across(spib:spob,~./1000)) %>% 
  cbind(tabp) %>% 
  adorn_totals("row") %>% 
  select(dispf,spib,ppib,sgto,pgto,n,pn,spob,ppob)

print(xtable(tab2,digits=1),include.rownames=FALSE)

# 3ra tabla # Gasto promedio por continente vs México
#datos méxico

#orden funciones
ord<-data.frame(fun=names(consolidado %>%  select(def:soc)),
                orden=c(2,4,9,5,1,7,6,3,8,10))
# datos de méxico y total
temp<-consolidado %>% filter(year==2017,!is.na(def),code=="MEX") %>% 
  select(cont,def:soc,gto2,pibf) %>% 
  mutate(cont="México",ncode=1,pibf=pibf/1000) %>% bind_rows(
  consolidado %>% filter(year==2017,!is.na(def)) %>% 
    select(code,cont,def:soc,gto2,pibf) %>% 
    mutate(cont="Total") %>% 
    group_by(cont) %>% 
    mutate(across(def:gto2,~./100*pibf)) %>% #convertir gto a dinero
    summarise(across(def:pibf,sum),ncode=n_distinct(code)) %>% 
    mutate(across(def:gto2,~./pibf*100),pibf=pibf/1000) # regresar a porcentaje, pib en miles de millones USD
  )
tab<-consolidado %>% filter(year==2017,!is.na(def)) %>% 
  select(code,year,cont,def:soc,gto2,pibf) %>% 
  mutate(across(def:gto2,~./100*pibf)) %>% #convertir gto a dinero
  group_by(cont) %>% 
  summarise(across(def:pibf,sum),ncode=n_distinct(code)) %>% 
  mutate(across(def:gto2,~./pibf*100),pibf=pibf/1000) %>% # regresar a porcentaje, pib en miles de millones USD
  bind_rows(temp) %>% # Pegar datos de México
  pivot_longer(cols = def:ncode,names_to="var",values_to="val") %>% #1er paso para transponer
  pivot_wider(names_from = cont,values_from=val) %>% #2do paso para transponer
  select(var,México,Africa:Total) %>% 
  arrange(desc(Total))

tab<-tab[c(4:13,3,1,2),]
tab$var[11:13]<-c("Total Gasto","PIB","#Paises")
print(xtable(tab,digits = 1),include.rownames = FALSE)

# Resumen de la clasificación de ingreso #
temp<-merge(selecWB %>% mutate(code=ifelse(code=="XKX","XKS",code)), extraFMI, all = TRUE) %>% 
  merge(consolidado %>% select(code,year,Nivel,gto2),all = TRUE) %>% 
  filter(year==2017) %>% 
  select(code,year,pibpc,pib,pibf,pop,gto2) %>% 
  mutate(fmi=is.na(pibf),pais=countrycode(code,"genc3c","country.name"),
         pop=pop/1e6) %>%
  filter(!is.na(pais)) #%>% 
temp %>% 
  summarise(across(pibf:pop,sum,na.rm=TRUE))

temp %>% ggplot(aes(x=pop))+
  geom_point(aes(y=pibpc))+
  geom_point(aes(y=pibf*pop),col="blue")
  scale_y_log10()+
  scale_x_continuous(trans = "log")

tab<-selecWB %>% mutate(code=ifelse(code=="XKX","XKS",code)) %>% 
  merge(extraFMI, all = TRUE) %>% 
  merge(consolidado %>% select(code,year,gto2),all = TRUE) %>%
  merge(wbclasif,all.x = TRUE) %>% 
  filter(year==2017) %>% 
  select(code,year,Nivel,pibpc,pib,pibf,pop,gtof,gto2) %>% 
  mutate(gto2=ifelse(is.na(gto2),gtof,gto2)) %>% 
  mutate(fmi=is.na(pibf),pais=countrycode(code,"genc3c","country.name")) %>% #agregar nombre de país
  filter(!is.na(pais)) %>% # para quitar entradas que no son países
  mutate(Nivel=ifelse(code=="TWN","Alto",Nivel)) %>% #recuperar nivel de Taiwan que no está en la base
  group_by(Nivel) %>% 
  mutate(gto2=gto2*pibf/100,pibpc=pibpc*pop,pib=pib/1E9) %>% # re escalar variables
  summarise(across(pibpc:gto2,sum,na.rm = TRUE),n=n()) %>% 
  mutate(pibpc=pibpc/pop,pop=pop/1E6) %>% # regresar a proporciones
  mutate(across(pibpc:gto2,~./1000)) %>% # re escalar a miles
  arrange(pibpc) %>% 
  adorn_totals("row") %>% 
  select(Nivel, n,pop,pibpc,pib)
tab
tab[5,4]<-tab[5,5]/tab[5,3]

print(xtable(tab,digits = 1),include.rownames = FALSE)
