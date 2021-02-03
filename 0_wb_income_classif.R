# Obtiene la clasificación de países por nivel de Ingreso según el Banco Mundial
# consolida la clasificación con variables del Banco Mundial:
#     gdp PPP, gdp pc PPP, pob, densidad pob, recursos, #negocios/1000hab, income share.
# lee: Country_classif, WDIData.csv
# crea: wbclasif.csv
#

# Paquetes
library(tidyverse)
library(countrycode) # para convertir nombres de paises a códigos 
suppressMessages(library(magrittr)) #para operadores pipeline como %<>%
suppressMessages(library(readxl)) # para leer archivos xls
library(xtable) #para imprimir tablas en formato latex

# Directorio

setwd("C:/Users/sgome/Dropbox/#tesis")

# Lectura archivos

#Clasificación
clasif<-read_excel(path = "./Data/World Bank/Country_classif.xls",
                   sheet="Country Analytical History",
                   range = "A6:AI229",
                   na="..", .name_repair="unique") 

names(clasif)[1:2]=c("code","country")
clasif17<- clasif %>% filter(!is.na(code)) %>% select(code, country,33)%>% rename(clas="2017")
 
#Population
wbdata<-read.csv(file="./Data/World Bank/WDIData.csv", encoding = "UTF-8")

#wbdata %>% group_by(Indicator.Name) %>% summarise(n=n_distinct(Country.Code))
pobla<- wbdata %>% filter(Indicator.Name=="Population, total") %>% 
  select(Country.Code,c(5:64)) %>%
  pivot_longer(cols = starts_with("X"), names_to = "year",values_to = "pob") %>%
  mutate(year=substr(year,2,5)) %>%
  rename(code=Country.Code)

#gdp per cápita GDP per capita, PPP (current international $)
dens<-wbdata %>% filter(Indicator.Name=="Population density (people per sq. km of land area)") %>% 
  select(Country.Code,c(5:64)) %>%
  pivot_longer(cols = starts_with("X"), names_to = "year",values_to = "surf") %>%
  mutate(year=substr(year,2,5))%>%
  rename(code=Country.Code)

reso<-wbdata %>% filter(Indicator.Name=="Total natural resources rents (% of GDP)") %>% 
  select(Country.Code,c(5:64)) %>%
  pivot_longer(cols = starts_with("X"), names_to = "year",values_to = "reso") %>%
  mutate(year=substr(year,2,5))%>%
  rename(code=Country.Code)

gdp<-wbdata %>% filter(Indicator.Name=="GDP, PPP (constant 2011 international $)") %>% 
  select(Country.Code,c(5:64)) %>%
  pivot_longer(cols = starts_with("X"), names_to = "year",values_to = "gdp") %>%
  mutate(year=substr(year,2,5))%>%
  rename(code=Country.Code)

gdpperc<-wbdata %>% filter(Indicator.Name=="GDP per capita, PPP (constant 2011 international $)") %>% 
  select(Country.Code,c(5:64)) %>%
  pivot_longer(cols = starts_with("X"), names_to = "year",values_to = "gdppc") %>%
  mutate(year=substr(year,2,5))%>%
  rename(code=Country.Code)

bussi<-wbdata %>% filter(Indicator.Name=="New business density (new registrations per 1,000 people ages 15-64)") %>% 
  select(Country.Code,c(5:64)) %>%
  pivot_longer(cols = starts_with("X"), names_to = "year",values_to = "bussi") %>%
  mutate(year=substr(year,2,5))%>%
  rename(code=Country.Code)

ishare<-wbdata %>% filter(str_detect(Indicator.Name,"Income share")) %>% 
  select(Country.Code,Indicator.Name,c(5:64)) %>%
  pivot_longer(cols = starts_with("X"), names_to = "year",values_to = "ishare") %>%
  mutate(year=substr(year,2,5))%>%
  rename(code=Country.Code) %>%
  pivot_wider(names_from = Indicator.Name, values_from = ishare)
names(ishare)[3:9]<-c("4th20","top10","top20","bot10","bot20","2nd20","3rd20")

clasif17 %>% group_by(clas)%>% summarise(n=n())
# Otras a agregar Income share held by , New business, Surface area (sq. km), Total natural resources rents (% of GDP)



# Guardar base para cruzar con otros ejercicios
# todos los años, datos de población, clasificación y gdppcPPP

lclasif<- clasif %>% filter(!is.na(code)) %>% 
  pivot_longer(cols = 3:35, names_to = "year", values_to = "clas")


wbclasif<- merge(x=lclasif,y=pobla) %>% merge(y=gdpperc) %>%
  merge(y=gdp) %>% merge(y=dens) %>% merge(reso) %>% merge(bussi) %>% merge(ishare)
tradclase<- data.frame(clas=c("L","LM","UM","H"),Nivel=c("Bajo","Medio-bajo","Medio-alto","Alto")) #Traducción categorías
wbclasif<-merge(wbclasif,tradclase)
write.csv(x=wbclasif,file = "./Data/wbclasif.csv",row.names = FALSE)

# Resumen categorías, ingreso y población
# filtrar los datos de 2017
pobla %<>% filter(year==2017) %>% select(-year)
gdpperc %<>% filter(year==2017)%>% select(-year)

clasif2<- merge(x=clasif17,pobla) %>% merge(y=gdpperc)

clasif2 %>% filter(is.na(gdppc))
clasif2 %>% mutate(nodato=is.na(gdppc)+is.na(pob)) %>%
  group_by(nodato,clas) %>%
  summarise(n=n_distinct(code),totpob=sum(pob)/1e9,medgdppc=median(gdppc)/1e3,pgdppc=mean(gdppc)/1e3)
rclasif<-clasif2 %>% na.omit() %>%
  group_by(clas) %>%
  summarise(n=n_distinct(code),totpob=sum(pob)/1e9,medgdppc=median(gdppc)/1e3,pgdppc=mean(gdppc)/1e3) %>%
  arrange(pgdppc)

tot<-rclasif %>% summarise_if(is.numeric,sum)
resumen<-rclasif %>% bind_rows(tot) %>% mutate(Nivel=c("Bajo","Medio-bajo","Medio-alto","Alto","Total"))%>%
  select(Nivel,n,totpob,medgdppc,pgdppc) %>%
  rename( "Población total"=totpob,
          "No. de países"=n,
          "Mediana PIB per cápita"=medgdppc,
          "Promedio PIB per cápita"=pgdppc)

print(xtable(resumen ,digits=1),include.rownames=FALSE)

# Anexo con la clasificación para todos los países
tradclase<- data.frame(clas=c("L","LM","UM","H"),Nivel=c("Bajo","Medio-bajo","Medio-alto","Alto"))
apenclasif<- clasif2 %>% na.omit() %>%
  merge(y=tradclase) %>%
  select(country,code,Nivel,pob,gdppc) %>%
  arrange(desc(gdppc)) %>%
  mutate(pob=pob/1e6,gdppc=gdppc/1e3) %>%
  rename(País=country,"Código"=code, "Población millones"=pob,"PIB per cápita miles $USD"=gdppc)

print(xtable(apenclasif ,digits=1),include.rownames=FALSE)
