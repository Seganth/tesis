# Regresiones

# paquetes ####
library(tidyverse)
library(plm)
library(ggrepel)
library(ggsci) # paletas de colores de journals
library(scales) # para break formatting functions
library("lmtest") #para coeftest
library(ggplot2)
library(countrycode)
library(compositions)
library(e1071) #para sesgo y kurtosis
library(stargazer)
library(xtable)
library(MVN)
library(janitor)

nivtrans<-function(x){# para ordenar niveles de ingreso
  x %>% mutate(Nivel=factor(Nivel,levels = c("Bajo","Medio-bajo","Medio-alto","Alto","México")))
}

# datos
setwd("C:/Users/sgome/Dropbox/#tesis")
consolidado=read.csv(file = "./Data/consolidado2b.csv",header = TRUE) %>% #sin filtrar NAs
  nivtrans()
paises<-read.csv("./Data/nomb_paises.csv",encoding = "UTF-8")
idhs<-read.csv("./Data/UN/variaciones/ind2.csv") # variantes índices desarrollo

minval<-consolidado %>% filter(across(def:soc,~.>0)) %>% summarise(across(def:soc,~min(.)/10,na.rm=TRUE)) %>% 
  pivot_longer(cols = def:soc,names_to="fun",values_to="min")
lsfun<-list("media"=mean,"dev"=sd,
            "p25"=function(x,na.rm=T) quantile(x,probs = 0.25,na.rm),
            "p75"=function(x,na.rm=T) quantile(x,probs = 0.75,na.rm),
            "mín"=min,"máx"=max,
            "sesgo"=skewness,"kurtosis"=kurtosis)

# Datos con PIB ####
funciones <- consolidado %>% filter(code!="TLS") %>% # quitar pais con pocas observaciones
  mutate(Nivel=ifelse(code=="TWN","Alto",as.character(Nivel))) %>% #completar Taiwan
  mutate(cont=countrycode(code,"genc3c","continent")) %>% #completar continente
  mutate(cont=ifelse(code=="XKS","Europe",cont)) %>% #completar continete para Kosovo
  #mutate(Nivel=ifelse(code=="MEX","México",Nivel)) %>% #México en categoría separada
  mutate(across(c(def:soc),~ifelse(.==0,min(minval$min),.))) %>% #imputación valores pequeños
  select(code:soc,gto2,cre,pibpc) %>% filter(across(code:gto2,~!is.na(.))) %>% nivtrans()
head(funciones)
levels(funciones$Nivel)


fgrupo<-read.csv("./Data/gto_conglo.csv") %>% # grupos y resumen de atípicos por grupo
  mutate(peso=ifelse(outlier,1/mD,1),Grupo=as.factor(Grupo))
levels(fgrupo$Grupo)

fgrupo %>%group_by(outlier) %>% summarise(across(mD,lsfun))
fgrupo %>%group_by(outlier) %>% summarise(across(peso,lsfun))

# g_t
temp<-funciones %>% select(code,year,cre,pibpc) %>% 
  pdata.frame() %>% 
  group_by(code) %>%
  mutate(g=100*pibpc/dplyr::lag(pibpc)-100)

plot(temp$cre,temp$g)

g_dat<-funciones %>% select(code,year,Nivel,pibpc,cre,gto2,def:soc) %>% 
  mutate(resto=seg+def+amb+cul+viv) %>% select(-c(seg,def,amb,cul,viv)) %>% #amalgama
  mutate(across(eco:resto,~log(./gob))) %>% select(-gob) %>% # logcocientes
  merge(fgrupo %>% select(code,year,Grupo:peso)) %>% # variables de grupos
  mutate(gtosq=gto2^2) %>% filter(year>=1990) %>% # gasto^2 y filtro año
  filter(!is.na(cre)) %>% # sólo datos disponibles
  mutate(d92=ifelse(year==1992,1,0),d93=ifelse(year==1993,1,0),
         d08=ifelse(year==2008,1,0),d09=ifelse(year==2009,1,0)) %>% #indicadoras crisis
  pdata.frame()
pdim(g_dat)  

g_mco<-plm(cre~gto2+soc+edu+eco+sal+resto,g_dat,model = "pooling") # ols
g_int<-plm(cre~gto2+soc+edu+eco+sal+resto,g_dat,model = "within", effect = "individual") # within
g_mcg<-plm(cre~gto2+soc+edu+eco+sal+resto,g_dat,model = "random", effect = "twoways") # gls

lmodel<-list(g_mco,g_int,g_mcg)
fnames<-c("soc","edu","eco","sal","resto")
sgama<-sapply(lmodel,function(x) sum(x$coefficients[fnames]))

stargazer(g_mco,g_int,g_mcg, type="text",
          digits = 2,
          title = "Regresión de panel para el crecimiento económico",
          column.labels = c("$\\hat{\\und\\gamma}_{\\text{MCO}}$",
                            "$\\hat{\\und\\gamma}_{\\text{W}}$",
                            "$\\hat{\\und\\gamma}_{\\text{MCG}}$"),
          single.row = T,
          dep.var.caption = "Variable dependiente: $g_t$",
          omit.stat = "F",
          #dep.var.labels = "$g_t$",
          label = "tab:gmod0",
          dep.var.labels.include = F,
          covariate.labels=c("gto","$\\log$(soc/gob)","$\\log$(edu/gob)",
                             "$\\log$(eco/gob)","$\\log$(sal/gob)",
                             "$\\log$(resto/gob)","ordenada"))

coeftest(g_mco,vcov. = vcovHC)
coeftest(g_mco2,vcov. = vcovHC)
coeftest(g_int,vcov. = vcovHC)
coeftest(g_int2,vcov. = vcovHC)
coeftest(g_mcg,vcov. = vcovHC)
coeftest(g_mcg2,vcov. = vcovHC)

coeftest(g_mco)
coeftest(g_mco2)
coeftest(g_int2)
fixef(g_int2)

# Coeficientes ####
# variables x Nivel
consolidado %>% filter(code!="TWN",year==2017) %>% group_by(Nivel) %>%
  summarise(gto=mean(gto2,na.rm = T),
            g=mean(cre,na.rm = T),
            n=n_distinct(code)) #%>% adorn_totals()


g_mco$coefficients

sum(g_mco$coefficients[fnames])
sum(g_int$coefficients[fnames])
sum(g_mcg$coefficients[fnames])

mean(g_dat$cre)
gm<-g_dat %>% filter(code=="MEX",year==2019) %>% select(eco:resto,gto2)
gm<-funciones %>% filter(code=="MEX",year==2019) %>%
  select(def:soc,gto2) %>% mutate(resto=def+amb+viv+seg+cul) %>% select(-c(def,amb,viv,seg,cul)) %>% 
  relocate(gto2,.after=resto)
gm

logob<-function(x) x %>% mutate(across(eco:resto,~log(./gob))) %>% 
  select(-gob)
logob(gm)
predict(g_mco,logob(gm))-
predict(g_mco,logob(gm+c(0,1,0,0,-1,0,0)))

predict(g_mco,logob(gm))-
  predict(g_mco,logob(gm+c(0,0,-1,1,0,0,0)))

fitted.values(g_mco)
# g plots ####
# Por nivel de ingreso
opciones<-list(theme_bw()+xlab("")+ylab(""),
                 coord_cartesian(ylim = c(0,6)),
                 scale_fill_locuszoom(),
                 theme(legend.position = "none",axis.text.x = element_text(angle = 90),
                       axis.text.y = element_blank(),axis.ticks=element_blank()))
temp<-consolidado %>% filter(year>=1990,!is.na(cre)) %>% select(code,pais,year,Nivel,cont,cre) %>% 
  mutate(Nivel=ifelse(code=="MEX","México",as.character(Nivel))) %>% nivtrans()
temp %>% group_by(Nivel) %>% summarise(n=n_distinct(code)) %>% adorn_totals()

temp %>% mutate(cont=ifelse(is.na(cont),countrycode(code,"genc3c","continent"),cont)) %>% 
  group_by(cont,Nivel) %>% summarise(n=n_distinct(code)) %>% adorn_totals()
a<-consolidado %>% filter(year>=1990,!is.na(cre)) %>% select(code,year,Nivel,cre) %>% 
  mutate(Nivel=ifelse(code=="MEX","México",as.character(Nivel))) %>% nivtrans() %>% #México en categoría separada
  group_by(Nivel) %>% 
  summarise(g=mean(cre,na.rm = T),N=n_distinct(code),n=n()/N,g2=100*geometricmean(1+cre/100)-100) %>% 
  ggplot(aes(x=reorder(Nivel,g),y=g,fill=Nivel)) +
  geom_bar(stat = "identity")+ #coord_flip()+
  geom_text(aes(label=round(g,2)),nudge_y = .5,angle=90,size=3)+
  theme_bw()+xlab("")+ylab("")+
  coord_cartesian(ylim = c(0,6))+
  scale_fill_locuszoom()+
  theme(legend.position = "none",axis.text.x = element_text(angle = 90),
        axis.text.y = element_blank(),axis.ticks=element_blank())

# Por continente
temp<-consolidado %>% filter(year>=1990,!is.na(cre)) %>% 
  mutate(cont=ifelse(is.na(cont),countrycode(code,"genc3c","continent"),cont))
temp %>% group_by(cont) %>% summarise(n=n_distinct(code)) %>% adorn_totals()
b<-consolidado %>% filter(year>=1990,!is.na(cre)) %>% 
  mutate(cont=ifelse(is.na(cont),countrycode(code,"genc3c","continent"),cont)) %>% 
  select(code,year,cont,cre) %>% 
  group_by(cont) %>% 
  summarise(g=mean(cre,na.rm = T),N=n_distinct(code),n=n()/N,g2=100*geometricmean(1+cre/100)-100) %>% 
  ggplot(aes(x=reorder(cont,g),y=g,fill=cont)) +
  geom_bar(stat = "identity")+# coord_flip()+
  geom_text(aes(label=round(g,2)),nudge_y = .5,angle=90,size=3)+
  theme_bw()+xlab("")+ylab("")+
  coord_cartesian(ylim = c(0,6))+
  scale_fill_aaas()+
  theme(legend.position = "none",axis.text.x = element_text(angle = 90),
        axis.text.y = element_blank(),axis.ticks=element_blank())

# Por grupo gasto público
g_dat %>% summarise(n=n_distinct(code))
c<-g_dat %>% filter(as.numeric(as.character(year))>=1990,!is.na(cre)) %>% select(code,year,Grupo,cre) %>% 
  group_by(Grupo) %>% 
  summarise(g=mean(cre,na.rm = T),N=n_distinct(code),n=n()/N,g2=100*geometricmean(1+cre/100)-100) %>% 
  ggplot(aes(x=reorder(paste("Grupo",as.numeric(Grupo)),g),y=g,fill=Grupo)) +
  geom_bar(stat = "identity")+ #coord_flip()+
  geom_text(aes(label=round(g,2)),nudge_y = .5,angle=90,size=3)+
  theme_bw()+xlab("")+ylab("")+
  coord_cartesian(ylim = c(0,6))+
  scale_fill_npg()+
  theme(legend.position = "none",axis.text.x = element_text(angle = 90),
        axis.text.y = element_blank(),axis.ticks=element_blank())


library(cowplot)
allplotslist <- align_plots(a,b,c, align = "hv")
library(ggpubr)

ggarrange(allplotslist[[1]],allplotslist[[2]],allplotslist[[3]],ncol=3)
ggsave("./TeX/Fig/441_0_ggrupos.pdf",width = 14,height = 8,units = "cm")

g_dat %>% group_by(Grupo) %>% summarise(across(cre:resto,mean)) #promedio var por grupo

# variables promedio por grupo
funciones %>% select(code,year,Nivel,cre,gto2,def:soc) %>% 
  mutate(resto=seg+def+amb+cul+viv) %>% select(-c(seg,def,amb,cul,viv)) %>% #amalgama
  #mutate(across(eco:resto,~log(./gob))) %>% select(-gob) %>% # logcocientes
  merge(fgrupo %>% select(code,year,Grupo:peso)) %>% # variables de grupos
  mutate(gtosq=gto2^2) %>% filter(year>=1990) %>% # gasto^2 y filtro año
  filter(!is.na(cre)) %>% # sólo datos disponibles
  mutate(d92=ifelse(year==1992,1,0),d93=ifelse(year==1993,1,0),
         d08=ifelse(year==2008,1,0),d09=ifelse(year==2009,1,0)) %>% #indicadoras crisis
  pdata.frame() %>% 
  group_by(Grupo) %>% summarise(across(cre:resto,mean)) %>% 
  xtable(caption = "Gasto y crecimiento de los países por grupos",
         label="ggasto_res",digits=1) %>% print(include.rownames=F)

# Individual effects ####

#within_intercept Overall Intercept for Within Models Along its Standard Error ###

within_intercept(g_int)

fixef(g_int,type="dmean")

mean(fixef(g_int))
data.frame(code=names(fixef(g_int,type = "dmean")),fijo=fixef(g_int),alea=ranef(g_mcg)) %>% 
ggplot(aes(x=reorder(code,fijo),y=fijo))+
  geom_bar(stat = "identity",position = "dodge")+
  theme(axis.text.x = element_text(angle=90))

data.frame(code=names(fixef(g_int)),fijo=fixef(g_int,type = "dmean"),alea=ranef(g_mcg)) %>% 
  merge(g_dat %>% group_by(code) %>% summarise(g=mean(cre))) %>% 
  merge(paises %>% select(code,esp)) %>% 
  ggplot(aes(x=fijo,y=alea,label=esp))+geom_point(aes(size=g)) +
  geom_text_repel(size=3)

# Time effects ####

#ranef.plm Extract the Random Effects
ranef(g_mcg)
data.frame(year=1990:2019,tempo=ranef(g_mcg,effect = "time")) %>% 
  merge(g_dat %>% group_by(year) %>% summarise(g=mean(cre))) %>% 
  pivot_longer(cols = tempo:g,names_to="serie",values_to="valor") %>% 
  ggplot(aes(x=year,y=valor,col=serie))+geom_line()


# % Observaciones totales, países N, años T_i *
pdim(g_dat)

# Pruebas ####
# I. presencia de efectos
pFtest(update(g_int2,effect="time"),g_mco2)
pFtest(update(g_int2,effect="twoways"),g_mco2)
pFtest(update(g_int2,effect="twoways"),g_int2)
pFtest(g_int2,g_mco)
pFtest(g_int2,g_mco)

extp<-function(prueba,param=2){# extrae tabla con resultados de la prueba
  if (param==2) {
    valores=data.frame(prueba$statistic,names(prueba$statistic),prueba$p.value,prueba$method,
              prueba$parameter[[1]],prueba$parameter[[2]],
              prueba$alternative)
    names(valores)<-c("Estadístico","Dist.","p-value","Tipo","df1","df2","Hipótesis alt.")
    } 
  else{ 
    if(param==1){
      valores=data.frame(prueba$statistic,names(prueba$statistic),prueba$p.value,prueba$method,
                         prueba$parameter[[1]],
                         prueba$alternative)
      names(valores)<-c("Estadístico","Dist.","p-value","Tipo","df1","Hipótesis alt.")
    } 
    else{
    valores=data.frame(prueba$statistic,names(prueba$statistic),prueba$p.value,prueba$method,
              prueba$alternative)
    names(valores)<-c("Estadístico","Dist.","p-value","Tipo","Hipótesis alt.")
    } 
  }
return(valores)
}


# F test for ind / time effects *
for (i in c("individual","time","twoways")){
  temp<-extp(pFtest(update(g_int,effect=i),g_mco))
  temp$efecto<-i
  if (i=="individual") res<-temp else res<-rbind(res,temp)
}
pruf<-res

# plmtest Lagrange FF Multiplier Tests for Panel Models *
for (i in c("individual","time","twoways")){
  temp<-extp(plmtest(g_mco,effect = i),param = F)
  temp$efecto<-i
  if (i=="individual") res<-temp else res<-rbind(res,temp)
}
prulm<-res

tab<-bind_rows(pruf,prulm) %>% 
  mutate(across(df1:df2,~ifelse(is.na(.),"",formatC(.,format = "f",big.mark = ",",digits = 0)))) %>% 
  mutate(`p-value`=format(`p-value`,format="e",digits = 2)) %>% 
  mutate(Tipo=c("F","","","Honda","","")) %>% dplyr::select(Tipo,everything()) %>% 
  mutate(efecto=rep(c("individual","temporal","ambos"),2))

print(xtable(tab %>% dplyr::select(-`Hipótesis alt.`),
             caption="Pruebas de la presencia de efectos",
             label="tab:g0effects"), include.rownames=FALSE)

# II. Correlación de efectos
# % Hausman: cor(X,eta) *
phtest(g_int,g_mcg) 
# H0:RE (mcg) vs H1:FE (int)/ H0 not rejected / testing if cor(e,X)<>0 H0: cor(e,X)=0

head(g_dat)
head(g_dat[,c(fnames,"gto2","cre")])
tab<-sapply(c(fnames,"gto2","cre"), function (x) cor(fixef(g_int), between(g_dat[,x]))) 

print(xtable(as.data.frame(t(tab))),include.rownames = F)

consolidado %>% filter(code!="TWN",year>=1990,!is.na(cre)) %>% 
  mutate(Nivel=ifelse(code=="MEX","México",as.character(Nivel))) %>% nivtrans() %>% 
  summarise(n=n_distinct(code))

# Resumen de gasto total y g por nivel con todos los datos
temp<-consolidado %>% filter(code!="TWN",year>=1990) %>% 
  mutate(Nivel=ifelse(code=="MEX","México",as.character(Nivel))) %>% nivtrans() %>% #México en categoría separada
  group_by(Nivel) %>%
  summarise(gto=mean(gto2,na.rm = T),
            g=mean(cre,na.rm = T),
            n=n_distinct(code))
temp %>% adorn_totals()

# Tabla resumen por Nivel
funciones %>% dplyr::select(code,year,Nivel,cre,gto2,def:soc) %>% 
  mutate(resto=seg+def+amb+cul+viv) %>% dplyr::select(-c(seg,def,amb,cul,viv)) %>% #amalgama
  mutate(gtosq=gto2^2) %>% filter(year>=1990) %>% # gasto^2 y filtro año
  mutate(Nivel=ifelse(code=="MEX","México",as.character(Nivel))) %>% nivtrans() %>% #México en categoría separada
  pdata.frame() %>% 
  group_by(Nivel) %>% summarise(across(eco:resto,mean)) %>% 
  mutate(gto=temp$gto,g=temp$g) %>% relocate(g:gto,.after=Nivel) %>% 
  xtable(caption = "Promedio del crecimiento económico y del gasto de los países",
         label="gnivel_res",digits=1) %>% print(include.rownames=F)



# III. Autocorrelación
# % AR: Autocorrelación de los residuales *
temp<-pbgtest(g_mco)
pbgtest(g_int)
pbgtest(g_mcg)
#pbltest Baltagi and Li Serial Dependence Test For Random Effects Models
temp<-pbltest(g_mcg)

#pdwtest Durbin-Watson Test for Panel Models
pdwtest(g_mco)
pdwtest(g_int)
pdwtest(g_mcg)

tab<-rbind(extp(pbgtest(g_mco),1),
      extp(pbgtest(g_int),1),
      extp(pbgtest(g_mcg),1),
      extp(pbltest(g_mcg),1)) %>% 
  bind_rows(rbind(
      extp(pdwtest(g_mco),0),
      extp(pdwtest(g_int),0),
      extp(pdwtest(g_mcg),0))) %>% 
  mutate(Tipo=c(rep("Breusch-Godfrey",3),"Baltagi-Li",rep("Durbin-Watson",3))) %>% 
  mutate(`p-value`=format(`p-value`,format="e",digits = 2)) %>% 
  mutate(df1=ifelse(is.na(df1),"",format(df1,digits = 0))) %>% 
  mutate(Modelo=c("MCO","W","MCG","MCG","MCO","W","MCG")) %>% 
  dplyr::select(Tipo,everything(),-`Hipótesis alt.`)

tab
print(xtable(tab,digits = 2),include.rownames=FALSE)
# library(MASS)
consolidado %>% filter(dplyr::between(cre,-10,10)) %>% 
  group_by(code) %>% mutate(lg=dplyr::lag(cre)) %>% 
  filter(!is.na(lg),!is.na(cre)) %>% 
  ggplot(aes(x=lg,y=cre))+
  geom_hline(yintercept = 0,col="darkgray")+geom_vline(xintercept = 0,col="darkgray")+
  geom_point(alpha=0.5,size=0.5)+
  geom_density_2d(size=1,col=3)+
  theme_bw()+scale_color_locuszoom()+
  xlab(expression(g[t-1]))+ylab(expression(g[t]))
ggsave(filename = "./TeX/Fig/441_01_glag.pdf",width = 10,height = 8,units = "cm")

cuads<-read.csv("./Rcode/cuadrantes.csv")
consolidado %>% group_by(code) %>% mutate(lg=dplyr::lag(cre)) %>% 
  filter(!is.na(lg),!is.na(cre)) %>% ungroup() %>% select(code:cont,lg,cre) %>%
  mutate(xsign=sign(lg),ysign=sign(cre),id=paste(xsign,ysign,sep = "")) %>% 
  merge(cuads %>% select(id,cuadrante)) %>% select(-c(id)) %>% 
  group_by(cuadrante) %>% summarise(n=n_distinct(code),m=n())

temp<-consolidado %>% filter(!is.na(cre))
auto<-by(temp$cre, temp$code, function(i) { acf(i,  plot = FALSE)$acf }) 
temp<-matrix(NA,nrow = 209,ncol = 19)
for (i in 1:209) {
  temp[i,1]<-names(auto)[i]
  m<-length(auto[[i]])
  temp[i,2:(m+1)]<-auto[[i]]
}
colnames(temp)<-c("code",0:17)
auto<-temp %>% as.data.frame() %>%
  pivot_longer(cols = `0`:`17`,names_to="lag",values_to="cor") %>% 
  na.omit() %>% mutate(cor=as.numeric(cor),lag=as.numeric(lag))

auto %>% filter(lag <=15) %>% 
  ggplot(aes(x=lag,y=cor,group=lag)) +
  geom_boxplot()+theme_bw()+
  ylab("")

ggsave("./TeX/Fig/441_02_acfbox.pdf",width = 10,height = 8,units = "cm")



# g system GMM ####

fsignif=function(x,lvl=c(0.1,0.05,0.01)){
  y<-as.numeric(x)
  codigos = (y<lvl[1]) + (y<lvl[2]) + (y<lvl[3])
  codigos[codigos==3]<-"$^{***}$"
  codigos[codigos==2]<-"$^{**}$"
  codigos[codigos==1]<-"$^{*}$"
  res=paste(x,replace_na(codigos,""),sep="")
  return(res)}

eqsgmm<-cre ~ lag(cre) + gto2+ 
  soc+edu+eco+sal+resto |
  lag(cre, 2:99) | # system GMM
  lag(gto2,2)+lag(soc, 2)+lag(edu,2)+lag(eco,2)+lag(sal,2)+lag(resto,2)

eqsgmm2<-cre ~ lag(cre) + gto2+
  soc+edu+eco+sal+resto+d08+d09 |
  lag(cre, 2:99) | # system GMM
  lag(gto2,2)+lag(soc, 2)+lag(edu,2)+lag(eco,2)+lag(sal,2)+lag(resto,2)+lag(d08,2)+lag(d09,2)


base <- pgmm(eqsgmm,g_dat,index=c("code", "year"),model="twosteps",#subset = Nivel!="Bajo",
             effect="twoways",transformation = "ld",collapse = TRUE)

base.ind<-pgmm(eqsgmm,g_dat,index=c("code", "year"),model="twosteps",#subset = Nivel!="Bajo",
              effect="individual",transformation = "ld",collapse = TRUE)

baseic<-pgmm(eqsgmm2,g_dat,index=c("code", "year"),model="twosteps",#subset = Nivel!="Bajo",
               effect="individual",transformation = "ld",collapse = TRUE)

sbase<-summary(base);sbasei<-summary(base.ind);sbaseic<-summary(baseic)

lmodel<-list(sbasei,sbaseic,sbase)
vsarg<-format(sapply(lmodel, function(x) x$sargan$p.value),digits = 1) 
ar1<-format(sapply(lmodel, function(x) x$m1$p.value),digits = 2)
ar2<-format(sapply(lmodel, function(x) x$m2$p.value),digits = 2)
waldmu<-c("","",format(sbase$wald.td$p.value[[1]],digits = 2))

stargazer(base.ind,baseic,base,#type = "text",
          digits = 2,
          title = "sys-GMM para el crecimiento económico",
          label = "tab:gsGMM",
          column.labels = c("Sin $\\mu$", "Sin $\\mu$ + Ind. crisis","Con $\\mu$"),
          single.row = T,
          dep.var.caption = "Variable dependiente: $g_t$",
          #dep.var.labels = "$g_t$",
          dep.var.labels.include = F,
          # covariate.labels=c("$\\rho$","gto","$\\log$(soc/gob)","$\\log$(edu/gob)",
          #                    "$\\log$(eco/gob)","$\\log$(sal/gob)",
          #                    "$\\log$(resto/gob)","Ind. 2008","Ind. 2009"),
          keep.stat = "f",
          add.lines = list(#c("Observaciones",rep("1,296",3)),
                           c("Países",rep(65,3)),
                           c("Instrumentos",41,43,69),
                           c("Sargan p-value",vsarg %>% fsignif()),
                           c("AR(1) p-value",ar1 %>% fsignif()),
                           c("AR(2) p-value",ar2 %>% fsignif()),
                           c("Wald $\\mu_t$ p-value",waldmu %>% fsignif())
                           ))


# Covariance
sqrt(diag(vcov(base)))[1:7]
sqrt(diag(vcovHC(base)))[1:7]

#sargan
sargan(base)
sargan(base.ind)
sargan(difer)
sargan(difer.ind)

#serial correlation
mtest(base,vcov = vcovHC)

# Gráficas auxiliares ####
# efectos temporales
# efectos temporales

tiempo<-base$coefficients[[2]][9:36]
tiempo<-data.frame(year=names(tiempo),mu=tiempo)
base$vcov[9:36,9:36]
#plot(as.numeric(names(tiempo)),tiempo,type="b")
tiempo %>% ggplot(aes(x=as.numeric(year),y=mu))+geom_line(size=1,col=1)+
  #geom_smooth(formula = 'y ~ poly(x,3)',method = 'glm' )+
  theme_bw()+
  xlab('Año')+ylab('')+scale_x_continuous(n.breaks = 6)
ggsave("./TeX/Fig/441_1_temporal.pdf",width = 12,height = 4.5,units = "cm")

coeftest(base)

# con s.e. de mu
tiempo %>% ggplot(aes(x=as.numeric(year),y=mu))+geom_line(size=1,col=1)+
  geom_ribbon(aes(ymin=mu-sqrt(diag(base$vcov))[9:36],ymax=mu+sqrt(diag(base$vcov))[9:36]),alpha=0.5)
mean(sqrt(diag(base$vcov))[9:36])
mean(tiempo$mu)
# con boxplot de crecimiento 
g_dat %>% ggplot(aes(x=as.numeric(as.character(year)),y=cre,group=year)) + geom_boxplot()+
  geom_line(data=tiempo,aes(x=as.numeric(year),y=mu,group=1),size=1,col=4)+
  theme_bw()+xlab('Año')+ylab('')+scale_x_continuous(n.breaks = 6)+
  coord_cartesian(ylim=c(-10,10),xlim = c(1991,2019))
ggsave("./TeX/Fig/441_1_temporal2.pdf",width = 12,height = 4.5,units = "cm")

# Crisis por nivel
consolidado %>% filter(!is.na(cre)) %>%
  mutate(Nivel=ifelse(code=="MEX","México",as.character(Nivel))) %>% nivtrans() %>% #México en categoría separada
  group_by(Nivel) %>% filter(year %in% c(1992,1993,1995,2001,2008,2009)) %>% 
  dplyr::select(code,year,Nivel,cre) %>% 
  pivot_wider(names_from = year,values_from=cre) %>% 
  summarise(across(`1992`:`2009`,mean,na.rm=T)) 
# Crisis por grupos ####
# Crisis 2008 por Nivel
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

consolidado %>% filter(year==2009,!is.na(cre)) %>%
  mutate(Nivel=ifelse(code=="TWN","Alto",as.character(Nivel))) %>% #completar Taiwan
  #mutate(Nivel=ifelse(code=="MEX","México",as.character(Nivel))) %>% nivtrans() %>% #México en categoría separada
  nivtrans() %>% group_by(Nivel) %>% 
  #mutate(outlier = ifelse(is_outlier(cre),pais,"")) %>%
  ggplot(aes(x=Nivel,y=cre)) + geom_boxplot()+
  theme_bw()+xlab("")+ylab("")+
  geom_point(data = subset(consolidado,code=="MEX"&year==2009),aes(3,cre),col="blue")+
  geom_text(data = subset(consolidado,code=="MEX"&year==2009),aes(2.5,cre,label="México"))
ggsave("./TeX/Fig/441_12_crisis09.pdf",width = 8,height = 8,units = "cm")

consolidado %>% filter(year==2008,!is.na(cre)) %>%
  mutate(Nivel=ifelse(code=="TWN","Alto",as.character(Nivel))) %>% #completar Taiwan
  mutate(cont=countrycode(code,"genc3c","continent")) %>% #completar continente
  mutate(region=countrycode(code,"genc3c","region")) %>% 
  #dplyr::select(code,year,cont,region,cre) %>% arrange(cre)
  #mutate(Nivel=ifelse(code=="MEX","México",as.character(Nivel))) %>% nivtrans() %>% #México en categoría separada
  nivtrans() %>% #group_by(cont) %>% 
  #mutate(outlier = ifelse(is_outlier(cre),pais,"")) %>%
  ggplot(aes(x=Nivel,y=cre)) + geom_boxplot()+
  theme_bw()+xlab("")+ylab("")+#coord_cartesian(ylim = c(-10,20))
  geom_point(data = subset(consolidado,code=="MEX"&year==2008),aes(3,cre),col="blue")+
  geom_text(data = subset(consolidado,code=="MEX"&year==2008),aes(2.5,cre,label="México"))
ggsave("./TeX/Fig/441_12_crisis08.pdf",width = 8,height = 8,units = "cm")

# Crecimiento por Nivel
consolidado %>% filter(!is.na(cre)) %>%
  mutate(Nivel=ifelse(code=="MEX","México",as.character(Nivel))) %>% nivtrans() %>% #México en categoría separada
  group_by(Nivel,year) %>% summarise(g=mean(cre,na.rm = T)) %>% 
  ggplot(aes(x=year,y=g,col=Nivel))+geom_line()+scale_color_locuszoom()

# fitted values
ajust<-baseic$fitted.values %>% as.data.frame() %>% 
  mutate(year=names(base$residuals[[1]]),
         model=c(rep('d',28),rep('l',29))) %>% 
  pivot_longer(!c(year,model),names_to='code',values_to='fitted') %>% 
  mutate(fitted=na_if(fitted,0))
ajust %>% ggplot(aes(x=as.numeric(year),y=fitted))+geom_point()
# errores ####

lsfun<-list("media"=mean,#"dev"=sd,
            "plo"=function(x,na.rm=T) quantile(x,probs = 0.25,na.rm),
            "pup"=function(x,na.rm=T) quantile(x,probs = 0.75,na.rm))

todo<-data.frame(year=NA,resid_media=NA,resid_plo=NA,resid_pup=NA,model=NA)
modelos<-list("(3) Temp"=base,"(1) Indiv"=base.ind,"(2) Indiv+crisis"=baseic)

for (i in 1:length(modelos)) {
  errores<-modelos[[i]]$residuals %>% as.data.frame() %>%
    mutate(year=names(base$residuals[[1]]),
           model=c(rep('d',28),rep('l',29))) %>% 
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
  guides(fill=guide_legend(nrow=1))+scale_x_continuous(n.breaks = 6)
ggsave("./TeX/Fig/441_2_errores.pdf",width = 12,height = 4.5,units = "cm",bg="transparent")




temp %>% ggplot(aes(x=as.numeric(year),y=resid_media))+
  geom_ribbon(aes(ymin=resid_plo,ymax=resid_pup),fill="grey90")+geom_line(size=1)

errores %>% ggplot(aes(x=as.numeric(year),y=resid))+geom_point()

temp <- merge(ajust,errores) %>% na.omit() %>% 
  merge(g_dat %>% select(code,year,outlier,Grupo,cre,Nivel),all.x = T)
temp %>% 
  filter(!outlier) %>% 
  ggplot(aes(x=fitted,y=resid))+geom_point(alpha=0.5)+
  geom_density2d()

temp%>% 
  ggplot(aes(x=fitted,y=cre))+
  geom_point(aes(col=Nivel))+#geom_smooth()+
  coord_fixed()+scale_color_locuszoom()+geom_density2d()

temp %>% filter(!outlier) %>%
  select(fitted,resid,Nivel) %>% 
  mvn(mvnTest = 'dh', subset = "Nivel",desc = F)

# Coeficientes ####
gm<-function(c="MEX",y=2018){
  res<-funciones %>% mutate(lg=dplyr::lag(cre)) %>% 
  filter(code==c,year==y) %>%
  dplyr::select(def:soc,gto2,cre,lg) %>%
  mutate(resto=def+amb+viv+seg+cul) %>%
  dplyr::select(-c(def,amb,viv,seg,cul)) %>% 
  relocate(lg,gto2,soc,edu,eco,sal,resto)
 return(res) 
}
gm()

logob<-function(x) x %>% mutate(across(soc:resto,~log(./gob))) %>% 
  dplyr::select(-gob)
gm("MEX",2018)
logob(gm("MEX",2018))

base.ind$coefficients[[2]]
base.ind$fitted.values[c("2018","2019"),"MEX"]

temp<-base.ind$fitted.values[29:57,"MEX"]


sum(logob(gm("MEX",1992))[1:7]*base.ind$coefficients[[2]])
temp<-funciones %>% mutate(lg=dplyr::lag(cre)) %>% 
  filter(code=="MEX",dplyr::between(year,1991,2019)) %>%
  unite("id",code:year,sep=".") %>%  
  dplyr::select(id,def:soc,gto2,cre,lg) %>%
  mutate(resto=def+amb+viv+seg+cul) %>%
  dplyr::select(-c(def,amb,viv,seg,cul)) %>%
  column_to_rownames("id") %>% 
  relocate(lg,gto2,soc,edu,eco,sal,resto) 

temp$calculos<-(temp %>% logob() %>% as.matrix())[,1:7]  %*%base.ind$coefficients[[2]]

temp$mfitted<-base.ind$fitted.values[29:57,"MEX"]
temp$calres<-temp$cre-temp$calculos
temp$mresid<-base.ind$residuals[["MEX"]][29:57]

# cambios en modelo individual
(logob(gm("MEX",2019)+c(0,0,0,0.5,0,-0.5,0,0,0))[1:7]*base.ind$coefficients[[2]] ) %>% sum()-
(logob(gm("MEX",2019)+c(0,0,0,0,0,0,0,0,0))[1:7]*base.ind$coefficients[[2]] ) %>% sum()

base.ind$coefficients[[2]]["edu"]/3.053979*0.5-
base.ind$coefficients[[2]]["sal"]/2.37477*0.5

# cambios en modelo con indicadoras
(logob(gm("MEX",2019)+c(0,0,0,0.5,0,-0.5,0,0,0))[1:7]*baseic$coefficients[[2]] ) %>% sum()-
  (logob(gm("MEX",2019)+c(0,0,0,0,0,0,0,0,0))[1:7]*baseic$coefficients[[2]] ) %>% sum()

baseic$coefficients[[2]]["edu"]/3.053979*0.5-
  baseic$coefficients[[2]]["sal"]/2.37477*0.5

# Cuadro resumen sustituciones ####
fnames<-c("edu","sal","resto")
gmex19<-gm("MEX",2019)[fnames]
base.ind$coefficients[[2]]

inicial<-gm("MEX",2019)
rebal<-inicial
rebal[fnames[1]]<-gm("MEX",2019)[fnames[1]]+cambios[1]
rebal[fnames[2]]<-gm("MEX",2019)[fnames[2]]-cambios[1]
inicial
rebal

sum(logob(inicial)[1:7]*base.ind$coefficients[[2]])-sum(logob(rebal)[1:7]*base.ind$coefficients[[2]])

cambios<-c(0.1,0.5,1)
reasignaciones<-function(modelo){
temp<-lapply(cambios, function(x){
  res=matrix(0,3,3)
  for(j in 1:3){
    for(k in 1:3){
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

tab<-cbind(reasignaciones(base.ind),reasignaciones(baseic)) %>% as.data.frame()
colnames(tab)<-paste(rep(c("Ind","Ind+Cri"),each=3),rep(fnames,2))
tab$fun<-rep(fnames,3)
tab$delta<-rep(cambios,each=3)

tab<-tab %>% relocate(delta,fun,`Ind edu`)

tab %>% xtable(caption = "Efectos de la reasignación del gasto",label="tab:gsGMM_delta") %>% 
  print(include.rownames=F)
  
# correlacion con dependiente
with(temp %>% filter(model=='l'),cor(cre,fitted)^2)
with(temp %>% filter(model=='d'),cor(cre,fitted)^2)


#Datos con IDH ####
funciones <- consolidado %>% filter(code!="TLS") %>% # quitar pais con pocas observaciones
  mutate(Nivel=ifelse(code=="TWN","Alto",as.character(Nivel))) %>% #completar Taiwan
  mutate(cont=countrycode(code,"genc3c","continent")) %>% #completar continente
  mutate(cont=ifelse(code=="XKS","Europe",cont)) %>% #completar continete para Kosovo
  #mutate(Nivel=ifelse(code=="MEX","México",Nivel)) %>% #México en categoría separada
  mutate(across(c(def:soc),~ifelse(.==0,min(minval$min),.))) %>% #imputación valores pequeños
  merge(idhs %>% dplyr::select(code,year,HDI),all.x = TRUE) %>% 
  dplyr::select(code:soc,gto2,HDI,pibpc) %>% filter(across(code:gto2,~!is.na(.))) %>% nivtrans()
head(funciones)
levels(funciones$Nivel)

#h_t ####
h_dat<-funciones %>% dplyr::select(code,year,Nivel,pibpc,HDI,gto2,def:soc) %>% 
  mutate(resto=seg+def+amb+cul+viv) %>% dplyr::select(-c(seg,def,amb,cul,viv)) %>% #amalgama
  mutate(across(eco:resto,~log(./gob))) %>% dplyr::select(-gob) %>% # logcocientes
  merge(fgrupo %>% dplyr::select(code,year,Grupo:peso)) %>% # variables de grupos
  group_by(code) %>% 
  mutate(gtosq=gto2^2,h=100*(HDI/dplyr::lag(HDI)-1)) %>% filter(year>=1990) %>% # gasto^2, h y filtro año
  filter(!is.na(h)) %>% # sólo datos disponibles
  mutate(d92=ifelse(year==1992,1,0),d93=ifelse(year==1993,1,0),d94=ifelse(year==1994,1,0),
         d08=ifelse(year==2008,1,0),d09=ifelse(year==2009,1,0)) %>% #indicadoras crisis
  pdata.frame()


class(h_dat$h)
h_dat %>% summarise(n=n_distinct(code))

h_dat %>% filter(h==0)
# codeobsres(h_dat)

# OLS, Within, GLS ####
h_mco<-plm(h~gto2+soc+edu+eco+sal+resto,h_dat,model = "pooling") # ols
h_int<-plm(h~gto2+soc+edu+eco+sal+resto,h_dat,model = "within", effect = "individual") # within
h_mcg<-plm(h~gto2+soc+edu+eco+sal+resto,h_dat,model = "random", effect = "individual") # gls


#presencia de efectos
pres<-c(format(pFtest(update(h_int,effect="twoways"),h_mco)$p.value,digits = 2),"","")
#Hausmant test, correlación de efectos
hcor<-c("","",format(phtest(h_int,h_mcg)$p.value,digits = 2))
#AR(1)
ar1<-sapply(list(h_mco,h_int,h_mcg),function(x) format(pbgtest(x)$p.value,digits = 2))
#AR(2)
ar2<-sapply(list(h_mco,h_int,h_mcg),function(x) format(pbgtest(x,order=2)$p.value,digits = 2))

stargazer(h_mco,h_int,h_mcg,digits = 2, #type="text",
          title = "Regresión de panel para desarrollo",
          label = "tab:hmod0",
          column.labels = c("$\\hat{\\und\\gamma}_{\\text{MCO}}$",
                            "$\\hat{\\und\\gamma}_{\\text{W}}$",
                            "$\\hat{\\und\\gamma}_{\\text{MCG}}$"),
          single.row = T,
          dep.var.caption = "Variable dependiente: $h_t$",
          omit.stat = "F",
          dep.var.labels.include = F,
          covariate.labels=c("gto","$\\log$(soc/gob)","$\\log$(edu/gob)",
                             "$\\log$(eco/gob)","$\\log$(sal/gob)",
                             "$\\log$(resto/gob)","ordenada"),
          add.lines = list(c("F (efectos) p-val",pres %>% fsignif()),
                           c("Hausman p-value",hcor%>% fsignif()),
                           c("AR(1) p-value",ar1%>% fsignif()),
                           c("AR(2) p-value",ar2%>% fsignif()))
          )

summary(h_mco)
coeftest(h_mco)
coeftest(h_int)
coeftest(h_mcg)

coeftest(h_int2)

temp<-ranef(h_mcg,effect = "time")
plot(names(temp),temp,type="b")


# h plots ####
# Por nivel de ingreso
temp<-consolidado %>% merge(idhs %>% dplyr::select(code,year,HDI),all.x = TRUE) %>%
  filter(year>=1990,!is.na(HDI)) %>% dplyr::select(code,pais,year,Nivel,cont,HDI) %>% 
  mutate(Nivel=ifelse(code=="MEX","México",as.character(Nivel))) %>% nivtrans()
temp %>% group_by(Nivel) %>% summarise(n=n_distinct(code)) %>% adorn_totals()

temp %>% mutate(cont=ifelse(is.na(cont),countrycode(code,"genc3c","continent"),cont)) %>% 
  group_by(cont,Nivel) %>% summarise(n=n_distinct(code)) %>% adorn_totals()
a<-consolidado %>% merge(idhs %>% dplyr::select(code,year,HDI),all.x = TRUE) %>%
  filter(year>=1990,!is.na(HDI)) %>% dplyr::select(code,year,Nivel,HDI) %>% 
  mutate(Nivel=ifelse(code=="MEX","México",as.character(Nivel))) %>% nivtrans() %>% #México en categoría separada
  group_by(code) %>% mutate(h=100*(HDI/dplyr::lag(HDI)-1)) %>% ungroup %>% #cálculo tasa h
  group_by(Nivel) %>% 
  summarise(g=mean(HDI,na.rm = T),h=mean(h,na.rm = T),N=n_distinct(code),n=n()/N) %>% 
  ggplot(aes(x=reorder(Nivel,h),y=h,fill=Nivel)) +
  geom_bar(stat = "identity")+ #coord_flip()+
  geom_text(aes(label=round(h,2)),nudge_y = .1,angle=90,size=3)+
  theme_bw()+xlab("")+ylab("")+
  coord_cartesian(ylim = c(0,1.5))+
  scale_fill_locuszoom()+
  theme(legend.position = "none",axis.text.x = element_text(angle = 90),
        axis.text.y = element_blank(),axis.ticks=element_blank())

# Por continente
temp<-consolidado %>% merge(idhs %>% dplyr::select(code,year,HDI),all.x = TRUE) %>%
  filter(year>=1990,!is.na(HDI)) %>% 
  mutate(cont=ifelse(is.na(cont),countrycode(code,"genc3c","continent"),cont))
temp %>% group_by(cont) %>% summarise(n=n_distinct(code)) %>% adorn_totals()

b<-consolidado %>% merge(idhs %>% dplyr::select(code,year,HDI),all.x = TRUE) %>%
  filter(year>=1990,!is.na(HDI)) %>% 
  mutate(cont=ifelse(is.na(cont),countrycode(code,"genc3c","continent"),cont)) %>% 
  dplyr::select(code,year,cont,HDI) %>% 
  group_by(code) %>% mutate(h=100*(HDI/dplyr::lag(HDI)-1)) %>% ungroup %>% #cálculo tasa h
  group_by(cont) %>% 
  summarise(HDI=mean(HDI,na.rm = T),N=n_distinct(code),n=n()/N,h=mean(h,na.rm = T)) %>% 
  ggplot(aes(x=reorder(cont,h),y=h,fill=cont)) +
  geom_bar(stat = "identity")+# coord_flip()+
  geom_text(aes(label=round(h,2)),nudge_y = .1,angle=90,size=3)+
  theme_bw()+xlab("")+ylab("")+
  coord_cartesian(ylim = c(0,1.5))+
  scale_fill_aaas()+
  theme(legend.position = "none",axis.text.x = element_text(angle = 90),
        axis.text.y = element_blank(),axis.ticks=element_blank())


# Por grupo gasto público
h_dat %>% summarise(n=n_distinct(code))
c<-h_dat %>% filter(as.numeric(as.character(year))>=1990,!is.na(HDI)) %>% dplyr::select(code,year,Grupo,HDI,h) %>% 
  #mutate(Nivel=ifelse(code=="MEX","México",as.character(Nivel))) %>% nivtrans() %>% #México en categoría separada
  group_by(Grupo) %>% 
  summarise(HDI=mean(HDI,na.rm = T),N=n_distinct(code),n=n()/N,h=mean(h,na.rm = T)) %>% 
  ggplot(aes(x=reorder(paste("Grupo",as.numeric(Grupo)),h),y=h,fill=Grupo)) +
  geom_bar(stat = "identity")+ #coord_flip()+
  geom_text(aes(label=round(h,2)),nudge_y = .1,angle=90,size=3)+
  theme_bw()+xlab("")+ylab("")+
  coord_cartesian(ylim = c(0,1.5))+
  scale_fill_npg()+
  theme(legend.position = "none",axis.text.x = element_text(angle = 90),
        axis.text.y = element_blank(),axis.ticks=element_blank())


library(cowplot)
allplotslist <- align_plots(a,b,c, align = "hv")
library(ggpubr)

ggarrange(allplotslist[[1]],allplotslist[[2]],allplotslist[[3]],ncol=3)
ggsave("./TeX/Fig/442_0_hgrupos.pdf",width = 14,height = 8,units = "cm")

g_dat %>% group_by(Grupo) %>% summarise(across(HDI:resto,mean)) #promedio var por grupo

# variables promedio por grupo
funciones %>% dplyr::select(code,year,Nivel,HDI,gto2,def:soc) %>% 
  mutate(resto=seg+def+amb+cul+viv) %>% dplyr::select(-c(seg,def,amb,cul,viv)) %>% #amalgama
  group_by(code) %>% mutate(h=100*(HDI/dplyr::lag(HDI)-1)) %>% ungroup %>% #cálculo tasa h
  #mutate(across(eco:resto,~log(./gob))) %>% dplyr::select(-gob) %>% # logcocientes
  merge(fgrupo %>% dplyr::select(code,year,Grupo:peso)) %>% # variables de grupos
  mutate(gtosq=gto2^2) %>% filter(year>=1990) %>% # gasto^2 y filtro año
  filter(!is.na(HDI)) %>% # sólo datos disponibles
  pdata.frame() %>% 
  group_by(Grupo) %>% summarise(across(HDI:h,mean,na.rm=T)) %>% 
  xtable(caption = "Gasto y desarrollo de los países por grupos",
         label="hgasto_res",digits=1) %>% print(include.rownames=F)

# Pruebas ####
# I. presencia de efectos


# F test for ind / time effects *
for (i in c("individual","time","twoways")){
  temp<-extp(pFtest(update(h_int,effect=i),h_mco))
  temp$efecto<-i
  if (i=="individual") res<-temp else res<-rbind(res,temp)
}
pruf<-res

# plmtest Lagrange FF Multiplier Tests for Panel Models *
for (i in c("individual","time","twoways")){
  temp<-extp(plmtest(h_mco,effect = i),param = F)
  temp$efecto<-i
  if (i=="individual") res<-temp else res<-rbind(res,temp)
}
prulm<-res


tab<-bind_rows(pruf,prulm) %>% 
  mutate(across(df1:df2,~ifelse(is.na(.),"",formatC(.,format = "f",big.mark = ",",digits = 0)))) %>% 
  mutate(`p-value`=format(`p-value`,format="e",digits = 2)) %>% 
  mutate(Tipo=c("\\multirow{3}{*}{F}","","","\\multirow{3}{*}{Honda}","","")) %>% 
  dplyr::select(Tipo,everything()) %>% 
  mutate(efecto=rep(c("individual","temporal","ambos"),2))
tab
print(xtable(tab %>% dplyr::select(-`Hipótesis alt.`)), include.rownames=FALSE)

# II. Correlación de efectos
# % Hausman: cor(X,eta) *
phtest(h_int,h_mcg)
# H0:RE (mcg) vs H1:FE (int)/ H0 not rejected / testing if cor(e,X)<>0 H0: cor(e,X)=0
fnames<-c("soc","edu","eco","sal","resto","gto2","h")
head(h_dat)
head(h_dat[,fnames])
tab<-sapply(names(h_dat[,fnames]), function (x) cor(fixef(h_int), between(h_dat[,x]))) %>% t()
tab
# temp<-update(h_int,effect="time")
# sapply(names(h_dat[,c(3:7,13,12)]), function (x) cor(fixef(temp,"time"), between(h_dat[,x],"time"))) 
rownames(tab)<-"correlación"
print(xtable(as.data.frame((tab)),
             caption = "Correlación entre $\\und \\eta_i$ y las variables del modelo",
             label="tab:h0coreta"),include.rownames = T)

consolidado %>% merge(idhs %>% dplyr::select(code,year,HDI),all = TRUE) %>% 
  filter(year>=1990,!is.na(HDI)) %>% 
  mutate(Nivel=ifelse(code=="MEX","México",as.character(Nivel))) %>% nivtrans() %>% 
  summarise(n=n_distinct(code))

# Resumen de gasto total y h por nivel con todos los datos
temp<-consolidado %>% 
  merge(idhs %>% dplyr::select(code,year,HDI),all = TRUE) %>% 
  group_by(code) %>% mutate(h=100*(HDI/dplyr::lag(HDI)-1)) %>% ungroup %>% #cálculo tasa h
  filter(year>=1990) %>% dplyr::select(code:cont,gto2,HDI,h) %>% 
  filter(code!="TWN",code!="XWB") %>% #filtrar Taiwán y Palestina por falta de datos
  mutate(pais=countrycode(code,"genc3c","country.name")) %>% #completar nombre de pais
  mutate(Nivel=ifelse(code=="SRB","Medio-alto",as.character(Nivel))) %>% 
  mutate(Nivel=ifelse(code=="MEX","México",as.character(Nivel))) %>% nivtrans() %>% #México en categoría separada
  group_by(Nivel) %>%
  summarise(gto=mean(gto2,na.rm = T),
            HDI=mean(HDI,na.rm = T),h=mean(h,na.rm = T),
            n=n_distinct(code))
temp %>% adorn_totals()

# Tabla resumen por Nivel
funciones %>% dplyr::select(code,year,Nivel,HDI,gto2,def:soc) %>% 
  mutate(resto=seg+def+amb+cul+viv) %>% dplyr::select(-c(seg,def,amb,cul,viv)) %>% #amalgama
  mutate(gtosq=gto2^2) %>% filter(year>=1990) %>% # gasto^2 y filtro año
  mutate(Nivel=ifelse(code=="MEX","México",as.character(Nivel))) %>% nivtrans() %>% #México en categoría separada
  pdata.frame() %>% 
  group_by(Nivel) %>% summarise(across(eco:resto,mean,na.rm=T)) %>% 
  mutate(gto=temp$gto,h=temp$h,IDH=format(temp$HDI,digits=2)) %>% relocate(IDH:gto,.after=Nivel) %>% 
  xtable(caption = "Promedio del crecimiento en IDH y del gasto de los países",
         label="tab:hnivel_res",digits=1) %>% print(include.rownames=F)

# III. Autocorrelación
# % AR: Autocorrelación de los residuales *
pbgtest(h_mco)
pbgtest(h_int)
pbgtest(h_mcg)
#pbltest Baltagi and Li Serial Dependence Test For Random Effects Models
pbltest(h_mcg)

#pdwtest Durbin-Watson Test for Panel Models
pdwtest(h_mco)
pdwtest(h_int)
pdwtest(h_mcg)

tab<-rbind(extp(pbgtest(h_mco),1),
           extp(pbgtest(h_int),1),
           extp(pbgtest(h_mcg),1),
           extp(pbltest(h_mcg),1)) %>% 
  bind_rows(rbind(
    extp(pdwtest(h_mco),0),
    extp(pdwtest(h_int),0),
    extp(pdwtest(h_mcg),0))) %>% 
  mutate(Tipo=c(rep("Breusch-Godfrey",3),"Baltagi-Li",rep("Durbin-Watson",3))) %>% 
  mutate(`p-value`=format(`p-value`,format="e",digits = 2)) %>% 
  mutate(df1=ifelse(is.na(df1),"",format(df1,digits = 0))) %>% 
  mutate(Modelo=c("MCO","W","MCG","MCG","MCO","W","MCG")) %>% 
  dplyr::select(Tipo,everything(),-`Hipótesis alt.`)

tab
print(xtable(tab,digits = 2,
             caption="Pruebas de autocorrelación para $\\und\\nu$",
             label="tab:h0auto"),include.rownames=FALSE)




# h system GMM ####
h_dat %>% ggplot(aes(log(HDI),h))+geom_point()

eqsgmm<-h ~ lag(HDI) + gto2+
  soc+edu+eco+sal+resto |
  lag(h, 2:99) | # exógenas en ef. ind. e idios.
  lag(gto2,2)+lag(soc, 2)+lag(edu,2)+lag(eco,2)+lag(sal,2)+lag(resto,2)

eqsgmm2<-h ~ lag(HDI) + gto2+
  soc+edu+eco+sal+resto+d08+d09 |
  lag(h, 2:99)| # exógenas en ef. ind. e idios.
  lag(gto2,2)+lag(soc, 2)+lag(edu,2)+lag(eco,2)+lag(sal,2)+lag(resto,2) +lag(d08,2)+lag(d09,2)

base <- pgmm(eqsgmm,h_dat,index=c("code", "year"),model="twosteps",
             effect="twoways",transformation = "ld",collapse = TRUE)
base.ind<-pgmm(eqsgmm,h_dat,index=c("code", "year"),model="twosteps",
               effect="individual",transformation = "ld",collapse = TRUE)
baseic<-pgmm(eqsgmm2,h_dat,index=c("code", "year"),model="twosteps",
             effect="individual",transformation = "ld",collapse = TRUE)

sbase<-summary(base)
sbasei<-summary(base.ind)
sbaseic<-summary(baseic)



lmodel<-list(sbasei,sbaseic,sbase)
vsarg<-format(sapply(lmodel, function(x) x$sargan$p.value),digits = 1) 
ar1<-format(sapply(lmodel, function(x) x$m1$p.value),digits = 1)
ar2<-format(sapply(lmodel, function(x) x$m2$p.value),digits = 1)
waldmu<-c("","",format(sbase$wald.td$p.value[[1]],digits = 1))

stargazer(base.ind,baseic,base,# type="text",
          digits = 2,
          title = "sys-GMM para el desarrollo",
          label = "tab:hsGMM",
          column.labels = c("Sin $\\mu$", "Sin $\\mu$ + Ind. crisis","Con $\\mu$"),
          dep.var.caption = "Variable dependiente: $h_t$",
          single.row = T,
          dep.var.labels.include = F,
          # covariate.labels=c("$IDH_{t-1}$","gto","$\\log$(soc/gob)","$\\log$(edu/gob)",
          #                    "$\\log$(eco/gob)","$\\log$(sal/gob)",
          #                    "$\\log$(resto/gob)","Ind. 2008","Ind. 2009"),
          omit.stat = "n",
          add.lines = list(#c("Observaciones",rep("1,296",3)),
            c("Países",rep(64,3)),
            c("Instrumentos",41,43,69),
            c("Sargan p-value",vsarg %>% fsignif()),
            c("AR(1) p-value",ar1 %>% fsignif()),
            c("AR(2) p-value",ar2 %>% fsignif()),
            c("Wald $\\mu_t$ p-value",waldmu %>% fsignif())
          ))

# Coeficientes ####
#Martins coefficients
(0.1*0.0872-0.1^2*0.381)*100 #educación

# Cifras México 
3.03/25.67
(3.03+2.57)/25.67
(3.03+2.57)

hm<-function(c="MEX",y=2018){
  res<-funciones %>% mutate(h=100*(HDI/dplyr::lag(HDI)-1),lH=dplyr::lag(HDI)) %>% 
    filter(code==c,year==y) %>%
    dplyr::select(def:soc,gto2,h,lH) %>%
    mutate(resto=def+amb+viv+seg+cul) %>%
    dplyr::select(-c(def,amb,viv,seg,cul)) %>% 
    relocate(lH,gto2,soc,edu,eco,sal,resto)
  return(res) 
}
hm()

base.ind$fitted.values[26+27,"MEX"]

# cambios en modelo individual
(logob(hm("MEX",2018)+c(0,0,0,2,0,-2,0,0,0))[1:7]*base.ind$coefficients[[2]] ) %>% sum()-
  (logob(hm("MEX",2018)+c(0,0,0,0,0,0,0,0,0))[1:7]*base.ind$coefficients[[2]] ) %>% sum()

base.ind$coefficients[[2]]["edu"]/3.026974*2-
  base.ind$coefficients[[2]]["sal"]/2.399094*2

# cambios en modelo con indicadoras
(logob(gm("MEX",2019)+c(0,0,0,0.5,0,-0.5,0,0,0))[1:7]*baseic$coefficients[[2]] ) %>% sum()-
  (logob(gm("MEX",2019)+c(0,0,0,0,0,0,0,0,0))[1:7]*baseic$coefficients[[2]] ) %>% sum()


# Gráficas auxiliares ####
# efectos temporales
tiempo<-base$coefficients[[2]][9:34]
plot(as.numeric(names(tiempo)),tiempo,type="b")
tiempo<-data.frame(year=names(tiempo),mu=tiempo)

tiempo %>% ggplot(aes(x=as.numeric(year),y=mu))+geom_point()+
  geom_smooth(formula = 'y ~ poly(x,3)',method = 'glm' )+
  theme_bw()+
  xlab('Año')+ylab('')
ggsave("./TeX/Fig/442_1_temporal.pdf",width = 12,height = 6,units = "cm")

coef(base)

# h dat
h_dat %>% ggplot(aes(x=1990+as.numeric(year),y=h))+geom_point()

# fitted values
ajust<-baseic$fitted.values %>% as.data.frame() %>% 
  mutate(year=names(base$residuals[[1]]),
         model=c(rep('d',26),rep('l',27))) %>% 
  pivot_longer(!c(year,model),names_to='code',values_to='fitted') %>% 
  mutate(fitted=na_if(fitted,0)) %>% na.omit()
ajust %>% ggplot(aes(x=as.numeric(year),y=fitted))+geom_point()
# errores ####

lsfun<-list("media"=mean,#"dev"=sd,
            "plo"=function(x,na.rm=T) quantile(x,probs = 0.25,na.rm),
            "pup"=function(x,na.rm=T) quantile(x,probs = 0.75,na.rm))


modelos<-list("(3) Temp"=base,"(1) Indiv"=base.ind,"(2) Indiv+crisis"=baseic)

for (i in 1:length(modelos)) {
  errores<-modelos[[i]]$residuals %>% as.data.frame() %>%
    mutate(year=names(base$residuals[[1]]),
           model=c(rep('d',26),rep('l',27))) %>% 
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
ggsave("./TeX/Fig/442_2_errores.pdf",width = 12,height = 5,units = "cm",bg="transparent")


summary(base)
coeftest(base)
