library(e1071);library(GGally);library(patchwork);library(survival)
dados <-read.csv2("C:/Users/Filipe.Ribeiro/Documents/Sobrevivencia/antibully.csv")

head(dados)
str(dados)


###### Descritiva
######




#####################################################democratas e republicanos 
tabela <- matrix(c(sum(dados$rep),sum(dados$dem)),1,2,
                 dimnames = list("Frequência",c("Republicanos","Democratas")))
tabela


################################################## diferença percentual entre votos reps e dems 

summary(dados$pol_compet)
hist(dados$pol_compet)
var(dados$pol_compet)
skewness(dados$pol_compet) ## Assimetrica a direita 
kurtosis(dados$pol_compet) ## leptocúrtica

######################################################## mensuração da ideologia cidadoes 

summary(dados$citiz_ideol)
var(dados$citiz_ideol)
hist(dados$citiz_ideol)
skewness(dados$citiz_ideol) # a direita 
kurtosis(dados$citiz_ideol) ## platicurtica 

####################################################### mensuração da ideologia governo 
summary(dados$gov_ideol)
var(dados$gov_ideol)
hist(dados$gov_ideol)
skewness(dados$gov_ideol) # a esquerda 
kurtosis(dados$gov_ideol) # platicurtica 



################################################ mensuração do proffisionalistmo legislativo ? 

summary(dados$leg_profes)
var(dados$leg_profes)
hist(dados$leg_profes)
skewness(dados$leg_profes) # a direita 
kurtosis(dados$leg_profes) # leptocúrtica 

################################# Pop  em cada estado no ano da adoçao ou no ultimo ano contado 

summary(dados$pop)
var(dados$pop)
hist(dados$pop)
skewness(dados$pop) # a direita 
kurtosis(dados$pop) # leptocúrtica 




################################### Correlações entre as vari numec 
dad_numec <- dados[,7:11]

cor(dad_numec)

ggcorr(dad_numec,label = TRUE) ## arredondado 2 casas decimais 

## leg_profes com pop bem alto 0.8 (é o esperado ? )
## ideologia dos cidadoes e ideologia do governo ( é o esperado ?)
dados$rep <- as.factor(dados$rep)
ggpairs(dados,columns = 6:11)
### comparando os ggplots de rep com as variaveis quase todos 
##  aparentam ter relaçao com o partido do estado 
g1 <- ggplot(dados,aes(x = rep , y = pol_compet))+
  geom_boxplot()+
  theme_bw()

g2 <- g1 <- ggplot(dados,aes(x = rep , y = citiz_ideol))+
  geom_boxplot()+
  theme_bw()

g3 <- g1 <- ggplot(dados,aes(x = rep , y = gov_ideol))+
  geom_boxplot()+
  theme_bw()

g4 <- g1 <- ggplot(dados,aes(x = rep , y =leg_profes))+
  geom_boxplot()+
  theme_bw()

g5 <- g1 <- ggplot(dados,aes(x = rep , y = pop))+
  geom_boxplot()+
  theme_bw()

g1+g2+g3+g4+g5




#####  

##### Analise de sobrevivencia 
#####

KM<-survfit(Surv(dados$tempo,dados$cens)~1,conf.int=F)
summary(KM)
plot(KM,conf.int=F, xlab="Tempo", ylab="S(t)",mark.time = T)

## pelo partido 
KMg<-survfit(Surv(dados$tempo,dados$cens)~dados$rep, conf.int=F)
summary(KMg)
plot(KMg,conf.int=F, xlab="Tempo", ylab="S(t)",lty=c(1,2),mark.time = T)
legend(10,1,lty=c(1,2),c("Democratas","Republicanos"))

## 

#Funcao risco acumulada KM#
sobkm=KM$surv
HHt=-log(sobkm) 

plot(stepfun(KM$time,c(0,HHt[-])),do.points = F,xlab="Tempo", 
     ylab="H(t)",main = "")

## funçao risco acumulada nelson alen

KM$cumhaz   #a funÃ§Ã£o risco acumulada calculada Ã© de Nelson Aalen

hj<-KM$n.event/(KM$n.risk)
testeHtNA<-cumsum(hj)  #funÃ§Ã£o risco acumulada calculada Ã© de Nelson Aalen

plot(KM,conf.int=F, fun="cumhaz", xlab="Tempo", ylab="H(t)") #a funÃ§Ã£o risco acumulada calculada Ã© de Nelson Aalen

#Nelson-Aalen#
ENA<-survfit(coxph(Surv(dados$tempo,dados$cens)~1, method="breslow"))
summary(ENA)
plot(ENA, conf.int=F,xlab="Tempo", ylab="S(t)",mark.time = T)

plot(ENA, conf.int=F,xlab="Tempo",fun="cumhaz", ylab="H(t)")

## Curva TTT
TTT(KM$time)
TTT(dados$tempo,col="red")

#### ANÁLISE DAS COVARIÁVEIS
## pelo partido 
KMg<-survfit(Surv(dados$tempo,dados$cens)~dados$rep, conf.int=F)
summary(KMg)
plot(KMg,conf.int=F,col=c("blue","red"), xlab="Tempo", ylab="S(t)",lty=c(1,2),mark.time = T)
legend("topright",lty=c(1,2),c("Democratas","Republicanos"),col=c("blue","red"))

par(mfrow=c(1,2))
TTT(dados$tempo[dados$dem == 0],col="red")
TTT(dados$tempo[dados$dem == 1],col="blue")

dados$Dr <- ifelse(dados$dem==1,"Democrata","Republicano")
survdiff(Surv(tempo,cens)~Dr, data = dados, rho= 1)

## pol_compet
summary(dados$pol_compet)
dados$polcompet <-ifelse(dados$pol_compet<=9.181,'<=9,18%','>9,18%')

KMpolcompet<-survfit(Surv(tempo,cens)~polcompet, data = dados,conf.int=F)
summary(KMpolcompet)
plot(KMpolcompet,lty = 1:2,conf.int=F,xlab="Tempo",ylab="S(t)",
     mark.time=T)
legend("topright", legend=c("Menor ou igual a 9,18%", "Maior que 9,18%"),
      lty=1:2, cex=0.8)

par(mfrow=c(1,2))
TTT(dados$tempo[dados$pol_compet<=9.181])
TTT(dados$tempo[dados$pol_compet>9.181])

survdiff(Surv(tempo,cens)~polcompet, data = dados, rho= 0)

## citiz_ideol
summary(dados$citiz_ideol)

dados$ideol <-ifelse(dados$citiz_ideol<=50.08,'<=50,08','>50,08')

KMideol<-survfit(Surv(tempo,cens)~ideol, data = dados,conf.int=F)
summary(KMideol)
plot(KMideol,lty = 1:2,conf.int=F,xlab="Tempo",ylab="S(t)",
     mark.time=T)
legend("topright", legend=c("Menor ou igual a 50,08", "Maior que 50,08"),
       lty=1:2, cex=0.8)

par(mfrow=c(1,2))
TTT(dados$tempo[dados$citiz_ideol<=50.08])
TTT(dados$tempo[dados$citiz_ideol>50.08])

survdiff(Surv(tempo,cens)~ideol, data = dados, rho= 1)

## gov_ideol
summary(dados$gov_ideol)

dados$ideolgov <-ifelse(dados$gov_ideol<=48.54,'<=48,54','>48,54')

KMideolgov<-survfit(Surv(tempo,cens)~ideolgov, data = dados,conf.int=F)
summary(KMideolgov)
plot(KMideolgov,lty = 1:2,conf.int=F,xlab="Tempo",ylab="S(t)",
     mark.time=T)
legend("topright", legend=c("Menor ou igual a 48,54", "Maior que 48,54"),
       lty=1:2, cex=0.8)

par(mfrow=c(1,2))
TTT(dados$tempo[dados$gov_ideol<=50.08])
TTT(dados$tempo[dados$gov_ideol>50.08])

survdiff(Surv(tempo,cens)~ideolgov, data = dados, rho= 1) ## fiquei na duvida nesse (suposição de proporcionalidade das curvas)

## leg_profes
summary(dados$leg_profes)

dados$legprof <-ifelse(dados$leg_profes<=0.154,'<=15,4%','>15,4%')

KMlegprof<-survfit(Surv(tempo,cens)~legprof, data = dados,conf.int=F)
summary(KMlegprof)
plot(KMlegprof,lty = 1:2,conf.int=F,xlab="Tempo",ylab="S(t)",
     mark.time=T)
legend("topright", legend=c("Menor ou igual a 15,4%", "Maior que 15,4%"),
       lty=1:2, cex=0.8)

par(mfrow=c(1,2))
TTT(dados$tempo[dados$leg_profes<=0.154])
TTT(dados$tempo[dados$leg_profes>0.154])

survdiff(Surv(tempo,cens)~legprof, data = dados, rho= 1)

## pop
summary(dados$pop)

dados$ppop <-ifelse(dados$pop<=4391767,'<=4.391.767',
                    '>4.391.767')

KMpop<-survfit(Surv(tempo,cens)~ppop, data = dados,conf.int=F)
summary(KMpop)
plot(KMpop,lty = 1:2,conf.int=F,xlab="Tempo",ylab="S(t)",
     mark.time=T)
legend("topright", legend=c("População menor ou igual a 4.391.767",
                            "Popualção maior que 4.391.767"),
       lty=1:2, cex=0.8)

par(mfrow=c(1,2))
TTT(dados$tempo[dados$pop<=4391767])
TTT(dados$tempo[dados$pop>4391767])

survdiff(Surv(tempo,cens)~ppop, data = dados, rho= 1)
