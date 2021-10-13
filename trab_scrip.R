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

## Weibull
mwe <- survreg(Surv((tempo+0.01),cens)~1, data=dados, dist = "weibull") # dÃ¡ erro se nÃ£o somar 0.01
summary(mwe)

(alphawe<-exp(mwe$coefficients[1]))
(gamawe<-1/mwe$scale)

pwe<-2
n<-nrow(dados)

AICwe<-(-2*mwe$loglik[1])+(2*pwe)
AICcwe<-AICwe + ((2*pwe*(pwe+1))/(n-pwe-1))
BICwe<-(-2*mwe$loglik[1]) + pwe*log(n)
(medidaswe<-cbind(AICwe,AICcwe,BICwe))

## Exponencial
mexp <- survreg(Surv((tempo+0.01),cens)~1, data=dados, dist = "exponential") 
summary(mexp)

(alphaexp<-exp(mexp$coefficients[1]))

pexp<-1
n<-nrow(dados)

AICexp<-(-2*mexp$loglik[1])+(2*pexp)
AICcexp<-AICexp + ((2*pexp*(pexp+1))/(n-pexp-1))
BICexp<-(-2*mexp$loglik[1]) + pexp*log(n)
(medidasexp<-cbind(AICexp,AICcexp,BICexp))

## Log-Normal
mlnorm <- survreg(Surv((tempo+0.01),cens)~1, data=dados, dist = "lognorm") 
summary(mlnorm)

(mi<-mlnorm$coefficients[1])
(sigma<-mlnorm$scale)

plnorm<-2

AIClnorm<-(-2*mlnorm$loglik[1])+(2*plnorm)
AICclnorm<-AIClnorm + ((2*plnorm*(plnorm+1))/(n-plnorm-1))
BIClnorm<-(-2*mlnorm$loglik[1]) + plnorm*log(n)
(medidaslnorm<-cbind(AIClnorm,AICclnorm,BIClnorm))

## Log-Logística
mll<- survreg(Surv((tempo+0.01),cens)~1, data=dados, dist = "loglogistic")
summary(mll)

(alphall<-exp(mll$coefficients[1]))
(gamall<-1/mll$scale)

pll<-2
AICllog<-(-2*mll$loglik[1])+(2*pll)
AICcllog<-AICllog + ((2*pll*(pll+1))/(n-pll-1))
BICllog<-(-2*mll$loglik[1]) + pll*log(n)
(medidasllog<-cbind(AICllog,AICcllog,BICllog))

## Weibull Exponencializada
tempo <- dados$tempo
censura <- dados$cens

veroWE <- function(theta, tempo, censura){
  gama <- theta[1]; alfa <- theta[2]; a <- theta[3]
  f <- (gama/(alfa^gama))*(tempo^(gama-1))*exp(-((tempo/alfa)^gama))*a*(1-exp(-((tempo/alfa)^gama)))^(a-1)
  s <- 1-(1-exp(-(tempo/alfa)^gama))^a
  ln <- sum((censura)*log(f)+(1-censura)*log(s))
  return(-ln) ## optim minimiza
}


(v1 <- optim(c(1,1,1), veroWE, tempo = dados$tempo + 0.01, 
             censura = dados$cens, hessian = T))

v1$convergence

(gammaWE<-v1$par[1])
(alphaWE<-v1$par[2])
(a<-v1$par[3])


## Verificando ajuste com FunÃ§Ã£o de sobrevivÃªncia
sWE<-1-(1-exp(-(time/alphaWE)^gammaWE))^a

## AIC, AICc e BIC
pWE <- 3
n <- 48
AICWE<- -2*(-v1$value)+2*pWE
AICcWE<-AICWE+(2*pWE*(pWE+1))/(n-pWE-1)
BICWE <- -2*(-v1$value)+pWE*log(n)
(medidasWexp<-cbind(AICWE,AICcWE,BICWE))

medidasexp
medidasWexp
medidaswe
medidaslnorm
medidasllog

### VERIFICANDO O AJUSTE
#KM
time<-KM$time
skm<-KM$surv ##sobrev de km

swe<-exp(-(time/alphawe)^gamawe) ##sobrev da weibull
sexp<-exp(-(time/alphaexp)) ## sobrev da exponencial
slognorm<-pnorm((-log(time)+mi)/sigma) ##sobrev da log-normal
sloglogi<-1/(1+(time/alphall)^gamall) ##sobrev da log-logística
sWE<-1-(1-exp(-(time/alphaWE)^gammaWE))^a ##sobrev da weibull exponencializada

plot(KM,conf.int=F, xlab="Tempo", ylab="S(t)",mark.time = T)
lines(c(0,time),c(1,swe),lty=2,col=2)
lines(c(0,time),c(1,sexp),lty=2,col=3)
lines(c(0,time),c(1,slognorm),lty=2,col=4)
lines(c(0,time),c(1,sloglogi),lty=2,col=5)
lines(c(0,time),c(1,sWE),lty=2,col=6)
legend(11,1,lty=c(1,2,3,4,5,6),col=c(1,2,3,4,5,6),c("Kaplan-Meier","Weibull",
                                                          "Exponencial","Log-Normal",
                                                          "Log-Logística","Weibull Exponencializada"),bty="n",cex=0.8)
