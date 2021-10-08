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

