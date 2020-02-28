library("readxl")
POL<-read_excel("C:/Users/32033_matheus/Desktop/Trabalho_Séries_Temporais/basepoluicao.xls")
attach(POL)

POL<-ts(POL, start = 1, frequency = 1)

ts.plot(POL)

acf(POL, lag.max = 40)
pacf(POL, lag.max = 40)

library(tseries)
adf.test(POL)
#O modelo n?o ? estacion?rio, pois temos mais de 2 lags significativos

#Calculando a m?dia
media<-vector()

j<-1

vet<-POL
for(i in 1:132){
  media[i]<-(vet[j]+vet[j+1])/2
  j<-j+2
}

media

#Calculando o desvio padr?o
desvio <- vector()

z<-1
for(i in 1:132){
  desvio[i]<-sd(c(vet[z],vet[z+1]))
  z<-z+2
}

desvio

plot(media,desvio)

#Como a vari?ncia n?o depende da m?dia, temos um modelo aditivo

###TESTE DO SINAL

require(randtests)

metade1<-vet[1:182]
metade2<-vet[183:364]

length(metade1)
length(metade2)

positivo <- 0
negativo <- 0

for(i in 1:182){
  if(metade1[i]<metade2[i]){
    positivo<-positivo+1
  }
  else{
    negativo<-negativo+1
  }
}


positivo

negativo

quantil<-qnorm(0.975,182*0.5,sqrt(182*0.5*0.5))
quantil

valor_comparar<-96+86-quantil
valor_comparar

#Como o n?mero de valores positivos deu maior que o valor a comparar, rejeitamos a hip?tese nula de n?o exist?ncia de tend?ncia

###TESTE DE FISHER PARA SAZONALIDADE
library(GeneCycle)
fisher.g.test(POL)

#Hip?tese nula: N?o existe sazonalidade
#Hip?tese alternativa: Existe sazonalidade

#Como o p-value foi menor que 5%, rejeitamos a hip?tese nula de que n?o existe sazonalidade na s?rie.

periodogram(ATM)

#Retirando a tend?ncia da s?rie

diffPOL<-diff(POL)
ts.plot(diffPOL)

#Fun??o de autocorrela??o e autocorrela??o parcial

acf(diffPOL, lag.max = 40, plot = FALSE) 
pacf(diffPOL, lag.max = 40, plot = FALSE)

acf(diffPOL, lag.max = 40) #qtd de lags iniciais nos d? uma ideia do valor q 
pacf(diffPOL, lag.max = 40) #qtd de lags iniciais nos d? uma ideia do valor p

#q = 2
#p = 1/2
#d = 1, pois fizemos apenas uma diferen?a para retirar a tend?ncia

#Ajustando o modelo automaticamente

library(forecast)
auto.arima(POL)

#ARIMA(1,1,0) 

modelo1<-arima(POL, order = c(1,1,0)) 
t1<-0.0562/0.0523
dt(t1, length(POL)) 

#O modelo n?o pode ser considerado, pois o ?nico par?metro n?o foi significativo.

#ARIMA(2,1,0) 

modelo2<-arima(POL, order = c(2,1,0))
t1<-0.0781/0.0486
t2<--0.3746/0.0485
dt(t1, length(POL)) 
dt(t2, length(POL)) 

#O modelo n?o pode ser considerado, pois um dos par?metros n?o deu significativo.

#ARIMA(0,1,2) 

modelo3<-arima(POL, order = c(0,1,2)) 
t1<--0.2161/0.0588
t2<--0.5556/0.0615
dt(t1, length(POL))
dt(t2, length(POL)) 

#O modelo pode ser considerado, pois os dois par?metros deram significativos.

#ARIMA(1,1,2) 

modelo4<-arima(POL, order = c(1,1,2)) 
t1<-0.4113/0.0692
t2<--0.4975/0.0640
t3<--0.4334/0.0554
dt(t1, length(POL))
dt(t2, length(POL)) 
dt(t3, length(POL)) 

#O modelo pode ser considerado, pois todos os 3 par?metros deram significativos.

#ARIMA(2,1,2) 

modelo5<-arima(POL, order = c(2,1,2)) 
t1<-0.4864/0.124
t2<--0.0743/0.0999
t3<--0.5645/0.1156
t4<--3643/0.1126
dt(t1, length(POL))
dt(t2, length(POL)) 
dt(t3, length(POL)) 
dt(t4, length(POL)) 

#O modelo n?o pode ser considerado, pois um dos par?metros n?o foi significativo.

#AIC MODELO 3 - 3100.21
#AIC MODELO 4 - 3074.56

ts.plot(modelo4$residuals)
hist(modelo4$residuals)
acf(modelo4$residuals, lag.max = 40) #Mais do que 2 lags significativos j? nao ? ru?do branco
pacf(modelo4$residuals, lag.max = 40) #Mais do que 2 lags significativos j? nao ? ru?do branco
Box.test(modelo4$residuals, type=c("Box-Pierce"))
cpgram(modelo4$residuals)
