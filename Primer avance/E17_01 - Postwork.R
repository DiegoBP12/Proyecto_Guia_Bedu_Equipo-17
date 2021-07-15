#####  Equipo 17  ##### 
# - Diego Bolaños Pardo
# - Luis Roberto Silva Reyes
# - Rafael Sebastián Carrillo Rivera
# - Ana Yessica Espinosa Ávila
# - Salvador Velázquez Moreno
# - Diana Alejandra Pérez Sánchez


#------------------------------------- POSTWORK 1-----------------------------------------------------------------
setwd("C:/Users/Thinkpad/Documents/BEDU/")
dir()
liga19 <- read.csv('SP3.csv')
library(dplyr)
dim(liga19)
#Obteniendo información de dataframe
str(liga19)
head(liga19); tail(liga19)
#Columnas que contienen los números de goles anotados por los equipos 
#que jugaron en casa (FTHG) y los goles anotados por los equipos que jugaron como visitante (FTAG)
(goles.casa19 <- liga19$FTHG)
(goles.visitante19 <- liga19$FTAG)
#Elabora tablas de frecuencias relativas para estimar las siguientes probabilidades:
#La probabilidad (marginal) de que el equipo que juega en casa anote x goles (x = 0, 1, 2, ...)
#La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y = 0, 1, 2, ...)
#La probabilidad (conjunta) de que el equipo que juega en casa anote x goles y el equipo que juega como visitante anote y goles (x = 0, 1, 2, ..., y = 0, 1, 2, ...)
casa.abs19 <- data.frame(table(goles.casa19))
visita.abs19 <- data.frame(table(goles.visitante19))
ambos.abs <- data.frame(table(goles.casa19, goles.visitante19))
(prob.casa19 <- mutate(casa.abs19, freq.relativa = Freq / sum(Freq)))
(prob.visitante19 <- mutate(visita.abs19, freq.relativa = Freq / sum(Freq)))
(prob.ambos19 <- mutate(ambos.abs, freq.relativa = Freq / sum(Freq)))

#-------------------------------------- POSTWORK 2 ---------------------------------------------------------------
# Importa los datos de soccer de las temporadas 2017/2018, 2018/2019 y 2019/2020
liga17 <- read.csv('SP1.csv')
liga18 <- read.csv('SP2.csv')
liga19 <- read.csv('SP3.csv')
# Revisa la estructura de de los data frames al usar las funciones: str, head, View y summary
str(liga17); str(liga18); str(liga19)
head(liga17); head(liga18); head(liga19)
View(liga17); View(liga18); View(liga19)
summary(liga17); summary(liga18); summary(liga19)
# Con la función select del paquete dplyr selecciona únicamente las columnas Date, HomeTeam, AwayTeam, FTHG, FTAG y FTR; 
# esto para cada uno de los data frames.
library(dplyr)
liga17 = select(liga17, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
liga18 = select(liga18, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
liga19 = select(liga19, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
# Asegúrate de que los elementos de las columnas correspondientes de los nuevos data frames sean del mismo tipo.
# Con ayuda de la función rbind forma un único data frame que 
# contenga las seis columnas mencionadas en el punto 3.
liga17 = mutate(liga17, Date = as.Date(Date, "%d/%m/%y"))
liga18 = mutate(liga18, Date = as.Date(Date, "%d/%m/%Y"))
liga19 = mutate(liga19, Date = as.Date(Date, "%d/%m/%Y"))
temporadas <- rbind(liga17, liga18, liga19)

#----------------------- POSTWORK 3 --------------------------------------------------------------------------------
#Cargamos el dataFrame del Prework 2
temporadas

#Calculamos las probabilidades (estimadas) maginales y conjuntas para el DataFrame con las 3 temporadas
(goles.casa <- temporadas$FTHG)
(goles.visitante <- temporadas$FTAG)
#Elaboramos tablas de frecuencias relativas
casa.abs <- data.frame(table(goles.casa))
visita.abs <- data.frame(table(goles.visitante))
ambos.abs <- data.frame(table(goles.casa, goles.visitante))
(prob.casa <- mutate(casa.abs, freq.relativa = Freq / sum(Freq)*100))
(prob.visitante <- mutate(visita.abs, freq.relativa = Freq / sum(Freq)*100))
(prob.ambos <- mutate(ambos.abs, freq.relativa = Freq / sum(Freq)*100))

#Graficamos (con ggplot2)
library(ggplot2)
library(plotly)
#GrÃ¡fica de probabilidad de que note equipo de casa
c4 = c("A", "B", "C")
df = cbind(df, c4)
grafico.casa <- ggplot(data = prob.casa, aes(x = goles.casa, y = freq.relativa))+
  geom_bar(stat = "identity", position = "stack", fill = "steelblue") +
  ggtitle("Probabilidad de que anote el equipo de casa") + 
  ylab("Frecuencia (%)") + 
  xlab("Goles")
ggplotly(grafico.casa)
#GrÃ¡fica de probabilidad de que note equipo visitante
grafico.visitante <- ggplot(data = prob.visitante, aes(x = goles.visitante, y = freq.relativa))+
  geom_bar(stat = "identity", position = "stack", fill = "steelblue") + 
  ggtitle("Probabilidad de que anote el equipo visitante") + 
  ylab("Frecuencia (%)") + 
  xlab("Goles")
ggplotly(grafico.visitante)
#GrÃ¡fica de calor con ambos con geom_title
grafico.ambos <- ggplot(prob.ambos, aes(x = goles.casa, y = goles.visitante, fill = freq.relativa)) + geom_tile() + 
  ggtitle("Probabilidades conjuntas estimadas de goles") + 
  ylab("Goles de equipo visitante") + 
  xlab("Goles de equipo de casa") 
grafico.ambos <- grafico.ambos + guides(fill=guide_legend(title="Recuencia relativa"))
ggplotly(grafico.ambos)


#----------------------- POSTWORK 4 --------------------------------------------------------------------------------
library(dplyr)
#Volvemos a obtener los elementos utilizados en el Postwork 2:
temp19_20 <- read.csv("SP1-2019-2020.csv")
goles.casa <- temp19_20$FTHG
goles.visitante <- temp19_20$FTAG
casa.abs <- data.frame(table(goles.casa))
visita.abs <- data.frame(table(goles.visitante))
ambos.abs <- data.frame(table(goles.casa, goles.visitante))
ambos.abs <- filter(ambos.abs,Freq>0)
#se cambió el nombre de las columnas freq.relativa por p.casa,p.visitante,p.conjunta
(prob.casa <- mutate(casa.abs, p.casa = Freq / sum(Freq)))
(prob.visitante <- mutate(visita.abs, p.visitante = Freq / sum(Freq)))
(prob.ambos <- mutate(ambos.abs, p.conjunta = Freq / sum(Freq)))
#Se seleccionan solo las columnas goles.casa, goles.visitante y  freq.relativa y se almacenan
(prob.casa<-select(prob.casa,goles.casa,p.casa))
(prob.visitante<-select(prob.visitante,goles.visitante,p.visitante))
(prob.ambos<-select(prob.ambos,goles.casa,goles.visitante,p.conjunta))
#Obtén una tabla de cocientes al dividir estas probabilidades 
#conjuntas por el producto de las probabilidades marginales correspondientes.
(cocientes<-merge(prob.ambos,merge(prob.casa,prob.visitante)))
(cocientes<-mutate(cocientes,cociente=p.conjunta/(p.casa*p.visitante)))

hist(cocientes$cociente, breaks = seq(0,5,0.1))

#Mediante un procedimiento de boostrap, obtén más cocientes similares a los 
#obtenidos en la tabla del punto anterior
#Utilizamos el paquete "rsample" para poder utilizar la función bootstrap
#install.packages("rsample")
library(rsample)    
#se crean 100 muestras con reemplazo del dataframe cocientes
set.seed(13)
bootstrap<-bootstraps(cocientes,100)
cocientes_muestreados<-vector()
data <- data.frame()

for(i in 1:100){
  current_bootstrap<-as.data.frame(bootstrap$splits[[i]])
  cocientes_muestreados<-c(cocientes_muestreados,current_bootstrap$cociente)
  data <-(rbind(data,current_bootstrap))
}
hist(cocientes_muestreados, breaks = seq(0,5,0.1))

###
library(ggplot2)
g <- ggplot(data, aes(cociente)) +
  geom_histogram(colour = 'green', 
                 fill = 'orange',
                 alpha = 0.7) +
  ggtitle("Histograma de cocientes muestreados") +
  xlab("Cociente") +
  ylab("Frecuencia") +
  theme_light()
library(plotly)
ggplotly(g)

#Menciona en cuáles casos le parece razonable suponer que los cocientes de la tabla en el
#punto 1, son iguales a 1 (en tal caso tendríamos independencia de las variables aleatorias X y Y
#debido a que son variables indep. si la probabilidad conjunta = producto de las marginales).
#Nos parece razonable suponer que los cocientes de la tabla 1 son 1 cuando
#la probabilidad conjunta = producto de las marginales.