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

#------------------------------------- POSTWORK 2 ---------------------------------------------------------------
# Importa los datos de soccer de las temporadas 2017/2018, 2018/2019 y 2019/2020
liga17 <- read.csv('SP1.csv')
liga18 <- read.csv('SP2.csv')
# Revisa la estructura de de los data frames al usar las funciones: str, head, View y summary
str(liga17); str(liga18); str(liga19)
head(liga17); head(liga18); head(liga19)
View(liga17); View(liga18); View(liga19)
summary(liga17); summary(liga18); summary(liga19)
# Con la función select del paquete dplyr selecciona únicamente las columnas Date, HomeTeam, AwayTeam, FTHG, FTAG y FTR; 
# esto para cada uno de los data frames.
library(dplyr)
liga17 <- select(liga17, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
liga18 <- select(liga18, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
liga19 <- select(liga19, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
# Asegúrate de que los elementos de las columnas correspondientes de los nuevos data frames sean del mismo tipo.
# Con ayuda de la función rbind forma un único data frame que 
# contenga las seis columnas mencionadas en el punto 3.
liga17 <- mutate(liga17, Date = as.Date(Date, "%d/%m/%y"))
liga18 <- mutate(liga18, Date = as.Date(Date, "%d/%m/%Y"))
liga19 <- mutate(liga19, Date = as.Date(Date, "%d/%m/%Y"))
temporadas <- rbind(liga17, liga18, liga19)

#------------------------------------- POSTWORK 3 --------------------------------------------------------------------------------
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
#Gráfica de probabilidad de que note equipo de casa
c4 = c("A", "B", "C")
df = cbind(df, c4)
grafico.casa <- ggplot(data = prob.casa, aes(x = goles.casa, y = freq.relativa))+
  geom_bar(stat = "identity", position = "stack", fill = "steelblue") +
  ggtitle("Probabilidad de que anote el equipo de casa") + 
  ylab("Frecuencia (%)") + 
  xlab("Goles")
ggplotly(grafico.casa)
#Gráfica de probabilidad de que note equipo visitante
grafico.visitante <- ggplot(data = prob.visitante, aes(x = goles.visitante, y = freq.relativa))+
  geom_bar(stat = "identity", position = "stack", fill = "steelblue") + 
  ggtitle("Probabilidad de que anote el equipo visitante") + 
  ylab("Frecuencia (%)") + 
  xlab("Goles")
ggplotly(grafico.visitante)
#Gráfica de calor con ambos con geom_title
grafico.ambos <- ggplot(prob.ambos, aes(x = goles.casa, y = goles.visitante, fill = freq.relativa)) + geom_tile() + 
  ggtitle("Probabilidades conjuntas estimadas de goles") + 
  ylab("Goles de equipo visitante") + 
  xlab("Goles de equipo de casa") 
grafico.ambos <- grafico.ambos + guides(fill=guide_legend(title="Recuencia relativa"))
ggplotly(grafico.ambos)


#------------------------------------- POSTWORK 4 --------------------------------------------------------------------------------
library(dplyr)
#Llamamos los elementos utilizados en el Postwork 2:
liga19 <- read.csv('SP3.csv')
(goles.casa19 <- liga19$FTHG)
(goles.visitante19 <- liga19$FTAG)
#Elabora tablas de frecuencias relativas para estimar las siguientes probabilidades:
#La probabilidad (marginal) de que el equipo que juega en casa anote x goles (x = 0, 1, 2, ...)
#La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y = 0, 1, 2, ...)
#La probabilidad (conjunta) de que el equipo que juega en casa anote x goles y el equipo que juega como visitante anote y goles (x = 0, 1, 2, ..., y = 0, 1, 2, ...)
casa.abs19 <- data.frame(table(goles.casa19))
visita.abs19 <- data.frame(table(goles.visitante19))
ambos.abs19 <- data.frame(table(goles.casa19, goles.visitante19))
(prob.casa19 <- mutate(casa.abs19, freq.relativa = Freq / sum(Freq)))
(prob.visitante19 <- mutate(visita.abs19, freq.relativa = Freq / sum(Freq)))
(prob.ambos19 <- mutate(ambos.abs, freq.relativa = Freq / sum(Freq)))
prob.ambos19 <-  filter(prob.casa19, Freq > 0)
#Se cambió el nombre de las columnas freq.relativa por p.casa,p.visitante,p.conjunta
(prob.casa19 <- mutate(casa.abs19, p.casa = Freq / sum(Freq)))
(prob.visitante19 <- mutate(visita.abs19, p.visitante = Freq / sum(Freq)))
(prob.ambos19 <- mutate(ambos.abs19, p.conjunta = Freq / sum(Freq)))
#Se seleccionan solo las columnas goles.casa, goles.visitante y  freq.relativa y se almacenan
(prob.casa19 <- select(prob.casa19,goles.casa19,p.casa))
(prob.visitante19 <- select(prob.visitante19,goles.visitante19,p.visitante))
(prob.ambos19 <- select(prob.ambos19,goles.casa19,goles.visitante19,p.conjunta))
#Obtén una tabla de cocientes al dividir estas probabilidades 
#conjuntas por el producto de las probabilidades marginales correspondientes.
(cocientes<-merge(prob.ambos19, merge(prob.casa19, prob.visitante19)))
(cocientes<-mutate(cocientes, cociente = p.conjunta/(p.casa*p.visitante)))

hist(cocientes$cociente, breaks = seq(0,5,0.1))

#Mediante un procedimiento de boostrap, obtén més cocientes similares a los 
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


#------------------------------------- POSTWORK 5 --------------------------------------------------------------------------------
library(dplyr)
library(fbRanks)
write.csv(temporadas, "temporada.csv", row.names = F)
lista <- lapply(dir(getwd(),"temporada*"), read.csv)
lista <- lapply(lista, select, c(date=Date, home.team=HomeTeam, home.score=FTHG, away.team=AwayTeam, away.score=FTAG))
data <- do.call(rbind, lista)
data <- mutate(data, date = as.Date(date, "%Y-%m-%d"))
write.csv(data,"soccer.csv",row.names = F)

listasoccer <- create.fbRanks.dataframes("soccer.csv")
anotaciones <- listasoccer$scores
equipos <- listasoccer$teams

fecha <- unique(data$date)
n <- length(fecha)
ranking <- rank.teams(anotaciones, equipos, max.date = fecha[n-1], min.date = fecha[1])

predic <- predict(ranking, date=fecha[n])

#------------------------------------- POSTWORK 6 --------------------------------------------------------------------------------

match <- read.csv("match.data.csv")
library(dplyr)
library(ggplot2)
dim(match)
#Agrega una nueva columna sumagoles que contenga la suma de goles por partido.
match$sumagoles <- match$home.score + match$away.score
#Obtén el promedio por mes de la suma de goles.
match <- mutate(match, date = as.Date(date, "%Y-%m-%d"))
match.sum <- data.frame(mes = format(match$date,'%Y-%m'), "sumagoles" = match$sumagoles)
match.mes <- aggregate(. ~ mes, match.sum, mean)

#Crea la serie de tiempo del promedio por mes de la suma de goles hasta diciembre de 2019.
match.ts <- ts(match.mes$sumagoles, st = c(2010, 8), end = c(2019, 12), fr = 12)
match.ts

#Grafica la serie de tiempo.
plot(match.ts, xlab = "Tiempo", ylab = "Promedio", main = "Serie de promedio por mes de la suma de goles",
     sub = "Serie mensual: Agosto de 2010 a Diciembre de 2019")

#------------------------------------- POSTWORK 7 --------------------------------------------------------------------------------
#Instalación de los paquetes de mongo 
install.packages("mongolite", dependencies = TRUE)
library(mongolite)
#Conexión con la base de datos
bd <- mongo(
  collection = "match",
  db = "match_games",
  url = "mongodb+srv://diegobp:bedu12@cluster0.jexdm.mongodb.net/test?authSource=admin&replicaSet=atlas-nn1rg3-shard-0&readPreference=primary&appname=MongoDB%20Compass&ssl=true",
  verbose = FALSE
)
#Count para conocer la cantidad de registros
bd$count()
# Se buscan los goles del Real Madrid del 20 de Diciembre del 2015
partidoMadrid <- bd$find(
  query ='{"date": "2015-12-20", "home.team": "Real Madrid"}',
  fields = '{"_id":false}'
)# Jugó contra el Vallecano y si fue goleada de 10 - 2
# Se cierra conexión con la bd
rm(bd)

#------------------------------------- POSTWORK 8 --------------------------------------------------------------------------------
library(shiny)
library(dplyr)
library(ggplot2)
data_datatable <- rename(read.csv('match.data.csv'), Fecha = date, Local = home.team, Marcador_Local = home.score, Visitante = away.team, Marcador_Visitante = away.score)
# Al elegir...
# Marcador_Local:
# Se desplegará una gráfica por cada equipo de La Liga, donde se mostrará la frecuencia de cada número de goles
# que les han anotado siendo visitantes.
#
# Marcador_Visitante:
# Se desplegará una gráfica por cada equipo de La Liga, donde se mostrará la frecuencia de cada número de goles
# que han anotado siendo visitantes.
# Define UI for application that draws a histogram
ui <- pageWithSidebar(
  
  headerPanel("Dashboard Postwork 8"),
  sidebarPanel(
    
    # selectInput("tipo", "Seleccione el valor de X", choices = c("Marcador_Local", "Marcador_Visitante"))
    selectInput("tipo", "Seleccione el tipo de marcador", choices = c("Marcador_Local", "Marcador_Visitante"))
  ),
  mainPanel(
    
    tabsetPanel(
      tabPanel("Gráficas de barras - Equipos Visitantes",   #Pestaña de Plots
               h3(textOutput("output_text")), 
               plotOutput("output_plot"), 
      ),
      tabPanel("Postwork 3",
               fluidRow(
                 column(12,
                        img( src = "post3-1.png", 
                             height = 450, width = 450)
                 ),
                 column(12,
                        img( src = "post3-2.png", 
                             height = 450, width = 450)
                 ),
                 column(12,
                        img( src = "post3-3.png", 
                             height = 450, width = 450)
                 )
               )
      ), 
      
      tabPanel("Data Table", dataTableOutput("datamatch_table")),
      
      tabPanel("Factores de ganancia",
               fluidRow(
                 column(12,
                        img( src = "rplot1.png", 
                             height = 450, width = 450)
                 ),
                 column(12,
                        img( src = "rplot2.png", 
                             height = 450, width = 450)
                 )
               )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$output_text <- renderText(input$tipo)
  
  # output$output_plot <- renderPlot({ ggplot(data_datatable, aes_string(input$tipo)) + geom_bar() })
  
  output$output_plot <- renderPlot({ 
    ggplot(data_datatable, aes_string(input$tipo)) + 
      geom_bar() +
      facet_wrap(vars(data_datatable[,4]), scales = 'free') +
      labs(y = "# Goles anotados")
  })
  
  output$datamatch_table <- renderDataTable(data_datatable, 
                                            options = list(aLengthMenu = c(10,25,50),
                                                           iDisplayLength = 10))
}

# Run the application 
shinyApp(ui = ui, server = server)
