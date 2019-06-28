#Bootcamp Cientista de Dados - Professor Fernando Amaral
library(shiny)
library(forecast)
library(ggplot2)

ui <- fluidPage(
    fluidRow( class ="text-center",
             column(width=6,offset = 3, titlePanel("Previsão de Series Temporais usando ARIMA"))
             #helpText(strong(("https://www.linkedin.com/in/silviocesarlima")))
    ),
    # fluidRow( class ="text-center",
    #           column(width=6,offset = 3, helpText(strong("https://www.linkedin.com/in/silviocesarlima")))
    #           ),
    fluidRow( class ="text-center",
              column(width=6,offset = 3, tags$a(href="https://www.linkedin.com/in/silviocesarlima", class="btn btn-default", "Linkedin - Silvio Lima")),
              #h5("Linkedin", a("Link", https://www.linkedin.com/in/silviocesarlima)))
              #tags$a(href="https://www.linkedin.com/in/silviocesarlima", class="btn btn-default", "https://www.linkedin.com/in/silviocesarlima"),
              #div(style = htmltools::css(height = "5000px")),
              uiOutput("try")),
    hr(),
    fluidRow(
        column(6,
               helpText(strong(h4("PETR4.SA"))),
               helpText(strong("Fechamentos mensais")),
               helpText(strong("Periodo dos dados coletados: 01/2017 a 05/2019"))
        ),
        
        column(6,
               sliderInput("PeridoPrevisao", "Informe quantos meses quer prever:", 5, min = 1, max = 48),
               #sliderInput("sexatas", "Gosto por Exatas",min = 5, max = 90,step = 10,value = 40)),
               actionButton("Processar","Processar")
        )
    ),
    hr(),
    fluidRow(
        column(12,
               plotOutput("GrafSerie"))
    ),
    fluidRow(
        column(12,
               plotOutput("GrafDec"))
    ),
    hr(),
    fluidRow(
        column(6, offset=,
               helpText(align="center",strong("PREVISÃO e INTERVALO DE CONFIANÇA")),
               plotOutput("GrafPrev")
        ),
        column(2,
               h1(textOutput("llower")),
               tableOutput("lower")
               
        ),
        column(2,
               h1(textOutput("lmean")),
               tableOutput("mean")
               
        ),
        column(2,
               h1(textOutput("lupper")),
               tableOutput("upper")
        )
    )
)

server <- function(input, output) {
    #evento do botao para processar
    observeEvent(input$Processar,
                 {
                     #objeto de leitura do arquivo
                     data =  read.csv("Petro-Mes2.csv", header = TRUE)
                     
                     #transforma o arquivo importanto em uma serie temporal
                     Valores =ts(data$Close, start = c(2017,01 ), end = c(2019, 05), frequency = 12)
                     
                     #rotinas de impressao da serie
                     output$GrafSerie = renderPlot({autoplot(Valores, main=" Gráfico da Série no periodo")})
                     dec =   decompose(Valores)
                     output$GrafDec <- renderPlot({autoplot(dec, main = "Decomposição: Dados, Sazonal, Tendência e Ruido")})
                     
                     #cria o modelo usando arima
                     fit <- auto.arima(Valores)
                     #varifica o per?odo escolhido
                     valr =  input$PeridoPrevisao
                     #faz a previsao
                     previsao = forecast(fit,h=valr)
                     
                     #dados da previsao
                     output$lower <- renderTable({previsao$lower})
                     output$mean <- renderTable({previsao$mean })
                     output$upper <- renderTable({previsao$upper })
                     
                     output$llower = renderText({"Lower"})
                     output$lupper = renderText({"Upper"})
                     output$lmean = renderText({"Mean"})
                     
                     #grafico da previsao
                     output$GrafPrev <- renderPlot({autoplot(previsao, main="Possível comportamento futuro")})
                     
                 })
    
}

shinyApp(ui = ui, server = server)
