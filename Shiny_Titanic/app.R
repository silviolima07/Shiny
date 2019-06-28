#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(randomForest)
library(shiny)


# Define UI for application that draws a histogram
ui <- fluidPage(
    fluidRow( class ="text-center",
              column(width=6,offset = 3, titlePanel("TITANIC"))
    ),
    fluidRow( class ="text-center",
              column(width=6,offset = 3, titlePanel("Classificação dos passageiros no acidente usando Random Forest"))
              #helpText(strong(("https://www.linkedin.com/in/silviocesarlima")))
    ),
    fluidRow( 
        column(6,
              class ="text-center",
              column(width=6,offset = 3, tags$a(href="https://rsilvio.shinyapps.io/Titanic_dash/", class="btn btn-default", "Gráficos dos atributos")),
              #h5("Linkedin", a("Link", https://www.linkedin.com/in/silviocesarlima)))
              #tags$a(href="https://www.linkedin.com/in/silviocesarlima", class="btn btn-default", "https://www.linkedin.com/in/silviocesarlima"),
              #div(style = htmltools::css(height = "5000px")),
              uiOutput("try1")
              ),
        column(6,
              class ="text-center",
              column(width=6,offset = 3, tags$a(href="https://www.linkedin.com/in/silviocesarlima", class="btn btn-default", "Linkedin - Silvio Lima")),
              #h5("Linkedin", a("Link", https://www.linkedin.com/in/silviocesarlima)))
              #tags$a(href="https://www.linkedin.com/in/silviocesarlima", class="btn btn-default", "https://www.linkedin.com/in/silviocesarlima"),
              #div(style = htmltools::css(height = "5000px")),
              uiOutput("try2")
              )),
    fluidRow(
        column(6,
               br(),
               tags$b("Dataset: Titanic.csv"),
               tags$p("891 observações e 12 atributos.Apenas 5 atributos foram utilizados."),
               textOutput("modelo"),
               textOutput("modelo1"),
               br(),
               tags$b("Selecione os atributos e teste."),
               tags$p("Confira se o passageiro sobreviveu ou não, de acordo com o algoritmo treinado.")
        ),
        column(6,
               br(),
               tags$b("Atributos"),
               textOutput("atributo1"),
               textOutput("atributo2"),
               textOutput("atributo3"),
               textOutput("atributo4"),
               textOutput("atributo5")
        )
        ),
    fluidRow(
        column(4,
               radioButtons("OpcaoSex", label = "Selecione o sexo",
                            choices = list("Homem" = 1, "Mulher" = 2), 
                            selected = 1)
        ),
        column(4,
               radioButtons("OpcaoClass", label = "Selecione a classe do bilhete",
                            choices = list("Primeira " = 1, "Segunda" = 2, "Terceira"= 3), 
                            selected = 1)
        ),
        column(4,
               radioButtons("OpcaoEmb", label = "Selecione onde embarcou",
                            choices = list("Cherboug" = "Cherboug", "Queenstown" = "Queenstown","Southampton"="Southampton"), 
                            selected = "Cherboug")
        )
    ),
    fluidRow(
        column(6,
               sliderInput("OpcaoAge", "Selecione a idade:", 5, min = 10, max = 80),
               actionButton("Processar","Processar")),
        column(6,
               sliderInput("OpcaoFare", "Valor pago pela passagem:", 50, min = 1, max = 600)
        )
    ),
    fluidRow(
        column(width=5,offset=2,
               h2(textOutput("Perfil")),
               h4(textOutput("Sexo")),
               h4(textOutput("Classe")),
               h4(textOutput("Idade")),
               h4(textOutput("Passagem")),
               h4(textOutput("Embarque")),
               br()
        ),
        column(width=3, offest=5,
               h2(textOutput("Resultado")),
               br()
        ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    library(caret)
    #setwd("C:/Users/silvi/Desktop/Shiny/Estudo/Shiny_Titanic")
    dados_treino<- read.csv("dataset_ok.csv",header = TRUE)
    #status<-table(dados_treino$Status)
    embarque<-table(dados_treino$Embarque)
    sexo<-table(dados_treino$Sexo)
    #idade<-table(dados_treino$Idade)
    #passagem<-table(dados_treino$Passagem)
    #dados_treino=select(dados_treino,Status,Classe,Sexo,Idade,Passagem,Embarque)
    # Divisao em treino e teste
    #index <- createDataPartition(dados_treino$Status, p = 0.7, list = FALSE)
    #train_data <- dados_treino[index, ]
    #test_data  <- dados_treino[-index, ]
    load("titanic_model_rf.rda") # model_rf_smote
    
    output$modelo<-renderText({"O dataset foi treinado com o algoritmo RANDOM FOREST."})
    output$modelo1<-renderText({"A matriz de confusão indicou uma precisão de 82%."})
    
    output$embarque <- renderTable({embarque},rownames = TRUE,Colnames=FALSE)
    
    output$sexo <- renderTable({sexo},rownames = TRUE,Colnames=FALSE)
    
    output$atributo1<-renderText({"Sexo: Homem ou Mulher"})
    output$atributo2<-renderText({"Classes: 1-Primeira 2-Segunda 3-Terceira"})
    output$atributo3<-renderText({"Idade: 1 a 80"})
    output$atributo4<-renderText({"Embarque: Cherboug Queenstown Southampton"})
    output$atributo5<-renderText({"Passagem (US$): 1 a 600"})
    
    output$distPlotAge <- renderPlot({
        hist(dados_treino$Idade, main = "Histograma de idades", col= 'red',xlab = "Idade",border= 'white')
    })
    output$distPlotClass <- renderPlot({
        hist(dados_treino$Classe, main = "Histograma de Classes",xlab="Classe",col='green')
    })
    output$distPlotFare <- renderPlot({
        hist(dados_treino$Passagem, main = "Histograma de Valor Pago",xlab="Passagem",col='blue')
    })
    output$distPlotEmb <- renderPlot({
        ggplot(dados_treino, aes(x=factor(Embarque)))+
            labs(x="Porto de embarque", y = "count")+
            geom_bar(stat="count", width=0.7, fill="steelblue")+
            theme_minimal()
    })
    output$distPlotSex <- renderPlot({
        ggplot(dados_treino, aes(x=factor(Sexo)))+
            labs(x="Sexo", y = "count")+
            geom_bar(stat="count", width=0.7, fill="steelblue")+
            theme_minimal()
    })
    
    #evento do botao para executar a predição
    observeEvent(input$Processar, {
        Sexo =  eval(parse(text=input$OpcaoSex))
        if (Sexo == 1)
            Sexo = 'homem'
        if (Sexo == 2)
            Sexo = 'mulher'
        Classe = eval(parse(text=input$OpcaoClass))
        Embarque =  input$OpcaoEmb
        Idade =  eval(parse(text=input$OpcaoAge))
        Passagem =  eval(parse(text=input$OpcaoFare))
        newdf=data.frame(Sexo,Classe,Idade,Passagem,Embarque) #Sexo+Classe+Idade+Passagem+Embarque
        prev<- predict(model_rf_smote, newdata = newdf)
        prev = paste0("Previsão: ",prev)
        output$Perfil<-renderText({"Perfil do passageiro"})
        perfil_sexo<-paste0("Sexo: ",Sexo)
        output$Sexo = renderText({perfil_sexo})
        perfil_classe<-paste("Classe do bilhete: ",Classe)
        output$Classe = renderText({perfil_classe})
        perfil_embarque<-paste0("Porto de Embarque: ",Embarque)
        output$Embarque = renderText({perfil_embarque})
        perfil_idade<-paste("Idade: ", Idade)
        output$Idade = renderText({perfil_idade})
        perfil_passagem<-paste("Valor da Passagem (US$): ", Passagem)
        output$Passagem = renderText({perfil_passagem})
        output$Resultado = renderText({prev})
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
