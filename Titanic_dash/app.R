#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
#setwd("C:/Users/silvi/Desktop/Shiny/Estudo/Titanic")
dados_treino<- read.csv("dataset_ok.csv",header = TRUE)
#status<-table(dados_treino$Status)
#embarque<-table(dados_treino$Embarque)
#sexo<-table(dados_treino$Sexo)

#dados_treino=select(dados_treino,Status,Classe,Sexo,Idade,Passagem,Embarque)

# Divisao em treino e teste
#index <- createDataPartition(dados_treino$Status, p = 0.7, list = FALSE)
#train_data <- dados_treino[index, ]
#test_data  <- dados_treino[-index, ]

# Random Forest
# ctrl <- trainControl(method = "repeatedcv", 
#                      number = 5, 
#                      repeats = 5, 
#                      verboseIter = FALSE,
#                      sampling = "smote")
# set.seed(42)
# model_rf_smote <- caret::train(Status ~ .,         
#                                data = train_data,
#                                method = "rf",
#                                preProcess = c("scale", "center"),
#                                trControl = ctrl)
# modelo_RF<-model_rf_smote

#load("titanic_model_rf.rda") # model_rf_smote


# final_smote <- data.frame(atual = test_data$Classes,
#                           predict(model_rf_smote, newdata = test_data, type = "prob"))
# 
# final_smote$predict <- as.factor(ifelse(final_smote$Morreu > 0.5, "Morreu", "Sobreviveu"))
# 
# cm_smote <- confusionMatrix(final_smote$predict, test_data$Classes)
# cm_smote


# Define UI for application that draws a histogram
ui <- fluidPage(
    fluidRow( class ="text-center",
              column(width=6,offset = 3, titlePanel("TITANIC"))
              #helpText(strong(("https://www.linkedin.com/in/silviocesarlima")))
    ),
    fluidRow( class ="text-center",
              column(width=6,offset = 3, titlePanel("Shiny - Gráficos dos principais atributos"))
              #helpText(strong(("https://www.linkedin.com/in/silviocesarlima")))
    ),
    fluidRow( class ="text-center",
              column(width=6,offset = 3, tags$a(href="https://www.linkedin.com/in/silviocesarlima", class="btn btn-default", "Linkedin - Silvio Lima")),
               #h5("Linkedin", a("Link", https://www.linkedin.com/in/silviocesarlima)))
              #tags$a(href="https://www.linkedin.com/in/silviocesarlima", class="btn btn-default", "https://www.linkedin.com/in/silviocesarlima"),
              #div(style = htmltools::css(height = "5000px")),
              uiOutput("try")),
              hr(),

    fluidRow(
        column(7,
               tags$b("Dataset: Titanic.csv"),
               tags$p("891 observações e 12 atributos.Apenas 5 atributos foram utilizados."),
               tags$p("Atributos: Embarque, Sexo, Idade, Classe, Passagem"),
               br(),
               tags$b("Gráficos")
               ),
        column(5,
                tags$b("Atributos"),
                textOutput("atributo1"),
                textOutput("atributo2"),
                textOutput("atributo3"),
                textOutput("atributo4"),
                textOutput("atributo5")
         )
        ),
    fluidRow(
        column(6,
               hr(),
               tags$b("Total de passageiros por cidade de embarque"),
               #tableOutput('embarque')),
               plotOutput("distPlotEmb")),
        column(6,
               hr(),
               tags$b("Total de passageiros por sexo"),
               plotOutput("distPlotSex"))
    ),
    fluidRow(
        column(4,
               hr(),
               plotOutput("distPlotAge")
        ),
        column(4,
               hr(),
               plotOutput("distPlotClass")
        ),
        column(4,
               hr(),
               plotOutput("distPlotFare")
        ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$modelo<-renderText({"O dataset foi treinado com o algoritmo Random Forest."})
    output$modelo2<-renderText({"A matriz de confusão indicou uma precisão de 82%. Que pode ser melhorada com certeza."})

    output$atributo1<-renderText({"Embarque: Cherboug Queenstown Southampton"})
    output$atributo2<-renderText({"Sexo: Homem ou Mulher"})
    output$atributo3<-renderText({"Idade: 1 a 80"})
    output$atributo4<-renderText({"Classes: 1-Primeira 2-Segunda 3-Terceira"})
    output$atributo5<-renderText({"Passagem (US$): 1 a 600"})
    
    #output$try <- renderUI("hello")
    
    output$distPlotAge <- renderPlot({
        hist(dados_treino$Idade, main = "Histograma de idades", col= 'red',xlab = "Idade",border= 'white')
    })
    output$distPlotClass <- renderPlot({
        hist(dados_treino$Classe, main = "Histograma de Classes",xlab="Classe",col='green')
    })
    output$distPlotFare <- renderPlot({
        hist(dados_treino$Passagem, main = "Histograma de Valor da Passagem ",xlab="Passagem",col='blue')
    })
    output$distPlotEmb <- renderPlot({
        ggplot(dados_treino, aes(x=factor(Embarque)))+
            labs(x="Embarque", y = "count")+
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
