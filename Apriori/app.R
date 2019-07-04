#
library(shiny)
library(arules)
library(arulesViz)

ui <- fluidPage(

  fluidRow( class ="text-center",
            column(width=6,offset = 3, style = "font-size: 25pt; line-height: 40pt; width = 100",titlePanel("Regras de Associação Algoritmo Apriori"))
  ),
  fluidRow( class ="text-center",
           column(width=6,offset = 3, tags$a(href="http://fbarth.net.br/materiais/cursoMineracaoDados/regrasAssociacao.pdf", class="btn btn-default", tags$strong("Teoria")),
            uiOutput("try"))),
  br(),
  fluidRow( class ="text-center",
            column(width=6,offset = 3, tags$a(href="https://www.linkedin.com/in/silviocesarlima", class="btn btn-default", tags$strong("Linkedin - Silvio Lima"))),
            uiOutput("try2")),
  hr(),
  fluidRow(  class ="text-center",
             column(width=12, style = "font-size: 15pt; line-height: 40pt; width = 100",tags$strong("INFOSIGA"),br(),
                    tags$strong("Sistema de Informações Gerenciais de Acidentes de Trânsito do Estado de São Paulo"),
                    helpText("Periodo: Jan/2019 a Maio/2019"))
  ),
  
  fluidRow(
    column(4,
           br(),
           class ="text-center",
           column(width=6,offset = 3,sliderInput("sup", "Valor de Suporte:", 0.1, min = 0.05, max = 0.9)
           )
    ),
    column(4,
           br(),
           class ="text-center",
           column(width=6,offset = 3,sliderInput("item", "Quantidade minima de itens:", 3, min = 2, max = 6)
           )
    ),
    column(4,
           br(),
           class ="text-center",
           column(width=6,offset = 1, sliderInput("conf", "Valor de Confiança:", 0.1, min = 0.05, max = 0.9))
    )
  ),
  fluidRow(
    column(12,
           class ="text-center",
           column(width=12,actionButton("Processar",tags$strong("Gerar Regras")),
                  br()
           ))),

  fluidRow(
           column(12,
                  br(),
                  class="text-center",
                  column(width=12,htmlOutput("msg3"),br()))
    ),
  fluidRow(
    column(4,

           plotOutput("Graf1")
    ),
    column(4,
           plotOutput("Graf2")
    ),
    column(4,

           plotOutput("Graf3")
    )
  ),
  fluidRow(
    column(12,
           br(),
           class="text-center",
           column(width=12,textOutput("msg"),htmlOutput("msg1"),htmlOutput("msg2"),br()))
  ),
  fluidRow(
    column(12,
  tableOutput("regras"))
    )
)

server <- function(input, output) {
#   #evento do botao para executar o calculo
  transacoes <-read.transactions(file="ItemList_app.csv",format="basket",sep=",")
  
    observeEvent(input$Processar, {

    basket <- apriori(transacoes, parameter= list(supp=input$sup, conf=input$conf, minlen=input$item))

    #convert to datframe

    df_basket <- as(basket,"data.frame")
    df_basket$confidence <- df_basket$confidence
    df_basket$support <- df_basket$support
    
    msg0<-paste0("")
    
    if(length(df_basket) > 0){
       
      output$Graf1 = renderPlot({  plot(basket, method="graph", control=list(type="items"))   })
      output$Graf2 = renderPlot({  plot(basket, method="matrix", control=list(type="items"))  })
      output$Graf3 = renderPlot({  plot(basket, method="matrix3D", measure="lift")  })
      
                       output$msg1 <- renderUI({
                         tags$div(
                           HTML('<p style="color:black; font-size: 13.2pt">TOP 20 REGRAS</p>'))})
                       output$msg2 <- renderUI({
                         tags$div(
                           HTML('<p style="color:red; font-size: 13.2pt">Quanto maior valor de lift<br>Maior será a influência de A em B para uma regra (A => B)</p>'))})
                       output$msg3 = renderText({msg0})
                       output$msg = renderText({msg0})
                       df_basket <- df_basket[order(df_basket$lift,decreasing=TRUE),]
                       df_basket<-head(df_basket,n=20)
                       output$regras <- renderTable({df_basket})
                     }
                     if(length(df_basket) == 0){
                       output$Graf1 = renderPlot({})
                       output$Graf2 = renderPlot({})
                       output$Graf3 = renderPlot({})
                       
                       output$msg3 <- renderUI({
                         tags$div(
                           HTML('<p style="color:red; font-size: 13.2pt"> Zero regras geradas - Valores muito altos<br>Use valores menores</p>'))})
                       output$msg1 = renderText({msg0})
                       output$msg2 = renderText({msg0})
                       output$regras <- renderTable({})
                     }
                     output$freq <- renderUI({})
                     output$summary<-renderTable({})

                   })
}
  

shinyApp(ui = ui, server = server)

