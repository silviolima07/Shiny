#
library(shiny)
library(GA)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("slate"),
    
    
    fluidRow( class ="text-center",
              column(width=6,offset = 3, style = "font-size: 25pt; line-height: 40pt; width = 100",titlePanel("ALGORITMOS GENÉTICOS"),titlePanel("OTIMIZAÇÃO"))
    ),
    fluidRow( class ="text-center",
              column(width=6,offset = 3, tags$a(href="https://www.linkedin.com/in/silviocesarlima", class="btn btn-default", tags$strong("Linkedin - Silvio Lima"))),
              uiOutput("try2")),
    br(),
    fluidRow(  class ="text-center",
               column(width=12, style = "font-size: 15pt; line-height: 40pt; width = 100",tags$strong("MALA DE MÃO"),br(),
                      tags$a(href="http://www.anac.gov.br/assuntos/passageiros/bagagens", class="btn btn-default", tags$strong("ANAC")),
                      #tags$strong("ANAC - Agência Nacional de Aviação Civil"),
                      htmlOutput("anac")
               )),
    fluidRow(
        column(width=12, align="center",
               img(src="Mala.png"),
               br(),
               br()
        )),
 fluidRow(  class ="text-center",
            column(width=12, style = "font-size: 15pt; line-height: 40pt; width = 100",tags$strong("O que vc levaria na mala ?"),
                   htmlOutput("lembrete")
            )),
 fluidRow(
     column(4,
            br(),
            class ="text-center",
            column(width=6,offset = 3,sliderInput("item1", "Camisa (250):", 1, min = 0, max = 10)),
            column(width=6,offset = 3,sliderInput("item2", "Camiseta (150):", 1, min = 0, max = 10)),
            column(width=6,offset = 3,sliderInput("item3", "Jeans (800):", 1, min = 0, max = 10)),
            column(width=6,offset = 3,sliderInput("item4", "Moleton (600):", 1, min = 0, max = 10)),
            column(width=6,offset = 3,sliderInput("item5", "Blusao (400):", 1, min = 0, max = 10)),
            column(width=6,offset = 3,sliderInput("item6", "Livro (400):", 1, min = 0, max = 10))
     ),
     column(4,
            br(),
            class ="text-center",
            column(width=6,offset = 3,sliderInput("item7", "Toalha de banho (500):", 1, min = 0, max = 10)),
            column(width=6,offset = 3,sliderInput("item8", "Tenis (200):", 1, min = 0, max = 10)),
            column(width=6,offset = 3,sliderInput("item9", "Meia (30):", 1, min = 0, max = 10)),
            column(width=6,offset = 3,sliderInput("item10", "Pijama (400):", 1, min = 0, max = 10)),
            column(width=6,offset = 3,sliderInput("item11", "Bermuda (400):", 1, min = 0, max = 10)),
            column(width=6,offset = 3,sliderInput("item12", "Revista (400):", 1, min = 0, max = 10))
     ),
     column(4,
            br(),
            class ="text-center",
            column(width=6,offset = 3,sliderInput("item13", "Chinelo (200):", 1, min = 0, max = 10)),
            column(width=6,offset = 3,sliderInput("item14", "Sapato (300):", 1, min = 0, max = 10)),
            column(width=6,offset = 3,sliderInput("item15", "Roupa intima (50):", 1, min = 0, max = 10)),
            column(width=6,offset = 3,sliderInput("item16", "Necessaire (200):", 1, min = 0, max = 10)),
            column(width=6,offset = 3,sliderInput("item17", "Medicamentos (150):", 1, min = 0, max = 10)),
            column(width=6,offset = 3,sliderInput("item18", "Presente (400):", 1, min = 0, max = 10))
     )
 ),
 fluidRow(
     column(12,
            class ="text-center",
            column(width=12,actionButton("Processar",tags$strong("Processar")),
            br(),br(),
            htmlOutput("calculo")
            ))
     ),
 fluidRow(
     column(6,
            align="center",
            style="display: block; margin-left: auto; margin-right: auto",
            br(),
            htmlOutput("calculo3"))
            
     ),
  fluidRow(
     column(6,
            align="center",
            style="display: block; margin-left: auto; margin-right: auto",
            br(),
            htmlOutput("itens_selecionados"),
            br(),
            tableOutput("Rfinal")

     ),
     column(6,
            br(),
            br(),
            br(),
            #class ="text-center",
            h3(textOutput("RQuantidadeIni")),
            h3(textOutput("RQuantidadeFinal")),
            h3(textOutput("RPesototal")),
            br(),
            htmlOutput("teoria")

     )
 )
)

server <- function(input, output) {
    
    itens <<-  read.csv("lista.csv", header=TRUE)
    z <<- nrow(itens)
    
        output$anac <- renderUI({
        tags$div(
            HTML('<p style="color:white; font-size: 12.5pt">Segundo a <b>ANAC - Agência Nacional de Aviação Civil</b>, a franquia de bagagem de mão é de, no mínimo, <b>10 Kg</b>, viagem <b>nacional</b>.<br>
           O passageiro tem direito de levar com ele na cabine da aeronave uma mala de mão até 10 kg sem 
             qualquer custo extra.</p>'))})
    
    output$lembrete <- renderUI({
        tags$div(
            HTML('<p style="color:white; font-size: 13.5pt"><b>Escolha o que levaria e quantidade (0 a 10).<br>
                 Item (Peso em gramas)</p>'))})
    
    
    observeEvent(input$Processar, {
    
        # Add new colum with number of itens definided from slider above
        
        output$lembrete <- renderUI({})
        
        itens$Qtd<<-NA # itens[i,4]
        itens$PesoFinal<<-NA # itens[i,5]
        
        itens$Qtd<-c(input$item1,input$item2,input$item3,input$item4,input$item5,
                     input$item6,input$item7,input$item8,input$item9,input$item10,
                     input$item11,input$item12,input$item13,input$item14,input$item15,
                     input$item16,input$item17,input$item18)
        
        for ( i in 1:z){
            itens[i,5] = itens[i,3] * itens[i,4] # Peso * Qtd
        }
    
        maxpeso = 10000
        
        output$calculo <- renderUI({
          tags$div(
            HTML('<p style="color:red; font-size: 12.5pt;text-align=justify">Calculado novo conjunto de itens</p>'))})
        Sys.sleep(0.25)
        
        f <-function(x)
        {
          peso = 0
          
          for (i in 1:z)
          {
            if (itens[i,4] != 0){
              if (x[ i ] != 0 )
              {
                itens[i,5] = itens[i,3] * itens[i,4]
                peso = peso + itens[i,5]
              }
            }
          }
          if ( peso > maxpeso )
            peso = 0
          return(peso)
        }
        
        # Verificar se o peso dos itens com qtde maior zero somados passa de 10kg
        
        if ( sum(itens$PesoFinal) > maxpeso)
          {
        
            resultado = ga("binary", fitness = f, nBits = z,popSize = 10, maxiter = 100)
            result = t(as.data.frame( summary(resultado)$solution))
            
            output$itens_selecionados <- renderUI({})
        
            output$itens_selecionados <- renderUI({
            tags$div(
                HTML('<p style="color:white; font-size: 13.5pt"> <b>Itens Selecionados pelo ALGORITMO</b></p>'))})
        
            result = itens[result[,1]==1,]
            result = result[result[,4] > 0,]
        
            output$Rfinal <- renderTable({result})
        
            output$RQuantidadeIni = renderText({    paste0("Quantidade Inicial: ", nrow(itens)  )})
            output$RQuantidadeFinal = renderText({  paste0("Quantidade Final: ", sum(result$Qtd))})
            pesoKg=sum(result$PesoFinal/1000)
            output$RPesototal = renderText({  paste0("Peso Total: ", pesoKg," Kg")})
        }
        else {
          
          output$itens_selecionados <- renderUI({})
          
          output$itens_selecionados <- renderUI({
            tags$div(
              HTML('<p style="color:white; font-size: 13.5pt"> <b>Itens Selecionados pelo USUÁRIO</b></p>'))})
      
          result = itens[itens[,4] > 0,]
          
          output$Rfinal <- renderTable({result})
          
          output$RQuantidadeIni = renderText({    paste0("Quantidade Inicial: ", nrow(itens)  )})
          output$RQuantidadeFinal = renderText({  paste0("Quantidade Final: ", sum(result$Qtd))})
          pesoKg=sum(result$PesoFinal)/1000
          output$RPesototal = renderText({  paste0("Peso Total: ", pesoKg," Kg")})
             
        }
        output$teoria <- renderUI({
          tags$div(
            HTML('<p style="color:white; font-size: 12.5pt;text-align=justify">Se o peso de todos itens somados nao ultrapassar 10 kg, a lista terá os itens que o usuário selecionou para levar.<br>
            Caso ultrapasse o valor, o Algoritmo Genético irá calcular o melhor conjunto que não ultrapasse o limite.<br>Em 100 iterações ele calcula o melhor 
                conjunto de itens que não ultrapassa a condição imposta, no caso, o peso total não pode ultrapassar 10kg.</p>'))})
    })
    }


shinyApp(ui = ui, server = server)

