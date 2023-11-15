library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(udpipe)
# source('NaiveBayes.R')


ui <- dashboardPage(
  skin = "red",
  title = "Proyecto 2",
  
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    tags$style("body { background-color: ghostwhite}"),
    
    fluidRow(
      box(
        title = "Argumentos efectivos",
        background = "red",
        icon = icon("folder-open", lib = "glyphicon"),
        textInput('texto','Texto a predecir',' '),
        textOutput("text"),
        verbatimTextOutput('value'),
        checkboxInput('inputId_Bayes', 'Naive Bayes', value = FALSE, width = NULL),
        checkboxInput('inputId_Random', 'Random Forest', value = FALSE, width = NULL)
      ),
    ),
    fluidRow(
      box(
        title = 'Eficiencia Naive Bayes',
        id='mynaiveR',
        textOutput('text_naive_head'),
        background = "teal",
        icon = icon("stats", lib = "glyphicon")
        
      ),
      box(
        title = 'Eficiencia Random Forest',
        id='myrandomR',
        textOutput('text_random_head'),
        background = "teal",
        icon = icon("tree-deciduous", lib = "glyphicon")
        
      ),
      
    ),
    fluidRow(
      
      box(
      
        actionButton("restore_box", "Eficiencia Naive Bayes", class = "bg-success", icon("hand-up", lib = "glyphicon"),
          style="color: #fff; background-color: #f24b59; border-color: #f24b4b"),
        actionButton("forest_box", "Eficiencia Random Forest", class = "bg-forest", icon("hand-up", lib = "glyphicon"),
                     style="color: #fff; background-color: #f24b59; border-color: #f24b4b")
      
      ),
    ),
    
   
    br(),
    box(
      title = textOutput("box_state"),
      id = "mybox",
      collapsible = TRUE,
      closable = TRUE,
      # infoBoxOutput("progressBox"),
      
      valueBox(59, "%", color = "orange"),
      textOutput('prec_naive'),
      icon = icon("stats", lib = "glyphicon")
    ),
    
    box(
      title = textOutput("box_random"),
      id = "myforest",
      collapsible = TRUE,
      closable = TRUE,
      textOutput('prec_random'),
      valueBox(56, "%", color = "orange"),
      icon = icon("tree-deciduous", lib = "glyphicon")
    )
  )
)

server <- function(input, output, session) {
  output$text <- renderText('Texto introducido')
  output$value <- renderText({ input$texto })
  
  output$text_naive_head <- renderText({
    paste("Eficiencia Naive Bayes")
    
    if(input$inputId_Bayes){
      if (input$texto==' '){
        paste('Esperando Texto')
      }else{
        
        output$text_naive_head<-renderText({
          source('NaiveBayes.R')
          paste(naiveBayesFun(input$texto))
          
          
        })
      }
      
    }
    
   
   
    
    
    
  })
  output$text_random_head <- renderText({
    paste("Eficiencia Random Forest")
    
    if(input$inputId_Random){
      if (input$texto==' '){
        paste('Esperando Texto')
      }else{
        output$text_random_head<-renderText({
          source('SVM.R')
          paste(head(randomForestPredic(input$texto),1))
          
          
        })
      }
      
    }
   
    
    
    
  })
  output$prec_naive<-renderText({
    
    if(input$inputId_Bayes){
      source('NaiveBayes.R')
      paste(naiveBayesFun_Ac(input$texto))
    }
    
  })
  output$prec_random<-renderText({
    
    if(input$inputId_Bayes){
      source('SVM.R')
      paste(randomForestPredic_Acc(input$texto))
    }
    
  })
 
  
  output$box_state <- renderText({
    state <- if (input$mybox$collapsed) "collapsed" else "uncollapsed"
    
    paste("Eficiencia Naive Bayes")
  })
  
 
  observeEvent(input$restore_box, {
    updateBox("mybox", action = "restore")
  })
  

  observeEvent(input$mybox$visible, {
    collapsed <- if (input$mybox$collapsed) "collapsed" else "uncollapsed"
    visible <- if (input$mybox$visible) "visible" else "hidden"
    message <- paste("Eficiencia Naive bayes")
    showNotification(message, type = "warning", duration = 1)
  })
  # Boton de Random Forest
  
  output$box_random <- renderText({
    state <- if (input$myforest$collapsed) "collapsed" else "uncollapsed"
    paste("Eficiencia Random Forest")
  })
  
  
  observeEvent(input$forest_box, {
    updateBox("myforest", action = "restore")
  })
  
  
  observeEvent(input$myforest$visible, {
    collapsed <- if (input$myforest$collapsed) "collapsed" else "uncollapsed"
    visible <- if (input$myforest$visible) "visible" else "hidden"
    message <- paste("Eficiencia Random Forest")
    showNotification(message, type = "warning", duration = 1)
  })
  
  
  
   #Info Box
   output$progressBox <- renderInfoBox({
     infoBox(
       "Progress", paste0(input$count), icon = icon("list"),
       color = "purple"
     )
   })
  
  
  
  
}

shinyApp(ui, server)
