# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(rvest)
library(plot.matrix)
library(glue)
library(rsconnect)


# run functions
source(('setup_grids.R'), local = TRUE)
source(('gen_next.R'), local = TRUE)
source(('reduced_mtrx.R'), local = TRUE)


# Git location:
github_url <- "https://github.com/Xavier-Chenede/app_jdlv/tree/master/models"

# default value:
## set generation number    
g <<- 0


##Minimal struct to far far away !
mini_struct <- matrix(nrow=3, ncol=7)
mini_struct[,1] <- c(NA,NA,1)
mini_struct[,2] <- c(1,NA,1)
mini_struct[,3] <- c(NA,NA,NA)
mini_struct[,4] <- c(NA,1,NA)
mini_struct[,5] <- c(NA,NA,1)
mini_struct[,6] <- c(NA,NA,1)
mini_struct[,7] <- c(NA,NA,1)
mini_struct 


# Define UI for the application
ui <- fluidPage(
  
  tabsetPanel(
    
    tabPanel("Main",
             titlePanel("JEU DE LA VIE"),
             # Panneau latéral
             sidebarLayout(
               sidebarPanel(
                 # Bouton d'entrée
                 selectInput("File_choice", "Choose a model", choices=NULL),
                 actionButton("update", "Update file list"),
                 hr(),
                 sliderInput("gen",
                             "Number of generations:",
                             min = 0,
                             max = 100,
                             value = 0,
                             animate=animationOptions(100)
                 ), 
                 "Basal configuration (Generation 0)",
                 plotOutput("gen_0"),
                 width=4,
                 #style = "overflow-y:scroll; max-height: 90vh; position:relative;",
                 plotOutput("evolution")
               ),
               
               # Zone de sortie principale
               mainPanel(
                 fluidRow(
                   textOutput("lab_gen"),
                   tags$head(tags$style("#lab_gen{color: blue; font-size: 40px; font-style: bold; }")),
                   numericInput("stp", label="Display 1 generation/n (enter n):", min=1,  value= 1, width= "300px"),
                   actionButton(inputId="Go", label="Launch evolution!"),
                   hr(),
                   textOutput("count_alive"),
                   textOutput("percent_var"),
                   # plotOutput("gen_grid"),tags$style(type = "text/css", "gen_grid { height: 100%; width: 100%; }")#revoir ici,
                   plotOutput("gen_grid",height="1000px",width="1000px"),
                   # tags$style(type = "text/css", "gen_grid { height: 800px; width: 800px; }")#revoir ici,
                 )
               )
             )
    ),
    
    tabPanel("Rules",
             h1("Rules of the Conway's game of life'"),
             hr(),
             h3("Une cellule possède huit voisins, qui sont les cellules adjacentes horizontalement,
                         verticalement et diagonalement. À chaque itération, l'état d’une cellule est
                         entièrement déterminé par l’état de ses huit cellules voisines, selon les règles
                         suivantes :"),
             hr(),
             h2("rule#1"),
             h3("Une cellule vivante possédant deux ou trois cellules voisines vivantes le reste,
                         sinon elle meurt"),
             hr(),
             h2("rule#2"),
             h3("Une cellule vide possédant exactement trois cellules voisines vivantes devient
                         vivante (elle naît)")
    ),
    
    tabPanel("Miscellaneous"),
  )
)





server <- function(input, output, session) {
  
  # Lorsque le bouton "update" est cliqué, récupérer la liste des fichiers dans le répertoire et mettre à jour l'objet selectInput
  observeEvent(input$update, {
    # Récupérer le contenu du répertoire à l'aide de read_html() et html_nodes()
    page <- read_html(github_url)
    files <- html_nodes(page, ".js-navigation-open") %>% html_text()
    # Mettre à jour l'objet selectInput avec la liste des fichiers
    updateSelectInput(session, "File_choice", choices = files[6:length(files)])
  })
  
  
  output$gen_0 <- renderPlot({
    plot(mini_struct, main = "", key=NULL, asp=TRUE,axis.col=NULL, axis.row=NULL, xlab='', ylab='') 
  })
  
  # Lorsque l'objet SelectInput est updaté, récupérer fichier et mettre à jour la grille 'generation 0'
  observeEvent(input$File_choice, {
    if (input$File_choice != "") {
      # updateNumericInput(session, inputId = "lab_gen", value = 0)
      output$lab_gen <- renderText("Generation 0")
      data_to_inject <<- setup_matrix(mini_struct)
      #setup and load the original grid in the right panel
      output$gen_grid <- renderPlot({
        plot(
          data_to_inject,
          main = "",
          key = NULL,
          asp = TRUE,
          axis.col = NULL,
          axis.row = NULL,
          xlab = '',
          ylab = ''
        )
      })
    }
  })

  rv <- reactiveValues(i = 0)
  maxIter <- 100
  
  output$gen_grid <- renderPlot( {
    if(rv$i > 0) {
      g <<- g+1 
      output$lab_gen <- renderText(glue("Generation {g}"))
      # hist(runif(25,0,100))
      data_to_inject <<- newgen(data_to_inject)
      #plot next gen
      plot(
        data_to_inject,
        main = "",
        key = NULL,
        asp = TRUE,
        axis.col = NULL,
        axis.row = NULL,
        xlab = '',
        ylab = ''
      )
      
      
      
    } 
  })
  
  observeEvent(input$Go, {
    rv$i <- 0
    observe({
      isolate({
        rv$i <- rv$i + 1
      })
      
      if (isolate(rv$i) < maxIter){
        invalidateLater(300, session)
      }
    })
  })
  
}





shinyApp(ui = ui, server = server)

