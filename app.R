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
                   "Basal configuration (Generation 0)",
                   plotOutput("gen_0"),
                   width=4,
                   #style = "overflow-y:scroll; max-height: 90vh; position:relative;",
                   plotOutput("evolution")
               ),
               
               # Zone de sortie principale
               mainPanel(
                    # Zone supérieure
                  fluidRow(
                     column(width=2, align= "center",
                         textOutput("lab_gen"),
                         tags$head(tags$style("#lab_gen{color: blue; font-size: 40px; font-style: bold; }"))
                     ),  
                     
                     column(width=3,
                         numericInput("nbgen", label="Enter the number of generations to run:", min=1,  value= 10, width= "300px"),
                         numericInput("speed", label="Enter refresh delay in ms:", min=1,  value= 100, width= "300px")
                         
                         # textOutput("count_alive"),
                         # textOutput("percent_var")
                     ),
                     
                     column(width=3, align= "center",
                        actionButton(inputId="Go", label="Launch evolution!"),
                        hr(),
                        actionButton(inputId="rst", label="Reset")
                     ),
                   ),
                  
                  # Zone inférieure
                  fluidRow(
                     plotOutput("gen_grid",height="1000px",width="1000px"),
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
    )
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
      
      # Lorsque l'objet SelectInput est updaté, récupérer fichier et préparation de la grille 'generation 0'
      observeEvent(input$File_choice, {
        if (input$File_choice != "") {
          data_to_inject <<- setup_matrix(mini_struct)
        }
      })
    
      rv <- reactiveValues(i = 0)

      output$gen_grid <- renderPlot( {
        if(rv$i > 0) {
          g <<- g+1 
          output$lab_gen <- renderText(glue("Generation {g}"))
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
          updateActionButton(inputId="Go",label = "Continue evolution!")
        } 
      })
      
      observeEvent(input$Go, {
        rv$i <- 0
        observe({
          isolate({
            rv$i <- rv$i + 1
          })
          
          if (isolate(rv$i) < input$nbgen){
            invalidateLater(input$speed, session)
          }
        })
      })
      
      
      observeEvent(input$rst, {
        output$lab_gen  <- renderText ("")
        g<<-0
        data_to_inject <<- setup_matrix(mini_struct)
        rv$i <- 0
        updateActionButton(inputId="Go",label = "Launch evolution!")
      }) 
      
}


shinyApp(ui = ui, server = server)

