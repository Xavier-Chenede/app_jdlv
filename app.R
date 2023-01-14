# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


library(shiny)
library(readr)
library(rvest)
library(plot.matrix)
library(glue)
library(rsconnect)

# run functions
source(('setup_grids_v2.0.R'), local = TRUE)
source(('gen_next_v2.0.R'), local = TRUE)
source(('reduced_mtrx_v2.0.R'), local = TRUE)

# Git location:
github_url <- "https://github.com/Xavier-Chenede/app_jdlv/tree/master/models"

# default value:
## set generation number    
g <<- 0

##Minimal struct to far far away !
mini_struct <- matrix(nrow=3, ncol=7)
mini_struct[,1] <- c(0,0,1)
mini_struct[,2] <- c(1,0,1)
mini_struct[,3] <- c(0,0,0)
mini_struct[,4] <- c(0,1,0)
mini_struct[,5] <- c(0,0,1)
mini_struct[,6] <- c(0,0,1)
mini_struct[,7] <- c(0,0,1)

#Random structure !
size <- 25
val <- c(1)
inp <-  sample(x = val, replace = TRUE, size = size*size)
rand_mat1 <- matrix(inp,nrow=size,ncol=size)
plot(rand_mat1)


 
Struc_in <- rand_mat1

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
                   plotOutput("evolution"),
                   hr(),
                   h1("Statistics"),
                   h2("Count living cells"),
                   textOutput("count_alive"),
                   tags$head(tags$style("#count_alive{color: red; font-size: 40px; font-style: bold; }"))
                   # plotOutput("evolution")
                   
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
             h3("Une cellule possède huit voisines, qui sont les cellules adjacentes horizontalement,
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
        plot(Struc_in, main = "", key=NULL, asp=TRUE,axis.col=NULL, axis.row=NULL, xlab='', ylab='', col=c('black','red')) 
      })
      
      # Lorsque l'objet SelectInput est updaté, récupérer fichier et préparation de la grille 'generation 0'
      observeEvent(input$File_choice, {
        if (input$File_choice != "") {
          data_to_inject <- reduced_mtrx(Struc_in)
          data_to_inject <<- setup_matrix(Struc_in)
        }
      })
    
      rv <- reactiveValues(i = 0)
      # evol <- NULL
      # gnr <-  NULL
      output$gen_grid <- renderPlot( {
        if(rv$i > 0) {
          g <<- g+1 
          output$lab_gen <- renderText(glue("Generation {g}"))
          output$count_alive <- renderText({sum(data_to_inject)})
          
          # *********************
          
          # gnr <- c(gnr,length(gnr)+1)
          # evol <- dataframe(x= gnr, y = c(evol,output$count_alive))
          # 
          # 
          # output$evolution <- renderPlot({
          #   ggplot(evol,(aes(gnr,evol)))+
          #     geom_area(show.legend = FALSE,na.rm = TRUE,colour="red")+
          #     scale_x_continuous(name = "Generations")+
          #     scale_y_continuous(name = "Evolution")+
          #     geom_hline(yintercept=c(-100,0,100))+
          #     theme_bw()+
          #     theme(axis.ticks = element_blank(), text=element_text(size=20) )
          #   
          # }) 
          # *********************
          
          
          
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
                ylab = '',
                col=col_mat
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
      
      #management of reset button
      observeEvent(input$rst, {
        output$lab_gen  <- renderText ("")
        g<<-0
        data_to_inject <<- setup_matrix(Struc_in)
        rv$i <- 0
        updateActionButton(inputId="Go",label = "Launch evolution!")
      }) 
      
    
      
      
   
}


shinyApp(ui = ui, server = server)

