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
    
    
    
    
    # Git location:
    github_url <- "https://github.com/Xavier-Chenede/app_jdlv/tree/master/models"
    
    
    # Generation 0 Grid (matrix object):
        #CROSS STRUCTURE
    mX <- matrix(nrow=8, ncol=8)
    mX[,1] <- c(NA,NA,1,1,1,1,NA,NA)
    mX[,2] <- c(NA,NA,1,NA,NA,1,NA,NA)
    mX[,3] <-c(1,1,1,NA,NA,1,1,1)
    mX[,4] <- c(1,NA,NA,NA,NA,NA,NA,1)
    mX[,5] <- c(1,NA,NA,NA,NA,NA,NA,1)
    mX[,6] <-c(1,1,1,NA,NA,1,1,1)
    mX[,7] <- c(NA,NA,1,NA,NA,1,NA,NA)
    mX[,8] <- c(NA,NA,1,1,1,1,NA,NA)
    mX
    
    # Define UI for the application
    ui <- fluidPage(
            tabsetPanel(
                tabPanel("Main",
                         titlePanel("JEU DE LA VIE"),
                         column(4,
                                wellPanel(
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
                                    style = "overflow-y:scroll; max-height: 90vh; position:relative;",
                                    plotOutput("evolution")
                                )
                         ),
                         column(8,
                                wellPanel( 
                                    
                                    textOutput("lab_gen"),
                                    textOutput("count_alive"),
                                    textOutput("percent_var"),
                                    plotOutput("gen_grid", height = "800px")
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
                
                tabPanel("Miscellaneous")
            )
    )
                 
                 
                 
    
    # Define server 
    server <- function(input, output,session) {
    
        # Lorsque le bouton "update" est cliqué, récupérer la liste des fichiers dans le répertoire et mettre à jour l'objet selectInput
        observeEvent(input$update, {
            # Récupérer le contenu du répertoire à l'aide de read_html() et html_nodes()
            page <- read_html(github_url)
            files <- html_nodes(page, ".js-navigation-open") %>% html_text()
            # Mettre à jour l'objet selectInput avec la liste des fichiers
            updateSelectInput(session, "File_choice", choices = files[5:length(files)])
        })
        
        
        output$gen_0 <- renderPlot({
            plot(mX, key=NULL, asp=TRUE,axis.col=NULL, axis.row=NULL, xlab='', ylab='') 
        })
        
        # Lorsque l'objet SelectInput est updaté, récupérer fichier et mettre à jour la grille 'generation 0'
        # observeEvent(input$File_choice, {
        # 
        #     output$gen_0 <- renderPlot({
        #             file <- input$File_choice
        #             ext  <- tools::file_ext(file$datapath)
        #             view <- readRDS(file$datapath)
        #             plot(view, key=NULL, asp=TRUE,axis.col=NULL, axis.row=NULL, xlab='', ylab='')
        #     })
        #     
        # })
        
    }
    
    # Run the application 
    shinyApp(ui = ui, server = server)
    
    file <- "https://github.com/Xavier-Chenede/app_jdlv/tree/master/models/canon"
    ext  <- tools::file_ext(file$datapath)
    view <- readRDS("https://github.com/Xavier-Chenede/app_jdlv/blob/c90d43c4bb97947d788dec686b6024511b9c19d0/models/canon")
    view
    view <- readRDS(file$datapath)
