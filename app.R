#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for the application
ui <- fluidPage(
        tabsetPanel(
            tabPanel("Main",
                     titlePanel("JEU DE LA VIE"),
                     column(4,
                            wellPanel(
                                sliderInput("gen",
                                            "Number of generations:",
                                            min = 0,
                                            max = 100,
                                            value = 0,
                                            animate=animationOptions(100)
                                ), 
                                "Basal config (Generation 0)",
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
                                # style = "overflow-y:scroll; max-height: 10vh; position:relative;", 
                                plotOutput("gen_grid", height = "800px")
                                # style = "overflow-y:scroll; max-height: 80vh; position:relative;" 
                                # style = "height:1000px;background-color: red;"
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
             
             
             
#              # Application title
#     titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )

# Define server logic required to draw a histogram
server <- function(input, output) {

    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white',
    #          xlab = 'Waiting time to next eruption (in mins)',
    #          main = 'Histogram of waiting times')
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
