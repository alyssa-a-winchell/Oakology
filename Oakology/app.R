#Oakology Shiny App
# Find out more about building applications with Shiny here: http://shiny.rstudio.com/


# load the shiny package
library(shiny)
library(shinythemes)

#load other packages
library(raster)

ras<-raster("data/sdm/scr/nofog/historic.tif")

ui<-fluidPage(theme = shinytheme("lumen"),
              titlePanel("Oakology"),
              navbarPage("NavBar",
                         tabPanel("Summary", "Put in summary info... Island oak (Quercus tomentella) is a 
                                  rare oak species that is endemic to six islands in the California Island 
                                  Archipelago. It is listed as endandered by the IUCN..."),
                         tabPanel("Example",
                                  sidebarPanel(
                                    fileInput("file", "File input:"),
                                    textInput("txt", "Text input:", "general"),
                                    sliderInput("slider", "Slider input:", 0, 100, 30),
                                    tags$h5("Deafult actionButton:"),
                                    actionButton("action", "Search"),
                                    
                                    tags$h5("actionButton with CSS class:"),
                                    actionButton("action2", "Action button", class = "btn-primary")
                                  ),
                                  mainPanel(
                                    tabsetPanel(
                                      tabPanel("Tab 1",
                                               h4("Table"),
                                               tableOutput("table"),
                                               h4("Verbatim text output"),
                                               verbatimTextOutput("txtout"),
                                               h1("Header 1"),
                                               h2("Header 2"),
                                               h3("Header 3"),
                                               h4("Header 4"),
                                               h5("Header 5")
                                      ),
                                      tabPanel("Tab 2", "This panel is intentionally left blank"),
                                      tabPanel("Tab 3", "This panel is intentionally left blank")
                                    )
                                  )
                         ),
                         tabPanel("Islands", "This panel is intentionally left blank"),
                         tabPanel("SDM", "This panel is intentionally left blank")
              )
)

# Define server logic ----
server <- function(input, output) {
  
}

# Run the app ----
shinyApp(ui = ui, server = server)


