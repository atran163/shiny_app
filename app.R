#
#title: "RGL Shiny App"
#authors: "David Barnett, Amanda Tran, Bryan Veres"
#date: "March 22, 2019"

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rgl)
library(shinyRGL)

ui <- fluidPage(
   
   # Application title
   titlePanel("RGL Demo Using mtcars"),
   
   # Sidebar with the inputs for the graph 
   sidebarLayout(
       sidebarPanel(
         
         h3("Inputs:"),
         h5(""),
         
         h5("Add a Title:"),
         #Title
         textInput(inputId = "title",
                   label = "Title:",
                   value = "Example RGL Plot"),
         
         h5(),
         h5("Determine Which Variables You Want to Use:"),
         
         #X Variable
         selectInput(inputId = "x",
                     label = "X Variable:",
                     choices = c("Miles Per Gallon" = "mpg",
                                 "Displacement (cu.in.)" = "disp",
                                 "Horsepower" = "hp",
                                 "Rear Axle Ratio" = "drat",
                                 "Weight (tons)" = "wt",
                                 "1/4 Mile Time (seconds)" = "qsec"),
                     selected = "wt"),
         
         #Y Variable
         selectInput(inputId = "y",
                     label = "Y Variable:",
                     choices = c("Miles Per Gallon" = "mpg",
                                 "Displacement (cu.in.)" = "disp",
                                 "Horsepower" = "hp",
                                 "Rear Axle Ratio" = "drat",
                                 "Weight (tons)" = "wt",
                                 "1/4 Mile Time (seconds)" = "qsec"),
                     selected = "qsec"),
         
         #Z Variable
         selectInput(inputId = "z",
                     label = "Z Variable:",
                     choices = c("Miles Per Gallon" = "mpg",
                                 "Displacement (cu.in.)" = "disp",
                                 "Horsepower" = "hp",
                                 "Rear Axle Ratio" = "drat",
                                 "Weight (tons)" = "wt",
                                 "1/4 Mile Time (seconds)" = "qsec"),
                     selected = "drat"),
           
          h5(),
          h5("Add Colors to Keep Track of Different Axes:"),
         
          #X Color
          selectInput(inputId = "x_color",
                      label = "Axis Color for X:",
                      choices = c("Red" = "red",
                                  "Blue" = "blue",
                                  "Yellow" = "yellow",
                                  "Green" = "green",
                                  "Orange" = "orange",
                                  "Purple" = "purple"),
                      selected = "red"),
       
          #Y Color
          selectInput(inputId = "y_color",
                      label = "Axis Color for Y:",
                      choices = c("Red" = "red",
                                  "Blue" = "blue",
                                  "Yellow" = "yellow",
                                  "Green" = "green",
                                  "Orange" = "orange",
                                  "Purple" = "purple"),
                      selected = "blue"),
       
          #Z Color
          selectInput(inputId = "z_color",
                      label = "Axis Color for Z:",
                      choices = c("Red" = "red",
                                  "Blue" = "blue",
                                  "Yellow" = "yellow",
                                  "Green" = "green",
                                  "Orange" = "orange",
                                  "Purple" = "purple"),
                      selected = "green"),
  
         
          h5(),
          h5("Add Labels to Your Axes:"),
         
          #X Label
          textInput(inputId = "xlab",
                    label = "X-Axis label:",
                    value = "X"),
          
          #Y Label
          textInput(inputId = "ylab",
                    label = "Y-Axis label:",
                    value = "Y"),
          
          #Z Label
          textInput(inputId = "zlab",
                    label = "Z-Axis label:",
                    value = "Z")
        
        ),
   
   
      mainPanel(
        h1("RGL Graph:"),
        h5("(Drag To Rotate, Scroll to Zoom)"),
        rglwidgetOutput(outputId = "mtPlot")
      )
   )
)


server <- function(input, output) {
   
   #Output Plot
   output$mtPlot <- renderRglwidget({
     data(mtcars)
     
     rgl.clear() # Clears the previous graph in order to create a new one when a new input is selected.
     
     # Changing the background to white so it blends in with the app screen and allows axes to be more readable.
     rgl.bg(color = "white")   
     
     rgl.points(x = mtcars[,input$x], y = mtcars[,input$y], z = mtcars[,input$z], color = "black")  # Plotting Points
     
     rgl.lines(c(0, max(mtcars[,input$x]) + 1), c(0, 0), c(0, 0), color = input$x_color)  # x line
     rgl.lines(c(0, 0), c(0, max(mtcars[,input$y]) + 1), c(0, 0), color = input$y_color)  # y line
     rgl.lines(c(0, 0), c(0, 0), c(0 ,max(mtcars[,input$z]) + 1), color = input$z_color)  # z line
     
     axis3d('x', pos = c(max(mtcars[,input$x])/2, 0, 0), color = input$x_color)  # x values
     axis3d('y', pos = c(0, max(mtcars[,input$y])/2, 0), color = input$y_color)  # y values
     axis3d('z', pos = c(0, 0, max(mtcars[,input$z])/2), color = input$z_color)  # z values
     
     rgl.texts(c(max(mtcars[,input$x]) / 2, 0, 0), text = paste(input$xlab), color = "black")  # x labels
     rgl.texts(c(0, max(mtcars[,input$y]) / 2, 0), text = paste(input$ylab), color = "black")  # y labels
     rgl.texts(c(0, 0, max(mtcars[,input$z]) / 2), text = paste(input$zlab), color = "black")  # z labels
     
     title3d(input$title) #Title will appear above the plot
     
     rglwidget()  #Allows the RGL widget to run
     })
   
}

#Running the App
shinyApp(ui = ui, server = server)

