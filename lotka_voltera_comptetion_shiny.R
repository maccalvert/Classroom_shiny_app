#This is a script to run a basic lotka volterra competition model within a shiny app. 
#You will need to install the 'shiny' and 'deSolve' pacakges to get it to work, if you haven't already 
#shiny performs better when called form R studio, but it can run from regular old command line R, too. 

#MBC 
#11/20/2019

library(shiny)
library(deSolve)

LotVmod_comp <- function (Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    dx = r1*x*((k1-x-alpha12*y))
    dy = r2*y*((k2-y-alpha21*x))
    return(list(c(dx, dy)))
  })
}

Pars <- c(r1 = 0.1, r2=0.2, k1=100, k2 = 200, alpha12 = 0.5, alpha21 = 0.8)
State <- c(x = 10, y = 10)
Time <- seq(0, 100, by = 1)

out <- as.data.frame(ode(func = LotVmod_comp, y = State, parms = Pars, times = Time))

matplot(out[,-1], type = "l", xlab = "time", ylab = "population")
legend("topright", c("Squirrels", "Chipmunks"), lty = c(1,2), col = c(1,2), box.lwd = 0)


# Define UI (user interface) for sliders and table / figure output----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Lotka Voltera comptetion"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to house the sliders ----
    sidebarPanel(
      
      # Input: Intrinsic growth rate of species 1 "squirrels" 
      sliderInput("r1", "Squirrel growth rate:",
                  min = 0, max = 10,
                  value = 0.1, step=0.1),
      
      # Input: Intrinsic growth rate of species 2 "chipmunks"
      sliderInput("r2", "Chipmunk growth rate:",
                  min = 0, max = 10,
                  value = 0.1, step = 0.1),
      
      # Input: Carrying capacity of species 1 "squirrels"
      sliderInput("k1", "Squirrel carrying capacity:",
                  min = 0, max = 1000,
                  value = 100, step=5),
      
      # Input: Carrying capacity of species 2 "chipmunks"
      sliderInput("k2", "Chipmunk carrying capacity:",
                  min = 0, max = 1000,
                  value = 200, step = 5),
      
      # Input: X - Competition coefficient, effect of species 2 "chipmunks" on species 1 "squirrels"
      sliderInput("alpha12", "effect if chipmunks on squirrels:",
                  min = 0, max = 2,
                  value = 0.5, step = 0.1),
      
      #Input: Y - Competition coefficient, effect of species 1 "squirrels
      sliderInput("alpha21", "effect of squirrels on chipmunks:",
                  min = 0, max = 2,
                  value = 0.5, step = 0.1),
      
      #Input: Y - predator starting density
      sliderInput("x", "squirrels starting pop:",
                  min = 0, max = 100,
                  value = 10, step = 1),
      
      #Input: Y - predator starting density
      sliderInput("y", "chipmunks starting pop:",
                  min = 0, max = 100,
                  value = 10, step = 1),
      
      #Input: time - how many steps to run the simulation
      sliderInput("time", "time:",
                  min = 0, max = 1000,
                  value = 100, step = 10),
      
    ),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Table summarizing the values entered ----
      tableOutput("values"),
      
      # Output: Table summarizing the values entered ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic for slider examples ----
#This is basically what to do with the input above. All of the slider values 
#above feed into an object "input" that can be called as an atomic vector using $
server <- function(input, output) {
  
  # Reactive expression to create data frame of all input values ----
  sliderValues <- reactive({
    
    data.frame(
      Name = c("r1",
               "r2",
               "k1",
               "k2", 
               "alpha12", 
               "alpha21",
               "x", 
               "y",
               "time"),
      Value = as.character(c(input$r1,
                             input$r2,
                             input$k1,
                             input$k2, 
                             input$alpha12,
                             input$alpha21,
                             input$x, 
                             input$y,
                             input$time)),
      stringsAsFactors = FALSE)
    
  })
  
  # Run the ode and output the figure 
  output$distPlot <- renderPlot({
    
    Pars <- c(r1 = input$r1, r2 = input$r2, k1 = input$k1, k2 = input$k2, alpha12 = input$alpha12, alpha21 = input$alpha21)
    State <- c(x = input$x, y = input$y)
    Time <- seq(0, input$time, by = 1)
    
    out <- as.data.frame(ode(func = LotVmod_comp, y = State, parms = Pars, times = Time))

    matplot(out[,-1], type = "l", xlab = "time", ylab = "population")
    legend("topright", c("Squirrels", "Chipmunks"), lty = c(1,2), col = c(1,2), box.lwd = 0)
    
  })
  
  
  # Show the values in an HTML table ----
  output$values <- renderTable({
    sliderValues()
  })
  
   
}

# Create Shiny app, calls the user interface and the server commands ----
shinyApp(ui, server)


