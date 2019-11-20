#This is a script to run a basic lotka volterra model within a shiny app. 
#To see the app, make sure it is run within Rstudio. 
#You will also need to install the shiny package to get it to work. 

library(shiny)
library(deSolve)

LotVmod <- function (Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    dx = (r1*(1-(x/k1)-((beta_12*y)/k2)))+sig_e1*
    dx = x*(alpha - beta*y)
    dy = -y*(gamma - delta*x)
    return(list(c(dx, dy)))
  })
}

LotV_comp <- function(Time, State, Pars)

Pars <- c(r1=0.5, r2=0.8, k1 = 1000, k2 = 1500, phi_e=0.5, sig_e1 = 0.02, sig_e2 = 0.02, sig_d1 = 1, sig_d2 = 1,
          beta_12 = 0.7, beta_21=0.9, u_e1 = rnorm())
State <- c(x = 800, y = 500)
Time <- seq(0, 500, by = 1)

out <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars, times = Time))

matplot(out[,-1], type = "l", xlab = "time", ylab = "population")
legend("topright", c("Cute bunnies", "Rabid foxes"), lty = c(1,2), col = c(1,2), box.lwd = 0)


# Define UI (user interface) for sliders and table / figure output----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Lotka Voltera predator-prey"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to house the sliders ----
    sidebarPanel(
      
      # Input: alpha value - linear prey growth rate
      sliderInput("alpha", "alpha:",
                  min = 0, max = 30,
                  value = 2, step=0.5),
      
      # Input: beta - predator capture rate
      sliderInput("beta", "beta:",
                  min = 0, max = 5,
                  value = 0.5, step = 0.1),
      
      # Input: "gamma - "predator vitality"
      sliderInput("gamma", "gamma:",
                  min = 0, max = 5,
                  value = 0.2, step=0.1),
      
      # Input: delta - predator death rate
      sliderInput("delta", "delta:",
                  min = 0, max = 5,
                  value = 0.6, step = 0.1),
      
      # Input: X - prey starting density
      sliderInput("prey", "prey:",
                  min = 0, max = 1000,
                  value = 10, step = 1),
      
      #Input: Y - predator starting density
      sliderInput("predator", "predator:",
                  min = 0, max = 1000,
                  value = 10, step = 1),
      
      #Input: time - how many steps to run the simulation
      sliderInput("time", "time:",
                  min = 0, max = 1000,
                  value = 100, step = 1),
      
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
      Name = c("alpha",
               "beta",
               "gamma",
               "delta", 
               "prey", 
               "predator",
               "time"),
      Value = as.character(c(input$alpha,
                             input$beta,
                             input$gamma,
                             input$delta, 
                             input$prey,
                             input$predator,
                             input$time)),
      stringsAsFactors = FALSE)
    
  })
  
  # Run the ode and output the figure 
  output$distPlot <- renderPlot({
    
    Pars <- c(alpha = input$alpha, beta = input$beta, gamma = input$gamma, delta = input$delta)
    State <- c(x = input$prey, y = input$predator)
    Time <- seq(0, input$time, by = 1)
    
    plot(input$alpha, input$beta)
    
    out <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars, times = Time))

    matplot(out[,-1], type = "l", xlab = "time", ylab = "population")
    legend("topright", c("Cute bunnies", "Rabid foxes"), lty = c(1,2), col = c(1,2), box.lwd = 0)
    
  })
  
  
  # Show the values in an HTML table ----
  output$values <- renderTable({
    sliderValues()
  })
  
   
}

# Create Shiny app, calls the user interface and the server commands ----
shinyApp(ui, server)


