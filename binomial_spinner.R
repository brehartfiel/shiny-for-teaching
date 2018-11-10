# Binomial Spinner
# Breanna Hartfiel (Gronner) 
# Original Date: 1/30/2014 Built under shiny 0.4.0 (2/22/2013 release)
# Date: 11/10/2018 - updating app for shiny 1.2.0 

library(shiny)

# Define UI for application that plots random distributions
ui <- fluidPage(
  
  # Application title
  headerPanel("Binomial Simulation and Spinner"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    
    # Specify the probaility of success
    sliderInput("psuccess", "Probability of Success:", 
                min = 0, max = 1, value = 0, step = .01),
    #Specify the sample size
    sliderInput("n", "Sample Size:",
                min = 1, max = 30, value = 1, step=1.0),
    
    # Specify the parameter
    sliderInput("sims", "Number of Repeated Samples:",
                min = 1, max = 100, value = 1, step= 1.0),
    
    # Specify if the user wants to show the p-value testing information
    checkboxInput("check",
                  label = strong("Test a specific value"),
                  value = FALSE),
    
    conditionalPanel(
      condition = "input.check == true",
      
      numericInput("obs", "Observed Count:", min = 0, value = 1.0),
      
      selectInput("test", "Choose a type of test:", 
                  list(" " = "blank",
                       "Left-tailed" = "left",
                       "Right-tailed" = "right", 
                       "Two-tailed" = "two"))    
    )
  ),
  
  # where the plots get displayed
  mainPanel(
    
    tabsetPanel(
      tabPanel("Spinner",
               # displaying piPlot under the Spinner tab
               plotOutput("circlePlot", height = "509px", width = "500px")
      ), 
      tabPanel("Simulations",
               # displaying the number of successes/failures in the first simulation
               h4("One simulation from the spinner"),
               tableOutput("onesimTable"),
               # displaying the remaining simulations
               h4("The remaining simulations"),
               tableOutput("remainingTable")
      ),
      tabPanel("Summary", 
               # displaying a histogram of the data under the Data tab
               plotOutput("histPlot")
      )
    )
  )
)


library(BHH2)

# Define server logic required to generate and plot a random distribution
server <- function(input, output) {
  
  # This is here to generate the random data.  To be able to use the data in multiple 
  # plots it must be in a function outside the renderPlot() functions and will need 
  # to be called later using myData()
  # the reactive() around the function makes it so that the lines for the spinner
  # will not change until you refresh the page
  myData <- reactive(function() {
    # Generate a uniform distribution from 0 to 2pi (think polar coordinates)
    runif(n=100, min=0, max=1)
    
  })
  
  binData <- reactive(function() {
    # Generate a binomal distribution 
    rbinom(1000, input$n, input$psuccess)
  })
  
  # Generates piPlot and allows it to be reactive
  output$circlePlot <- renderPlot({
    
    # changing the value of pi into an x and y coordinate using polar coordinate knowledge.
    # x = rcos(theta) and y = rsin(theta) where theta is between 0 and 2pi
    xcart = cos(input$psuccess*2*pi)
    ycart = sin(input$psuccess*2*pi)
    
    # getting the data and setting the data up to be used
    data = myData()
    data = data.frame(data*(2*pi))
    data = data[1:input$n,]
    
    # Setting up the plot area for the circle
    plot(0,0,
         xlim = c(-1,1), ylim = c(-1,1), 
         xlab = "", ylab = "", frame.plot = FALSE, axes=FALSE)
    
    # coloring in the circle 
    if(input$psuccess <= .25){
      # the x coordinates for the curve
      x = c(0, xcart, seq(xcart, 1, (1-xcart)/50), 1)
      # the y coordinates for the curve
      y = c(0, ycart, sqrt(1-(seq(xcart, 1, (1-xcart)/50)^2)), 0) 
      # drawing the polygon
      polygon(x, y, col='skyblue', border = NA)
    }
    
    else if(input$psuccess <=.5){
      # the curve for the first section but for a full 25%
      # replaced xcart with 0 and ycart with 1 because 25% of the circle (rotating like polar coordinates) is at point (0,1)
      x = c(0,0,seq(0,1,(1-0)/50),1)
      y = c(0,1, sqrt(1-(seq(0, 1, (1-0)/50)^2)), 0) 
      polygon(x, y, col='skyblue', border = NA)
      
      # the curve for the second section 
      x = c(xcart, seq(xcart, 0, (1-xcart)/50), -.00000000001, 0)
      y = c(ycart, sqrt(1-(seq(xcart, 0, (1-xcart)/50)^2)), .999999999, 0) 
      polygon(x, y, col='skyblue', border = NA)
    }
    
    else if(input$psuccess <= .75){
      
      # the curve for the first section but for a full 25%
      # replaced xcart with 0 and ycart with 1 because 25% of the circle (rotating like polar coordinates) is at point (0,1)
      x = c(0,0,seq(0, 1, 1/50), 1)
      y = c(0,1, sqrt(1-(seq(0, 1, 1/50)^2)), 0) 
      polygon(x, y, col='skyblue', border = NA)
      
      # the curve for the second section but for a full 50%
      # replaced xcart with 1 and ycart with 0 because 50% of the circle (rotating like polar coordinates) is at point (-1,0)
      x = c(-1, seq(-1, 0, 1/50), 0, 0)
      y = c(0, sqrt(1-(seq(-1, 0, 1/50)^2)), 1, 0) 
      polygon(x, y, col='skyblue', border = NA)
      
      # the curve for the third section of the pie
      x = c(-1, seq(-1, xcart, abs(1-xcart)/50), xcart, 0)
      # using negative sqrt because we are drawing the bottom half of a circle
      y = c(0, -sqrt(1-(seq(-1, xcart, abs(1-xcart)/50)^2)), ycart, 0) 
      polygon(x, y, col='skyblue', border = NA)
    }
    else if(input$psuccess < 1){
      
      # the curve for the first section but for a full 25%
      # replaced xcart with 0 and ycart with 1 because 25% of the circle (rotating like polar coordinates) is at point (0,1)
      x = c(0,0,seq(0, 1, 1/50), 1)
      y = c(0,1, sqrt(1-(seq(0, 1, 1/50)^2)), 0) 
      polygon(x, y, col='skyblue', border = NA)
      
      # the curve for the second section but for a full 50%
      # replaced xcart with 1 and ycart with 0 because 50% of the circle (rotating like polar coordinates) is at point (-1,0)
      x = c(-1, seq(-1, 0, 1/50), 0, 0)
      y = c(0, sqrt(1-(seq(-1, 0, 1/50)^2)), 1, 0) 
      polygon(x, y, col='skyblue', border = NA)
      
      # the curve for the third section but for a full 75%
      # replaced the xcart with 0 and ycart with 1 because 75% of the circle is at point (0,-1)
      x = c(-1, seq(-1, 0, 1/50), 0, 0)
      # using negative sqrt because we are drawing the bottom half of a circle
      y = c(0, -sqrt(1-(seq(-1, 0, 1/50)^2)), -1, 0) 
      polygon(x, y, col='skyblue', border = NA)
      
      # the curve for the fourth section of the pie
      x = c(-1, seq(-1, xcart, abs(1-xcart)/50), xcart, 0)
      # using negative sqrt because we are drawing the bottom half of a circle
      y = c(0, -sqrt(1-(seq(-1, xcart, abs(1-xcart)/50)^2)), ycart, 0) 
      polygon(x, y, col='skyblue', border = NA)
    }
    else {
      
      # the curve for the first section but for a full 25%
      # replaced xcart with 0 and ycart with 1 because 25% of the circle (rotating like polar coordinates) is at point (0,1)
      x = c(0,0,seq(0, 1, 1/50), 1)
      y = c(0,1, sqrt(1-(seq(0, 1, 1/50)^2)), 0) 
      polygon(x, y, col='skyblue', border = NA)
      
      # the curve for the second section but for a full 50%
      # replaced xcart with 1 and ycart with 0 because 50% of the circle (rotating like polar coordinates) is at point (-1,0)
      x = c(-1, seq(-1, 0, 1/50), 0, 0)
      y = c(0, sqrt(1-(seq(-1, 0, 1/50)^2)), 1, 0) 
      polygon(x, y, col='skyblue', border = NA)
      
      # the curve for the third section but for a full 75%
      # replaced the xcart with 0 and ycart with 1 because 75% of the circle is at point (0,-1)
      x = c(-1, seq(-1, 0, 1/50), 0, 0)
      # using negative sqrt because we are drawing the bottom half of a circle
      y = c(0, -sqrt(1-(seq(-1, 0, 1/50)^2)), -1, 0) 
      polygon(x, y, col='skyblue', border = NA)
      
      # the curve for the fourth section of the pie
      x = c(0, seq(0, 1, abs(1-0)/50), 1, 0)
      # using negative sqrt because we are drawing the bottom half of a circle
      y = c(-1, -sqrt(1-(seq(0, 1, abs(1-0)/50)^2)), 0, 0) 
      polygon(x, y, col='skyblue', border = NA)
    }
    
    # drawing a circle using curve()
    curve(sqrt(1-x^2), from = -1, to = 1, n = 200, add = TRUE, xlab = "")
    curve(-sqrt(1-x^2), from = -1, to = 1, n = 200, add = TRUE, xlab = "")
    
    # drawing the random lines with theta between 0 and 2pi x = rcos(data) and y = rsin(data) *data is theta*
    segments(0, 0, cos(data), sin(data))
    })
  
  # Generates onesimTable and allows it to be reactive
  output$onesimTable <- renderTable({
    
    # getting the spinner data
    unifdata = myData()
    unifdata = data.frame(c(unifdata))
    unifdata = unifdata[1:input$n,]
    
    # determining if each part is a success or failure
    success = data.frame(c(sum(unifdata <= input$psuccess)))
    failure = data.frame(c(input$n - sum(unifdata <= input$psuccess)))
    
    # binding the successes and failures together to display them in simulationtable
    simulationtable = cbind(format(success, nsmall = 0),format(failure, nsmall = 0))
    names(simulationtable)[1] <- "Success"
    names(simulationtable)[2] <- "Failure"
    simulationtable
  })
  
  # Generates remainingTable and allows it to be reactive
  output$remainingTable <- renderTable({
    
    # getting the binomial data
    data = binData()
    data = data.frame(c(data))
    
    # displaying the successes and failures and displaying them in situationtable
    success = data[0:(input$sims - 1),]
    failure = data.frame(c(input$n - success))
    simulationtable = cbind(format(success, nsmall = 0), format(failure, nsmall = 0))
    names(simulationtable)[1] <- "Success"
    names(simulationtable)[2] <- "Failure"
    simulationtable
  })
  
  # Generates distPlot and allows it to be reactive
  output$histPlot <- renderPlot({
    
    # getting the data displayed in the spinner
    unifdata = myData()
    unifdata = data.frame(c(unifdata))
    unifdata = unifdata[1:input$n,]
    
    # counting the number of successes in the spinner
    data = data.frame(c(sum(unifdata <= input$psuccess)))
    names(data)[1] <- "Success"
    
    # this if statement only grabs the extra binomial simulation data if sim is greater than 1
    if (input$sims > 1){
      
      # getting the remaining data for the simulation
      remainingsuccesses = binData()
      remainingsuccesses = data.frame(c(remainingsuccesses))
      remainingsuccesses = data.frame(c(remainingsuccesses[0:(input$sims - 1),]))
      names(remainingsuccesses)[1] <- "Success"
      
      # binding together the successes for the data
      data = rbind(data, remainingsuccesses)  
    }
    
    # Initializing variables because if statements in shiny do not like input variables
    # If you declare the drop-down inputs as a variable, it avoids a lot of potential errors
    # This does that
    check = input$check  
    test = input$test 
    
    # Dot plot of the binomial simulations
    dotPlot(data, xlim=c(0,input$n), xlab="Number of Successes")
    
    # For testing and creating a p-value
    if(check==TRUE & test!="blank"){
      
      # For right-tailed tests
      if(test=="right"){
        
        # Calculating the p-value
        countdots = sum(data >= input$obs)
        pvalue = countdots/input$sims
        
        # Making the grey box around the counted segments
        # If statement makes it so n doesn't go beyond n
        if (input$obs >= input$n) {
          rect(input$n, 0, input$n, 1, col="#0000ff22", pch=16, cex=3)
        }
        else{
          rect(input$obs, 0, input$n, 1, col="#0000ff22", pch=16, cex=3)
        }
      }
      
      # For left-tailed tests
      if(test == "left"){
        
        # Calculating the p-value
        countdots = sum(data <= input$obs)  
        pvalue = countdots/input$sims
        
        # Making the grey box around the counted segments
        # if statement is to avoid the box going over n
        if(input$obs > input$n){
          rect(0, 0, input$n, 1, col="#0000ff22", pch=16, cex=3)
        }
        else{          
          rect(0, 0, input$obs, 1, col="#0000ff22", pch=16, cex=3)
        }
      }
      
      # For two-tailed tests
      if(test == "two"){
        
        # Calculating the expected value and the distance from the observed to the expected value
        expectedvalue = input$n * input$psuccess
        distance = abs(expectedvalue - input$obs)
        
        # Calculating the p-value for when the expected value is the same as the observed value
        if(distance <= 0){
          countdots = input$sims
          pvalue = 1
          # Making the grey box around the counted segments
          rect(0, 0, input$n, 1, col="#0000ff22", pch=16, cex=3)
        }
        
        # Calculating the p-value for when the expected value is not the same as the observed value
        if(distance > 0){
          countdots = sum(data >= (expectedvalue + distance)) + sum(data <= (expectedvalue - distance))
          pvalue = countdots/input$sims
          
          # Used to make the grey boxes around the counted segments
          if(pvalue != 1){
            leftbox = (expectedvalue - distance)
            rightbox = (expectedvalue + distance)
            
            # Used to avoid having the grey boxes go outside the plot area
            if(rightbox > input$n){
              rightbox = input$n
            }
            if(leftbox < 0){
              leftbox = 0
            }
            
            # Making the boxes
            rect(rightbox, 0, input$n, 1, col="#0000ff22", pch=16, cex=3)
            rect(0, 0, leftbox, 1, col="#0000ff22", pch=16,cex=3)
          }
          
          # Sometimes when the expected value is between possible observations so the boxes look odd 
          # in the above code... this fixes that
          if (pvalue == 1) {
            rect(0, 0, input$n, 1, col="#0000ff22", pch=16, cex=3)
          }
        }
      }
      
      # Displaying the p-value
      text(3, .75, 
           paste("p-value = ", countdots,"/", input$sims," = ", round(pvalue, 4)), col = "red" )
    }
  })
}

shinyApp(ui = ui, server = server)