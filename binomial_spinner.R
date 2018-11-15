# Binomial Spinner
# Breanna Hartfiel (Gronner) 
# Original Date: 1/30/2014 Built under shiny 0.4.0 (2/22/2013 release)
# Date: 11/10/2018 - updating app for shiny 1.2.0 

library(shiny)

# Define UI for application that plots random distributions
ui <- fluidPage(
  headerPanel("Binomial Simulation using a Spinner"),
  sidebarPanel(
    sliderInput("psuccess", "Probability of Success:", min = 0, max = 1, value = 0, step = .01),
    sliderInput("n", "Sample Size:", min = 1, max = 30, value = 1, step=1.0),
    sliderInput("sims", "Total Number of Samples:", min = 1, max = 100, value = 1, step= 1.0),
    
    checkboxInput("check", label = strong("Test a specific value"), value = FALSE),
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
  mainPanel(
    tabsetPanel(
      tabPanel("Spinner", plotOutput("circlePlot", height = "500px", width = "500px")
      ), 
      tabPanel("Simulations", 
               h4("One simulation from the spinner"), 
               tableOutput("onesimTable"),
               
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
  data = isolate({runif(n=100, min=0, max=1)*(2*pi)})
  bindata <- reactive({rbinom(1000, input$n, input$psuccess)})
  
  output$circlePlot <- renderPlot({
    # setting up a plot
    plot(0,0, xlim = c(-1,1), ylim = c(-1,1), xlab = "", ylab = "", frame.plot = FALSE, axes=FALSE)
    
    # this sets up a series of coordinates that will be used to shade the circle
    # starting at (0,0) so the center is the first edge point
    # calculating cartesian corrdinates for an entire circle in increments of 1000 to be called later
    xcart = c(0, cos(2*pi*seq(0,1,1/1000)))
    ycart = c(0, sin(2*pi*seq(0,1,1/1000)))
    
    # shading the section of the circle representing P(success) 
    polygon(c(0,xcart[1:(input$psuccess*1000+2)]), c(0,ycart[1:(input$psuccess*1000+2)]), col='skyblue', border = NA)
    
    # drawing a circle using curve()
    curve(sqrt(1-x^2), from = -1, to = 1, n = 200, add = TRUE, xlab = "", lwd = 2)
    curve(-sqrt(1-x^2), from = -1, to = 1, n = 200, add = TRUE, xlab = "", lwd = 2)  
    segments(0, 0, cos(data[1:input$n]), sin(data[1:input$n]))
  })
  
  # Generates onesimTable and allows it to be reactive
  output$onesimTable <- renderTable({
    # determining if each part is a success or failure
    success = sum(data[1:input$n] <= input$psuccess*2*pi)
    failure = input$n - success
    
    # binding the successes and failures together to display them in onetable
    onetable = cbind(success, failure)
    colnames(onetable) <- c("Success", "Failure")
    onetable
  })
  
  # Generates remainingTable and allows it to be reactive
  output$remainingTable <- renderTable({
    # displaying the successes and failures and displaying them in situationtable
    bindata = bindata()
    simsuccess = bindata[1:(input$sims)]
    simfailure = input$n - simsuccess
    simulationtable = cbind(simsuccess, simfailure)
    colnames(simulationtable) <- c("Success", "Failure")
    simulationtable
  })
  
  # Generates distPlot and allows it to be reactive
  output$histPlot <- renderPlot({
    # getting the data displayed in the spinner
    success = sum(data[1:input$n] <= input$psuccess*2*pi)
  
    # getting the remaining data for the simulation
    remainingsuccesses = data.frame(bindata())
    remainingsuccesses = data.frame(remainingsuccesses[0:(input$sims - 1),])
    names(remainingsuccesses)[1] <- "Success"
    
    # binding together the successes for the data
    data = rbind(success, remainingsuccesses)  

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