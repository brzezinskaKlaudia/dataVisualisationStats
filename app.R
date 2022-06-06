#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(utils)
library(vioplot)
library(ggplot2)
library(gridExtra)
library(grid)
library(GGally)
library(e1071)
library(psych)
library(moments)
summary(swiss)
summary(state.x77)
# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage(title="Mini-App",
             
             tabPanel("Swiss",
                      verbatimTextOutput("swissSummary"),
                      img(src='corPlotSwiss.png', align = "right"),
                     
                      sidebarPanel(
                        sliderInput("bins",
                                    "Number of bins:",
                                    min = 1,
                                    max = 50,
                                    value = 30),
                        selectInput("outcome", label = h3("Outcome"),
                                    choices = list("Fertility" = "Fertility",
                                                   "Agriculture" = "Agriculture",
                                                   "Examination" = "Examination",
                                                   "Education" = "Education",
                                                   "Catholic" = "Catholic",
                                                   "Infant.Mortality" = "Infant.Mortality"), selected = 1),
                        
                        selectInput("indepvar", label = h3("Explanatory variable"),
                                    choices = list("Fertility" = "Fertility",
                                                   "Agriculture" = "Agriculture",
                                                   "Examination" = "Examination",
                                                   "Education" = "Education",
                                                   "Catholic" = "Catholic",
                                                   "Infant.Mortality" = "Infant.Mortality"), selected = 1)
                      ),
                      
                      # Show a plot of the generated distribution
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Scatterplot", plotOutput("scatterplot")), # Plot
                                    tabPanel("Distribution", # Plots of distributions
                                             fluidRow(
                                               column(6, plotOutput("distribution1")),
                                               column(6, plotOutput("distribution2")))
                                    ),
                                    tabPanel("Model Summary", verbatimTextOutput("summary")), # Regression output
                                    tabPanel("Data", DT::dataTableOutput('tbl')), # Data as datatable
                                    tabPanel("Fertility", 
                                             fluidRow(
                                               column(6,plotOutput("distPlot")),
                                               column(6,plotOutput("distBoxPlot"))
                                             ),
                                             fluidRow(
                                               column(6,plotOutput("distqqnormPlot")),
                                               column(6,plotOutput("distqqline"))
                                             )
                                    ),
                                    tabPanel("Agriculture", 
                                             fluidRow(
                                               column(6,plotOutput("distPlot2")),
                                               column(6,plotOutput("distBoxPlot2"))
                                             ),
                                             fluidRow(
                                               column(6,plotOutput("distqqnormPlot2")),
                                               column(6,plotOutput("distqqline2"))
                                             )
                                    ),
                                    
                                    tabPanel("Education", 
                                             fluidRow(
                                               column(6,plotOutput("distPlot3")),
                                               column(6,plotOutput("distBoxPlot3"))
                                             ),
                                             fluidRow(
                                               column(6,plotOutput("distqqnormPlot3")),
                                               column(6,plotOutput("distqqline3"))
                                             )
                                    ),
                                    tabPanel("Catholic", 
                                             fluidRow(
                                               column(6,plotOutput("distPlot4")),
                                               column(6,plotOutput("distBoxPlot4"))
                                             ),
                                             fluidRow(
                                               column(6,plotOutput("distqqnormPlot4")),
                                               column(6,plotOutput("distqqline4"))
                                             )
                                    ),
                                    tabPanel("Infant.Mortality", 
                                             fluidRow(
                                               column(6,plotOutput("distPlot5")),
                                               column(6,plotOutput("distBoxPlot5"))
                                             ),
                                             fluidRow(
                                               column(6,plotOutput("distqqnormPlot5")),
                                               column(6,plotOutput("distqqline5"))
                                             )
                                    ),
                                   
                        )
                      )
             ),
             
             tabPanel("state.x77",
                      verbatimTextOutput("stateSummary"),
                      img(src='corPlot.png', align = "right"),
                      hr(),
                      sidebarPanel(
                        sliderInput("bins",
                                    "Number of bins:",
                                    min = 1,
                                    max = 50,
                                    value = 30),
                        selectInput("outcome2", label = h3("Outcome"),
                                    choices = list("Population" = "Population",
                                                   "Income" = "Income",
                                                   "Illiteracy" = "Illiteracy",
                                                   "Life Exp" = "Life Exp",
                                                   "Murder" = "Murder",
                                                   "HS Grad" = "HS Grad",
                                                   "Frost" = "Frost",
                                                   "Area" = "Area"), selected = 1),
                        
                        selectInput("indepvar2", label = h3("Explanatory variable"),
                                    choices = list("Population" = "Population",
                                                   "Income" = "Income",
                                                   "Illiteracy" = "Illiteracy",
                                                   "Life Exp" = "Life Exp",
                                                   "Murder" = "Murder",
                                                   "HS Grad" = "HS Grad",
                                                   "Frost" = "Frost",
                                                   "Area" = "Area"), selected = 1)
                        
                      ),
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Scatterplot", plotOutput("scatterplot2")), # Plot
                                    tabPanel("Distribution", # Plots of distributions
                                             fluidRow(
                                               column(6, plotOutput("distribution12")),
                                               column(6, plotOutput("distribution22")))
                                    ),
                                    tabPanel("Model Summary", verbatimTextOutput("summary2")), # Regression output
                                    tabPanel("Data", DT::dataTableOutput('tbl2')), # Data as datatable
                                    tabPanel("Population", 
                                             fluidRow(
                                               column(6,plotOutput("distPlot77")),
                                               column(6,plotOutput("distBoxPlot77"))
                                             ),
                                             fluidRow(
                                               column(6,plotOutput("distqqnormPlot77")),
                                               column(6,plotOutput("distqqline77"))
                                             )
                                    ),
                                    
                                    tabPanel("Income", 
                                             fluidRow(
                                               column(6,plotOutput("distPlot277")),
                                               column(6,plotOutput("distBoxPlot277"))
                                             ),
                                             fluidRow(
                                               column(6,plotOutput("distqqnormPlot277")),
                                               column(6,plotOutput("distqqline277"))
                                             )
                                    ),
                                    tabPanel("Illiteracy", 
                                             fluidRow(
                                               column(6,plotOutput("distPlot377")),
                                               column(6,plotOutput("distBoxPlot377"))
                                             ),
                                             fluidRow(
                                               column(6,plotOutput("distqqnormPlot377")),
                                               column(6,plotOutput("distqqline377"))
                                             )
                                    ),
                                    tabPanel("Life Exp", 
                                             fluidRow(
                                               column(6,plotOutput("distPlot477")),
                                               column(6,plotOutput("distBoxPlot477"))
                                             ),
                                             fluidRow(
                                               column(6,plotOutput("distqqnormPlot477")),
                                               column(6,plotOutput("distqqline477"))
                                             )
                                    ),
                                    tabPanel("Murder", 
                                             fluidRow(
                                               column(6,plotOutput("distPlot577")),
                                               column(6,plotOutput("distBoxPlot577"))
                                             ),
                                             fluidRow(
                                               column(6,plotOutput("distqqnormPlot577")),
                                               column(6,plotOutput("distqqline577"))
                                             )
                                    ),
                                    tabPanel("HS Grad", 
                                             fluidRow(
                                               column(6,plotOutput("distPlot677")),
                                               column(6,plotOutput("distBoxPlot677"))
                                             ),
                                             fluidRow(
                                               column(6,plotOutput("distqqnormPlot677")),
                                               column(6,plotOutput("distqqline677"))
                                             )
                                    ),
                                    tabPanel("Frost", 
                                             fluidRow(
                                               column(6,plotOutput("distPlot777")),
                                               column(6,plotOutput("distBoxPlot777"))
                                             ),
                                             fluidRow(
                                               column(6,plotOutput("distqqnormPlot777")),
                                               column(6,plotOutput("distqqline777"))
                                             )
                                    ),
                                    tabPanel("Area", 
                                             fluidRow(
                                               column(6,plotOutput("distPlot877")),
                                               column(6,plotOutput("distBoxPlot877"))
                                             ),
                                             fluidRow(
                                               column(6,plotOutput("distqqnormPlot877")),
                                               column(6,plotOutput("distqqline877"))
                                             )
                                    ),
                        )
                      ),
                      
             )
  ),
  

    )


# Define server logic required to draw a histogram
server <- function(input, output) {

  output$swissSummary <- renderPrint({
    summary(swiss)
  })
  # Regression output
  output$summary <- renderPrint({
    fit <- lm(swiss[,input$outcome] ~ swiss[,input$indepvar])
    names(fit$coefficients) <- c("Intercept", input$var2)
    summary(fit)
  })
  
  # Data output
  output$tbl = DT::renderDataTable({
    DT::datatable(swiss, options = list(lengthChange = FALSE))
  })
  
  
  # Scatterplot output
  output$scatterplot <- renderPlot({
    plot(swiss[,input$indepvar], swiss[,input$outcome], main="Scatterplot",
         xlab=input$indepvar, ylab=input$outcome, pch=19)
    abline(lm(swiss[,input$outcome] ~ swiss[,input$indepvar]), col="red")
    lines(lowess(swiss[,input$indepvar],swiss[,input$outcome]), col="blue")
  }, height=400)
  
  
  # Histogram output var 1
  output$distribution1 <- renderPlot({
    hist(swiss[,input$outcome], main="", xlab=input$outcome)
  }, height=300, width=300)
  
  # Histogram output var 2
  output$distribution2 <- renderPlot({
    hist(swiss[,input$indepvar], main="", xlab=input$indepvar)
  }, height=300, width=300)
  
  #Fertility
  fertility   <- swiss$Fertility
    output$distPlot <- renderPlot({
        
        bins <- seq(min(fertility), max(fertility), length.out = input$bins + 1)
        hist(fertility, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$distBoxPlot <- renderPlot({
      boxplot(fertility, main="value", horizontal=TRUE, xlab= "BoxPlot")
    })
    
    output$distqqnormPlot <- renderPlot({
      qqnorm(fertility)
      qqline(fertility, col = 2)
    })
    output$distqqline <- renderPlot({
      vioplot(fertility, main="value", horizontal = TRUE, xlab= "QQ Line")
    })
    #Agriculture 
    
    agriculture    <- swiss$Agriculture
    output$distPlot2 <- renderPlot({
      bins <- seq(min(agriculture), max(agriculture), length.out = input$bins + 1)
      hist(agriculture, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$distBoxPlot2 <- renderPlot({
      boxplot(agriculture, main="value", horizontal=TRUE, xlab= "BoxPlot")
    })
    
    output$distqqnormPlot2 <- renderPlot({
      qqnorm(agriculture)
      qqline(agriculture, col = 2)
    })
    output$distqqline2 <- renderPlot({
      vioplot(agriculture, main="Agriculture", horizontal = TRUE, xlab= "QQ Line")
    })
  #Education
    education    <- swiss$Education
    output$distPlot3 <- renderPlot({
      bins <- seq(min(education), max(education), length.out = input$bins + 1)
      hist(education, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$distBoxPlot3 <- renderPlot({
      boxplot(education, main="value", horizontal=TRUE, xlab= "BoxPlot")
    })
    
    output$distqqnormPlot3 <- renderPlot({
      qqnorm(education)
      qqline(education, col = 2)
    })
    output$distqqline3 <- renderPlot({
      vioplot(education, main="Edccation", horizontal = TRUE, xlab= "QQLine")
    })
    #Catholic 
    
    catholic    <- swiss$Catholic
    output$distPlot4 <- renderPlot({
      bins <- seq(min(catholic), max(catholic), length.out = input$bins + 1)
      hist(catholic, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$distBoxPlot4 <- renderPlot({
      boxplot(catholic, main="value", horizontal=TRUE, xlab= "BoxPlot")
    })
    
    output$distqqnormPlot4 <- renderPlot({
      qqnorm(catholic)
      qqline(catholic, col = 2)
    })
    output$distqqline4 <- renderPlot({
      vioplot(catholic, main="value", horizontal = TRUE, xlab= "QQLine")
    })
    #Infant.Mortality
    infant   <- swiss$`Infant.Mortality`
    output$distPlot5 <- renderPlot({
      bins <- seq(min(infant), max(infant), length.out = input$bins + 1)
      hist(infant, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$distBoxPlot5 <- renderPlot({
      boxplot(infant, main="value", horizontal=TRUE, xlab= "BoxPlot")
    })
    
    output$distqqnormPlot5 <- renderPlot({
      qqnorm(infant)
      qqline(infant, col = 2)
    })
    output$distqqline5 <- renderPlot({
      vioplot(infant, main="value", horizontal = TRUE, xlab= "QQLine")
    })
#################################
    state.x77 <- as.data.frame(state.x77)
    output$stateSummary <- renderPrint({
      summary(state.x77)
    })
    # Regression output
    output$summary2 <- renderPrint({
      fit <- lm(state.x77[,input$outcome2] ~ state.x77[,input$indepvar2])
      names(fit$coefficients) <- c("Intercept", input$var2)
      summary(fit)
    })
    
    # Data output
    output$tbl2 = DT::renderDataTable({
      DT::datatable(state.x77, options = list(lengthChange = FALSE))
    })
    
    
    # Scatterplot output
    output$scatterplot2 <- renderPlot({
      plot(state.x77[,input$indepvar2], state.x77[,input$outcome2], main="Scatterplot",
           xlab=input$indepvar2, ylab=input$outcome2, pch=19)
      abline(lm(state.x77[,input$outcome2] ~ state.x77[,input$indepvar2]), col="red")
      lines(lowess(state.x77[,input$indepvar2],state.x77[,input$outcome2]), col="blue")
    }, height=400)
    
    
    # Histogram output var 1
    output$distribution12 <- renderPlot({
      hist(state.x77[,input$outcome2], main="", xlab=input$outcome2)
    }, height=300, width=300)
    
    # Histogram output var 2
    output$distribution22 <- renderPlot({
      hist(state.x77[,input$indepvar2], main="", xlab=input$indepvar2)
    }, height=300, width=300)
    
    #population 
    population    <- state.x77$Population
    output$distPlot77 <- renderPlot({
      
      bins <- seq(min(population), max(population), length.out = input$bins + 1)
      hist(population, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$distBoxPlot77 <- renderPlot({
      boxplot(population, main="value", horizontal=TRUE, xlab= "BoxPlot")
    })
    
    output$distqqnormPlot77 <- renderPlot({
      qqnorm(population)
      qqline(population, col = 2)
    })
    output$distqqline77 <- renderPlot({
      vioplot(population, main="value", horizontal = TRUE, xlab= "QQLine")
    })
    #Income 
    
    income    <- state.x77$Income
    output$distPlot277 <- renderPlot({
      bins <- seq(min(income), max(income), length.out = input$bins + 1)
      hist(income, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$distBoxPlot277 <- renderPlot({
      boxplot(income, main="value", horizontal=TRUE, xlab= "BoxPlot")
    })
    
    output$distqqnormPlot277 <- renderPlot({
      qqnorm(income)
      qqline(income, col = 2)
    })
    output$distqqline277 <- renderPlot({
      vioplot(income, main="value", horizontal = TRUE, xlab= "QQLine")
    })
    #Illiteracy
    illiteracy    <- state.x77$Illiteracy
    output$distPlot377 <- renderPlot({
      bins <- seq(min(illiteracy), max(illiteracy), length.out = input$bins + 1)
      hist(illiteracy, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$distBoxPlot377 <- renderPlot({
      boxplot(illiteracy, main="value", horizontal=TRUE, xlab= "BoxPlot")
    })
    
    output$distqqnormPlot377 <- renderPlot({
      qqnorm(illiteracy)
      qqline(illiteracy, col = 2)
    })
    output$distqqline377 <- renderPlot({
      vioplot(illiteracy, main="value", horizontal = TRUE, xlab= "QQLine")
    })
    #Life Exp 
    
    lifeExp    <- state.x77$`Life Exp`
    output$distPlot477 <- renderPlot({
      bins <- seq(min(lifeExp), max(lifeExp), length.out = input$bins + 1)
      hist(lifeExp, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$distBoxPlot477 <- renderPlot({
      boxplot(lifeExp, main="value", horizontal=TRUE, xlab= "Boxplot")
    })
    
    output$distqqnormPlot477 <- renderPlot({
      qqnorm(lifeExp)
      qqline(lifeExp, col = 2)
    })
    output$distqqline477 <- renderPlot({
      vioplot(lifeExp, main="value", horizontal = TRUE, xlab= "xlabel")
    })
    #Murder
    murder   <- state.x77$Murder
    output$distPlot577 <- renderPlot({
      bins <- seq(min(murder), max(murder), length.out = input$bins + 1)
      hist(murder, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$distBoxPlot577 <- renderPlot({
      boxplot(murder, main="value", horizontal=TRUE, xlab= "BoxPlot")
    })
    
    output$distqqnormPlot577 <- renderPlot({
      qqnorm(murder)
      qqline(murder, col = 2)
    })
    output$distqqline577 <- renderPlot({
      vioplot(murder, main="value", horizontal = TRUE, xlab= "QQLine")
    })
    
    
    #HS Grad
      hsGrad   <- state.x77$`HS Grad`
    output$distPlot677 <- renderPlot({
      bins <- seq(min(hsGrad), max(hsGrad), length.out = input$bins + 1)
      hist(hsGrad, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$distBoxPlot677 <- renderPlot({
      boxplot(hsGrad, main="value", horizontal=TRUE, xlab= "BoxPlot")
    })
    
    output$distqqnormPlot677 <- renderPlot({
      qqnorm(hsGrad)
      qqline(hsGrad, col = 2)
    })
    output$distqqline677 <- renderPlot({
      vioplot(hsGrad, main="value", horizontal = TRUE, xlab= "QQLine")
    })
    
    #Frost
    frost   <- state.x77$Frost
    output$distPlot777 <- renderPlot({
      bins <- seq(min(frost), max(frost), length.out = input$bins + 1)
      hist(frost, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$distBoxPlot777 <- renderPlot({
      boxplot(frost, main="value", horizontal=TRUE, xlab= "BoxPlot")
    })
    
    output$distqqnormPlot777 <- renderPlot({
      qqnorm(frost)
      qqline(frost, col = 2)
    })
    output$distqqline777 <- renderPlot({
      vioplot(frost, main="value", horizontal = TRUE, xlab= "QQLine")
    })
    
    #Area
    area   <- state.x77$Area
    output$distPlot877 <- renderPlot({
      bins <- seq(min(area), max(area), length.out = input$bins + 1)
      hist(area, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$distBoxPlot877 <- renderPlot({
      boxplot(area, main="value", horizontal=TRUE, xlab= "BoxPlot")
    })
    
    output$distqqnormPlot877 <- renderPlot({
      qqnorm(area)
      qqline(area, col = 2)
    })
    output$distqqline877 <- renderPlot({
      vioplot(area, main="value", horizontal = TRUE, xlab= "QQLine")
    })
    
    

}

# Run the application 
shinyApp(ui = ui, server = server)
