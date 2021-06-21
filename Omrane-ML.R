#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(randomForest)
dataml <- read.csv(file = "./data-ml.csv", header = TRUE, row.names = 1)
regions = c(distinct(dataml,Description))
designation = distinct(dataml,Designation)
typesUnite = c(distinct(dataml,TypeUnité))
typesProduit = c(distinct(dataml,TypeProduit))
#set.seed(100)
#train <- sample(nrow(dataml), 0.7*nrow(dataml), replace = FALSE)
#TrainSet <- dataml[train,]
model <- randomForest(PrixTotal ~ ., data = dataml, ntree = 500, mtry = 4, importance = TRUE)
#predTrain <- predict(model, TrainSet)
#table(predTrain, TrainSet$PrixTotal) 
print(model)





library(shiny)
library(shinythemes)
# Define UI for application that draws a histogram
ui <- fluidPage( theme = shinytheme("superhero"),
    shinythemes::themeSelector(), 
    # Application title
    titlePanel("Omrane Products Price Prediction"),
    tags$h6("Here you can chose the different specification of your desired product and our ML model will predict the total price for you."),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        
        sidebarPanel(
            
            HTML("<h3>Input parameters</h3>"),
            
            selectInput(
                "Region",
                "Choisir une region",
                choices =  regions
            ),
            #selectInput(
            #    "typeUnite",
            #    "Choisir un type d'unité",
            #    choices = typesUnite
            #),
            selectInput(
                "typeProduit",
                "Choisir un type de produit",
                choices = typesProduit
            ),
            #
            sliderInput(
                "Superficie",
                "Superficie :",
                min = min(dataml$Superficie),
                max = max(dataml$Superficie),
                value=0,
                step=5
            ),
            
            sliderInput(
                "PrixUnitaire",
                "Prix unitaire:",
                min = min(dataml$PrixUnitaire),
                max = max(dataml$PrixUnitaire),
                value = 100,
                step=100
            ),
            #actionButton("submit", "Submit", class= "btn btn-primary")
            submitButton("submit")
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
            tags$label(h3('Status/Output')),
            verbatimTextOutput('contents'),
            textOutput("result")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    datasetInput <- reactive({  
        
        # Region, Type Unite, Type Produit, Superficie  
        df <- data.frame(
            Name = c("Region",
                     "typeProduit",
                     "Superficie",
                     "PrixUnitaire"),
            Value = as.character(c(input$Region,
                                   input$typeProduit,
                                   input$Superficie,
                                   input$PrixUnitaire)
                                 ),
            stringsAsFactors = FALSE)
        
        PrixTotal <- "PrixTotal"
        df <- rbind(df, PrixTotal)
        input <- t(df)
        write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
        
        test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
        
        #test$outlook <- factor(test$outlook, levels = c("overcast", "rainy", "sunny"))
        
        
        Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
        print(Output)
    })
    
   output$contents <- renderPrint({
       #isolate(datasetInput()) 
       datasetInput()
       paste("You chose", input$Region)
   })
   
   output$result <- renderText({
        paste("You chose", input$Region)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)
