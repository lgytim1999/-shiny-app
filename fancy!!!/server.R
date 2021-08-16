# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

# Work PC uses: rsconnect 0.4.3 

# Initialize the user selections and tooltip (title)
selections <- vector(mode = "character", length = 0)
edgeLabels <- vector(mode = "character", length = 0)
tips <- vector(mode = "character", length = 0)

# Initialize empty data.frames for nodes and edges
nodes <- data.frame(id = integer(), label = character(), title = character(), 
                    shape = character(), icon.face = character(), icon.code = character(), 
                    color = character(), stringsAsFactors = FALSE)

# Initialize edges data
edges <- data.frame(from = numeric(), to = numeric(), length = numeric())

# Load all datasets
load("./data/item_pairs_30.rda")  # Load data  - old files end in rds, e.g., "item_pairs.rds"
load("./data/item_pairs_15.rda")
# load("./data/item_ref.rds")  
load("./data/item_ref_30.rda")
item_ref <- item_ref_30; rm(item_ref_30)

source("./www/make_breaks.R")
source('./classifier.R')

shinyServer(function(input, output, session) {
    # Start Button -------------------------------------------------------------
    observeEvent(input$startBtn, {
        updateNavbarPage(session, "navBar",
                         selected = "careerPF"
        )
    })
    
    
    observeEvent(input$button3, {
        
        output$contents1 <- renderTable({
            # input$file1 will be NULL initially. After the user selects
            # and uploads a file, it will be a data frame with 'name',
            # 'size', 'type', and 'datapath' columns. The 'datapath'
            # column will contain the local filenames where the data can
            # be found.
            inFile1 <- input$file1
            if (is.null(inFile1))
                return(NULL)
            read.csv(inFile1$datapath, header =TRUE ,sep=",")
        })
        
        output$contents2 <- renderTable({
            # input$file1 will be NULL initially. After the user selects
            # and uploads a file, it will be a data frame with 'name',
            # 'size', 'type', and 'datapath' columns. The 'datapath'
            # column will contain the local filenames where the data can
            # be found.
            inFile2 <- input$file2
            if (is.null(inFile2))
                return(NULL)
            read.csv(inFile2$datapath, header =TRUE ,sep=",")
        })
        
        output$contents3 <- renderTable({
            # input$file1 will be NULL initially. After the user selects
            # and uploads a file, it will be a data frame with 'name',
            # 'size', 'type', and 'datapath' columns. The 'datapath'
            # column will contain the local filenames where the data can
            # be found.
            inFile3 <- input$file3
            if (is.null(inFile3))
                return(NULL)
            read.csv(inFile3$datapath, header =TRUE ,sep=",")
        })
        
        output$selected_var <- renderUI({
            
            mainPanel(fileInput('file1', HTML(paste("<h2>Upload your sample of proetomics data: </h2> <br/>Upload a csv or excel file (Data needs to be uploaded with sample patients as rows and in .csv or .xlsx files)<br/>  ", sep = '<br/><br/> ')), accept=c( "text/csv","text/comma-separated-values","text/plain",".csv", '.xlsx') ), tableOutput("contents1"),
                      fileInput('file2', HTML(paste("<h2>Upload your sample of metabolics data: </h2> <br/>Upload a csv or excel file (Data needs to be uploaded with sample patients as rows and in .csv or .xlsx files)<br/>  ", sep = '<br/><br/> ')), accept=c( "text/csv","text/comma-separated-values","text/plain",".csv", '.xlsx') ), tableOutput("contents2"), 
                      fileInput('file3', HTML(paste("<h2>Upload your sample of clinical data: </h2> <br/>Upload a csv or excel file (Data needs to be uploaded with sample patients as rows and in .csv or .xlsx files)<br/>  ", sep = '<br/><br/> ')), accept=c( "text/csv","text/comma-separated-values","text/plain",".csv", '.xlsx') ), tableOutput("contents3")
            )
        })
    })
    
    observeEvent(input$button5, {
        output$selected_var <- renderUI({
            
            mainPanel("top 10 protein")
            
            # Sidebar layout with input and output definitions ----
            sidebarLayout(
                # Sidebar panel for inputs ----
                sidebarPanel(
                    
                    # Input: Slider for the number of bins ----
                    selectInput(inputId="color1",label="Graph Color",choices = c("Red"="Red","Blue"="Blue","Green"="Green"),
                                selected = "Blue",multiple = F),
                    
                    radioButtons(inputId = "border1",label = "Graph Border",choices = c("Black"="#000000","White"="#ffffff")),
                    
                    selectInput(inputId="type",label="Data Type",choices = c("protein"="protein",
                                                                             "metabolite"="metabolite"),
                                selected = "protein",multiple = F),
                    
                    conditionalPanel(condition = "input.type == 'protein'",
                                     sliderInput(inputId = "pro1",
                                                 label = "Number of proteins:",
                                                 min = 1,
                                                 max = 10,
                                                 value = 5)
                    ),
                    
                    conditionalPanel(condition = "input.type == 'metabolite'",
                                     sliderInput(inputId = "meta1",
                                                 label = "Number of metabolites:",
                                                 min = 1,
                                                 max = 10,
                                                 value = 5),
                    ),
                    
                    
                ),
                mainPanel(
                    plotOutput("ProteinPlot"),
                )
            )})
        
        # Define server logic required to draw a histogram ----
        
        output$ProteinPlot <- renderPlot({

            if(input$color1=="Red"){
                sColor = "#ff3300"
            }else if(input$color1=="Blue"){
                sColor = "#3399ff"
            }else if(input$color1=="Green"){
                sColor = "#66ff33"
            }
            
            
            
            if(input$type == "protein"){
                #hist(hist(),fill=sColor)
                #make_hist()
                
            }else if(input$type == "metabolite"){
                # hist(x,x, breaks = breaks,,fill=sColor)
                #make_hist()
            }
            
        })
        
    })
    
    
    
    observeEvent(input$button4, {
        output$selected_var <- renderUI({
            mainPanel(
                tableOutput("min_max"),
                downloadButton("results", "Download")
            )
        })
        
        output$min_max <- renderTable({
            
            inFile1 <- input$file1
            inFile2 <- input$file2
            inFile3 <- input$file3
            if (is.null(inFile1) ||is.null(inFile2)  ||is.null(inFile3)){
                return(NULL)
            }
            else{
                withProgress(message = 'Predictions in progress. Please wait ...', 
                             {
                                 par(mar = c(2, 2, 2, 2))
                                 test_proteomics = read.csv(inFile1$datapath, header = TRUE, sep = ",")
                                 test_metabolics = read.csv(inFile2$datapath, header = TRUE, sep = ",")
                                 test_clinical = read.csv(inFile3$datapath, header = TRUE, sep = ",")
                                 results(test_proteomics, test_metabolics, test_clinical) 
                             }
                )
            }
        })
        
        output$results <- downloadHandler(
            filename = function() {
                paste("results", ".csv", sep = "")
            },
            content = function(file) {
                write.csv(results(test_proteomics, test_metabolics, test_clinical), file = "results.csv", row.names = FALSE)
            }
        )
    })
    
    
})