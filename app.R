library(shiny)
library(shinyjs)
options(shiny.port = 8888)
library(shinythemes)
library(ggplot2)
source('classifier.R')


options(shiny.host = "127.0.0.1")

protein_10 <- c("interleukin_6_interferon_beta_2",
"chemokine_c_c_motif_ligand_7",
"fatty_acid_binding_protein_2_intestinal",
"keratin_19","chemokine_c_c_motif_ligand_20",
"kit_ligand","s100_calcium_binding_protein_a12",
"chemokine_c_c_motif_ligand_13",
"chemokine_c_x_c_motif_ligand_5",
"fibroblast_growth_factor_21")

meta_10 <- c("	
n_acetyltryptophan",
"x2_hydroxyhippurate_salicylurate",
"orotidine",
"gluconate",
"quinate",
"x4_ethylphenylsulfate",
"methyl_4_hydroxybenzoate_sulfate",
"linoleoyl_linoleoyl_glycerol_18_2_18_2_1",
"x2_aminophenol_sulfate",
"x3_methylhistidine")






# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("united"),
                titlePanel(h1("CovClassifier-Covid-19 Risk Classifier") ),
                
                sidebarLayout(
                  sidebarPanel(
                    fluidRow(
                      column(1, offset = -1,
                             actionButton("button1", "About CovClassifier", class = "btn-primary",
                             style = "color: white;
                             left: 3%; 
                             height: 40px; 
                             width: 150px; 
                             text-align:center; 
                             text-indent: -2px; 
                             border-radius: 6px; 
                             border-width: 2px;
                             margin:5px"))),
                    
                    fluidRow(
                      column(1, offset = -1,
                             actionButton("button2", "How to use",  class = "btn-primary",
                                          style = "color: white; 
                                          left: 3%; 
                                          height: 40px; 
                                          width: 150px; 
                                          text-align:center; 
                                          text-indent: -2px; 
                                          border-radius: 6px; 
                                          border-width: 2px;
                                          margin:5px"))),
                    
                    fluidRow(
                      column(1, offset = -1,
                             actionButton("button3", "Analaysis",  class = "btn-primary",
                                          style = "color: white; 
                                          left: 3%; 
                                          height: 40px; 
                                          width: 150px; 
                                          text-align:center; 
                                          text-indent: -2px; 
                                          border-radius: 6px; 
                                          border-width: 2px;
                                          margin:5px"))),
                    fluidRow(
                      column(1, offset = -1,
                             actionButton("button5", "Figure",  class = "btn-primary",
                                          style = "color: white; 
                                          left: 3%; 
                                          height: 40px; 
                                          width: 150px; 
                                          text-align:center; 
                                          text-indent: -2px; 
                                          border-radius: 6px; 
                                          border-width: 2px;
                                          margin:5px"))),
                    
                    fluidRow(
                      column(1, offset = -1,
                             actionButton("button4", "Results",  class = "btn-primary",
                                          style = "color: white; 
                                          left: 3%; 
                                          height: 40px; 
                                          width: 150px; 
                                          text-align:center; 
                                          text-indent: -2px; 
                                          border-radius: 6px; 
                                          border-width: 2px;
                                          margin:5px"))),
                    
                    width = 3
                    
                  ),
                  
                  mainPanel(
                    htmlOutput("selected_var")
                  )
                ),
                
                tags$head(tags$style(HTML("body{background-color: #f0f8ff; color: black}")))
                
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$button1, {
    output$selected_var <- renderUI({
      HTML(paste("<h2>About CovClassifier </h2> The CovClassifier helps doctors and nurses predict the severity of the covid-19 disease that hospital patients are infected with based on gene expression markers. The gene expression markers consist of the patients plasma proteins and metabolites.", "<h2>Source of data </h2>The data used as training and testing sets for the CovClassifier is collected from Su et al., Cell . Multi-Omics Resolves a Sharp Disease-State Shift between Mild and Moderate COVID-19. Cell. 2020 Dec 10;183(6):1479-1495.e20. doi: 10.1016/j.cell.2020.10.037. Epub 2020 Oct 28. PMID: 33171100; PMCID: PMC7598382.", sep = '<br/><br/>'))
    })
  })
  
  observeEvent(input$button2, {
    output$selected_var <- renderUI({
      
      HTML(paste("<h2>How to use the risk calculator</h2>

The risk calculator requires the user to input clinical variables and upload two datasets (proteomic and metabolic data) which are samples of patients gene expression data, consisting of a set of pre defined genes. The calculator will then calculate the severity of the covid-19 disease that the patient has based on the given gene expression.

<h3>Step 1. Input clinical variables</h3>

On the left panel, there is a button named 'Analysis', click on it you will see two tabset panel. The first tab called 'Clinical Variables' contains clinical variables that need to input. 

<h3>Step 2. Upload Gene expression data</h3>

The second tabset panel named 'Proteomic/Metabolic Data' shows the upload interface for the gene expression. The input of the data should typically have the rows as the samples of the patients. The classifier only accepts .csv and excel files .


<h3>Step 3. Previewing the Gene expression data</h3>

After successfully uploading the gene expression data,a preview of the sample of the data you uploaded is shown to allow you to see if it’s correct.


<h3>Step 4. View the severity results.</h3>

On the left panel, there is a button called 'Results'. The results of the predicted severity of the disease of the sample patient are shown. The results will either show 'severe' which indicates that the covid-19 disease that the patient suffers from is severe and dangerous, and 'moderate' which means that the covid-19 disease that the patient suffers from is moderate", sep = '<br/><br/>'))
      
    })
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

          
        }else if(input$type == "metabolite"){

        }
        
      })
    
  })
  
  
  
  observeEvent(input$button4, {
    output$selected_var <- renderUI({
      mainPanel(
        dataTableOutput("proetomics"),
        dataTableOutput("meta"),
        tableOutput("min_max"),
        downloadButton("results", "Download")
        )
      })
    output$meta <- renderDataTable({
      
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
                       metabolites_table(test_proteomics, test_metabolics, test_clinical) 
                     }
        )
      }
    })
    
    output$proetomics <- renderDataTable({
      
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
                       proetomics_table(test_proteomics, test_metabolics, test_clinical) 
                     }
        )
      }
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
}

# Run the application 
shinyApp(ui = ui, server = server)






