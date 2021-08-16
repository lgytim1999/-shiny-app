# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
# Developed with R version 3.3.2 (64-bit)
library(dplyr)
library(stringr)
library(png)
library(shinyjs)
library(DT)
library(visNetwork)
library(rintrojs)

source("carouselPanel.R")
source('classifier.R')

# Panel div for visualization
# override the currently broken definition in shinyLP version 1.1.0
panel_div <- function(class_type, content) {
    div(class = sprintf("panel panel-%s", class_type),
        div(class = "panel-body", content)
    )
}

shinyUI(navbarPage(title = img(src="title.png", height = "40px"), id = "navBar",
                   theme = "paper.css",
                   collapsible = TRUE,
                   inverse = TRUE,
                   windowTitle = "Los Angeles County Career PathFinder",
                   position = "fixed-top",
                   footer = includeHTML("./www/include_footer.html"),
                   header = tags$style(
                       ".navbar-right {
                       float: right !important;
                       }",
                       "body {padding-top: 75px;}"),
                   
                   tabPanel("HOW TO USE", value = "how to use",
                            
                            shinyjs::useShinyjs(),
                            
                            tags$head(tags$script(HTML('
                                                       var fakeClick = function(tabName) {
                                                       var dropdownList = document.getElementsByTagName("a");
                                                       for (var i = 0; i < dropdownList.length; i++) {
                                                       var link = dropdownList[i];
                                                       if(link.getAttribute("data-value") == tabName) {
                                                       link.click();
                                                       };
                                                       }
                                                       };
                                                       '))),
                            fluidRow(
                                HTML("
                                     <section class='wrapper'>
                                     <h2 class='parallax'>COVID-19 Classifier</h2>
                                     <p class='parallax_description'>Classify whether the patient is mild or severe.</p>
                                     </section>
                                     ")
                                ),
                            
                            # WHAT
                            fluidRow(
                                column(3),
                                column(6,
                                       shiny::HTML("<br><br><center> <h1>How to use the risk calculator</h1> </center><br>"),
                                       shiny::HTML("<h5>The risk calculator requires the user to input clinical variables and upload two datasets (proteomic and metabolic data) 
                                       which are samples of patients gene expression data, 
                                       consisting of a set of pre defined genes. 
                                       The calculator will then calculate the severity of the covid-19 disease that the patient has based on the given gene expression.</h5>")
                                       ),
                                column(3)
                                       ),
                            
                            fluidRow(
                                
                                style = "height:50px;"),
                            
                            # PAGE BREAK
                            tags$hr(),
                            
                            # HOW
                            fluidRow(
                                column(3),
                                column(6,
                                       shiny::HTML("<br><br><center> <h1>Step 1. Input clinical variables</h1> </center><br>"),
                                       shiny::HTML("<h5>On the left panel, there is a button named 'Analysis', 
                                                   click on it you will see three places to upload your file. 
                                                   Simply click browse and choose your file. </h5>")
                                       ),
                                column(3)
                                       ),
                            
                            fluidRow(
                                
                                style = "height:50px;"),
                            
                            # PAGE BREAK
                            tags$hr(),
                            
                            # WHERE
                            fluidRow(
                                column(3),
                                column(6,
                                       shiny::HTML("<br><br><center> <h1>Step 2. Previewing the Gene expression data</h1> </center><br>"),
                                       shiny::HTML("<h5>After successfully uploading the gene expression data,a preview of the sample of the data 
                                                   you uploaded is shown to allow you to see if it’s correct.</h5>")
                                       ),
                                column(3)
                                       ),
                            
                            fluidRow(
                                
                                style = "height:50px;"),
                            
                            # PAGE BREAK
                            tags$hr(),
                            
                            # HOW TO START
                            fluidRow(
                                column(3),
                                column(6,
                                       shiny::HTML("<br><br><center> <h1>Step 3. View the severity results.</h1> </center><br>"),
                                       shiny::HTML("<h5>On the left panel, there is a button called 'Results'. 
                                                   The results of the predicted severity of the disease of the sample patient are shown. 
                                                   The results will either show 'severe' which indicates that the covid-19 disease that the patient suffers from is 
                                                   severe and dangerous, and 'moderate' which means that the covid-19 disease that the patient suffers from is moderate</h5>")
                                       ),
                                column(3)
                                       ),
                            
                          
                            
                            fluidRow(
                                
                                style = "height:50px;"),
                            
                            # PAGE BREAK
                            tags$hr(),
                            
                            fluidRow(shiny::HTML("<br><br><center> <h1>Ready to Get Started?</h1> </center>
                                                 <br>")
                            ),
                            fluidRow(
                                column(3),
                                column(6,
                                       tags$div(align = "center", 
                                                tags$a("Start", 
                                                       onclick="fakeClick('classifier')", 
                                                       class="btn btn-primary btn-lg")
                                       )
                                ),
                                column(3)
                            ),
                            fluidRow(style = "height:25px;"
                            )
                            
                            ), # Closes the first tabPanel called "Home"
                   
                   tabPanel("CLASSIFIER", value = "classifier",
                            
                            sidebarLayout( 
                              sidebarPanel(
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
                            )  # Closes the sidebarLayout
                   ),  # Closes the second tabPanel called "Career PathFinder"
                   
                   tabPanel("ABOUT", value = "about",
                            
                            fluidRow(
                                shiny::HTML("<br><br><center> 
                                            <h1>About CovClassifier</h1> 
                                            <h4>What's behind the data.</h4>
                                            </center>
                                            <br>
                                            <br>"),
                                style = "height:250px;"),
                            
                            fluidRow(
                              column(3),
                              column(6,
                                     shiny::HTML("<h5>The CovClassifier helps doctors and nurses predict the severity of the covid-19 disease 
                                    that hospital patients are infected with based on gene expression markers. 
                                                 The gene expression markers consist of the patients plasma proteins and metabolites.</h5>")
                              ),
                              column(3)
                            ),

                            # TEAM BIO
                            fluidRow(
                                column(3),
                                column(6,
                                       shiny::HTML("<br><br><center> <h5>About the team</h5> </center><br>"),
                                       shiny::HTML("<h6>WE ARE THE BEST!</h6>")
                                       ),
                                column(3)
                                       ),
                            
                            fluidRow(
                                
                                style = "height:50px;"),
                            
                            fluidRow(
                              column(3),
                                # Chuyang Zhou
                                column(2,
                                       div(class="panel panel-default", 
                                           div(class="panel-body",  width = "600px",
                                               align = "center",
                                               div(
                                                   tags$img(src = "man_beard_1.svg", 
                                                            width = "50px", height = "50px")
                                               ),
                                               div(
                                                   tags$h5("Chuyang Zhou"),
                                                   tags$h6( tags$i("Visionary & Project Lead"))
                                               ),
                                               div(
                                                   "My County career path started as a Human Resources Analyst."
                                               )
                                           )
                                       )
                                ),
                                # Wei Yao
                                column(2,
                                       div(class="panel panel-default",
                                           div(class="panel-body",  width = "600px", 
                                               align = "center",
                                               div(
                                                   tags$img(src = "man.svg", 
                                                            width = "50px", height = "50px")
                                               ),
                                               div(
                                                   tags$h5("Wei Yao"),
                                                   tags$h6( tags$i("Data Scientist & Programmer"))
                                               ),
                                               div(
                                                   "My County career path started as an Intermediate Typist Clerk."
                                               )
                                           )
                                       )
                                ),
                                # Kevin Marcelino
                                column(2,
                                       div(class="panel panel-default",
                                           div(class="panel-body",  width = "600px", 
                                               align = "center",
                                               div(
                                                   tags$img(src = "woman.svg", 
                                                            width = "50px", height = "50px")),
                                               div(
                                                   tags$h5("Kevin Marcelino"),
                                                   tags$h6( tags$i("Writer"))
                                               ),
                                               div(
                                                   "My County career path started as an Administrative Assistant."
                                               )
                                           )
                                       )
                                ),
                                column(3)
                                ),
                                fluidRow(
                                  
                                  style = "height:50px;"),
                                
                                fluidRow(
                                  column(3),
                                # Guangyi Liu
                                column(2,
                                       div(class="panel panel-default", 
                                           div(class="panel-body",  width = "600px",
                                               align = "center",
                                               div(
                                                 tags$img(src = "man_beard_1.svg", 
                                                          width = "50px", height = "50px")
                                               ),
                                               div(
                                                 tags$h5("Guangyi Liu"),
                                                 tags$h6( tags$i("Visionary & Project Lead"))
                                               ),
                                               div(
                                                 "My County career path started as a Human Resources Analyst."
                                               )
                                           )
                                       )
                                ),
                                # Tinghui Li
                                column(2,
                                       div(class="panel panel-default",
                                           div(class="panel-body",  width = "600px", 
                                               align = "center",
                                               div(
                                                 tags$img(src = "man.svg", 
                                                          width = "50px", height = "50px")
                                               ),
                                               div(
                                                 tags$h5("Tinghui Li"),
                                                 tags$h6( tags$i("Data Scientist & Programmer"))
                                               ),
                                               div(
                                                 "My County career path started as an Intermediate Typist Clerk."
                                               )
                                           )
                                       )
                                ),
                                # Chunyue Zheng
                                column(2,
                                       div(class="panel panel-default",
                                           div(class="panel-body",  width = "600px", 
                                               align = "center",
                                               div(
                                                 tags$img(src = "woman.svg", 
                                                          width = "50px", height = "50px")),
                                               div(
                                                 tags$h5("Chunyue Zheng"),
                                                 tags$h6( tags$i("Writer"))
                                               ),
                                               div(
                                                 "My County career path started as an Administrative Assistant."
                                               )
                                           )
                                       )
                                ),
                                column(3)
                            ),
                            fluidRow(style = "height:150px;")
                                )  # Closes About tab
                   
                            )
        
                   )