library(shiny)
library(leaflet)
library(plotly)

choices <- c("Sex"="SEX_labelled", "Race" ="RACE", "Age" = "AGECAT", "Education" ="EDUCATION")
scatterchoices <- c("Percent Black Population" = "Percent_black", 
                     "Percent White Population" = "Percent_white",
                     "Percent Asian Population" = "Percent_asian",
                     "Percent Hispanic Population" = "Percent_hispanic",
                     "Percent Other Population" = "Percent_other",
                     "Percent with Bachelors Degree or Higher" = "Percent_bachelor_or_higher")

shinyUI(fluidPage(theme = "cerulean.css",
                  titlePanel("The Observatory Of Economic Inequality"),
                    
                    div(class="outer",
                    
                      tags$head(
                      #Include our custom CSS
                      #includeCSS("styles.css"),
                      includeScript("gomap.js")
                    ),
                   
                    sidebarLayout(
                      
                      
                      sidebarPanel(
                        tabsetPanel(
                        
                          tabPanel("Application Guide",
                                   
                                   wellPanel(
                                     
                                     h3("Find the story about inequality that matters to you!"),
                                     
                                     p("Now it's your turn to investigate the data. Follow these rules to navigate the application:"),
                                     tags$ol(
                                       tags$li(strong("Look within or between regions"),
                                               p("Choose whether you'd like a detailed view of inequality between groups within a given region, or whether you'd like to see broader trends across regions.")),
                                       tags$li(strong("Select a main comparison variable"),
                                               p("This is the variable that will be compared within each graph. The different levels of the variable will be represented by different colors.")),
                                       tags$li(strong("Select control variable"),
                                               p("After selecting a main variable, you will have the option to add up to three control variables that will further subset the data, allowing for a unique narrative."))
                                     )),
                                   
                                   
                                   fluidRow(div(align = "center",
                                                HTML('<img src="OEI_logo.png"/>')))),
                                   
                                   
                                   
                                   
                                  
                          
                          
                          tabPanel("Within County Inequality",
                        
                      
                          h3("In this panel, you can investigate inequality within a region by clicking on the map."),
                      

                              selectInput("first_choice", label = "Choose your first (main) comparison variable:", choices=choices, selected="Sex"),
                              uiOutput("first_group"),
                              uiOutput("second_choice"),
                              uiOutput("second_group"),
                              strong("Boxplot showing distribution of income:5th, 25th, 50th, 75th, 95th, and 99th percentile"),
                              plotOutput("bar", height = 200),
                              strong("Line chart showing ratio of 95th percentile of income to 50th percentile over time"),
                              plotOutput("time", height = 200)
                                ),
                        tabPanel("Between County Inequality",
                                
                        wellPanel(
                          
                          h3("In this panel, you can investigate inequality more broadly across regions"),
                          p("You can examine inequality across regions by using the zoom functionality of the map. All of the counties in range will 
                            appear on the scatterplot below."),
                          br(),
                          p("The plot shows the relationship between median income (x-axis) and the Gini coefficient (y-axis) for different counties. The line represents a weighted regression, 
                            based on your choice of input variable. Once you select an input variable, the size of the points will change proportionally to the percentage 
                            of the specified population in a given county. The larger points contain more mass, and thus they will have a larger impact in determining the slope of the line."),
                          selectInput("scatter_size", label = "Choose a variable to illustrate in the scatterplot.", choices = scatterchoices, selected = "Percent_black"),
                          plotlyOutput("scatter_plot")
                          
                        ))
                        )),
                      
                      mainPanel(
                                
                                leafletOutput("map", width="100%", height=800)
                          
                                )
                            
                            )
                      ),
                            

                

                                
                                tags$div(id="cite",
                                         'Data compiled for ', tags$em('MSiA 411: Data Visualization'), ' by Terry, Elie, Tina, and Bala.'
                                )
                            )
                      )
                   

