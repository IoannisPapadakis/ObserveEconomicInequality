library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(leaflet)
library(ggplot2)
library(tidyr)
library(plotly)


#setwd("T:/ObserveEconomicInequality")
#us_county <- readOGR("shp/cb_2014_us_county_500k.shp",layer = "cb_2014_us_county_500k", verbose = FALSE)
#dat <- readShapeSpatial("shp/cb_2014_us_county_500k.shp")
shinyServer(function(input, output, session) {
  
  load("data/Gender.rdata")
  load("data/Age.rdata")
  load("data/Age_and_Education.rdata")
  load("data/Education.rdata")
  load("data/Gender_and_Educ.rdata")
  load("data/Gender_and_Race.rdata")
  load("data/Race_and_Education.rdata")
  load("data/Race.rdata")
  load("data/Age_and_Race.rdata")
  load("data/Gender_and_Race.rdata")
  load("data/Gender_and_Age.rdata")
  load("data/mapFinal.rdata")
  load("data/county_scatter.rdata")
  sex <- c("Male", "Female")
  race <- c("Asian","Black","Hispanic","Other","White" )
  age <- c("18-25", "25-35", "35-45", "45-55", "55-65", "65+")
  educ <- c("less than HS"="less than HS","HS or GED" = "HS or GED",
            "Associates/Some College" = "Associates/Some College", 
            "Bachelors" = "Bachelors", 
            "Masters/Professional" = "Masters/Professional",
            "Doctoral" = "Doctoral" )
  choices <- c("Sex"="SEX_labelled", "Race"="RACE", "Age"="AGECAT", "Education"="EDUCATION")
  
  
 

  
  output$first_group <- renderUI({
    
    if (input$first_choice == "SEX_labelled"){
      
      selectInput(inputId = "if_sex", label = "Select sexes to display:", choices = sex, multiple = TRUE, selected=c("Male", "Female"))
    
    } else if (input$first_choice == "EDUCATION") {
      
      selectInput(inputId = "if_education", label = "Select levels of education to display:", choices = educ, multiple = TRUE, selected=c("HS", "Bachelors"))
    
    } else if (input$first_choice == "AGECAT") {
      
      selectInput(inputId = "if_age", label = "Select age groups to display:", choices = age, multiple = TRUE, selected = "25-35")
    
    } else {
      
      selectInput(inputId = "if_race", label = "Select races to display:", choices = race, multiple = TRUE, selected = c("African American", "White"))}
  
  })
  
  
  output$second_choice <- renderUI({
    
    selectInput("second_choice_ui", label = "Select a control variable", choices = c(choices[choices != input$first_choice], "None"), selected = "None")
  
  })
  
  output$second_group <- renderUI({
    
    if (input$second_choice_ui == "SEX_labelled"){
      
      selectInput(inputId = "if_sex", label = "Select sexes to display:", choices = sex, multiple = TRUE, selected=c("Male", "Female"))
    
    } else if (input$second_choice_ui == "EDUCATION") {
      
      selectInput(inputId = "if_education", label = "Select levels of education to display:", choices = educ, multiple = TRUE, selected=c("HS", "Bachelors"))
    
    } else if (input$second_choice_ui == "AGECAT") {
      
      selectInput(inputId = "if_age", label = "Select age groups to display:", choices = age, multiple = TRUE, selected = "25-35")
    
    } else if (input$second_choice_ui == "RACE"){
      
      selectInput(inputId = "if_race", label = "Select races to display:", choices = race, multiple = TRUE, selected = c("African American", "White"))
    
    } else {}
  
    })
  
  groups <- reactive({
      
      event <- input$map_shape_click

    if (is.null(input$map_shape_click)) {
       
        return(NULL)
      }
    else {
      
      if (input$first_choice == "AGECAT" & input$second_choice_ui == "None"){
        
        subset(Age, AGECAT %in% input$if_age & county == event$id)
        
      } else if (input$first_choice == "SEX_labelled" & input$second_choice_ui == "None"){                  
        
        subset(gender, SEX_labelled %in% input$if_sex & county == event$id)
        
      } else if (input$first_choice == "RACE" & input$second_choice_ui == "None"){
        
        subset(Race, RACE %in% input$if_race & county == event$id)
        
      } else if (input$first_choice == "EDUCATION" & input$second_choice_ui == "None"){
        
        subset(Education, EDUCATION %in% input$if_education & county == event$id)
        
      }
    
    ##two inputs
 
        else if ((input$first_choice == "AGECAT" & input$second_choice_ui == "SEX_labelled") | (input$first_choice == "SEX_labelled" & input$second_choice_ui == "AGECAT")) {
        
        subset(Gender_and_Age, AGECAT %in% input$if_age & SEX_labelled %in% input$if_sex & county == event$id)
        
      } else if ((input$first_choice == "AGECAT" & input$second_choice_ui == "RACE")|(input$first_choice == "RACE" & input$second_choice_ui == "AGECAT")){
        
        subset(Age_and_Race, AGECAT %in% input$if_age & RACE %in% input$if_race & county == event$id)
        
      } else if ((input$first_choice == "AGECAT" & input$second_choice_ui == "EDUCATION")|(input$first_choice == "EDUCATION" & input$second_choice_ui == "AGECAT")){
        
        subset(Age_and_Education, AGECAT %in% input$if_age & EDUCATION %in% input$if_education & county == event$id)
        
      } else if ((input$first_choice == "SEX_labelled" & input$second_choice_ui == "RACE")|(input$first_choice == "RACE" & input$second_choice_ui == "SEX_labelled")){
        
        subset(Gender_and_Race, SEX_labelled %in% input$if_sex & RACE %in% input$if_race & county == event$id)
        
      } else if ((input$first_choice == "SEX_labelled" & input$second_choice_ui == "EDUCATION")|(input$first_choice == "EDUCATION" & input$second_choice_ui == "SEX_labelled")){
       
        subset(Gender_and_Educ, SEX_labelled %in% input$if_sex & EDUCATION %in% input$if_education & county == event$id)
        
      } else if ((input$first_choice == "EDUCATION" & input$second_choice_ui == "RACE")|(input$first_choice == "RACE" & input$second_choice_ui == "EDUCATION")){
        
        subset(Race_and_Education, RACE %in% input$if_race & EDUCATION %in% input$if_education & county == event$id)
        
      }
    }
  })
  
    county_zoom <- reactive({
    if (is.null(input$map_bounds))
      return(county_scatter[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(county_scatter,
           lat >= latRng[1] & lat <= latRng[2] &
             long >= lngRng[1] & long <= lngRng[2])
  })
 
  output$map <- renderLeaflet({
    
    popup <- paste0("<strong>Name: </strong>", 
                    
                    paste(us_county$NAME," ",us_county$State), "<br> <strong> Gini Coefficient: </strong>",
                    us_county$Gini)
    
    geoID <- as.vector(us_county@data$GEOID)
   
     pal <- colorNumeric(
      palette = "Reds",
      domain = us_county@data$Gini
    )
     
    leaflet() %>%
      
      addTiles(
        
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      
        ) %>%
      
      addPolygons(
        
        data = us_county, stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, 
        color = ~pal(Gini), popup = popup, layerId = geoID
      ) %>%
      
      addLegend("bottomright", pal = pal, values = us_county$Gini,
                title = "Gini Coefficient",
                opacity = 1
      ) %>%
      
      
      setView(lng = -81.99, lat = 32.77, zoom = 4
              
      )
      
      

  })
  
  output$scatter_plot <- renderPlotly({
    tmp <- county_zoom()
    
    gg <- ggplot(tmp, aes_string( x = "median_income", y = "Gini", size = input$scatter_size)) +
      geom_point(aes(text = paste("County =", cntyname)), shape = 21, fill = "orangered", colour = "black", alpha = 0.6) +
      stat_smooth(method = "lm", aes_string(weight = input$scatter_size)) + 
      guides(size = FALSE, weight = FALSE, colour = FALSE) +
      theme(panel.background = element_rect(fill= "white")) + 
      geom_vline(xintercept = (min(tmp$median_income) - 100)) + 
      geom_hline(yintercept = (min(tmp$Gini) - 0.02))

    ggplotly(gg)
    
    # In case we want to use plotly 
    #plot_ly(tmp, x = median_income, y = Gini, size = paste(input$scatter_size), mode = "markers")
    })
  
  output$bar <- renderPlot({
    
    if (is.null(input$map_shape_click)) {
        return(NULL)
        }
    
        tmp <- subset(groups(), YEAR == 2010)
    
        p <- ggplot(tmp, aes_string(x = input$first_choice, fill=input$first_choice)) + 
          geom_errorbar(aes(ymin=fifth,ymax=ninetyfive), width=0.4) +
          geom_boxplot(aes(lower = twentyfive, upper = seventyfive, middle = fifty, ymin = fifth, ymax = ninetyfive), stat="identity") +
          geom_point(aes_string(x= input$first_choice, y = "ninetynine", fill = input$first_choice), size=4, shape = 21, colour = "black") + 
          #scale_y_continuous(limits=c(0,max(tmp$ninetynine)+10))+
          labs(x="")+
          ylab("Personal Income,2015 Dollars")+
          theme(axis.ticks.x=element_blank(), axis.text.x=element_blank(), text=element_text(size=10), 
                panel.background = element_rect(fill="white"))+
          scale_fill_brewer(toString(input$first_choice), palette = "Reds")
          #scale_color_brewer(palette = "Blues")
 
    
    if (input$second_choice_ui == "None") {
      p 
      } else {
      p <- p + facet_grid(paste("~", input$second_choice_ui), scales="free")
      }
    
      p

  })
  
  
  
  output$time <- renderPlot({
    
    
    tmp <- groups()
    tmp$ratio <- tmp$ninetyfive/tmp$fifty
    
    if (is.null(input$map_shape_click)) {
      
      return(NULL)
    }
    
   
    
    p <- ggplot(tmp, aes_string(x = "YEAR", y = "ratio", colour = input$first_choice)) + 
            geom_line(stat = "identity", size = 1.2) +
            geom_point(aes_string(x = "YEAR", y = "ratio", fill = input$first_choice), colour = "black", shape = 21, size = 2) + 
            ylab("Ratio")+
            theme(axis.ticks.x=element_blank(), text=element_text(size=10), panel.background = element_rect(fill = "white"))+
            scale_colour_brewer(palette = "Reds") +
            scale_fill_brewer(palette = "Reds") +
            guides(fill = FALSE)
    
    
    if (input$second_choice_ui == "None") {
      
      p 
      
    } else {
      
      p <- p + facet_grid(paste("~", input$second_choice_ui), scales="free")
      
    }
    
    p
    })
  })

  


  
