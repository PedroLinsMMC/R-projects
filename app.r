####################################
# Data Professor                   #
# http://youtube.com/dataprofessor #
# http://github.com/dataprofessor  #
####################################

# Modified from Winston Chang, 
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html

# Concepts about Reactive programming used by Shiny, 
# https://shiny.rstudio.com/articles/reactivity-overview.html

# Load R packages
library(shiny)
library(shinythemes)
library(ggplot2)
library(RCurl)
library(gapminder)
library(tidyverse)

#x <- getURL("https://raw.githubusercontent.com/PedroLinsMMC/R-projects/main/openpowerliftingsmall.csv")
#df <- read.csv(text = x)

ds = "openpowerliftingsmall.csv" 
df = read_csv(ds)

  # Define UI
ui <- fluidPage(theme = shinytheme("united"),
                navbarPage("Powerlifting Analytics:",
                           
                           tabPanel("Lifter Analysis",
                                    # Input values
                                    sidebarPanel(
                                      HTML("<h3>Input parameters</h3>"),
                                      textInput("txt1", "Full Name:", ""),
                                      selectInput(inputId = "equipment", label = "Equipment:", 
                                                  choices = list("Raw" = "Raw", "Multi-ply" = "Multi-ply", "Single-ply" = "Single-ply", "Unlimited" = "Unlimited", "Wraps" = "wraps") 
                                      ),
                                      actionButton("submitbutton", 
                                                   "Submit", 
                                                   class = "btn btn-primary")
                                    ),
                                    
                                    mainPanel(
                                      tags$label(h3('Status/Output')), # Status/Output Text Box
                                      verbatimTextOutput('contents'),
                                      h4("Chances of a good lift:"),
                                      tableOutput('tabledata'), # Results table
                                      tableOutput('tabledata2'), # Results table
                                      tableOutput('tabledata3'), # Results table
                                      #h4("Output 1"),
                                      verbatimTextOutput("txtout")
                                    ) # mainPanel()
                                    
                           ), #tabPanel(), Home
                           
                         
                           
                ) # navbarPage()
) # fluidPage()

  
  # Define server function  
server <- function(input, output, session) {
  
  # Input Data
  
  #########
  # Squat #
  #########
  
  datasetInput <- reactive({  
    
    
    
    Name1 = input$txt1
    Comptype = input$equipment
    
    # First Squat
    sq1 = df %>%
      filter(Name == Name1) %>%
      filter(Equipment == Comptype) %>%
      count(Squat1Kg > 0)
    #colnames(sq1) = c("Squat1", "sq1")
    
    if(sq1[1,1] == TRUE){
      Squat1 = data.frame(c(1))
    } else {Squat1 = sq1[2,2] / (sq1[1,2] + sq1[2,2])
    }
    #colnames(sq1idx) = c("Squat 1")
    
    
    # Second Squat
    sq2 = df %>%
      filter(Name == Name1) %>%
      filter(Equipment == Comptype) %>%
      count(Squat2Kg > 0)
    #colnames(sq2) = c("Squat2", "sq2")
    
    if(sq2[1,1] == TRUE){
      Squat2 = data.frame(c(1))
    } else {Squat2 = sq2[2,2] / (sq2[1,2] + sq2[2,2])
    }
    #colnames(sq2idx) = c("Squat 2")
    
    
    # Thrid Squat
    sq3 = df %>%
      filter(Name == Name1) %>%
      filter(Equipment == Comptype) %>%
      count(Squat3Kg > 0)
    #colnames(sq3) = c("Squat3", "sq3")
    
    if(sq3[1,1] == TRUE){
      Squat3 = data.frame(c(1))
    } else {Squat3 = sq3[2,2] / (sq3[1,2] + sq3[2,2])
    }
    #colnames(sq3idx) = c("Squat 3")
    
    SquatIndex = (Squat1 + Squat2 + Squat3)/3
    # colnames(sq) = c("SquatIndex")
    
    sq_index = data.frame(Squat1, Squat2, Squat3, SquatIndex)
    colnames(sq_index) = c("Squat1", "Squat2", "Squat3", "SquatIndex")
    
    print(sq_index)
    
  })   
    
  
  ###############
  # Bench Press #
  ###############
  
  datasetInput2 <- reactive({  
    
    Name1 = input$txt1
    Comptype = input$equipment
    
    # First Bench Press
    bp1 = df %>%
      filter(Name == Name1) %>%
      filter(Equipment == Comptype) %>%
      count(Bench1Kg > 0)
    #colnames(bp1) = c("Bench1", "bp1")
    
    if(bp1[1,1] == TRUE){
      bp1idx = data.frame(c(1))
    } else {bp1idx = bp1[2,2] / (bp1[1,2] + bp1[2,2])
    }
    #colnames(bp1idx) = c("Bench 1")
    
    
    # Second Bench Press
    bp2 = df %>%
      filter(Name == Name1) %>%
      filter(Equipment == Comptype) %>%
      count(Bench2Kg > 0)
    #colnames(bp2) = c("Bench2", "bp2")
    
    if(bp2[1,1] == TRUE){
      bp2idx = data.frame(c(1))
    } else {bp2idx = bp2[2,2] / (bp2[1,2] + bp2[2,2])
    }
    #colnames(bp2idx) = c("Bench 2")
    
    
    # Thrid Bench Press
    bp3 = df %>%
      filter(Name == Name1) %>%
      filter(Equipment == Comptype) %>%
      count(Bench3Kg > 0)
    #colnames(bp3) = c("Bench3", "bp3")
    
    if(bp3[1,1] == TRUE){
      bp3idx = data.frame(c(1))
    } else {bp3idx = bp3[2,2] / (bp3[1,2] + bp3[2,2])
    }
    #colnames(bp3idx) = c("Bench 3")
    
    bp = (bp1idx + bp2idx + bp3idx)/3
    #colnames(bp) = c("Bench Press Index")
    
    bp_index = data.frame(bp1idx, bp2idx, bp3idx, bp)
    colnames(bp_index) = c("Bench1", "Bench2", "Bench3", "BenchIndex")
    
    print(bp_index)
    
  })
  
  ############
  # Deadlift #
  ############
  
  datasetInput3 <- reactive({  
    
    Name1 = input$txt1
    Comptype = input$equipment
    
    # First Deadlift
    dl1 = df %>%
      filter(Name == Name1) %>%
      filter(Equipment == Comptype) %>%
      count(Deadlift1Kg > 0)
    #colnames(dl1) = c("Deadlift1", "dl1")
    
    if(dl1[1,1] == TRUE){
      dl1idx = data.frame(c(1))
    } else {dl1idx = dl1[2,2] / (dl1[1,2] + dl1[2,2])
    }
    #colnames(dl1idx) = c("Deadlift 1")
    
    # Second Deadlift
    dl2 = df %>%
      filter(Name == Name1) %>%
      filter(Equipment == Comptype) %>%
      count(Deadlift2Kg > 0)
    #colnames(dl2) = c("Deadlift2", "dl2")
    
    if(dl2[1,1] == TRUE){
      dl2idx = data.frame(c(1))
    } else {dl2idx = dl2[2,2] / (dl2[1,2] + dl2[2,2])
    }
    #colnames(dl2idx) = c("Deadlift 2")
    
    # Thrid Deadlift
    dl3 = df %>%
      filter(Name == Name1) %>%
      filter(Equipment == Comptype) %>%
      count(Deadlift3Kg > 0)
    #colnames(dl3) = c("Deadlift3", "dl3")
    
    if(dl3[1,1] == TRUE){
      dl3idx = data.frame(c(1))
    } else {dl3idx = dl3[2,2] / (dl3[1,2] + dl3[2,2])
    }
    #colnames(dl3idx) = c("Deadlift 3")
    
    dl = (dl1idx + dl2idx + dl3idx)/3
    #colnames(dl) = c("Deadlift Index")
    
    dl_index = data.frame(dl1idx, dl2idx, dl3idx, dl)    
    colnames(dl_index) = c("Deadlift1", "Deadlift2", "Deadlift3", "DeadliftIndex")
    
    print(dl_index)
    
  })
  
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Squat table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
  # Bench Press table
  output$tabledata2 <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput2()) 
    } 
  })
  
  # Deadlift table
  output$tabledata3 <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput3()) 
    } 
  })
  
  } # server


  # Create Shiny object
  shinyApp(ui = ui, server = server)
