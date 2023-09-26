source("Base.R")
library(tidyverse)
library(dplyr)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("rankr"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          actionButton("run", "Start"),
          textAreaInput("names", "Names", value = "Ross
Rachel
Monica
Chandler
Phoebe
Joey", width = NULL, placeholder = NULL),
          textAreaInput("descriptions", "Descriptions (Optional)", value = "Archaeologist
Waitress
Chef
Transponster
Masseuse
Actor", width = NULL, placeholder = NULL),
          p("Names can be copied from Excel or entered manually with line breaks between each name. If descriptions are provided, the number of descriptions must match the number of names. Duplicate names cannot be used."),
          br(),
          p("Ranking algorithm used is standard Elo. 25% of draws are maximal possible divergence, 25% are minimal, and 50% are random. Some names will be drawn more than others, but the sampling should ensure a solid floor.")
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
          fluidRow(
            column(6,offset=0,align = 'center',
                   actionButton("select_draw", " Tie ")
            )),
          fluidRow(
            column(3,offset=0,align = 'center',
                   actionButton("select_1", " --- 1 --- "),
                   div(style="text-align:center;
        outline-style: solid;
                font-weight:bold;
        font-size:15px;
        position:relative;",
                       textOutput("Name1")
                   )
            ),
            column(3,offset=0,align = 'center',
                   actionButton("select_2", " --- 2 --- "),
                   div(style="text-align:center;
        outline-style: solid;
        font-weight:bold;
        font-size:15px;
        position:relative;",
                       textOutput("Name2")
                   )
            )),
          
          fluidRow(
            column(3,offset=0,align = 'center',
                   div(style="text-align:center;
        font-size:10px;
                padding-top: 10px;
        position:relative;",
                       textOutput("Desc1")
                   )
            ),
            column(3,offset=0,align = 'center',
                   div(style="text-align:center;
        font-size:10px;
                padding-top: 10px;
        position:relative;",
                       textOutput("Desc2")
                   )
            )),
          fluidRow(
),
          plotOutput("distPlot"),
          tableOutput("table"),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  values <- reactiveValues()
  values$runner <- c()
  values$aq_list <- c()
  values$runtime <- 1
  
  observeEvent(input$run, {
    
    values$runner <- c()
    values$aq_list <- c()
    values$runtime <- 1

    start_names <- input$names
    start_names <- str_split(start_names,"\n")
    start_names <- start_names[[1]]
    start_names <- start_names[start_names |> nchar()>2]
    start_names <- data.frame(start_names)
    
    start_desc <- input$descriptions
    if(start_desc==""){
       start_desc <- "Blah"}
      
    start_desc <- str_split(start_desc,"\n")
    start_desc <- start_desc[[1]]
    if(length(start_desc)<3)
    {
      start_names$description <- " "
      colnames(start_names) <- c("name","description")
    }
    start_desc <- start_desc[start_desc |> nchar()>2]
    if(length(start_desc)<3 || length(start_desc)!=nrow(start_names))
    {
      start_names$description <- " "
      colnames(start_names) <- c("name","description")
    }
    if(length(start_desc)>=3 & length(start_desc)==nrow(start_names))
    {
      start_desc <- start_desc[start_desc |> nchar()>2]
      start_names$description <- start_desc
      colnames(start_names) <- c("name","description")
    }   
    
    values$frame <- initialize(start_names$name, start_names$description)
    values$merger <- list_setup(start_names$name)
    values$aq_list[[1]] <- "Test_Test"
    values$runner <- extract_recommended_names(values$frame, values$aq_list, values$runtime, values$merger)
    values$aq_list <- values$runner[[3]]
    output$Name1 <- renderText(values$runner[[1]])
    output$Name2 <- renderText(values$runner[[2]])
    output$Desc1 <- renderText(get_desc(values$frame,values$runner[[1]]))
    output$Desc2 <- renderText(get_desc(values$frame,values$runner[[2]]))
    output$table <- renderTable(frame_burn(values$frame))
    output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      # draw the histogram with the specified number of bins
      hist(values$frame$score, breaks = 15, col = 'darkgray', border = 'white',
           xlab = '',
           main = 'Histogram of scores')
    })
    
  })
  
  observeEvent(input$select_1, {
    if(values$runtime==(nrow(values$merger)-1))
    {
      values$runtime <- 0
    }
    values$runtime <- values$runtime + 1
    values$frame <- update(values$frame, values$runner[[1]], values$runner[[2]])
    values$runner <- extract_recommended_names(values$frame, values$aq_list, values$runtime, values$merger)
    values$aq_list <- values$runner[[3]]
    output$Name1 <- renderText(values$runner[[1]])
    output$Name2 <- renderText(values$runner[[2]])
    output$Desc1 <- renderText(get_desc(values$frame,values$runner[[1]]))
    output$Desc2 <- renderText(get_desc(values$frame,values$runner[[2]]))
    output$table <- renderTable(frame_burn(values$frame))
    output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      # draw the histogram with the specified number of bins
      hist(values$frame$score, breaks = 15, col = 'darkgray', border = 'white',
           xlab = '',
           main = 'Histogram of scores')
    })
  })
  
  observeEvent(input$select_2, {
    if(values$runtime==(nrow(values$merger)-1))
    {
      values$runtime <- 0
    }
    values$runtime <- values$runtime + 1
    values$frame <- update(values$frame, values$runner[[2]], values$runner[[1]])
    values$runner <- extract_recommended_names(values$frame, values$aq_list, values$runtime, values$merger)
    values$aq_list <- values$runner[[3]]
    output$Name1 <- renderText(values$runner[[1]])
    output$Name2 <- renderText(values$runner[[2]])
    output$Desc1 <- renderText(get_desc(values$frame,values$runner[[1]]))
    output$Desc2 <- renderText(get_desc(values$frame,values$runner[[2]]))
    output$table <- renderTable(frame_burn(values$frame))
    output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      # draw the histogram with the specified number of bins
      hist(values$frame$score, breaks = 15, col = 'darkgray', border = 'white',
           xlab = '',
           main = 'Histogram of scores')
    })
  })
  
  observeEvent(input$select_draw, {
    values$runtime <- values$runtime + 1
    values$frame <- update_tie(values$frame, values$runner[[2]], values$runner[[1]])
    values$runner <- extract_recommended_names(values$frame, values$aq_list, values$runtime, values$merger)
    values$aq_list <- values$runner[[3]]
    output$Name1 <- renderText(values$runner[[1]])
    output$Name2 <- renderText(values$runner[[2]])
    output$Desc1 <- renderText(get_desc(values$frame,values$runner[[1]]))
    output$Desc2 <- renderText(get_desc(values$frame,values$runner[[2]]))
    output$table <- renderTable(frame_burn(values$frame))
    output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      # draw the histogram with the specified number of bins
      hist(values$frame$score, breaks = 15, col = 'darkgray', border = 'white',
           xlab = '',
           main = 'Histogram of scores')
    })
  })
  
  
  


}

# Run the application 
shinyApp(ui = ui, server = server)
