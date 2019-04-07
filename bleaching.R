# Name: Vikram Hanumanthrao Patil
# ID: 29389690
# Assignment-2: FIT5147
####################################

# setting the working directory where the file is present
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(ggplot2)
library(shiny)
library(leaflet)
library(shinythemes)

##############################################################################################################
# 1 - Reading the data.
df <- read.csv("assignment-02-data-formated.csv") # reading the csv file

colnames(df)[6] <- "bleaching" # changing the column name from value --> bleaching
df$bleaching<-sapply(df$bleaching, function(x) as.numeric(gsub("%", "", x))) #removing %symbol from bleaching

#ordering the data by latitude
df$order_facet <- factor(df$location, levels = unique(df$location[order(-df$latitude)]))

#############################################################################################################



# 2 - Shiny app

#UI
ui <- fluidPage(theme=shinytheme("superhero"),
                
                headerPanel("Bleaching Explorer across different sites"),
                # First row to accept the input selection
                fluidRow(
                  column(1,offset=1,
                         selectInput("coral_type","select coral type:",choices = (levels(df$coralType))),
                         selectInput("smooth_type","select smooth type:",choices = (c("auto","lm", "glm", "gam", "loess")),selected ="auto")
                  ),
                  column(width=5,offset=1,
                         h4("Below is the coral bleaching data for 8 sites in the Great Barrier Reef(Australia). It gives the % of bleaching for different kinds of coral: hard corals, sea pens, blue corals, soft corals and sea fans over the last 8 years")
                  ),
                  
                  fluidRow(),
                  
                  # Second row to display the output
                  fluidRow(
                    column(width=1,offset=1,h4("Bleaching across different sites"),
                           plotOutput("coral_plot", height = "500px",width = "700px")
                    ),
                    
                    column(width=5,offset=5,h4("Below are the sites location"),
                           leafletOutput("mymap")
                    )
                  )
                  
                  
                )
)


# Server
server <- function(input, output, session) {
  
  # Based on the input selection of coral type, selecting respective dataframe and making it reactive
  dataset <- reactive({
    df[df$coralType==input$coral_type,]
  })
  
  # Plotting the bleaching % in different sites
  output$coral_plot = renderPlot({
    graph<- ggplot(data=dataset(),height = 200, width = 200,aes(x=year,y=bleaching,colour=order_facet)) + geom_point(size=4)
    graph + facet_grid(.~order_facet) + 
      geom_smooth(method=input$smooth_type) + 
      labs(colour="Sites") + 
      theme(text = element_text(size=20),axis.text.x = element_text(angle=90, hjust=1))
  })
  
  # creating leaflet map
  output$mymap <- renderLeaflet({
    
    leaflet(data =dataset()) %>% addTiles() %>%
      addMarkers(~longitude, ~latitude, label = ~as.character(location),labelOptions = labelOptions(noHide = T,direction = 'bottom'))
  })
  
}

shinyApp(ui, server)

