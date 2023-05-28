library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(plotly)
library(usmap)

# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Welcome to the shinyapp of Budweiser EDA"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      br(),
      br(),
      
      # Input: Select the random distribution type ----
      radioButtons("plot", "Distribution type:",
                   c("Histogram" = "hist",
                     "Boxplot" = "box"
                     )),
      
      # br() element to introduce extra vertical spacing ----
      br(),
      br(),
      br(),
      
      # Input: Slider for the number of observations to generate ----
      selectInput("text", "Filter by State", 
                  choices = c("No Filter","AL","AK", "AZ","AR", "CA","CO","CT","DE","FL",
                              "GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD", "MA",
                              "MI","MN","MS" ,"MO","MT","NE" ,"NV","NH","NJ","NM","NY","NC" ,"ND",
                              "OH" ,"OK","OR","PA" ,"RI","SC" ,"SD","TN","TX","UT","VT" ,"VA", "WA" ,"WV",
                              "WI" ,"WY")),
      
      br(),
      br(),
      
      selectInput("line", "Add a regression line",
                  choices = c("Yes","No")),
      
      br(),
      br(),
      
      ),
    
    
 
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Table", tableOutput("table")),
                  tabPanel("ABV", plotOutput(outputId = "distPlot")),
                  tabPanel("IBU", plotOutput("boxPlot"))),
      
      tabsetPanel(type = "tabs",
                  tabPanel("ScatterPlot", plotOutput("scatterPlot")),
                  tabPanel("Map", plotOutput("my_map"))),

      
  )
))





# Define server logic for random distribution app ----
server <- function(input, output) {
  
  output$table = renderTable({
    
    if (input$text != "No Filter"){
      d2=all %>% filter( State == input$text)
      head(d2,n=10)
    }
    else if(input$text == "No Filter"){
      head(all, n=10)
    }
    
  })
  
  output$distPlot <- renderPlot({
    
    if (input$plot == "hist")
    {
      if (input$text == "No Filter"){
        x = all$ABV 
        hist(x,  breaks = 40, col = "#007bc2",  border = "white",
             xlab = "ABV",
             main = "Histogram of ABV")
      }
        else if (input$text != "No Filter"){
          x = all %>% filter( State == input$text) %>% select(ABV)
          x= unlist(x)
          hist(x,  breaks = 40, col = "#007bc2",  border = "white",
               xlab = "ABV",
               main = input$text)
        }
    }
    if (input$plot == "box")
    {
      if (input$text == "No Filter"){
        x = all$ABV 
        boxplot(x, col = "#007bc2",
                xlab = "ABV",
                main = "Boxplot of ABV")
      }
      else if (input$text != "No Filter"){
        x = all %>% filter( State == input$text) %>% select(ABV)
        boxplot(x, col = "#007bc2",
                xlab = "ABV",
                main = input$text)
      }
    }  
  })
  
  output$boxPlot = renderPlot({
    
    if (input$plot == "hist")
    {
      if (input$text == "No Filter"){
        x = all$IBU
        hist(x,  breaks = 40, col = "#007bc2",  border = "white",
             xlab = "IBU",
             main = "Histogram of IBU")
      }
      else if (input$text != "No Filter"){
        x = all %>% filter( State == input$text) %>% select(IBU)
        x= unlist(x)
        hist(x,  breaks = 40, col = "#007bc2",  border = "white",
             xlab = "IBU",
             main = input$text)
      }
    }
    if (input$plot == "box")
    {
      if (input$text == "No Filter"){
        x = all$IBU 
        boxplot(x, col = "#007bc2",
                xlab = "IBU",
                main = "Boxplot of IBU")
      }
      else if (input$text != "No Filter"){
        x = all %>% filter( State == input$text) %>% select(IBU)
        boxplot(x, col = "#007bc2",
                xlab = "IBU",
                main = input$text)
      }
    } 
  })

  output$scatterPlot = renderPlot({
    if (input$text == "No Filter"){
      
      if (input$line == "Yes"){
        ggplot(all,aes(x=ABV,y=IBU))+
          geom_point()+geom_smooth(method = "lm")+
          ggthemes::theme_clean()+
          ggtitle("ABV VS IBU")
      }
      else if (input$line == "No")
        ggplot(all,aes(x=ABV,y=IBU))+
        geom_point()+
        ggthemes::theme_clean()+
        ggtitle("ABV VS IBU")
    }
    else if (input$text != "No Filter"){

       d=all %>% filter( State == input$text)
       
       if (input$line == "Yes"){
         ggplot(d,aes(x=ABV,y=IBU))+
           geom_point()+geom_smooth(method = "lm")+
           ggthemes::theme_clean()+
           ggtitle("ABV VS IBU")
       }
       
       else if (input$line == "No")
         ggplot(d,aes(x=ABV,y=IBU))+
         geom_point()+
         ggthemes::theme_clean()+
         ggtitle("ABV VS IBU")
      
    }
  })
  
  output$my_map = renderPlot({
    
    if (input$text == "No Filter"){
      
      plot_usmap(data = brewerybystate, regions = "state", values = "n", color = "#3E4A89FF", labels = TRUE, label_color = "#35B779FF") +  
        ggrepel::geom_label_repel(data = brewerybystate, aes(x = lon, y = lat, label = n), size = 3, alpha = 0.8, label.r = unit(0.5, "lines"), label.size = 0.5, segment.color = "black", segment.size = 1)+
        ggtitle("Number of Breweries in Each State")
    }
    
    else if (input$text != "No Filter"){
      
      d3= brewerybystate%>%filter(state==input$text)
      
      plot_usmap(data = d3, regions = "state", values = "n", color = "#3E4A89FF", labels = TRUE, label_color = "#35B779FF") +  
        ggrepel::geom_label_repel(data = d3, aes(x = lon, y = lat, label = n), size = 3, alpha = 0.8, label.r = unit(0.5, "lines"), label.size = 0.5, segment.color = "black", segment.size = 1)+
        ggtitle("Number of Breweries in Each State")
      
    }
    
    })
}  

shinyApp(ui, server)

