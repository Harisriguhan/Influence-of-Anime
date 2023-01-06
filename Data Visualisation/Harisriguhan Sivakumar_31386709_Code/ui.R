#----------------------
# Name          : Harisriguhan Sivakumar
# Student ID    : 31386709
# Script Name   : ui.R
# Date Created  : 28th October 2021
# Date Modified : 31th October 2021
#----------------------

# First we need to install the packages. In case you haven't installed any of the 
# packages please uncomment the below code and run

# install.packages("shiny")
# install.packages("tidyverse")
# install.packages("leaflet")
# install.packages("plotly")
# install.packages("shinythemes")

# First we will import all the packages/libraries that are required.

require(shiny)
require(tidyverse)
require(leaflet)
require(plotly)
require(shinythemes)

# read the datasets we may require for the slider values

df_4 <- read_csv("AnimeViews.csv")
df_5 <- read_csv("UsersJoin.csv")

#Declare a shiny UI so that we can make a layout

shinyUI(
  
  # Fluid Page allows us to make our layout without any complications
  
  fluidPage(
    
    theme = shinytheme("cerulean"),
    
    # It assigns a title which is displayed at the title bar
    
    title = "The Influence of Anime",
    
    # Basically its the header of the Shiny Webpage
    
    titlePanel(h1("The Influence of Anime",align = "center")),
    
    # fluidRow defines a row in a shiny application which can be further divided into columns
    # Please Maximize the window to resemble the Layout.
    
    fluidRow(
      column(width = 8,
        
        # Add some content for the user to understand the plot
        
        HTML(paste("<h2>Global Market Share of Anime</h2><br/>"),
             "<b>According to our Research, around 85% of the market share has been from only 37 countries ",
             "that have been displayed below. So we have decided to show the market share of those 37 ",
             "countries only as they have a significant market share</b>"),     
        
        # Add a radio button in order to switch between Map and Bar chart
        
        radioButtons("chrt", "Chart Type:",
                      c("Choropleth Map" = 0,
                        "Bar Chart (Top 10 countries only)" = 1
                        ),
                        selected = 0,
                     inline = T),
        
        # Add some content for the user to understand the plot
        
        HTML(paste("<b>Click on a country/bar below to get the User distribution for that specific country.</b>",
                "<br/><h4>NOTE:</h4> <b>If you want to reset the User Distribution to Global, Switch from Choropleth Map to Bar chart and Vice Versa</b>"))
             
      ),
      column(width = 4,
             
             # Add a heading for the Sunburst chart
             
             HTML(paste("<h2>User Distribution</h2>")),
             
             # Add a radio button in order to switch between the hierarchies.
             
             radioButtons("hier", "Sunburst Hierarchy:",
                          c("Gender -> Age" = 0,
                            "Age -> Gender" = 1
                          ),
                          selected = 0,
                          inline = T),
             
             # Add some content for the user to understand the plot
             
             HTML(paste("<b>Click on the Inner donut of the sunburst chart to expand it</b>"))
             
      )
    ),
    fluidRow(
      column(width = 8,
           
          # Add a conditional panel that will show the map or bar chart depending on the radial button input
            
          conditionalPanel(
            condition = "input.chrt == 0",
            leafletOutput("MAP",width = "100%",height = 610)
          ),
          conditionalPanel(
            condition = "input.chrt == 1",
            plotOutput("BC",width = "100%",height = 610,click = "BC_click")
          )
          
          
            ),
      column(width = 4,
             
             # Display the sunburst chart
             
             plotlyOutput("SB",width = "100%",height = 610)
             
      )
    ),
    fluidRow(
      column(width = 5,
             
             HTML(paste("<br/><br/>")),
             
             # Add a heading for the bar chart
             
             HTML(paste("<h2>Factors determining the market share and quality of an Anime</h2>")),
             
             # Display the bar chart
             
             plotlyOutput("FI",width = "100%",height = 610),
             
             # Add some content for the user to understand the plot
             
             HTML(paste("<h4>Note: For those who have no idea what Factor Importance is!</h4>",
                        "<b>According to the rpart documentation, It is the sum of the goodness of",
                        "split measures for each split when it was the primary variable/root node</b>"))
             
      ),
      column(width = 1,
             
             HTML(paste("<br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/>")),
             
             # Have a drop down list that will list out the factors
             
             selectInput("factor", "Choose a factor (X-axis):",
                         list("Genre" = "genre",
                              "Source" = "source",
                              "Type" = "type",
                              "Episodes" = "episodes",
                              "Episode Duration" = "Duration_in_Minutes",
                              "Rating" = "rating",
                              "Studio" = "studio"
                              ),
                         selected = "genre"
                         ),
             
             # Add a radio button in order to switch between Market Share and Average Score
             
             radioButtons("stat", "Choose Statistic (Y-axis)",
                          c("Average Score" = "avgscore",
                            "Market Share" = "mktshare"
                          ),
                          selected = "avgscore",
                          inline = F),
             
             # Have a check box for displaying the top 10 results only
             
             checkboxInput("top10", "Show Top 10 only ", value = FALSE, width = NULL)
             
      ),
      column(width = 6,
             
             HTML(paste("<br/><br/><br/><br/><br/><br/>")),
             
             # Add a heading for the bar chart
             
             HTML(paste("<h2>",textOutput("FVST"),"</h2>")),
             
             # Add a conditional panel that will show the Top 10 only or not depending on the check box input
             
             conditionalPanel(
               condition = "input.top10 == 0",
               plotlyOutput("SBG",width = "100%",height = 610)
             ),
             conditionalPanel(
               condition = "input.top10 == 1",
               plotlyOutput("SBGT10",width = "100%",height = 610)
             )
             
      )
    ),
    fluidRow(
      column(width = 3,
             
             HTML(paste("<br/><br/><br/><br/><br/><br/>")),
             
             # Add a radio button in order to switch between Views and Users
             
             radioButtons("attr", "Choose Attribute",
                          c("Total Views" = 0,
                            "Total Users" = 1
                          ),
                          selected = 0,
                          inline = T),
             
             # Add a conditional panel that will show the appropriate slider depending on the radial button input
             
             conditionalPanel(
               condition = "input.attr == 0",
             sliderInput("VR", "Year Range", min = min(df_4$my_start_date), 
                         max = max(df_4$my_start_date), value=range(df_4$my_start_date),
                         sep = "")
                        
               ),
             conditionalPanel(
               condition = "input.attr == 1",
             sliderInput("UR", "Year Range", min = min(df_5$join_date), 
                         max = max(df_5$join_date), value=range(df_5$join_date),
                         sep = "")),
             
             HTML(paste("<h4>Note:</h4>",
                        "<b>If both ends of the range are the same (2004-2004) we will get a dot that represents",
                        "the Users/Views of that year.</b>"))
      ),
      column(width = 6,
             
             HTML(paste("<br/><br/>")),
             
             # Add a heading for the line chart
             
             HTML(paste("<h2>",textOutput("UVT"),"</h2>")),
             
             # Add a conditional panel that will show the appropriate line chart depending on the radial button input
             
             conditionalPanel(
               condition = "input.attr == 0",
               plotlyOutput("AUDV",width = "100%",height = 610)
             ),
             conditionalPanel(
               condition = "input.attr == 1",
               plotlyOutput("AUD",width = "100%",height = 610)
             )
             
      ),
      column(width = 3,
             
             HTML(paste("<br/><br/>")),
             
             HTML(paste0("<h3> Data Source:</h3><br/>",
                        "<b>Racinsky, Matej ,'MyAnimeList Dataset.' Kaggle, 2018, ",
                        "doi: 10.34740/KAGGLE/DSV/45582. URL: https://www.kaggle.com/azathoth42/myanimelist .</b>"))
             )
    )
  )
)
    
