#----------------------
# Name          : Harisriguhan Sivakumar
# Student ID    : 31386709
# Script Name   : server.R
# Date Created  : 28th October 2021
# Date Modified : 31th October 2021
#----------------------

# References

# [1] "shiny leaflet ploygon click event",Stack Overflow. https://stackoverflow.com/questions/42798377/shiny-leaflet-ploygon-click-event .
# [2] "How to make bars in ggplot2 barplot interactive in shiny?", Stack Overflow. https://stackoverflow.com/questions/31961622/how-to-make-bars-in-ggplot2-barplot-interactive-in-shiny 


# First we need to install the packages. In case you haven't installed any of the 
# packages please uncomment the below code and run

# install.packages("shiny")
# install.packages("tidyverse")
# install.packages("leaflet")
# install.packages("plotly")
# install.packages("maps")
# install.packages("rpart")
# install.packages("ggplot2")

# First we will import all the packages/libraries that are required.

require(shiny)
require(plotly)
require(tidyverse)
require(maps)
require(leaflet)
require(rpart)
require(ggplot2)

# now we will read all the data frames that we need for our Data Visualization project

glbmkt <- read_csv("GlobalMktShare.csv") # for global market share
UserDemo <- read_csv("UsersClean.csv",na = character()) # for User demographic
AnimeStat <- read_csv("AnimeStatsRefined.csv") # For details about anime
views <- read_csv("AnimeViews.csv") # Record of the views over the years
Userjoin <- read_csv("UsersJoin.csv") # The joining year of the users

# as we have now read all the data we need we need to do some more wrangling

# we will begin by creating a subset of the glbmkt dataset where it will only have the top 10 countries

glbmktT10 <- head(glbmkt[order(glbmkt$percent,decreasing = T),],10)
glbmktT10$Location_Name <- factor(glbmktT10$Location_Name, levels = glbmktT10$Location_Name)

# Now for the leaflet we need a map for it, with the help of the maps library we will get a world map with only the 
# polygons of the countries we need.

world = map("world", fill = TRUE, plot = FALSE,regions = unlist(glbmkt$Location_Name))

# we will get only the first part of the names in order for us to match it with our data properly
spliteNames <- strsplit(world$names, ":")
firstPartNames <- lapply(spliteNames, function(x) x[1]) 

# match them with our dataset to get the appropriate market share and its percentage
mktper <- glbmkt$percent[match(firstPartNames, glbmkt$Location_Name)]
mktshare <- glbmkt$mktshare[match(firstPartNames, glbmkt$Location_Name)]
# assign the Dark 2 color palette to the mktshare based on which the data will be colored. 
cpal <- colorFactor("Dark2", mktshare)

# we will create a function that will return a data frame that is suitable for our sunburst chart
# we will take our UserDemo data set, the hierarchy and country name as parameters

get_sb_df <- function(df,hierarchy,country = "global"){
  # make a copy of the data frame 
  df_copy <- df
  if (country != "global"){
    # If a country is selected take only users of that country
    df_copy <- df_copy[df_copy$Location_Name == country,]
  }
  
  # save the number of records 
  total <- nrow(df_copy)
  
  
  if (hierarchy == 0){
    # If Gender -> Age hierarchy is selected first group the data by gender then save the results
    # summarise a new column called percent that stores the percent
    df_copy_1 <- df_copy %>% group_by(gender) %>% summarise(percent = n()*100/total)
    # create a new country called parent that will have the country name
    df_copy_1["parent"] <- country
    # Now group them based on gender as well as age and then save the results
    # summarise a new column called percent that stores the percent
    df_copy_2 <- df_copy %>% group_by(gender,age) %>% summarise(percent = n()*100/total)
    # Now create vectors that represent each column/attribute that needs to be there for a sunburst chart
    # by the combining the columns in the data sets we just created
    # ids represent the uniques names for them
    ids <- c(country,df_copy_1$gender,paste(df_copy_2$gender,"-",df_copy_2$age))
    # parents represent their parents
    parent <- c("",df_copy_1$parent,df_copy_2$gender)
    # labels represent the name to be displayed
    labels <- c(country,df_copy_1$gender,df_copy_2$age)
    # values store the percent value
    values <- c(100,df_copy_1$percent,df_copy_2$percent)
    # create a dataframe with these four values and return it
    df_sb <- data.frame(ids,parent,labels,values)
    return(df_sb)
  }
  else if (hierarchy == 1){
    # If Age -> Gender hierarchy is selected first group the data by age then save the results
    # summarise a new column called percent that stores the percent
    df_copy_1 <- df_copy %>% group_by(age) %>% summarise(percent = n()*100/total)
    # create a new country called parent that will have the country name
    df_copy_1["parent"] <- country
    # Now group them based on age as well as gender and then save the results
    # summarise a new column called percent that stores the percent
    df_copy_2 <- df_copy %>% group_by(age,gender) %>% summarise(percent = n()*100/total)
    # Now create vectors that represent each column/attribute that needs to be there for a sunburst chart
    # by the combining the columns in the data sets we just created
    # ids represent the uniques names for them
    ids <- c(country,df_copy_1$age,paste(df_copy_2$age,"-",df_copy_2$gender))
    # parents represent their parents
    parent <- c("",df_copy_1$parent,df_copy_2$age)
    # labels represent the name to be displayed
    labels <- c(country,df_copy_1$age,df_copy_2$gender)
    # values store the percent value
    values <- c(100,df_copy_1$percent,df_copy_2$percent)
    # create a dataframe with these four values and return it
    df_sb <- data.frame(ids,parent,labels,values)
    return(df_sb)
  }
}

# Now we will make a function that returns a dataframe for representing a factor with their market share or avg score
# we will take the AnimeStat data frame the x axix and y axis as attributes. 
get_ms_df <- function(df,x,y){
  # make a copy of the dataset and get only the columns we want
  df_copy <-df
  df_copy <- df_copy[c("anime_id",x,"score","mktshare")]
  # remove the duplicate columns
  df_copy <- unique(df_copy)
  if (y == "avgscore"){
    # if the Average Score Option was chosen
    # group by the factor and calculate the avg score 
    df_ms <- df_copy %>% group_by_at(x) %>% summarise(avgscore = mean(score))
    # round the score to 2 decimal places
    df_ms$avgscore <- round(df_ms$avgscore,2)
    # order them by descending order of average score
    df_ms <- df_ms[order(df_ms$avgscore,decreasing = T),]
    # Impute NA values with its string representation
    df_ms[is.na(df_ms)] <- "NA"
    # use factor function so that it will remain in descending order when plotted
    df_ms[[x]] <- factor(df_ms[[x]], levels = df_ms[[x]])
    # return the dataframe
    return(df_ms)
  }
  else if (y == "mktshare"){
    # if the Market Share Option was chosen
    # group by the factor and calculate the total market share
    df_ms <- df_copy %>% group_by_at(x) %>% summarise(mktshare = sum(mktshare))
    # round the market share to 2 decimal places
    df_ms$mktshare <- round(df_ms$mktshare,2)
    # order them by descending order of market share
    df_ms <- df_ms[order(df_ms$mktshare,decreasing = T),]
    # Impute NA values with its string representation
    df_ms[is.na(df_ms)] <- "NA"
    # use factor function so that it will remain in descending order when plotted
    df_ms[[x]] <- factor(df_ms[[x]], levels = df_ms[[x]])
    # return the dataframe
    return(df_ms)
  }
}

# A small function to format the x/y axis names in order for it to be presentable
capitalize <- function(x){
  
  # Make the first letter capital
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  # remove the _ in the column names
  x <- gsub("_"," ",x)
  # return the value
  return(x)
}


# create an rpart model on the factors with the score as the target variable
factor_model <- rpart(score ~ ., data = AnimeStat[c("type","source","episodes","Duration_in_Minutes","rating","score","studio","genre")])

# we extract a tibble that contains the factor importance for the factors
factor_importance <- tibble(
  variable = names(factor_model$variable.importance),
  importance = factor_model$variable.importance)

# order them in descending order
factor_importance <- factor_importance %>% 
  mutate(variable = fct_reorder(variable, importance))


# As all the preprocessing is done we will create the server part of Shiny. 

shinyServer(function(input,output){
  
  # We will first initialize the reactive values and variables that will play a big role in making our webpage 
  # interactive
  # A reactive Variable for the country selected in the map or bar chart
  country_selected <- reactiveVal("global")
  # A reactive Variable for the radial button input for the hierarchy of the sunburst chart
  hierachy <- reactive(input$hier)
  # A reactive list of values for highlighting the bar selected in the bar chart [2]
  select_bar <- reactiveValues(toHighlight = rep(FALSE, length(glbmktT10$Location_Name)), 
                           selectedBar = NULL)
  
  # A reactive Variable for the drop down list with the factors
  factor_r <- reactive(input$factor)
  # A reactive Variable for the radial button input for statistic to view
  stat_r <- reactive(input$stat)
  # reactive dataframes for the views and users distribution that changes according to the year range
  views_R <- reactive({views[views$my_start_date>=input$VR[1] & views$my_start_date<=input$VR[2],]})
  Userjoin_R <- reactive({Userjoin[Userjoin$join_date>=input$UR[1] & Userjoin$join_date<=input$UR[2],]})
  
  # Plotly output for the cumulative number of views over the years
  # appropriate styling has been done too
  output$AUDV <- renderPlotly({
    
    plot_ly(views_R(), x = ~my_start_date, y = ~cumsum(views), type = 'scatter', mode = 'lines+markers',
            marker = list(color='#FC766AFF',size=12),line = list(color='#5B84B1FF',width = 6)) %>%
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Total Number of Views")
      )
    
    
  })
  
  # Plotly output for the cumulative number of users over the years
  # appropriate styling has been done too
  output$AUD <- renderPlotly({
    
    plot_ly(Userjoin_R(), x = ~join_date, y = ~cumsum(users), type = 'scatter', mode = 'lines+markers',
            marker = list(color='#5CC8D7FF',size=12),line = list(color='#B1624EFF',width = 6)) %>%
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Total Number of Users")
      )
    
    
  })
  
  # Plotly output for the factor vs score/market share
  # appropriate styling has been done too
  output$SBG <- renderPlotly({
    
    plot_ly(get_ms_df(AnimeStat,factor_r(),stat_r()), 
            x = get_ms_df(AnimeStat,factor_r(),stat_r())[[factor_r()]], 
            y = get_ms_df(AnimeStat,factor_r(),stat_r())[[stat_r()]],
            type = 'bar') %>% 
      layout(
              xaxis = list(title = capitalize(factor_r())),
              yaxis = list(title = ifelse(stat_r() == "avgscore","Average Score (out of 10)","Market Share (in percent)"))
            )
      
    
  })
  
  # Plotly output for the factor vs score/market share (only top 10)
  # appropriate styling has been done too
  output$SBGT10 <- renderPlotly({
    
    plot_ly(head(get_ms_df(AnimeStat,factor_r(),stat_r()),10), 
            x = head(get_ms_df(AnimeStat,factor_r(),stat_r())[[factor_r()]],10), 
            y = head(get_ms_df(AnimeStat,factor_r(),stat_r())[[stat_r()]],10),
            type = 'bar') %>% 
      layout(
        xaxis = list(title = capitalize(factor_r())),
        yaxis = list(title = ifelse(stat_r() == "avgscore","Average Score (out of 10)","Market Share (in percent)"))
      )
    
    
  })
  
  # Plotly output for the user distribution sunburst chart
  # appropriate styling has been done too
  output$SB <- renderPlotly({
    
    plot_ly(
      
      data = get_sb_df(UserDemo,hierachy(),country_selected()),
      
      ids = ~ids,
      
      labels = ~labels,
      
      parents = ~parent,
      
      values = ~values,
      
      type = 'sunburst',
      
      branchvalues = "total"
      
    )
  }
  )
  
  
  # GGplot output for the user distribution bar chart
  # appropriate styling has been done too
  output$BC <- renderPlot({
    
    ggplot(data = glbmktT10,aes(x=Location_Name,y=percent,label = percent ,fill = ifelse(select_bar$toHighlight,yes = "yes", no = "no"))) + 
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("yes" = "blue", "no" = "black" ), guide = "none" ) +
      geom_text(position=position_dodge(0.5), vjust=-0.25,size = 5) +
      labs(x="Location Name",y="Percent of Market share")
                              
  }
  )
  
  # Plotly output for the factor importance bar chart
  # appropriate styling has been done too
  output$FI <- renderPlotly({
    
    plot_ly(factor_importance, 
            x = ~importance, 
            y = ~variable, 
            type = 'bar') %>%
      layout(
        xaxis = list(title = "Importance (Sum of goodness per primary split)"),
        yaxis = list(title = "Factor")
      )
    
    
  }
  )
  
  # renderLeaflet is used to send our leaflet to the UI.
  
  output$MAP <- renderLeaflet({
    leaflet(world) %>% # create a blank canvas
      addTiles() %>% # add tile
      setView(20,20,2) %>%
      addPolygons( # draw polygons on top of the base map (tile)
        weight = 2,
        smoothFactor = 0.2, 
        fillOpacity = 0.5,
        layerId = ~names,
        color = ~cpal(mktshare),
        popup  = paste0("Country Name: ",firstPartNames,"</br>Market Share: ",mktper, "%</br>Market Category: ",mktshare),
        label = firstPartNames,
        highlightOptions = highlightOptions(color = "black", weight = 2,
                                            bringToFront = TRUE)
      ) %>%
      addLegend(position = "bottomleft",pal=cpal,values = mktshare,title = "Market Share")
    
  })
  
  # a render text to show the factor versus stat title based on what the user has selected.
  output$FVST <- renderText({ 
    paste(capitalize(input$factor),"VS",ifelse(input$stat == "avgscore","Average Score","Market Share"))
  })
  
  # a render text to show the factor versus stat title based on what the user has selected.
  output$UVT <- renderText({ 
    paste(ifelse(input$attr == 0,"Total Views","Total Users"),"from",
          ifelse(input$attr == 0,input$VR[1],input$UR[1]), "to",
          ifelse(input$attr == 0,input$VR[2],input$UR[2]))
  })
  
  # an observe event is used to update the country_selected reactive variable with will make changes to all 
  # the charts associated with it when we click on a country in the leaflet map [1].
  
  observeEvent(input$MAP_shape_click, { 
    # when a country is selected it extracts the id of the polygon
    country_selected(input$MAP_shape_click$id)
    # we now only take the first part name
    country_selected(unlist(strsplit(country_selected(), ":"))[1])
    # if the first part name is USA or UK we change it to United States and United Kingdom respectively.
    if (country_selected() == "USA"){ country_selected("United States")}
    else if (country_selected() == "UK"){ country_selected("United Kingdom")}
  })
  
  # an observe event is used to update the country_selected reactive variable which will make changes to all 
  # the charts associated with it when we click the bar chart [2].
  
  observeEvent(eventExpr = input$BC_click, {
    
    # According to the bar selected store its column name
    select_bar$selectedBar <- glbmktT10$Location_Name[round(input$BC_click$x)]
    # update the toHighlight vector so that it will know what bar to highlight
    select_bar$toHighlight <- glbmktT10$Location_Name %in% select_bar$selectedBar
    # change the country_selected to the column name of the bar selected
    country_selected(input$BC_click$domain$discrete_limits$x[[round(input$BC_click$x)]])
    # if the column name is USA or UK we change it to United States and United Kingdom respectively.
    if (country_selected() == "USA"){ country_selected ("United States")}
    else if (country_selected() == "UK"){ country_selected("United Kingdom")}
  })
  
  
  # an observe event is used to reset the sunburst chart if we change the view from bar to map.  
  # this also serves as a reset function.
  observeEvent(input$chrt, { # update the location selectInput on map clicks
    country_selected("global")
    select_bar$toHighlight <-c(F,F,F,F,F,F,F,F,F,F)
  })
  
  
})