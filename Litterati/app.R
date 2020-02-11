library(dplyr)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(leaflet)

#read in the csv file
df<-read.table(file = "Litterati.csv", header = TRUE, sep = ",")
#filter out rows outside latitude range to remove junk location readings
df<-df[(df$lat<42.5 & df$lat>41.5 ),]

#Change the default usernames to a more readable format
df$username <- gsub('litterati', 'User', df$username)

#calculate the number of pieces picked in each row and store it(as number of tags will always be 1 more than the number of commas)
df$no<-lengths(regmatches(df$tags, gregexpr(",", df$tags))) + 1

#convert timestamp to the format with lubridate
df$litterTimestamp<- ymd_hms(df$litterTimestamp, tz="UTC")

#convert UTC time to Chicago time
df$litterTimestamp<- with_tz(df$litterTimestamp,tzone = "america/chicago")

#Calculate the total number of pieces picked up by summing the
total<-sum(df$no)

#store the hour values of each observation
df$hr<-hour(df$litterTimestamp)

#replace empty tags with "Untagged"
df$tags <- gsub("^$", "untagged", df$tags)

#extract month, date and weekday individually from timestamp and store them
df$month <- months(df$litterTimestamp,abbreviate = FALSE)
df$dt<-as.Date(df$litterTimestamp)
df$day <- factor(weekdays(df$litterTimestamp, abbreviate = TRUE), levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

#extract the username and aggregate the number of objects picked up 
aggdata <-setNames(aggregate(df$no, by=list(df$username), FUN=sum, na.rm=TRUE),c("User","Number"))
aggdata<-aggdata[order(aggdata$Number, decreasing = TRUE),]

#extract just the names and add 'all' at the beginning for menu options
tnames<-aggdata$User
tnames<-append(tnames,"All",after = 0)

#extract tags and their frequency
TagFreq<-setNames(as.data.frame(table(unlist(strsplit(df$tags, ",", fixed=TRUE)))),c("Tag","Number"))
TagFreq<-TagFreq[order(TagFreq$Number, decreasing = TRUE),]
TagFreqh<-head(TagFreq,10,2)

#extract just tags and add 'all' at the beggining for menu options
allTags<-as.character.factor(TagFreq$Tag)
allTags<-append(allTags,"All",after = 0)


#store all months of a year in a dataframe for menu options
mnts<-c("January","February","March","April","May","June","July","August","September","October","November","December")
mnts<-append(mnts,"All",after = 0)

#store all hours of a day in a dataframe for menu options
hrs<-c(23:0)
hrs<-append(hrs,"All",after = 0)


# UI
ui <- dashboardPage(
  
  
  dashboardHeader(title = "Litterati Challenge Analysis"),
  
  
  
  
 dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   
                   sidebarMenu(
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                   
                     
                     menuItem("Home", tabName = "home", icon = icon("home", lib = "glyphicon"), selected=TRUE),
                     menuItem("About", tabName = "about", icon = icon("info-sign", lib = "glyphicon")),  
                       
                   selectInput("uname", "Select the Username to visualize", tnames, selected = "All"),
                  selectInput("tod", "Select the Hour of Day to visualize", hrs, selected = "All"),
                  selectInput("month", "Select the Month to visualize", mnts, selected = "All"),
                  selectInput("tag", "Select the Tag to visualize", allTags, selected = "All")
                  
                   
                   
                   #   radioButtons("type", "View type:",
                   #                c("Plot" = "pt",
                   #                 "Table" = "tb"
                   #                )),  
                   
                   )        
                   
                   
  ),
  
dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "home",
    
    

  
  fluidRow(
    verbatimTextOutput("total") ),
  
  
  
  #row 1 
  
  fluidRow(
    
    column(2,
           fluidRow(
             box(title = "Map", solidHeader = TRUE, status = "primary", width = 12,
                 leafletOutput("leaf", height = 900)
             ),
           )
    ),
    
    column(7,
           
           fluidRow(
             box(title = "Number of Objects Picked by Date", solidHeader = TRUE, status = "primary", width = 12,
                 
                 plotOutput("bar", height = 900)
             )
           ),
           
    ),
    column(3, 
           fluidRow(
             box( title = "Objects Picked by Day", solidHeader = TRUE, status = "primary", width = 12,
                  plotOutput("bar2", height = 900),)
           )
           
           
    )
    
    
  ),
 
  
  #Row 2
  fluidRow(
    
    
    column(9,
           fluidRow(
             
             column(6,   
                    fluidRow(
                      box( title = "Top tags", solidHeader = TRUE, status = "primary", width = 12,
                           plotOutput("bar4", height = 900)
                      )
                    )
             ),  
          
           
           column(6,
                  
                  fluidRow(
                    box( title = "Pieces picked by time", solidHeader = TRUE, status = "primary", width = 12,
                         plotOutput("bar3", height = 900)
                    )
                  )   
                  
           ),  
           
           column(4,
                  
                  fluidRow(
                    box( title = "Top Tags", solidHeader = TRUE, status = "primary", width = 12,
                         dataTableOutput("tbl", height = 900)
                    )
                  )
                  
                  
           ),
           column(4,
                  
                  fluidRow(
                    box( title = "Pieces picked by day", solidHeader = TRUE, status = "primary", width = 12,
                         dataTableOutput("tbl2", height = 900)
                    )
                  )
                  
                  
           ),
           column(4,
                  
                  fluidRow(
                    box( title = "Pieces picked by time", solidHeader = TRUE, status = "primary", width = 12,
                         dataTableOutput("tbl3", height = 900)
                    )
                  )
                  
                  
           )
                  
                  
                  
          
           
           
           
    ),
    ),
    
    column(3,
           
           fluidRow(
             box( title = "Top pickers", solidHeader = TRUE, status = "primary", width = 12,
                  dataTableOutput("topp", height = 900)
             )
           ),
           
           fluidRow(
             box( title = "Pieces picked by date", solidHeader = TRUE, status = "primary", width = 12,
                  dataTableOutput("tbl4", height = 900)
             )
           )
   
    
    ),
    
  )
  
),

#tab 2 
  tabItem(tabName = "about",h1("About"),
          
          fluidRow(
            h1("Project Author: Parikshit Solunke"),
            h1("Libraries Used: 1)dplyr,2)shiny,3)shinydashboard,4)ggplot2,5)leaflet,6)DT,7)Lubridate"),
            h1("Data: Local data from Litterati, Source: https://www.evl.uic.edu/aej/424/litteratichallenge-65.csv")
            
            
          )
          
          
          
          )
  
  


),

)
)







# Define server logic required to draw a histogram
server <- function(input, output) {

#Define custom function to use with filter in the following section    
  conditional <- function(condition, success) {
    if (condition) success else TRUE
  }
  
#Apply filters according to user input and store the filtered data in df2
 df2 <- reactive({
                  df %>%
                    filter(
                      conditional(input$month != 'All', month == input$month ),
                      conditional(input$uname != 'All', username == input$uname ),
                      conditional(input$tod != 'All', hr == input$tod ),
                      conditional(input$tag !='All', grepl(input$tag,tags ) )
                   
                  )
 })
 
 #Apply filters(except tag based filter, for tag tables) according to user input and store the filtered data in df3
 df3 <- reactive({
   df %>%
     filter(
       conditional(input$month != 'All', month == input$month ),
       conditional(input$uname != 'All', username == input$uname ),
       conditional(input$tod != 'All', hr == input$tod ),
       
     )
 })
 
 
 
 
  output$bar <- 
    renderPlot({
      #temporary variables to manipulate without touching the original dataframes
      a<-df2()
      b<-df
      
      #if filtered data is not empty and if filters are applied
      if(nrow(a)!=0 && nrow(a)!=nrow(b))
      ggplot( environment = environment()) + geom_bar(data=df,aes(x=dt, y=no),  fill="steelblue", stat="identity")+ geom_bar(data=a,aes(x=dt, y=no),fill= "salmon" ,stat="identity") + labs(x="Time", y = "Pieces picked") + scale_x_date(date_breaks = "1 month", date_labels =  "%b %y", expand = c(0,0))
      
      else
      ggplot(df, aes(x=dt, y=no), environment = environment() ) + geom_bar(stat="identity", fill="steelblue") + labs(x="Time", y = "Pieces picked") + scale_x_date(date_breaks = "1 month", date_labels =  "%b %y", expand = c(0,0))
      
      
    })
  
  output$bar2 <- renderPlot({
    #temporary variables to manipulate without touching the original dataframes
    a<-df2()
    b<-df
    
    #if filtered data is not empty and if filters are applied
    if(nrow(a)!=0 && nrow(a)!=nrow(b))
    ggplot( environment = environment() ) + geom_bar(data=df,aes(x=day, y=no),  fill="steelblue", stat="identity")+ geom_bar(data=a,aes(x=day, y=no),fill= "salmon" ,stat="identity") + labs(x="Day", y = "Pieces picked") 
    
    else
    ggplot(df, aes(x=day , y=no), environment = environment() ) + geom_bar(stat="identity", fill="steelblue") + labs(x="Day", y = "Pieces picked") 
  })    
  
  
  output$bar3 <- renderPlot({
    #temporary variables to manipulate without touching the original dataframes
    a<-df2()
    b<-df
    
    #if filtered data is not empty and if filters are applied
    if(nrow(a)!=0 && nrow(a)!=nrow(b))
    ggplot(environment = environment() ) + geom_bar(data=df,aes(x=hr, y=no),  fill="steelblue", stat="identity")+ geom_bar(data=a,aes(x=hr, y=no),fill= "salmon" ,stat="identity") + labs(x="", y = "Items picked")+  scale_x_continuous("Hour", labels = 0:23,breaks = 0:23) 
   
     else
     ggplot(df, aes(x=hr, y=no), environment = environment() ) + geom_bar(stat="identity", fill="steelblue") + labs(x="", y = "Items picked")+  scale_x_continuous("Hour", labels = 0:23,breaks = 0:23)
    
  })
  
  output$bar4 <- renderPlot({
    #temporary variables to manipulate without touching the original dataframes
    
    a<-df2()
    b<-df
    
    #if filtered data is not empty and if filters are applied
    if(nrow(a)!=0 && nrow(a)!=nrow(b))
    {
    #The original list of tags is imported into TagFreq2, number column is made 0 , TagFreq3 is used to store details about the filtered data and get the count of the tags, the tags are compared with the original tags and only the corresponding data is loaded into TagFreq2 
    TagFreq2<-TagFreqh
    TagFreq2$Number=0
    TagFreq3<-setNames(as.data.frame(table(unlist(strsplit(a$tags, ",", fixed=TRUE)))),c("Tag","Number"))
    TagFreq3<-TagFreq3[order(TagFreq3$Number, decreasing = TRUE),]
    TagFreq2$Number<-TagFreq3[match(TagFreq2$Tag, TagFreq3$Tag),2]
    TagFreq2[is.na(TagFreq2)]<- 0
  
    ggplot(environment = environment()) + geom_bar(data=TagFreqh,aes(x=Tag, y=Number),  fill="steelblue", stat="identity")+ geom_bar(data= TagFreq2, aes(x=Tag, y=Number),fill= "salmon" ,stat="identity") + labs(x="Tags", y = "Number") 
    }
    #plot the summary top ten tags
    else
    ggplot(data=TagFreqh, aes(x=Tag , y=Number)) + geom_bar(stat="identity", fill="steelblue") + labs(x="Tags", y = "Number") 
    
  })  
  
  
  #used to display the total count
  output$total <- renderText({paste("Total pieces of litter picked up through the campaign:", total,"\nNOTE: Blue bars indicate summary, orange indicate filtered data. Likewise yellow & green for the Map")})
  
  
  #Top pickers table
  output$topp<-renderDataTable({
    a<-df2()
    tbldata <-setNames(aggregate(a$no, by=list(a$username), FUN=sum, na.rm=TRUE),c("User","Number"))
    tbldata<-tbldata[order(tbldata$Number, decreasing = TRUE),]
    
    datatable(tbldata,selection =list(mode = 'none'),rownames=FALSE,options = list(pageLength = 10))
  })
  
    
  #Top Tags table
  
  output$tbl<-
    renderDataTable({
    a<-df3()
    TagFreq3<-setNames(as.data.frame(table(unlist(strsplit(a$tags, ",", fixed=TRUE)))),c("Tag","Number"))
    TagFreq3<-TagFreq3[order(TagFreq3$Number, decreasing = TRUE),]
      
    datatable(TagFreq3,selection =list(mode = 'none'),rownames=FALSE,options = list(pageLength = 10))
  })
  
  # Table of objects picked by day of the week
  
  output$tbl2<-renderDataTable({
    a<-df2()
    dayT<-a[,c("day","no")]
    dayT<- setNames(aggregate(dayT$no, by=list(dayT$day), FUN=sum, na.rm=TRUE),c("Day", "Number"))
    
    datatable(dayT,selection =list(mode = 'none'),rownames=FALSE,options = list(pageLength = 10))
  })
  
  # Table of objects picked by time of the day
  
  output$tbl3<-renderDataTable({
    a<-df2()
    hourT<-a[,c("hr","no")]
    hourT<-setNames(aggregate(hourT$no, by=list(hourT$hr), FUN=sum, na.rm=TRUE),c("Hour", "Number"))
    datatable(hourT,selection =list(mode = 'none'),rownames=FALSE,options = list(pageLength = 10))
  })
  
  # Table of objects picked by Date
  
  output$tbl4<-renderDataTable({
    a<-df2()
    dateT<-a[,c("dt","no")]
    dateT<-setNames(aggregate(dateT$no, by=list(dateT$dt), FUN=sum, na.rm=TRUE),c("Date", "Number"))
    datatable(dateT,selection =list(mode = 'none'),rownames=FALSE,options = list(pageLength = 10))
  })
  
  
  
  output$leaf <- renderLeaflet({
    #temp variables to manipulate dataframe
    a<-df2()
    b<-df
    map <- leaflet()
    map <- addTiles(map)
    map <- setView(map, lng = -87.73, lat = 41.86513, zoom = 9)
    
    #modify default cluster marker colours(as size affects the colour)
    map <- addMarkers(map, lng = df$lon, lat = df$lat, clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {    
    var childCount = cluster.getChildCount(); 
    var c = ' marker-cluster-';  
    if (childCount < 100) {  
      c += 'medium';  
    } else if (childCount < 1000) {  
      c += 'medium';  
    } else { 
      c += 'medium';  
    }    
    return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', className: 'marker-cluster' + c, iconSize: new L.Point(40, 40) });

    }")))
    
    #if filters are added
    if(nrow(a)!=nrow(b))
    {
    #modify default cluster marker colours(as size affects the colour)
      
    map <- addMarkers(map, lng = a$lon, lat = a$lat, clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {    
    var childCount = cluster.getChildCount(); 
    var c = ' marker-cluster-';  
    if (childCount < 100) {  
      c += 'small';  
    } else if (childCount < 1000) {  
      c += 'small';  
    } else { 
      c += 'small';  
    }    
    return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', className: 'marker-cluster' + c, iconSize: new L.Point(40, 40) });

    }")))
    }
    map
  })
}

# Run the application 
shinyApp(ui = ui, server = server)




