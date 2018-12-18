library(shinydashboard)
library(tidyverse)
library(benford.analysis)
library(plotly)
library(gapminder)

athlete<-read_csv("athlete_events.csv")
region<-read_csv("noc_regions.csv")
athlete[which(athlete$Sex=="M"),3]<-"Male"
athlete[which(athlete$Sex=="F"),3]<-"Female"
  

# the design of UI------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Olympic Games", titleWidth = 450,
                  
  
  ## Dropdown menu - team message.......................................
  dropdownMenu(type = "messages",
               messageItem(
                 from = "Wenjia(Grace) Xie",
                 message = "wenjia@bu.edu"
               )
               
  )),
  
  
  ## siderbar content...................................................
  
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarmenu",
      menuItem("Introduction",
               tabName = "Introduction", 
               icon = icon("leaf",
                           lib = "glyphicon")),
      menuItem("Visualization",
               tabName = "Visualization",
               icon = icon("chevron-down",lib="glyphicon"),
               menuItem("Gender",
                        tabName = "gender",
                        icon = icon("map-marker",lib = "glyphicon")
               ),
               menuItem("Sports",
                        tabName = "sports",
                        icon = icon("thumbs-up", lib = "glyphicon")
               ),
               menuItem("Country",
                        tabName = "country",
                        icon = icon("bar-chart-o")
               )
      ),
      menuItem("Benford Analysis", 
               tabName = "bfd", 
               icon = icon("question-circle")),
      menuItem("Data Table", 
               tabName = "data", 
               icon = icon("th-list",lib = "glyphicon"))
    )
  ),
  
  
  
  ## body content.......................................................
  dashboardBody(
    
    tabItems(
      
      
    ###Indroduction structure
      tabItem(
        tabName = "Introduction",
        fluidRow(
          box(
            title = "Introduction", 
            width = 12, 
            solidHeader = TRUE, 
            status = "primary",
            collapsible  = TRUE,
            h3("120 years of Olympic history:"),
            print("This is a historical dataset on the modern Olympic Games, including all the Games from Athens 1896 to Rio 2016. "),
            h4(""),
            print("The Olympic data on www.sports-reference.com is the result of an incredible amount of research by a group of Olympic history enthusiasts and self-proclaimed 'statistorians'. 
                  Check out their blog for more information. Randi H Griffin scraped this data from the webpage and consolidated their work into a convenient format for data analysis.")
             )
          )
        ),
              
      
    ### gender structure
      tabItem(tabName = "gender",
              h2("Gender difference")
      ),
    
    
    ### game difference structure  
      tabItem(
        tabName = "sports",
        fluidRow(
          column(4, 
                 selectInput('season2', 'Season:', 
                             c("Winter","Summer"),
                             "Summer")
          ),
          column(8,
                 selectInput('gender2', 'Gender:',
                             c("Male","Female"),
                             "Bronze")
                             
          ),
          
          fluidRow(
            column(width = 10,
                   box(
                     title = "Differrence of height and weight between sports", 
                     height=500,width = 12,
                     solidHeader = TRUE, 
                     status = "primary",
                     collapsible  = T,
                     plotlyOutput("sport")
                   )
            )
          )
        )
              
      ),
    
    
    
    
    
      
    ### countries structure  
       tabItem(
         tabName = "country",
         fluidRow(
           column(4, 
                  selectInput('season', 'Season:', 
                              c("Winter","Summer"),
                              "Summer")
           ),
           column(8,
                  selectInput('medal', 'The type of medal:',
                              c("Gold","Silver","Bronze"),
                              "Gold")
           ),
           
           fluidRow(
             column(width = 12,
                    box(
                      title = "Top 10 business", 
                      height=600,width = 12,
                      solidHeader = TRUE, 
                      status = "primary",
                      collapsible  = T,
                      plotlyOutput("map")
                        )
                    )
                    )
                 )
         ),
    
    
    
    
    ### Benford Analysis tab content
    tabItem(
      tabName = "bfd",
      fluidRow(
        box(title="Control Bar",
            width=4,
            radioButtons("Gender", "Gender:",
              c("Male" = "Male", "Female" = "Female"),
                   "Female"),
            radioButtons("Physique", "Physique:",
              c("Height" = "Height", "Weight" = "Weight"),
                   "Weight"),
        sliderInput("range", 
                    "Number of bins:",
                    min = 1, 
                    max = 100,
                    value = 50)),
        box(title="Distribution plot",
            width=8,
            plotlyOutput("distribution"))
        ),
      fluidRow(
        box(title="Benford Analysis",
            plotOutput("bfd",height = 500),
            width=12)
      )
    ),
    
    
    ### data table
    tabItem(
      tabName = "data",
      fluidRow(
        
      box(title = "Original Dataset", status = "success", 
          collapsible = TRUE,
            solidHeader = TRUE,
          DT::dataTableOutput("table"), 
          width = 12, height = 800)
        
      )
    )
    )
  )
)


# Define server logic required to draw a histogram----------------------

server <- function(input, output) {

  
  ## the benford analysis...............................................
  
  output$bfd <- renderPlot({
    
    bfdata<-athlete %>% 
      select(Name,Sex,Height,Weight) %>% 
      filter(Sex==input$Gender) %>% 
      unique() %>% 
      select(input$Physique) %>% 
      na.omit() 
    
    bfd.hw<- benford(bfdata[[1]],1)
    plot(bfd.hw)
    
  })
  
  ## the distribution of height and weight...............................
  output$distribution <- renderPlotly({
    
    dis_data<-athlete %>% 
      select(Name,Sex,as.vector(input$Physique)) %>%
      unique() %>% 
      na.omit()
  
    
    p<-ggplot(dis_data,
              aes(x=get(input$Physique),color=Sex))+
      geom_density(size=0.8,adjust=1.5)+
      geom_histogram(aes(y=..density..,fill=Sex), 
                     alpha=0.1, 
                     position="identity",
                     bins=input$range)+
      labs(title=paste("Density Distribution of Athletes'",
                       input$Physique),
           x=input$Physique,
           y="Density")
      theme_minimal()
    
    ggplotly(p)
    
    
  })
  
  
  ## the geo map of Olympics History ..................................
  output$map <- renderPlotly({
  
    Gcountry<-athlete %>% 
      select(Team,NOC,Year,Season,Sport,Games,Event,Medal) %>% 
      filter(Season==input$season) %>% 
      filter(Medal==input$medal) %>% 
      unique(by=c("NOC","Year","Event")) %>% 
      group_by(Year,NOC) %>% 
      summarise(nmedal=n()) 
      
    
   
    l <- list(color = toRGB("grey"), width = 0.5)  ### light grey boundaries 
    
    
    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator')
    )     ### specify map projection/options
    
    p <- plot_geo(Gcountry) %>%
      add_trace(
        z = ~nmedal, 
        color = ~nmedal,
        colors = 'Blues',
        frame = ~Year,
        locations = ~NOC,
        marker = list(line = l)
      ) %>%
      colorbar(title = paste("Number of",input$medal,"Medal")) %>%
      layout(title ='Evolution of the Olympics over time', 
             geo = g)
    
    p
    
 })

  
  ## the difference between sports .....................................
  
  
    output$sport <- renderPlotly({
      
      number<-athlete %>% 
        select(Year,Name,Sex,Height,Weight,Season,Sport) %>% 
        filter(Season==input$season2) %>% 
        filter(Sex==input$gender2) %>%
        group_by(Sport) %>% 
        summarise(number=n()) 
      
      
      sport<-athlete %>% 
        select(Year,Name,Sex,Height,Weight,Season,Sport) %>% 
        filter(Season==input$season2) %>% 
        filter(Sex==input$gender2) %>% 
        unique(by=c(Year,Name)) %>% 
        na.omit() %>% 
        group_by(Sport) %>% 
        summarise(mweight=mean(Weight),mheight=mean(Height)) %>% 
        left_join(number,by="Sport") 
      
      sport$mweight<-round(sport$mweight,2)
      sport$mheight<-round(sport$mheight,2)
      
      p <- plot_ly(sport, 
                   x = ~mweight, 
                   y = ~mheight, 
                   color = ~Sport,
                   colors='Paired',
                   size = ~number,
                   type = 'scatter',
                   mode = 'markers', 
                   marker = list(symbol = 'circle', 
                                 sizemode = 'diameter',
                                 line = list(width = 2, color = '#FFFFFF')),
                   text = ~paste('Sport:', Sport, 
                                 '<br> Mean Height:',round(mheight,2),
                                 '<br>Mean Weight:', round(mweight,2),
                                 '<br> Number of Athletes:', number)) %>%
        layout(title = 'Mean Weight vs. Mean Height,1986-2016',
               xaxis = list(title = 'Mean Weight',
                            gridcolor = 'rgb(255, 255, 255)',
                            range = c((min(sport$mweight)-5),(max(sport$mweight)+5)),
                            zerolinewidth = 1,
                            ticklen = 5,
                            gridwidth = 2),
               yaxis = list(title = 'Mean Height',
                            gridcolor = 'rgb(255, 255, 255)',
                            range = c((min(sport$mheight)-5),(max(sport$mheight)+5)),
                            zerolinewidth = 1,
                            ticklen = 5,
                            gridwith = 2),
               paper_bgcolor = 'rgb(243, 243, 243)',
               plot_bgcolor = 'rgb(243, 243, 243)') %>% 
        layout(showlegend = FALSE)
      
      
      
    })
  
  
  
  ## the orign data table................................................
    output$table<- DT::renderDataTable({
      tabledata <- na.omit(athlete)
      DT::datatable(tabledata, options = list(searching = TRUE,pageLength = 50,lengthMenu = c( 50, 100, 500), scrollX = T,scrollY = "300px"),rownames= FALSE
    )
  }) 

  
} 

# Run the application --------------------------------------------------
shinyApp(ui = ui, server = server)
