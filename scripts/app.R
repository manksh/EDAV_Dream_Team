## app.R ##
library(shiny)
library(shinydashboard)
source('global.R')

######### Load Data ############
spotifydf <- read.csv("billboard-spotify.csv", stringsAsFactors=FALSE)
spotifydf <- as.tibble(spotifydf)

spotifydf<-spotifydf%>%
    filter(!is.na(duration_ms))%>%
    mutate(duration=duration_ms/1000/60)

spotifydf <- spotifydf%>%
    group_by(year) 

##############UI ########################
ui <- dashboardPage(
    dashboardHeader(title = "DreamTeam Final Project"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Artists", tabName = "dashboard1"),
            menuItem("Songs", tabName = "dashboard2"),
            menuItem("Evolution over Time", tabName = "dashboard3")
        )
    ),
    dashboardBody(
        tabItems(
            
            ### ARTISTS DASHBOARD
            tabItem(tabName = "dashboard1",
                    sidebarLayout(
                        sidebarPanel(
                            h3('What information about artists are you interested in?'),
                            selectInput("artists", label = h5("Select:"), choices = c("Artist Lifespan" =  "relevance",
                                                                                      "Most Collaborative Artists" = "collaboration"
                            ),c('Artist Lifespan')),
                            hr(),
                            helpText("The dropdown menu above contains three interesting aspects of the artists that have appeared in the Billboard Top 100 that we have investigated. 
                                     You can look at an artist's lifespan or the most collaborative artists.All analyses are computed over 1965-2015."),
                            
                            br()),
                        mainPanel(
                            h2('Analysis of Billboard Top 100 Artists'),
                            
                            hr(),
                            #plotOutput("artistlifespan"), # Note that artistlifespan should appear as a reactive in server
                            hr(),
                            #plotOutput("collaborative"),
                            hr()

                        )
                        )),
            #### WORDS ANALYSIS DASHBOARD
            tabItem(tabName = "dashboard2",
                    sidebarLayout(
                        sidebarPanel(
                            h3('Top 100 Ngrams'),
                            selectInput("ngramdecade", label = h5("Select:"), choices = c("1960s"="1960s",
                                                                                      "1970s"="1970s",
                                                                                      "1980s"="1980s",
                                                                                      "1990s"="1990s",
                                                                                      "2000s"="2000s",
                                                                                      "2010s"="2010s"
                                                                                    ),
                            c('1960s')),
                            hr(),
                            br()),
                        mainPanel(
                            h1('Analysis of Songs'),
                            
                            hr(),
                            plotlyOutput('ngrams')
                        )
                    )
            ),
            ### evolution over time dashboard
            tabItem(tabName = "dashboard3",
                    sidebarLayout(
                        sidebarPanel(
                            h3('Select a variable of interest'),
                            selectInput("variable", label = h5("Select:"), choices = c("Duration" =  "duration",
                                                                                      "Acousticness" = "acousticness",
                                                                                      "Danceability" = "danceability",
                                                                                      "Energy" = "energy",
                                                                                      "Instrumentalness" = "instrumentalness",
                                                                                      "Liveness" = "liveness",
                                                                                      "Loudness" = "loudness",
                                                                                      "Speechiness" = "speechiness",
                                                                                      "Tempo" = "tempo",
                                                                                      "Valence" = "valence"
                                                                                      ),c('Duration')),
                            hr(),
                            # helpText("Continuous variables within Billboard Top 100 include: Duration, Acousticness, Danceability
                            #          , Energy, Instrumentalness, Liveness, Loudness, Speechiness, Tempo, and Valence"),
                        
                            br()),
                        
                        
                        mainPanel(
                            h2('Evolution of Music Between 1965-2015'),
                            hr(),
                            plotOutput('evolution1'),
                            textOutput('evolution2'),
                            hr(),
                            plotOutput('evolution3')
                        )
                    ))
        )
    )
)






server <- function(input, output) {

    
    ####################
    #Dashboard 3 graphs
    #Line Graphs
    output$evolution1 <- renderPlot({
        if(input$variable=="duration"){
                spotifydf2<-spotifydf%>%
                                    summarize(avg_duration = mean(duration))
                g1<- ggplot(spotifydf2, aes(year, avg_duration)) + geom_line() +
                    ggtitle("Average Duration over Time")+
                    ylab("Average Duration")+
                    xlab("Year")+
                    scale_y_continuous(breaks = seq(2.5, 5, .5))+
                    scale_x_continuous(breaks = seq(1960,2020,5))
                print(g1)
                renderText({"Test"})
        } else if (input$variable=="acousticness") {
            spotifydf3<-spotifydf%>%
                summarize(average_acoustic = mean(acousticness))
            g2<- ggplot(spotifydf3, aes(year, average_acoustic)) + geom_line() +
                ggtitle("Average Acousticness over Time") +
                ylab("Average Acousticness")+
                xlab("Year")+
                scale_x_continuous(breaks = seq(1960,2020,5))+
                scale_y_continuous(breaks = seq(0.0, 0.5, 0.05))
            print(g2)
            renderText({"Test1"})
        } else if (input$variable=="danceability") {
            spotifydf4<- spotifydf%>%
                summarize(avg_dance = mean(danceability))
            
            g3 <- ggplot(spotifydf4, aes(year, avg_dance)) + geom_line() +
                ggtitle("Average Danceability over Time")+
                ylab("Average Danceability ")+
                xlab("Year")+
                scale_x_continuous(breaks = seq(1960,2020,5))+
                scale_y_continuous(breaks = seq(0.40, 0.8, 0.05))
            g3
        } else if (input$variable=="energy") {
            spotifydf5<- spotifydf%>%
                summarize(avg_energy = mean(energy))
            
            g4 <- ggplot(spotifydf5, aes(year, avg_energy)) + geom_line() +
                ggtitle("Average Energy over Time")+
                ylab("Average Energy ")+
                xlab("Year")+
                scale_x_continuous(breaks = seq(1960,2020,5))+
                scale_y_continuous(breaks = seq(0.40, 0.8, 0.05))
            g4
        } else if (input$variable=="instrumentalness") {
            spotifydf6<- spotifydf%>%
                summarize(avg_instrument = mean(instrumentalness))
            
            g5 <- ggplot(spotifydf6, aes(year, avg_instrument)) + geom_line() +
                ggtitle("Average Instrumentalness over Time")+
                ylab("Average Instrumentalness ")+
                xlab("Year")
                scale_x_continuous(breaks = seq(1960,2020,5))
            g5
        } else if (input$variable=="liveness") {
            spotifydf7<- spotifydf%>%
                summarize(avg_liveness = mean(liveness))
            
            g6 <- ggplot(spotifydf7, aes(year, avg_liveness)) + geom_line() +
                ggtitle("Average Liveness over Time")+
                ylab("Average Liveness ")+
                xlab("Year")+
                scale_x_continuous(breaks = seq(1960,2020,5))+
                scale_y_continuous(breaks = seq(0.0, 0.3, 0.02))
            g7
        } else if (input$variable=="loudness") {
            spotifydf8<- spotifydf%>%
                summarize(avg_loudness = mean(loudness))
            
            g7 <- ggplot(spotifydf8, aes(year, avg_loudness)) + geom_line() +
                ggtitle("Average Loudness over Time")+
                ylab("Average Loudness ")+
                xlab("Year")+
                scale_x_continuous(breaks = seq(1960,2020,5))
            
            g8
        } else if (input$variable=="speechiness") {
            spotifydf9<- spotifydf%>%
                summarize(avg_speech = mean(speechiness))
            
            g8 <- ggplot(spotifydf9, aes(year, avg_speech)) + geom_line() +
                ggtitle("Average Speechiness over Time")+
                ylab("Average Speechiness ")+
                xlab("Year")+
                scale_x_continuous(breaks = seq(1960,2020,5))+ 
                scale_y_continuous(breaks = seq(0.0,0.15,0.03))
            g8
        } else if (input$variable=="tempo") {
            spotifydf10<- spotifydf%>%
                summarize(avg_tempo = mean(tempo))
            
            g9 <- ggplot(spotifydf10, aes(year, avg_tempo)) + geom_line() +
                ggtitle("Average Tempo over Time")+
                ylab("Average Tempo ")+
                xlab("Year")+
                scale_x_continuous(breaks = seq(1960,2020,5))
            g9
        }
        
        else {
            spotifydf11<- spotifydf%>%
                summarize(avg_valence = mean(valence))
            
            g10 <- ggplot(spotifydf11, aes(year, avg_valence)) + geom_line() +
                ggtitle("Average Valence over Time")+
                ylab("Average Valence")+
                xlab("Year")
                scale_x_continuous(breaks = seq(1960,2020,5))
            g10
        }
    })
    # Explanatory Text with Average Line Graphs
    output$evolution2 <- renderText({
        paste("You have selected", input$variable)
        if(input$variable=='duration'){
            "There is no clear overall trend in Duration over the years."
        } else if(input$variable =='acousticness'){
            "There is a clear decline in Acousticness of songs over the years of our dataset."
        } else if (input$variable =='danceability'){
            "There is a surprising dip in Danceability in 2010."
        } else if (input$variable =='energy'){
            "There seems to be a general upward trend seen in Energy throughout the years but the data also shows around 3 plateaus every 20 years."
        } else if (input$variable =='instrumentalness'){
            "There is a general decline in Instrumentalness of songs over the years."
        } else if (input$variable =='liveness'){
            "There is no clear trend in Liveness of songs over the years. Liveness does peak at 2010 though."
        } else if (input$variable =='loudness'){
            "There is a general upward increasing trend in Loudness of songs over the years. There seems to be a peak around 1983 which may require further investigation why that is. Note negative scale for loudness makes sense."
        } else if (input$variable =='speechiness'){
            "From 1965-1990, there was a pretty constant level of speechiness in songs. Speechiness peaks around 2000-2005."
        } else if (input$variable =='tempo'){
            "Tempo remains fairly consistent around 115 during the years 1985-2005. And there is a dip in tempo in 2010."
        } else{
           "Valence generally declines over the years in our dataset."
        }
    })
    # Box Plots
    output$evolution3 <- renderPlot({
        if(input$variable=='duration'){
            ggplot(spotifydf) + geom_boxplot(aes(year, duration, group=year))+
                ggtitle("Duration over Time")+
                ylab("Duration (in Minutes)")+
                xlab("Year")+
                scale_x_continuous(breaks = seq(1960,2020,5))
        } else if(input$variable =='acousticness'){
            ggplot(spotifydf) + geom_boxplot(aes(year, acousticness, group=year)) +
                ggtitle("Acousticness over Time")+
                ylab("Acousticness")+
                xlab("Year")+
                scale_x_continuous(breaks = seq(1960,2020,5))
        } else if (input$variable =='danceability'){
            ggplot(spotifydf) + geom_boxplot(aes(year, danceability, group=year)) +
                ggtitle("Danceability over Time")+
                ylab("Danceability ")+
                scale_x_continuous(breaks = seq(1960,2020,5))
        } else if (input$variable =='energy'){
            ggplot(spotifydf) + geom_boxplot(aes(year, energy, group=year)) +
                ggtitle("Energy over Time")+
                ylab("Energy ")+
                xlab("Year")+
                scale_x_continuous(breaks = seq(1960,2020,5))
        } else if (input$variable =='instrumentalness'){
            ggplot(spotifydf) + geom_boxplot(aes(year, instrumentalness, group=year)) +
                ggtitle("Instrumentalness over Time")+
                ylab("Instrumentalness ")+
                xlab("Year")+
                scale_x_continuous(breaks = seq(1960,2020,5))
        } else if (input$variable =='liveness'){
            ggplot(spotifydf) + geom_boxplot(aes(year, liveness, group=year)) +
                ggtitle("Liveness over Time")+
                ylab("Liveness ")+
                xlab("Year")+
                scale_x_continuous(breaks = seq(1960,2020,5))
        } else if (input$variable =='loudness'){
            ggplot(spotifydf) + geom_boxplot(aes(year, loudness, group=year)) +
                ggtitle("Loudness over Time")+
                ylab("Loudness ")+
                xlab("Year")+
                scale_x_continuous(breaks = seq(1960,2020,5))
        } else if (input$variable =='speechiness'){
            ggplot(spotifydf) + geom_boxplot(aes(year, speechiness, group=year)) +
                ggtitle("Speechiness over Time")+
                ylab("Speechiness ")+
                scale_x_continuous(breaks = seq(1960,2020,5))
            
        } else if (input$variable =='tempo'){
            ggplot(spotifydf) + geom_boxplot(aes(year, tempo, group=year)) +
                ggtitle("Tempo over Time")+
                ylab("Tempo ")+
                xlab("Year")+
                scale_x_continuous(breaks = seq(1960,2020,5))
        } else{
            ggplot(spotifydf) + geom_boxplot(aes(year, valence, group=year)) +
                ggtitle("Valence over Time")+
                ylab("Valence")+
                xlab("Year")+
                scale_x_continuous(breaks = seq(1960,2020,5))
        }
        
        
    })
#End marker for server         
}


shinyApp(ui, server)