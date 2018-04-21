## app.R ##
library(shiny)
library(shinydashboard)
source('global.R')


######### Load Data ############
spotifydf <- read.csv("billboard-spotify.csv", stringsAsFactors=FALSE)
spotifydf <- as.tibble(spotifydf)

#Get rid of duplicates
df = spotifydf[!(duplicated(spotifydf[c('artist', 'song')])),]
# calculate words per second
temp = strsplit(df$lyrics, split=" ")
df['words_per_sec'] = sapply(temp, length) / (df['duration_ms'] / 1000)

# calculate duration in minutes
df['duration_min'] = df['duration_ms'] / 1000 / 60

# create a decade column
df['decade'] = floor(df['year'] / 10) * 10

# create base artist by stripping away featured artists
df = mutate(df, artist_base = str_replace_all(artist, "\\s\\(*feat.*", ""))

spotifydf<-spotifydf%>%
    filter(!is.na(duration_ms))%>%
    mutate(duration=duration_ms/1000/60)

spotifydf <- spotifydf%>%
    group_by(year) 

#Define subsetted spotify df with keepcols
keepcols <- c('year', 'acousticness', 'danceability', 'duration', 'energy', 'instrumentalness', 'liveness','loudness','speechiness', 'tempo', 'valence')

spotifydf_s <- spotifydf%>%
    dplyr::select(keepcols) %>%
    group_by(year)%>%
    summarize(acoustic = mean(acousticness), 
              danceability = mean(danceability), 
              duration = mean(duration), 
              energy = mean(energy), 
              instrument = mean(instrumentalness), 
              liveness = mean(liveness), 
              loudness = mean(loudness), 
              speechiness = mean(speechiness), 
              tempo = mean(tempo), 
              valence = mean(valence)
    )
# %>%
#                 gather(key='variable', value = 'Freq', -year)
spotifydf_s$year<- factor(spotifydf_s$year, levels = unique(spotifydf_s$year))

########################################
#Word cloud stuff 

# The list of valid n-grams
grams <<- list("N-grams" = "n-grams",
               "Unigrams" = "unigram",
               "Bigrams" = "bigram",
               "Trigrams" = "trigram")

decades <<-  list("1960s"="1960",
                  "1970s"="1970",
                  "1980s"="1980",
                  "1990s"="1990",
                  "2000s"="2000",
                  "2010s"="2010")

# Using "memoise" to automatically cache the results
getN_gram <- memoise(function(gram, decade) {
    if (!(gram %in% grams))
        stop("Unknown category")
    if (!(decade %in% decades))
        stop("Unknown category")
    current_decade = as.numeric(decade)
    current_gram = gram

    n_gram <- read.csv("n_gram.csv")
    if (current_gram == "n-grams") {
        # print('hi')
        n_gram_filtered <- filter(n_gram, n_gram$decade == current_decade)
    } else {
        # print('hello')
        n_gram_filtered <- filter(n_gram, n_gram$decade == current_decade, n_gram$gram == current_gram)
    }
    return(n_gram_filtered)
})
######################
#Covers Over Time
cover <- read.csv("cover_year.csv")

# Word Count over Time
word_count <- read.csv("word_count_per_year.csv")


##############UI ########################
ui <- dashboardPage(
    dashboardHeader(title = "DreamTeam Final Project"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Artists", tabName = "dashboard1"),
            menuItem("Word Clouds", tabName = "dashboard2"),
            menuItem("Songs", tabName = "dashboard4"),
            menuItem("Evolution over Time", tabName = "dashboard3")
        
        )
    ),
    dashboardBody(
        # uiOutput("ui"),
        tabItems(
            
            ### ARTISTS DASHBOARD
            tabItem(tabName = "dashboard1",
                    sidebarLayout(
                        sidebarPanel(
                            helpText("Maximize the browser window. There are three aspects of Billboard Top 100 Artists plotted on this tab. Scroll down to see them all:
                                     you can look at an artist's lifespan, the most collaborative artists and most featured, or the level of collaboration over time. All analyses are computed over 1965-2015."),
                            checkboxGroupInput("ArtistsTabs", label= h4("Collaboration"), choices = list('View Collaboration Levels over Time'='collabo'),selected=NULL),
                            br()),
                        mainPanel(
                            h2('Analysis of Billboard Top 100 Artists'),
                            conditionalPanel(
                                condition = "input.ArtistsTabs=='collabo'",
                                tabBox(
                                    title = "Collaboration over Time",
                                    id="ttabs2", width = 250, height = "300px",
                                    dygraphOutput("COT", height = 200)
                                )),
                            plotOutput("artistlifespan"),
                            plotOutput('collaborative')

                        )
                        )),
            #### WORDS ANALYSIS DASHBOARD
            tabItem(tabName = "dashboard2",
                    sidebarLayout(
                        sidebarPanel(
                            h3('Top 100 Ngrams'),
                            selectInput("selection", 
                                        "Choose your n-gram range:",
                                        choices = grams),
                            selectInput("decade", "Choose your decade:", choices = decades),
                            helpText("Please be patient as the word clouds may appear slowly at first."),
                            hr(),
                            br()),
                        
                        mainPanel(
                            h1('Word Clouds'),
                            plotOutput('ngrams', width = "700px", height = "700px")

                        )
                    )
            ),
            ### Songs (# Covers + Word Counts over Time)
            tabItem(tabName = "dashboard4",
                    sidebarLayout(
                        sidebarPanel(
                            h3('Word Count over Time'),
                            helpText("Choose any two words to compare how they're used over time"),
                            selectInput("word1",
                                        "Choose a word:",
                                        choices = c("boy" = "boy",
                                                    "girl" = "girl",
                                                    "like" = "like",
                                                    "love" = "love",
                                                    "man" = "man",
                                                    "peace" = "peace",
                                                    "war" = "war",
                                                    "woman" = "woman",
                                                    " " = " "), c("love")),
                            selectInput("word2", "Choose another word:", choices = c("boy" = "boy",
                                                                                     "girl" = "girl",
                                                                                     "like" = "like",
                                                                                     "love" = "love",
                                                                                     "man" = "man",
                                                                                     "peace" = "peace",
                                                                                     "war" = "war",
                                                                                     "woman" = "woman",
                                                                                     " " = " "), c("like")),
                            hr(),
                            h3('# Covers over Time'),
                            helpText("Slide the slider to change the time intervals analyzed for # of covers in that time period."),
                            hr(), 
                            sliderInput("binwidth",
                                        "Binwidth:",
                                        min = 1,  max = 15, value = 1),
                            br()),
                        
                        mainPanel(
                            h1('Analysis of Songs'),
                            hr(),
                            plotOutput("word_graph"),
                            uiOutput("video"),
                            hr(), 
                            br(),
                            plotOutput("cover_hist")
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
                            helpText("Click and drag to zoom in (double click to zoom back out)."),
                            hr(),
                            
                            checkboxGroupInput("Tabs", label= h4("Interactive"), choices = list('Parallel Coordinates'='parcoord'),selected=NULL),
                            helpText("Note: It may be necessary to maximize the dashboard window and hide the navigation  menu on the left to visualize the parallel coordinates entirely. Also note that the years have an
additional comma."),
                            # helpText("Continuous variables within Billboard Top 100 include: Duration, Acousticness, Danceability
                            #          , Energy, Instrumentalness, Liveness, Loudness, Speechiness, Tempo, and Valence"),
                        
                            br()),
                        
                        
                        mainPanel(
                            conditionalPanel(
                                condition = "input.Tabs=='parcoord'",
                                tabBox(
                                    title = "Variables Tracked between 1965-2015",
                                    id="ttabs", width = 15, height = "300px",
                                    parcoordsOutput('parcoordvar', width=750, height = "300px")
                                )),
                            h2('Evolution of Music Between 1965-2015'),
                            hr(),
                            dygraphOutput("evolution1"),
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
    #Dashboard 1: ARTISTS graphs
        output$COT <- renderDygraph({
                collaborations = df %>%
                    mutate(is_collab = str_detect(artist, 'feat')) %>%
                    group_by(year) %>%
                    summarize(num_collaborations = sum(is_collab))
    
                dygraph(collaborations,
                        ylab = 'Number of top 100 singles featuring a guest artist',
                        xlab = "Year") %>%
                    dyOptions(drawGrid = input$showgrid)
        })
        
        output$artistlifespan <- renderPlot({
            top_artists = df %>%
                group_by(artist_base) %>%
                summarize(num_singles = n(), earliest_hit = min(year), latest_hit = max(year), longevity = latest_hit - earliest_hit, hits_per_year = num_singles / longevity) %>%
                arrange(desc(longevity))
            top_artists_30 = top_artists[0:30,]
            
            ggplot(top_artists_30) +
                geom_segment(aes(x=earliest_hit, xend=latest_hit, y=reorder(artist_base, longevity), yend=reorder(artist_base, longevity), color=num_singles), size=5) +
                geom_text(aes(x=latest_hit + 3, y=reorder(artist_base, longevity), label=paste(longevity, 'years'))) +
                ggtitle('Career spans of most timeless artists') + ylab("Artist")+xlab("Earliest Hit in this Year")
        })
        
        output$collaborative <- renderPlot({
            most_featuring_artists = df %>%
                mutate(is_collab = str_detect(artist, 'feat')) %>%
                group_by(artist_base) %>%
                summarize(num_collaborations = sum(is_collab)) %>%
                arrange(desc(num_collaborations))
            most_featuring_artists = most_featuring_artists[0:20,]
            
            p1 = ggplot(most_featuring_artists, aes(x = reorder(artist_base, num_collaborations),
                                                    y = num_collaborations)) +
                geom_col() +
                xlab('Main artist') +
                ylab('Number of top 100 singles featuring a guest artist') +
                ggtitle("Artists who Feature Others Most")+
                scale_y_continuous(breaks=1:9, labels=1:9) + 
                coord_flip()
            
            matches = str_match(as.list(df['artist'])$artist, 'featuring\\s(.*)')
            matches = matches[, 2]
            matches = matches[!is.na(matches)]
            matches = as_tibble(matches)
            
            featured_artists = matches %>%
                group_by(value) %>%
                summarize(num_features = n()) %>%
                arrange(desc(num_features))
            featured_artists = featured_artists[1:20,]
            
            p2 = ggplot(featured_artists, aes(x = reorder(value, num_features),
                                              y = num_features)) +
                geom_col() +
                xlab('Featured artist') +
                ylab('Number of top 100 singles featured as guest') +
                ggtitle("Most Featured Artists")+
                scale_y_continuous(breaks=1:12, labels=1:12) + 
                coord_flip()
            
            graph <- grid.arrange(p1, p2, ncol=2)
            print(graph)
        })
    
    ####################
    #Dashboard 2 Graphs
    
    pal <- brewer.pal(9, "OrRd")
    pal <- pal[-(1:3)]
    
    output$ngrams <- renderPlot({
        ngram_selection <- getN_gram(input$selection, input$decade)
        wordcloud(ngram_selection$word, ngram_selection$count, scale=c(5, 1), min.freq=3, random.order = FALSE , random.color = FALSE,
                  rot.per=.15, colors=pal)
    }, width=700, height=700)
    
    output$word_graph <- renderPlot({

        word_1 <- input$word1
        word_2 <- input$word2

        if (word_1 == " " & word_2 == " "){}
        else if (word_1 == " "){
            data = filter(word_count, word_count$word == word_2)
            ggplot(data, aes(x = year, y = count)) +
                geom_line() +
                theme(legend.position="bottom") +
                ggtitle(word_2) +
                theme(plot.title = element_text(hjust = 0.5)) +
                xlab("Year") +
                ylab("Count")
        }
        else if (word_2 == " " | word_1 == word_2){
            data = filter(word_count, word_count$word == word_1)
            ggplot(data, aes(x = year, y = count)) +
                geom_line() +
                theme(legend.position="bottom") +
                ggtitle(word_1) +
                theme(plot.title = element_text(hjust = 0.5)) +
                xlab("Year") +
                ylab("Count")
        }
        else {
            data = filter(word_count, word_count$word == word_1 | word_count$word == word_2)
            ggplot(data, aes(x = year, y = count, col = word)) +
                geom_line() +
                theme(legend.position="bottom") +
                ggtitle(paste(word_1,word_2, sep="/")) +
                theme(plot.title = element_text(hjust = 0.5)) +
                xlab("Year") +
                ylab("Count")
        }
    })

    output$video <- renderUI({
        word_1 <- input$word1
        word_2 <- input$word2
        if (word_1 == " " & word_2 == " "){
            HTML(paste0('<iframe width="500" height="300" src="https://www.youtube.com/embed/SHJ1HL4eMLQ", frameborder="0" allowfullscreen></iframe>'))
        }
        else{}
    })

    output$cover_hist <- renderPlot({

        bin_width <- input$binwidth
        ggplot(cover, aes(year)) +
            geom_histogram(color = "black", fill = "lightblue", binwidth = bin_width) +
            theme(legend.position="bottom") +
            ggtitle("# of Covers over Time") +
            theme(plot.title = element_text(hjust = 0.5)) +
            xlab("Year") +
            ylab("Count")
    })
    
    ####################
    #Dashboard 3 graphs
    #Parallel Coordinate Graph
    spotifydf_s$year <- year(as.Date(as.character(spotifydf_s$year), "%Y"))
    rownames(spotifydf_s)<- spotifydf_s$year
    
    output$parcoordvar = renderParcoords(
        spotifydf_s%>% arrange(year) %>%
            parcoords(
                rownames = F,
                brushMode = "1D-axes",
                reorderable = T,
                queue = T, 
                alpha=.8, 
                color = list(colorBy = "year", colorScale = htmlwidgets::JS("d3.scale.category10()")), 
                height = 300
            )
    )
    #Line Graphs
    # output$evolution1 <- renderPlotly({
    output$evolution1 <- renderDygraph({
        if(input$variable=="duration"){
                spotifydf2<-spotifydf%>%
                                    summarize(avg_duration = mean(duration))
                dygraph(spotifydf2, 
                        main = "Average Duration(in Minutes) from 1965-2015",
                        ylab = "Average Duration(Min)",
                        xlab = "Year") %>%
                dyOptions(drawGrid = input$showgrid)
        } else if (input$variable=="acousticness") {
            spotifydf3<-spotifydf%>%
                summarize(average_acoustic = mean(acousticness))
            dygraph(spotifydf3, 
                    main = "Average Acousticness from 1965-2015",
                    ylab = "Average Acousticness",
                    xlab = "Year") %>%
                dyOptions(drawGrid = input$showgrid)
        } else if (input$variable=="danceability") {
            spotifydf4<- spotifydf%>%
                summarize(avg_dance = mean(danceability))
            dygraph(spotifydf4, 
                    main = "Average Danceability from 1965-2015",
                    ylab = "Average Danceability",
                    xlab = "Year") %>%
                dyOptions(drawGrid = input$showgrid)
        } else if (input$variable=="energy") {
            spotifydf5<- spotifydf%>%
                summarize(avg_energy = mean(energy))
            
            dygraph(spotifydf5, 
                    main = "Average Energy from 1965-2015",
                    ylab = "Average Energy",
                    xlab = "Year") %>%
                dyOptions(drawGrid = input$showgrid)
        } else if (input$variable=="instrumentalness") {
            spotifydf6<- spotifydf%>%
                summarize(avg_instrument = mean(instrumentalness))
            
            dygraph(spotifydf6, 
                    main = "Average Instrumentalness from 1965-2015",
                    ylab = "Average Instrumentalness",
                    xlab = "Year") %>%
                dyOptions(drawGrid = input$showgrid)
        } else if (input$variable=="liveness") {
            spotifydf7<- spotifydf%>%
                summarize(avg_liveness = mean(liveness))
            
            dygraph(spotifydf7, 
                    main = "Average Liveness from 1965-2015",
                    ylab = "Average Livness",
                    xlab = "Year") %>%
                dyOptions(drawGrid = input$showgrid)
        } else if (input$variable=="loudness") {
            spotifydf8<- spotifydf%>%
                summarize(avg_loudness = mean(loudness))
            
            dygraph(spotifydf8, 
                    main = "Average Loudness from 1965-2015",
                    ylab = "Average Loudness",
                    xlab = "Year") %>%
                dyOptions(drawGrid = input$showgrid)
        } else if (input$variable=="speechiness") {
            spotifydf9<- spotifydf%>%
                summarize(avg_speech = mean(speechiness))
            
            dygraph(spotifydf9, 
                    main = "Average Speechiness from 1965-2015",
                    ylab = "Average Speechiness",
                    xlab = "Year") %>%
                dyOptions(drawGrid = input$showgrid)
        } else if (input$variable=="tempo") {
            spotifydf10<- spotifydf%>%
                summarize(avg_tempo = mean(tempo))
            
            dygraph(spotifydf10, 
                    main = "Average Tempo from 1965-2015",
                    ylab = "Average Tempo",
                    xlab = "Year") %>%
                dyOptions(drawGrid = input$showgrid)
        }
        
        else {
            spotifydf11<- spotifydf%>%
                summarize(avg_valence = mean(valence))
            
            dygraph(spotifydf11, 
                    main = "Average Valence from 1965-2015",
                    ylab = "Average Valence",
                    xlab = "Year") %>%
                dyOptions(drawGrid = input$showgrid)
        }
    })
    
    output$from <- renderText({
        strftime(req(input$dygraph_date_window[[1]]), "%Y ")      
    })
    
    output$to <- renderText({
        strftime(req(input$dygraph_date_window[[2]]), "%Y")
    })
    
    output$clicked <- renderText({
        strftime(req(input$dygraph_click$x), "%Y")
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