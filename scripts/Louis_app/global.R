function(input, output, session) {
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getN_gram(input$selection)
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    pal <- brewer.pal(9, "OrRd")
    pal <- pal[-(1:3)]
    wordcloud_rep(v$word, v$count,  min.freq = input$freq, scale=c(5,0.5), random.order = FALSE , random.color = FALSE,
                  colors=pal)
  })
}