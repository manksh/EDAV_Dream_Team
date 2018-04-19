function(input, output, session) {
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getN_gram(input$selection, input$decade)
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  #wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    pal <- brewer.pal(9, "OrRd")
    pal <- pal[-(1:3)]
    wordcloud(v$word, v$count, scale=c(5, 1), min.freq=3, random.order = FALSE , random.color = FALSE,
              rot.per=.15, colors=pal)
  }, width=700, height=700)
}