library(tm)
library(wordcloud)
library(memoise)
library(dplyr)

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
  
  n_gram <- read.csv("../../data/Louis/n_gram.csv")
  if (current_gram == "n-grams") {
    print('hi')
    n_gram_filtered <- filter(n_gram, n_gram$decade == current_decade)
  } else {
    print('hello')
    n_gram_filtered <- filter(n_gram, n_gram$decade == current_decade, n_gram$gram == current_gram)
  }
  return(n_gram_filtered)
})