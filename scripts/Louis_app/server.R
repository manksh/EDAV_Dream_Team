library(tm)
library(wordcloud)
library(memoise)

# The list of valid n-grams
grams <<- list("N-grams" = "n-grams",
               "Unigrams" = "unigram",
               "Bigrams" = "bigram",
               "Trigrams" = "trigram")

# Using "memoise" to automatically cache the results
getN_gram <- memoise(function(gram) {
  if (!(gram %in% grams))
    stop("Unknown category")
  
  n_gram <- read.csv("../../data/Louis/n_gram.csv")
  if (gram == "n-grams") {
    n_gram_filtered <- filter(n_gram, n_gram$decade == 1960+10*i)
  } else {
    n_gram_filtered <- filter(n_gram, n_gram$decade == 1960+10*i, n_gram$gram == gram)
  }
})