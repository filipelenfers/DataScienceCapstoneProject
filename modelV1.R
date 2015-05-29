#With all words, frequence > 10

library(tm)
library(data.table)

#All the data has the score pre calculated considering alpha as 0.4
load("spData4.RData")

bad.words <- read.csv("en_profanity_words.txt")
bad.words <- tolower(bad.words[,1])

predict <- function(input,num.results = 3) {
  #Clean input
  input <- tolower(input) 
  input <- removePunctuation(input)
  input <- removeNumbers(input)
  input <- stripWhitespace(input)
  
  #Get keys
  input.words <- strsplit(input," ")[[1]]
  input.words.size <- length(input.words)
  tetragram.key <- ifelse(input.words.size >= 3, paste(input.words[(input.words.size-2):input.words.size],collapse = " "), NA)
  trigram.key <- ifelse(input.words.size >= 2, paste(input.words[(input.words.size-1):input.words.size],collapse = " "), NA)
  bigram.key <- input.words[input.words.size]
  #   print(tetragram.key)
  #   print(trigram.key)
  #   print(bigram.key)
  
  #4-gram case, 1.0 (for this 4-gram model) , just the top num.results
  tetragram.subset <- tetragram.data.table[key == tetragram.key,]
  predictions <- head(tetragram.subset[order(tetragram.subset$score, decreasing = T)],n = num.results)
  num.rows <- nrow(predictions)
  #print(paste("Num.rows tetragram",num.rows))
  if(num.rows < num.results) {
    #3-gram case, 0.4 (for this 4-gram model) , just the top num.results
    trigram.subset <- trigram.data.table[key == trigram.key,]
    trigram.subset <- trigram.subset[!(trigram.subset$target %chin% predictions$target),]
    trigram.predictions <- head(trigram.subset[order(trigram.subset$score, decreasing = T)],n = num.results-num.rows)
    
    predictions <- rbind(predictions, trigram.predictions)
    num.rows <- nrow(predictions)
    
    if(num.rows < num.results) {
      #2-gram case, 0.4 * 0.4 (for this 4-gram model) , just the top num.results
      bigram.subset <- bigram.data.table[key == bigram.key,]
      bigram.subset <- bigram.subset[!(bigram.subset$target %chin% predictions$target),]
      bigram.predictions <- head(bigram.subset[order(bigram.subset$score, decreasing = T)],n = num.results-num.rows)
      predictions <- rbind(predictions, bigram.predictions)
      num.rows <- nrow(predictions)     
      if(num.rows < num.results) {
        #Unigram case, 0.4 * 0.4 * 0.4 (for this 4-gram model) , just the top num.results
        predictions <- rbind(predictions, head(unigram.predictions.cache.data.table,n = num.results-num.rows))
      }
    }
  }
  #predictions <- predictions[order(predictions$score, decreasing = T)]
  return(predictions$target)
}

# Benchmark
print("Benchmark modelV1")
source("benchmark.R")

benchmark(predict, 
          # additional parameters to be passed to the prediction function can be inserted here
          sent.list = list('quizzes' = quizzes, 
                           'tweets' = tweets, 
                           'blogs' = blogs), 
          ext.output = T)