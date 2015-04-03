# Stupid backoff

# some part of the N-grams are dominated by stopwords

library(tm)

load("spData.RData")

#Stupid backoff 4-gram------------------------------------------------------------------------------
# http://www.aclweb.org/anthology/D07-1090.pdf

##Parameters
input <- "Very early observations on the Bills game: Offense still struggling but the"



#alpha <-  0.4

#tetragram.df.clean$score <- tetragram.df.clean$prob
#trigram.df.clean$score <- trigram.df.clean$prob * alpha
#bigram.df.clean$score <- bigram.df.clean$prob * (alpha ^ 2)

#unigram.df$score <- unigram.df$prob * (alpha ^ 3)
#unigram.predictions.cache <- head(unigram.df[order(unigram.df$score, decreasing = T),],n = 3)

#library(data.table)

#tetragram.data.table <- as.data.table(tetragram.df.clean)
#setkey(tetragram.data.table,key)
#trigram.data.table <- as.data.table(trigram.df.clean)
#setkey(trigram.data.table,key)
#bigram.data.table <- as.data.table(bigram.df.clean)
#setkey(bigram.data.table,key)
#unigram.predictions.cache.data.table <- as.data.table(unigram.predictions.cache)


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
  #tetragram.subset$score <- tetragram.subset$prob
  predictions <- head(tetragram.subset[order(tetragram.subset$score, decreasing = T)],n = num.results)
  num.rows <- nrow(predictions)
  #print(paste("Num.rows tetragram",num.rows))
  if(num.rows < num.results) {
    #3-gram case, 0.4 (for this 4-gram model) , just the top num.results
    trigram.subset <- trigram.data.table[key == trigram.key,]
    #trigram.subset$score <- trigram.subset$prob * alpha
    trigram.predictions <- head(trigram.subset[order(trigram.subset$score, decreasing = T)],n = num.results-num.rows)
    predictions <- rbind(predictions, trigram.predictions)
    num.rows <- nrow(predictions)
    
    if(num.rows < num.results) {
      #2-gram case, 0.4 * 0.4 (for this 4-gram model) , just the top num.results
      bigram.subset <- bigram.data.table[key == bigram.key,]
      #bigram.subset$score <- bigram.subset$prob * (alpha ^ 2)
      bigram.predictions <- head(bigram.subset[order(bigram.subset$score, decreasing = T)],n = num.results-num.rows)
      predictions <- rbind(predictions, bigram.predictions)
      num.rows <- nrow(predictions)     
      if(num.rows < num.results) {
        #Unigram case, 0.4 * 0.4 * 0.4 (for this 4-gram model) , just the top num.results
        predictions <- rbind(predictions, head(unigram.predictions.cache.data.table,n = num.results-num.rows))
      }
    }
  }
  
  return(predictions$target)
}

#------------------------------------------------------------------------------

#Accuracy check----------------------------------------------------------------

#library(doParallel)
#library(tm)

load("test.data.last.word.RData")

#nodes <- detectCores() - 1
#cl <- makeCluster(nodes)
#registerDoParallel(cl)

testPrediction <- function(input,target){
  target %in% predict(input)  
}


library(rbenchmark)


benchmark(pNew = sapply(test.data.last.word[1:2000,1],predict), replications = 1)

predictNew(input)

test.results <- mapply(testPrediction,test.data.last.word[1:100,1],test.data.last.word[1:100,2], USE.NAMES = F)

# test.results <- foreach(i=1:100) %dopar% {
#   testPrediction(test.data.last.word[i,1],test.data.last.word[i,2])
# }


sum(test.results)/length(test.results)



#------------------------------------------------------------------------------