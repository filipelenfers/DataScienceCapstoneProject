# Stupid backoff

# some part of the N-grams are dominated by stopwords

library(tm)
library(data.table)



#All the data has the score pre calculated considering alpha as 0.4
load("spData4.RData")

# remove < 5 frquency N-grams does not impact accuracy, and giveme a lot of economy in memory. Should check this removal better to remove more useless data.
#tetragram.data.table <- tetragram.data.table[Freq > 5]
#trigram.data.table <- trigram.data.table[Freq > 5]
#bigram.data.table <- bigram.data.table[Freq > 5]

#save(tetragram.data.table,trigram.data.table,bigram.data.table, unigram.predictions.cache.data.table, file = "spData3.RData")

#----------------------------------------------------------------
#keep the best 3 scores for each key
library(data.table)
library(dplyr)

onlyTop3ForKey <- function(ngram.data.table){
  group.order.by.score <- ngram.data.table %>% group_by(key) %>% arrange(desc(score)) %>% mutate(group.count = n())
  ngram.data.table <- union(group.order.by.score %>% filter(group.count > 3) %>% group_by(key) %>% slice(1:3),
                            group.order.by.score %>% filter(group.count <= 3))
  ngram.data.table <- ngram.data.table %>% select(key,target,score)
  ngram.data.table <- data.table(ngram.data.table)
  setkey(ngram.data.table,"key")
  ngram.data.table  
}

tetragram.data.table <- onlyTop3ForKey(tetragram.data.table)
trigram.data.table <- onlyTop3ForKey(trigram.data.table)
bigram.data.table <- onlyTop3ForKey(bigram.data.table)

unigram.predictions.cache.data.table <- unigram.predictions.cache.data.table %>% select(key,target,score)
unigram.predictions.cache.data.table <- data.table(unigram.predictions.cache.data.table)
setkey(unigram.predictions.cache.data.table,"key")

save(tetragram.data.table,trigram.data.table,bigram.data.table, unigram.predictions.cache.data.table, file = "spData4b.RData")

#----------------------------------------------------------------


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

#------------------------------------------------------------------------------

#Accuracy check----------------------------------------------------------------

#library(doParallel)
#library(tm)
#library(stringi)

load("mini.test.data.last.word.RData")

#test.data.last.word <- test.data.last.word[1:1000]
#nodes <- detectCores() - 2
#cl <- makeCluster(nodes)
#registerDoParallel(cl)

testPrediction <- function(input,target){
  target %in% predict(input)  
}


#mini.test.data.last.word <- test.data.last.word[1:100000,]
#save(mini.test.data.last.word,file="mini.test.data.last.word.RData")

test.results <- mapply(testPrediction,mini.test.data.last.word[,1],mini.test.data.last.word[,2], USE.NAMES = F)



accuracy <- sum(test.results)/length(test.results)
accuracy



#------------------------------------------------------------------------------



#Context checking--------------------------------------------------------------
#reside 
#Check for the words, in the unigram, the probabiliticy to be in the same phrase that contains a word that has the meaning of residde.


text1 <- "I really love this place, really. Live in New York is one os the best things I have done. Kisses to  everyone, ok?"
text2 <- "New yOrk is the best place to reside!"
text3 <- "Anyone that lives in new york herE? I cant find a good restaurant in new york"
text.sample <- c(text1,text2,text3)
target <- "york"

reside.dictionary <- c("reside", "live", "inhabit")
reside.pattern <- sprintf("%s",paste(reside.dictionary,collapse="|"))

text.sample <- tolower(text.sample)

phrases <- as.vector(stri_split_regex(text.sample,pattern="[\\.\\?!]", simplify = T, omit_empty = T))

phrases.with.target <- phrases[grepl(target,phrases)]

#count how many phrases have a work in the group. #Frequency
#sum(grepl(reside.pattern,phrases.with.target))
#divide abouve count / number of phrases with target. #Probabilit
sum(grepl(reside.pattern,phrases.with.target))/length(phrases.with.target)



#------------------------------------------------------------------------------